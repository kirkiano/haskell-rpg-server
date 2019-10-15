
module RPG.Engine.Game.Loop ( gameLoop,
                              LoopState(LoopState) ) where

import RPG.Engine.Common           hiding ( say, handle )
import Control.Monad.Trans.State          ( StateT, gets, put )
import qualified Data.Map                 as M
import qualified System.Log               as L
import qualified RPG.Engine.Log           as L
import RPG.World                          ( CharID )
import RPG.Engine.Player                  ( Player(Player) )
import RPG.Message                        ( Message(..),
                                            PlayerMessage(..), )
import RPG.Request                        ( Request(..),
                                            PlayerRequest(Join, Quit))
import RPG.Engine.Game.Play               ( play )
import RPG.Engine.Game.Drive              ( drive )
import RPG.DB                             ( Db )


data LoopState m = LoopState {
  _smap :: M.Map CharID (PlayerMessage -> m ())
}

type L m = StateT (LoopState m) m


instance (Monad m, L.LogThreshold m) => L.LogThreshold (L m) where
  logThreshold = lift L.logThreshold

instance L.Log m a => L.Log (L m) a where
  logWrite lev = lift . (L.logWrite lev)

-----------------------------------------------------------

gameLoop :: (MonadIO m,
             Db m,
             L.Log m L.Game,
             L.Log m Request,
             L.Log m Message)
            =>
            m (Request, Message -> m ()) -> L m ()
gameLoop nextRequest = forever $ lift nextRequest >>= handle where

  handle p = do lift (L.log L.Debug . L.ProcessingRequest . fst $ p)
                process p
                return ()

  process (PlayerRequest cid pq, sm) = processPlayerRequest cid pq sm
  process (q, sendMsg)               = lift $ do
    msg <- drive q
    L.log (logMsgLevel msg) msg
    sendMsg msg


processPlayerRequest :: (MonadIO m, Db m, L.Log m L.Game) =>
                        CharID -> PlayerRequest -> (Message -> m ()) -> L m ()
processPlayerRequest cid pq sendMsg = either err ok =<< playIt where
  playIt         = lift $ runReaderT (runExceptT $ play pq) $ Player cid
  ok             = (updateSendMap pq cid sendPlayer >>) . emit
  err            = lift . void . sendPlayer . PlayerError
  sendPlayer     = sendMsg . (PlayerMessage cid)
  emit (vM, ers) = forM ers eventsNotify >> maybe (return ()) report vM where
    eventsNotify (evt, cids) = do
      L.log L.Debug $ L.EmittingEvent evt cids
      forM cids $ sendCharacter $ EventMessage evt
    report = lift . void . sendPlayer . ValueMessage


sendCharacter :: (MonadIO m, L.Log m L.Game) =>
                 PlayerMessage -> CharID -> L m ()
sendCharacter msg cid = gets _smap >>= (maybe fatal sendOK . (M.lookup cid))
  where fatal         = lg $ L.SendingFunction L.NoSendingFunction cid
        lg            = L.log L.Critical
        sendOK sendIt = lift . sendIt $ msg


updateSendMap :: (MonadIO m, L.Log m L.Game)
                 =>
                 PlayerRequest ->
                 CharID ->
                 (PlayerMessage -> m ()) ->
                 L m ()
updateSendMap q cid sendPlayer = do
  smap0 <- gets _smap
  let lg = L.log L.Debug
  smap1 <- case q of
    Join -> do lg $ L.SendingFunction L.AddingSendingFunction cid
               return $ M.insert cid sendPlayer smap0
    Quit -> do lg $ L.SendingFunction L.DroppingSendingFunction cid
               return $ M.delete cid smap0
    _    -> do return smap0
  put $ LoopState smap1


logMsgLevel :: Message -> L.Level
logMsgLevel (Error _ _) = L.Info
logMsgLevel _           = L.Debug
