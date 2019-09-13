{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NumericUnderscores,
             TupleSections,
             TypeSynonymInstances,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts #-}

module RPGServer.Game.Loop ( gameLoop,
                             LoopState(LoopState) ) where

import RPGServer.Common           hiding ( say, handle )
import Control.Monad.Trans.State         ( StateT, gets, put )
import Control.Concurrent                ( threadDelay )
import qualified Data.Map                as M
import qualified System.Log              as L
import qualified SendReceive             as SR
import qualified RPGServer.Log           as L
import RPGServer.World                   ( CID )
import RPGServer.Player                  ( Player(Player) )
import RPGServer.Message                 ( Message(..),
                                           PlayerMessage(..), )
import RPGServer.Request                 ( Request(..),
                                           PlayerRequest(Join, Quit))
-- import RPGServer.Error                   ( GameError(CannotGrantRequest) )
import RPGServer.DB.Class                ( DriverDB, PlayDB )
import RPGServer.DB.Play                 ( )
import RPGServer.DB.Drive                ( )
import RPGServer.Game.Play               ( play )
import RPGServer.Game.Drive              ( drive )


data LoopState m = LoopState {
  _smap :: M.Map CID (SR.Send m PlayerMessage)
}

type L m = StateT (LoopState m) m


instance (Monad m, L.LogThreshold m) => L.LogThreshold (L m) where
  logThreshold = lift L.logThreshold

instance L.Log m a => L.Log (L m) a where
  logWrite lev = lift . (L.logWrite lev)

-----------------------------------------------------------

gameLoop :: (MonadIO m,
             DriverDB m, -- DriverDB implies PlayDB
             L.Log m L.Game,
             L.Log m Request,
             L.Log m Message)
            =>
            SR.WaitReceive m (Request, SR.Send m Message) -> L m ()
gameLoop nextRequest = forever $ lift nextRequest >>= handle where

  handle (SR.Received p) = L.log L.Debug (fst p) >> process p
  handle _               = return ()
  {- handle SR.CannotReceive = do L.log L.Critical L.CannotReceiveNextRequest
                               liftIO $ threadDelay 10_000_000 -- wait 10s -}

  process (PlayerRequest cid pq, sm) = processPlayerRequest cid pq sm
  process (q, sendMsg)               = lift $ do
    msg <- drive q
    L.log (logMsgLevel msg) msg
    sendSuccess <- sendMsg msg
    when (not sendSuccess) $ L.log L.Info $ L.CannotSendMessage msg


processPlayerRequest :: (MonadIO m, PlayDB m, L.Log m L.Game) =>
                        CID -> PlayerRequest -> SR.Send m Message -> L m ()
processPlayerRequest cid pq sendMsg = either err ok =<< playIt where
  playIt         = lift $ runReaderT (runExceptT $ play pq) $ Player cid
  ok             = (updateSendMap pq cid sendPlayer >>) . emit
  err            = lift . void . sendPlayer . PlayerError
  sendPlayer     = sendMsg . (PlayerMessage cid)
  emit (vM, ers) = forM ers eventsNotify >> maybe (return ()) report vM where
    eventsNotify (evt, cids) = do
      L.log L.Debug $ L.EmittingEvent evt cids
      forM cids $ sendCharacter $ EventMessage evt
    report v = do let vMsg = ValueMessage v
                  L.log L.Debug $ L.SendingPlayerMessage cid vMsg
                  lift . void . sendPlayer $ vMsg


sendCharacter :: (MonadIO m, L.Log m L.Game) => PlayerMessage -> CID -> L m ()
sendCharacter msg cid = gets _smap >>= (maybe fatal sendOK . (M.lookup cid))
  where fatal         = lg $ L.SendingFunction L.NoSendingFunction cid
        lg            = L.log L.Critical
        sendOK sendIt = do ok <- lift . sendIt $ msg
                           when (not ok) $ lg $ L.CannotSendToCharacter cid msg


updateSendMap :: (MonadIO m, L.Log m L.Game)
                 =>
                 PlayerRequest ->
                 CID ->
                 SR.Send m PlayerMessage ->
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
logMsgLevel (Error _) = L.Info
logMsgLevel _         = L.Debug
