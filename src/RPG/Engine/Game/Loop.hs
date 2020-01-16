
module RPG.Engine.Game.Loop ( gameLoop ) where

import RPG.Engine.Common           hiding ( say, handle )
import qualified Data.Set                 as S
import qualified System.Log               as L
import qualified RPG.Engine.Log           as L
import RPG.World                          ( CharID )
import RPG.Engine.Player                  ( Player(Player) )
import RPG.Event                          ( Event )
import RPG.Message                        ( Message(..),
                                            CharMessage(..), )
import RPG.Request                        ( Request(..),
                                            CharRequest,
                                            isJoin,
                                            isQuit )
import RPG.Engine.Game.Play               ( play )
import RPG.DB                             ( Db )
import qualified RPG.Error.Data           as D
import RPG.Engine.Drive                   ( Drive(..) )
import RPG.Error                          ( Error(DataError) )
import RPG.Engine.Game.LoopState          ( L,
                                            notify,
                                            addEventSender,
                                            dropEventSender )

gameLoop :: (MonadIO m,
             Db m,
             L.Log m L.Game,
             L.Log m Request,
             L.Log m Message) => m (Request, Message -> m ()) -> L m ()
gameLoop nextRequest = forever $ lift nextRequest >>= handle where
  handle (q, sendMsg) = do
    maybe (return ()) (loginChar sendMsg) $ isJoin q
    maybe (return ()) logoutChar          $ isQuit q
    (msgM, ers) <- lift $ drive q
    mapM_ (uncurry notify) ers
    maybe (return ()) report msgM
    where
      report msg = L.log (logLevel msg) msg >> lift (sendMsg msg) where
        logLevel (Error _ _) = L.Info
        logLevel _           = L.Debug

loginChar :: (Monad m, L.Log m L.Game) => (Message -> m ()) -> CharID -> L m ()
loginChar sendMsg cid = do
  let sndr = sendMsg . CharMessage cid . EventMessage
  L.log L.Debug $ L.SendingFunction L.AddingSendingFunction cid
  addEventSender cid sndr

logoutChar :: (Monad m, L.Log m L.Game) => CharID -> L m ()
logoutChar cid = do
  L.log L.Debug $ L.SendingFunction L.DroppingSendingFunction cid
  dropEventSender cid

-----------------------------------------------------------
-- drive

drive :: (MonadIO m, Db m, L.Log m L.Game) =>
         Request -> m (Maybe Message, S.Set (Event, S.Set CharID))

drive q@(CharactersByPrefix pf) = runAndMsg q f $ CharIDsWithPrefix pf where
  f = getCharactersByPrefix pf

drive q@(CreateCharacter n d pid) = runAndMsg q f $ CharacterCreated n where
  f = createCharacter n d pid

drive q@(DestroyCharacter cid) = runAndMsg q f CharacterDestroyed where
  f = deleteCharacter cid >> return cid

drive (CharRequest cid pq) = do
  (cmsgM, ers) <- charRequest cid pq
  return (fmap (CharMessage cid) cmsgM, ers)


runAndMsg :: Monad m =>
             Request -> ExceptT D.Error m a -> (a -> Message) ->
             m (Maybe Message, S.Set (Event, S.Set CharID))
runAndMsg q f sd = (, S.empty) . Just . either err sd <$> runExceptT f where
  err = Error q . DataError

-----------------------------------------------------------
-- play

charRequest :: (MonadIO m, Db m, L.Log m L.Game) =>
               CharID ->
               CharRequest ->
               m (Maybe CharMessage, S.Set (Event, S.Set CharID))
charRequest cid cq = return . either err ok =<< playIt where
  playIt       = runReaderT (runExceptT $ play cq) $ Player cid
  err e        = (Just $ CharError cq e, S.empty)
  ok (vM, ers) = (fmap ValueMessage vM, ers)
