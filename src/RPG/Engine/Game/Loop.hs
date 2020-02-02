
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
import RPG.Error                          ( Error(DataError,
                                                  CharacterNotLoggedIn) )
import RPG.Engine.Game.LoopState          ( L,
                                            notify,
                                            isCharIDRegistered,
                                            addEventSender,
                                            dropEventSender )

gameLoop :: (MonadIO m,
             Db m,
             L.Log m L.Game,
             L.Log m Request,
             L.Log m Message) => m (Request, Message -> m ()) -> L m ()
gameLoop nextRequest = forever $ lift nextRequest >>= handle where
  handle (q, sendMsg) = do
    L.log L.Info q
    maybe (return ()) (loginChar sendMsg) $ isJoin q
    (msgM, ers) <- drive q
    mapM_ (uncurry notify) ers
    maybe (return ()) report msgM
    maybe (return ()) logoutChar $ isQuit q
    where
      report msg = L.log (logLevel msg) msg >> lift (sendMsg msg) where
        logLevel (Error _ _) = L.Warn
        logLevel _           = L.Info

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
         Request -> L m (Maybe Message, S.Set (Event, S.Set CharID))

drive q@(CharactersByPrefix pf) = runAndMsg q f $ CharIDsWithPrefix pf where
  f = getCharactersByPrefix pf

drive q@(CreateCharacter n d pid) = runAndMsg q f $ CharacterCreated n where
  f = createCharacter n d pid

drive q@(DestroyCharacter cid) = runAndMsg q f CharacterDestroyed where
  f = deleteCharacter cid >> return cid

drive q@(CharRequest cid pq) = do
  loggedIn <- isCharIDRegistered cid
  if loggedIn then ok else return err where
    ok  = do (cmsgM, ers) <- lift $ charRequest cid pq
             return (fmap (CharMessage cid) cmsgM, ers)
    err = (Just . Error q . CharacterNotLoggedIn $ cid, S.empty)


runAndMsg :: Monad m =>
             Request -> ExceptT D.Error m a -> (a -> Message) ->
             L m (Maybe Message, S.Set (Event, S.Set CharID))
runAndMsg q f sd = lift $ (, S.empty) . Just . either err sd <$> runExceptT f
  where err = Error q . DataError

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
