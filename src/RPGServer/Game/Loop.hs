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
import RPGServer.World                   ( CharacterID )
import RPGServer.Player                  ( Player(Player) )
import RPGServer.Message                 ( Message(..),
                                           PlayerMessage(..), )
import RPGServer.Request                 ( Request(..),
                                           PlayerRequest(Join, Quit))
import RPGServer.DB.Class                ( DriverDB )
import RPGServer.DB.Play                 ( )
import RPGServer.Game.Play               ( play )
-- import RPGServer.Game.Drive              ( drive )


data LoopState m = LoopState {
  _smap :: M.Map CharacterID (SR.Send m PlayerMessage)
}

type L m = StateT (LoopState m) m


instance (Monad m, L.LogThreshold m) => L.LogThreshold (L m) where
  logThreshold = lift L.logThreshold

instance L.Log m L.Game => L.Log (L m) L.Game where
  logWrite lev = lift . (L.logWrite lev)



gameLoop :: (MonadIO m,
             DriverDB m, -- DriverDB implies PlayDB
             L.Log m L.Game)
            =>
            SR.WaitReceive m (Request, SR.Send m Message) -> L m ()
gameLoop nextRequest = forever $ lift nextRequest >>= handle where

  handle (SR.Received x)  = process x
  handle SR.CannotReceive = do L.log L.Critical L.CannotReceiveNextRequest
                               liftIO $ threadDelay 10_000_000 -- wait 10s

  process (PlayerRequest cid q, sendMsg) = do
    L.log L.Debug $ L.ReceivedPlayerRequest cid q
    either err ok =<< (lift $ runReaderT (runExceptT $ play q) $ Player cid)
    where
    ok             = (updateSendMap q cid sendPlayer >>) . emit
    err            = lift . void . sendPlayer . PlayerError
    sendPlayer     = sendMsg . (PlayerMessage cid)
    emit (vM, ers) = forM ers eventsNotify >> maybe (return ()) report vM where
      eventsNotify (evt, cids) = do
        L.log L.Debug $ L.EmittingEvent evt cids
        forM cids $ sendCharacter $ EventMessage evt
      report v = do
        L.log L.Debug $ L.SendingValue v cid
        lift . void . sendPlayer . ValueMessage $ v

  process _ = return () -- TODO: continue


sendCharacter :: (MonadIO m, L.Log m L.Game)
                 =>
                 PlayerMessage -> CharacterID -> L m ()
sendCharacter msg cid = gets _smap >>= (maybe fatal sendOK . (M.lookup cid))
  where fatal         = L.log L.Critical $ L.NoSenderForCharacter cid
        sendOK sendIt = do
          ok <- lift . sendIt $ msg
          when (not ok) $ L.log L.Critical $ L.CannotSendToCharacter cid msg


updateSendMap :: (MonadIO m, L.Log m L.Game)
                 =>
                 PlayerRequest ->
                 CharacterID ->
                 SR.Send m PlayerMessage ->
                 L m ()
updateSendMap q cid sendPlayer = do
  smap0 <- gets _smap
  smap1 <- case q of
    Join -> do L.log L.Debug $ L.AddingSender cid
               return $ M.insert cid sendPlayer smap0
    Quit -> do L.log L.Debug $ L.DroppingSender cid
               return $ M.delete cid smap0
    _    -> do return smap0
  put $ LoopState smap1
