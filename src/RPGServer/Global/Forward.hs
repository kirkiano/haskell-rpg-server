{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.Global.Forward ( F,
                                  createForwarderTWM,
                                  destroyForwarderTWM,
                                  forkForwarderWatcher ) where

import RPGServer.Common
import qualified Control.Workflow           as W
import qualified Control.Concurrent.MVar    as V
import Control.Concurrent                   as C
import Control.Concurrent.ThreadWithMailbox as T
import Data.Map.Lazy                        as M
import qualified Forwarder                  as F
import qualified RPGServer.Log              as L
import RPGServer.DB.Class                   ( AdminDB(..) )
import RPGServer.World                      ( CharacterID )
import RPGServer.Message                    ( Message )


type F m = F.FTWM CharacterID m Message
type FL m = F.ForwardLog CharacterID m Message


createForwarderTWM :: (W.HasFork m,
                       L.Log m L.Main,
                       L.Log m (FL m)) => m (F m)
createForwarderTWM = do
  L.log L.Info L.StartingForwarder
  T.forkTWM $ Synchronous F.forwarder


destroyForwarderTWM :: (MonadIO m, L.Log m L.Main) => (F m) -> m ()
destroyForwarderTWM f = do
  L.log L.Info L.StoppingForwarder
  T.killTWM f


forkForwarderWatcher :: (W.HasFork m,
                         L.Log m L.Main,
                         AdminDB m) =>
                        F m -> Int -> m C.ThreadId
forkForwarderWatcher f waitMuSecs = do
  L.log L.Info L.StartingForwarderWatcher
  (send, recv) <- F.fwdSRMVar f <$> liftIO V.newEmptyMVar
  W.fork $ forever $ do
    void $ send F.GetRegistrants
    (F.Registrants rs) <- recv
    void $ runExceptT $ markLoggedInSet $ M.keys rs
    liftIO $ C.threadDelay waitMuSecs
