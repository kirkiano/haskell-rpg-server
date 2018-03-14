{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses,
             UndecidableInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.Global.Forward ( F,
--                                  Forward(..),
                                  createForwardTWM,
                                  destroyForwardTWM ) where

import RPGServer.Common
import Control.Workflow                     ( HasFork )
import Control.Concurrent.ThreadWithMailbox as T
import qualified SendReceive                as SR
import qualified Forwarder                  as F
import qualified RPGServer.Log              as L
import RPGServer.DB.Class                   ( DB(..) )
import RPGServer.World                      ( CharacterID )
import RPGServer.Message                    ( Message )


type FTWM a m g = TWM (F.Request a m g)
type F        m = FTWM CharacterID m Message


createForwardTWM :: (Ord a,
                     HasFork m,
                     L.Log m L.Main) => m (FTWM a m g)
createForwardTWM = do
  L.log L.Info L.StartingForwarder
  T.forkTWM $ Synchronous F.forwarder


destroyForwardTWM :: (MonadIO m,
                      L.Log m L.Main) => FTWM a m g -> m ()
destroyForwardTWM f = do
  L.log L.Info L.StoppingForwarder
  T.killTWM f

------------------------------------------------------------
{-
class Monad m => Forward a g m where
  forward :: g -> [a] -> m ()

instance MonadIO m => Forward a g (ReaderT (FTWM a m g) m) where
  forward msg ids = do let f = F.Forward msg ids
                           r = const (return True :: m Bool)
                           q = F.Request f r
                       fw <- ask
                       void $ SR.send fw q

instance DB m => DB (ReaderT (F m) m) where
  authUser u = lift . (authUser u)
  getThing   = mapExceptT lift . getThing
-}
