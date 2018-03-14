{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             AllowAmbiguousTypes,
             TypeSynonymInstances,
             UndecidableInstances,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.DB.Pool ( Pool,
                           PoolParams(..) ) where

import RPGServer.Common
import Prelude hiding        ( getContents )
import Data.Time.Clock       ( NominalDiffTime )
import Data.Pool             ( Pool,
                               createPool,
                               destroyAllResources,
                               withResource )
import qualified RPGServer.Log  as L
import RPGServer.DB.Error    ( D )
import RPGServer.DB.Class    ( DB(..),
                               MakeDB(..) )


data PoolParams a = PoolParams {
  nStripes          :: Int,
  maxConnsPerStripe :: Int,
  maxIdleSeconds    :: NominalDiffTime,
  connInfo          :: a
}


instance MakeDB L.L a r => MakeDB L.L (Pool a) (PoolParams r) where
  connect r  = do s <- ask
                  liftIO $ createPool
                    (runReaderT (connect $ connInfo r) s)
                    (\c -> runReaderT (disconnect c) s)
                    (nStripes r)
                    (maxIdleSeconds r)
                    (maxConnsPerStripe r)
  disconnect = liftIO . destroyAllResources

------------------------------------------------------------

instance DB (ReaderT a L.L) => DB (ReaderT (Pool a) L.L) where
  authUser uname      = liftSome . (authUser uname)
  loginCharacter b    = liftSome . (loginCharacter b)
  getThing            = liftIt . getThing
  getLocation         = liftIt . getLocation
  getPlace            = liftIt . getPlace
  getOccupants        = liftIt . getOccupants
  getContents         = liftIt . getContents
  setLocation tid     = liftIt . (setLocation tid)
{- setThing t          = withResource p (\d -> setThing d t) -}

------------------------------------------------------------

instance L.LogThreshold (ReaderT a L.L) =>
         L.LogThreshold (ReaderT (Pool a) L.L) where
  logThreshold = liftSome L.logThreshold

instance L.Log (ReaderT a L.L) t => L.Log (ReaderT (Pool a) L.L) t where
  logWrite lev = liftSome . (L.logWrite lev)

------------------------------------------------------------

liftIt :: D (ReaderT a L.L) b -> D (ReaderT (Pool a) L.L) b
liftIt = mapExceptT liftSome

liftSome :: ReaderT a L.L b -> ReaderT (Pool a) L.L b
liftSome x = do
  p  <- ask
  lh <- lift ask
  liftIO $ withResource p (\d -> runReaderT (runReaderT x d) lh)
