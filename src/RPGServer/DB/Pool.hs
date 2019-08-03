{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             AllowAmbiguousTypes,
             TypeSynonymInstances,
             UndecidableInstances,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.DB.Pool ( {- Pool,
                           PoolParams(..) -} ) where
{-
import RPGServer.Common
import Prelude hiding           ( getContents )
import Data.Time.Clock          ( NominalDiffTime )
import Data.Pool                ( Pool,
                                  createPool,
                                  destroyAllResources,
                                  withResource )
import qualified RPGServer.Log  as L
import RPGServer.Listen.Auth    ( Auth(..) )
import RPGServer.DB.Error       ( D )
import RPGServer.DB.Class       ( AdminDB(..),
                                  PlayDB(..),
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

instance Auth (ReaderT a L.L) => Auth (ReaderT (Pool a) L.L) where
  authUser            = liftSome . authUser


instance AdminDB (ReaderT a L.L) => AdminDB (ReaderT (Pool a) L.L) where
  markLoggedInSet     = liftIt . markLoggedInSet


instance PlayDB (ReaderT a L.L) => PlayDB (ReaderT (Pool a) L.L) where
  loginCharacter b    = liftIt . (loginCharacter b)
  getThing            = liftIt . getThing
  getThingDescription = liftIt . getThingDescription
  getTHandle          = liftIt . getTHandle
  getCoPlace          = liftIt . getCoPlace
  getCoExits          = liftIt . getCoExits
  getAddress          = liftIt . getAddress
  getCoOccupantIDs    = liftIt . getCoOccupantIDs
  getCoContentHandles = liftIt . getCoContentHandles

  setLocation pid                = liftIt . (setLocation pid)
  setUtterance tid               = liftIt . (setUtterance tid)
  updateThing                    = liftIt . updateThing

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
-}