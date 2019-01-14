{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings,
             RankNTypes,
             AllowAmbiguousTypes,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             UndecidableInstances,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.DB.Caching ( CachedDB(..) ) where

import RPGServer.Common
import Prelude hiding              ( getContents )
import qualified RPGServer.Log     as L
import RPGServer.DB.Class          ( MakeDB(..),
                                     AuthDB(..),
                                     AdminDB(..),
                                     PlayDB(..) )

data CachedDB d c = CachedDB {
  _db    :: d,
  _cache :: c
}


instance (MonadIO m,
          L.Log m L.Main,
          MakeDB m d di,
          MakeDB m c ci) =>
         MakeDB m (CachedDB d c) (di, ci) where
  connect (di, ci) = CachedDB <$> connect di <*> connect ci
  disconnect s     = do disconnect $ _db s
                        disconnect $ _cache s

type C d c = ReaderT (CachedDB d c)

------------------------------------------------------------

instance L.LogThreshold (ReaderT d m) =>
         L.LogThreshold (C d c m) where
  logThreshold = withReaderT _db L.logThreshold

instance (Monad m,
          L.Log (ReaderT d m) t) => L.Log (C d c m) t where
  logWrite lev = withReaderT _db . (L.logWrite lev)

------------------------------------------------------------

instance (Monad m,
          AdminDB (ReaderT d m),
          AdminDB (ReaderT c m)) => AdminDB (C d c m) where
  markLoggedInSet = mapExceptT (withReaderT _db) . markLoggedInSet


instance (Monad m,
          AuthDB (ReaderT d m),
          AuthDB (ReaderT c m)) => AuthDB (C d c m) where

  authUser u p = do
    db <- asks _db
    lift $ runReaderT (authUser u p) db

  loginCharacter b cid = do
    db <- asks _db
    lift $ runReaderT (loginCharacter b cid) db


instance (Monad m,
          PlayDB (ReaderT d m),
          PlayDB (ReaderT c m)) => PlayDB (C d c m) where

  getThing tid = ExceptT $ do
    cache <- asks _cache
    etr   <- lift $ runReaderT (runExceptT $ getThing tid) cache
    either goSlow (return . Right) etr
    where
    goSlow _ = asks _db >>= \db -> lift $ runReaderT (runExceptT $ getThing tid) db

  getLocation       = mapExceptT (withReaderT _db) . getLocation
  getPlace          = mapExceptT (withReaderT _db) . getPlace
  getContents       = mapExceptT (withReaderT _db) . getContents
  getOccupants      = mapExceptT (withReaderT _db) . getOccupants
  setLocation tid   = mapExceptT (withReaderT _db) . (setLocation tid)
  saveUtterance tid = mapExceptT (withReaderT _db) . (saveUtterance tid)

{-  
  setThing s t = do setThing (_db s) t
                    setThing (_cache s) t

  getLocation = getAndCache getLocation setLocation

  setLocation s tid pid = do setLocation (_db s) tid pid
                             setLocation (_cache s) tid pid

  getContents  = getContents . _db
  getOccupants = getOccupants . _db


getAndCache :: (DB m d pd,
                DB m c pc) =>
               (forall e. forall q. DB m e q => e -> i -> D m a) ->
               (c -> i -> a -> D m ()) ->
               CachedDB d c ->
               i ->
               D m a
getAndCache get set c i = either goSlow return =<< tryFast
  where tryFast  = lift $ runExceptT $ get (_cache c) i
        goSlow _ = helper (get $ _db c) (set (_cache c) i) i
        helper :: (Monad m) => (a -> m b) -> (b -> m ()) -> a -> m b
        helper getSlow setFast arg = do x <- getSlow arg
                                        setFast x
                                        return x
-}
