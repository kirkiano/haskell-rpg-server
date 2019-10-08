{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances #-}

module RPGServer.DB.Postgres.Common ( PG,
                                      pgQ,
                                      pgQ1,
                                      pgE,
                                      pgE1,
                                      Conn(..),
                                      ConnectInfo(..),
                                      beginTxn,
                                      commitTxn ) where

import RPGServer.Common           hiding ( catch, handle )
import Control.Monad.Catch               ( catch )
import Data.Time                         ( UTCTime )
import Database.PostgreSQL.Simple        ( begin,
                                           commit,
                                           Connection,
                                           ConnectInfo(..),
                                           execute,
                                           FromRow,
                                           Query,
                                           formatQuery,
                                           query,
                                           ToRow )
import qualified System.Log              as L
import qualified RPGServer.Log           as L
import RPGServer.DB.Error                ( D, DBError(..) )


instance Show Connection where
  show = const "Postgres DB"


data Conn = Conn {
  _ctime :: UTCTime,
  _db    :: Connection
} deriving Show


type PG m = ReaderT Conn m


getDB :: Monad m => PG m Connection
getDB = asks _db

showQuery :: (MonadIO m, ToRow q) => Query -> q -> PG m String
showQuery sql params = do c <- getDB
                          liftIO $ show <$> formatQuery c sql params

logQuery :: (MonadIO m, ToRow q, L.Log m L.DB) => Query -> q -> PG m ()
logQuery sql params = do s <- showQuery sql params
                         lift $ L.log L.DebugBytes $ L.DBQuery s

beginTxn :: MonadIO m => D (PG m) ()
beginTxn = lift $ liftIO . begin =<< getDB

commitTxn :: MonadIO m => D (PG m) ()
commitTxn = lift $ liftIO . commit =<< getDB

-----------------------------------------------------------
-- queries

pgQ :: (MonadIO m,
        L.Log m L.DB,
        Show p,
        ToRow p,
        FromRow r) => Query -> p -> D (PG m) [r]
pgQ sql params = do
  lift $ logQuery sql params
  db <- lift getDB
  let q        = query db sql params
      handle e = return $ Left $ InternalError $ show (e :: SomeException)
  ExceptT $ liftIO $ catch (Right <$> q) handle


-- query that should return a single row
pgQ1 :: (MonadIO m,
         L.Log m L.DB,
         Show q,
         ToRow q,
         FromRow r) => Query -> q -> D (PG m) r
pgQ1 q params = pgQ q params >>= expectSingleton


expectSingleton :: Monad m => [r] -> D (PG m) r
expectSingleton []  = throwE Empty
expectSingleton [x] = return x
expectSingleton _   = throwE NotSingleton

-----------------------------------------------------------
-- updates

pgE :: (MonadIO m, MonadCatch m,
        L.Log m L.DB,
        Show q, ToRow q)
        => Query -> q -> D (PG m) Int
pgE sql params = do
  lift $ logQuery sql params
  db <- lift getDB
  let q        = fromIntegral . toInteger <$> (liftIO $ execute db sql params)
      handle e = return $ Left $ InternalError $ show (e :: SomeException)
  ExceptT $ catch (Right <$> q) handle


-- update that should affect a single row
pgE1 :: (MonadIO m, MonadCatch m,
         L.Log m L.DB,
         Show q, ToRow q)
         => Query -> q -> D (PG m) ()
pgE1 q params = pgE q params >>= expectSingle


expectSingle :: MonadIO m => Int -> D (PG m) ()
expectSingle 1 = except $ Right ()
expectSingle 0 = except $ Left NoUpdate
expectSingle _ = except $ Left MultipleUpdate
