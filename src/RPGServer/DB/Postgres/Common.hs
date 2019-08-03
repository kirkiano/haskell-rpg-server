{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances #-}

module RPGServer.DB.Postgres.Common ( P,
                                      execSingle,
                                      pgQ,
                                      pgE,
                                      Conn(..),
                                      ConnectInfo(..) ) where

import RPGServer.Common
import Data.Time                         ( UTCTime )
import Database.PostgreSQL.Simple        ( Connection,
                                           ConnectInfo(..),
                                           execute,
                                           FromRow,
                                           Query,
                                           query,
                                           ToRow )
import RPGServer.DB.Error                ( D )


instance Show Connection where
  show = const "Postgres DB"


data Conn = Conn {
  _ctime :: UTCTime,
  _db    :: Connection
} deriving Show


type P m = ReaderT Conn m


pgQ :: (MonadIO m,
        Show q,
        ToRow q,
        FromRow r) => Query -> q -> D (P m) [r]
pgQ q params = lift $ do { db <- asks _db; liftIO $ query db q params }


pgE :: (MonadIO m, Show q, ToRow q) => Query -> q -> D (P m) Integer
pgE q params = lift $ do db <- asks _db
                         toInteger <$> (liftIO $ execute db q params)


execSingle :: (MonadIO m, Show q, ToRow q) => Query -> q -> P m ()
execSingle q params = do
  nE <- runExceptT $ pgE q params
  liftIO $ case nE of
    Right 0 -> logInsertionErr ("No insertion by query " ++ show q ++
                                " with params " ++ show params)
    Right n -> when (n /= 1) $ logInsertionErr
               ("Nonunique insertion by query " ++ show q ++
                " with params " ++ show params)
    Left e  -> fail $ show e


logInsertionErr :: String -> IO ()
logInsertionErr = fail . ("Postgres: log insertion error: " ++)
