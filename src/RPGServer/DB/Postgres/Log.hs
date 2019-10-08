{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             InstanceSigs,
             FlexibleContexts,
             TypeSynonymInstances #-}

module RPGServer.DB.Postgres.Log () where

import RPGServer.Common
import RPGServer.Util.Text
import Data.Text                          ( toLower )
import RPGServer.DB.Postgres.Common       ( PG, pgE1 )
import Database.PostgreSQL.Simple         ( Query,
                                            ToRow )
import Database.PostgreSQL.Simple.ToField ( Action(Escape),
                                            ToField(..) )
import Data.Time.Clock.POSIX              ( getCurrentTime )
import qualified RPGServer.Log            as L


instance (Monad m, L.LogThreshold m) => L.LogThreshold (PG m) where
  logThreshold = lift L.logThreshold


instance (MonadIO m,
          MonadCatch m,
          L.Log m L.DB,
          L.Log m L.Connection) => L.Log (PG m) L.Connection where
  logWrite lev (L.AcceptedConnection d s) = logConnection lev d $ Just s
  logWrite lev (L.ConnectionClosed d r) = logConnection lev d $ Just r
  logWrite lev (L.RejectedOriginlessConnection ct) = do
    t <- liftIO getCurrentTime
    insertSingle sql (t, ct, lev) where
      sql = "insert into log_rejectedoriginlessconnection \
           \        (time, level_id, type_id) \
           \ select ?, L.id, CT.id \
           \ from log_level L, log_connectiontype CT \
           \ where CT.code = ? \
           \   and L.code = ?"


instance L.Log m L.Transmission => L.Log (PG m) L.Transmission where
  logWrite lev = lift . (L.logWrite lev)


instance L.Log m L.Game => L.Log (PG m) L.Game where
  logWrite lev = lift . (L.logWrite lev)


logConnection :: (MonadIO m, MonadCatch m, L.Log m L.DB) =>
                 L.Level ->
                 L.ConnectionType ->
                 Maybe String ->
                 PG m ()
logConnection lev ct rM = do
  t  <- liftIO getCurrentTime
  let esc     = Escape . encodeUtf8
      hello   = esc "hello"
      goodbye = esc "goodbye"
      cet = maybe hello (const goodbye) rM
  insertSingle sql (t, rM, cet, lev, ct)
  where
    sql = "insert into log_connectionevent \
         \        (time, event_id, level_id, type_id, comment) \
         \ select ?, CE.id, L.id, CT.id, ? \
         \ from log_level L, \
         \      log_connectiontype CT, \
         \      log_connectioneventtype CE \
         \ where CE.code = ? \
         \   and L.code = ? \
         \   and CT.code = ?"


instance ToField L.ConnectionType where
  toField = Escape . encodeUtf8 . f where
    f L.Websocket = "ws"
    f L.Socket    = "s"
    
instance ToField L.Level where
  toField = Escape . encodeUtf8 . toLower . text

-----------------------------------------------------------

insertSingle :: (MonadIO m, MonadCatch m, L.Log m L.DB, Show q, ToRow q)
                =>
                Query -> q -> PG m ()
insertSingle sql prms = runExceptT (pgE1 sql prms) >>= either bad return where
  bad = fail . ("Postgres: log insertion error: " ++) . show
