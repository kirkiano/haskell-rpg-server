{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances #-}

module RPGServer.DB.Postgres.Class ( Conn(..),
                                     ConnectInfo(..) ) where

import RPGServer.Common
import RPGServer.Util.Text
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Control.Exception        as E
import Data.Time.Clock.POSIX              ( getCurrentTime )
import Database.PostgreSQL.Simple as PG   ( ConnectInfo(..),
                                            connect,
                                            close,
                                            execute,
                                            Only(..),
                                            query )
import qualified RPGServer.Log            as L
import qualified RPGServer.World          as W
import qualified Crypto.KDF.PBKDF2        as K
import RPGServer.DB.Error                 ( DBError(..) )
import RPGServer.DB.Class                 ( DB(..),
                                            MakeDB(..) )
import RPGServer.DB.Postgres.Common       ( P,
                                            pgE,
                                            pgQ,
                                            Conn(..) )


instance (MonadIO m, L.Log m L.Main) => MakeDB m Conn ConnectInfo where
  connect ci   = do
    L.log L.Debug L.AttemptingToConnectToPostgres
    tryConn <- liftIO $ E.handle
               (\e -> return $ Left (e :: SomeException))
               (Right <$> (Conn <$> getCurrentTime <*> PG.connect ci))
    let bad e  = do L.log L.Critical $ L.CannotConnectToPostgres (show e)
                    E.throw e
        good c = L.log L.Debug L.ConnectedToPostgres >> return c
    either bad good tryConn
  disconnect c = do liftIO $ close $ _db c
                    L.log L.Debug L.DisconnectedFromPostgres


instance MonadIO m => DB (P m) where

  authUser uname pw = do
    let sql = "select C.thing_ptr_id, U.password \
             \ from auth_user U \
             \ left join world_character C on U.id = C.user_id \
             \ where U.username = ?"
        pG = encodeUtf8 pw
        match pS = if B.take 7 pG == "pbkdf2_"
          then pS == pG
          else h == hash where
            [_, _, salt, hash] = B.split (B.c2w '$') pS
            h = B64.encode $ K.fastPBKDF2_SHA256 m pG salt
            m = K.Parameters 100000 32
    db <- asks _db
    rs <- liftIO $ query db sql $ Only uname
    return $ case rs of
      [(cid, pass)] -> toMaybe (match pass) cid
      _             -> Nothing

  loginCharacter b cid = do
    let sql = "update world_character \
             \ set is_logged_in = ? \
             \ where thing_ptr_id = ?"
    db <- asks _db
    void $ liftIO $ execute db sql (b, cid)

  getThing tid = pgQ sql (Only tid) >>= f where
    sql   = "select name from world_thing where id = ?"
    f [r] = return $ W.ThingRec Nothing tid $ fromOnly r
    f   _ = throwE $ InvalidThingID tid

  getLocation tid = pgQ sql (Only tid) >>= f where
    sql   = "select place_id from world_located where thing_id = ?"
    f [r] = return $ fromOnly r
    f   _ = throwE $ InvalidThingID tid

  getPlace pid = do
    let sql = "select P.name, \
             \        P.description, \
             \        E.id, \
             \        R.name, \
             \        X.name, \
             \        R.trans, \
             \        D.id, \
             \        D.name \
             \ from world_place P \
             \ inner join world_exit E on E.source_id = P.id \
             \ inner join world_direction X on X.id = E.direction_id \
             \ inner join world_portal R on R.id = E.portal_id \
             \ inner join world_place D on D.id = E.destination_id \
             \ where P.id = ?"
    rs <- pgQ sql $ Only pid
    let getExit (_, _, eid, rn, x, t, did, dn) = W.ExitRec eid rn x t (did, dn)
        (pName, pDesc, _, _, _, _, _, _) {- repeated -} = rs !! 0
        exits                                    = map getExit rs
    liftIO $ sayn $ "pDesc = " ++ show pDesc
    return $ W.PlaceRec pid pName pDesc exits

  getContents pid = pgQ sql (Only pid) >>= f where
    sql = "select T.id, T.name \
         \ from world_located L \
         \ inner join world_thing T on T.id = L.thing_id \
         \ where L.place_id = ?"
    f = return . (map $ uncurry $ W.ThingRec Nothing)

  getOccupants pid = pgQ sql (Only pid) >>= f where
    sql = "select C.thing_ptr_id, T.name, C.is_logged_in as is_awake \
         \ from world_character C \
         \ inner join world_thing T on T.id = C.thing_ptr_id \
         \ inner join world_located L on L.thing_id = T.id \
         \ where L.place_id = ?"
    f = return . (map $ \(i,n,a) -> W.ThingRec a i n)

  setLocation tid pid = do
    let sql = "update world_located set place_id = ? where thing_id = ?"
    n <- pgE sql [pid, tid]
    if n == 1
      then return ()
      else throwE $ InvalidPlaceID pid

{-
  setThing c t = void $ pgE c sql (name t, idn t) where
    sql = "update world_thing set name = ? where id = ?"

-}
