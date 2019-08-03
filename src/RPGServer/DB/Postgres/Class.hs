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
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString            as B
import qualified Data.ByteString.Internal   as B
import qualified Control.Exception          as E
import Data.Time.Clock.POSIX                ( getCurrentTime )
import Database.PostgreSQL.Simple as PG     ( ConnectInfo(..),
                                              connect,
                                              close,
                                              In(..),
                                              Only(..),
                                              query )
import Database.PostgreSQL.Simple.FromRow   ( FromRow(..), field )
import Database.PostgreSQL.Simple.FromField ( FromField )
import qualified RPGServer.Log              as L
import qualified RPGServer.World            as W
import qualified Crypto.KDF.PBKDF2          as K
import RPGServer.Listen.Auth                ( Auth(..), Credentials(..) )
import RPGServer.DB.Error                   ( DBError(..) )
import RPGServer.DB.Class                   ( AdminDB(..),
                                              PlayDB(..),
                                              MakeDB(..) )
import RPGServer.DB.Postgres.Common         ( P,
                                              pgE,
                                              pgQ,
                                              Conn(..) )

-- needed to make getCoExits work below.
instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n) =>
    FromRow (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    fromRow = (,,,,,,,,,,,,,) <$> field <*> field <*> field <*> field <*> field
                              <*> field <*> field <*> field <*> field <*> field
                              <*> field <*> field <*> field <*> field


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


instance MonadIO m => Auth (P m) where

  authUser (Credentials uname pw) = do
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


-- TODO: This query marks a character as logged out and then as logged in.
-- Check that it is really supposed to do so.
instance MonadIO m => AdminDB (P m) where
  markLoggedInSet = pgE sql . Only . In where
    sql = "begin; \
         \ update world_character set is_logged_in = false; \
         \ update world_character set is_logged_in = true \
         \ where thing_ptr_id in ?; \
         \ commit"


instance MonadIO m => PlayDB (P m) where

  loginCharacter b cid = void $ pgE sql (b, cid) where
    sql = "update world_character set is_logged_in = ? where thing_ptr_id = ?"

  getThing tid = pgQ sql (Only tid) >>= f where
    sql        = "select name, description from world_thing where id = ?"
    f [(n, d)] = return $ W.Thing Nothing tid n $ maybe "" id d
    f _        = throwE $ InvalidThingID tid

  getTHandle tid = pgQ sql (Only tid) >>= f where
    sql          = "select name from world_thing where id = ?"
    f [(Only n)] = return $ W.THandle tid n
    f _          = throwE $ InvalidThingID tid

  getThingDescription tid = pgQ sql (Only tid) >>= f where
    sql          = "select description from world_thing where id = ?"
    f [(Only d)] = return $ maybe "" id d
    f _          = throwE $ InvalidThingID tid

  getAddress aid = makeAddress . (!! 0) <$> pgQ sql (Only aid) where
    makeAddress (an, am, sn, cn, tn) = W.Address aid an am sn cn tn
    sql = "select A.name, A.number, S.name, C.name, T.name \
         \ from address A \
         \ inner join street S on S.id = A.street_id \
         \ inner join city C on C.id = S.city_id \
         \ inner join country T on T.id = C.country_id \
         \ where A.id = ?"

  getCoPlace tid = makePlace . (!! 0) <$> pgQ sql (Only tid) where
    makePlace (pid, pn, pdM, aiM, anM, smM, snM, cM, tM) =
      W.Place pid pn (maybe "" id pdM) $ tryAddress aiM anM smM snM cM tM
    sql  = "select P.id, \
          \        P.name, \
          \        P.description, \
          \        A.id, \
          \        A.name, \
          \        A.number, \
          \        S.name, \
          \        C.name, \
          \        T.name \
          \ from world_place         P \
          \ inner join world_located L on    ? = L.thing_id \
          \                           and P.id = L.place_id \
          \  left join address       A on A.id = P.address_id \
          \  left join street        S on S.id = A.street_id \
          \  left join city          C on C.id = S.city_id \
          \  left join country       T on T.id = C.country_id"

  getCoExits tid = map makeExit <$> pgQ sql (Only tid) where
    makeExit (eid, en, si, sn, x, t, di, dn, aiM, anM, smM, snM, cM, tM) =
      W.Exit eid en (si, sn) (di, dn, aM) x t where
        aM = tryAddress aiM anM smM snM cM tM
    sql = "select E.id, \
         \        R.name, \
         \        S.id, \
         \        S.name, \
         \        X.name, \
         \        R.trans, \
         \        D.id, \
         \        D.name, \
         \        DA.id, \
         \        DA.name, \
         \        DA.number, \
         \        DS.name, \
         \        DC.name, \
         \        DT.name \
         \ from world_exit            E \
         \ inner join world_located   L  on L.place_id = E.source_id \
         \                              and ?          = L.thing_id \
         \ inner join world_place     S  on S.id       = E.source_id \
         \ inner join world_direction X  on X.id       = E.direction_id \
         \ inner join world_portal    R  on R.id       = E.portal_id \
         \ inner join world_place     D  on D.id       = E.destination_id \
         \  left join address         DA on DA.id      = D.address_id \
         \  left join street          DS on DS.id      = DA.street_id \
         \  left join city            DC on DC.id      = DS.city_id \
         \  left join country         DT on DT.id      = DC.country_id"

  getCoContentHandles tid = map makeTHandle <$> pgQ sql (Only tid) where
    makeTHandle = uncurry W.THandle
    sql = "select T.id, T.name \
         \ from world_thing         T \
         \ inner join world_located LT on T.id       = LT.thing_id \
         \ inner join world_located L  on L.place_id = LT.place_id \
         \                            and L.thing_id = ?"

  getCoOccupantIDs tid = map fromOnly <$> pgQ sql (Only tid) where
    sql = "select L.thing_id \
         \ from world_located L \
         \ inner join world_located LT on LT.place_id = L.place_id \
         \                            and LT.thing_id = ?"

  setLocation pid tids = void $ pgE sql (pid, PG.In tids) where
    sql = "update world_located set place_id = ? where thing_id in ?"

  setUtterance tid speech = void $ pgE sql (tid, speech, tid) where
    sql = "insert into world_utterance \
         \ (time_created, speaker_id, place_id, speech) \
         \ select now(), ?, L.place_id, ? \
         \ from world_located L \
         \ where L.thing_id = ?"

  updateThing t = void $ pgE sql (W.name t, W.desc t, W.idn t) where
    sql = "update world_thing set name = ?, description = ? where id = ?"


tryAddress :: Maybe W.AddressID ->
              Maybe W.AddressName ->
              Maybe W.StreetNumber ->
              Maybe W.StreetName ->
              Maybe W.CityName ->
              Maybe W.CountryName ->
              Maybe W.Address
tryAddress iM anM smM snM cM tM = fmap (\i -> W.Address i an sm sn c t) iM where
  -- the defaults ("" and 0) will never be used, as aiM guards against it.
  an  = maybe "" id anM
  sm  = maybe  0 id smM
  sn  = maybe "" id snM
  c   = maybe "" id  cM
  t   = maybe "" id  tM
