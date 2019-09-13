{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances #-}

module RPGServer.DB.Postgres.Class ( Conn(..),
                                     ConnectInfo(..) ) where

import RPGServer.Common
-- import RPGServer.Util.Text
-- import qualified Data.ByteString.Base64     as B64
-- import qualified Data.ByteString            as B
-- import qualified Data.ByteString.Internal   as B
import qualified Control.Exception          as E
import Data.Time.Clock.POSIX                ( getCurrentTime )
import Database.PostgreSQL.Simple as PG     ( ConnectInfo(..),
                                              connect,
                                              close,
                                              In(..),
                                              Only(..),
                                              {- query -} )
-- import Database.PostgreSQL.Simple.FromRow   ( FromRow(..), field )
-- import Database.PostgreSQL.Simple.FromField ( FromField )
import qualified RPGServer.Log              as L
import qualified RPGServer.World            as W
-- import qualified Crypto.KDF.PBKDF2          as K
-- import RPGServer.DB.Error                   ( DBError(..) )
import RPGServer.DB.Class                   ( AdminDB(..),
                                              PlayDB(..),
                                              DriverDB(..),
                                              MakeDB(..) )
import RPGServer.DB.Postgres.Common         ( PG,
                                              pgE, pgE1,
                                              pgQ, pgQ1,
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

--
--instance MonadIO m => Auth (PG m) where
--
--  authUser (Credentials uname pw) = do
--    let sql = "select C.thing_ptr_id, U.password \
--             \ from auth_user U \
--             \ left join world_character C on U.id = C.user_id \
--             \ where U.username = ?"
--        pG = encodeUtf8 pw
--        match pS = if B.take 7 pG == "pbkdf2_"
--          then pS == pG
--          else h == hash where
--            [_, _, salt, hash] = B.split (B.c2w '$') pS
--            h = B64.encode $ K.fastPBKDF2_SHA256 m pG salt
--            m = K.Parameters 100000 32
--    db <- asks _db
--    rs <- liftIO $ query db sql $ Only uname
--    return $ case rs of
--      [(cid, pass)] -> toMaybe (match pass) cid
--      _             -> Nothing


instance (MonadIO m, MonadCatch m) => AdminDB (PG m) where
  markLoggedInSet = void . pgE q . Only . In where
    q = "begin;\
       \ update world_character set is_logged_in = false;\
       \ update world_character set is_logged_in = true where thing_ptr_id in ?;\
       \ commit"


instance (MonadIO m, MonadCatch m) => DriverDB (PG m) where
  createCharacter = undefined


instance (MonadIO m, MonadCatch m) => PlayDB (PG m) where

  loginCharacter b cid = pgE1 sql (b, cid) where
    sql = "update world_character set is_logged_in = ? where thing_ptr_id = ?"

  getThing tid = make <$> pgQ1 sql (Only tid) where
    sql         = "select name, description from world_thing where id = ?"
    make (n, d) = W.Thing Nothing tid n $ maybe "" id d

  getTHandle tid = W.THandle tid . fromOnly <$> pgQ1 sql (Only tid) where
    sql = "select name from world_thing where id = ?"

  getThingDescription tid = maybe "" fromOnly <$> pgQ1 sql (Only tid) where
    sql = "select description from world_thing where id = ?"

  getAddress aid = make <$> pgQ1 sql (Only aid) where
    make (an, am, sn, cn, tn) = W.Address aid an am sn cn tn
    sql = "select A.name, A.number, S.name, C.name, T.name \
         \ from address A \
         \ inner join street S on S.id = A.street_id \
         \ inner join city C on C.id = S.city_id \
         \ inner join country T on T.id = C.country_id \
         \ where A.id = ?"

  getCoPlace tid = make <$> pgQ1 sql (Only tid) where
    make (pid, pn, pdM, aiM, anM, smM, snM, cM, tM) =
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

  getCoContentHandles tid = map (uncurry W.THandle) <$> pgQ sql (Only tid) where
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

  setLocation pid tids = pgE1 sql (pid, PG.In tids) where
    sql = "update world_located set place_id = ? where thing_id in ?"

  setUtterance tid speech = pgE1 sql (tid, speech, tid) where
    sql = "insert into world_utterance \
         \ (time_created, speaker_id, place_id, speech) \
         \ select now(), ?, L.place_id, ? \
         \ from world_located L \
         \ where L.thing_id = ?"

  updateThing t = pgE1 sql (W.name t, W.desc t, W.idn t) where
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
