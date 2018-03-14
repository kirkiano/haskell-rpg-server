{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             OverloadedStrings,
             TupleSections #-}

module RPGServer.DB.Redis ( Conn(..),
                            ConnectInfo(..) ) where

import RPGServer.Common
import RPGServer.Util.ByteString
import RPGServer.Util.Text
import qualified RPGServer.Log    as L
import Prelude hiding             ( concat,
                                    lookup )
import Data.Time.Clock.POSIX      ( getPOSIXTime,
                                    POSIXTime )
import Database.Redis             ( Redis,
                                    Reply,
                                    Connection,
                                    ConnectInfo(..),
                                    checkedConnect,
                                    hget,
                                    hgetall,
                                    quit,
                                    runRedis )
import RPGServer.World            ( ThingID )
import RPGServer.DB.Error         ( D,
                                    DBError(..) )
import RPGServer.DB.Class         ( DB(..),
                                    MakeDB(..) )


data Conn = Conn {
  _ctime     :: POSIXTime,
  _conn      :: Connection
}


instance (MonadIO m, L.Log m L.Main) => MakeDB m Conn ConnectInfo where
  connect ci   = L.logDone L.Debug L.AttemptingToConnectToRedis
                 conn
                 L.ConnectedToRedis where
                   conn  = liftIO $ catch c err
                   c     = Conn <$> getPOSIXTime <*> checkedConnect ci
                   err e = do let es = show (e :: SomeException)
                              sayn $ "CRITICAL: NO REDIS CONNECTION: " ++ es
                              throw e

  disconnect c = do liftIO $ void $ runRedis (_conn c) quit
                    L.log L.Debug L.DisconnectedFromRedis

------------------------------------------------------------

type R  m = ReaderT Conn m
type DR m = D (R m)


instance MonadIO m => DB (R m) where

  authUser _ _ = return Nothing

  loginCharacter _ _ = return ()

  getThing tid = fetchAssoc Thing tid >>= parse2obj

{-
{-
  setThing c t = void $ run c (annc >> hmset hkey keys) where
  hkey = key Thing $ idn t
  keys = [("id", i2bs $ idn t), ("name", encodeUtf8 $ name t)]
  annc = liftIO $ putStrLn $ "setting " ++ show hkey ++ " to " ++ show t
-}

  setThing c t = invalidateCache c Thing (showBS $ idn t)

{-
  deleteThing c tid = void $ run c (annc >> del [k]) where
  k    = key Thing tid
  annc = liftIO $ putStrLn ("deleting thing " ++ bs2s k)

  getLocation c tid = fetchAssoc c Thing tid >>= handle where
    handle = maybe (err "No PID") parse . (lookup thingPlaceKey) . fromList
    parse  = maybe (err "Unparseable PID") return . readBSMaybe
    err    = throwE . DataMappingError

  setLocation c tid pid = void $ do
    invalidateCache c Thing $ showBS tid
    invalidateCache c Contents $ showBS pid
    pidOld <- getThingLocation c tid
    invalidateCache c Contents pidOld

  getPlace     = undefined -- fetchAssoc c Place pid >>= parse2obj
  getContents  = undefined -- fetchAssoc c Contents pid >>= parse2obj
  getOccupants = undefined -- fetchAssoc c Occupants pid >>= parse2obj


invalidateCache :: RedisConn -> OType -> ByteString -> D IO ()
invalidateCache c t i = do
  let cname = concat [showBS t, "versions"]
  annc c $ "Incrementing version for " ++ show t ++ " " ++ show i
  void $ run c $ hincrby cname i 1

-}
-}


type BSAssoc = [(ByteString, ByteString)]


fetchAssoc :: MonadIO m => OType -> Int -> DR m BSAssoc
fetchAssoc t = run . hgetall . (key t)


parse2obj :: (Monad m, FromTextMap a) => BSAssoc -> DR m a
parse2obj assc = maybe (err msg) return $ fromTextMap $ bassc2tmap assc where
  err = throwE . DataMappingError
  msg = "Can't parse Thing from Redis: " ++ show assc


data OType = Thing | Place | Contents | Occupants

instance Show OType where
  show Thing     = "thing"
  show Place     = "place"
  show Contents  = "contents"
  show Occupants = "occupants"


thingPlaceKey :: ByteString
thingPlaceKey = "pid" :: ByteString


key :: OType -> Int -> ByteString
key t i = concat [showBS t, showBS i]


getThingLocation :: MonadIO m => ThingID -> DR m ByteString
getThingLocation tid = run cmd >>= maybe err return where
  cmd = hget (key Thing tid) thingPlaceKey
  err = throwE $ DataMappingError "bad thing PID"

------------------------------------------------------------
{-
annc :: L.Log m L.Redis => String -> RR m ()
annc s = liftIO $ log $ "REDIS(" ++ show (_ctime c) ++ "): " ++ s
-}

run :: MonadIO m =>
       Redis (Either Reply a) -> DR m a
run q = do let err = throwE . GeneralError . show
           rc <- lift ask
           either err return =<< liftIO (runRedis (_conn rc) q)
