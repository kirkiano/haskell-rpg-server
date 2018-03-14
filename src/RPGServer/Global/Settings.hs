{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections,
             MultiParamTypeClasses,
             FlexibleInstances #-}

module RPGServer.Global.Settings ( Settings(..),
                                   getSettings ) where

import RPGServer.Common
import Control.Monad.Trans.Reader           ( ReaderT,
                                              asks,
                                              runReaderT )
import Control.Monad.IO.Class               ( MonadIO )
import Data.Time.Clock                      ( NominalDiffTime )
import System.IO                            ( stdout )
import qualified Options.Applicative        as O
import Data.Semigroup                       ((<>))
import qualified System.Log                 as L
import qualified Bouncer                    as B
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis             as Redis


data Settings = Settings {
  logThresh         :: L.Level,

  tcpPort           :: Int,
  websockPort       :: Int,
  
  connTries         :: B.NumTriesLeft, -- max allowed number of login attempts
  connTimeout       :: NominalDiffTime,
  
  pgSettings        :: PG.ConnectInfo,
  redisSettings     :: Redis.ConnectInfo,

  dbPoolNStripes    :: Int,
  dbPoolNPerStripe  :: Int,
  dbPoolMaxIdleTime :: NominalDiffTime -- in seconds
  }


instance Show Settings where
  show s = intercalate "\t\n" $ map f [
    ("log threshold",                     show $ logThresh s),
    ("tcp port",                          show $ tcpPort s),
    ("websocket port",                    show $ websockPort s),
    ("max login attempts per connection", show $ connTries s),
    ("max time to login (seconds)",       show $ connTimeout s),
    ("postgres settings",                 show $ pgSettings s),
    ("redis settings",                    show $ redisSettings s),
    ("number of db pool stripes",         show $ dbPoolNStripes s),
    ("number of db resources per stripe", show $ dbPoolNPerStripe s),
    ("max idle time for db resource",     show $ dbPoolMaxIdleTime s)
    ]
    where f (k, v) = k ++ ": " ++ v

------------------------------------------------------------

data Subsettings = Subsettings {
  debug       :: Bool,
  _tcpPort    :: Int
}


getSubsettings :: O.Parser Subsettings
getSubsettings = Subsettings
                 <$> O.switch
                 ( O.long "debug"
                   <> O.short 'd'
                   <> O.help "run in debug mode, generating more output" )
                 <*> O.option O.auto
                 ( O.long "tcpport"
                   <> O.short 'p'
                   <> O.help "port number of TCP listener"
                   <> O.value 11237
                   <> O.showDefault
                   <> O.metavar "INT" )


getSettings :: IO Settings
getSettings = O.execParser opts >>= ss2s where
  opts = O.info (getSubsettings O.<**> O.helper)
         ( O.fullDesc
           <> O.progDesc "RPG Server"
           <> O.header "rpg - a text-based RPG" )


ss2s :: Subsettings -> IO Settings
ss2s s = return $ Settings {
  logThresh         = if (debug s) then L.Debug else L.Info,

  tcpPort           = _tcpPort s,
  websockPort       = _tcpPort s + 1000,

  connTries         = 3,
  connTimeout       = 300,
                 
  pgSettings        = pg,
  redisSettings     = redis,

  dbPoolNStripes    = 1,
  dbPoolNPerStripe  = 20,
  dbPoolMaxIdleTime = 10
}
  where pg    = PG.ConnectInfo "localhost" 5432 "rpg" "" "rpg"
        redis = Redis.ConnInfo "localhost" (Redis.PortNumber 6379)
                Nothing 1 1 60 Nothing

------------------------------------------------------------

instance MonadIO m => L.LogThreshold (ReaderT Settings m) where
  logThreshold = asks logThresh

instance (MonadIO m, Show t) => L.Log (ReaderT Settings m) t where
  logWrite lev msg = do
    runReaderT (L.logWrite lev $ show msg) . (,stdout) =<< L.logThreshold
