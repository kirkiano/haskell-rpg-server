{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}


module RPGServer.Listen.Auth ( Auth(..),
                               Username,
                               Password,
                               Credentials(..) ) where

import GHC.Generics
import Data.Aeson                ( FromJSON )
import RPGServer.Util.Text       ( Text )
import qualified RPGServer.World as W

type Username = Text
type Password = Text

data Credentials = Credentials Username Password
                 deriving (Generic, Show)

instance FromJSON Credentials


-- used by the forwarder watcher
class Monad m => Auth m where
  authUser :: Credentials -> m (Maybe W.CharacterID)
