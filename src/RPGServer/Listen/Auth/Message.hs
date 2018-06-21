{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Listen.Auth.Message ( Credentials(..),
                                       Response(..),
                                       Username,
                                       Password ) where

import GHC.Generics
import RPGServer.Util.Text
import qualified Data.Aeson as J
import RPGServer.World.Thing     ( CharacterID )


type Username = Text
type Password = Text


data Credentials = Credentials {
  user :: Username,
  pass :: Password
} deriving (Generic, Show)  -- TODO: hide password

instance J.FromJSON Credentials

------------------------------------------------------------

data Response = Welcome CharacterID
              | BadCredentials
              | AlreadyLoggedIn
              deriving (Generic, Show)

instance J.ToJSON Response where
  toEncoding = J.genericToEncoding J.defaultOptions
