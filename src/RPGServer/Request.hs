{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Request ( Request(..) ) where

import GHC.Generics
import Data.Text                               ( Text )
import Data.Aeson                              ( (.:),
                                                 FromJSON(..),
                                                 withObject )
import RPGServer.World                         ( ExitID )
import qualified RPGServer.Listen.Auth.Message as A


data Request = Login A.Credentials
             | WhoAmI
             | WhereAmI
             | WhatIsHere
             | Exit ExitID
             | Say Text
             | Join -- sent internally only, so don't FromJSON it
             | Quit
             deriving (Show, Generic)


instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    t <- o .: "type"
    case t of
      "login"          -> Login <$> o .: "creds"
      "whoami"         -> return WhoAmI
      "whereami"       -> return WhereAmI
      "whatishere"     -> return WhatIsHere
      "exit"           -> Exit <$> o .: "eid"
      "say"            -> Say <$> o .: "value"
      "quit"           -> return Quit
      _                -> fail $ "Unknown type '" ++ t ++ "'"
