{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Request ( Request(..), isQuit ) where

import GHC.Generics
import Data.Text                               ( Text )
import Data.Aeson                              ( (.:),
                                                 FromJSON(..),
                                                 withObject )
import RPGServer.Listen.Auth                   ( Credentials(Credentials) )
import qualified RPGServer.World               as W


data Request = Login Credentials
             | WhoAmI
             | WhereAmI
             | WhatIsHere
             | WaysOut
             | EditMe W.ThingDesc
             | DescribeThing W.ThingID
             | Exit W.ExitID
             | Say     Text
             | Whisper Text W.CharacterID
             | Join -- sent internally only, so don't FromJSON it
             | Quit
             deriving (Show, Generic)


isQuit :: Request -> Bool
isQuit Quit = True
isQuit _    = False


instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> do
    t <- o .: "type"
    case t of
      "login"             -> do credsJ <- o .: "creds"
                                user   <- credsJ .: "user"
                                pass   <- credsJ .: "pass"
                                return $ Login $ Credentials user pass
      "whoami"            -> return WhoAmI
      "whereami"          -> return WhereAmI
      "whatishere"        -> return WhatIsHere
      "waysout"           -> return WaysOut
      "describething"     -> DescribeThing <$> o .: "tid"
      "editme"            -> EditMe <$> o .: "desc"
      "exit"              -> Exit <$> o .: "eid"
      "say"               -> Say <$> o .: "value"
      "whisper"           -> Whisper <$> o .: "value" <*> o .: "to"
      "quit"              -> return Quit
      _                   -> fail $ "Unknown type '" ++ t ++ "'"
