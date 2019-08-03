{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module RPGServer.Message ( Message(..), Challenge(..) ) where

import GHC.Generics
import Data.Aeson                 ( ToJSON(..),
                                    genericToEncoding,
                                    defaultOptions )
import RPGServer.World            ( CharacterID )
import RPGServer.Value            ( Value )
import RPGServer.Event            ( Event )

-----------------------------------------------------------
-- challenge

data Challenge = SendCredentials
               | BadCredentials
               | Welcome CharacterID
               deriving (Generic, Show)

instance ToJSON Challenge where
  toEncoding = genericToEncoding defaultOptions


-----------------------------------------------------------
-- server message

data Message = ValueMessage Value
             | EventMessage Event
             | ChallengeMessage Challenge
             deriving Show

instance ToJSON Message where
  toJSON (ValueMessage   val) = toJSON val
  toJSON (EventMessage   evt) = toJSON evt
  toJSON (ChallengeMessage c) = toJSON c
