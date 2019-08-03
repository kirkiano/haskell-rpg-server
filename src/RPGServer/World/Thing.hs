{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.World.Thing ( CharacterID,
                               Thing(Thing),
                               THandle(THandle),
                               ThingID,
                               ThingName,
                               ThingDesc ) where

import RPGServer.Common
import Prelude hiding                ( lookup )
import Data.Aeson                    ( (.=),
                                       object,
                                       ToJSON(..) )
import qualified RPGServer.Util.Text as T
import RPGServer.World.Common        ( ID(..)
                                     , Name(..)
                                     , Desc(..) )

type CharacterID = Int
type ThingID     = Int
type ThingName   = T.Text
type ThingDesc   = T.Text

-----------------------------------------------------------
-- Thing handle

data THandle = THandle ThingID ThingName
             deriving Show

instance Eq THandle where
  (==) = (==) `on` id

instance ID THandle where
  idn (THandle i _) = i

instance Name THandle where
  name (THandle _ n) = n

instance ToJSON THandle where
  toJSON t = object [
    "type"  .= ("THandle" :: T.Text),
    "value" .= object ["id"    .= idn t,
                       "name"  .= name t]]

-----------------------------------------------------------
-- Thing

data Thing = Thing {
  isAwake   :: Maybe Bool,
  thingId   :: ThingID,
  thingName :: ThingName,
  thingDesc :: ThingDesc
} deriving Show


instance Eq Thing where
  (==) = (==) `on` id

instance ID Thing where
  idn  = thingId

instance Name Thing where
  name = thingName

instance Desc Thing where
  desc = thingDesc

instance ToJSON Thing where
  toJSON t = object [
    "type"  .= ("thing" :: T.Text),
    "value" .= object ["id"    .= idn t,
                       "name"  .= name t,
                       "desc"  .= desc t,
                       "awake" .= isAwake t]]
