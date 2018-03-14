{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.World.Place ( Direction,
                               ExitID,
                               ExitRec(..),
                               ExitName,
                               Place(..),
                               PlaceID,
                               PlaceName,
                               PlaceRec(PlaceRec),
                               Preposition ) where

import Data.Text              ( Text )
import Data.Aeson             ( (.=),
                                object,
                                ToJSON(..) )
import RPGServer.World.Common ( ID(..),
                                Name(..) )

type Direction   = Text
type ExitID      = Int
type ExitName    = Text
type PlaceID     = Int
type PlaceName   = Text
type Preposition = Text


data ExitRec = ExitRec {
  exitID            :: ExitID,
  exitName          :: ExitName,
  exitDirection     :: Direction,
  isExitTransparent :: Bool,
  exitDestination   :: (PlaceID, PlaceName)
} deriving Show


instance ToJSON ExitRec where
  toJSON e = object [
    "eid"   .= exitID e,
    "name"  .= exitName e,
    "dir"   .= exitDirection e,
    "trans" .= isExitTransparent e,
    "nbr"   .= exitDestination e
    ]

------------------------------------------------------------

class (ID a, Name a) => Place a where
  exits :: a -> [ExitRec]

------------------------------------------------------------

data PlaceRec = PlaceRec {
  placeId    :: PlaceID,
  placeName  :: PlaceName,
  placeExits :: [ExitRec]
} deriving Show

instance ID PlaceRec where
  idn = placeId

instance Name PlaceRec where
  name = placeName

instance Place PlaceRec where
  exits = placeExits

instance ToJSON PlaceRec where
  toJSON p = object [
    "pid"   .= idn p,
    "name"  .= name p,
    "exits" .= exits p
    ]
