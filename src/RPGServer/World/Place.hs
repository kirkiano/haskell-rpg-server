{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses #-}

module RPGServer.World.Place ( Address(Address),
                               AddressID,
                               AddressName,
                               CityName,
                               CountryName,
                               Direction,
                               ExitID,
                               Exit(Exit,
                                    exitSource,
                                    exitDestination,
                                    exitDirection,
                                    isExitTransparent),
                               ExitName,
                               Place(Place, address),
                               PlaceID,
                               PlaceName,
                               Preposition,
                               StreetNumber,
                               StreetName ) where

import Data.Aeson             ( (.=),
                                object,
                                ToJSON(..) )
import RPGServer.World.Common ( Desc(..),
                                ID(..),
                                Name(..), )
import RPGServer.Util.Text    ( Text )


------------------------------------------------------------
-- address

type AddressID    = Int
type AddressName  = Text
type StreetNumber = Int
type StreetName   = Text
type CityName     = Text
type CountryName  = Text

data Address = Address AddressID AddressName
                  StreetNumber StreetName CityName CountryName
             deriving Show

instance ID Address where
  idn (Address i _ _ _ _ _) = i

instance Name Address where
  name (Address _ n _ _ _ _) = n

instance ToJSON Address where
  toJSON (Address i n m s c t) = object [
    "id"      .= i,
    "name"    .= n,
    "number"  .= m,
    "street"  .= s,
    "city"    .= c,
    "country" .= t
    ]

------------------------------------------------------------
-- place

type PlaceID          = Int
type PlaceName        = Text
type PlaceDescription = Text
type Preposition      = Text

data Place = Place {
  placeID      :: PlaceID,
  placeName    :: PlaceName,
  placeDesc    :: PlaceDescription,
  address      :: Maybe Address
} deriving Show

instance ID Place where
  idn = placeID

instance Name Place where
  name = placeName

instance Desc Place where
  desc = placeDesc

instance ToJSON Place where
  toJSON p = object [
    "pid"   .= idn p,
    "name"  .= name p,
    "desc"  .= placeDesc p,
    "addr"  .= address p
    ]

------------------------------------------------------------
-- exits

type Direction    = Text -- TODO: make this strongly typed, and define opposites
type ExitID       = Int
type ExitName     = Text

data Exit = Exit {
  exitID            :: ExitID,
  exitName          :: ExitName,
  exitSource        :: (PlaceID, PlaceName),
  exitDestination   :: (PlaceID, PlaceName, Maybe Address),
  exitDirection     :: Direction,
  isExitTransparent :: Bool
} deriving Show


instance ToJSON Exit where
  toJSON e = object [
    "eid"   .= exitID e,
    "name"  .= exitName e,
    "src"   .= exitSource e,
    "dst"   .= if t then (Just $ exitDestination e) else Nothing,
    "dir"   .= exitDirection e,
    "trans" .= t
    ]
    where t = isExitTransparent e

instance ID Exit where
  idn = exitID

instance Name Exit where
  name = exitName