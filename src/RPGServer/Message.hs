{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Message ( Message(..) ) where

import Data.Text                               ( Text )
import Data.Aeson                              ( (.=),
                                                 object,
                                                 ToJSON(..) )
import RPGServer.DB.Error                      ( DBError )
import qualified RPGServer.World               as W
import qualified RPGServer.Listen.Auth.Message as A


data Message = Auth A.Response
             | Joined W.ThingRec
             | YouAre W.ThingRec
             | Place W.PlaceRec
             | PlaceContents [W.ThingRec]
             | Said W.ThingRec Text
             | EnteredFrom W.ThingRec W.PlaceName
             | ExitedTo    W.ThingRec W.PlaceName
             | Error DBError
             | Disjoined W.ThingRec
              deriving Show


instance ToJSON Message where

  toJSON (Auth r) = object [
    "type"  .= ("auth" :: Text),
    "value" .= r]

  toJSON (YouAre t) = object [
    "type"  .= ("youare" :: Text),
    "value" .= t]

  toJSON (Place place) = object [
    "type"  .= ("place" :: Text),
    "value" .= place]

  toJSON (PlaceContents contents) = object [
    "type"  .= ("placecontents" :: Text),
    "value" .= contents]

  toJSON (Said t s) = object [
    "type"  .= ("said" :: Text),
    "value" .= object ["speaker" .= W.name t,
                       "speech"  .= s]]

  toJSON (EnteredFrom t p) = object [
    "type"  .= ("enteredfrom" :: Text),
    "value" .= object ["thing" .= t,
                       "place" .= p]]

  toJSON (ExitedTo t p) = object [
    "type"  .= ("exitedto" :: Text),
    "value" .= object ["thing" .= t,
                       "place" .= p]]

  toJSON (Error e) = object [
    "type"  .= ("error" :: Text),
    "value" .= e]

  toJSON (Joined t) = object [
    "type"  .= ("joined" :: Text),
    "value" .= t]

  toJSON (Disjoined tid) = object [
    "type"  .= ("disjoined" :: Text),
    "value" .= tid]
