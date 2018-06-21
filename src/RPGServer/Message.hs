{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Message ( Message(..) ) where

import Data.Text                               ( Text )
import Data.Aeson                              ( (.=),
                                                 object,
                                                 ToJSON(..) )
import RPGServer.World                         ( PlaceRec,
                                                 PlaceName,
                                                 ThingID,
                                                 ThingName,
                                                 ThingRec(..) )
import RPGServer.DB.Error                      ( DBError )
import qualified RPGServer.Listen.Auth.Message as A


data Message = Auth A.Response
             | Joined ThingRec
             | YouAre ThingRec
             | Place PlaceRec
             | PlaceContents [ThingRec]
             | Said ThingName Text
             | EnteredFrom ThingRec PlaceName
             | ExitedTo ThingID PlaceName
             | Error DBError
             | Disjoined ThingID
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

  toJSON (Said tn s) = object [
    "type"  .= ("said" :: Text),
    "value" .= object ["speaker" .= tn,
                       "speech"  .= s]]

  toJSON (EnteredFrom t p) = object [
    "type"  .= ("enteredfrom" :: Text),
    "value" .= object ["thing" .= t,
                       "place" .= p]]

  toJSON (ExitedTo tid p) = object [
    "type"  .= ("exitedto" :: Text),
    "value" .= object ["tid"   .= tid,
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
