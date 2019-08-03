{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Value ( Value(..) ) where

import Data.Text                               ( Text )
import Data.Aeson                              ( (.=),
                                                 object,
                                                 ToJSON(..) )
import RPGServer.DB.Error                      ( DBError )
import qualified RPGServer.World               as W


data Value = YouAre W.Thing
           | ThingDescription W.ThingID Text
           | Place W.Place
           | Exits [W.Exit]
           | PlaceContents [W.THandle]
           | Error DBError
           deriving Show


instance ToJSON Value where

  toJSON (YouAre t) = object [
    "type"  .= ("youare" :: Text),
    "value" .= t]

  toJSON (ThingDescription tid dsc) = object [
    "type"  .= ("thingdescription" :: Text),
    "value" .= object ["tid" .= tid,
                       "dsc" .= dsc]]

  toJSON (Place place) = object [
    "type"  .= ("place" :: Text),
    "value" .= place]

  toJSON (Exits es) = object [
    "type"  .= ("exits" :: Text),
    "value" .= es]

  toJSON (PlaceContents tHandles) = object [
    "type"  .= ("placecontents" :: Text),
    "value" .= tHandles]

  toJSON (Error e) = object [
    "type"  .= ("error" :: Text),
    "value" .= e]
