{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module RPGServer.Event ( Event(..) ) where

import Data.Text                               ( Text )
import Data.Aeson                              ( (.=),
                                                 object,
                                                 ToJSON(..) )
import qualified RPGServer.World               as W


data Event = Joined      W.THandle
           | ThingEdited W.ThingID
           | Looked      W.ThingID W.ThingID
           | Said        W.ThingID Text
           | Whispered   W.ThingID W.ThingID (Maybe Text)
           | Entered     W.ExitName W.THandle W.PlaceName W.Direction
           | Exited      W.ExitID   W.ThingID
           | Disjoined   W.ThingID -- a player has quit the game. Use
                                   -- 'Disjoined' because 'Quit' is one of
                                   -- the Requests.
           deriving Show

instance ToJSON Event where

  toJSON (ThingEdited tid) = object [
    "type"  .= ("thingedited" :: Text),
    "value" .= tid]

  toJSON (Looked t1 t2) = object [
    "type"  .= ("looked" :: Text),
    "value" .= object ["looker" .= t1,
                       "lookee" .= t2]]

  toJSON (Said tid s) = object [
    "type"  .= ("said" :: Text),
    "value" .= object ["speaker" .= tid,
                       "speech"  .= s]]

  toJSON (Whispered from to sM) = object [
    "type"  .= ("whispered" :: Text),
    "value" .= object ["from"   .= from,
                       "to"     .= to,
                       "speech" .= sM]]

  toJSON (Entered eName th nbrName dxn) = object [
    "type"  .= ("entered" :: Text),
    "value" .= object ["tid"   .= W.idn th,
                       "tname" .= W.name th,
                       "ename" .= eName,
                       "nbr"   .= nbrName,
                       "dxn"   .= dxn]]

  toJSON (Exited eid tid) = object [
    "type"  .= ("exited" :: Text),
    "value" .= object ["eid" .= eid,
                       "tid" .= tid]]

  toJSON (Joined t) = object [
    "type"  .= ("joined" :: Text),
    "value" .= t]

  toJSON (Disjoined tid) = object [
    "type"  .= ("disjoined" :: Text),
    "value" .= tid]
