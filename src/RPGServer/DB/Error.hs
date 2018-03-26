{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.DB.Error ( D, DBError(..) ) where

import Control.Monad.Trans.Except ( ExceptT )
import Data.Text                  ( Text )
import Data.Aeson                 ( (.=),
                                    object,
                                    ToJSON(..) )
import RPGServer.World            ( CharacterID,
                                    PlaceID,
                                    ExitID,
                                    ThingID )

type D = ExceptT DBError

data DBError = InvalidThingID ThingID
             | InvalidPlaceID PlaceID
             | InvalidCharacterIDs [CharacterID]
             | InvalidExitID ExitID
             | DataMappingError String
             | GeneralError String
             | CannotConnect String
             | Unimplemented String
             deriving Show


instance ToJSON DBError where

  toJSON (InvalidThingID tid) = object [
    "type"  .= ("invalidThingID" :: Text),
    "value" .= tid]

  toJSON (InvalidPlaceID pid) = object [
    "type"  .= ("invalidPlaceID" :: Text),
    "value" .= pid]

  toJSON (InvalidCharacterIDs cids) = object [
    "type"  .= ("invalidCharacterIDs" :: Text),
    "value" .= cids]

  toJSON (InvalidExitID ptid) = object [
    "type"  .= ("invalidExitID" :: Text),
    "value" .= ptid]

  toJSON (DataMappingError e) = object [
    "type"  .= ("dataMappingError" :: Text),
    "value" .= e]

  toJSON (GeneralError e) = object [
    "type"  .= ("generalError" :: Text),
    "value" .= e]

  toJSON (CannotConnect e) = object [
    "type"  .= ("cannotConnect" :: Text),
    "value" .= e]

  toJSON (Unimplemented msg) = object [
    "type"  .= ("unimplmeneted" :: Text),
    "value" .= msg]
