{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.World.Thing ( Character,
                               CharacterID,
                               Thing,
                               ThingID,
                               ThingName,
                               ThingRec(..) ) where

import RPGServer.Common
import Prelude hiding                ( lookup )
import Data.Aeson                    ( (.=),
                                       object,
                                       ToJSON(..) )
import Data.Map                      ( lookup )
import qualified RPGServer.Util.Text as T
import RPGServer.World.Common        ( ID(..),
                                       Name(..) )

type CharacterID = Int
type ThingID     = Int
type ThingName   = T.Text


class (ID a, Name a) => Thing a

class Thing a => Character a


data ThingRec = ThingRec {
  isAwake   :: Maybe Bool,
  thingId   :: ThingID,
  thingName :: ThingName
} deriving Show


instance Eq ThingRec where
  (==) = (==) `on` id

instance ID ThingRec where
  idn  = thingId

instance Name ThingRec where
  name = thingName

instance ToJSON ThingRec where
  toJSON t = object [
    "type"  .= ("thing" :: T.Text),
    "value" .= object ["id"    .= idn t,
                       "name"  .= name t,
                       "awake" .= isAwake t]]

instance T.FromTextMap ThingRec where
  fromTextMap tm = do
    is <- lookup "id" tm
    case (T.decimal is :: Either String (ThingID, T.Text)) of
      Right (i, s) ->
        if not $ T.null s
        then Nothing
        else ThingRec a i <$> lookup "name" tm where
          a = fmap (== "t") $ lookup "is_awake" tm
      _ -> Nothing
