{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances,
             UndecidableInstances #-}

module RPGServer.Util.Text ( Text,
                             ToText(..),
                             FromTextMap(..),
                             ReadTextMaybe(..),
                             bassc2tmap,
                             empty,
                             intercalate,
                             length,
                             null,
                             pack,
                             strip,
                             unpack,
                             decimal,
                             decodeUtf8,
                             encodeUtf8 ) where

import Prelude hiding            ( null, length )
import Data.Text                 ( Text,
                                   empty,
                                   intercalate,
                                   length,
                                   pack,
                                   strip,
                                   unpack,
                                   null )
import qualified Data.Map        as M
import Data.Text.Read
import Data.Text.Encoding        ( encodeUtf8,
                                   decodeUtf8 )
import RPGServer.Util.ByteString ( ByteString )


bassc2tmap :: [(ByteString, ByteString)] -> M.Map Text Text
bassc2tmap = M.fromList . (map f) where
    f (k, v) = (decodeUtf8 k, decodeUtf8 v)


class FromTextMap a where
  fromTextMap :: M.Map Text Text -> Maybe a


class ReadTextMaybe a where
  readTextMaybe :: Text -> Maybe a


class ToText a where
  text :: a -> Text


instance Show a => ToText a where
  text = pack . show
