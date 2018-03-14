{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.Error ( R, RPGError(..) ) where

import Data.Text                  ( Text )
import Control.Monad.Trans.Except ( ExceptT(..) )
import Data.Aeson                 ( (.=),
                                    object,
                                    ToJSON(..) )
import RPGServer.DB.Error         ( DBError(..) )


data RPGError = DBError DBError
              deriving Show

type R = ExceptT RPGError

instance ToJSON RPGError where
  toJSON (DBError e) = object [
    "type" .= ("dbError" :: Text),
    "value" .= toJSON e]
