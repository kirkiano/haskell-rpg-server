{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module RPGServer.Drive ( Dr, Drive(..) ) where

import Prelude                      hiding ( length )
import RPGServer.Common
import RPGServer.Util.Text          hiding ( null )
import RPGServer.DB.Error                  ( DBError )
import qualified RPGServer.World           as W


type Dr  = ExceptT DBError


type CID = W.CharacterID

class Monad m => Drive m where
  spawnCharacter  :: W.ThingName     ->     W.PlaceID    ->    Dr m CID
