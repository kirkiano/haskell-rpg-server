{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module RPGServer.Drive ( Dr, Drive(..) ) where

import Prelude                      hiding ( length )
import Control.Monad.Trans.Except          ( ExceptT )
import RPGServer.DB.Error                  ( DBError )
import qualified RPGServer.World           as W


type Dr  = ExceptT DBError


class Monad m => Drive m where
  spawnCharacter  :: W.ThingName     ->     W.PlaceID    ->    Dr m W.CID
  deleteCharacter :: W.CID                               ->    Dr m ()
