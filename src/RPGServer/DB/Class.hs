{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies #-}

module RPGServer.DB.Class ( D,
                            DB(..),
                            MakeDB(..) ) where

import qualified RPGServer.World               as W
import RPGServer.DB.Error                      ( D )
import qualified RPGServer.Listen.Auth.Message as A


class Monad m => MakeDB m d p | d -> p where
  connect    :: p -> m d
  disconnect :: d -> m ()


class Monad m => DB m where
  authUser        :: A.Username -> A.Password    -> m (Maybe W.CharacterID)
  loginCharacter  :: Bool       -> W.CharacterID -> m ()
  getThing        :: W.ThingID  ->                  D m W.ThingRec
  getLocation     :: W.ThingID  ->                  D m W.PlaceID
  getPlace        :: W.PlaceID  ->                  D m W.PlaceRec
  getOccupants    :: W.PlaceID  ->                  D m [W.ThingRec]
  setLocation     :: W.ThingID  -> W.PlaceID     -> D m ()
  getContents     :: W.PlaceID  ->                  D m [W.ThingRec]
{-
  setThing        :: W.ThingRec -> D m ()
-}
