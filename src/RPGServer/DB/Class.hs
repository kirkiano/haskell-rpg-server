{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies #-}

module RPGServer.DB.Class ( D,
                            AuthDB(..),
                            AdminDB(..),
                            PlayDB(..),
                            MakeDB(..) ) where

import RPGServer.Util.Text
import qualified RPGServer.World               as W
import RPGServer.DB.Error                      ( D )
import qualified RPGServer.Listen.Auth.Message as A


class Monad m => MakeDB m d p | d -> p where
  connect    :: p -> m d
  disconnect :: d -> m ()


class Monad m => AuthDB m where
  authUser        :: A.Username -> A.Password    -> m (Maybe W.CharacterID)
  loginCharacter  :: Bool       -> W.CharacterID -> m ()


class Monad m => AdminDB m where
  markLoggedInSet :: [W.CharacterID] -> D m Integer


class Monad m => PlayDB m where
  getThing        :: W.ThingID  ->                  D m W.ThingRec
  getLocation     :: W.ThingID  ->                  D m W.PlaceID
  getPlace        :: W.PlaceID  ->                  D m W.PlaceRec
  getOccupants    :: W.PlaceID  ->                  D m [W.ThingRec]
  setLocation     :: W.ThingID  -> W.PlaceID     -> D m ()
  getContents     :: W.PlaceID  ->                  D m [W.ThingRec]
  saveUtterance   :: W.ThingID  -> Text          -> D m ()
{-
  setThing        :: W.ThingRec -> D m ()
-}
