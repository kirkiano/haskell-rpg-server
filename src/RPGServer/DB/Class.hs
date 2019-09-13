{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FunctionalDependencies,
             MultiParamTypeClasses #-}

module RPGServer.DB.Class ( AdminDB(..),
                            PlayDB(..),
                            MakeDB(..),
                            DriverDB(..) ) where

import Prelude                     hiding ( length )
import RPGServer.Util.Text         hiding ( null )
import qualified RPGServer.World          as W
import RPGServer.DB.Error                 ( D )


class Monad m => MakeDB m d p | d -> p where
  connect    :: p -> m d
  disconnect :: d -> m ()


class Monad m => AdminDB m where
  markLoggedInSet :: [W.CharacterID] -> D m ()


class Monad m => PlayDB m where
  loginCharacter       :: Bool          -> W.CharacterID -> D m ()
  getThing             :: W.ThingID                      -> D m W.Thing
  getTHandle           :: W.ThingID                      -> D m W.THandle
  getThingDescription  :: W.ThingID                      -> D m Text
  getCoPlace           :: W.ThingID                      -> D m W.Place
  getCoExits           :: W.ThingID                      -> D m [W.Exit]
  getAddress           :: W.AddressID                    -> D m W.Address
  getCoOccupantIDs     :: W.ThingID                      -> D m [W.CharacterID]
  getCoContentHandles  :: W.ThingID                      -> D m [W.THandle]

  setLocation          :: W.PlaceID     -> [W.ThingID]   -> D m ()
  setUtterance         :: W.CharacterID -> Text          -> D m ()
  updateThing          :: W.Thing                        -> D m ()


class PlayDB m => DriverDB m where
  createCharacter      :: W.ThingName   -> W.PlaceID     -> D m W.CharacterID
