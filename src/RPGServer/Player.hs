{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.Player ( Player(Player) ) where

import RPGServer.Common
import Prelude hiding                   ( getContents )
import RPGServer.Log                    as L
import RPGServer.World                  ( CharacterID )
import RPGServer.Play                   ( HasCID(..) )
import RPGServer.DB.Class               ( PlayDB(..), DriverDB(..) )


newtype Player = Player { _cid :: CharacterID }

type PS = ReaderT Player -- player state

instance Monad m => HasCID (PS m) where
  getCID = asks _cid

instance (Monad m, L.LogThreshold m) => L.LogThreshold (PS m) where
  logThreshold = lift L.logThreshold

instance L.Log m L.Game => L.Log (PS m) L.Game where
  logWrite lev = lift . (L.logWrite lev)

-----------------------------------------------------------
-- for player state

instance PlayDB m => PlayDB (PS m) where
  loginCharacter b           = mapExceptT lift . (loginCharacter b)
  getThing                   = mapExceptT lift . getThing
  getTHandle                 = mapExceptT lift . getTHandle
  getThingDescription        = mapExceptT lift . getThingDescription
  getCoPlace                 = mapExceptT lift . getCoPlace
  getCoExits                 = mapExceptT lift . getCoExits
  getAddress                 = mapExceptT lift . getAddress
  getCoOccupantIDs           = mapExceptT lift . getCoOccupantIDs
  getCoContentHandles        = mapExceptT lift . getCoContentHandles

  setLocation pid            = mapExceptT lift . (setLocation pid)
  setUtterance tid           = mapExceptT lift . (setUtterance tid)
  updateThing                = mapExceptT lift . updateThing


instance DriverDB m => DriverDB (PS m) where
  createCharacter n          = mapExceptT lift . (createCharacter n)
