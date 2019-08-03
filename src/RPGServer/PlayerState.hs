{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections,
             OverloadedStrings,
             MultiParamTypeClasses,
             TypeSynonymInstances,
             InstanceSigs,
             UndecidableInstances,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.PlayerState ( PlayerState(PlayerState) ) where

import RPGServer.Common
import Prelude hiding                    ( getContents )
import RPGServer.Log                     as L
import qualified RPGServer.World         as W
import RPGServer.Play                    ( HasCID(..) )
import RPGServer.DB.Class                ( PlayDB(..) )


data PlayerState = PlayerState { _cid  :: W.CharacterID }

type PS = ReaderT PlayerState

instance Monad m => HasCID (PS m) where
  getCID = asks _cid

instance (Monad m, L.LogThreshold m) => L.LogThreshold (PS m) where
  logThreshold = lift L.logThreshold

instance L.Log m L.Game => L.Log (PS m) L.Game where
  logWrite lev = lift . (L.logWrite lev)

instance PlayDB m => PlayDB (PS m) where
  loginCharacter b           = mapExceptT lift . (loginCharacter b)
  getThing                   = mapExceptT lift . getThing
  getThingDescription        = mapExceptT lift . getThingDescription
  getTHandle                 = mapExceptT lift . getTHandle
  getCoPlace                 = mapExceptT lift . getCoPlace
  getCoExits                 = mapExceptT lift . getCoExits
  getAddress                 = mapExceptT lift . getAddress
  getCoOccupantIDs           = mapExceptT lift . getCoOccupantIDs
  getCoContentHandles        = mapExceptT lift . getCoContentHandles
  setLocation pid            = mapExceptT lift . (setLocation pid)
  setUtterance tid           = mapExceptT lift . (setUtterance tid)
  updateThing                = mapExceptT lift . updateThing
