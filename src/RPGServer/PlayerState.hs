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
import RPGServer.DB.Class                ( AuthDB(..),
                                           PlayDB(..) )


data PlayerState = PlayerState { _cid  :: W.CharacterID }

type PS = ReaderT PlayerState


instance Monad m => HasCID (PS m) where
  getCID = asks _cid


instance (Monad m, L.LogThreshold m) => L.LogThreshold (PS m) where
  logThreshold = lift L.logThreshold

instance L.Log m L.Game => L.Log (PS m) L.Game where
  logWrite lev = lift . (L.logWrite lev)


instance AuthDB m => AuthDB (PS m) where
  authUser u       = lift . (authUser u)
  loginCharacter b = lift . (loginCharacter b)


instance PlayDB m => PlayDB (PS m) where
  getThing         = mapExceptT lift . getThing
  getLocation      = mapExceptT lift . getLocation
  getPlace         = mapExceptT lift . getPlace
  getOccupants     = mapExceptT lift . getOccupants
  getContents      = mapExceptT lift . getContents
  setLocation tid  = mapExceptT lift . (setLocation tid)
