{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.Respond ( respond ) where

import RPGServer.Request   ( Request(..) )
import RPGServer.Message   ( Message(..) )
import RPGServer.World     ( CharacterID )
import RPGServer.DB.Error  ( D )
import RPGServer.Play      ( Play(..) )


respond :: Play m =>
           (Message -> [CharacterID] -> m ()) ->
           Request ->
           D m (Maybe Message)
respond _  WhoAmI       = Just . YouAre <$> whoAmI
respond _  WhereAmI     = Just . Place <$> whereAmI
respond fw (Exit eid)   = exit fw eid >> respond fw WhereAmI
respond fw (Say s)      = say fw s >> return Nothing
respond _  WhatIsHere   = Just . PlaceContents <$> whatIsHere
respond fw Join         = Just <$> join fw
respond fw Quit         = Just <$> quit fw
respond _  _            = return Nothing
