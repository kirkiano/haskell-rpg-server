{-# OPTIONS_GHC -Wall #-}

module RPGServer.Game.Result where

import RPGServer.Value      ( Value )
import RPGServer.Event      ( Event )
import RPGServer.World      ( CharacterID )


type Result = (Maybe Value, [(Event, [CharacterID])])