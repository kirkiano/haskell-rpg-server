{-# OPTIONS_GHC -Wall #-}

module RPGServer.Game.Drive ( drive ) where

import RPGServer.Request        ( Request )
import RPGServer.Message        ( Message )
import RPGServer.DB.Class       ( DriverDB )



drive :: DriverDB m
         =>
         Request -> m Message
drive = undefined -- ALSO CONTINUE HERE
-- IN GENERAL: get types working before implementing