{-# OPTIONS_GHC -Wall #-}

module RPGServer.Game.Drive ( drive ) where

import Control.Monad.Trans.Except   ( runExceptT )
import qualified System.Log         as L
import RPGServer.Request            ( Request(..) )
import RPGServer.Message            ( Message(..) )
import RPGServer.Error              ( GameError(..) )
import RPGServer.DB.Error           ( DBError )
import RPGServer.Drive              ( Drive(..) )
import qualified RPGServer.World    as W


drive :: Drive m => Request -> m Message
drive q@(SpawnCharacter n pid) = runExceptT create >>= either (err q) msg where
  create = spawnCharacter n pid
  msg    = return . (CharacterSpawned n pid)


err :: Monad m => Request -> DBError -> m Message
err q = return . Error . CannotGrantRequest q . show
