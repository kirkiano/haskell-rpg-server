{-# OPTIONS_GHC -Wall #-}

module RPGServer.Game.Drive ( drive ) where

import Control.Monad.Trans.Except   ( ExceptT, runExceptT )
import RPGServer.Request            ( Request(..) )
import RPGServer.Message            ( Message(..) )
import RPGServer.Error              ( GameError(..) )
import RPGServer.DB.Error           ( DBError )
import RPGServer.Drive              ( Drive(..) )


drive :: Drive m => Request -> m Message

drive q@(CharactersByPrefix pf) = runAndMsg q (getCharactersByPrefix pf) $
  CIDsWithPrefix pf

drive q@(SpawnCharacter n pid) = runAndMsg q (spawnCharacter n pid) $
  (CharacterSpawned n pid)

drive q@(DestroyCharacters cids) = runAndMsg q (deleteCharacters cids) $
  CharactersDestroyed


runAndMsg :: Monad m =>
             Request -> ExceptT DBError m a -> (a -> Message) -> m Message
runAndMsg q f msg = either (err q) msg <$> runExceptT f


err :: Request -> DBError -> Message
err q = Error . CannotGrantRequest q . show
