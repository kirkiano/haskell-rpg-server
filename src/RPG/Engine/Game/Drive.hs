
module RPG.Engine.Game.Drive ( drive ) where

import Control.Monad.Trans.Except    ( ExceptT, runExceptT )
import qualified Data.Set            as S
import RPG.Request                   ( Request(..) )
import RPG.Message                   ( Message(..) )
import RPG.Error                     ( Error(DataError) )
import RPG.DB                        ( Db )
import qualified RPG.Error.Data      as D
import RPG.Engine.Drive              ( Drive(..) )


drive :: Db m => Request -> m Message

drive q@(CharactersByPrefix pf) = runAndMsg q f $ CharIDsWithPrefix pf where
  f = S.toList <$> getCharactersByPrefix pf

drive q@(SpawnCharacter n d pid) = runAndMsg q f $ CharacterSpawned n pid where
  f = spawnCharacter n d pid

drive q@(DestroyCharacters cids) = runAndMsg q f CharactersDestroyed where
  f = deleteCharacters (S.fromList cids) >> return cids

-- TODO: consider handling PlayerRequest here


runAndMsg :: Monad m =>
             Request -> ExceptT D.Error m a -> (a -> Message) -> m Message
runAndMsg q f msg = either (Error q . DataError) msg <$> runExceptT f
