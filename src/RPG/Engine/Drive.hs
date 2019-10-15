
module RPG.Engine.Drive ( Drive(..) ) where

import Prelude                      hiding ( length, lookup )
import Data.Set                            ( Set )
import Data.Text                           ( Text )
import Database.CRUD
import RPG.World
import RPG.DB                              ( Db )
import RPG.Error.Data                      ( D )


class Monad m => Drive m where
  getCharactersByPrefix :: Text -> D m (Set CharID)

  spawnCharacter :: CharName -> CharDescription -> PlaceID -> D m CharID

  deleteCharacters :: Set CharID -> D m ()

------------------------------------------------------------

instance (Monad m, Db m) => Drive m where
  getCharactersByPrefix = lookup . CharName

  spawnCharacter n d pid = save (Chars, (n, d, pid))

  deleteCharacters = delete
