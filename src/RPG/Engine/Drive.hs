
module RPG.Engine.Drive ( Drive(..) ) where

import Prelude                      hiding ( length, lookup )
import Data.Set                            ( Set, singleton )
import Database.CRUD
import RPG.World
import RPG.DB                              ( Db )
import RPG.Error.Data                      ( D )


class Monad m => Drive m where
  getCharactersByPrefix :: CharName -> D m (Set CharID)

  createCharacter :: CharName -> CharDescription -> PlaceID -> D m CharID

  deleteCharacter :: CharID -> D m ()

------------------------------------------------------------

instance (Monad m, Db m) => Drive m where
  getCharactersByPrefix = lookup

  createCharacter n d pid = save (Chars, (n, d, pid))

  deleteCharacter = delete . singleton
