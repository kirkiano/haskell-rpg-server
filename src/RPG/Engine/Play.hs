
module RPG.Engine.Play ( Play(..),
                         HasCID(..),
                         myID ) where

import Prelude                       hiding ( length, lookup )
import Control.Monad.Trans.Except           ( withExceptT )
import qualified Data.Set                   as S
import Data.Text                            ( Text )
import Database.CRUD
import RPG.Common.Has
import RPG.Engine.Common
import RPG.Error                            ( RPG,
                                              Error(DataError) {- PlayerError -} )
import RPG.Error.Data                       ( D )
import RPG.World
import qualified RPG.World.IDMap            as IDM
import RPG.DB                               ( Db )


-- | A monad that knows which character is playing.
class Monad m => HasCID m where
  -- | Get the `CharacterID` of the character playing in this monad.
  getCID :: m CharID


myID :: (HasCID m, MonadTrans t) => t m CharID
myID = lift getCID


-- | A monad representing the play of a specific character.
class HasCID m => Play m where
  join :: RPG m ()

  whoAmI :: RPG m (IDV CharR, CharDescription)

  myHandle :: RPG m (IDV CharR)

  whereAmI :: RPG m (IDV PlaceR)

  whatIsHere :: RPG m (S.Set (IDV ThingR))

  whoIsHere :: RPG m (S.Set (IDV CharR))

  exits :: RPG m (S.Set (IDV ExitR))

  -- | leave the current place by this exit, and return it, the characters in
  -- | the old place, those in the new place
  exit :: ExitID -> RPG m (PortalName, PlaceName,
                           S.Set CharID,
                           S.Set CharID,
                           CharID, CharName, Direction)

  -- | if input is nonwhitespace, then say it and return it and hearers
  say :: Text -> RPG m (Text, S.Set CharID)

  -- | if input is nonwhitespace, then say it and return it and observers
  whisper :: Text -> CharID -> RPG m (Text, S.Set CharID)

  describeChar :: CharID -> RPG m CharDescription

  describeThing :: ThingID -> RPG m ThingDescription

  editMe :: CharDescription -> RPG m ()

  quit :: RPG m ()

------------------------------------------------------------

instance (HasCID m, Db m) => Play m where
  join = ok . update . (, LoggedIn True)  =<< myID
  quit = ok . update . (, LoggedIn False) =<< myID

  whoAmI = ok $ do cid                  <- myID
                   r :: IDV CharR       <- lookup cid
                   d :: CharDescription <- lookup cid
                   return (r, d)

  myHandle = ok $ lookup =<< myID

  whereAmI = ok $ do cid               <- myID
                     pid :: PlaceID    <- lookup cid
                     p   :: IDV PlaceR <- lookup pid
                     return p

  whatIsHere = ok $ do cid                        <- myID
                       pid :: PlaceID             <- lookup cid
                       ts  :: S.Set (IDV ThingR)  <- lookup pid
                       return ts

  whoIsHere = ok $ do cid                    <- myID
                      pid :: PlaceID         <- lookup cid
                      cs  :: IDM.IDMap CharR <- lookup pid
                      return . S.fromList . IDM.toList $ cs

  describeChar = ok . lookup

  describeThing tid = ok $ do t :: IDV ThingR <- lookup tid
                              getM t

  editMe d = ok . update . (, d) =<< myID


ok :: Functor m => D m a -> RPG m a
ok = withExceptT DataError
