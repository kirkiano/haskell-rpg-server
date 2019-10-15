
module RPG.Engine.Play ( Play(..),
                         HasCID(..),
                         myID ) where

import Prelude                       hiding ( length, lookup )
import Control.Monad.Trans.Except           ( withExceptT )
import qualified Data.Set                   as S
import Data.Text                            ( Text )
import Database.CRUD
import RPG.Common.Id
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

  whoAmI :: RPG m (CharID, CharName, CharDescription)

  myHandle :: RPG m (CharID, CharName)

  whereAmI :: RPG m (PlaceID, PlaceName)

  whatIsHere :: RPG m ([(CharID,  CharName)],
                       [(ThingID, ThingName)])

  whoseIDsAreHere :: RPG m [CharID]

  exits :: RPG m [(ExitID, PortalName)]

  -- | leave the current place by this exit, and return it, the characters in
  -- | the old place, those in the new place
  exit :: ExitID -> RPG m (PortalName, PlaceName, [CharID],
                           [CharID], CharID, CharName, Direction)

  -- | if input is nonwhitespace, then say it and return it and hearers
  say :: Text -> RPG m (Text, [CharID])

  -- | if input is nonwhitespace, then say it and return it and observers
  whisper :: Text -> CharID -> RPG m (Text, [CharID])

  describeChar :: CharID -> RPG m CharDescription

  describeThing :: ThingID -> RPG m ThingDescription

  editMe :: CharDescription -> RPG m ()

  quit :: RPG m ()

------------------------------------------------------------

instance (HasCID m, Db m) => Play m where
  join = ok . update . (, LoggedIn True)  =<< myID
  quit = ok . update . (, LoggedIn False) =<< myID

  whoAmI = ok $ do cid <- myID
                   (n :: CharName, _ :: LoggedIn) <- lookup cid
                   d  :: CharDescription          <- lookup cid
                   return (cid, n, d)

  myHandle = ok $ do cid <- myID
                     (n :: CharName, _ :: LoggedIn) <- lookup cid
                     return (cid, n)

  whereAmI = ok $ do cid <- myID
                     pid :: PlaceID    <- lookup cid
                     p   :: IDV PlaceR <- lookup pid
                     pn  :: PlaceName  <- getM p
                     return (pid, pn)

  whatIsHere = ok $ do cid <- myID
                       pid :: PlaceID                        <- lookup cid
                       ts  :: S.Set (IDV ThingR)             <- lookup pid
                       cs  :: IDM.IDMap (CharName, LoggedIn) <- lookup pid
                       let f (Id n (cn, _)) = (Id n Chars, cn)
                           csL = map f $ IDM.toList cs
                           g t = (getF t :: ThingID, getF t :: ThingName)
                           tsL = map g $ S.toList ts
                       return (csL, tsL)

  whoseIDsAreHere = ok $ do cid            <- myID
                            pid :: PlaceID <- lookup cid
                            cs  :: IDM.IDMap (CharName, LoggedIn) <- lookup pid
                            return . map (flip Id Chars) . IDM.keys $ cs

  describeChar = ok . lookup

  describeThing tid = ok $ do t :: IDV ThingR <- lookup tid
                              getM t

  editMe d = ok . update . (, d) =<< myID


ok :: Functor m => D m a -> RPG m a
ok = withExceptT DataError
