
module RPG.Engine.Play ( Play(..),
                         HasCID(..),
                         myID ) where

import Prelude                       hiding ( length, lookup )
import Control.Monad.Trans.Except           ( withExceptT )
import qualified Data.Set                   as S
import Data.Text                            ( Text, strip )
import Database.CRUD
import RPG.Common.Id
import RPG.Common.Has
import RPG.Engine.Common
import RPG.Error                            ( RPG,
                                              Error(DataError,
                                                    InvalidExit) )
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

  myPID :: RPG m PlaceID

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

  describeChar :: CharID -> RPG m CharDescription

  describeThing :: ThingID -> RPG m ThingDescription

  editMe :: CharDescription -> RPG m ()

  quit :: RPG m ()

------------------------------------------------------------

instance (HasCID m, Db m) => Play m where
  join = withE . update . (, LoggedIn True)  =<< myID
  quit = withE . update . (, LoggedIn False) =<< myID

  myPID = withE $ lookup =<< myID
             
  whoAmI = withE $ do cid <- myID
                      (,) <$> lookup cid <*> lookup cid

  myHandle = withE . lookup =<< myID

  whereAmI = withE . lookup =<< myPID

  whatIsHere = withE . lookup =<< myPID

  whoIsHere = S.fromList . IDM.toList <$> (withE . lookup =<< myPID)

  describeChar = withE . lookup

  describeThing tid = withE $ do t :: IDV ThingR <- lookup tid
                                 getM t

  editMe d = withE . update . (, d) =<< myID

  say = decide . strip where
    decide "" = return ("", S.empty)
    decide ss = (ss,) . S.map (getF :: IDV CharR -> CharID) <$> whoIsHere

  exits = S.fromList . IDM.toList <$> (withE . lookup =<< myPID)

  exit eid = do
    e@(Id _ er) :: IDV ExitR <- withE $ lookup eid
    pid <- myPID
    when (pid /= sourceID er) $ throwE (InvalidExit eid)
    cids :: S.Set CharID <- S.map getF <$> whoIsHere
    c  :: IDV CharR <- myHandle
    let meID :: CharID = getF c
        pid'           = destinationID er
    withE $ update (meID, pid')
    cids' :: S.Set CharID <- S.map getF <$> whoIsHere
    let rid :: PortalID = getF e
    r  :: IDV PortalRec <- withE $ lookup rid
    p' :: IDV PlaceR    <- withE $ lookup pid'
    let myName :: CharName   = getF c
        pn     :: PlaceName  = getF p'
        d      :: Direction  = getF e
        rn     :: PortalName = getF r
    return (rn, pn, cids, cids', meID, myName, d)


withE :: Functor m => D m a -> RPG m a
withE = withExceptT DataError
