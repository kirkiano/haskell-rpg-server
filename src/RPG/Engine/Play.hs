
module RPG.Engine.Play ( Play(..),
                         HasCID(..),
                         myID ) where

import Prelude                       hiding ( length,
                                              lookup,
                                              reverse )
import Control.Monad.Trans.Except           ( withExceptT )
import qualified Data.Set                   as S
import Database.CRUD
import RPG.Common.Id
import RPG.Common.Has
import RPG.Common.Construct
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

type ExitDetails = (Direction, PortalName, IDV PlaceRec)


-- | A monad representing the play of a specific character.
class HasCID m => Play m where
  join :: RPG m (IDV CharR, CharDescription)

  myPID :: RPG m PlaceID

  myHandle :: RPG m (IDV CharR)


  whereAmI :: RPG m (IDV PlaceRec)

  whatIsHere :: RPG m (S.Set (IDV ThingR))

  whoIsHere :: RPG m (S.Set (IDV CharR))

  cidsLoggedInHere :: RPG m (S.Set CharID)
  cidsLoggedInHere = S.map getF . S.filter f <$> whoIsHere where
    f c = b where LoggedIn b = getF c

  exits :: RPG m (S.Set (IDV ExitDetails))

  -- | leave the current place by this exit. Return the characters in
  -- | the old place and those in the new place.
  exit :: ExitID -> RPG m (S.Set CharID, S.Set CharID)

  describeChar :: CharID -> RPG m CharDescription

  describeThing :: ThingID -> RPG m ThingTypeDescription

  editMe :: CharDescription -> RPG m ()

  quit :: RPG m (IDV CharR)

------------------------------------------------------------

instance (HasCID m, Db m) => Play m where
  join = do withE . update . (, LoggedIn True) =<< myID
            cid <- myID
            withE $ (,) <$> lookup cid <*> lookup cid

  quit = do withE . update . (, LoggedIn False) =<< myID
            myHandle

  myPID = withE $ lookup =<< myID
             
  myHandle = withE . lookup =<< myID

  whereAmI = withE . lookup =<< myPID

  whatIsHere = {-return $ S.singleton t where -} withE . lookup =<< myPID
-- t :: IDV ThingR = Id 555 $ construct (ThingTypeName "ipod", SerialNumber 444)

  whoIsHere = S.fromList . IDM.toList <$> (withE . lookup =<< myPID)

  describeChar = withE . lookup

  describeThing = withE . lookup

  editMe d = withE . update . (, d) =<< myID

  exits = do
    pid <- myPID
    es :: IDM.IDMap ExitRec <- withE $ lookup pid
    let f er = (dxn, rn, nbr) where
              (src, dst) = getF er :: (IDV PlaceRec, IDV PlaceRec)
              rev        = pid == (getF dst :: PlaceID)
              dxn        = (if rev then reverse else id) (getF er :: Direction)
              nbr        = if rev then src else dst
              rn         = getF er :: PortalName
    return . S.fromList . map (fmap f) . IDM.toList $ es

  exit eid = do
    Id _ er :: IDV ExitR <- withE $ lookup eid
    pid <- myPID
    let cis   = pid == sourceID er
        trans = pid == destinationID er
    when (not $ cis || trans) . throwE . InvalidExit $ eid
    cs :: S.Set CharID <- cidsLoggedInHere
    let selector = if cis then destinationID else sourceID
    withE . update . (, selector er) =<< myID
    cs' :: S.Set CharID <- cidsLoggedInHere
    return (cs, cs')


withE :: Functor m => D m a -> RPG m a
withE = withExceptT DataError
