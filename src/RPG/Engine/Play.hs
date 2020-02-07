
module RPG.Engine.Play ( Play(..),
                         HasCID(..),
                         myID ) where

import Prelude                       hiding ( length, lookup )
import Control.Monad.Trans.Except           ( withExceptT )
import qualified Data.Set                   as S
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

  whereAmI :: RPG m (IDV (PlaceName, PlaceDescription,
                          Maybe (IDV (AddressName, AddressNumber,
                                      StreetName, CityName, CountryName))))

  whatIsHere :: RPG m (S.Set (IDV ThingR))

  whoIsHere :: RPG m (S.Set (IDV CharR))

  cidsLoggedIn :: RPG m (S.Set CharID)
  cidsLoggedIn = S.map getF . S.filter f <$> whoIsHere where
    f c = b where LoggedIn b = getF c

  exitDetails :: ExitR -> RPG m (Direction, PortalName, PlaceName,
                                 Maybe (IDV AddressR))

  exits :: RPG m (S.Set (IDV (Direction, PortalName, PlaceName,
                              Maybe (IDV AddressR))))

  -- | leave the current place by this exit, and return it, the characters in
  -- | the old place, those in the new place
  exit :: ExitID -> RPG m (PortalName, PlaceName,
                           S.Set CharID,
                           S.Set CharID,
                           Direction)

  describeChar :: CharID -> RPG m CharDescription

  describeThing :: ThingID -> RPG m ThingTypeDescription

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

  whereAmI = do p :: (IDV PlaceR) <- withE . lookup =<< myPID
                let aidM  = getF p :: Maybe AddressID
                    n     = getF p :: PlaceName
                    d     = getF p :: PlaceDescription
                    f aid = do a :: IDV AddressR <- withE . lookup $ aid
                               let sid = getF a :: StreetID
                               s :: IDV StreetR <- withE . lookup $ sid
                               let cid = getF s :: CityID
                               c :: IDV CityR <- withE . lookup $ cid
                               let tid = getF c :: CountryID
                               t :: IDV CountryRec <- withE . lookup $ tid
                               let an = getF a :: AddressName
                                   am = getF a :: AddressNumber
                                   sn = getF s :: StreetName
                                   cn = getF c :: CityName
                                   tn = getF t :: CountryName
                               return $ fmap (const (an, am, sn, cn, tn)) aid
                aInfoM <- mapM f aidM
                return $ fmap (const (n, d, aInfoM)) p

  whatIsHere = withE . lookup =<< myPID

  whoIsHere = S.fromList . IDM.toList <$> (withE . lookup =<< myPID)

  describeChar = withE . lookup

  describeThing = withE . lookup

  editMe d = withE . update . (, d) =<< myID

  exitDetails er = do
    r :: IDV PortalRec <- withE . lookup $ portalID er
    p :: IDV PlaceR    <- withE . lookup $ destinationID er
    let aidM = getF p :: Maybe AddressID
    aM :: Maybe (IDV AddressR) <- mapM (withE . lookup) aidM
    return (direction er, getF r, getF p, aM)

  exits = do
    es <- withE . lookup =<< myPID
    S.fromList . IDM.toList <$> IDM.mapM exitDetails es

  exit eid = do
    e@(Id _ er) :: IDV ExitR <- withE $ lookup eid
    pid <- myPID
    when (pid /= sourceID er) $ throwE (InvalidExit eid)
    cids :: S.Set CharID <- S.map getF <$> whoIsHere
    i <- myID
    let pid' = destinationID er
    withE $ update (i, pid')
    cids' :: S.Set CharID <- S.map getF <$> whoIsHere
    let rid :: PortalID = getF e
    r  :: IDV PortalRec <- withE $ lookup rid
    p' :: IDV PlaceR    <- withE $ lookup pid'
    let pn :: PlaceName  = getF p'
        d  :: Direction  = getF e
        rn :: PortalName = getF r
    return (rn, pn, cids, cids', d)


withE :: Functor m => D m a -> RPG m a
withE = withExceptT DataError
