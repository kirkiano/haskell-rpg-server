{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module RPGServer.Play ( P,
                        Play(..),
                        HasCID(..) ) where

import Prelude                      hiding ( length )
import RPGServer.Common
import RPGServer.Util.Text          hiding ( null )
import Data.List                    ( find, delete )
import qualified Text.Regex         as R
import qualified System.Log         as L
import qualified RPGServer.DB.Error as D
import qualified RPGServer.DB.Class as DB
import qualified RPGServer.Log      as L
import qualified RPGServer.World    as W
import qualified RPGServer.Value    as V
import qualified RPGServer.Event    as E


-- | A monad that knows which character is playing.
class Monad m => HasCID m where
  -- | Get the `CharacterID` of the character playing in this monad.
  getCID :: m W.CharacterID


type P m = DB.D m (Maybe V.Value, [(E.Event, [W.CharacterID])])


-- | A monad representing the play of a specific character.
class HasCID m => Play m where
  join           ::                                   P m
  whoAmI         ::                                   P m
  whereAmI       ::                                   P m
  whatIsHere     ::                                   P m
  exits          ::                                   P m
  exit           :: W.ExitID    ->                    P m
  say            :: Text        ->                    P m
  whisper        :: Text        -> W.CharacterID ->   P m
  describeThing  :: W.ThingID   ->                    P m
  editMe         :: W.ThingDesc ->                    P m
  quit           ::                                   P m


instance (DB.PlayDB m,
          HasCID m,
          L.Log m L.Game) => Play m where

  whoAmI = do me <- DB.getThing =<< myID
              return (Just $ V.YouAre me, [])

  whereAmI = do here <- DB.getCoPlace =<< myID
                return (Just $ V.Place here, [])

  whatIsHere = do ths <- DB.getCoContentHandles =<< myID
                  return (Just $ V.PlaceContents ths, [])

  exits = do es <- DB.getCoExits =<< myID
             return (Just $ V.Exits es, [])

  exit eid = maybe err doExit =<< theExitM where
    err      = throwE $ D.InvalidExitID eid
    theExitM = find ((== eid) . W.idn) <$> (DB.getCoExits =<< myID)
    doExit e = do
      let (did, _, _) = W.exitDestination e
          eName       = W.name e
          nbrName     = snd $ W.exitSource e
      meH@(W.THandle cid _) <- myHandle
      oldRecips <- DB.getCoOccupantIDs cid
      DB.setLocation did [cid]
      newRecips <- DB.getCoOccupantIDs cid
      let exited  = E.Exited  eid   cid
          entered = E.Entered eName meH nbrName $ W.exitDirection e
      -- newRecips won't understand what eid refers to (think!), so pass them
      -- instead the names of the exit and the neighboring place
      return (Nothing, [(exited,  oldRecips),
                        (entered, newRecips)])

  say s = maybe (return (Nothing, [])) bcast nonwhite where
    nonwhite = R.matchRegex (R.mkRegex "[^ ]") $ unpack s
    bcast _  = do cid    <- myID
                  recips <- DB.getCoOccupantIDs cid
                  DB.setUtterance cid $ text s
                  return (Nothing, [(E.Said cid s, recips)])

  whisper s recipID = if len > 0 then sendIt stripped else return (Nothing, []) where
    len       = length stripped
    stripped  = strip s
    sendIt ss = do
      cid         <- myID
      occupantIDs <- DB.getCoOccupantIDs cid
      if not $ recipID `elem` occupantIDs
        then throwE $ D.InvalidThingID recipID
        else do let otherIDs = delete recipID $ delete cid occupantIDs
                    whispr   = E.Whispered cid recipID
                return (Nothing, [(whispr $ Just ss, [cid, recipID]),
                                  (whispr   Nothing, otherIDs)])

  describeThing tid = do
    cid  <- myID
    desc <- DB.getThingDescription tid
    cids <- DB.getCoOccupantIDs cid
    return (Just $ V.ThingDescription tid desc, [(E.Looked cid tid, cids)])

  editMe dsc = do
    me <- DB.getThing =<< myID
    let cid = W.idn me
    DB.updateThing $ W.Thing (Just True) cid (W.name me) dsc
    occupants <- DB.getCoOccupantIDs cid
    return (Nothing, [(E.ThingEdited cid, occupants)])

  join = joinOrQuit True  =<< (E.Joined    <$> myHandle)
  quit = joinOrQuit False =<< (E.Disjoined <$>     myID)


joinOrQuit :: (DB.PlayDB m, HasCID m, L.Log m L.Game) => Bool -> E.Event -> P m
joinOrQuit isJoin evt = do
  DB.loginCharacter isJoin =<< myID
  recips <- DB.getCoOccupantIDs =<< myID
  return (Nothing, [(evt, recips)])


myID :: (HasCID m, MonadTrans t) => t m W.ThingID
myID = lift getCID


myHandle :: (HasCID m, DB.PlayDB m) => DB.D m W.THandle
myHandle = DB.getTHandle =<< myID
