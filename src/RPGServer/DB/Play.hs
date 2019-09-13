{-# OPTIONS_GHC -Wall -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings,
             TupleSections,
             FlexibleInstances,
             UndecidableInstances #-}

{-
   WARNING: since this instance is an orphan, this module will have to
   be included in any other that expects Play to be deduced from
   HasCID and PlayDB
-}
module RPGServer.DB.Play where

import RPGServer.Common
import Data.List                                ( find, delete )
import RPGServer.Util.Text                      ( strip )
import RPGServer.Error                          ( PlayerError(..) )
import RPGServer.Play                           ( Play(..), P, HasCID, myID )
import RPGServer.DB.Class                       ( PlayDB(..) )
import RPGServer.DB.Error                       ( D )
import qualified RPGServer.World                as W


-- add HasCID to PlayDB and you get Play
instance (PlayDB m, HasCID m) => Play m where

  myHandle        = assertOK $ getTHandle          =<< myID
  whoAmI          = assertOK $ getThing            =<< myID
  whereAmI        = assertOK $ getCoPlace          =<< myID
  whoseIDsAreHere = assertOK $ getCoOccupantIDs    =<< myID
  whatIsHere      = assertOK $ getCoContentHandles =<< myID
  exits           = assertOK $ getCoExits          =<< myID

  exit eid = maybe err doExit =<< theExitM where
    err      = throwE $ InvalidExitID eid
    theExitM = find ((== eid) . W.idn) <$> exits
    doExit e = do let (did, _, _) = W.exitDestination e
                  oldRecips <- whoseIDsAreHere
                  _         <- assertOK $ setLocation did . (:[]) =<< myID
                  newRecips <- whoseIDsAreHere
                  meH       <- myHandle
                  return (e, oldRecips, newRecips, meH)

  say = sayNonWhite . strip where
    sayNonWhite "" = throwE NothingToSay
    sayNonWhite s  = do cid <- myID
                        assertOK $ setUtterance cid s
                        (s,) <$> whoseIDsAreHere

  whisper ss recipID = whisperNonWhite $ strip ss where
    whisperNonWhite "" = throwE $ NothingToWhisper recipID
    whisperNonWhite s  = do cid  <- myID
                            cids <- whoseIDsAreHere
                            if not $ recipID `elem` cids
                              then throwE $ InvalidThingID recipID
                              else return (s, delete recipID $ delete cid cids)

  describeThing tid = do
    withExceptT (const $ InvalidThingID tid) $ getThingDescription tid

  editMe desc = do
    me <- assertOK $ getThing =<< myID
    let cid = W.idn me
    assertOK $ updateThing $ W.Thing (Just True) cid (W.name me) desc

  join = joinOrQuit True
  quit = joinOrQuit False


joinOrQuit :: (PlayDB m, HasCID m) => Bool -> P m ()
joinOrQuit isJoin = assertOK $ loginCharacter isJoin =<< myID


assertOK :: Functor m => D m a -> ExceptT PlayerError m a
assertOK = withExceptT $ const ServerError
