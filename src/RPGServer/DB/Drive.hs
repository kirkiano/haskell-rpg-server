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
module RPGServer.DB.Drive where

import RPGServer.Drive                          ( Drive(..) )
import RPGServer.DB.Class                       ( DriverDB(..) )


instance (Monad m, DriverDB m) => Drive m where
  getCharactersByPrefix = getCIDsByPrefix
  spawnCharacter        = createCharacter
  deleteCharacters      = destroyCharacters

{-
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

assertOK :: Functor m => D m a -> Dr m a
assertOK = withExceptT $ const InternalError

-}
