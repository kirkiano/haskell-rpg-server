
module RPG.Engine.Game.Play ( play ) where

import qualified Data.Set                   as S
import Data.Text                            ( strip )
import RPG.Common.Has                       ( getF )
import RPG.Request                          ( CharRequest(..) )
import RPG.Engine.Play                      ( Play(..), myID )
import RPG.Engine.Game.Result               ( Result )
import qualified RPG.Value                  as V
import qualified RPG.Event                  as E
import RPG.Error                            ( RPG )
import RPG.World


play :: Play m => CharRequest -> RPG m Result

play WhereAmI   = (, S.empty) . Just . V.Place <$> whereAmI

play WhatIsHere = (, S.empty) . Just . V.Contents <$> whatIsHere

play WhoIsHere = (, S.empty) . Just . V.Occupants <$> whoIsHere

play HowCanIExit = (, S.empty) . Just . V.WaysOut <$> exits

play (Exit eid) = do
  (rName, nbrName, nbrAddrM, oldRecips, newRecips, dir) <- exit eid
  me <- myHandle
  let -- oldRecips can see eid, but newRecips can't. So pass them
      -- instead the names of the exit and the neighboring place
      exited  = E.Exited  (getF me) eid
      entered = E.Entered me rName nbrName nbrAddrM dir
      evts    = S.fromList [(exited,  oldRecips),
                            (entered, newRecips)]
  return (Nothing, evts)

play (Say s) = (Nothing,) <$> do
  let ss = strip s
  if ss == ""
    then return S.empty
    else do cid <- myID
            S.singleton . (E.Said cid ss,) <$> cidsLoggedIn

play (Whisper s recipID) = (Nothing,) <$> do
  let ss = strip s
  if ss == ""
    then return S.empty
    else do cid <- myID
            cids <- cidsLoggedIn
            let duo    = S.fromList [cid, recipID]
                whispr = E.Whispered cid recipID
            return $ S.fromList [(whispr $ Just ss,                   duo),
                                 (whispr   Nothing, S.difference cids duo)]

play (EditMe d) = do
  editMe d
  cid <- myID
  (Nothing,) . S.singleton . (E.Edited cid,) <$> cidsLoggedIn

play Join = do
  join
  (c, d) <- whoAmI
  let v = V.YouAre c d
  (Just v,) . S.singleton . (E.Joined c,) <$> cidsLoggedIn

play Quit = do
  quit
  cid <- myID
  (Nothing,) . S.singleton . (E.Disjoined cid,) <$> cidsLoggedIn

play (Describe (Left cid)) = do
  CharDescription d <- describeChar cid
  let v = Just $ V.Description (Left cid) d
  myCID <- myID
  (v,) . S.singleton . (E.Looked myCID (Left cid),) <$> cidsLoggedIn

play (Describe (Right tid)) = do
  ThingTypeDescription d <- describeThing tid
  let v = Just . V.Description (Right tid) $ d
  cid <- myID
  (v,) . S.singleton . (E.Looked cid (Right tid),) <$> cidsLoggedIn
