
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

play (Join sendDetails) = do
  (c, d) <- join
  vs     <- if sendDetails then placeDetails else return []
  let v = V.YouAre $ fmap ((,d) . getF) c
      e = E.Joined $ fmap getF c
  (v:vs,) . S.singleton . (e,) <$> cidsLoggedInHere

play Quit = do
  me :: IDV CharR <- quit
  let n :: (IDV CharName) = fmap getF me
  ([],) . S.singleton . (E.Disjoined n,) <$> cidsLoggedInHere

play WhereAmI = (, S.empty) . (:[]) . V.Place <$> whereAmI

play WhatIsHere = (, S.empty) . (:[]) . V.Contents <$> whatIsHere

play WhoIsHere = (, S.empty) . (:[]) . V.Occupants <$> whoIsHere where

play HowCanIExit = (, S.empty) . (:[]) . V.WaysOut <$> exits

play (Exit eid sendDetails) = do
  (oldRecips, newRecips) <- exit eid
  me <- myHandle
  let -- oldRecips can see eid, but newRecips can't. So pass them
      -- instead the names of the exit and the neighboring place
      exited  = E.Exited  (fmap getF me) eid
      entered = E.Entered (fmap getF me) eid
      evts    = S.fromList [(exited,  oldRecips),
                            (entered, newRecips)]
  vs <- if sendDetails then placeDetails else return []
  return (vs, evts)

play (Say s) = ([],) <$> do
  let ss = strip s
  if ss == ""
    then return S.empty
    else do cid <- myID
            S.singleton . (E.Said cid ss,) <$> cidsLoggedInHere

play (Whisper s recipID) = ([],) <$> do
  let ss = strip s
  if ss == ""
    then return S.empty
    else do cid <- myID
            cids <- cidsLoggedInHere
            let duo    = S.fromList [cid, recipID]
                whispr = E.Whispered cid recipID
            return $ S.fromList [(whispr $ Just ss,                   duo),
                                 (whispr   Nothing, S.difference cids duo)]

play (EditMe d) = do
  editMe d
  cid <- myID
  ([],) . S.singleton . (E.Edited cid,) <$> cidsLoggedInHere

play (Describe (Left cid)) = do
  CharDescription d <- describeChar cid
  let v = V.Description (Left cid) d
  myCID <- myID
  ([v],) . S.singleton . (E.Looked myCID (Left cid),) <$> cidsLoggedInHere

play (Describe (Right tid)) = do
  ThingTypeDescription d <- describeThing tid
  let v = V.Description (Right tid) $ d
  cid <- myID
  ([v],) . S.singleton . (E.Looked cid (Right tid),) <$> cidsLoggedInHere


------------------------------------------------------------

placeDetails :: Play m => RPG m [V.Value]
placeDetails = mapM ((head . fst <$>) . play) [WhereAmI, HowCanIExit,
                                               WhatIsHere, WhoIsHere]
