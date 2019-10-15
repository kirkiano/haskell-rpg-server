
module RPG.Engine.Game.Play ( play ) where

import RPG.Engine.Common             hiding ( say )
import RPG.Request                          ( PlayerRequest(..) )
import RPG.Engine.Play                      ( Play(..), myID )
import RPG.Engine.Game.Result               ( Result )
import qualified RPG.Value                  as V
import qualified RPG.Event                  as E
import RPG.Error                            ( Error )
import RPG.World


play :: Play m => PlayerRequest -> ExceptT Error m Result
play WhoAmI     = do (cid, cname, cdesc) <- whoAmI
                     let v = Just $ V.YouAre cid cname cdesc
                     return (v, [])
play WhereAmI   = (, []) . (Just .  uncurry V.Place) <$> whereAmI
play WhatIsHere = (, []) . (Just .  V.PlaceContents) <$> whatIsHere
play WaysOut    = (, []) . (Just .          V.Exits) <$> exits

play (Exit eid) = do
  (rName, nbrName, oldRecips, newRecips, meID, myName, dir) <- exit eid
  let -- newRecips won't understand what eid refers to (think!), so pass them
      -- instead the names of the exit and the neighboring place
      exited  = E.Exited  eid   meID
      entered = E.Entered rName meID myName nbrName dir
  return (Nothing, [(exited,  oldRecips),
                    (entered, newRecips)])

play (Say s) = do
  (ss, ourIDs) <- say s
  cid          <- myID
  return (Nothing, [(E.Said cid ss, ourIDs)])

play (Whisper s recipID) = do
  cid            <- myID
  (ss, otherIDs) <- whisper s recipID
  let whispr = E.Whispered cid recipID
  return (Nothing, [(whispr $ Just ss, [cid, recipID]),
                    (whispr   Nothing, otherIDs)])

play (EditMe d) = do
  editMe d
  cid <- myID
  (Nothing,) . (:[]) . (E.CharEdited cid,) <$> whoseIDsAreHere

play Join = do
  join
  (cid, cName) <- myHandle
  (Nothing,) . (:[]) . (E.Joined cid cName,) <$> whoseIDsAreHere

play Quit = do
  quit
  cid <- myID
  (Nothing,) . (:[]) . (E.Disjoined cid,) <$> whoseIDsAreHere

play (DescribeChar cid)   = do
  CharDescription d <- describeChar cid
  let v = Just $ V.CharDescription cid d
  myCID <- myID
  (v,) . (:[]) . (E.Looked myCID cid,) <$> whoseIDsAreHere

play (DescribeThing tid)   = do
  ThingDescription d <- describeThing tid
  let v = Just $ V.ThingDescription tid d
--  cid <- myID
--  (v,) . (:[]) . (E.Looked cid tid,) <$> whoseIDsAreHere
  return (v, [])
