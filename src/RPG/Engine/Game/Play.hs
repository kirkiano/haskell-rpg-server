
module RPG.Engine.Game.Play ( play ) where

import qualified Data.Set                   as S
import RPG.Common.Has                       ( getF )
import RPG.Request                          ( CharRequest(..) )
import RPG.Engine.Play                      ( Play(..), myID )
import RPG.Engine.Game.Result               ( Result )
import qualified RPG.Value                  as V
import qualified RPG.Event                  as E
import RPG.Error                            ( RPG )
import RPG.World


play :: Play m => CharRequest -> RPG m Result

play WhoAmI     = do (cr, cdesc) <- whoAmI
                     let cid   :: CharID   = getF cr
                         cname :: CharName = getF cr
                         v                 = V.YouAre cid cname cdesc
                     return (Just v, S.empty)

play WhereAmI   = do p :: IDV PlaceR <- whereAmI
                     let pid :: PlaceID          = getF p
                         pnm :: PlaceName        = getF p
                         pd  :: PlaceDescription = getF p
                         v                       = V.Place pid pnm pd
                     return (Just v, S.empty)

play WhatIsHere = do ts :: S.Set (IDV ThingR) <- whatIsHere
                     let f ti = (getF ti :: ThingID,
                                 getF ti :: ThingName)
                         v    = V.PlaceContents . S.map f $ ts
                     return (Just v, S.empty)

play WaysOut    = (, S.empty) . Just . V.Exits <$> exits

play (Exit eid) = do
  (rName, nbrName, oldRecips, newRecips, meID, myName, dir) <- exit eid
  let -- newRecips won't understand what eid refers to (think!), so pass them
      -- instead the names of the exit and the neighboring place
      exited  = E.Exited  meID eid
      entered = E.Entered meID myName rName nbrName dir
      eL      = [(exited,  oldRecips),
                 (entered, newRecips)]
  return (Nothing, S.fromList eL)

play (Say s) = do
  (ss, cids) <- say s
  cid        <- myID
  let evts = if S.null cids then S.empty else S.singleton (E.Said cid ss, cids)
  return (Nothing, evts)

play (Whisper s recipID) = do
  cid        <- myID
  (ss, cids) <- say s -- reuse 'say'
  let evts   = if S.null cids then S.empty else S.fromList eL
      eL     = [(whispr $ Just ss, duo),
                (whispr   Nothing, S.difference cids duo)]
      whispr = E.Whispered cid recipID
      duo    = S.fromList [cid, recipID]  
  return (Nothing, evts)

play (EditMe d) = do
  editMe d
  cid <- myID
  (Nothing,) . S.singleton . (E.CharEdited cid,) . S.map getF <$> whoIsHere

play Join = do
  join
  cr <- myHandle
  let cid :: CharID   = getF cr
      cn  :: CharName = getF cr
  (Nothing,) . S.singleton . (E.Joined cid cn,) . S.map getF <$> whoIsHere

play Quit = do
  quit
  cid <- myID
  (Nothing,) . S.singleton . (E.Disjoined cid,) . S.map getF <$> whoIsHere

play (DescribeChar cid)   = do
  CharDescription d <- describeChar cid
  let v = Just $ V.CharDescription cid d
  myCID <- myID
  (v,) . S.singleton . (E.Looked myCID cid,) . S.map getF <$> whoIsHere

play (DescribeThing tid)   = do
  ThingDescription d <- describeThing tid
  let v = Just $ V.ThingDescription tid d
--  cid <- myID
--  (v,) . (:[]) . (E.Looked cid tid,) <$> whoseIDsAreHere
  return (v, S.empty)
