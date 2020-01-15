
module RPG.Engine.Game.Play ( play ) where

import qualified Data.Set                   as S
import RPG.Common.Id
import RPG.Common.Has                       ( getF )
import RPG.Engine.Common             hiding ( say )
import RPG.Request                          ( PlayerRequest(..) )
import RPG.Engine.Play                      ( Play(..), myID )
import RPG.Engine.Game.Result               ( Result )
import qualified RPG.Value                  as V
import qualified RPG.Event                  as E
import RPG.Error                            ( Error )
import RPG.World


play :: Play m => PlayerRequest -> ExceptT Error m Result

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

play WaysOut    = do es :: S.Set (IDV ExitR) <- exits
                     let f (ei :: IDV ExitR) = (getF ei :: ExitID,
                                 sourceID      . fromId $ ei,
                                 destinationID . fromId $ ei,
                                 portalID      . fromId $ ei,
                                 direction     . fromId $ ei)
                         v = V.Exits . S.map f $ es
                     return (Just v, S.empty)

play (Exit eid) = do
  (rName, nbrName, oldRecips, newRecips, meID, myName, dir) <- exit eid
  let -- newRecips won't understand what eid refers to (think!), so pass them
      -- instead the names of the exit and the neighboring place
      exited  = E.Exited  meID eid
      entered = E.Entered meID eid myName
      eL      = [(exited,  oldRecips),
                 (entered, newRecips)]
  return (Nothing, S.fromList eL)

play (Say s) = do
  (ss, ourIDs) <- say s
  cid          <- myID
  return (Nothing, S.singleton (E.Said cid ss, ourIDs))

play (Whisper s recipID) = do
  cid            <- myID
  (ss, otherIDs) <- whisper s recipID
  let whispr = E.Whispered cid recipID
      eL     = [(whispr $ Just ss, S.fromList [cid, recipID]),
                (whispr   Nothing, otherIDs)]
  return (Nothing, S.fromList eL)

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
