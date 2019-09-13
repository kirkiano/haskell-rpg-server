{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module RPGServer.Game.Play ( play ) where

import RPGServer.Common             hiding ( say )
import RPGServer.Request                   ( PlayerRequest(..) )
import RPGServer.Play                      ( Play(..), myID )
import RPGServer.Game.Result               ( Result )
import qualified RPGServer.World           as W
import qualified RPGServer.Value           as V
import qualified RPGServer.Event           as E
import RPGServer.Error                     ( PlayerError )


play :: Play m => PlayerRequest -> ExceptT PlayerError m Result
play WhoAmI     = (, []) . (Just .        V.YouAre) <$> whoAmI
play WhereAmI   = (, []) . (Just .         V.Place) <$> whereAmI
play WhatIsHere = (, []) . (Just . V.PlaceContents) <$> whatIsHere
play WaysOut    = (, []) . (Just .         V.Exits) <$> exits

play (Exit eid) = do
  (e, oldRecips, newRecips, meH@(W.THandle cid _)) <- exit eid
  let eName    = W.name e
      -- newRecips won't understand what eid refers to (think!), so pass them
      -- instead the names of the exit and the neighboring place
      nbrName  = snd $ W.exitSource e
      exited   = E.Exited  eid   cid
      entered  = E.Entered eName meH nbrName $ W.exitDirection e
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
  (Nothing,) . (:[]) . (E.ThingEdited cid,) <$> whoseIDsAreHere

play Join = do
  join
  meH <- myHandle
  (Nothing,) . (:[]) . (E.Joined meH,) <$> whoseIDsAreHere

play Quit = do
  quit
  cid <- myID
  (Nothing,) . (:[]) . (E.Disjoined cid,) <$> whoseIDsAreHere

play (DescribeThing tid)   = do
  d <- describeThing tid
  let v = Just $ V.ThingDescription tid d
  cid <- myID
  (v,) . (:[]) . (E.Looked cid tid,) <$> whoseIDsAreHere
