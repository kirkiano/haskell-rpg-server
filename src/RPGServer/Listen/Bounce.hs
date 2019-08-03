{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings,
             AllowAmbiguousTypes,
             FlexibleInstances,
             FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}

module RPGServer.Listen.Bounce ( admit,
                                 bounce ) where

import RPGServer.Common
import Control.Workflow                        ( HasFork )
import qualified Data.Time                     as TM
import qualified System.Log                    as L
import qualified RPGServer.Log                 as L
import qualified SendReceive                   as SR
import qualified Bouncer                       as B
import qualified RPGServer.Request             as Q
import qualified RPGServer.Message             as S
import RPGServer.World.Thing                   ( CharacterID )
import qualified RPGServer.DB.Class            as DB
import qualified RPGServer.Listen.Auth         as A
import qualified RPGServer.Listen.Connection   as C


bounce :: (A.Auth m,
           DB.PlayDB m,
           HasFork m,
           C.Client m a,
           L.Log m L.Auth,
           L.Log m L.Game)
          =>
          (CharacterID -> a -> m ()) ->
          (CharacterID -> m ()) ->
          SR.Send m (CharacterID, Q.Request) ->
          TM.NominalDiffTime ->
          B.NumTriesLeft ->
          a ->
          m ()
bounce reg dereg sendToGameLoop tOut nTries = do
  B.makeBouncer tOut nTries (admit reg dereg sendToGameLoop) reject challenge


admit :: (MonadIO m,
          DB.PlayDB m,
          C.Client m a,
          L.Log m L.Game)
         =>
         (CharacterID -> a -> m ()) ->
         (CharacterID -> m ()) ->
         SR.Send m (CharacterID, Q.Request) ->
         a ->
         CharacterID ->
         m ()
admit reg dereg sendToGameLoop clt cid = join >> play where
  play = SR.waitRecv clt >>= convey where
    convey (SR.Received q) | not (Q.isQuit q) = sendToGameLoop (cid, q) >> play
    convey _                                  = quit
  join = do reg cid clt
            void $ SR.send clt $ S.ChallengeMessage $ S.Welcome cid
            sendToGameLoop (cid, Q.Join)
  quit = C.closeClientQuit clt >> dereg cid


challenge :: (MonadIO m,
              A.Auth m,
              L.Log m L.Auth,
              C.Client m a)
             =>
             TM.NominalDiffTime ->
             B.NumTriesLeft ->
             a ->
             m (Maybe CharacterID)
challenge _ _ c = prompt >> wait >>= auth where
  prompt = SR.send c $ S.ChallengeMessage S.SendCredentials
  wait   = do L.log L.Debug $ L.WaitingForCredentialsFrom $ show c
              SR.waitRecv c
  auth (SR.Received (Q.Login creds@(A.Credentials user _))) = do
    result <- A.authUser creds
    case result of
      g@(Just _) -> do L.log L.Info $ L.AuthSucceeded user
                       return g
      Nothing    -> do void $ SR.send c $ S.ChallengeMessage S.BadCredentials
                       L.log L.Info $ L.AuthFailed user
                       return Nothing
  -- if the client does not know to send credentials, say nothing in return
  auth _ = return Nothing


reject :: C.Client m a => a -> m ()
reject = C.closeServerQuit