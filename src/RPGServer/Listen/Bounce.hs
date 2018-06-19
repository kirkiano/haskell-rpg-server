{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric,
             OverloadedStrings,
             AllowAmbiguousTypes,
             FlexibleInstances,
             FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}

module RPGServer.Listen.Bounce ( admit
                               , bounce ) where

import RPGServer.Common
import Control.Workflow                        ( HasFork )
import Data.Maybe                              ( isJust )
import qualified Data.Time                     as TM
import qualified System.Log                    as L
import qualified RPGServer.Log                 as L
import qualified SendReceive                   as SR
import qualified Bouncer                       as B
import RPGServer.Respond                       ( respond )
import qualified RPGServer.Request             as Q
import qualified RPGServer.Message             as S
import RPGServer.World.Thing                   ( CharacterID )
import RPGServer.PlayerState                   ( PlayerState(..) )
import qualified RPGServer.DB.Class            as DB
import RPGServer.Listen.Auth.LDAP              ( authLDAP )
import qualified RPGServer.Listen.Auth.Message as A
import qualified RPGServer.Listen.Connection   as C


bounce :: (DB.AuthDB m,
           DB.PlayDB m,
           HasFork m,
           C.Client m a,
           L.Log m L.Auth,
           L.Log m L.Game)
          =>
          (CharacterID -> a -> m ()) ->
          (CharacterID -> m ()) ->
          (S.Message -> [CharacterID] -> m ()) ->
          (CharacterID -> m Bool) ->
          TM.NominalDiffTime ->
          B.NumTriesLeft ->
          a ->
          m ()
bounce reg dereg fw alreadyHere tOut nTries = do
  B.makeBouncer tOut nTries (admit reg dereg fw alreadyHere) reject challenge


reject :: C.Client m a => a -> m ()
reject c = SR.send c (S.Auth A.BadCredentials) >> C.closeServerQuit c


admit :: (MonadIO m,
          DB.AuthDB m,
          DB.PlayDB m,
          C.Client m a,
          L.Log m L.Auth,
          L.Log m L.Game)
         =>
         (CharacterID -> a -> m ()) ->
         (CharacterID -> m ()) ->
         (S.Message -> [CharacterID] -> m ()) ->
         (CharacterID -> m Bool) ->
         a ->
         CharacterID ->
         m ()
admit reg dereg fw alreadyHere c cid = do
  here <- alreadyHere cid
  if not here
    then do void $ SR.send c $ S.Auth A.AlreadyLoggedIn
            L.log L.Info $ L.UserAlreadyLoggedIn cid
            reject c
    else do void $ SR.send c $ S.Auth A.Welcome
            DB.loginCharacter True cid
            reg cid c
            runReaderT play $ PlayerState cid
  where
  play = resp Q.Join >> loop
  loop = do
    r <- lift $ SR.waitRecv c
    case r of
      SR.CannotReceive -> quit
      (SR.Received q)  -> do
        msgE <- resp q
        case either (Just . S.Error) id msgE of
          Nothing                -> loop
          (Just (S.Disjoined _)) -> quit
          (Just m)               -> do
            b <- lift . (SR.send c) $ m
            if not b then quit else loop
  quit = do lift $ C.closeClientQuit c
            lift $ dereg cid
            DB.loginCharacter False cid
  resp = runExceptT . (respond $ \m -> lift . (fw m))


challenge :: (MonadIO m,
              DB.AuthDB m,
              DB.PlayDB m,
              L.Log m L.Auth,
              C.Client m a)
             =>
             TM.NominalDiffTime ->
             B.NumTriesLeft ->
             a ->
             m (Maybe CharacterID)
challenge _ _ c = do
  L.log L.Debug $ L.WaitingForCredentialsFrom $ show c
  credsR <- SR.waitRecv c
  case credsR of
    SR.Received (Q.Login creds) -> do
      let uname = A.user creds
          tryDB = do
            cidM <- DB.authUser (A.user creds) (A.pass creds)
            let t = if isJust cidM then L.AuthDBSucceeded else L.AuthDBFailed
            L.log L.Info $ t uname
            return cidM
          tryLDAP = do
            cidM <- authLDAP creds
            let t = case cidM of
                  Just _  -> L.AuthLDAPSucceeded
                  Nothing -> L.AuthLDAPFailed
            L.log L.Info $ t uname
            return cidM
      maybe tryLDAP (return . Just) =<< tryDB
    _ -> return Nothing
