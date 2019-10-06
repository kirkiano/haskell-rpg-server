{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             RankNTypes,
             UndecidableInstances,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses #-}

module RPGServer.Listen.Socket ( listen,
                                 getMyIPAddr ) where

import RPGServer.Common
import Control.Concurrent                    ( forkIO )
import qualified RPGServer.Log               as L
import qualified Network.Socket              as N
import qualified SendReceive                 as SR
import SendReceive.UTF8Handle                ( )
import RPGServer.Request                     ( Request )
import RPGServer.Message                     ( Message(..) )
import qualified RPGServer.Global.Env        as G


class (Show a,
       SR.Disconnect m a,
       SR.SendTo a m Message,
       SR.WaitReceiveFrom a m Request) => Driver m a

instance (Show a,
          SR.Disconnect m a,
          SR.SendTo a m Message,
          SR.WaitReceiveFrom a m Request) => Driver m a


listen :: Int -> (forall c. Driver G.G c => c -> G.G ()) -> G.G ()
listen port continue = G.gCatch act err where
  act     = G.gBracket (createListener port) destroyListener loop
  err e   = do
    let msg = "Listening socket: " ++ show (e :: SomeException)
    L.log L.Critical $ L.SocketError msg
    throwM e
  loop sl = forever $ do
    G.gBracketOnError
      (acceptDriver sl) (lift . flip SR.disconnect Nothing) $ \c -> do
        env <- ask
        lh  <- lift ask
        liftIO . void . forkIO . (G.runG env lh) $ G.gCatch (continue c) $ \e -> do
          L.log L.Critical $ L.General $ show (e :: SomeException)
          lift $ SR.disconnect c $ Just $ show e


acceptDriver :: (MonadIO m, L.Log m (SR.Connected N.SockAddr N.Socket))
                =>
                N.Socket ->
                m (SR.Logging (SR.JSON (SR.Logging (SR.WithSocketAddress SR.UTF8Handle))))
acceptDriver listener = do
  (s, sa) <- liftIO $ N.accept listener
  L.log L.Info $ SR.Connected sa $ Right s
  h <- liftIO $ SR.socketToUTF8Handle s
  return $ SR.Logging L.Debug $ SR.JSON $ SR.Logging L.DebugBytes $
    SR.makeWithSocketAddress sa h


createListener :: Int -> G.G N.Socket
createListener port = do
  let jhints = Just $ N.defaultHints { N.addrFlags = [N.AI_PASSIVE] }
  a:_  <- liftIO $ N.getAddrInfo jhints (Just "127.0.0.1") (Just $ show port)
  G.gBracketOnError
    (liftIO $ N.socket (N.addrFamily a) N.Stream N.defaultProtocol)
    (\s -> do let msg = "Can't configure listening socket"
              L.log L.Critical $ L.SocketError msg
              destroyListener s)
    (\s -> do
        liftIO $ do
          N.setSocketOption s N.ReuseAddr 1
          N.setSocketOption s N.NoDelay 1
          N.bind s $ N.addrAddress a
          N.listen s 3
        L.log L.Info $ L.ListeningForConnections L.Socket port
        return s)


destroyListener :: N.Socket -> G.G ()
destroyListener s = do
  liftIO $ N.close s
  L.log L.Info $ L.NoLongerListeningForConnections L.Socket


-- currently unused, but still export it, to silence the
-- compiler's warning
getMyIPAddr :: IO N.HostName
getMyIPAddr = do
  let hints = N.defaultHints { N.addrFlags = [N.AI_NUMERICHOST,
                                              N.AI_NUMERICSERV],
                               N.addrSocketType = N.Datagram }
  a:_ <- N.getAddrInfo (Just hints) (Just "8.8.8.8") (Just "80")
  s   <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
  N.connect s $ N.addrAddress a
  mySockAddr    <- N.getSocketName s
  (ipMaybe, _) <- N.getNameInfo [N.NI_NUMERICHOST] True False mySockAddr
  N.close s
  return $ fromJust ipMaybe
