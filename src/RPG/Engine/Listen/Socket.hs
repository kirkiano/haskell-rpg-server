
module RPG.Engine.Listen.Socket ( listen,
                                  getMyIPAddr ) where

import Prelude                         hiding ( drop )
import RPG.Engine.Common
import Control.Concurrent                     ( forkIO )
import qualified RPG.Engine.Log               as L
import qualified Network.Socket               as N
import qualified SendReceive                  as SR
import SendReceive.UTF8Handle                 ( )
import RPG.Request                            ( Request )
import RPG.Message                            ( Message(..) )
import qualified RPG.Engine.Global.Env        as G


class    (Show a,
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
  loop s = forever $ G.gBracketOnError (accept s) drop go where
    drop   = lift . flip SR.disconnect Nothing
    go c   = do
      env <- ask
      lh  <- lift ask
      liftIO . void . forkIO . (G.runG env lh) $ G.gCatch (continue c) $
        \e -> do L.log L.Critical $ L.General $ show (e :: SomeException)
                 lift $ SR.disconnect c $ Just $ show e


type LoggingHandle = SR.Logging (SR.WithSocketAddress SR.UTF8Handle)

accept :: (MonadIO m,
           L.Log m (SR.Connected N.SockAddr IOError N.Socket)) =>
          N.Socket -> m (SR.Logging (SR.JSON LoggingHandle))
accept listener = do
  (s, sa) <- liftIO $ N.accept listener
  L.log L.Info . SR.Connected sa $ (Right s :: Either IOError N.Socket)
  h <- liftIO $ SR.socketToUTF8Handle s
  return . SR.Logging L.Debug . SR.JSON . SR.Logging L.DebugBytes $
    SR.makeWithSocketAddress sa h


createListener :: Int -> G.G N.Socket
createListener port = mkListen . head =<< addresses where
  mkListen a = G.gBracketOnError (liftIO $ open a) close (start a)
  open a     = N.socket (N.addrFamily a) N.Stream N.defaultProtocol
  close s    = do let msg = "Can't configure listening socket"
                  L.log L.Critical $ L.SocketError msg
                  destroyListener s
  start a s  = do liftIO $ do N.setSocketOption s N.ReuseAddr 1
                              N.setSocketOption s N.NoDelay 1
                              N.bind s $ N.addrAddress a
                              N.listen s 3
                  L.log L.Info $ L.ListeningForConnections L.Socket port
                  return s
  addresses  = liftIO $ N.getAddrInfo jhints (Just ipS) (Just pS) where
    jhints = Just $ N.defaultHints { N.addrFlags = [N.AI_PASSIVE] }
    ipS    = "127.0.0.1"
    pS     = show port


destroyListener :: N.Socket -> G.G ()
destroyListener s = liftIO (N.close s) >> lg where
  lg = L.log L.Info $ L.NoLongerListeningForConnections L.Socket


-- currently unused, but export it anyway, to silence the
-- compiler's warning
getMyIPAddr :: IO N.HostName
getMyIPAddr = do
  let addrFlags = [N.AI_NUMERICHOST, N.AI_NUMERICSERV]
      hints     = N.defaultHints { N.addrSocketType = N.Datagram,
                                   N.addrFlags      = addrFlags }
  a <- head <$> N.getAddrInfo (Just hints) (Just "8.8.8.8") (Just "80")
  s <- N.socket (N.addrFamily a) (N.addrSocketType a) (N.addrProtocol a)
  N.connect s $ N.addrAddress a
  mySockAddr   <- N.getSocketName s
  (ipMaybe, _) <- N.getNameInfo [N.NI_NUMERICHOST] True False mySockAddr
  N.close s
  return $ fromJust ipMaybe
