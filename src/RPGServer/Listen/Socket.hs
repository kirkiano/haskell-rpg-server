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
import RPGServer.Util.ByteString
import RPGServer.Util.Text
import Control.Concurrent                    ( forkIO )
import qualified Data.Time                   as T
import qualified Data.IORef                  as IOR
import qualified System.IO                   as Y
import qualified GHC.IO.Handle               as H
import qualified RPGServer.Log               as L
import qualified Network.Socket              as S
import qualified SendReceive                 as SR
import qualified RPGServer.Listen.Connection as C
import qualified RPGServer.Listen.Wrap       as W
import qualified RPGServer.Global.Env        as G


data Socket = Socket {
  _addr :: S.SockAddr,
  _t0   :: T.UTCTime,
  _h    :: H.Handle,
  _op   :: IOR.IORef Bool
}

instance Show Socket where
  show s = "Socket<" ++ show (_h s) ++ ", " ++ show (_addr s) ++ ">"


makeSocket :: MonadIO m => S.Socket -> S.SockAddr -> m Socket
makeSocket s sa = liftIO $ do
  t  <- T.getCurrentTime
  op <- IOR.newIORef True
  h  <- S.socketToHandle s Y.ReadWriteMode
  H.hSetBuffering h H.LineBuffering
  H.hSetEncoding h =<< Y.mkTextEncoding "UTF8"
  return $ Socket sa t h op


instance (MonadIO m,
          L.Log m L.General,
          L.Log m L.Connection) => C.Connection m Socket where
  connectionType = const $ return L.Socket
  timeOpened     = return . _t0
  isOpen         = liftIO . IOR.readIORef . _op
  close s r      = do
    isopen <- C.isOpen s
    when isopen $ do
      liftIO $ IOR.writeIORef (_op s) False
      L.log L.Debug $ L.General $ "Closing handle on " ++ show s
      liftIO . Y.hClose ._h $ s
      L.log L.Info $ L.ConnectionClosed L.Socket r


instance (MonadIO m,
          L.Log m L.General,
          L.Log m L.Connection ,
          L.Log m L.Transmission) => SR.Sender Socket m ByteString where
  send s b = do
    let good _ = do
          L.log L.Debug $ L.SentToHost L.Socket b
          return True
        bad e = do
          L.log L.Info $ L.CannotSendToHost L.Socket $ Just $ show e
          C.closeCannotSend s
          return False
        doSend = catch
                 (do Y.hPutStrLn (_h s) $ unpack $ decodeUtf8 b
                     Y.hFlush (_h s)
                     return $ Right ())
                 (\e -> do _ <- return (e :: SomeException)
                           return $ Left e)
    isopen <- C.isOpen s
    if not isopen
       then return False
       else either bad good =<< liftIO doSend


instance (MonadIO m,
          L.Log m L.General,
          L.Log m L.Connection,
          L.Log m L.Transmission) =>
         SR.WaitingReceiver Socket m ByteString where
  waitRecv s = do
    isopen <- C.isOpen s
    if not isopen
       then return SR.CannotReceive
       else either bad good =<< getNextLine where
         getNextLine = do
           L.log L.Debug $ L.WaitingToReceive L.Socket $ show s
           liftIO $ catch
             (Right . encodeUtf8 . pack <$> Y.hGetLine (_h s))
             (return . Left)
         good b = do
           L.log L.Debug $ L.ReceivedFromHost L.Socket b
           return $ SR.Received b
         bad e = do
           let eS = show (e :: SomeException)
           L.log L.Debug $ L.CannotReceiveFromHost L.Socket $ Just eS
           C.closeCannotReceive s
           return SR.CannotReceive


listen :: Int -> (forall c. C.Client G.G c => c -> G.G ()) -> G.G ()
listen port continue = G.gCatch act err where
  act     = G.gBracket (createListener port) destroyListener loop
  err e   = do
    let msg = "Listening socket: " ++ show (e :: SomeException)
    L.log L.Critical $ L.SocketError msg
    throwM e
  loop sl = forever $ do
    G.gBracketOnError
      (liftIO $ S.accept sl)
      (\(s, _) -> closeSocket s)
      (\(s, sAddr) -> do
          L.log L.Info $ L.AcceptedConnection L.Socket $ show s
          skt <- makeSocket s sAddr
          let wskt = W.wrapConn skt :: W.Wrapped G.G Socket
          env <- ask
          lh  <- lift ask
          liftIO $ void $ forkIO $ G.runG env lh $ G.gCatch
            (continue wskt)
            (\e -> do L.log L.Critical $ L.General $ show (e :: SomeException)
                      C.closeInternalError wskt))


createListener :: Int -> G.G S.Socket
createListener port = do
  let jhints = Just $ S.defaultHints { S.addrFlags = [S.AI_PASSIVE] }
  a:_  <- liftIO $ S.getAddrInfo jhints (Just "127.0.0.1") (Just $ show port)
  G.gBracketOnError
    (liftIO $ S.socket (S.addrFamily a) S.Stream S.defaultProtocol)
    (\s -> do let msg = "Can't configure listening socket"
              L.log L.Critical $ L.SocketError msg
              destroyListener s)
    (\s -> do
        liftIO $ do
          S.setSocketOption s S.ReuseAddr 1
          S.setSocketOption s S.NoDelay 1
          S.bind s $ S.addrAddress a
          S.listen s 3
        L.log L.Info $ L.ListeningForConnections L.Socket port
        return s)


destroyListener :: S.Socket -> G.G ()
destroyListener s = do
  liftIO $ S.close s
  L.log L.Info $ L.NoLongerListeningForConnections L.Socket


closeSocket :: S.Socket -> G.G ()
closeSocket s = do
  L.log L.Debug $ L.General $ "Closing " ++ show s
  liftIO $ S.close s


-- currently unused, but still export it, to silence the
-- compiler's warning
getMyIPAddr :: IO S.HostName
getMyIPAddr = do
  let hints = S.defaultHints { S.addrFlags = [S.AI_NUMERICHOST,
                                              S.AI_NUMERICSERV],
                               S.addrSocketType = S.Datagram }
  a:_ <- S.getAddrInfo (Just hints) (Just "8.8.8.8") (Just "80")
  s   <- S.socket (S.addrFamily a) (S.addrSocketType a) (S.addrProtocol a)
  S.connect s $ S.addrAddress a
  mySockAddr    <- S.getSocketName s
  (ipMaybe, _) <- S.getNameInfo [S.NI_NUMERICHOST] True False mySockAddr
  S.close s
  return $ fromJust ipMaybe
