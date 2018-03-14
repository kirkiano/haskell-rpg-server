{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             RankNTypes,
             UndecidableInstances,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses #-}

module RPGServer.Listen.Websocket ( listen ) where

import RPGServer.Common
import RPGServer.Util.ByteString
import qualified Data.ByteString.Lazy        as BZ
import qualified Data.IORef                  as IOR
import qualified Data.Time                   as T
import qualified Network.WebSockets          as WS
import qualified RPGServer.Log               as L
import qualified SendReceive                 as SR
import qualified RPGServer.Listen.Connection as C
import qualified RPGServer.Listen.Wrap       as W
import qualified RPGServer.Global.Env        as G


data Websocket = Websocket {
  _t0     :: T.UTCTime,
  _conn   :: WS.Connection,
  _op     :: IOR.IORef Bool
}


instance Show Websocket where
  show w = "Websocket<opened at " ++ show (_t0 w) ++ ">"


instance (MonadIO m,
          L.Log m L.Connection) => C.Connection m Websocket where
  connectionType = const $ return L.Websocket
  timeOpened     = liftIO . return . _t0
  isOpen         = liftIO . IOR.readIORef . _op
  close ws r     = do
    _isOpen <- C.isOpen ws
    when _isOpen $ do
      liftIO $ catch
        (WS.sendClose (_conn ws) (showBS r))
        (\e -> sayn $ "Websocket close failed: " ++ show (e :: SomeException))
      liftIO $ IOR.writeIORef (_op ws) False
      L.log L.Info $ L.ConnectionClosed L.Websocket r


instance (MonadIO m,
          L.Log m L.Connection,
          L.Log m L.Transmission) => SR.Sender Websocket m ByteString where
  send ws b = do
    let good _ = do
          L.log L.Debug $ L.SentToHost L.Websocket b
          return True
        bad e  = do
          L.log L.Info $ L.CannotSendToHost L.Websocket $ Just $ show e
          C.closeCannotSend ws
          return False
        doSend = catch
                 (WS.sendTextData (_conn ws) b >> (return $ Right ()))
                 (\e -> return (e :: SomeException) >> (return $ Left e))
    isopen <- C.isOpen ws
    if not isopen
      then return False
      else either bad good =<< liftIO doSend


instance (MonadIO m,
          L.Log m L.Transmission,
          L.Log m L.Connection) =>
         SR.WaitingReceiver Websocket m ByteString where
  waitRecv ws = recv >>= either bad good where
    recv = liftIO $ catch
           (do (WS.Text b _) <- WS.receiveDataMessage $ _conn ws
               return . Right . BZ.toStrict $ b)
           (\e -> return $ Left (e :: SomeException))
    good b = do L.log L.Debug $ L.ReceivedFromHost L.Websocket b
                return $ SR.Received b
    bad e = do
      L.log L.Info $ L.CannotReceiveFromHost L.Websocket (Just $ show e)
      C.closeCannotReceive ws
      return SR.CannotReceive


listen :: Int -> (forall c. C.Client G.G c => c -> G.G ()) -> G.G ()
listen port continue =
  G.gBracket
  (do L.log L.Info $ L.ListeningForConnections L.Websocket port
      env <- ask
      lh  <- lift ask
      return (env, lh))
  (const $ L.log L.Info $ L.NoLongerListeningForConnections L.Websocket)
  (\(env, lh) -> liftIO $ do
      WS.runServer "127.0.0.1" port $ \pc -> G.runG env lh $
        G.gCatch
        (enter continue pc)
        (\e -> L.log L.Critical $ L.General $ show (e :: SomeException)))


enter :: (forall c. C.Client G.G c => c -> G.G ()) ->
         WS.PendingConnection ->
         G.G ()
enter continue pc = do
  L.log L.Debug $ L.General "incoming websocket pending conn"
  G.gBracketOnError
    (do c      <- liftIO $ WS.acceptRequest pc
        L.log L.Debug $ L.General "websocket pending conn accepted"
        t      <- liftIO T.getCurrentTime
        isopen <- liftIO $ IOR.newIORef True
        let w :: W.Wrapped G.G Websocket
            w = W.wrapConn $ Websocket t c isopen
        return w)
    (\wc -> do L.log L.Critical L.WebsocketError
               C.closeInternalError wc)
    continue
