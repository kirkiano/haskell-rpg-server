{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}

module RPGServer.Listen.Conn {- ( Conn, makeConn ) -} where
{-
import RPGServer.Common
import RPGServer.Util.ByteString
import qualified Data.Time                   as T
import qualified Network.Socket              as N
import qualified SendReceive                 as SR
import qualified RPGServer.Log               as L
import qualified RPGServer.Listen.Connection as C


------------------------------------------------------------
{-
instance (MonadIO m,
          L.Log m L.Transmission,
          SR.Sender a IO ByteString) =>
         SR.Sender (Conn a) m ByteString where
  send h b = do
    sr <- liftIO $ SR.send (_conn h) b
    let cd = (_connType h, _host h)
    if sr
      then L.log L.Debug $ L.SentToHost cd b
      else L.log L.Warn  $ L.CannotSendToHost cd
    return sr


instance (MonadIO m,
          L.Log m L.Transmission,
          SR.WaitingReceiver a IO ByteString) =>
         SR.WaitingReceiver (Conn a) m ByteString where
  waitRecv h = do
    rb <- liftIO $ SR.waitRecv . _conn $ h
    let cd = (_connType h, _host h)
    case rb of
          SR.Received b    -> L.log L.Debug $ L.ReceivedFromHost cd b
          SR.CannotReceive -> L.log L.Warn  $ L.CannotReceiveFromHost cd
    return rb
-}
------------------------------------------------------------


{-

import qualified GHC.IO.Handle               as H
import qualified RPGServer.Listen.Wrap       as WC
import qualified RPGServer.Global            as G


listen :: forall e2.
          forall e1.
          forall pc.
          forall c.
          (Exception e1,
           Show e2,
           SR.Sender c IO ByteString,
           SR.WaitingReceiver c IO ByteString) =>
          ((pc -> IO ()) -> IO ()) ->
          L.ConnectionType ->
          (pc -> Maybe N.HostName) ->
          (e1 -> Maybe e2) ->
          (c -> IO ()) ->
          (pc -> IO c) ->
          (pc -> IO ()) ->
          (forall wc. C.Client G.G wc => wc -> G.G ()) ->
          G.G ()
listen loop cType tryHost pickExc close accept reject continue = do
  env <- ask
  lh  <- lift ask
  liftIO $ loop $ \pc -> do
    let rjct = do liftIO $ reject pc
                  L.log L.Info $ L.RejectedOriginlessConnection cType
    maybe
      (runG env lh rjct)
      (\h -> accept pc >>= acceptor env lh pickExc close cType h continue)
      (tryHost pc)


acceptor :: Exception e1 =>
            G.Env -> (L.Level, H.Handle) ->
            (e1 -> Maybe e2) ->
            (c -> IO ()) ->
            L.ConnectionType ->
            N.HostName ->
            (forall wc. C.Client G.G wc => wc -> G.G ()) ->
            c ->
            IO ()
acceptor env lh pickExc close ct h cnt c = handleJust pickExc hb act where
  hb :: Exception e => e -> IO ()
  hb = handleBreak env lh h ct
  act = runG env lh $ do
    hdl <- liftIO $ makeConn c h (close c) ct
    L.log L.Info $ L.AcceptedConnection (ct, h)
    cnt $ WC.wrapConn hdl


handleBreak :: Exception e =>
               G.Env -> (L.Level, H.Handle) ->
               N.HostName -> L.ConnectionType -> e -> IO ()
handleBreak env lh h ct e = runG env lh $ do
  L.log L.Info $ L.ConnectionClosed (ct, h) $ L.ByClient (Just $ show e)


runG :: G.Env -> (L.Level, H.Handle) -> G.G a -> IO a
runG env lh g = runReaderT (runReaderT g env) lh
-}
-}
