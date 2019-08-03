{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses,
             UndecidableInstances,
             FlexibleInstances,
             FlexibleContexts #-}

module RPGServer.Listen.Connection ( Connection(..), Client ) where

import qualified Data.Time                    as T
import qualified SendReceive                  as SR
import qualified RPGServer.Log                as L
import RPGServer.Request                      ( Request )
import RPGServer.Message                      ( Message )


class L.Log m L.Connection => Connection m a where
  connectionType :: a -> m L.ConnectionType
  timeOpened     :: a -> m T.UTCTime
  close          :: a -> String -> m ()  -- should be idempotent
  isOpen         :: a -> m Bool

  closeCannotReceive :: a -> m ()
  closeCannotReceive c = close c "Cannot receive from client"

  closeCannotSend    :: a -> m ()
  closeCannotSend c = close c "Cannot send to client"

  closeInternalError :: a -> m ()
  closeInternalError c = close c "Internal server error"

  closeClientQuit    :: a -> m ()
  closeClientQuit c = close c "Client quits"

  closeServerQuit    :: a -> m ()
  closeServerQuit c = close c "Server quits"


class (Show a,
       Connection m a,
       SR.Sender a m Message,
       SR.WaitingReceiver a m Request) => Client m a

------------------------------------------------------------
{-
instance (Monad m,
          SR.Sender a m ByteString) => SR.Sender a m A.Response where
  send c = SR.send c . J.encode


instance (Monad m,
          SR.WaitingReceiver a m ByteString) =>
         SR.WaitingReceiver a m A.Credentials where
  waitRecv c = do
    b <- SR.waitRecv c
    case b of
      SR.Received bb   -> (either errAndRetry (return . SR.Received) .
                           J.eitherDecode') bb
      SR.CannotReceive -> return SR.CannotReceive
      where
        errAndRetry e = do
          let msg = concat ["{ \"parseError\": \"", BU.fromString e, "\"}"]
          void $ SR.send c msg
          SR.waitRecv c


instance (Connection m a,
          Client a m ByteString ByteString) =>
         Client a m A.Response A.Credentials


instance Client a m ByteString ByteString => AuthClient m a

------------------------------------------------------------

instance (Monad m, SR.Sender a m ByteString) => SR.Sender a m Message where
  send c = SR.send c . J.encode


instance (Monad m,
          SR.WaitingReceiver a m ByteString) =>
         SR.WaitingReceiver a m Request where
  waitRecv c = do
    b <- SR.waitRecv c
    case b of
      SR.Received bb   -> (either errAndRetry (return . SR.Received) .
                           J.eitherDecode') bb
      SR.CannotReceive -> return SR.CannotReceive
      where
        errAndRetry e = do
          let msg = concat ["{ \"parseError\": \"", BU.fromString e, "\"}"]
          void $ SR.send c msg
          SR.waitRecv c


instance (Connection m a,
          SR.Sender a m ByteString,
          Client a m ByteString ByteString) => Client a m Message Request


instance Client a m ByteString ByteString => GameClient m a
-}
