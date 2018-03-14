{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module RPGServer.Log ( module System.Log,
                       L,
                       Main(..),
                       Connection(..),
                       ConnectionType(..),
                       Transmission(..),
                       Game(..),
                       General(..),
                       Auth(..) ) where

import RPGServer.Common
import System.Log
import System.IO                      ( Handle )
import RPGServer.Util.ByteString
import qualified RPGServer.Message    as M
import qualified RPGServer.Request    as R
import RPGServer.Listen.Auth.Message  ( Username )


type L = ReaderT (Level, Handle) IO

type Port = Int

data Main = StartingForwarder
          | StoppingForwarder
          | AttemptingToConnectToRedis
          | CannotConnectToRedis String
          | ConnectedToRedis
          | DisconnectedFromRedis
          | AttemptingToConnectToPostgres
          | CannotConnectToPostgres String
          | ConnectedToPostgres
          | DisconnectedFromPostgres
          | ListeningForConnections ConnectionType Port
          | NoLongerListeningForConnections ConnectionType
          | WebsocketError
          | SocketError String
          deriving Show


data ConnectionType = Socket | Websocket
                    deriving Show


type ClosureReason = String

data Connection = AcceptedConnection ConnectionType
                | RejectedOriginlessConnection ConnectionType
                | ConnectionClosed ConnectionType ClosureReason
                deriving Show


data Transmission = SentToHost ConnectionType ByteString
                  | CannotSendToHost ConnectionType (Maybe String)
                  | ReceivedFromHost ConnectionType ByteString
                  | CannotReceiveFromHost ConnectionType (Maybe String)
                  deriving Show


data Auth = WaitingForCredentialsFrom String
          | AuthDBSucceeded Username
          | AuthDBFailed Username
          | AuthLDAPSucceeded Username
          | AuthLDAPFailed Username
          | UserAlreadyLoggedIn Username
          deriving Show


data Game = Game R.Request (Maybe M.Message)
          deriving Show


data General = General String
             deriving Show
