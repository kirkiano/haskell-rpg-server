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
import RPGServer.Request              ( Request )
import RPGServer.Event                ( Event )
import RPGServer.Listen.Auth          ( Username )
import RPGServer.World.Thing          ( CharacterID )


type L = ReaderT (Level, Handle) IO

type Port = Int

data Main = StartingForwarder
          | StoppingForwarder
          | StartingForwarderWatcher
          | StoppingForwarderWatcher
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
          | AuthSucceeded Username
          | AuthFailed Username
          | UserAlreadyLoggedIn CharacterID
          deriving Show


data Game = Game Request (Maybe Event)
          deriving Show


data General = General String
             deriving Show
