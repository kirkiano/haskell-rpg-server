{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module RPGServer.Log ( module System.Log,
                       L,
                       Main(..),
                       Connection(..),
                       ConnectionType(..),
                       Transmission(..),
                       Game(..),
                       Drive(..),
                       General(..)) where

import RPGServer.Common
import System.Log
import System.IO                      ( Handle )
import RPGServer.Util.ByteString
import RPGServer.Request              ( PlayerRequest )
import RPGServer.Message              ( PlayerMessage )
import RPGServer.Value                ( Value )
import RPGServer.Event                ( Event )
import RPGServer.World                ( CharacterID )


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

data Connection = AcceptedConnection ConnectionType String
                | RejectedOriginlessConnection ConnectionType
                | ConnectionClosed ConnectionType ClosureReason
                deriving Show


data Transmission = SentToHost ConnectionType ByteString
                  | CannotSendToHost ConnectionType (Maybe String)
                  | WaitingToReceive ConnectionType String
                  | ReceivedFromHost ConnectionType ByteString
                  | CannotReceiveFromHost ConnectionType (Maybe String)
                  deriving Show


data Drive = RegisteringPlayer CharacterID
           | DeregisteringPlayer CharacterID
           deriving Show

data Game = CannotReceiveNextRequest
          | ReceivedPlayerRequest CharacterID PlayerRequest
          | SendingValue Value CharacterID
          | CannotSendToCharacter CharacterID PlayerMessage
          | AddingSender CharacterID
          | DroppingSender CharacterID
          | NoSenderForCharacter CharacterID
          | EmittingEvent Event [CharacterID]
          deriving Show


data General = General String
             deriving Show
