{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module RPGServer.Log ( module System.Log,
                       L,
                       Main(..),
                       DB(..),
                       Connection(..),
                       ConnectionType(..),
                       Transmission(..),
                       Game(..),
                       Drive(..),
                       SendingFunction(..),
                       General(..)) where

import RPGServer.Common
import System.Log
import System.IO                      ( Handle )
import RPGServer.Util.ByteString
import RPGServer.Request              ( Request )
import RPGServer.Event                ( Event )
import qualified RPGServer.World      as W


type L = ReaderT (Level, Handle) IO

type Port = Int

data Main = AttemptingToConnectToPostgres
          | CannotConnectToPostgres String
          | ConnectedToPostgres
          | DisconnectedFromPostgres
          | ListeningForConnections ConnectionType Port
          | NoLongerListeningForConnections ConnectionType
          | SocketError String
          deriving Show


data DB = DBQuery String deriving Show


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


data Drive = RegisteringPlayer W.CID
           | DeregisteringPlayer W.CID
           deriving Show


data SendingFunction = AddingSendingFunction
                     | DroppingSendingFunction
                     | NoSendingFunction
                     deriving Show


data Game = SendingFunction SendingFunction W.CID
          | ProcessingRequest Request
          | EmittingEvent Event [W.CID]
          deriving Show


data General = General String
             deriving Show
