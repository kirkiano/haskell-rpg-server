
module RPG.Engine.Log ( module System.Log,
                        L,
                        Main(..),
                        Connection(..),
                        ConnectionType(..),
--                        Transmission(..),
                        Game(..),
                        Drive(..),
                        SendingFunction(..),
                        General(..)) where

import RPG.Engine.Common
import System.Log
import System.IO                       ( Handle )
-- import RPG.Util.ByteString
import RPG.Request                     ( Request )
import RPG.Event                       ( Event )
import RPG.World


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


data ConnectionType = Socket | Websocket
                    deriving Show


type ClosureReason = String

data Connection = AcceptedConnection ConnectionType String
                | RejectedOriginlessConnection ConnectionType
                | ConnectionClosed ConnectionType ClosureReason
                deriving Show

{-
data Transmission = SentToHost ConnectionType ByteString
                  | CannotSendToHost ConnectionType (Maybe String)
                  | WaitingToReceive ConnectionType String
                  | ReceivedFromHost ConnectionType ByteString
                  | CannotReceiveFromHost ConnectionType (Maybe String)
                  deriving Show
-}

data Drive = RegisteringPlayer CharID
           | DeregisteringPlayer CharID
           deriving Show


data SendingFunction = AddingSendingFunction
                     | DroppingSendingFunction
                     | NoSendingFunction
                     deriving Show


data Game = SendingFunction SendingFunction CharID
          | ProcessingRequest Request
          | EmittingEvent Event [CharID]
          deriving Show


data General = General String
             deriving Show
