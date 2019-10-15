
module RPG.Engine.Common ( module Data.Maybe,
                           (<|>),
                           ReadMaybe(..),
                           toMaybe,
                           bracket,
                           bracketOnError,
                           throwM,
                           MonadCatch(..),
                           catchJust,
                           handle,
                           handleJust,
                           modifyIOError,
                           Exception(..),
                           SomeException,
                           forever,
                           forM,
                           void,
                           when,
                           MonadIO,
                           MonadTrans,
                           liftIO,
                           lift,
                           ExceptT(..),
                           except,
                           catchE,
                           throwE,
                           mapExceptT,
                           runExceptT,
                           withExceptT,
                           intercalate,
                           say,
                           sayn,
                           ReaderT(..),
                           mapReaderT,
                           withReaderT,
                           ask,
                           asks,
                           on ) where

import Prelude hiding                       ( log )
import Control.Monad                        ( forever,
                                              forM,
                                              void,
                                              when )
import Control.Monad.Catch                  ( MonadCatch(..),
                                              Exception(..),
                                              SomeException,
                                              bracket,
                                              bracketOnError,
                                              catch,
                                              catchJust,
                                              handle,
                                              handleJust,
                                              throwM )
import Control.Monad.IO.Class               ( MonadIO,
                                              liftIO )
import Control.Monad.Trans.Class            ( MonadTrans,
                                              lift )
import Control.Monad.Trans.Except           ( ExceptT(..),
                                              except,
                                              mapExceptT,
                                              runExceptT,
                                              withExceptT,
                                              catchE,
                                              throwE )
import Control.Monad.Trans.Reader           ( ReaderT,
                                              runReaderT,
                                              mapReaderT,
                                              withReaderT,
                                              ask,
                                              asks )
import Control.Applicative                  ( (<|>) )
import Data.List                            ( intercalate )
import Data.Function                        ( on )
import Data.Maybe
import System.IO.Error                      ( modifyIOError )
import System.IO                            ( hFlush,
                                              stdout )

------------------------------------------------------------

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

------------------------------------------------------------

class ReadMaybe a where
  readMaybe :: String -> Maybe a

instance (Read a) => ReadMaybe a where
  readMaybe = f . reads where
    f [(x,"")] = Just x
    f _        = Nothing

------------------------------------------------------------

say :: String -> IO ()
say s = putStr s >> hFlush stdout

sayn :: String -> IO ()
sayn s = putStrLn s >> hFlush stdout
