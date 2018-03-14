{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances,
             UndecidableInstances,
             MultiParamTypeClasses,
             FlexibleInstances #-}

module RPGServer.Common ( module Data.Maybe,
                          ReadMaybe(..),
                          toMaybe,
                          bracket,
                          bracketOnError,
                          throw,
                          catch,
                          catchJust,
                          handle,
                          handleJust,
                          Exception(..),
                          SomeException,
                          forever,
                          void,
                          when,
                          MonadIO,
                          liftIO,
                          lift,
                          ExceptT(..),
                          mapExceptT,
                          runExceptT,
                          throwE,
                          intercalate,
                          say,
                          sayn,
                          stdout,
                          ReaderT(..),
                          mapReaderT,
                          withReaderT,
                          ask,
                          asks,
                          on ) where

import Prelude hiding                       ( log )
import Control.Monad                        ( forever,
                                              void,
                                              when )
import Control.Exception                    ( bracket,
                                              bracketOnError,
                                              throw,
                                              catch,
                                              catchJust,
                                              handle,
                                              handleJust,
                                              Exception(..),
                                              SomeException )
import Control.Monad.IO.Class               ( MonadIO,
                                              liftIO )
import Control.Concurrent.MonadIO.Utils     ()
import Control.Monad.Trans.Class            ( lift )
import Control.Monad.Trans.Except           ( ExceptT(..),
                                              mapExceptT,
                                              runExceptT,
                                              throwE )
import Control.Monad.Trans.Reader           ( ReaderT,
                                              runReaderT,
                                              mapReaderT,
                                              withReaderT,
                                              ask,
                                              asks )
import Data.List                            ( intercalate )
import Data.Function                        ( on )
import Data.Maybe
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
