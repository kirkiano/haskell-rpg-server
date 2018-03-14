{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             TypeSynonymInstances #-}

module RPGServer.DB.Postgres ( Conn(..),
                               ConnectInfo(..) ) where

import RPGServer.DB.Postgres.Common   ( Conn(..),
                                        ConnectInfo(..) )
import RPGServer.DB.Postgres.Class    ( )
import RPGServer.DB.Postgres.Log      ( )
