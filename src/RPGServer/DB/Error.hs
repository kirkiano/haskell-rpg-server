{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module RPGServer.DB.Error ( D, DBError(..) ) where

import Control.Monad.Trans.Except ( ExceptT )


type D = ExceptT DBError

data DBError = Empty
             | NotSingleton
             | NoUpdate
             | MultipleUpdate
             | InternalError String
             deriving Show
