{-# OPTIONS_GHC -Wall #-}

module RPGServer.World.Common ( Desc(..),
                                ID(..),
                                Name(..) ) where

import Data.Text ( Text )


class ID a where
  idn :: a -> Int

class Name a where
  name :: a -> Text

class Desc a where
  desc :: a -> Text
