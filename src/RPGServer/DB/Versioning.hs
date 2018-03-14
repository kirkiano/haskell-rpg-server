{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses #-}
module RPGServer.DB.Versioning () where  {- CreateRead(..),
                                 VersioningCreateRead(..),
                                 VK(..) ) where

import Prelude hiding        ( concat )
import Data.ByteString       ( concat )
import Data.ByteString.Char8 ( split )
import RPGServer.Util        ( ShowBS(..),
                               ReadBSMaybe(..),
                               toMaybe )
import RPGServer.DB.CRUD     ( CR(..),
                               CRUD(..) )

------------------------------------------------------------

data VK a = VK a Int -- versioned key

instance (ShowBS a) => ShowBS (VK a) where
  showBS (VK k v) = concat [showBS k, "-", showBS v]

instance (ReadBSMaybe a) => ReadBSMaybe (VK a) where
  readBSMaybe s =
    let pcs = split '-' s in
    if length pcs < 2
      then Nothing
      else VK <$> readBSMaybe $ concat $ init pcs
              <*> readBSMaybe $ last pcs

------------------------------------------------------------

class (Monad m) => Versioner m a k where
  incrementVersion :: a -> k -> m ()
  latestVersion :: a -> k -> m Int


instance (ShowBS k,
          ReadBSMaybe k,
          CR a m (VK k) v) =>
         CRUD a m k v where
  create db (VCR db) (VK i) x = create db
-}
