{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances,
             UndecidableInstances,
             MultiParamTypeClasses #-}

module RPGServer.Util.ByteString ( ByteString
                                 , FromBSMap(..)
                                 , ShowBS(..)
                                 , bs2s
                                 , concat
                                 , length) where

import Prelude hiding         ( concat
                              , length )
import Data.Char              ( chr )
import Data.Map               ( Map )
import Data.ByteString        ( ByteString
                              , concat
                              , length
                              , pack
                              , unpack )

------------------------------------------------------------

class ShowBS a where
  showBS :: a -> ByteString

instance Show a => ShowBS a where
  showBS = s2bs . show

------------------------------------------------------------

class FromBSMap a where
  fromBSMap :: Map ByteString ByteString -> Maybe a

------------------------------------------------------------

{-
saybn :: ByteString -> IO ()
saybn = sayn . (map (chr . fromEnum)) . unpack

-}

bs2s :: ByteString -> String
bs2s = map (chr . fromEnum) . unpack


s2bs :: String -> ByteString
s2bs = pack . (map $ fromIntegral . fromEnum)

{-
i2bs :: Int -> ByteString
i2bs = s2bs . show
-}
