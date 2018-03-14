{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric,
             OverloadedStrings,
             RankNTypes,
             UndecidableInstances,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses #-}

module RPGServer.Listen.Wrap ( wrapConn,
                               Wrapped ) where

import RPGServer.Util.ByteString
import Prelude hiding                          ( concat )
import qualified Data.ByteString.Lazy          as BZ
import qualified Data.ByteString.Lazy.UTF8     as BZU
import qualified Data.Aeson                    as J
import qualified SendReceive                   as SR
import qualified RPGServer.Listen.Connection   as C
import RPGServer.Request                       ( Request )
import RPGServer.Message                       ( Message )


type Wrapped m a =
  SR.WrapSendReceive a m Message ByteString ByteString String Request


wrapConn :: (SR.Sender a m ByteString,
             SR.WaitingReceiver a m ByteString ) => a -> Wrapped m a
wrapConn c = SR.wrapSendReceive
             c
             (return . BZ.toStrict. J.encode)
             (return . J.eitherDecodeStrict')
             (\e -> BZ.toStrict $
                    BZ.concat ["{ \"parseError\": \"", BZU.fromString e, "\"}"])


instance C.Connection m a => C.Connection m (Wrapped m a) where
  connectionType = C.connectionType . SR.getWrapped
  timeOpened     = C.timeOpened     . SR.getWrapped
  close          = C.close          . SR.getWrapped
  isOpen         = C.isOpen         . SR.getWrapped


instance Show a => Show (Wrapped m a) where
  show = show . SR.getWrapped


instance (Show a,
          C.Connection m a ,
          SR.Sender a m ByteString,
          SR.WaitingReceiver a m ByteString) => C.Client m (Wrapped m a)
