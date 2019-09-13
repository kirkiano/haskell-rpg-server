{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections,
             FlexibleContexts #-}

module RPGServer.Listen.Driver ( driverAction ) where

import RPGServer.Common
import RPGServer.Util.Fork                     ( HasFork )
import qualified Data.Set                      as S
import qualified System.Log                    as L
import qualified RPGServer.Log                 as L
import SendReceive                             as SR
import RPGServer.Request                       ( Request(..),
                                                 PlayerRequest(..))
import RPGServer.Message                       ( Message(..) )
import RPGServer.DB.Class                      ( PlayDB )
import RPGServer.Listen.Connection             ( Client,
                                                 Connection(closeClientQuit) )


driverAction :: (MonadIO m,
                 HasFork m,
                 PlayDB m,
                 Client m a,
                 L.Log m L.Drive)
                =>
                SR.Send m (Request, SR.Send m Message) -> a -> m ()
driverAction sendToGameLoop driver = loop S.empty where
  loop cids0 = waitRecv driver >>= process where
    process (SR.Received q)  = do
      cids1 <- updateCids q
      void $ sendToGameLoop (q, send driver)
      loop cids1
    process SR.CannotReceive = do -- dismiss driver & shut down its active cids
      closeClientQuit driver
      mapM_ (sendToGameLoop . (, send driver) . (flip PlayerRequest Quit)) cids0
    updateCids (PlayerRequest cid Join) = do logReg True  cid
                                             return $ S.insert cid cids0
    updateCids (PlayerRequest cid Quit) = do logReg False cid
                                             return $ S.delete cid cids0
    updateCids _                        = do return cids0
    logReg isReg = L.log L.Info . ctor where
      ctor = if isReg then L.RegisteringPlayer else L.DeregisteringPlayer
