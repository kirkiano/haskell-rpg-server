{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts,
             AllowAmbiguousTypes,
             OverloadedStrings #-}
module Main ( main ) where

import RPGServer.Common
import Control.Monad.Trans.State            ( evalStateT )
import qualified Control.Concurrent         as C
import Control.Concurrent.STM               ( atomically )
import Control.Concurrent.STM.TQueue        ( newTQueue,
                                              readTQueue,
                                              writeTQueue )
import qualified Data.Map                   as M
import System.IO                            ( stdout )
import SendReceive                          ( ReceiveResult(Received) )
import qualified System.Posix.Signals       as P
import RPGServer.Util.Fork                  ( )
import qualified RPGServer.Global           as G
import qualified RPGServer.Listen.Socket    as S
import RPGServer.Listen.Driver              ( driverAction )
import RPGServer.Game.Loop                  ( gameLoop,
                                              LoopState(LoopState) )


data TopEnv = TopEnv {
    env                  :: G.Env
  , socketListenerTID    :: C.ThreadId
  }


instance Show TopEnv where
  show te = ("sock thread: " ++ show (socketListenerTID te) ++
             "\nenv: " ++ show (env te))


main :: IO ()
main = G.getSettings >>= go where
  go s = print s >> putStrLn "" >> bracket startup (shutdown s) queryUser where
    lh = (G.logThresh s, stdout)
    startup = do
      e     <- runReaderT (runReaderT G.createEnv s) lh
      q     <- atomically newTQueue
      let dequeue   = Received <$> (liftIO . atomically . readTQueue $ q)
          sPort     = G.tcpPort s
          toGame x  = liftIO $ atomically $ writeTQueue q x >> return True
          -- SR.Send m (Request, SR.Send m Message) -> a -> m ()
          slst      = S.listen sPort $ driverAction toGame
          frk       = C.forkIO . (G.runG e lh)
      _     <- frk $ evalStateT (gameLoop dequeue) $ LoopState M.empty
      slTID <- frk slst
      return $ TopEnv e slTID


queryUser :: TopEnv -> IO ()
queryUser _ = do
  tid <- C.myThreadId
  _ <- P.installHandler P.keyboardSignal (P.Catch $ C.killThread tid) Nothing
  forever $ do
    ln <- getLine
    if ln == "quit"
      then C.throwTo tid $ userError "terminate"
      else putStrLn $ "Could not understand: " ++ ln


shutdown :: G.Settings -> TopEnv -> IO ()
shutdown s te = do
  let lh  = (G.logThresh s, stdout)
      err = userError "terminate"
  C.throwTo (socketListenerTID    te) err
  runReaderT (G.destroyEnv $ env te) lh
