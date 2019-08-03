{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts,
             AllowAmbiguousTypes,
             OverloadedStrings #-}
module Main ( main ) where

import RPGServer.Common
import qualified Control.Concurrent         as C
import Control.Concurrent.STM               ( atomically )
import Control.Concurrent.STM.TQueue        ( newTQueue,
                                              writeTQueue )
import System.IO                            ( stdout )
import qualified System.Posix.Signals       as P
import qualified SendReceive                as SR
import qualified Forwarder                  as F
import qualified RPGServer.Global           as G
import RPGServer.World                      ( CharacterID )
import qualified RPGServer.Listen.Websocket as WS
import qualified RPGServer.Listen.Socket    as S
import RPGServer.Listen.Bounce              ( admit
                                            , bounce )
import RPGServer.GameLoop                   ( gameLoop )


data TopEnv = TopEnv {
    env                  :: G.Env
  , forwardTWM           :: G.F G.G
  , fwdWatcherTID        :: C.ThreadId
  , websocketListenerTID :: C.ThreadId
  , socketListenerTID    :: C.ThreadId
  }


instance Show TopEnv where
  show te = ("sock thread: " ++ show (socketListenerTID te) ++
             "\nwebsock thread: " ++ show (websocketListenerTID te) ++
             "\nenv: " ++ show (env te))
  

main :: IO ()
main = G.getSettings >>= go where
  go s = do print s >> putStrLn ""
            bracket startup (shutdown s) queryUser
    where
    lh = (G.logThresh s, stdout)
    startup = do
      e     <- runReaderT (runReaderT G.createEnv s) lh
      f     <- G.runG e lh G.createForwarderTWM -- :: IO (G.F G.G)
      fwtch <- G.runG e lh $ G.forkForwarderWatcher f 5000000
      masterQueue <- atomically newTQueue
      let sndfw     = void . (F.sendFwdTWM f Nothing) -- ignore response
          fw m      = sndfw . (F.Forward m)
          dereg :: CharacterID -> G.G ()
          dereg     = sndfw . F.Deregister . (:[])
          reg cid c = sndfw $ F.Register [(cid, SR.send c)]
          tOut      = G.connTimeout s
          tries     = G.connTries s
          sPort     = G.tcpPort s
          wPort     = G.websockPort s
          ssPort    = G.sessionServerPort s
          sendToGameLoop = liftIO . atomically . (\q -> writeTQueue masterQueue q >> return True)
          slst      = S.listen  sPort $ bounce reg dereg sendToGameLoop tOut tries
          wlst      = WS.listen ssPort wPort
                      (\c cid -> admit reg dereg sendToGameLoop c cid)
          frk       = C.forkIO . (G.runG e lh)
      _         <- frk $ gameLoop masterQueue fw
      slTID     <- frk slst
      wlTID     <- frk wlst
      return $ TopEnv e f fwtch wlTID slTID


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
  C.throwTo (websocketListenerTID te) err
  C.throwTo (fwdWatcherTID        te) err
  G.runG (env te) lh (G.destroyForwarderTWM $ forwardTWM te)
  runReaderT (G.destroyEnv $ env te) lh
