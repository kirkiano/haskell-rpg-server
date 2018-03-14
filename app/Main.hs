{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts,
             AllowAmbiguousTypes,
             OverloadedStrings #-}
module Main ( main ) where

import RPGServer.Common
import qualified Control.Concurrent         as C
import qualified Control.Concurrent.MVar    as M
import qualified System.Posix.Signals       as P
import qualified SendReceive                as SR
import qualified Forwarder                  as F
import qualified RPGServer.Global           as G
import RPGServer.World                      ( CharacterID )
import RPGServer.Message                    ( Message )
import qualified RPGServer.Listen.Websocket as WS
import qualified RPGServer.Listen.Socket    as S
import RPGServer.Listen.Bounce              ( bounce )


data TopEnv = TopEnv {
  env                  :: G.Env,
  websocketListenerTID :: C.ThreadId,
  socketListenerTID    :: C.ThreadId
  }


instance Show TopEnv where
  show te = ("sock thread: " ++ show (socketListenerTID te) ++
             "\nwebsock thread: " ++ show (websocketListenerTID te) ++
             "\nenv: " ++ show (env te))
  

main :: IO ()
main = G.getSettings >>= go where
  go s = do print s >> putStrLn ""
            bracket startup shutdown queryUser
    where
    startup = do
      e <- runReaderT (runReaderT G.createEnv s) lh
      f <- G.runG e lh G.createForwardTWM :: IO (G.F G.G)
      let sndf :: F.Action CharacterID G.G Message -> G.G ()
          sndf a    = void $ SR.send f $ F.Request a (const $ return True)
          fw m      = sndf . (F.Forward m)
          dereg :: CharacterID -> G.G ()
          dereg     = sndf . F.Deregister . (:[])
          reg cid c = sndf $ F.Register [(cid, SR.send c)]
          tOut      = G.connTimeout s
          tries     = G.connTries s
          sPort     = G.tcpPort s
          wPort     = G.websockPort s
          slst      = S.listen  sPort (bounce reg dereg fw alyHr tOut tries)
          wlst      = WS.listen wPort (bounce reg dereg fw alyHr tOut tries)
          frk       = C.forkIO . (G.runG e lh)
          alyHr :: CharacterID -> G.G Bool
          alyHr cid = do
            v <- liftIO M.newEmptyMVar
            let resp :: F.Response CharacterID G.G Message -> G.G Bool
                resp r = liftIO $ M.putMVar v r >> return True
            void $ liftIO $ SR.send f $ F.Request (F.GetRegistrant cid) resp
            r <- liftIO $ M.readMVar v
            return $ case r of
              F.Registrant _ _ -> True
              _                -> False
      sltid <- frk slst
      wltid <- frk wlst
      return $ TopEnv e wltid sltid
    shutdown te = do
      let e = userError "terminate"
      C.throwTo (socketListenerTID    te) e
      C.throwTo (websocketListenerTID te) e
      runReaderT (G.destroyEnv $ env te) lh
    lh = (G.logThresh s, stdout)


queryUser :: TopEnv -> IO ()
queryUser _ = do
  tid <- C.myThreadId
  _ <- P.installHandler P.keyboardSignal (P.Catch (C.killThread tid)) Nothing
  forever $ do
    ln <- getLine
    if ln == "quit"
      then C.throwTo tid (userError "terminate")
      else do putStrLn $ "Could not understand: " ++ ln
