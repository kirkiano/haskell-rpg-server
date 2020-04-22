
module Main ( main ) where

import Prelude                       hiding ( init )
import RPG.Engine.Common
import Control.Monad.Trans.State            ( evalStateT )
import qualified Control.Concurrent         as C
import Control.Exception                    ( throw )
import Control.Concurrent.STM               ( atomically )
import Control.Concurrent.STM.TQueue        ( newTQueue,
                                              readTQueue,
                                              writeTQueue )
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import System.IO                            ( stdout )
import qualified System.Posix.Signals       as P
import qualified RPG.Engine.Global          as G
import qualified RPG.Engine.Listen.Socket   as K
import RPG.Engine.Listen.Driver            ( driverAction )
import RPG.Engine.Game                     ( gameLoop,
                                             LoopState(LoopState) )


data TopEnv = TopEnv {
    env               :: G.Env
  , socketListenerTID :: C.ThreadId
  }

instance Show TopEnv where
  show te = ("sock thread: " ++ show (socketListenerTID te) ++
             "\nenv: " ++ show (env te))


main :: IO ()
main = G.getSettings >>= go where
  go s = print s >> putStrLn "" >> bracket startup (shutdown s) queryUser where
    lh      = (G.logThresh s, stdout)
    startup = init >>= either throw run where
      init     = runReaderT (runReaderT (runExceptT G.createEnv) s) lh
      run _env = do
        q       <- atomically newTQueue
        let dequeue  = liftIO . atomically . readTQueue $ q
            sPort    = G.tcpPort s
            toGame x = liftIO . atomically $ writeTQueue q x
            drive c  = void $ evalStateT (driverAction toGame c) S.empty
            slst     = K.listen sPort drive
            frk      = C.forkIO . (G.runG _env lh)
        _ <- frk $ evalStateT (gameLoop dequeue) $ LoopState M.empty
        TopEnv _env <$> frk slst


queryUser :: TopEnv -> IO ()
queryUser _ = do
  tid <- C.myThreadId
  void $ P.installHandler P.keyboardSignal (P.Catch $ C.killThread tid) Nothing
  forever $ do
    ln <- getLine
    if ln == "quit"
      then C.throwTo tid $ userError "terminate"
      else putStrLn $ "Could not understand: " ++ ln


shutdown :: G.Settings -> TopEnv -> IO ()
shutdown s te = do
  let lh  = (G.logThresh s, stdout)
      err = userError "terminate"
  C.throwTo (socketListenerTID te) err
  runReaderT (G.destroyEnv $ env te) lh
