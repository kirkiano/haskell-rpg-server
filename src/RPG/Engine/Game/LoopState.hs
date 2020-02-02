
module RPG.Engine.Game.LoopState ( L,
                                   LoopState(LoopState),
                                   notify,
                                   isCharIDRegistered,
                                   addEventSender,
                                   dropEventSender ) where

import RPG.Engine.Common           hiding ( say, handle )
import Control.Monad.Trans.State          ( StateT, gets, modify' )
import qualified Data.Set                 as S
import qualified Data.Map                 as M
import qualified System.Log               as L
import qualified RPG.Engine.Log           as L
import RPG.World                          ( CharID )
import RPG.Event                          ( Event )


type L m = StateT (LoopState m) m

data LoopState m = LoopState { emap :: M.Map CharID (Event -> m ()) }


notify :: (MonadIO m, L.Log m L.Game) => Event -> S.Set CharID -> L m ()
notify evt cids = lgE >> mapM_ notifyOne cids where
  lgE           = L.log L.Debug $ L.EmittingEvent evt cids
  notifyOne cid = gets emap >>= maybe (lgF cid) ok . (M.lookup cid) where
    ok s = lift $ s evt
    lgF  = L.log L.Critical . L.SendingFunction L.NoSendingFunction


addEventSender :: Monad m => CharID -> (Event -> m ()) -> L m ()
addEventSender cid sndr = modify' $ LoopState . (M.insert cid sndr) . emap


dropEventSender :: Monad m => CharID -> L m ()
dropEventSender cid = modify' $ LoopState . M.delete cid . emap


isCharIDRegistered :: Monad m => CharID -> L m Bool
isCharIDRegistered cid = M.member cid <$> gets emap

-----------------------------------------------------------
-- log

instance (Monad m, L.LogThreshold m) => L.LogThreshold (L m) where
  logThreshold = lift L.logThreshold

instance L.Log m a => L.Log (L m) a where
  logWrite lev = lift . (L.logWrite lev)
