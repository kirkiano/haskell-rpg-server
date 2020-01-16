
module RPG.Engine.Listen.Driver ( driverAction ) where

import RPG.Engine.Common
import Control.Monad.Trans.State                ( StateT, get, modify' )
import Control.Monad.Trans.Except               ( catchE, mapExceptT )
import qualified Data.Set                       as S
import qualified System.Log                     as L
import qualified RPG.Engine.Log                 as L
import qualified SendReceive                    as SR
import RPG.World                                ( CharID )
import RPG.Request                              ( Request(..),
                                                  CharRequest(..),
                                                  isJoin,
                                                  isQuit )
import RPG.Message                              ( Message(..) )


type DState = StateT (S.Set CharID)


driverAction :: (MonadIO m,
                 SR.WaitReceiveFrom a m Request,
                 SR.SendTo a m Message,
                 SR.Disconnect m a,
                 L.Log m L.Drive)
                =>
                ((Request, Message -> m ()) -> m ()) -> a -> DState m ()
driverAction toGame d = void . runExceptT $ catchE loop (lift . dismiss) where
  sendDriver = void . runExceptT . (SR.send d)
  loop       = do
    q <- mapExceptT lift $ SR.waitRecv d
    lift $ do maybe (return ()) (regChar  True) $ isJoin q
              maybe (return ()) (regChar False) $ isQuit q
              lift $ toGame (q, sendDriver)
    loop
  dismiss e = do -- problem w/ driver, so dismiss it & shut down active cids
    lift . SR.disconnect d . Just . show $ e
    let f = lift . toGame . (, sendDriver) . (flip CharRequest Quit)
    mapM_ f =<< get


regChar :: (Monad m, L.Log m L.Drive) => Bool -> CharID -> DState m ()
regChar reg cid = lg >> md where
  md = modify' $ (if reg then S.insert else S.delete) cid
  lg = lift $ L.log L.Info (ctor cid) where
    ctor = if reg then L.RegisteringPlayer else L.DeregisteringPlayer
