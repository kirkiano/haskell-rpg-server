
module RPG.Engine.Global.Env ( Env(..),
                               G,
                               runG,
                               gCatch,
                               gBracket,
                               gBracketOnError,
                               createEnv,
                               destroyEnv ) where

import RPG.Engine.Common
import Prelude hiding                        ( getContents )
import Data.IORef                            ( IORef )
import GHC.IO.Handle                         ( Handle )
import qualified SendReceive                 as SR
import qualified Database.PostgreSQL.Simple  as P
import Database.CRUD
import qualified RPG.Engine.Log              as L
import qualified RPG.Engine.Global.Settings  as S
import RPG.Error.Data                        ( Error )
import qualified RPG.DB                      as DB


type DBase = DB.Cover (L.Logging (IORef DB.Maps)) (L.Logging P.Connection)

data Env = Env { dBase   :: L.Logging DBase,
                 saveUtt :: Bool }
         deriving Show

------------------------------------------------------------

type G = ReaderT Env L.L

runG :: Env -> (L.Level, Handle) -> G a -> IO a
runG env lh g = runReaderT (runReaderT g env) lh

gCatch :: Exception e => G a -> (e -> G a) -> G a
gCatch act hdl = do
  env <- ask
  lh  <- lift ask
  liftIO $ catch (runG env lh act) (\e -> runG env lh (hdl e))

gBracket :: G a -> (a -> G ()) -> (a -> G b) -> G b
gBracket create destroy action = do
  env <- ask
  lh <- lift ask
  liftIO $ bracket
    (runG env lh create)
    (\r -> runG env lh $ destroy r)
    (\r -> runG env lh $ action r)

gBracketOnError :: G a -> (a -> G ()) -> (a -> G b) -> G b
gBracketOnError create destroy action = do
  env <- ask
  lh <- lift ask
  liftIO $ bracketOnError
    (runG env lh create)
    (\r -> runG env lh $ destroy r)
    (\r -> runG env lh $ action r)

------------------------------------------------------------

instance L.LogThreshold G where
  logThreshold = lift L.logThreshold

instance Show a => L.Log G a where
  logWrite lev = lift . (L.logWrite lev)

------------------------------------------------------------

type DBConnError = Either (Either () IOError) Error

createEnv :: (MonadIO m) => ExceptT DBConnError (ReaderT S.Settings m) Env
createEnv = Env <$> connDB <*> (lift $ asks S.saveUtterances) where
  connDB  = do pg <- lift $ asks S.pgSettings
               SR.connect $ L.Logging L.Debug (L.Logging L.Debug (),
                                               L.Logging L.Debug pg)

destroyEnv :: (MonadIO m,
               L.Log m (SR.Disconnected P.Connection),
               L.Log m (SR.Disconnected (IORef DB.Maps)),
               L.Log m (SR.Disconnected DBase)) =>
              Env -> m ()
destroyEnv = flip SR.disconnect Nothing . dBase

------------------------------------------------------------

instance (Monad m,
          SaveM e m DBase i o,
          L.Log m (SaveLog DBase i o e)) => SaveM e m Env i o where
  saveM i = saveM i . dBase

instance (Monad m,
          LookupM e m DBase i o,
          L.Log m (LookupLog DBase i o e)) => LookupM e m Env i o where
  lookupM i = lookupM i . dBase

instance (Monad m, AbsentM e m DBase i) => AbsentM e m Env i where
  assertAbsentM i = assertAbsentM i . dBase

instance (Monad m,
          UpdateM e m DBase i,
          L.Log m (UpdateLog DBase i e)) => UpdateM e m Env i where
  updateM i = updateM i . dBase

instance (Monad m,
          UpsertM e m DBase i o,
          L.Log m (UpsertLog DBase i o e)) => UpsertM e m Env i o where
  upsertM i = upsertM i . dBase

instance (Monad m,
          DeleteM e m DBase i,
          L.Log m (DeleteLog DBase i e)) => DeleteM e m Env i where
  deleteM i = deleteM i . dBase
