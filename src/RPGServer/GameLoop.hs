{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module RPGServer.GameLoop ( gameLoop ) where

import RPGServer.Common       hiding ( say, handle)
import Control.Concurrent.STM        ( atomically )
import Control.Concurrent.STM.TQueue ( TQueue,
                                       readTQueue )
import qualified System.Log          as L
import qualified RPGServer.Log       as L
import RPGServer.World               ( CharacterID )
import RPGServer.Message             ( Message(..) )
import RPGServer.Value               ( Value(Error) )
import RPGServer.Request             ( Request(..) )
import RPGServer.Play                ( Play(..), P, HasCID )
import RPGServer.PlayerState         ( PlayerState(..) )
import RPGServer.DB.Class            ( PlayDB )


type Forward m = Message -> [CharacterID] -> m ()


gameLoop :: (MonadIO m,
             L.Log m L.Game,
             PlayDB m)
             =>
             TQueue (CharacterID, Request) -> Forward m -> m ()
gameLoop requests fw = forever $ getNextRequest >>= uncurry handle where
  getNextRequest = liftIO $ atomically $ readTQueue requests
  handle cid req = either bad good =<< resp req where
    good (vM, ers) = do maybe (return ()) (\m -> fw (ValueMessage m) [cid]) vM
                        let mrs = map (\(evt, rs) -> (EventMessage evt, rs)) ers
                        void $ forM mrs $ uncurry fw
    bad e          = fw (ValueMessage $ Error e) [cid]
    resp q         = runReaderT (runExceptT $ respond q) (PlayerState cid)


respond :: (PlayDB m, HasCID m, L.Log m L.Game) => Request -> P m
respond WhoAmI                = whoAmI
respond WhereAmI              = whereAmI
respond WhatIsHere            = whatIsHere
respond WaysOut               = exits
respond (Exit eid)            = exit eid
respond (Say s)               = say s
respond (Whisper s tid)       = whisper s tid
respond (EditMe d)            = editMe d
respond Join                  = join
respond Quit                  = quit
respond (DescribeThing tid)   = describeThing tid
respond _                     = return (Nothing, [])