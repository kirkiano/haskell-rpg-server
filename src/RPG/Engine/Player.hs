
module RPG.Engine.Player ( Player(Player) ) where

import Prelude hiding                   ( getContents, lookup )
import Database.CRUD
import RPG.Engine.Common
import RPG.Engine.Log                   as L
import RPG.World                        ( CharID )
import RPG.Engine.Play                  ( HasCID(..) )


newtype Player = Player { _cid :: CharID }

type PS = ReaderT Player -- player state

instance Monad m => HasCID (PS m) where
  getCID = asks _cid

instance (Monad m, L.LogThreshold m) => L.LogThreshold (PS m) where
  logThreshold = lift L.logThreshold

instance L.Log m l => L.Log (PS m) l where
  logWrite lev = lift . (L.logWrite lev)

------------------------------------------------------------

instance Save e m i o => SaveM e m Player i o where
 saveM i _ = save i

instance Lookup e m i o => LookupM e m Player i o where
  lookupM i _ = lookup i

instance Absent e m i => AbsentM e m Player i where
  assertAbsentM i _ = assertAbsent i

instance Update e m i => UpdateM e m Player i where
  updateM i _ = update i

instance Upsert e m i o => UpsertM e m Player i o where
  upsertM i _ = upsert i

instance Delete e m i => DeleteM e m Player i where
  deleteM i _ = delete i
