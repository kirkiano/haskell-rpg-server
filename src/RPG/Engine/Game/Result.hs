
module RPG.Engine.Game.Result where

import Data.Set              ( Set )
import RPG.Value             ( Value )
import RPG.Event             ( Event )
import RPG.World             ( CharID )


type Result = ([Value], Set (Event, Set CharID))
