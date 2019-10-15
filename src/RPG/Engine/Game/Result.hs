
module RPG.Engine.Game.Result where

import RPG.Value             ( Value )
import RPG.Event             ( Event )
import RPG.World             ( CharID )


type Result = (Maybe Value, [(Event, [CharID])])
