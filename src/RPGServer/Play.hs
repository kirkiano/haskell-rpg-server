{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module RPGServer.Play ( P,
                        Play(..),
                        HasCID(..),
                        myID ) where

import Prelude                      hiding ( length )
import RPGServer.Common
import RPGServer.Util.Text          hiding ( null )
import RPGServer.Error                     ( PlayerError )
import qualified RPGServer.World           as W


-- | A monad that knows which character is playing.
class Monad m => HasCID m where
  -- | Get the `CharacterID` of the character playing in this monad.
  getCID :: m W.CharacterID

myID :: (HasCID m, MonadTrans t) => t m W.ThingID
myID = lift getCID


type P = ExceptT PlayerError

-- | A monad representing the play of a specific character.
class HasCID m => Play m where
  join            ::                                 P m ()
  whoAmI          ::                                 P m W.Thing
  myHandle        ::                                 P m W.THandle
  whereAmI        ::                                 P m W.Place
  whatIsHere      ::                                 P m [W.THandle]
  whoseIDsAreHere ::                                 P m [W.CharacterID]
  exits           ::                                 P m [W.Exit]
  -- | leave the current place by this exit, and return it, the characters in
  -- | the old place, those in the new place, and my handle
  exit            :: W.ExitID ->
    P m (W.Exit, [W.CharacterID], [W.CharacterID], W.THandle)
  -- | if input is nonwhitespace, then say it and return it and hearers
  say             :: Text        ->                  P m (Text, [W.CharacterID])
  -- | if input is nonwhitespace, then say it and return it and observers
  whisper         :: Text        -> W.CharacterID -> P m (Text, [W.CharacterID])
  describeThing   :: W.ThingID   ->                  P m Text
  editMe          :: W.ThingDesc ->                  P m ()
  quit            ::                                 P m ()
