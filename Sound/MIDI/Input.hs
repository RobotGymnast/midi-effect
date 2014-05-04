{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Sound.MIDI.Input ( midiIn
                        ) where

import Prelude ()
import BasicPrelude as Base

import Control.Eff
import Control.Eff.Lift as Eff
import Control.Eff.State.Strict
import Control.Lens
import Data.HashMap.Strict as HashMap

import Sound.MIDI.Core
import Sound.MIDI.Types

import qualified Sound.ALSA.Sequencer.Event as E
import Sound.ALSA.Exception

-- | Accumulate a monadic value while a monadic predicate is True
whileM :: (Applicative m, Monad m) => m Bool -> m a -> m [a]
whileM p m = p >>= \b -> if b then (:) <$> m <*> whileM p m else return []

-- | MIDI input events
midiIn :: MIDI env
       => Eff env [(Maybe Velocity, Note)]
midiIn = do
    s <- get
    let h = view seqT s
    Eff.lift
        $ mapMaybe (fromEvent $ view sourceToInstr s)
      <$> whileM
              (hasInput h)
              (E.input h)
  where
        hasInput h
            = (E.inputPending h True <&> (> 0))
              `Sound.ALSA.Exception.catch` \_ -> return False

fromEvent :: HashMap MIDIAddress Instrument -> E.T -> Maybe (Maybe Velocity, Note)
fromEvent instrs e
    = case E.body e of
        E.NoteEv state note ->
          let
            (v, p) = fromALSA note
            source = E.source e
          in HashMap.lookup source instrs
          <&> \instrument -> (guard (state /= E.NoteOff) >> v, (p, instrument))
        _ -> Nothing
