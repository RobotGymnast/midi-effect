{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Sound.MIDI.Input ( midiIn
                        ) where

import Prelude ()
import BasicPrelude hiding (lift, lookup)

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Lens
import Data.HashMap.Strict

import Sound.MIDI.Core
import Sound.MIDI.Types

import qualified Sound.ALSA.Sequencer.Event as E

-- | Accumulate a monadic value until a monadic predicate is True
repeatM :: (Applicative m, Monad m) => m Bool -> m a -> m [a]
repeatM p m = p >>= \b -> if b then return [] else (:) <$> m <*> repeatM p m

-- | MIDI input events
midiIn :: MIDI env
       => Eff env [(Maybe Velocity, Note)]
midiIn = get
     >>= lift . (ioFetchConvertedEvents <$> view sourceToInstr <*> view seqT)
    where
        ioFetchConvertedEvents instrs h = mapMaybe (fromEvent instrs)
                                      <$> repeatM (E.inputPending h False <&> (== 0))
                                                  (E.input h)

fromEvent :: HashMap MIDIAddress Instrument -> E.T -> Maybe (Maybe Velocity, Note)
fromEvent instrs e = case E.body e of
        E.NoteEv state note -> let (v, p) = fromALSA note
                               in Just (guard (state /= E.NoteOff) >> v, (p, instrument))
        _ -> Nothing
    where
        instrument = fromMaybe (error "Unknown input source") (lookup (E.source e) instrs)
