{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad ( module Monad
                        , module Types
                        , tempo
                        , alsaNote
                        , startNote
                        , stopNote
                        , playNotes
                        ) where

import Prelewd

import IO

import Data.Foldable as F
import Data.Word

import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer.Time as T

import Sound.MIDI.Monad.Core as Monad
import Sound.MIDI.Monad.Types as Types

tempo :: Word32     -- ^ Microseconds per beat
      -> MIDI
tempo t = ioMIDI $ \(h, q, _) -> io $ Q.control h q (E.QueueTempo $ E.Tempo $ fromIntegral t) Nothing

flush :: MIDI
flush = ioMIDI $ \(h, _, _) -> io $ void $ E.drainOutput h

-- | Convert Note to Sound.ALSA.Sequence.Event.Note
alsaNote :: Note -> E.Note
alsaNote note = E.simpleNote (E.Channel $ instr note)
                             (E.Pitch $ pitch note)
                             (E.Velocity $ vcty note)

noteEvent :: E.NoteEv -> Word32 -> Note -> MIDI
noteEvent e start note = ioMIDI $ \(h, q, conn) -> let
            event = (E.forConnection conn $ E.NoteEv e $ alsaNote note)
                { E.queue = q
                , E.time = T.consAbs $ T.Tick start
                }
        in io $ void $ E.output h event

startNote, stopNote :: Tick -> Note -> MIDI
[startNote, stopNote] = noteEvent <$> [E.NoteOn, E.NoteOff]

-- | Play a melody and flush the buffer
playNotes :: Foldable t => t (Tick, Tick, Note) -> MIDI
playNotes notes = F.mapM_ playNote notes >> flush
    where
        playNote (start, duration, note) = startNote start note >> stopNote (start + duration) note
