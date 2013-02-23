{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad.Output ( tempo
                               , flush
                               , alsaNote
                               , startNote
                               , stopNote
                               , playNotes
                               ) where

import Prelewd

import IO

import Data.Word

import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer.Time as T

import Sound.MIDI.Monad.Core as Monad
import Sound.MIDI.Monad.Types as Types

tempo :: Word32     -- ^ Microseconds per beat
      -> MIDI ()
tempo t = ioMIDI $ \cxt -> io $ Q.control (seqT cxt) (qT cxt) (E.QueueTempo $ E.Tempo $ fromIntegral t) Nothing

flush :: MIDI ()
flush = ioMIDI $ \cxt -> io $ void $ E.drainOutput $ seqT cxt

-- | Convert Note to Sound.ALSA.Sequence.Event.Note
alsaNote :: Note -> E.Note
alsaNote note = E.simpleNote (E.Channel $ instr note)
                             (E.Pitch $ pitch note)
                             (E.Velocity $ vcty note)

noteEvent :: E.NoteEv -> Tick -> Note -> MIDI ()
noteEvent e start note = ioMIDI $ \cxt -> let
            event = (E.forConnection (connOut cxt) $ E.NoteEv e $ alsaNote note)
                { E.queue = qT cxt
                , E.time = T.consRel $ T.Tick start
                }
        in io $ void $ E.output (seqT cxt) event

startNote, stopNote :: Tick -> Note -> MIDI ()
[startNote, stopNote] = noteEvent <$> [E.NoteOn, E.NoteOff]

-- | Play a melody and flush the buffer
playNotes :: Foldable t => t (Tick, Tick, Note) -> MIDI ()
playNotes notes = traverse_ playNote notes >> flush
    where
        playNote (start, duration, note) = startNote start note >> stopNote (start + duration) note
