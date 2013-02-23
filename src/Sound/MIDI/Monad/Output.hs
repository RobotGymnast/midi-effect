{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad.Output ( tempo
                               , instrument
                               , flush
                               , alsaNote
                               , startNote
                               , stopNote
                               , playNotes
                               ) where

import Prelewd

import IO
import Impure
import STM

import Data.Word
import Storage.List
import Storage.Map
import Storage.Refcount
import Storage.Set

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
alsaNote :: Note -> Word8 -> E.Note
alsaNote note c = E.simpleNote (E.Channel c) (E.Pitch $ pitch note) (E.Velocity $ vcty note)

event :: Tick -> E.Data -> MIDI ()
event start e = ioMIDI $ \cxt -> let
            ev = (E.forConnection (connOut cxt) e)
                { E.queue = qT cxt
                , E.time = T.consRel $ T.Tick start
                }
        in io $ void $ E.output (seqT cxt) ev

startNote, stopNote :: Tick -> Note -> MIDI ()
startNote t note = do
        c <- instrument (instr note)
        _ <- ioMIDI $ atomically . (`modifyTVar` refInsert c) . channels
        event t $ E.NoteEv E.NoteOn $ alsaNote note c

stopNote t note = do
        c <- lookup (instr note) <$> ioMIDI (atomically . readTVar . instrs) <&> (<?> error "Note not started")
        _ <- ioMIDI $ atomically . (`modifyTVar` \m -> refDelete c m <?> m) . channels
        event t $ E.NoteEv E.NoteOff $ alsaNote note c

instrument :: Instrument -> MIDI Word8
instrument i = do
                chnls <- ioMIDI $ \cxt -> atomically $ readTVar $ instrs cxt
                return <$> lookup i chnls <?> allocChannel chnls
    where
        allocChannel chnls = let c = unusedChannel chnls
                                 e = E.Ctrl (E.Channel c) (E.Parameter 0) $ E.Value $ fromIntegral i
                             in do event 0 $ E.CtrlEv E.PgmChange e
                                   ioMIDI $ \cxt -> atomically $ c <$ modifyTVar (instrs cxt) (insert i c)

        unusedChannel chnls = let midiChannels = [0..9] <> [11..15]
                              in head (toList $ set midiChannels \\ set (toList chnls)) <?> error "Out of channels"

-- | Play a melody and flush the buffer
playNotes :: Foldable t => t (Tick, Tick, Note) -> MIDI ()
playNotes notes = traverse_ playNote notes >> flush
    where
        playNote (start, duration, note) = startNote start note >> stopNote (start + duration) note
