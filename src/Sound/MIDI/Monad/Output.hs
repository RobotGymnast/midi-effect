{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad.Output ( tempo
                               , instrument
                               , flush
                               , toALSA
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

event :: Tick -> E.Data -> MIDI ()
event start e = ioMIDI $ \cxt -> let
            ev = (E.forConnection (connOut cxt) e)
                { E.queue = qT cxt
                , E.time = T.consRel $ tickALSA start
                }
        in io $ void $ E.output (seqT cxt) ev

noteEvent :: Tick -> E.NoteEv -> Note -> E.Channel -> MIDI ()
noteEvent t e note = event t . E.NoteEv e . toALSA (pitch note, vcty note)

startNote, stopNote :: Tick -> Note -> MIDI ()
startNote t note = do
        c <- instrument (instr note)
        _ <- ioMIDI $ atomically . (`modifyTVar` refInsert c) . channels
        noteEvent t E.NoteOn note c

stopNote t note@(Note { instr = Percussion }) = noteEvent t E.NoteOff note percussionChannel
stopNote t note@(Note { instr = Instrument i }) = do
        c <- lookup i <$> ioMIDI (atomically . readTVar . instrs) <&> (<?> error "Note not started")
        _ <- ioMIDI $ atomically . (`modifyTVar` \m -> refDelete c m <?> m) . channels
        noteEvent t E.NoteOff note c

instrument :: Instrument -> MIDI E.Channel
instrument Percussion = return percussionChannel
instrument (Instrument i) = do
                chnls <- ioMIDI $ \cxt -> atomically $ readTVar $ instrs cxt
                return <$> lookup i chnls <?> allocChannel chnls
    where
        allocChannel chnls = let c = unusedChannel chnls
                                 e = E.Ctrl c (E.Parameter 0) $ E.Value $ fromIntegral i
                             in do event 0 $ E.CtrlEv E.PgmChange e
                                   ioMIDI $ \cxt -> atomically $ c <$ modifyTVar (instrs cxt) (insert i c)

        unusedChannel chnls = let midiChannels = set (E.Channel <$> [0..15]) \\ set [percussionChannel]
                              in head (toList $ set midiChannels \\ set (toList chnls)) <?> error "Out of channels"

-- | Play a melody and flush the buffer
playNotes :: Foldable t => t (Tick, Tick, Note) -> MIDI ()
playNotes notes = traverse_ playNote notes >> flush
    where
        playNote (start, duration, note) = startNote start note >> stopNote (start + duration) note
