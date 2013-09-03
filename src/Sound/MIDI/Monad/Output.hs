{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad.Output ( tempo
                               , flush
                               , toALSA
                               , startNote
                               , stopNote
                               , playNotes
                               ) where

import Summit.Data.List
import Summit.Data.Map
import Summit.Data.Refcount
import Summit.Data.Set hiding (insert)
import Summit.IO
import Summit.Impure
import Summit.Prelewd
import Summit.STM

import Data.Word

import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer.Time as T

import Sound.MIDI.Monad.Core as Monad
import Sound.MIDI.Monad.Types as Types

(<??>) :: Monad m => m (Maybe a) -> m a -> m a
(<??>) m a = do x <- m
                return <$> x <?> a

tempo :: Word32     -- ^ Microseconds per beat
      -> MIDI ()
tempo t = ioMIDI $ \cxt -> io $ Q.control (seqT cxt) (qT cxt) (E.QueueTempo $ E.Tempo $ fromIntegral t) Nothing

flush :: MIDI ()
flush = ioMIDI $ \cxt -> io $ void $ E.drainOutput $ seqT cxt

event :: Tick -> E.Data -> MIDI ()
event start e = ioMIDI $ io <<< (multiDestEvent <$> qT <*> seqT <*> connsOut)
    where
        multiDestEvent q sequ = traverse_ $ singleDestEvent q >>> E.output sequ
        singleDestEvent q out = ( E.forConnection out e )
                                { E.queue = q
                                , E.time = T.consRel $ tickALSA start
                                }

noteEvent :: Tick -> E.NoteEv -> Pitch -> Maybe Velocity -> E.Channel -> MIDI ()
noteEvent t e p v = event t . E.NoteEv e . toALSA p (v <?> 0)

startNote :: Word8 -> Tick -> Velocity -> Note -> MIDI ()
startNote drumChannel t v (p, instr) = do
        c <- instrumentChannel drumChannel instr <??> allocateChannel drumChannel instr
        _ <- ioMIDI $ atomically . (`modifyTVar` refInsert c) . channels
        noteEvent t E.NoteOn p (Just v) c

stopNote :: Word8 -> Tick -> Note -> MIDI ()
stopNote drumChannel t (p, instr) = do
        c <- instrumentChannel drumChannel instr <&> (<?> error "Note not started.")
        _ <- noteEvent t E.NoteOff p Nothing c
        void $ ioMIDI $ releaseChannel c
    where
        releaseChannel c = atomically . (`modifyTVar` \m -> refDelete c m <?> m) . channels

instrumentChannel :: Word8 -> Instrument -> MIDI (Maybe E.Channel)
instrumentChannel drumChannel Percussion = pure $ pure $ E.Channel drumChannel
instrumentChannel _ (Instrument i) = ioMIDI $ atomically . readTVar . instrChannels
                                          >>> map (lookup i)

allocateChannel :: Word8 -> Instrument -> MIDI E.Channel
allocateChannel drumChannel Percussion = pure $ E.Channel drumChannel
allocateChannel drumChannel (Instrument i) = do
        c <- unusedChannel
        event 0 $ E.CtrlEv E.PgmChange
                $ E.Ctrl c (E.Parameter 0)
                $ E.Value $ fromIntegral i
        c <$ ioMIDI (\cxt -> atomically $ modifyTVar (instrChannels cxt) (insert i c))
    where
        unusedChannel = ioMIDI $ \cxt -> do
                chnls <- keys <$> atomically (readTVar $ channels cxt)
                let unusedChannels = set (E.Channel <$> [0..15])
                                  \\ set chnls
                                  \\ set [E.Channel drumChannel]
                pure $ head (toList unusedChannels) <?> error "Out of channels"

-- | Play a melody and flush the buffer
playNotes :: Foldable t => Word8 -> t (Tick, Tick, Velocity, Note) -> MIDI ()
playNotes drumChannel notes = traverse_ playNote notes >> flush
    where
        playNote (start, duration, v, note) = startNote drumChannel start v note
                                           >> stopNote drumChannel (start + duration) note
