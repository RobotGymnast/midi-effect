{-# LANGUAGE NoImplicitPrelude
           , TypeOperators
           , FlexibleContexts
           #-}
module Sound.MIDI.Output ( tempo
                         , flush
                         , toALSA
                         , startNote
                         , stopNote
                         , playNote
                         , playNotes
                         ) where

import Summit.Data.List
import Summit.Data.Map (lookup, insert, keys)
import Summit.Data.Refcount
import Summit.Data.Set hiding (insert)
import Summit.IO
import Summit.Impure
import Summit.Prelewd

import Data.Word
import Data.Tuple.All
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer.Time as T

import Sound.MIDI.Core
import Sound.MIDI.Types as Types

(<??>) :: Monad m => m (Maybe a) -> m a -> m a
(<??>) m a = do x <- m
                return <$> x <?> a

tempo :: MIDI env
      => Word32     -- ^ Microseconds per beat
      -> Eff env ()
tempo t = do
          midi <- get
          lift $ io $ Q.control (seqT midi) (qT midi) (E.QueueTempo $ E.Tempo $ fromIntegral t) Nothing

flush :: MIDI env => Eff env ()
flush = get >>= \cxt -> lift $ io $ void $ E.drainOutput $ seqT cxt

event :: MIDI env
      => Tick -> E.Data -> Eff env ()
event start e = get >>= lift . io . (multiDestEvent <$> qT <*> seqT <*> connsOut)
    where
        multiDestEvent q sequ = traverse_ $ singleDestEvent q >>> E.output sequ
        singleDestEvent q out = ( E.forConnection out e )
                                { E.queue = q
                                , E.time = T.consRel $ tickALSA start
                                }

noteEvent :: MIDI env
          => Tick -> E.NoteEv -> Pitch -> Maybe Velocity -> E.Channel -> Eff env ()
noteEvent t e p v = event t . E.NoteEv e . toALSA p (v <?> 0)

startNote :: MIDI env
          => Word8 -> Tick -> Velocity -> Note -> Eff env ()
startNote drumChannel t v (p, instr) = do
        c <- instrumentChannel drumChannel instr <??> allocateChannel drumChannel instr
        _ <- modify $ channels' $ refInsert c
        noteEvent t E.NoteOn p (Just v) c

stopNote :: MIDI env
         => Word8 -> Tick -> Note -> Eff env ()
stopNote drumChannel t (p, instr) = do
        c <- instrumentChannel drumChannel instr <&> (<?> error "Note not started.")
        _ <- noteEvent t E.NoteOff p Nothing c
        void $ releaseChannel c
    where
        releaseChannel c = modify $ channels' $ \m -> refDelete c m <?> m

instrumentChannel :: Member (State MIDIState) env => Word8 -> Instrument -> Eff env (Maybe E.Channel)
instrumentChannel drumChannel Percussion = pure $ pure $ E.Channel drumChannel
instrumentChannel _ (Instrument i) = get <&> instrChannels <&> lookup i

allocateChannel :: MIDI env
                => Word8 -> Instrument -> Eff env E.Channel
allocateChannel drumChannel Percussion = pure $ E.Channel drumChannel
allocateChannel drumChannel (Instrument i) = do
        c <- unusedChannel
        event 0 $ E.CtrlEv E.PgmChange
                $ E.Ctrl c (E.Parameter 0)
                $ E.Value $ fromIntegral i
        c <$ modify (instrChannels' $ insert i c)
    where
        unusedChannel = get <&> \cxt -> let
                chnls = keys $ channels cxt
                unusedChannels = set (E.Channel <$> [0..15])
                              \\ set chnls
                              \\ set [E.Channel drumChannel]
                in head (toList unusedChannels) <?> error "Out of channels"

-- | Play a melody and flush the buffer
playNotes :: (MIDI env, Foldable t) => Word8 -> t (Tick, Tick, Velocity, Note) -> Eff env ()
playNotes drumChannel notes = traverse_ (uncurryN $ playNote drumChannel) notes >> flush

playNote :: MIDI env => Word8 -> Tick -> Tick -> Velocity -> Note -> Eff env ()
playNote drumChannel start dur v note = startNote drumChannel start v note
                                     >> stopNote drumChannel (start + dur) note
