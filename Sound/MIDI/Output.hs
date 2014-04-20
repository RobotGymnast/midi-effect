{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Sound.MIDI.Output ( tempo
                         , flush
                         , toALSA
                         , startNote
                         , stopNote
                         , playNote
                         , playNotes
                         ) where

import Prelude ()
import BasicPrelude hiding (lift, forM_)

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Lens (view, over)
import Data.Foldable (Foldable, forM_, traverse_)
import Data.HashSet as HashSet (toList, fromList, difference)
import Data.HashMap.Strict as HashMap (lookup, insert)
import Data.Refcount

import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer.Time as T

import Sound.MIDI.Core
import Sound.MIDI.Types as Types

(<??>) :: Monad m => m (Maybe a) -> m a -> m a
(<??>) m a = m >>= maybe a return

tempo :: MIDI env
    => Word32     -- ^ Microseconds per beat
    -> Eff env ()
tempo t = do
        midi <- get
        lift $ Q.control
                (view seqT midi)
                (view qT midi)
                (E.QueueTempo $ E.Tempo $ fromIntegral t)
                Nothing

flush :: MIDI env => Eff env ()
flush = get >>= \cxt -> lift $ void $ E.drainOutput $ view seqT cxt

event :: MIDI env
    => Tick -> E.Data -> Eff env ()
event start e = get >>= lift . (multiDestEvent <$> view qT <*> view seqT <*> view connsOut)
  where
      multiDestEvent q sequ = traverse_ $ E.output sequ . singleDestEvent q
      singleDestEvent q out = ( E.forConnection out e )
                              { E.queue = q
                              , E.time = T.consRel $ tickALSA start
                              }

noteEvent :: MIDI env
        => Tick -> E.NoteEv -> Pitch -> Maybe Velocity -> Channel -> Eff env ()
noteEvent t e p v = event t . E.NoteEv e . toALSA p (fromMaybe 0 v)

startNote :: MIDI env
        => Word8 -> Tick -> Velocity -> Note -> Eff env ()
startNote drumChannel t v (p, instr) = do
      c <- instrumentChannel drumChannel instr <??> allocateChannel drumChannel instr
      _ <- modify $ over channels $ insertRef c
      noteEvent t E.NoteOn p (Just v) c

stopNote :: MIDI env => Word8 -> Tick -> Note -> Eff env ()
stopNote drumChannel t (p, instr) = do
      c <- fromMaybe (error "Note not started.") <$> instrumentChannel drumChannel instr
      _ <- noteEvent t E.NoteOff p Nothing c
      void $ releaseChannel c
  where
      releaseChannel c = modify $ over channels $ \m -> fromMaybe m $ deleteRef c m

instrumentChannel :: Member (State MIDIState) env => Word8 -> Instrument -> Eff env (Maybe Channel)
instrumentChannel drumChannel Percussion = pure $ pure $ Channel drumChannel
instrumentChannel _ (Instrument i) = HashMap.lookup i . view instrChannels <$> get

allocateChannel :: MIDI env
              => Word8 -> Instrument -> Eff env Channel
allocateChannel drumChannel Percussion = pure $ Channel drumChannel
allocateChannel drumChannel (Instrument i) = do
      Channel c <- unusedChannel <$> get
      event 0 $ E.CtrlEv E.PgmChange
              $ E.Ctrl (E.Channel c) (E.Parameter 0)
              $ E.Value $ fromIntegral i
      Channel c <$ modify (over instrChannels $ HashMap.insert i $ Channel c)
  where
      unusedChannel cxt = let
              chnls = refcounted $ view channels cxt
              unusedChannels = foldl' difference (fromList (Channel <$> [0..15]))
                              [ fromList chnls
                              , fromList [Channel drumChannel]
                              ]
              in case toList unusedChannels of
                  [] -> error "Out of channels"
                  c:_ -> c

-- | Play a melody and flush the buffer
playNotes :: (MIDI env, Foldable t) => Word8 -> t (Tick, Tick, Velocity, Note) -> Eff env ()
playNotes drumChannel notes = do
    forM_ notes $ \(start, len, v, note) -> playNote drumChannel start len v note
    flush

playNote :: MIDI env => Word8 -> Tick -> Tick -> Velocity -> Note -> Eff env ()
playNote drumChannel start dur v note = do
    startNote drumChannel start v note
    stopNote drumChannel (start + dur) note
