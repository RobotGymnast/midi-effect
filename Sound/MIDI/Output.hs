{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Sound.MIDI.Output ( tempo
                         , flush
                         , toALSA
                         , startNote
                         , stopNote
                         ) where

import Prelude ()
import BasicPrelude as Base hiding (lift)

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Lens (view, over, set)
import Data.Foldable (traverse_)
import Data.HashSet as HashSet (toList, fromList, difference)
import Data.HashMap.Strict as HashMap
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

event :: MIDI env => E.Data -> Eff env ()
event e = get >>= lift . (multiDestEvent <$> view qT <*> view seqT <*> view connsOut)
  where
      multiDestEvent q sequ = traverse_ $ E.output sequ . singleDestEvent q
      singleDestEvent q out = ( E.forConnection out e )
                              { E.queue = q
                              , E.time = T.consRel $ T.Tick 0
                              }

noteEvent :: MIDI env => E.NoteEv -> Pitch -> Maybe Velocity -> Channel -> Eff env ()
noteEvent e p v = event . E.NoteEv e . toALSA p (fromMaybe 0 v)

startNote :: MIDI env => Word8 -> Velocity -> Note -> Eff env ()
startNote drumChannel v (p, instr) = do
      c <- instrumentChannel drumChannel instr <??> allocateChannel drumChannel instr
      _ <- modify $ over channels $ insertRef c
      noteEvent E.NoteOn p (Just v) c

stopNote :: MIDI env => Word8 -> Note -> Eff env ()
stopNote drumChannel (p, instr) = do
      case instr of
        Percussion ->
            noteEvent E.NoteOff p Nothing $ Channel drumChannel
        Instrument i -> do
            c <- fromMaybe (error "Note not started.") . HashMap.lookup i . view instrChannels <$> get
            noteEvent E.NoteOff p Nothing c
            modify $ \s ->
              case deleteRef c $ view channels s of
                Nothing -> s
                Just chnls ->
                    let s' = set channels chnls s in
                    if refcount c chnls == 0
                    then over instrChannels (HashMap.delete i) s'
                    else s'

instrumentChannel :: Member (State MIDIState) env => Word8 -> Instrument -> Eff env (Maybe Channel)
instrumentChannel drumChannel Percussion = pure $ pure $ Channel drumChannel
instrumentChannel _ (Instrument i) = HashMap.lookup i . view instrChannels <$> get

allocateChannel :: MIDI env => Word8 -> Instrument -> Eff env Channel
allocateChannel drumChannel Percussion = pure $ Channel drumChannel
allocateChannel drumChannel (Instrument i) = do
      Channel c <- unusedChannel <$> get
      event $ E.CtrlEv E.PgmChange
            $ E.Ctrl (E.Channel c) (E.Parameter 0)
            $ E.Value $ fromIntegral i
      Channel c <$ modify (over instrChannels $ HashMap.insert i $ Channel c)
  where
      unusedChannel cxt = let
              chnls = refcounted $ view channels cxt
              unusedChannels = Base.foldl' HashSet.difference (HashSet.fromList (Channel <$> [0..15]))
                              [ HashSet.fromList chnls
                              , HashSet.fromList [Channel drumChannel]
                              ]
              in case HashSet.toList unusedChannels of
                  [] -> error "Out of channels"
                  c:_ -> c
