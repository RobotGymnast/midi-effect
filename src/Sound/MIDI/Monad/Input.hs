{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad.Input ( midiIn
                              ) where

import Prelewd

import Impure
import IO

import Sound.MIDI.Monad.Core
import Sound.MIDI.Monad.Types

import qualified Sound.ALSA.Sequencer.Event as E

-- | Accumulate a monadic value until a monadic predicate is True
repeatM :: (Applicative m, Monad m) => m Bool -> m a -> m [a]
repeatM p m = do b <- p
                 iff b (return []) $ m <&> (:) <*> repeatM p m

-- | MIDI note events; True indicates pressed.
midiIn :: MIDI [(Bool, Note)]
midiIn = ioMIDI $ \cxt -> io $ mapMaybe id <$> midiInIO (seqT cxt)
    where
        midiInIO h = fromEvent <$$> repeatM (E.inputPending h True <&> (== 0)) (E.input h)

fromEvent :: E.T -> Maybe (Bool, Note)
fromEvent e = case E.body e of
        E.NoteEv E.NoteOn note -> Just $ fromALSA note
        E.NoteEv E.NoteOff _ -> error "ALSA input NoteOff event"
        E.CtrlEv _ _ -> Nothing
        E.EmptyEv _ -> Nothing
        _ -> traceShow e undefined
