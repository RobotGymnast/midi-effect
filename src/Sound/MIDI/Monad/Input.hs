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

-- | MIDI note events; True indicates pressed.
midiIn :: MIDI [(Bool, Note)]
midiIn = ioMIDI $ \cxt -> io $ midiInIO $ seqT cxt
    where
        midiInIO h = do
                n <- E.inputPending h True
                iff (n == 0)
                    (return [])
                    (fromEvent <$> E.input h <&> try (:) <*> midiInIO h)

fromEvent :: E.T -> Maybe (Bool, Note)
fromEvent e = case E.body e of
        E.NoteEv E.NoteOn note -> let alsa = fromALSA note
                                  in Just (vcty alsa > 0, alsa)
        E.NoteEv E.NoteOff _ -> error "ALSA input NoteOff event"
        E.CtrlEv _ _ -> Nothing
        _ -> traceShow e undefined
