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
                    $ fromEvent <$> E.input h <&> try (:) <*> midiInIO h

fromEvent :: E.T -> Maybe (Bool, Note)
fromEvent e = case E.body e of
        E.NoteEv E.NoteOn note -> Just (True, alsaNote note)
        E.NoteEv E.NoteOff note -> Just (False, alsaNote note)
        E.CtrlEv _ _ -> Nothing
        _ -> traceShow e undefined

alsaNote :: E.Note -> Note
alsaNote note = Note (E.unPitch $ E.noteNote note)
                     (E.unChannel $ E.noteChannel note)
                     (E.unVelocity $ E.noteVelocity note)
