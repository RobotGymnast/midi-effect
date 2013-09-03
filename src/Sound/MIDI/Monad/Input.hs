{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           #-}
module Sound.MIDI.Monad.Input ( midiIn
                              ) where

import Summit.Data.Map
import Summit.Impure
import Summit.IO
import Summit.Prelewd

import Sound.MIDI.Monad.Core
import Sound.MIDI.Monad.Types

import qualified Sound.ALSA.Sequencer.Event as E

-- | Accumulate a monadic value until a monadic predicate is True
repeatM :: (Applicative m, Monad m) => m Bool -> m a -> m [a]
repeatM p m = do b <- p
                 iff b (return []) $ m <&> (:) <*> repeatM p m

-- | MIDI input events
midiIn :: MIDI [(Maybe Velocity, Note)]
midiIn = ioMIDI $ io . (ioFetchConvertedEvents <$> sourceToInstr <*> seqT)
    where
        ioFetchConvertedEvents instrs h = repeatM (E.inputPending h True <&> (== 0))
                                                  (E.input h)
                                      <&> mapMaybe (fromEvent instrs)

fromEvent :: Map MIDIAddress Instrument -> E.T -> Maybe (Maybe Velocity, Note)
fromEvent instrs e = case E.body e of
        E.NoteEv state note -> Just $ if' (state == E.NoteOff) (map2 (>> Nothing))
                                    $ map (, instrument)
                                    $ fromALSA note
        _ -> Nothing
    where
        instrument = lookup (E.source e) instrs <?> error "Unknown input source"
