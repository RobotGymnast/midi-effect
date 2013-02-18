module Sound.MIDI.Monad.Types ( Note (..)
                              , Tick
                              ) where

import Data.Word

data Note = Note
        { pitch :: Word8    -- ^ In semitones. 48 is middle C
        , instr :: Word8
        , vcty :: Word8
        }

-- | 1/96th of a beat
type Tick = Word32
