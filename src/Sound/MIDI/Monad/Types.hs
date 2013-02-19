{-# LANGUAGE NoImplicitPrelude
           , TemplateHaskell
           #-}
module Sound.MIDI.Monad.Types ( Note (..)
                              , Tick
                              , pitch'
                              , instr'
                              , vcty'
                              ) where

import Prelewd

import Data.Word
import Template.MemberTransformer
import Text.Show

data Note = Note
        { pitch :: Word8    -- ^ In semitones. 48 is middle C
        , instr :: Word8
        , vcty :: Word8
        }
    deriving (Eq, Show)

$(memberTransformers ''Note)

-- | 1/96th of a beat
type Tick = Word32
