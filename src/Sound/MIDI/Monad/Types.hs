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
        { pitch :: Word8    -- ^ In semitones. 60 is middle C
        , instr :: Word8
        , vcty :: Word8
        }
    deriving (Eq, Show)

instance Ord Note where
    compare n1 n2 = (compare `on` pitch) n1 n2
                  <> (compare `on` vcty) n1 n2
                  <> (compare `on` instr) n1 n2

$(memberTransformers ''Note)

-- | 1/96th of a beat
type Tick = Word32
