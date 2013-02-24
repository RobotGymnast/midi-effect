{-# LANGUAGE NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           , TemplateHaskell
           #-}
module Sound.MIDI.Monad.Types ( Note (..)
                              , Tick (..)
                              , Pitch (..)
                              , Instrument (..)
                              , Velocity (..)
                              , pitch'
                              , instr'
                              , vcty'
                              , tickALSA
                              , toALSA
                              , fromALSA
                              , middleC
                              ) where

import Prelewd

import Data.Word
import Template.MemberTransformer
import Text.Show

import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Time as T

newtype Tick = Tick Word32 deriving (Show, Eq, Ord, Num, Real, Enum, Integral)
newtype Pitch = Pitch Word8 deriving (Show, Eq, Ord, Num, Real, Enum, Integral)
newtype Instrument = Instrument Word8 deriving (Show, Eq, Ord, Num, Real, Enum, Integral)
newtype Velocity = Velocity Word8 deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

data Note = Note
        { pitch :: Pitch
        , instr :: Instrument
        , vcty :: Velocity
        }
    deriving (Eq, Show)

$(memberTransformers ''Note)

instance Ord Note where
    compare n1 n2 = (compare `on` pitch) n1 n2
                 <> (compare `on` vcty) n1 n2
                 <> (compare `on` instr) n1 n2

tickALSA :: Tick -> T.Stamp
tickALSA (Tick t) = T.Tick t

-- | Convert Note to Sound.ALSA.Sequence.Event.Note
toALSA :: (Pitch, Velocity) -> E.Channel -> E.Note
toALSA (Pitch p, Velocity v) c = E.simpleNote c (E.Pitch p) (E.Velocity v)

-- | The Instrument is just the Channel
fromALSA :: E.Note -> Note
fromALSA = Note 
       <$> Pitch . E.unPitch . E.noteNote
       <*> Instrument . E.unChannel . E.noteChannel
       <*> Velocity . E.unVelocity . E.noteVelocity

middleC :: Pitch
middleC = 60
