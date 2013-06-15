{-# LANGUAGE NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           #-}
module Sound.MIDI.Monad.Types ( Note
                              , MIDIAddress
                              , Tick (..)
                              , Pitch (..)
                              , Instrument (..)
                              , Velocity (..)
                              , tickALSA
                              , toALSA
                              , fromALSA
                              , middleC
                              , percussionChannel
                              ) where

import Prelewd

import Data.Word
import Text.Show

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Time as T

import Test.QuickCheck (Arbitrary (..))

newtype Tick = Tick Word32 deriving (Show, Eq, Ord, Num, Real, Enum, Bounded, Integral)
newtype Pitch = Pitch Word8 deriving (Show, Eq, Ord, Num, Real, Enum, Bounded, Integral)
newtype Velocity = Velocity Word8 deriving (Show, Eq, Ord, Num, Real, Enum, Bounded, Integral)

data Instrument = Percussion
                | Instrument Word8
    deriving (Show, Eq, Ord)

type Note = (Pitch, Instrument)
type MIDIAddress = Addr.T

instance Arbitrary Tick where arbitrary = Tick <$> arbitrary
instance Arbitrary Pitch where arbitrary = Pitch <$> arbitrary
instance Arbitrary Velocity where arbitrary = Velocity <$> arbitrary

instance Arbitrary Instrument where
    arbitrary = Instrument <$$> arbitrary <&> (<?> Percussion)

tickALSA :: Tick -> T.Stamp
tickALSA (Tick t) = T.Tick t

-- | Convert Note to Sound.ALSA.Sequence.Event.Note
toALSA :: Pitch -> Velocity -> E.Channel -> E.Note
toALSA (Pitch p) (Velocity v) c = E.simpleNote c (E.Pitch p) (E.Velocity v)

-- | The Instrument is just the Channel
fromALSA :: E.Note -> (Maybe Velocity, Note)
fromALSA = cast (> 0) . Velocity . E.unVelocity . E.noteVelocity
       &&& Pitch . E.unPitch . E.noteNote
       &&& toInstrument . E.noteChannel
    where
        toInstrument c = iff (c == percussionChannel) Percussion $ Instrument $ E.unChannel c

middleC :: Pitch
middleC = 60

percussionChannel :: E.Channel
percussionChannel = E.Channel 9
