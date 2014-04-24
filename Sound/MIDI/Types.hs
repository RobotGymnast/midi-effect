{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Sound.MIDI.Types ( Note
                        , MIDIAddress
                        , Tick (..)
                        , Pitch (..)
                        , Instrument (..)
                        , Velocity (..)
                        , Channel (..)
                        , toALSA
                        , fromALSA
                        , middleC
                        ) where

import Prelude (Show, show)
import BasicPrelude

import Text.Read
import Test.QuickCheck (Arbitrary (..))

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Event as E

newtype Tick = Tick Word32 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral, Typeable, Hashable)
newtype Pitch = Pitch Word8 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral, Typeable, Hashable)
newtype Velocity = Velocity Word8 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral, Typeable, Hashable)
newtype Channel = Channel Word8 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral, Typeable, Hashable)

data Instrument = Percussion
                | Instrument Word8
    deriving (Eq, Ord, Typeable)

instance Hashable Instrument where
  hashWithSalt s i
      = hashWithSalt s
      $ case i of
          Percussion -> Nothing
          Instrument w -> Just w

type Note = (Pitch, Instrument)
type MIDIAddress = Addr.T

instruments :: [String]
instruments =
  [ "AcousticGrandPiano"
  , "BrightAcousticPiano"
  , "ElectricGrandPiano"
  , "HonkytonkPiano"
  , "ElectricPiano1"
  , "ElectricPiano2"
  , "Harpsichord"
  , "Clavinet"
  , "Celesta"
  , "Glockenspiel"
  , "MusicBox"
  , "Vibraphone"
  , "Marimba"
  , "Xylophone"
  , "TubularBells"
  , "Dulcimer"
  , "DrawbarOrgan"
  , "PercussiveOrgan"
  , "RockOrgan"
  , "ChurchOrgan"
  , "ReedOrgan"
  , "Accordion"
  , "Harmonica"
  , "TangoAccordion"
  , "AcousticGuitarNylon"
  , "AcousticGuitarSteel"
  , "ElectricGuitarJazz"
  , "ElectricGuitarClean"
  , "ElectricGuitarMuted"
  , "OverdrivenGuitar"
  , "DistortionGuitar"
  , "GuitarHarmonics"
  , "AcousticBass"
  , "ElectricBassFinger"
  , "ElectricBassPick"
  , "FretlessBass"
  , "SlapBass1"
  , "SlapBass2"
  , "SynthBass1"
  , "SynthBass2"
  , "Violin"
  , "Viola"
  , "Cello"
  , "Contrabass"
  , "TremoloStrings"
  , "PizzicatoStrings"
  , "OrchestralHarp"
  , "Timpani"
  , "StringEnsemble1"
  , "StringEnsemble2"
  , "SynthStrings1"
  , "SynthStrings2"
  , "ChoirAahs"
  , "VoiceOohs"
  , "SynthChoir"
  , "OrchestraHit"
  , "Trumpet"
  , "Trombone"
  , "Tuba"
  , "MutedTrumpet"
  , "FrenchHorn"
  , "BrassSection"
  , "SynthBrass1"
  , "SynthBrass2"
  , "SopranoSax"
  , "AltoSax"
  , "TenorSax"
  , "BaritoneSax"
  , "Oboe"
  , "EnglishHorn"
  , "Bassoon"
  , "Clarinet"
  , "Piccolo"
  , "Flute"
  , "Recorder"
  , "PanFlute"
  , "Blownbottle"
  , "Shakuhachi"
  , "Whistle"
  , "Ocarina"
  , "Lead1Square"
  , "Lead2Sawtooth"
  , "Lead3Calliope"
  , "Lead4Chiff"
  , "Lead5Charang"
  , "Lead6Voice"
  , "Lead7Fifths"
  , "Lead8BassLead"
  , "Pad1Newage"
  , "Pad2Warm"
  , "Pad3Polysynth"
  , "Pad4Choir"
  , "Pad5Bowed"
  , "Pad6Metallic"
  , "Pad7Halo"
  , "Pad8Sweep"
  , "FX1Rain"
  , "FX2Soundtrack"
  , "FX3Crystal"
  , "FX4Atmosphere"
  , "FX5Brightness"
  , "FX6Goblins"
  , "FX7Echoes"
  , "FX8Scifi"
  , "Sitar"
  , "Banjo"
  , "Shamisen"
  , "Koto"
  , "Kalimba"
  , "Bagpipe"
  , "Fiddle"
  , "Shanai"
  , "TinkleBell"
  , "Agogo"
  , "SteelDrums"
  , "Woodblock"
  , "TaikoDrum"
  , "MelodicTom"
  , "SynthDrum"
  , "ReverseCymbal"
  , "GuitarFretNoise"
  , "BreathNoise"
  , "Seashore"
  , "BirdTweet"
  , "TelephoneRing"
  , "Helicopter"
  , "Applause"
  , "Gunshot"
  ]

instance Show Instrument where
  show Percussion = "Percussion"
  show (Instrument i) = instruments !! fromIntegral i

instance Read Instrument where
  readPrec = parens $ lexP >>= \(Ident s) ->
              if s == "Percussion"
              then return Percussion
              else case elemIndex s instruments of
                    Nothing -> pfail
                    Just i -> return $ Instrument $ fromIntegral i

instance Arbitrary Tick where arbitrary = Tick <$> arbitrary
instance Arbitrary Pitch where arbitrary = Pitch <$> arbitrary
instance Arbitrary Velocity where arbitrary = Velocity <$> arbitrary

instance Arbitrary Instrument where
    arbitrary = maybe Percussion Instrument <$> arbitrary

-- | Convert Note to Sound.ALSA.Sequence.Event.Note
toALSA :: Pitch -> Velocity -> Channel -> E.Note
toALSA (Pitch p) (Velocity v) (Channel c) = E.simpleNote (E.Channel c)
                                                         (E.Pitch p)
                                                         (E.Velocity v)

fromALSA :: E.Note -> (Maybe Velocity, Pitch)
fromALSA = mfilter (> 0) . return . Velocity . E.unVelocity . E.noteVelocity
       &&& Pitch . E.unPitch . E.noteNote

middleC :: Pitch
middleC = 60
