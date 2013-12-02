{-# LANGUAGE NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           #-}
module Sound.MIDI.Types ( Note
                        , MIDIAddress
                        , Tick (..)
                        , Pitch (..)
                        , Instrument (..)
                        , Velocity (..)
                        , tickALSA
                        , toALSA
                        , fromALSA
                        , middleC
                        ) where

import Summit.Prelewd
import Summit.Test

import Data.List (elemIndex)
import Data.Word
import Text.Show (Show (..))
import Text.Read

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Time as T

newtype Tick = Tick Word32 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral)
newtype Pitch = Pitch Word8 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral)
newtype Velocity = Velocity Word8 deriving (Show, Read, Eq, Ord, Num, Real, Enum, Bounded, Integral)

data Instrument = Percussion
                | Instrument Word8
    deriving (Eq, Ord)

type Note = (Pitch, Instrument)
type MIDIAddress = Addr.T

instruments :: [Text]
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
  show (Instrument i) = instruments ! fromIntegral i

instance Read Instrument where
  readPrec = parens $ do
              Ident s <- lexP
              iff (s == "Percussion")
                  (return Percussion)
                  ( return . Instrument . fromIntegral
                <$> elemIndex s instruments
                <?> pfail
                  )

instance Arbitrary Tick where arbitrary = Tick <$> arbitrary
instance Arbitrary Pitch where arbitrary = Pitch <$> arbitrary
instance Arbitrary Velocity where arbitrary = Velocity <$> arbitrary

instance Arbitrary Instrument where
    arbitrary = Instrument <$$> arbitrary <&> (<?> Percussion)

instance ResultEq Tick
instance ResultEq Pitch
instance ResultEq Velocity
instance ResultEq Instrument

tickALSA :: Tick -> T.Stamp
tickALSA (Tick t) = T.Tick t

-- | Convert Note to Sound.ALSA.Sequence.Event.Note
toALSA :: Pitch -> Velocity -> E.Channel -> E.Note
toALSA (Pitch p) (Velocity v) c = E.simpleNote c (E.Pitch p) (E.Velocity v)

fromALSA :: E.Note -> (Maybe Velocity, Pitch)
fromALSA = cast (> 0) . Velocity . E.unVelocity . E.noteVelocity
       &&& Pitch . E.unPitch . E.noteNote

middleC :: Pitch
middleC = 60
