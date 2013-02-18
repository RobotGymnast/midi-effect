{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI ( runMIDI
                  , tempo
                  , Note (..)
                  , playNote
                  , startNote
                  , stopNote
                  ) where

import Prelewd hiding (length)

import IO

import Data.Word

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as C
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Port as P
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer.Time as Time
import qualified Sound.ALSA.Sequencer as S

data Note = Note
        { pitch :: Word8    -- ^ In semitones. 48 is middle C
        , instr :: Word8
        , vcty :: Word8
        }

type MIDIHandle = (S.T S.DuplexMode, Q.T, Connect.T)

runMIDI :: Text -> (MIDIHandle -> SystemIO a) -> SystemIO a
runMIDI name f = S.withDefault S.Block $ \h -> do
        C.setName h name
        P.withSimple h "out"
            (P.caps [P.capRead, P.capSubsRead, P.capWrite])
            (P.types [P.typeMidiGeneric, P.typeApplication])
            $ \p -> Q.with h $ \q -> do
                    conn <- Connect.createTo h p =<< Addr.parse h "128:0"
                    Q.control h q E.QueueStart Nothing
                    f (h, q, conn)

tempo :: MIDIHandle
      -> Word32     -- ^ Microseconds per beat
      -> SystemIO ()
tempo (h, q, _) t = Q.control h q (E.QueueTempo $ E.Tempo $ fromIntegral t) Nothing

noteEvent :: E.NoteEv -> MIDIHandle -> Word32 -> Note -> SystemIO ()
noteEvent e (h, q, conn) start note = let
            alsaNote = E.simpleNote (E.Channel $ instr note)
                                    (E.Pitch $ pitch note)
                                    (E.Velocity $ vcty note)
            event = (E.forConnection conn $ E.NoteEv e alsaNote)
                { E.queue = q
                , E.time = Time.consAbs $ Time.Tick start
                }
        in E.output h event >> E.drainOutput h $> ()

startNote :: MIDIHandle -> Word32 -> Note -> SystemIO ()
startNote = noteEvent E.NoteOn

stopNote :: MIDIHandle -> Word32 -> Note -> SystemIO ()
stopNote = noteEvent E.NoteOff

playNote :: MIDIHandle
         -> Word32      -- ^ Start time
         -> Word32      -- ^ Duration, in 1/96ths of a beat
         -> Note
         -> SystemIO ()
playNote h start length note = startNote h start note >> stopNote h (start + length) note
