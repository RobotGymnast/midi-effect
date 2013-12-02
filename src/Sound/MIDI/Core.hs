{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , DeriveDataTypeable
           , FlexibleInstances
           , FlexibleContexts
           , UndecidableInstances
           , TemplateHaskell
           #-}
module Sound.MIDI.Core ( MIDI
                       , MIDIState (..)
                       , seqT'
                       , qT'
                       , connsOut'
                       , connsIn'
                       , instrChannels'
                       , channels'
                       , sourceToInstr'
                       , runMIDI
                       ) where

import Summit.Data.Map
import Summit.Data.Refcount
import Summit.IO
import Summit.Prelewd
import Summit.Template.MemberTransformer

-- extensible-effects
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

import Data.Typeable (Typeable)
import Data.Word

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as C
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Port as P
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer as S

import Sound.MIDI.Types

-- | traverse on the first of two type parameters
traverse2 :: (Ord b, Applicative m) => (a -> m b) -> Map a z -> m (Map b z)
traverse2 f = assocs >>> traverse (map2 f >>> sequence2) >>> map fromList
    where
        sequence2 (m, x) = m <&> (, x)

class (SetMember Lift (Lift IO) e, Member (State MIDIState) e) => MIDI e
instance (SetMember Lift (Lift IO) e, Member (State MIDIState) e) => MIDI e

-- | Context for MIDI I/O actions
data MIDIState = MIDIState
               { seqT          :: S.T S.DuplexMode             -- ^ Sequencer handle
               , qT            :: Q.T                          -- ^ Queue handle
               , connsOut      :: [Connect.T]                  -- ^ Outbound connection handles
               , connsIn       :: [Connect.T]                  -- ^ Inbound connection handles
               , instrChannels :: Map Word8 E.Channel          -- ^ What instrument's on what
                                                               -- channel?
               , channels      :: Refcount E.Channel           -- ^ What channels are in use?
               , sourceToInstr :: Map MIDIAddress Instrument   -- ^ Determine instrument
                                                               -- from input source
               }
  deriving (Typeable)

$(memberTransformers ''MIDIState)

-- | Initialize the MIDI state
runMIDI :: Text                             -- ^ Client name
        -> [Text]                           -- ^ MIDI output destinations
        -> Map Text Instrument              -- ^ MIDI input source to instrument mapping
        -> (MIDIState -> IO ())
        -> IO ()
runMIDI name outputs inputs f = io
                              $ S.withDefault S.Nonblock
                              $ \h -> do
                                  C.setName h name
                                  P.withSimple h "io"
                                      (P.caps [P.capRead, P.capSubsRead, P.capWrite])
                                      (P.types [P.typeMidiGeneric, P.typeApplication])
                                      $ \p -> Q.with h $ \q -> do
                                              dests <- traverse (Addr.parse h) outputs
                                                   >>= traverse (Connect.createTo h p)
                                              sourcesToInstrs <- traverse2 (Addr.parse h) inputs
                                              sources <- traverse (Connect.createFrom h p) $ keys sourcesToInstrs
                                              Q.control h q E.QueueStart Nothing
                                              runIO $ f MIDIState { seqT              = h
                                                                  , qT                = q
                                                                  , connsOut          = dests
                                                                  , connsIn           = sources
                                                                  , instrChannels     = mempty
                                                                  , channels          = mempty
                                                                  , sourceToInstr     = sourcesToInstrs
                                                                  }
