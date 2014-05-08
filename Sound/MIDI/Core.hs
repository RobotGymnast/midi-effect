{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Sound.MIDI.Core ( MIDI
                       , MIDIState
                       , seqT
                       , qT
                       , connsOut
                       , connsIn
                       , instrChannels
                       , channels
                       , sourceToInstr
                       , drumChannel
                       , runMIDI
                       ) where

import Prelude ()
import BasicPrelude

import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Lens
import Data.HashMap.Strict as HashMap
import Data.Refcount

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as C
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Port as P
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer as S

import Sound.MIDI.Types

traverseKeys :: (Eq a, Eq b, Hashable a, Hashable b, Functor m, Monad m) => (a -> m b) -> HashMap a z -> m (HashMap b z)
traverseKeys f m = fromList <$> forM (HashMap.toList m) (\(a, z) -> f a <&> (, z))

class (SetMember Lift (Lift IO) e, Member (State MIDIState) e) => MIDI e
instance (SetMember Lift (Lift IO) e, Member (State MIDIState) e) => MIDI e

instance Hashable P.T where
  hashWithSalt s (P.Cons p) = hashWithSalt s p

instance Hashable C.T where
  hashWithSalt s (C.Cons c) = hashWithSalt s c

instance Hashable Addr.T where
  hashWithSalt s a = hashWithSalt s (Addr.client a, Addr.port a)

-- | Context for MIDI I/O actions
data MIDIState = MIDIState
               { _seqT          :: S.T S.DuplexMode               -- ^ Sequencer handle
               , _qT            :: Q.T                            -- ^ Queue handle
               , _connsOut      :: [Connect.T]                    -- ^ Outbound connection handles
               , _connsIn       :: [Connect.T]                    -- ^ Inbound connection handles
               , _instrChannels :: HashMap Word8 Channel          -- ^ What instrument's on what
                                                                  -- channel?
               , _channels      :: Refcount Channel               -- ^ What channels are in use?
               , _drumChannel   :: Channel                        -- ^ Channel for percussion I/O
               , _sourceToInstr :: HashMap MIDIAddress Instrument -- ^ Determine instrument
                                                                  -- from input source
               }
  deriving (Typeable)

$(makeLenses ''MIDIState)

-- | Initialize the MIDI state
runMIDI :: String                         -- ^ Client name
        -> [String]                       -- ^ MIDI output destinations
        -> HashMap String Instrument      -- ^ MIDI input source to instrument mapping
        -> Channel                        -- ^ Percussion channel
        -> (MIDIState -> IO ())
        -> IO ()
runMIDI name outputs inputs drumc f
      = S.withDefault S.Nonblock
      $ \h -> do
            C.setName h name
            P.withSimple h name
                (P.caps [P.capRead, P.capSubsRead, P.capWrite])
                (P.types [P.typeMidiGeneric, P.typeApplication])
                $ \p -> Q.with h $ \q -> do
                        dests <- traverse (Addr.parse h) outputs
                              >>= traverse (Connect.createTo h p)
                        sourcesToInstrs <- traverseKeys (Addr.parse h) inputs
                        sources <- traverse (Connect.createFrom h p) $ keys sourcesToInstrs
                        Q.control h q E.QueueStart Nothing
                        f MIDIState
                            { _seqT             = h
                            , _qT               = q
                            , _connsOut         = dests
                            , _connsIn          = sources
                            , _instrChannels    = mempty
                            , _channels         = mempty
                            , _drumChannel      = drumc
                            , _sourceToInstr    = sourcesToInstrs
                            }
