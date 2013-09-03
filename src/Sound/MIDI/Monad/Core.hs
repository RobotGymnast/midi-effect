{-# LANGUAGE NoImplicitPrelude
           , TupleSections
           , TemplateHaskell
           #-}
module Sound.MIDI.Monad.Core ( MIDI
                             , MIDIContext (..)
                             , seqT'
                             , qT'
                             , connsOut'
                             , connsIn'
                             , instrChannels'
                             , channels'
                             , sourceToInstr'
                             , ioMIDI
                             , runMIDI
                             ) where

import Summit.Data.Map
import Summit.Data.Refcount
import Summit.IO
import Summit.Prelewd
import Summit.STM
import Summit.Template.MemberTransformer

import Data.Word

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as C
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Port as P
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer as S

import Sound.MIDI.Monad.Types

-- | traverse on the first of two type parameters
traverse2 :: (Ord b, Applicative m) => (a -> m b) -> Map a z -> m (Map b z)
traverse2 f = assocs >>> traverse (map2 f >>> sequence2) >>> map fromList
    where
        sequence2 (m, x) = m <&> (, x)

-- | Context for MIDI I/O actions
data MIDIContext = MIDIContext
            { seqT          :: S.T S.DuplexMode             -- ^ Sequencer handle
            , qT            :: Q.T                          -- ^ Queue handle
            , connsOut      :: [Connect.T]                  -- ^ Outbound connection handles
            , connsIn       :: [Connect.T]                  -- ^ Inbound connection handles
            , instrChannels :: TVar (Map Word8 E.Channel)   -- ^ What instrument's on what
                                                            -- channel?
            , channels      :: TVar (Refcount E.Channel)    -- ^ What channels are in use?
            , sourceToInstr :: Map MIDIAddress Instrument   -- ^ Determine instrument
                                                            -- from input source
            }

$(memberTransformers ''MIDIContext)

-- | MIDI I/O type
newtype MIDI a = MIDI { raw :: MIDIContext -> IO a }

instance Monad MIDI where
    return = MIDI . return . return
    (MIDI m) >>= f = MIDI $ \h -> m h >>= ($ h) . raw . f

instance MonadPlus MIDI where
    mzero = empty
    mplus = (<|>)

instance Alternative MIDI where
    empty = MIDI $ \_-> empty
    MIDI f <|> MIDI g = MIDI $ liftA2 (<|>) f g

instance Applicative MIDI where
    pure = return
    (<*>) = ap

instance Functor MIDI where fmap = liftA

-- | Perform MIDI I/O
runMIDI :: Text                         -- ^ Client name
        -> [Text]                       -- ^ MIDI output destinations
        -> Map Text Instrument          -- ^ MIDI input source to instrument mapping
        -> MIDI ()                      -- ^ MIDI action
        -> IO ()
runMIDI name outputs inputs m = io $ S.withDefault S.Nonblock $ \h -> do
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
                    instruments <- newTVarIO mempty
                    chnls <- newTVarIO mempty
                    runIO $ raw m $ MIDIContext
                            { seqT              = h
                            , qT                = q
                            , connsOut          = dests
                            , connsIn           = sources
                            , instrChannels     = instruments
                            , channels          = chnls
                            , sourceToInstr     = sourcesToInstrs
                            }

-- | Lift IO to MIDI I/O
ioMIDI :: (MIDIContext -> IO a) -> MIDI a
ioMIDI = MIDI
