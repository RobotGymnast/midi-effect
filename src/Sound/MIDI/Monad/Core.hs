{-# LANGUAGE NoImplicitPrelude
           #-}
module Sound.MIDI.Monad.Core ( MIDI
                             , MIDIContext
                             , ioMIDI
                             , runMIDI
                             ) where

import Prelewd

import IO

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as C
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Port as P
import qualified Sound.ALSA.Sequencer.Event as E
import qualified Sound.ALSA.Sequencer.Queue as Q
import qualified Sound.ALSA.Sequencer as S

-- | Context for MIDI I/O actions
type MIDIContext = (S.T S.DuplexMode, Q.T, Connect.T, Connect.T)

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
runMIDI :: Text     -- ^ Client name
        -> Text     -- ^ Output destination
        -> Text     -- ^ Input destination
        -> MIDI ()  -- ^ MIDI action
        -> IO ()
runMIDI name output input m = io $ S.withDefault S.Nonblock $ \h -> do
        C.setName h name
        P.withSimple h "io"
            (P.caps [P.capRead, P.capSubsRead, P.capWrite])
            (P.types [P.typeMidiGeneric, P.typeApplication])
            $ \p -> Q.with h $ \q -> do
                    outConn <- Connect.createTo h p =<< Addr.parse h output
                    inConn <- Connect.createFrom h p =<< Addr.parse h input
                    Q.control h q E.QueueStart Nothing
                    runIO $ raw m (h, q, outConn, inConn)

-- | Lift IO to MIDI I/O
ioMIDI :: (MIDIContext -> IO a) -> MIDI a
ioMIDI = MIDI
