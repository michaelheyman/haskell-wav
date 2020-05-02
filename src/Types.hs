module Types where

import           Data.ByteString (ByteString)
import           Data.Word

data Wave = Wave
    { waveRiff   :: Riff
    , waveFormat :: Format
    , waveData   :: Data
    }
    deriving (Show, Eq)

data Riff = Riff
    { riffChunkID     :: ByteString
    , riffChunkSize   :: Word32
    , riffChunkFormat :: ByteString
    }
    deriving (Show, Eq)

data Format = Format
    { formatChunkID       :: ByteString
    , formatChunkSize     :: Word32
    , formatAudioFormat   :: Word16
    , formatNumChannels   :: Word16
    , formatSampleRate    :: Word32
    , formatByteRate      :: Word32
    , formatBlockAlign    :: Word16
    , formatBitsPerSample :: Word16
    }
    deriving (Show, Eq)

data Data = Data
    { dataChunkID   :: Word32
    , dataChunkSize :: Word32
    , dataData      :: ByteString
    }
    deriving (Show, Eq)
