module Types where

import           Data.Text (Text)
import           Data.Word

data Wave = Wave
    { waveRiff   :: Riff
    , waveFormat :: Format
    , waveData   :: Data
    }
    deriving (Show, Eq)

data Riff = Riff
    { riffChunkID     :: Text
    , riffChunkSize   :: Word32
    , riffChunkFormat :: Text
    }
    deriving (Show, Eq)

data Format = Format
    { formatChunkID       :: Text
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
    , dataData      :: Text
    }
    deriving (Show, Eq)
