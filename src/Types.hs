-- |
-- Module:      Types
-- Copyright:   (c) 2020 Michael Heyman
-- Description: Definition of the WAV types
-- License:     MIT
-- Maintainer:  Michael Heyman <michaelheyman@users.noreply.github.com>
-- Stability:   experimental
-- Portability: portable
--
-- Definition of the WAV types.
-- This file defines the data types associated with WAV files.
--
-- More information can be found in the [WAV Specification](https://en.wikipedia.org/wiki/WAV#Specification) page.
--
-- = WAV Specification
--
-- The WAV binary file format is split three chunks:
--
-- * RIFF
-- * fmt
-- * data
--
-- == RIFF chunk
--
-- | RIFF table
--
-- +------------+--------------------+---------------------+--------+
-- | Field Name | Field Size (Bytes) | File Offset (Bytes) | Endian |
-- +============+====================+=====================+========+
-- | ChunkID    |                  4 |                   0 | Big    |
-- +------------+--------------------+---------------------+--------+
-- | ChunkSize  |                  4 |                   4 | Little |
-- +------------+--------------------+---------------------+--------+
-- | Format     |                  4 |                   8 | Big    |
-- +------------+--------------------+---------------------+--------+
--
-- == fmt chunk
--
-- | fmt table
--
-- +---------------+--------------------+---------------------+--------+
-- | Field Name    | Field Size (Bytes) | File Offset (Bytes) | Endian |
-- +===============+====================+=====================+========+
-- | SubchunkID    |                  4 |                  12 | Big    |
-- +---------------+--------------------+---------------------+--------+
-- | SubchunkSize  |                  4 |                  16 | Little |
-- +---------------+--------------------+---------------------+--------+
-- | AudioFormat   |                  2 |                  20 | Little |
-- +---------------+--------------------+---------------------+--------+
-- | NumChannels   |                  2 |                  22 | Little |
-- +---------------+--------------------+---------------------+--------+
-- | SampleRate    |                  4 |                  24 | Little |
-- +---------------+--------------------+---------------------+--------+
-- | ByteRate      |                  4 |                  28 | Little |
-- +---------------+--------------------+---------------------+--------+
-- | BlockAlign    |                  2 |                  32 | Little |
-- +---------------+--------------------+---------------------+--------+
-- | BitsPerSample |                  2 |                  34 | Little |
-- +---------------+--------------------+---------------------+--------+
--
-- == `data` chunk
--
-- | data table
--
-- +--------------+--------------------+---------------------+--------+
-- | Field Name   | Field Size (Bytes) | File Offset (Bytes) | Endian |
-- +==============+====================+=====================+========+
-- | SubchunkID   |                  4 |                  36 | Big    |
-- +--------------+--------------------+---------------------+--------+
-- | SubchunkSize |                  4 |                  40 | Little |
-- +--------------+--------------------+---------------------+--------+
-- | Data         |       SubchunkSize |                  44 | Little |
-- +--------------+--------------------+---------------------+--------+
--

module Types where

import           Data.ByteString (ByteString)
import           Data.Word


-- | Record that contains all of the chunks
data Wav = Wav
    { wavRiff   :: Riff
    , wavFormat :: Format
    , wavData   :: Data
    }
    deriving (Show, Eq)

-- | Riff chunk record
data Riff = Riff
    { riffChunkID     :: ByteString
    , riffChunkSize   :: Word32
    , riffChunkFormat :: ByteString
    }
    deriving (Show, Eq)

-- | fmt chunk record
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

-- | data chunk record
data Data = Data
    { dataChunkID   :: Word32
    , dataChunkSize :: Word32
    , dataData      :: ByteString
    }
    deriving (Show, Eq)
