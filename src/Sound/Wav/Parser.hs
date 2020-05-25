{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Parser
-- Copyright:   (c) 2020 Michael Heyman
-- Description : Definition of the WAV spec parsers
-- License:     MIT
-- Maintainer:  Michael Heyman <michaelheyman@users.noreply.github.com>
-- Stability:   experimental
-- Portability: portable
--
-- Definition of the WAV spec parsers.

module Sound.Wav.Parser where

import           Data.Attoparsec.Binary     (anyWord16le, anyWord32be,
                                             anyWord32le)
import           Data.Attoparsec.ByteString (Parser, parseOnly, string, take)
import           Data.ByteString            (readFile)
import           Prelude                    hiding (readFile, take)

import           Sound.Wav.Parser.Types

-- | Parse a WAV file
parseWavFile :: FilePath -> IO (Either String Wav)
parseWavFile filePath = do
    contents <- readFile filePath
    return $ parseOnly wavParser contents

-- | Parse the RIFF chunk
riffParser :: Parser Riff
riffParser = do
    chunkID <- string "RIFF"
    chunkSize <- divAnyWord anyWord32le
    chunkFormat <- string "WAVE"
    return $ Riff chunkID chunkSize chunkFormat

-- | Parse the fmt chunk
{-# ANN formatParser ("HLint: ignore Use <$>" :: String) #-}
formatParser :: Parser Format
formatParser = do
    chunkID <- string "fmt"
    chunkSize <- divAnyWord anyWord32le
    audioFormat <- divAnyWord anyWord16le
    numChannels <- divAnyWord anyWord16le
    sampleRate <- divAnyWord anyWord32le
    byteRate <- divAnyWord anyWord32le
    blockAlign <- divAnyWord anyWord16le
    bitsPerSample <- divAnyWord anyWord16le
    return $ Format chunkID chunkSize audioFormat numChannels sampleRate byteRate blockAlign bitsPerSample

-- | Parse the data chunk
{-# ANN dataParser ("HLint: ignore Use <$>" :: String) #-}
dataParser :: Parser Data
dataParser = do
    chunkID <- divAnyWord anyWord32be
    chunkSize <- divAnyWord anyWord32le
    chunkData <- take $ fromIntegral $ toInteger chunkSize
    return $ Data chunkID chunkSize chunkData

-- | Parse the WAV file
{-# ANN wavParser ("HLint: ignore Use <$>" :: String) #-}
wavParser :: Parser Wav
wavParser = do
    riff <- riffParser
    format <- formatParser
    dataChunk <- dataParser
    return $ Wav riff format dataChunk

-- | Helper function to normalize the resulting value of a Parser with a
-- polymorphic type that is constrained to be an integral.
divAnyWord :: Integral a => Parser a -> Parser a
divAnyWord = fmap (`div` 256)
