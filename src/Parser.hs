{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Attoparsec.Binary     (anyWord16le, anyWord32be,
                                             anyWord32le)
import           Data.Attoparsec.ByteString (Parser, string, take)
import           Prelude                    hiding (take)

import           Types

riffParser :: Parser Riff
riffParser = do
    chunkID <- string "RIFF"
    chunkSize <- divAnyWord anyWord32le
    chunkFormat <- string "WAVE"
    return $ Riff chunkID chunkSize chunkFormat

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

{-# ANN dataParser ("HLint: ignore Use <$>" :: String) #-}
dataParser :: Parser Data
dataParser = do
    chunkID <- divAnyWord anyWord32be
    chunkSize <- divAnyWord anyWord32le
    chunkData <- take $ fromIntegral $ toInteger chunkSize
    return $ Data chunkID chunkSize chunkData

{-# ANN wavParser ("HLint: ignore Use <$>" :: String) #-}
wavParser :: Parser Wav
wavParser = do
    riff <- riffParser
    format <- formatParser
    dataChunk <- dataParser
    return $ Wav riff format dataChunk

divAnyWord :: Integral a => Parser a -> Parser a
divAnyWord = fmap (`div` 256)
