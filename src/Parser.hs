{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Attoparsec.Binary     (anyWord16le, anyWord32be,
                                             anyWord32le)
import           Data.Attoparsec.ByteString (Parser, string, take)
import           Data.ByteString            (ByteString)
import           Data.Word                  (Word16, Word32)
import           Prelude                    hiding (concat, take)

import           Types

parseChunkID :: Parser ByteString
parseChunkID = string "RIFF"

parseChunkSize :: Parser Word32
parseChunkSize = anyWord32le

riffParser :: Parser Riff
riffParser = do
    chunkID <- string "RIFF"
    chunkSize <- anyWord32le
    chunkFormat <- string "WAVE"
    return $ Riff chunkID chunkSize chunkFormat

parseAudioFormat :: Parser Word16
parseAudioFormat = anyWord16le

{-# ANN formatParser ("HLint: ignore Use <$>" :: String) #-}
formatParser :: Parser Format
formatParser = do
    chunkID <- string "fmt"
    chunkSize <- anyWord32le
    audioFormat <- anyWord16le
    numChannels <- anyWord16le
    sampleRate <- anyWord32le
    byteRate <- anyWord32le
    blockAlign <- anyWord16le
    bitsPerSample <- anyWord16le
    return $ Format chunkID chunkSize audioFormat numChannels sampleRate byteRate blockAlign bitsPerSample

{-# ANN dataParser ("HLint: ignore Use <$>" :: String) #-}
dataParser :: Parser Data
dataParser = do
    chunkID <- anyWord32be
    chunkSize <- anyWord32le
    chunkData <- take $ fromIntegral $ toInteger chunkSize
    return $ Data chunkID chunkSize chunkData

{-# ANN wavParser ("HLint: ignore Use <$>" :: String) #-}
wavParser :: Parser Wav
wavParser = do
    riff <- riffParser
    format <- formatParser
    dataChunk <- dataParser
    return $ Wav riff format dataChunk
