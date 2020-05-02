{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Attoparsec.Binary     (anyWord32le)
import           Data.Attoparsec.ByteString (Parser, string)
import           Data.ByteString            (ByteString)
import           Data.Word

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

formatParser :: Parser Format
formatParser = undefined

dataParser :: Parser Data
dataParser = undefined
