module Parser where

import           Data.Attoparsec.ByteString (Parser)
import           Prelude                    hiding (take)

import           Types

riffParser :: Parser Riff
riffParser = undefined

formatParser :: Parser Format
formatParser = undefined

dataParser :: Parser Data
dataParser = undefined
