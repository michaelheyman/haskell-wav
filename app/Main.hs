module Main where

import           Data.Attoparsec.ByteString (parseOnly)
import           Data.ByteString            (readFile)
import           Prelude                    hiding (readFile)

import           Sound.Wav.Parser

main :: IO ()
main = do
    putStrLn "Enter the path to the WAV file:"
    filePath <- getLine
    contents <- readFile filePath
    case parseOnly wavParser contents of
        Left e -> do
            putStrLn $ "There was a problem parsing the file: " ++ e
            return $ error e
        Right wav -> do
            print wav
            return ()
