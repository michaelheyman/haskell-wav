module Main where

import           Data.Attoparsec.ByteString (parseOnly)
import           Data.ByteString            (readFile)
import           Prelude                    hiding (readFile)

import           Parser
import           Types

parseFile :: FilePath -> IO Wav
parseFile filePath = do
    contents <- readFile filePath
    case parseOnly wavParser contents of
        Left e -> do
            putStrLn e
            return $ error e
        Right wav -> return wav

main :: IO ()
main = do
    putStrLn "Enter the path to the WAV file:"
    -- filePath <- getLine
    -- let filePath = "/Users/michael/code/haskell-wav/dist/wav/cat_meow_x.wav"
    let filePath = "/Users/michael/code/haskell-wav/dist/wav/CantinaBand3.wav"
    -- let filePath = "/Users/michael/code/haskell-wav/dist/wav/about_time.wav"
    -- case parseFile filePath of

    wav <- parseFile filePath
    print wav
    return ()
