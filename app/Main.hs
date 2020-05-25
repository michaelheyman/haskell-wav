module Main where

import           Sound.Wav.Parser

main :: IO ()
main = do
    putStrLn "Enter the path to the WAV file:"
    filePath <- getLine
    parse <- parseWavFile filePath
    case parse of
        Left e -> do
            putStrLn $ "There was a problem parsing the file: " ++ e
            return $ error e
        Right wav -> do
            print wav
            return ()
