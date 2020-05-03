{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString       (ByteString, concat)
import           Parser
import           Test.Hspec
import           Test.Hspec.Attoparsec

import           Prelude               hiding (concat)

specRiff :: Spec
specRiff =
    describe "riffParser" $ do
        it "should parse a valid riff chunk" $
            riffParser `shouldSucceedOn` ("RIFF1000WAVE" :: ByteString)
        it "should leave nothing unconsumed with a valid riff chunk" $
            ("RIFF1000WAVE" :: ByteString) ~?> riffParser `leavesUnconsumed` ""
        it "should leave extra characters unconsumed with a valid riff chunk with extra characters" $
            ("RIFF1000WAVES" :: ByteString) ~?> riffParser `leavesUnconsumed` "S"
        it "should fail when all sub-chunks are missing" $
            riffParser `shouldFailOn` ("" :: ByteString)
        it "should fail when the chunkID does not equal 'RIFF'" $
            riffParser `shouldFailOn` ("FFIR" :: ByteString)
        it "should fail when the size and format chunks are missing" $
            riffParser `shouldFailOn` ("RIFF" :: ByteString)
        it "should fail when the format chunk is missing" $
            riffParser `shouldFailOn` ("RIFF1000" :: ByteString)
        it "should fail when the chunkFormat does not equal 'WAVE'" $
            riffParser `shouldFailOn` ("RIFF1000OGG" :: ByteString)

specFormat :: Spec
specFormat =
    describe "formatParser" $ do
        it "should parse a valid format" $ do
            let validFormat = "fmt11112233444455556677" :: ByteString
            formatParser `shouldSucceedOn` validFormat
            validFormat ~?> formatParser `leavesUnconsumed` ""
        it "should fail when the chunkID does not equal 'fmt'" $
            formatParser `shouldFailOn` ("tmf11112233444455556677" :: ByteString)
        it "should fail when chunks are missing" $ do
            formatParser `shouldFailOn` ("" :: ByteString)
            formatParser `shouldFailOn` ("fmt" :: ByteString)
            formatParser `shouldFailOn` ("fmt" :: ByteString)
            formatParser `shouldFailOn` ("fmt1111" :: ByteString)
            formatParser `shouldFailOn` ("fmt111122" :: ByteString)
            formatParser `shouldFailOn` ("fmt11112233" :: ByteString)
            formatParser `shouldFailOn` ("fmt111122334444" :: ByteString)
            formatParser `shouldFailOn` ("fmt1111223344445555" :: ByteString)
            formatParser `shouldFailOn` ("fmt111122334444555566" :: ByteString)

specData :: Spec
specData =
    describe "dataParser" $ do
        it "should parse a valid data chunk of size 1" $ do
            -- The data chunk is little endian, meaning that the bits read in the opposite order,
            -- and the encoding symbols will map to ASCII. Therefore the encoding `\SOH\NUL\NUL\NUL`
            -- equals 1: 1000
            -- TODO: find a more natural way of generating these symbols, and consider using QuickCheck
            let chunk = "0001\SOH\NUL\NUL\NUL1" :: ByteString
            dataParser `shouldSucceedOn` chunk
            chunk ~?> dataParser `leavesUnconsumed` ""
        it "should parse a valid data chunk of size 10" $ do
            let chunk = "0001\LF\NUL\NUL\NUL1234567890" :: ByteString
            dataParser `shouldSucceedOn` chunk
            chunk ~?> dataParser `leavesUnconsumed` ""
        it "should fail when all sub-chunks are missing" $
            dataParser `shouldFailOn` ("" :: ByteString)
        it "should fail when the size and data chunks are missing" $
            dataParser `shouldFailOn` ("1111" :: ByteString)
        it "should fail when the chunk data is not as long as the chunk size" $ do
            dataParser `shouldFailOn` ("0001\SOH\NUL\NUL\NUL" :: ByteString)
            dataParser `shouldFailOn` ("0001\STX\NUL\NUL\NUL1" :: ByteString)
            dataParser `shouldFailOn` ("0001\ETX\NUL\NUL\NUL12" :: ByteString)
            dataParser `shouldFailOn` ("0001\EOT\NUL\NUL\NUL123" :: ByteString)
            dataParser `shouldFailOn` ("0001\ENQ\NUL\NUL\NUL1234" :: ByteString)
            dataParser `shouldFailOn` ("0001\ACK\NUL\NUL\NUL12345" :: ByteString)
            dataParser `shouldFailOn` ("0001\BEL\NUL\NUL\NUL123456" :: ByteString)
            dataParser `shouldFailOn` ("0001\BS\NUL\NUL\NUL1234567" :: ByteString)
            dataParser `shouldFailOn` ("0001\HT\NUL\NUL\NUL12345678" :: ByteString)
            dataParser `shouldFailOn` ("0001\LF\NUL\NUL\NUL123456789" :: ByteString)

specWav :: Spec
specWav =
    describe "wavParser" $
        it "should parse a valid wav file" $ do
            let validRiff = "RIFF1000WAVE"
            let validFormat = "fmt11112233444455556677"
            let validData = "0001\SOH\NUL\NUL\NUL1"
            let wav = concat [validRiff, validFormat, validData]
            wavParser `shouldSucceedOn` wav
            wav ~?> wavParser `leavesUnconsumed` ""

main :: IO ()
main = do
    hspec specRiff
    hspec specFormat
    hspec specData
    hspec specWav
