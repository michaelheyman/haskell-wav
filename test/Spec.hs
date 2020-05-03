{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString       (ByteString)
import           Parser
import           Test.Hspec
import           Test.Hspec.Attoparsec

specChunkId :: Spec
specChunkId =
    describe "parseChunkID" $
      it "should parse the bytestring riff" $ do
        parseChunkID `shouldSucceedOn` ("RIFF" :: ByteString)
        ("RIFF" :: ByteString) ~?> parseChunkID `leavesUnconsumed` ""

specChunkSize :: Spec
specChunkSize =
    describe "parseChunkSize" $ do
        -- TODO: use QuickCheck to generate valid Word32 and replace these tests
        it "should parse the 32-bit word '0000'" $ do
            let word = "0000" :: ByteString
            parseChunkSize `shouldSucceedOn` word
            word ~?> parseChunkSize `leavesUnconsumed` ""
        it "should parse the 32-bit word '1000'" $ do
            let word = "1000" :: ByteString
            parseChunkSize `shouldSucceedOn` word
            word ~?> parseChunkSize `leavesUnconsumed` ""
        it "should parse the 32-bit word '5555'" $ do
            let word = "5555" :: ByteString
            parseChunkSize `shouldSucceedOn` word
            word ~?> parseChunkSize `leavesUnconsumed` ""
        it "should parse the 32-bit word '9999'" $ do
            let word = "9999" :: ByteString
            parseChunkSize `shouldSucceedOn` word
            word ~?> parseChunkSize `leavesUnconsumed` ""

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
        it "should fail when the size and format chunks are missing" $
            riffParser `shouldFailOn` ("RIFF1000" :: ByteString)
        it "should fail when the chunkFormat does not equal 'WAVE'" $
            riffParser `shouldFailOn` ("RIFF1000OGG" :: ByteString)

specAudioFormat :: Spec
specAudioFormat =
    describe "parseAudioFormat" $
      it "should parse the audio format" $ do
        parseAudioFormat `shouldSucceedOn` ("10" :: ByteString)
        ("10" :: ByteString) ~?> parseAudioFormat `leavesUnconsumed` ""

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


main :: IO ()
main = do
    hspec specChunkId
    hspec specChunkSize
    hspec specRiff
    hspec specAudioFormat
    hspec specFormat
