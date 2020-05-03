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

main :: IO ()
main = do
    hspec specChunkId
    hspec specChunkSize
    hspec specRiff
