{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString       (ByteString)
import           Parser
import           Test.Hspec
import           Test.Hspec.Attoparsec

specChunkId :: Spec
specChunkId =
    describe "parseChunkID" $
      it "should parse the bytestring riff" $
        parseChunkID `shouldSucceedOn` ("RIFF" :: ByteString)

specChunkSize :: Spec
specChunkSize =
    describe "parseChunkSize" $ do
        it "should parse the 32-bit word '0000'" $
            parseChunkSize `shouldSucceedOn` ("0000" :: ByteString)
        it "should parse the 32-bit word '1000'" $
            parseChunkSize `shouldSucceedOn` ("1000" :: ByteString)
        it "should parse the 32-bit word '5555'" $
            parseChunkSize `shouldSucceedOn` ("5555" :: ByteString)
        it "should parse the 32-bit word '9999'" $
            parseChunkSize `shouldSucceedOn` ("9999" :: ByteString)

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
