{-# LANGUAGE OverloadedStrings #-}

import           Data.Binary           (encode)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Lazy  as BL
import           Data.Word
import           Debug.Trace
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
            -- and the encoding symbols will map to ASCII. Therefore the encoding `\NUL\SOH\NUL\NUL`
            -- equals 256
            -- TODO: consider using QuickCheck
            let chunkSize = encodeWord 1
            let chunk = BL.concat ["0001", chunkSize, "1"]
            dataParser `shouldSucceedOn` chunk
            chunk ~?> dataParser `leavesUnconsumed` ""
        it "should parse a valid data chunk of size 10" $ do
            let chunkSize = encodeWord 10
            let chunk = BL.concat ["0001", chunkSize, "1234567890"]
            dataParser `shouldSucceedOn` chunk
            chunk ~?> dataParser `leavesUnconsumed` ""
        it "should fail when all sub-chunks are missing" $
            dataParser `shouldFailOn` ("" :: ByteString)
        it "should fail when the size and data chunks are missing" $
            dataParser `shouldFailOn` ("1111" :: ByteString)
        it "should fail when the chunk data is not as long as the chunk size" $ do
            -- Will fail when attempting to parse chunkSize N bytes of a chunkData size of N - 1
            dataParser `shouldFailOn` BL.concat ["0001", encode (1 :: Word32), ""]
            dataParser `shouldFailOn` BL.concat ["0001", encode (2 :: Word32), "1"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (3 :: Word32), "12"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (4 :: Word32), "123"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (5 :: Word32), "1234"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (6 :: Word32), "12345"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (7 :: Word32), "123456"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (8 :: Word32), "1234567"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (9 :: Word32), "12345678"]
            dataParser `shouldFailOn` BL.concat ["0001", encode (10 :: Word32), "123456789"]

specWav :: Spec
specWav =
    describe "wavParser" $
        it "should parse a valid wav file" $ do
            let validRiff = "RIFF1000WAVE"
            let validFormat = "fmt11112233444455556677"
            let validData = BL.concat ["0001", encodeWord 1, "1"]
            let wav = BL.concat [validRiff, validFormat, validData]
            wavParser `shouldSucceedOn` wav
            wav ~?> wavParser `leavesUnconsumed` ""

{-|
    Encodes a Word32 into a lazy ByteString and shifts its result to the left.
    Example:

    encodeWord (1 :: Word32)
    -- "\NUL\SOH\NUL\NUL"
-}
encodeWord :: Word32 -> BL.ByteString
encodeWord x =
    let m = x * 256 in
        trace ("value: " ++ show x) encode (m * 256 :: Word32)

main :: IO ()
main = do
    hspec specRiff
    hspec specFormat
    hspec specData
    hspec specWav
