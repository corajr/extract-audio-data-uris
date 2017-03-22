{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.AudioDataURIsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Text.HTML.AudioDataURIs

import System.FilePath (joinPath)
import System.Directory (listDirectory)
import System.IO.Temp (withSystemTempDirectory)

exampleHTML :: T.Text
exampleHTML = T.unlines
  [ "<audio controls=\"controls\" >"
  , " <source src=\"data:audio/wav;base64,UklGRigAAABXQVZFZm10IBAAAAABAAEAIlYAAESsAAACABAAZGF0YQQAAAAAAP9/\" type=\"audio/wav\" />"
  , "   Your browser does not support the audio element."
  , "</audio>"
  , "<div><!-- MORE --></div>"
  ]

exampleConvertedHTML :: T.Text
exampleConvertedHTML = T.unlines
  [ "<audio controls=\"controls\">"
  , " <source src=\"/static/out/out0.wav\" type=\"audio/wav\"></source>"
  , "   Your browser does not support the audio element."
  , "</audio>"
  , "<div><!-- MORE --></div>"
  ]


dataUri :: T.Text
dataUri = "data:audio/wav;base64,UklGRigAAABXQVZFZm10IBAAAAABAAEAIlYAAESsAAACABAAZGF0YQQAAAAAAP9/"

exampleWave :: BL.ByteString
exampleWave = "RIFF(\NUL\NUL\NULWAVEfmt \DLE\NUL\NUL\NUL\SOH\NUL\SOH\NUL\"V\NUL\NULD\172\NUL\NUL\STX\NUL\DLE\NULdata\EOT\NUL\NUL\NUL\NUL\NUL\255\DEL"

writeToTemp :: [(FilePath, BL.ByteString)] -> IO [(FilePath, BL.ByteString)]
writeToTemp inp =
  withSystemTempDirectory "out_files" $ \dirname -> do
    writeToFiles dirname inp
    written <- listDirectory dirname
    mapM (\x -> (,) <$> pure x <*> BL.readFile (joinPath [dirname, x])) written

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "convert" $ do
    context "when given a uri prefix (ending in /) and an HTML string" $ do
      it "returns a new HTML string (preserving comments) and a list of (FilePath, ByteString) tuples" $ do
        convert "/static/out/" exampleHTML `shouldBe` (exampleConvertedHTML, [("out0.wav", exampleWave)])
  describe "parseDataURI" $ do
    it "takes a base64-encoded Text as input and gives back a ByteString" $ do
      parseDataURI dataUri `shouldBe` exampleWave
  describe "writeToFiles" $ do
    context "when given an out directory" $
      it "writes the (FilePath, ByteString) tuples to files in the directory" $ do
      let xs = [("out1.txt", "Test."), ("out2.txt", "Test2.")]
      writeToTemp xs `shouldReturn` xs
