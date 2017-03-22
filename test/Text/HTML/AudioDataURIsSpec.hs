{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.AudioDataURIsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Text.HTML.AudioDataURIs

exampleHTML :: T.Text
exampleHTML = T.unlines
  [ "<audio controls=\"controls\" >"
  , " <source src=\"data:audio/wav;base64,UklGRigAAABXQVZFZm10IBAAAAABAAEAIlYAAESsAAACABAAZGF0YQQAAAAAAP9/\" type=\"audio/wav\" />"
  , "   Your browser does not support the audio element."
  , "</audio>"
  ]

exampleConvertedHTML :: T.Text
exampleConvertedHTML = T.unlines
  [ "<audio controls=\"controls\" >"
  , " <source src=\"/static/out/out1.wav\" type=\"audio/wav\" />"
  , "   Your browser does not support the audio element."
  , "</audio>"
  ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "convert" $ do
    context "when given a uri prefix (ending in /), an out directory, and an HTML string" $ do
      it "returns a new HTML string and a list of (FilePath, ByteString) tuples" $ do
        convert "/static/out/" "_site/out/" exampleHTML `shouldBe` (exampleConvertedHTML, [("out1.wav", BL.empty)])
