{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.AudioDataURIs where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)

convert :: String -> FilePath -> Text -> (Text, [(FilePath, ByteString)])
convert uri outDir html = ("", [])

cliMain :: IO ()
cliMain = return ()
