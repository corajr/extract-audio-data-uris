{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.AudioDataURIs where

import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import System.FilePath (joinPath)

convert :: String -> Text -> (Text, [(FilePath, ByteString)])
convert uri html = ("", [])

writeToFiles :: FilePath -> [(FilePath, ByteString)] -> IO ()
writeToFiles outDir = mapM_ f
  where f (fname, value) = BL.writeFile (joinPath [outDir, fname]) value

cliMain :: IO ()
cliMain = return ()
