{-| Extract audio data URIs from HTML and output it to files.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.HTML.AudioDataURIs where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy (Text(..))
import System.FilePath (joinPath)
import System.Environment (getArgs)
import Data.ByteString.Base64.Lazy (decodeLenient)

import Debug.Trace (traceShowId)
import Data.Data (Data)

import Text.HTML.TagSoup
import Text.StringLike

import Data.List (stripPrefix)

import Control.Monad.State

type DataURIsState = State [(FilePath, ByteString)]

-- | Extracts audio data URIs from HTML and returns them as a list of filename/ByteString tuples.
--
-- Takes a URI prefix where the files will be available online and the HTML as
-- input.
convert :: String -> Text -> (Text, [(FilePath, ByteString)])
convert uri html
  | last uri == '/' = (renderTags tags', results)
  | otherwise = convert (uri ++ "/") html
  where tags = parseTags html
        (tags', results) = runState (mapM (extractAudioURIs uri) tags) []

-- | Statefully extracts audio data URIs from HTML5 "source" tags.
extractAudioURIs :: String -> Tag Text -> DataURIsState (Tag Text)
extractAudioURIs uri x =
  case x of
    TagOpen "source" atts -> do
      let mimetype = lookup "type" atts
      case mimetype of
        Just "audio/wav" -> do
          newAtts <- mapM g atts
          return $ TagOpen "source" newAtts
        _ -> return x
    _ -> return x
  where
    g :: Attribute Text -> DataURIsState (Attribute Text)
    g ("src", dataUri) = do
      n <- gets length
      let fname = "out" ++ show n ++ ".wav"
      modify (\xs -> (fname, parseDataURI dataUri) : xs)
      return ("src", fromString $ uri ++ fname)
    g x = return x

-- | Turn a WAV data URI into a ByteString or fail.
parseDataURI :: Text -> ByteString
parseDataURI xs =
  case T.stripPrefix "data:audio/wav;base64," xs of
    Just xs' -> decodeLenient $ E.encodeUtf8 xs'
    Nothing -> error "Unexpected data URI"

-- | Utility function to write the list of filenames/file data into a directory.
writeToFiles :: FilePath -> [(FilePath, ByteString)] -> IO ()
writeToFiles outDir = mapM_ f
  where f (fname, value) = BL.writeFile (joinPath [outDir, fname]) value

-- | Main entry point: pass URI prefix and outdir as args, input HTML to STDIN.
cliMain :: IO ()
cliMain = do
  [uri, outDir] <- getArgs
  inputHTML <- TIO.getContents
  let (newHTML, results) = convert uri inputHTML
  writeToFiles outDir results
  TIO.putStr newHTML
