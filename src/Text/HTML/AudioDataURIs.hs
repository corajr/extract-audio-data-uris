{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.HTML.AudioDataURIs where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Lazy (Text(..))
import System.FilePath (joinPath)
import System.Environment (getArgs)
import Data.ByteString.Base64.Lazy (decodeLenient)

import Data.Data (Data)

import Text.HTML.TagSoup (Attribute(..))
import Text.HTML.TagSoup.Tree
import Text.StringLike

import Data.List (stripPrefix)

import Control.Monad.State.Strict

type DataURIsState = State [(FilePath, ByteString)]

convert :: String -> Text -> (Text, [(FilePath, ByteString)])
convert uri html = (renderTree tree', results)
  where tree = parseTree html
        (tree', results) = runState (transformTreeM (extractAudioURIs uri) tree) []

transformTreeM :: (Monad m) => (TagTree str -> m [TagTree str]) -> [TagTree str] -> m [TagTree str]
transformTreeM act = liftM concat . mapM f
  where
    f (TagBranch a b inner) = transformTreeM act inner >>= (act . TagBranch a b)
    f x = act x

extractAudioURIs :: (StringLike str, Data str) => String -> TagTree str -> DataURIsState [TagTree str]
extractAudioURIs uri = f
  where
    f :: (StringLike str) => TagTree str -> DataURIsState [TagTree str]
    f x@(TagBranch "source" atts inner) = do
      let (mimetype, src) = (lookup "type" atts, lookup "src" atts)
      case (mimetype, src) of
        (Just "audio/wav", _) -> do
          newAtts <- mapM g atts
          return [TagBranch "source" newAtts inner]
        _ -> return [x]
    f x = return [x]
    g :: (StringLike str) => Attribute str -> DataURIsState (Attribute str)
    g ("src", dataUri) = do
      xs <- get
      let fname = "out" ++ show (length xs) ++ ".wav"
      modify (\xs -> (fname, parseDataURI dataUri) : xs)
      return ("src", fromString $ uri ++ fname)
    g x = return x

parseDataURI :: StringLike str => str -> ByteString
parseDataURI = getData . toString
  where getData xs = case stripPrefix "data:audio/wav;base64," xs of
          Just xs' -> decodeLenient $ BL.pack xs'
          Nothing -> error "Unexpected data URI"

writeToFiles :: FilePath -> [(FilePath, ByteString)] -> IO ()
writeToFiles outDir = mapM_ f
  where f (fname, value) = BL.writeFile (joinPath [outDir, fname]) value

cliMain :: IO ()
cliMain = do
  [uri, outDir] <- getArgs
  inputHTML <- TIO.getContents
  let (newHTML, results) = convert uri inputHTML
  writeToFiles outDir results
  TIO.putStr newHTML
