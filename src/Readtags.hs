{-# LANGUAGE OverloadedStrings #-}

module Readtags where

import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import Data.List
import Data.Maybe

import System.Directory
import System.FilePath

data Tag = Tag
  { _def :: Text
  , _loc :: FilePath
  } deriving (Show)

allDirsTo :: FilePath -> [FilePath]
allDirsTo dir = tail $ scanl combine "" (splitPath dir)

isBelowHome :: FilePath -> Bool
isBelowHome p = not (p `elem` ["/", "/home/"])

getSearchDirs :: FilePath -> [FilePath]
getSearchDirs = filter isBelowHome . allDirsTo

findTagsFiles :: IO [FilePath]
findTagsFiles = do
  current <- getCurrentDirectory
  findFiles (getSearchDirs current) "tags"

loadTags :: IO [Text]
loadTags = findTagsFiles 
  >>= mapM readOnlyUTF8
  >>= pure . T.lines . T.concat

readPackageDeclaration :: FilePath -> IO (Maybe Text)
readPackageDeclaration path = fmap getPackageFromFile (TIO.readFile path)

-- concat package statements, see https://stackoverflow.com/questions/3541053/multiple-packages-definition
getPackageFromFile :: Text -> Maybe Text
getPackageFromFile txt = case filter isPackageLine (T.lines txt) of
  [] -> Nothing
  pkgs -> Just $ T.intercalate "." (catMaybes $ map getPackagePath pkgs)

-- drop package statement and check second word is not object
isPackageLine pkg = (T.isPrefixOf "package" $ T.strip pkg)

getPackagePath :: Text -> Maybe Text
getPackagePath = fmap T.strip . T.stripPrefix "package" . noSemis
  where noSemis = T.filter (/= ';')

readTagLine :: Text -> Maybe Tag
readTagLine ln = case T.words ln of
  (def : loc : _) -> Just $ Tag def (T.unpack loc)
  _ -> Nothing

tagPackage :: Tag -> IO (Maybe Text)
tagPackage (Tag def loc) = readPackageDeclaration loc

-- filter lines that do not start with caps
-- remove dups by inserting into set
-- serialize to vim format

readOnlyUTF8 :: FilePath -> IO Text
readOnlyUTF8 path = BS.readFile path 
  >>= pure . T.unlines . stripBadLines . C8.lines
  where 
    stripBadLines = catMaybes . map decode
    decode bs = case decodeUtf8' bs of
      Left unicodeErr -> Nothing
      Right txt -> Just txt

tagMatches :: Text -> Text -> Bool
tagMatches ident tag = case T.words tag of
  (w : _) -> w == ident
  _ -> False

go = do
  tagsLines <- loadTags
  let matched = filter (tagMatches "num") tagsLines
  let tags = catMaybes (map readTagLine matched)
  packages <- mapM tagPackage tags
  mapM_ TIO.putStrLn (catMaybes packages)
