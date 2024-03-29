{-# LANGUAGE OverloadedStrings #-}

module Readtags where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Text (Text)
import Data.Text.Encoding
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.Function
import Data.Ord (Ordering(..))
import Control.Monad (unless, when)
import Control.Monad.Loops (takeWhileM)
import System.Directory
import System.FilePath
import System.IO
import Debug.Trace
import Data.Bits

data Tag = Tag
  { _def :: Text
  , _loc :: FilePath
  } deriving (Show, Eq, Ord)

allDirsTo :: FilePath -> [FilePath]
allDirsTo dir = tail $ scanl combine "" (splitPath dir)

isBelowHome :: FilePath -> Bool
isBelowHome p = not (p `elem` ["/", "/home/"])

getSearchDirs :: FilePath -> [FilePath]
getSearchDirs = filter isBelowHome . allDirsTo

findTagsFiles :: IO [FilePath]
findTagsFiles = do
  current <- getCurrentDirectory
  (++) <$> findFiles (getSearchDirs current) "tags" <*> findFiles (getSearchDirs current) "dep_tags"

loadTags :: IO [Text]
loadTags = findTagsFiles 
  >>= mapM readOnlyUTF8
  >>= pure . T.lines . T.concat

readPackageDeclaration :: FilePath -> IO (Maybe Text)
readPackageDeclaration path = fmap getPackageFromFile (TIO.readFile (baseDirectory ++ path))

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

filterPackageObjects :: [Text] -> [Text]
filterPackageObjects = filter (not . T.isSuffixOf "{")

getMatchingTags :: Text -> [Text] -> [Tag]
getMatchingTags toMatch = catMaybes
  . map readTagLine
  . filter (tagMatches toMatch)

appendIdent :: Text -> Text -> Text
appendIdent ident ln = T.concat [ln, ".", ident]

thing ident = Set.map (appendIdent ident) . Set.fromList . filterPackageObjects . catMaybes

go :: String -> IO ()
go ident = loadTags 
  >>= mapM tagPackage . getMatchingTags packed
  >>= mapM_ TIO.putStrLn . thing packed
  where
    packed = T.pack ident

doit :: Text -> IO (Maybe Text)
doit line = case readTagLine line of
  Just tag -> tagPackage tag
  Nothing -> pure Nothing

baseDirectory = "./"

go2 :: String -> String -> IO ()
go2 path ident = do
  lines <- withFile (baseDirectory ++ path) ReadMode (seekUntilMatch $ T.pack ident)
  xs <- mapM doit lines
  let toPrint = thing (T.pack ident) xs
  mapM_ TIO.putStrLn toPrint
  -- lines <- withFile "tags" ReadMode (seekUntilMatch $ T.pack ident)
  -- xs <- mapM doit lines
  -- let toPrint = thing (T.pack ident) xs
  -- mapM_ TIO.putStrLn toPrint

showline :: Text -> Handle -> IO Text
showline inp handle = do
  filesizeBytes <- hFileSize handle
  hSeek handle AbsoluteSeek (filesizeBytes `div` 2)
  readUntilNewline handle
  TIO.hGetLine handle

seekUntilMatch :: Text -> Handle -> IO [Text]
seekUntilMatch inp handle = do
  filesizeBytes <- hFileSize handle
  loop filesizeBytes inp handle (filesizeBytes `div` 2) 1

whatever :: Integer -> Integer -> Integer
whatever size i = max 1 $ shift size $ fromIntegral (negate (i + 1))

loop :: Integer -> Text -> Handle -> Integer -> Integer -> IO [Text]
loop size inp handle pos iterations = do
  hSeek handle AbsoluteSeek pos
  readUntilNewline handle
  line <- TIO.hGetLine handle
  let maxIterations = ceiling (log $ fromIntegral size) + 10
  if iterations > maxIterations
  then pure []
  else
    case compareTagLine inp line of
       LT -> loop size inp handle (pos - (whatever size iterations)) (iterations + 1)
       GT -> loop size inp handle (pos + (whatever size iterations)) (iterations + 1)
       EQ -> do
        readBackwards inp handle
        beforeLine <- readUntilMatching inp handle
        fmap (\ls -> beforeLine : ls) $ readAllMatching inp handle

compareTagLine :: Text -> Text -> Ordering
compareTagLine text line = text `compare` (head $ T.words line)

readUntilNewline :: Handle -> IO ()
readUntilNewline handle = do
  c <- hGetChar handle
  if c == '\n' then pure () else readUntilNewline handle

readUntilMatching :: Text -> Handle -> IO Text
readUntilMatching inp handle = do
  line <- TIO.hGetLine handle
  if (tagMatches inp line) then pure line
  else readUntilMatching inp handle

readAllMatching :: Text -> Handle -> IO [Text]
readAllMatching inp handle = do
  line <- TIO.hGetLine handle
  if (tagMatches inp line) then fmap (\ls -> line : ls) (readAllMatching inp handle)
  else pure []

readBackwards :: Text -> Handle -> IO () 
readBackwards inp handle = do
  hSeek handle RelativeSeek (-10000)
  readUntilNewline handle
  line <- TIO.hGetLine handle
  let m = tagMatches inp line
  when (tagMatches inp line) $ readBackwards inp handle
