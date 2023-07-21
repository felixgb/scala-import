{-# LANGUAGE OverloadedStrings #-}

module ImportSorter where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.List

dispatch :: String -> String -> IO ()
dispatch path ln = TIO.readFile path
  >>= pure . insertImport ("import " ++ ln)
  >>= TIO.writeFile path

insertImport :: String -> Text -> Text
insertImport toInsert txt = T.unlines $ insertAfterPackage (T.pack toInsert) txt

insertThirdLine :: Text -> Text -> [Text]
insertThirdLine importLine text = case T.lines text of
  (a : b : c : rest) -> a : b : c : importLine : rest
  other -> importLine : other

insertAfterPackage :: Text -> Text -> [Text]
insertAfterPackage importLine text = loop importLine $ T.lines text

loop :: Text -> [Text] -> [Text]
loop importLine (l : ls) = if "package" `T.isPrefixOf` l then l : "" : importLine : ls else l : (loop importLine ls)
loop importLine other = importLine : other

data Import = Import
  { _path :: Text
  , _typ :: ImportStyle
  } deriving (Eq, Show)

data ImportStyle
  = Single Text
  | Multi [Text]
  | Wildcard
  deriving (Eq, Show)

textifyImport :: Import -> Text
textifyImport (Import path typ) = T.concat [path, ".", textifyTyp typ]
  where
    textifyTyp typ = case typ of
      Single txt -> txt
      Multi txts -> T.concat ["{", T.intercalate ", " txts, "}"]
      Wildcard -> "_"

squashTypes :: [ImportStyle] -> ImportStyle
squashTypes = Multi . foldr ((++) . getText) []
  where
    getText (Single t) = [t]
    getText (Multi ts) = ts
    getText Wildcard = []

squash :: Text -> [ImportStyle] -> Import
squash path = Import path . squashTypes

collapseImports :: [Import] -> [Import]
collapseImports = Map.elems 
  . Map.mapWithKey squash
  . Map.fromListWith (++)
  . map (\i -> (_path i, [_typ i]))
