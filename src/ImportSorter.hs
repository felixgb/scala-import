{-# LANGUAGE OverloadedStrings #-}

module ImportSorter where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.List

dispatch :: [String] -> IO ()
dispatch args = case args of
  [ln, path] -> TIO.readFile path >>= pure . insertImport ln >>= TIO.writeFile path

-- filter out the starting import lines
insertImport :: String -> Text -> Text
insertImport importLine text = T.unlines (T.pack importLine : T.lines text)

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
