module Main where

import System.Environment

import ImportSorter
import Readtags
import ReadtagsBS

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["find", toFind] -> go2 toFind
--     ["insert", word, file] -> dispatch file word

main :: IO ()
main = go3
