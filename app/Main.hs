module Main where

import System.Environment

import ImportSorter
import Readtags
import System.CPUTime

time :: IO t -> IO t
time a = do
  start <- getCPUTime
  r <- a
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10 ^ 9) :: Double
  putStrLn (show diff)
  pure r

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["find", toFind] -> do
      go2 "dep_tags" toFind
      go2 "tags" toFind
    ["insert", word, file] -> dispatch file word

-- main :: IO ()
-- main = go3
