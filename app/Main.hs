module Main where

import System.Environment

import ImportSorter

main :: IO ()
main = getArgs >>= dispatch
