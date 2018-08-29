{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Test.HUnit

import ImportSorter

main = runTestTT $ TestList
  [ TestLabel "check imports turned to text ok" textifyTest
  , TestLabel "check imports collapsed" collapseTest
  ]

textifyTest = TestCase (assertEqual "Text" (map textifyImport imports) importTexts)
  where
    imports =
      [ Import "foo.lol.bar" Wildcard
      , Import "foo.lol.bar" (Single "Foo")
      ]
    importTexts =
      [ "foo.lol.bar._"
      , "foo.lol.bar.Foo"
      ]

collapseTest = TestCase (assertEqual "Collapse" collapsed (collapseImports imports))
  where
    imports =
      [ Import "foo.lol.bar" (Single "Foo")
      , Import "foo.lol.bar" (Multi ["Dog", "Cat"])
      , Import "foo.bloop.bar" (Single "Bar")
      , Import "foo.lol.dong" (Multi ["Fish", "Dish"])
      , Import "foo.lol.bar" (Single "Bar")
      ]
    collapsed = 
      [ Import "foo.lol.bar" (Multi ["Foo", "Dog", "Cat", "Bar"])
      , Import "foo.bloop.bar" (Multi ["Bar"])
      , Import "foo.lol.dong" (Multi ["Fish", "Dish"])
      ]
       
