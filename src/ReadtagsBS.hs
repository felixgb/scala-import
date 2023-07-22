{-# LANGUAGE OverloadedStrings #-}

module ReadtagsBS where

import qualified Data.ByteString as B
import Data.Word
import Data.Int (Int64)
import Data.Ord (Ordering(..))
import Debug.Trace
import Data.List (find)

lf :: Word8
lf = toEnum $ fromEnum '\n'

space :: Word8
space = toEnum $ fromEnum ' '

tab :: Word8
tab = toEnum $ fromEnum '\t'

w2c :: Word8 -> Char
w2c = toEnum . fromEnum

c2w :: Char -> Word8
c2w = toEnum . fromEnum

string2bs :: String -> B.ByteString
string2bs i = B.pack $ map c2w i

bs2String :: B.ByteString -> String
bs2String i = map w2c (B.unpack i)

go3 :: IO ()
go3 = do
  bs <- B.readFile "dep_tags"
  let l = B.length bs
  -- print $ backUntilNewline bs (l `div` 2)
  putStrLn $ bs2String (binarySearch (string2bs "LocalDate") bs l)

backUntilNewline :: B.ByteString -> Int -> Int
backUntilNewline bs i = loop i
  where
    loop i = if bs `B.index` i == 10 then i + 1 else loop (i + 1)

-- backUntilNewline :: B.ByteString -> Int -> Maybe Int
-- backUntilNewline bs i = find pred [i..]
--   where
--     pred n = bs `B.index` n == 10

readUntilTab :: B.ByteString -> Int -> B.ByteString
readUntilTab bs i = B.takeWhile (/= tab) $ B.drop i bs
  where
    pred c = c /= tab

-- readUntilTab :: B.ByteString -> Int -> (Int, String)
-- readUntilTab bs i = loop [] bs i
--   where
--     loop acc bs i | bs `B.index` i == tab = (i, reverse acc)
--     loop acc bs i | otherwise = loop (w2c (bs `B.index` i) : acc) bs (i + 1)

readLastWord :: B.ByteString -> Int  -> B.ByteString
readLastWord bs i = readUntilTab bs $ backUntilNewline bs i

binarySearch :: B.ByteString -> B.ByteString -> Int -> B.ByteString
binarySearch inp bs size = loop (size `div` 2)
  where
    lastWord i = readLastWord bs i
    loop i = let w = lastWord i in 
      case inp `compare` w of
        LT -> loop (i `div` 2)
        GT -> loop (i + (i `div` 2))
        EQ -> w


