module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
 | n == 0 = "zero"
 | n == 1 = "one"
 | n == 2 = "two"
 | n == 3 = "three"
 | n == 4 = "four"
 | n == 5 = "five"
 | n == 6 = "six"
 | n == 7 = "seven"
 | n == 8 = "eight"
 | n == 9 = "nine"
 | otherwise = "unsupported-number"

 -- :[] the : is the cons operator, i.e., : []
digits :: Int -> [Int]
digits n = go n' []
    where n' = divMod n 10
          go (0, a) b = (a :[]) ++ b
          go (a, b) c = go (divMod a 10) ((b :[]) ++ c)


-- Prelude> wordNumber 12324546
-- "one-two-three-two-four-five-four-six"
wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
