module Ch9 where

import Data.Bool
import Data.Char 

safeTail        :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead        :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_)  = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool x y
    | x > y     = []
    | x == y    = [x]
    | otherwise = [x, y]

eftEnumOrd :: (Enum a, Ord a) => a -> a -> [a]
eftEnumOrd x y
    | x > y     = []
    | x == y    = [x]
    | otherwise = x : (eftEnumOrd (succ x) y)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftEnumOrd

eftInt :: Int -> Int -> [Int]
eftInt = eftEnumOrd

eftChar :: Char -> Char -> [Char]
eftChar = eftEnumOrd

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords x = x' : myWords (dropWhile (/= ' ') x)
    where x' = (takeWhile (/= ' ') x) 

acronymGenerator :: String -> String
acronymGenerator s =
    [x | x <- s, elem x ['A'..'Z']]

squareCube = [(x, y) | x <- mySqr, y <- myCube]
    where mySqr     = [x^2 | x <- [1..5]]
          myCube    = [x^3 | x <- [1..5]]  

squareCube' = [x | x <- squareCube, fst x < 50, snd x < 50]

withBool = map (\x -> bool x (-x) (x == 3)) [1..10]

multThree = filter (\x -> (rem x 3 == 0))

countMultThree = length . multThree

myFilter :: String -> [String]
myFilter = filter (\x -> 
    x /= "a" && x /= "an" && x /= "the") . words

myZip :: [a] -> [b] -> [(a, b)]
myZip [] x = []
myZip x [] = []
myZip x y  = (head x, head y) : myZip (tail x) (tail y)

myZipWith :: (a -> b -> c) 
          -> [a] -> [b] -> [c]
myZipWith f [] _   = []
myZipWith f _ []   = []
myZipWith f xs ys  = f (head xs) (head ys) :
    myZipWith f (tail xs) (tail ys)

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)

filterUpper :: String -> String
filterUpper = filter isUpper

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs

myToUpper :: String -> String
myToUpper []     = []
myToUpper (x:xs) = toUpper x : myToUpper xs

firstCap :: String -> Char
firstCap = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x 
    then True
    else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) =
    if (f x) == True
    then True
    else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) =
    if x == y
    then True
    else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' x y = myAny (== x) y

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ (x : [])

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy f (x:[])    = x
myMaximumBy f (x:y:xs)
    | f x y == GT   = myMaximumBy f (x:xs)
    | otherwise     = myMaximumBy f (y:xs)

myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy f (x:[])    = x
myMinimumBy f (x:y:xs)
    | f x y == LT   = myMinimumBy f (x:xs)
    | otherwise     = myMinimumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
