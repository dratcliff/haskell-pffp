module Ch10 where

fibs    = takeWhile (\x -> x < 100) $ 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

factorial :: Integer -> Integer
factorial x = (take (x' + 1) (scanl (*) 1 [1..])) !! x' where
    x' = fromIntegral x

stops  = "pbtdkg"
vowels = "aeiou"

svs s v = [(x, y, z) | x <- s, y <- v, z <- s]
svs' s v = filter startsWithP $ svs s v where
    startsWithP ('p', _, _) = True
    startsWithP (_, _, _)   = False
--svs stops vowels
--svs' stops vowels

nouns = ["cat", "dog", "ball", "boy"]
verbs = ["runs", "eats", "throws"]

--svs nouns verbs

seekritFunc x = 
    div (sum (map length (words x)))
        (length (words x))

seekritFunc' x = 
    (fromIntegral (sum (map length (words x)))) /
        (fromIntegral (length (words x)))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\y -> \z -> f y || z) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\y -> \z -> y == x || z) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x y = any (\z -> x == z) y

myElem'' x = any (\z -> x == z)

myElem''' :: Eq a => a -> [a] -> Bool
myElem''' = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x y -> (f x) ++ y) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f z = foldr (\x y -> if f x y == GT then x else y) (head z) z

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a

myMinimumBy f z = foldr (\x y -> if f x y == LT then x else y) (head z) z