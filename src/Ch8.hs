module Ch8 where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) =>
  a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

--applyTimes 5 (+1) 5
-- (+1) (applyTimes 4 (+1) 5)
-- (+1) (+1) (applyTimes 3 (+1) 5)
-- (+1) (+1) (+1) (applyTimes 2 (+1) 5)
-- (+1) (+1) (+1) (+1) (applyTimes 1 (+1) 5)
-- (+1) (+1) (+1) (+1) (+1) (applyTimes 0 (+1) 5)
-- (+1) (+1) (+1) (+1) (+1) 5
-- 10

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

data DividedResult =
    Result Integer
  | DividedByZero
  deriving Show

dividedBy :: Integral a => a -> a -> (DividedResult, a)
dividedBy num denom 
    | denom == 0 = (DividedByZero, 0)
    | otherwise = fixSign sign (go num' denom' 0)
        where 
            num' = abs num
            denom' = abs denom
            sign = if num * denom < 0 then (-1) else 1
            fixSign s n
                | s == 1 = (Result (fst n), snd n)
                | otherwise = (Result (negate (fst n)), snd n)
            go n d count
                | n < d = (count, n)
                | otherwise =
                    go (n - d) d (count + 1)

-- dividedBy 15 2 = go 15 2 0
-- go 13 2 1
-- go 11 2 2
-- go 9 2 3
-- go 7 2 4
-- go 5 2 5
-- go 3 2 6
-- go 1 2 7 = (7, 1)

sumToN :: (Eq a, Num a) => a -> a
sumToN 0 = 0
sumToN n = n + sumToN (n - 1)

multiply :: (Integral a) => a -> a -> a
multiply n 0 = 0
multiply n o = n + (multiply n (o - 1))

mc91 :: Integer -> Integer
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91 (mc91 (x + 11))