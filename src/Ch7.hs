module Ch7 where

{- addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1
    -} 

addOneIfOdd = \x ->
    case odd x of
        True -> f x
        False -> x
        where f x = x + 1

{-
addFive x y = (if x > y then y else x) + 5
-}

addFive = \x -> \y -> (if x > y then y else x) + 5

--mflip f = \x -> \y -> f y x
mflip f x y = f y x

data WherePenguinsLive =
      Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
    deriving (Eq, Show)

data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p)
                      || (antarcticPenguin p)

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- if x + 1 == 1 then "AWESOME" else "wut"
funcZ x =
    case x + 1 == 1 of
        True  -> "AWESOME"
        False -> "wut"

pal xs =
    case xs == reverse xs of
        True  -> "yes"
        False -> "no"

functionC x y =
    case x > y of
        True  -> x
        False -> y

ifEvenAdd2 n =
    case even n of
        True  -> n + 2
        False -> n

nums x =
    case compare x 0 of
        LT -> -1
        EQ -> 0
        GT -> 1

myAbs x = if x < 0 then (-x) else x

myAbs' x =
    case x < 0 of
        True  -> (-x)
        False -> x

myAbs'' x
  | x < 0 = (-x)
  | otherwise = x

avgGrade :: (Fractional a, Ord a)
    => a -> Char
avgGrade x
    | y >= 0.7 = 'C'    
    | y >= 0.9 = 'A'
    | y >= 0.8 = 'B'
    | y >= 0.59 = 'D'
    | y < 0.59 = 'F'
    where y = x / 100

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = tens x
  where modDiv    = flip divMod
        modDivTen = modDiv 10
        div'      = fst . modDivTen
        div''     = snd . modDivTen
        tens      = div'' . div'

hundredsDigit :: Integral a => a -> a
hundredsDigit x = tensDigit' . f $ x
  where f = flip div $ 10

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

foldBool :: a -> a -> Bool -> a
foldBool x y z =
    case z of
        True  -> y
        False -> x

foldBool' :: a -> a -> Bool -> a
foldBool' x y z
 | z         = y
 | otherwise = x
 
g :: (a -> b) -> (a, c) -> (b, c) 
g f (a, c) = (f a, c)

data SumOfThree a b c =
    FirstPossible  a
  | SecondPossible b
  | ThirdPossible  c
  deriving (Eq, Show)

sumToInt :: SumOfThree a b c -> Integer
sumToInt (FirstPossible _)  = 0
sumToInt (SecondPossible _) = 1
sumToInt (ThirdPossible _)  = 2

-- not pointfree, imagine pointfree versions
blah x = x
addAndDrop x y = x + 1
reverseMkTuple a b = (b, a)
reverseTuple (a, b) = (b, a)

-- pointfree versions of the above
blah' = id
addAndDrop' = const . (1 +)
reverseMkTuple' = flip (,)
reverseTuple' = uncurry (flip (,))