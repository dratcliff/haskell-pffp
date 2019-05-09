module Ch5 where

tc1 :: a -> a
tc1 x = x

tc2 :: a -> b -> a
tc2 x y = x

tc2'' :: b -> a -> b
tc2'' = tc2

tc2' :: a -> b -> b
tc2' x y = y

tc5 :: [a] -> [a]
tc5 x = x

tc5' :: [a] -> [a]
tc5' x = take 1 x

tc6 :: (b -> c) -> (a -> b) -> a -> c
tc6 x y z = x (y z)

tc7 :: (a -> c) -> a -> a
tc7 x y = y

tc7' :: (a -> b) -> a -> b
tc7' x y = x y