module Cipher where

import Data.Char

caesar :: String -> Int -> String
caesar [] _     = []
caesar (x:xs) y  = shiftRight y x : caesar xs y where
    shiftRight y x = chr $ go y x where
        go y x
            | elem x ['a'..'z'] = wrap x y 'z' 
            | elem x ['A'..'Z'] = wrap x y 'Z'
            | otherwise = ord x
        wrap x y z = if (ord x) + y > ord z then
            (ord x) + y - 26 else (ord x) + y

unCaesar :: String -> Int -> String
unCaesar [] _       = []
unCaesar (x:xs) y  = shiftLeft y x : unCaesar xs y where
    shiftLeft y x = chr $ go y x where
        go y x
            | elem x ['a'..'z'] = wrap x y 'a' 
            | elem x ['A'..'Z'] = wrap x y 'A'
            | otherwise = ord x
        wrap x y z = if (ord x) - y < ord z then
            (ord x) - y + 26 else (ord x) - y

