module PoemLines where

stringSplit :: String -> Char -> [String]
stringSplit s c = split s where
    split [] = []
    split (c':xs)
        | c' == c   = split xs
        | otherwise = x' : split (dropWhile (/= c) x)
            where x' = (takeWhile (/= c) x)
                  x  = c':xs

myLines :: String -> [String]
myLines [] = []
myLines ('\n':xs) = myLines xs
myLines x = x' : myLines (dropWhile (/= '\n') x)
    where x' = (takeWhile (/= '\n') x) 

myLines' = flip stringSplit '\n'
myWords' = flip stringSplit ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
    \ symmetry?"
sentences = firstSen ++ secondSen
    ++ thirdSen ++ fourthSen

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]