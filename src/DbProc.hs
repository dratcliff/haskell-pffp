module DbProc where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)
                  
theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]

filterDbDate = map g . filter f where
    f (DbDate _)   = True
    f _            = False
    g (DbDate a)   = a

filterDbNumber :: [DatabaseItem]
               -> [Integer]

filterDbNumber = map g . filter f where
    f (DbNumber _)      = True
    f _                 = False
    g (DbNumber a)      = a

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent x = foldr max (head f) f where
    f = filterDbDate x

sumDb :: [DatabaseItem] -> Integer
sumDb x = foldr max (head f) f where
    f = filterDbNumber x

myAcc :: Integer 
      -> (Integer, Integer) 
      -> (Integer, Integer)
myAcc x (y, z) = (y + 1, z + x)

avgDb :: [DatabaseItem] -> Double
avgDb x = myAvg $ foldr myAcc (0, 0) f where
    f = filterDbNumber x
    myAvg (y, z) = (fromIntegral z) / (fromIntegral y)