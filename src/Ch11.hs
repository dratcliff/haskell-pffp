module Ch11 where

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer =
                Mini
              | Mazda
              | Tata
                deriving (Eq, Show)

data Airline = 
            PapuAir
          | CatapultsR'Us
          | TakeYourChancesUnited
            deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

isCar :: Vehicle -> Bool
isCar  (Car _ _)    = True
isCar  _          = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _)       = True
isPlane _               = False

areCars :: [Vehicle] -> [Bool]
areCars = map (\x -> isCar x)

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x
getManu _         = undefined

newtype Goats = Goats Int deriving Show
newtype NamedQuantity = NamedQuantity (Int, String)
newtype SumQuantities = SumQuantities (Int, Int)
newtype What a = What (a, a)

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 43

instance TooMany NamedQuantity where
    tooMany (NamedQuantity (a, b)) = a > 44

instance TooMany SumQuantities where
    tooMany (SumQuantities (a, b)) = a + b > 43

instance (Num a, TooMany a) => TooMany (What a) where
    tooMany (What a) = True

-- with this pragma {-# LANGUAGE FlexibleInstances #-}
-- can also do
-- instance (Num a, TooMany a) => TooMany (a, a) where
    -- tooMany (x, y) = True

--data Person = MkPerson String Int deriving (Eq, Show)

--namae :: Person -> String
--namae (MkPerson s _) = s

data Person =
    Person { name :: String
           , age  :: Int }
           deriving (Eq, Show)

jm = Person "julie" 108
ca = Person "chris" 16

--data Fiction = Fiction deriving Show
--data Nonfiction = Nonfiction deriving Show

--data BookType = FictionBook Fiction
--             | NonfictionBook Nonfiction
--              deriving Show

type AuthorName = String
data Author =
    Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)

type Gardener = String

data Garden = Gardenia Gardener
            | Daisy Gardener
            | Rose Gardener
            | Lilac Gardener
            deriving Show

data GuessWhat =
    Chickenbutt deriving (Eq, Show)

data Id a =
    MkId a deriving (Eq, Show)

data Product a b =
    Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst  :: a
                  , psecond :: b }
                  deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPig Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [
        GnuPlusLinux
      , OpenBSDPlusNevermindJustBSDStill
      , Mac
      , Windows
    ]

allLanguages :: [ProgLang]
allLanguages = 
    [
        Haskell
      , Agda
      , Idris
      , PureScript
    ]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { 
    lang = x, os = y } | x <- allLanguages, y <- allOperatingSystems ]