{-# LANGUAGE ConstraintKinds, PolyKinds, ScopedTypeVariables, TypeApplications, GeneralizedNewtypeDeriving, AllowAmbiguousTypes #-}
module Tensor where
import GHC.TypeLits
import GHC.Enum
import Numeric.AD
import FreeNum
type BEnum a = (Enum a, Bounded a)

enumAll :: BEnum a => [a]
enumAll = [minBound .. maxBound]

instance BEnum a => Foldable ((->) a) where
    foldMap f g = foldMap f (map g enumAll)  

    --  Traversable holds the keys to the universe
    
instance BEnum a => Traversable ((->) a) where
    traverse f x =  (fmap unsafe_convert) (traverse f (map x enumAll))

    {-
    traverse f :: (a' -> f b) g :: a -> a' = (fmap f g)
        :: f (a -> b)
        sequenceA f :: a -> f b = [f a | a <- enumAll] :: f (a -> b)
        -}


sum_lu l u f =sum [f i |  i <- [l .. u]]
-- sum :: (Foldable f, Num a) => f a -> a
-- index and tabulate.
unsafe_convert :: forall a b. BEnum a => [b] -> (a -> b)
unsafe_convert xs = \a -> xs !! ((fromEnum a) - (fromEnum (minBound @a)))


--newtype Fin n = Fin Int deriving (Num, Enum, Eq, Ord, Show)
newtype Fin = Fin {runFin :: Int} deriving (Num, Enum, Eq, Ord, Show)
newtype Circle = Circle {runCircle :: Double} deriving (Num, Floating, Fractional, Eq, Ord, Show)
instance Enum Circle where
    toEnum n = Circle ((fromIntegral n) * dx) where dx = 2 * pi / 128 -- 128 samples
    fromEnum (Circle x) = round (x / dx) where dx = 2 * pi / 128 

instance Bounded Circle where
    minBound = Circle 0
    maxBound = Circle (2 * pi - dx) where dx = 2 * pi / 128

instance Bounded Fin where
    minBound = 0
    maxBound = 127



v :: Bool -> Int
v False = 1
v True = 2

ex3 = sum v

dot :: (BEnum a, Num b) => (a -> b) -> (a -> b) -> b
dot v w = sum $ \i -> (v i) * (w i)

-- kronecker delta
delta :: (Eq i, Num a) => i -> i -> a
delta i j | i == j    = 1
          | otherwise = 0

-- rewrite rules?

data I3 = X3 | Y3 | Z3 deriving (Eq, Enum, Bounded, Ord, Show)
{-
xor :: 
xor = (/=)
eps3 i j k | i == j || j == k || i == k =  0
           | (i < j) `xor` (j < k)      =  1
           | otherwise                  = -1 
           -}
-- 3d levi civita
eps3 X3 Y3 Z3 = 1
eps3 Z3 X3 Y3 = 1
eps3 Y3 Z3 X3 = 1
eps3 X3 Z3 Y3 = -1
eps3 Z3 Y3 X3 = -1
eps3 Y3 X3 Z3 = -1
eps3 _ _ _ = 0

bsize :: forall b. BEnum b => Int
bsize = (fromEnum (maxBound @b)) - (fromEnum (minBound @b)) + 1

-- No enum instance, but there is Bounded?
instance (Enum a, BEnum b) => Enum (a,b) where
    fromEnum (x,y) = (fromEnum x) * (bsize @b) + (fromEnum y) where
    toEnum n =  (toEnum x, toEnum y) where 
                                     (x,y) = divMod n (bsize @b)
cross :: Num a => (I3 -> a) -> (I3 -> a) -> (I3 -> a)
cross v w = \k -> sum $ \(i,j) -> (eps3 i j k) * (v i) * (w j)

int dx f = dx * (sum f)
integ f = int dx f where dx = (toEnum 1) - (toEnum 0)

int_ex = integ (sin @Circle)
int_ex2 = integ (\x -> (sin @Circle (x / 2 ) ^ 2))



-- fft?
{-


-}

instance (BEnum a, Show a, Show b) => Show (a -> b) where
    show f = "Function Tabulation: " ++ (show [(a,f a) | a <- enumAll])

{-
data Tensor ind = Sum (ind -> Tensor ind) | NumOp (FreeNum (Tensor ind)) 


| FreeInd (\ind -> )

data FreeNum a = 
      Plus (FreeNum a) (FreeNum a) 
   | Times (FreeNum a) (FreeNum a) 
   | Minus (FreeNum a) (FreeNum a) 
   |  Neg (FreeNum a) 
   | FromInteger Integer 
   | Lit a 
   | Signum (FreeNum a) 
   | Abs (FreeNum a) deriving (Show, Eq) -- structural equality? Functor, Traversable, Foldable, 
instance Num (FreeNum a) where -- could remove minus
    (+) = Plus
    (-) = Minus
    (*) = Times
    negate = Neg
    fromInteger = FromInteger
    abs = Abs
    signum = Signum

-}

exd :: Num a => (Bool -> a) -> a
exd f = sum $ \i -> (f i) * (f i) 

dexd :: Num a => (Bool -> a) -> (Bool -> a)
dexd = grad exd

class Summable f where
    sum' :: (i -> f a) -> f a



