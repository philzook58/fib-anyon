{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeInType, RankNTypes, ImpredicativeTypes, MultiParamTypeClasses #-}

module Main where

import Lib
import qualified Data.Map.Strict as Map
import Linear.Vector
import Data.Tuple
import Data.Complex
{-
type MapVec r k = Map.Map k r

vadd :: (Ord k, Num r) => MapVec r k -> MapVec r k -> MapVec r k
vadd = Map.unionWith (+)

smul :: (Ord k, Num r) => k -> MapVec r k -> MapVec r k
smul

-- kmett alread did this stuff 
-}

-- http://blog.sigfpe.com/2007/03/monads-vector-spaces-and-quantum.html


data W b a = W { runW :: [(a,b)] } deriving (Eq,Show,Ord)
mapW f (W l) = W $ map (\(a,b) -> (a,f b)) l
instance Functor (W b) where
    fmap f (W a) = W $ map (\(a,p) -> (f a,p)) a

instance Num b => Applicative (W b) where
	pure x = W [(x,1)]
	x <*> y = error "I don't feel like it"

instance Num b => Monad (W b) where
   return x = W [(x,1)]
   l >>= f = W $ concatMap (\(W d,p) -> map (\(x,q)->(x,p*q)) d) (runW $ fmap f l)

a .* b = mapW (a*) b

instance (Eq a,Show a,Num b) => Num (W b a) where
     W a + W b = W $ (a ++ b)
     a - b = a + (-1) .* b
     _ * _ = error "Num is annoying"
     abs _ = error "Num is annoying"
     signum _ = error "Num is annoying"
     fromInteger a = if a==0 then W [] else error "fromInteger can only take zero argument"

collect :: (Ord a,Num b) => W b a -> W b a
collect = W . Map.toList . Map.fromListWith (+) . runW

type P a = W Float a

type Q a = W (Complex Float) a

v1 :: Map.Map Bool Double 
v1 = zero

boolbase :: [Map.Map Bool Double]
boolbase = basis

e1 = Map.singleton True 1.0
e2 = Map.singleton False 1.0

{-
There are two types of particles


-}
data FibAnyon = Id | Tau

data FibTree (root :: a) (leaves :: b) where
   ITT :: FibTree 'Tau l -> FibTree 'Tau l' -> FibTree 'Id (l,l') -- we would like to enforce that isingany can split into a b, but thisis tough
   TTI :: FibTree 'Tau l -> FibTree 'Id l' -> FibTree 'Tau (l,l')
   TIT :: FibTree 'Id l -> FibTree 'Tau l' -> FibTree 'Tau (l,l')
   TTT :: FibTree 'Tau l -> FibTree 'Tau l' -> FibTree 'Tau (l,l')
   -- III :: FibTree 'Id l -> FibTree 'Id l' -> FibTree 'Id (l,l') -- maybe shouldn't exist. uneccessary. Does really help for monoidal instance
   TLeaf :: FibTree 'Tau 'Tau



type TreeFun a b c d = FibTree a b -> FibTree c d

lmap :: (forall (a :: FibAnyon). FibTree (a :: FibAnyon) b -> FibTree (a :: FibAnyon) c) -> (forall (e :: FibAnyon). FibTree e (b,d) -> FibTree e (c,d)) --  forall (a :: FibAnyon). 
lmap f (ITT l r) = (ITT (f l) r) 
lmap f (TTI l r) = (TTI (f l) r)
lmap f (TIT l r) = (TIT (f l) r)
lmap f (TTT l r) = (TTT (f l) r)

linlmap :: (forall (a :: FibAnyon). FibTree a b -> Q (FibTree a c)) -> (FibTree e (b,d) -> Q (FibTree e (c,d)))
linlmap f (ITT l r) = fmap (\l' -> ITT l' r) (f l)
linlmap f (TTI l r) = fmap (\l' -> TTI l' r) (f l)
linlmap f (TIT l r) = fmap (\l' -> TIT l' r) (f l)
linlmap f (TTT l r) = fmap (\l' -> TTT l' r) (f l)

rmap :: (forall (a :: FibAnyon). FibTree (a :: FibAnyon) b -> FibTree (a :: FibAnyon) c) -> (forall (e :: FibAnyon). FibTree e (d,b) -> FibTree e (d,c)) --  forall (a :: FibAnyon). 
rmap f (ITT l r) = ITT l (f r)
rmap f (TTI l r) = TTI l (f r)
rmap f (TIT l r) = TIT l (f r)
rmap f (TTT l r) = TTT l (f r)

fibswap :: FibTree a (l,l') -> FibTree a (l',l)
fibswap (ITT l r) = (ITT r l) 
fibswap (TTI l r) = (TIT r l)
fibswap (TIT l r) = (TTI r l)
fibswap (TTT l r) = (TTT r l)

{-
fibassoc :: FibTree a ((c,d),e) -> FibTree a (c,(d,e))
fibassoc (ITT l r) = (ITT r l) 
fibassoc (TTI l r) = (TIT r l)
fibassoc (TIT l r) = (TTI r l)
fibassoc (TTT l r) = (TTT r l)
-}
-- Looks like we need more constructors for the tree
fmove :: FibTree a (c,(d,e)) -> FibTree a ((c,d),e)
-- fmove (ITT  a  (TTI b c)) = ITI ( TTT  a b) c
fmove (ITT  a  (TIT b c)) = ITT ( TTI  a b) c
fmove (ITT  a  (TTT b c)) = ITT ( TTT  a b) c

fmove (TTT  a  (TTI b c)) = TTI ( TTT  a b) c
fmove (TTT  a  (TIT b c)) = TTT ( TTI  a b) c 
fmove (TTT  a  (TTT b c)) = TTT ( TTT  a b) c

fmove (TIT  a  (TTI b c)) = TTI ( TIT  a b) c
-- fmove (TIT  a  (TIT b c)) = TTT ( III  a b) c 
fmove (TIT  a  (TTT b c)) = TTT ( TIT  a b) c


test1 = lmap fibswap
test2 = lmap $ lmap fibswap
test3 = rmap fmove

type Vec1 b r = [(b, r)]

smul :: Num r => r -> Vec1 b r -> Vec1 b r
smul s = map (\(b,s') -> (b, s' * s))

linapply :: Num r => (a -> Vec1 b r) -> (Vec1 a r -> Vec1 b r) -- bind
linapply f [] = []
linapply f ((b,s) : xs) = smul s (f b) ++ linapply f xs


-- Vec1 (FibTree 'Tau l) r -> Vec1 (FibTree 'Tau l') r -> Vec1 (FibTree 'Id (l,l')) r

-- linearized tensor products
--linITT :: Num r => Vec1 (FibTree 'Tau l) r -> Vec1 (FibTree 'Tau l') r -> Vec1 (FibTree 'Id (l,l')) r -- we would like to enforce that isingany can split into a b, but thisis tough
--linITT = [ (ITT l r, s * s')  |  (l, s) <- v1 , (r, s') <- v2   ]

-- class AutoNode c a b where
      -- prod ::  FibTree a l -> FibTree b l' -> FibTree  c (l,l')
{-
 instance AutoProd 'Tau 'Tau 'Tau
    prod = TTT
 instance AutoProd 'Id 'Tau 'Tau
    prod = ITT
-- etc.

fibfst :: AutoNode a e _ => FibTree a (b,c) -> FibTree e b


-}
class TProd a b c where
	tprod :: Vec1 (FibTree a l) r -> Vec1 (FibTree b l') r -> Vec1 (FibTree c (l,l')) r 


--class Index tree n  -- reference leaf by number?

-- class AutoFMove tree n 

{-
linTTI :: FibTree 'Tau l -> FibTree 'Id l' -> FibTree 'Tau (l,l')
linTIT :: FibTree 'Id l -> FibTree 'Tau l' -> FibTree 'Tau (l,l')
libTTT :: FibTree 'Tau l -> FibTree 'Tau l' -> FibTree 'Tau (l,l')
-}
(~*) :: r -> b -> (b,r) -- **, `smul`
(~*) = flip (,) 

(~+) :: a -> [a] -> [a] -- ++ instead?
(~+) = (:)

test4 = [1 ~* 'a', 2 ~* 'b'] -- eh. What's the point.
-- test3 = (lmap . lmap) fibswap
--lmap (ITT l r)
-- ^+^
-- zero
-- 

main :: IO ()
main = someFunc
