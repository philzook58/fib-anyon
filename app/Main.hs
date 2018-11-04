{-# LANGUAGE GADTs, StandaloneDeriving, ScopedTypeVariables, FlexibleInstances, DataKinds, PolyKinds, RankNTypes, ImpredicativeTypes, MultiParamTypeClasses #-}
--- 
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
instance Semigroup (W b a) where
  (W x) <> (W y) = W (x <> y)
instance Monoid (W b a) where
  mempty = W mempty 
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

type P a = W Double a

type Q a = W (Complex Double) a

v1 :: Map.Map Bool Double 
v1 = zero

boolbase :: [Map.Map Bool Double]
boolbase = basis

e1 = Map.singleton True 1.0
e2 = Map.singleton False 1.0


star = mapW conjugate

{-
There are two types of particles


-}
data FibAnyon = Id | Tau
-- I started using DataKinds and it ended up being a problem.
data Tau
data Id
data A a 

data FibTree root leaves where
   ITT :: FibTree Tau l -> FibTree Tau l' -> FibTree Id (l,l') -- we would like to enforce that isingany can split into a b, but thisis tough
   TTI :: FibTree Tau l -> FibTree Id l' -> FibTree Tau (l,l')
   TIT :: FibTree Id l -> FibTree Tau l' -> FibTree Tau (l,l')
   TTT :: FibTree Tau l -> FibTree Tau l' -> FibTree Tau (l,l')
   -- III :: FibTree 'Id l -> FibTree 'Id l' -> FibTree 'Id (l,l') -- maybe shouldn't exist. uneccessary. Does really help for monoidal instance
   TLeaf :: FibTree Tau Tau
   ILeaf :: FibTree Id Id

deriving instance Show (FibTree a b)
deriving instance Eq (FibTree a b)
{-
instance Enum (FibTree 'Tau (A 'Tau)) where
  toEnum _ = TLeaf
  fromEnum _ = 0
-}
{-
instance Enum (FibTree 'Id 'Id) where
  toEnum _ = TLeaf
  fromEnum _ = 0
  -}
-- and no inhabitants types?
--instance Enum (FibTree d b), Enum (FibTree e c) => Enum (FibTree a (b,c))
{-
instance (Enum (FibTree 'Tau b), 
  Enum (FibTree 'Tau c), 
  Enum (FibTree ' b), 
  Enum (FibTree 'Id c)) => Enum (FibTree 'Tau (b,c))
  toEnum 0 = TTI
  toEnum 
  fromEnum 
-}

--instance Enum (FibTree a (b,c)) Enum (FibTree a (b,c))=> Enum (FibTree 'Id (b,c))

-- (FibTree a (b,c))


type TreeFun a b c d = FibTree a b -> FibTree c d
{-
lmap :: (forall a. FibTree a b -> FibTree (a) c) -> (forall e. FibTree e (b,d) -> FibTree e (c,d)) --  forall (a :: FibAnyon). 
lmap f (ITT l r) = (ITT (f l) r) 
lmap f (TTI l r) = (TTI (f l) r)
lmap f (TIT l r) = (TIT (f l) r)
lmap f (TTT l r) = (TTT (f l) r)
-}
lmap :: (forall a. FibTree a b -> Q (FibTree a c)) -> (FibTree e (b,d) -> Q (FibTree e (c,d)))
lmap f (ITT l r) = fmap (\l' -> ITT l' r) (f l)
lmap f (TTI l r) = fmap (\l' -> TTI l' r) (f l)
lmap f (TIT l r) = fmap (\l' -> TIT l' r) (f l)
lmap f (TTT l r) = fmap (\l' -> TTT l' r) (f l)
{-
rmap :: (forall a. FibTree a b -> FibTree a c) -> (forall e. FibTree e (d,b) -> FibTree e (d,c)) --  forall (a :: FibAnyon). 
rmap f (ITT l r) = ITT l (f r)
rmap f (TTI l r) = TTI l (f r)
rmap f (TIT l r) = TIT l (f r)
rmap f (TTT l r) = TTT l (f r)
-}
rmap :: (forall a. FibTree (a :: *) b -> Q (FibTree a c)) -> (FibTree e (d,b) -> Q (FibTree e (d,c)))
rmap f (ITT l r) = fmap (\r' -> ITT l r') (f r)
rmap f (TTI l r) = fmap (\r' -> TTI l r') (f r)
rmap f (TIT l r) = fmap (\r' -> TIT l r') (f r)
rmap f (TTT l r) = fmap (\r' -> TTT l r') (f r)


fibswap :: FibTree a (l,l') -> FibTree a (l',l)
fibswap (ITT l r) = (ITT r l) 
fibswap (TTI l r) = (TIT r l)
fibswap (TIT l r) = (TTI r l)
fibswap (TTT l r) = (TTT r l)

eye = 0 :+ 1

braid :: FibTree a (l,l') -> Q (FibTree a (l',l))
braid (ITT l r) = W [(ITT r l,  cis $ 2 * pi / 5)]  -- different scalar factors for trivial and non trivial fusion
braid (TTI l r) = W [(TIT r l,  1)] -- exchange with trivial means nothing
braid (TIT l r) = W [(TTI r l,  1)]
braid (TTT l r) = W [(TTT r l,  - (cis $ 4 * pi / 5))]

-- The inverse of braid
braid' = star . braid

-- property 
-- braid (braid' v) == v
-- braid' (braid v) == v

{-
fibassoc :: FibTree a ((c,d),e) -> FibTree a (c,(d,e))
fibassoc (ITT l r) = (ITT r l) 
fibassoc (TTI l r) = (TIT r l)
fibassoc (TIT l r) = (TTI r l)
fibassoc (TTT l r) = (TTT r l)
-}
-- Looks like we need more constructors for the tree
{-
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
-}

fmove :: FibTree a (c,(d,e)) -> Q (FibTree a ((c,d),e))
-- fmove (ITT  a  (TTI b c)) = pure $ ITI ( TTT  a b) c -- pure (auto (auto a b) c) -- no maybe not. The internal one isn't auto
fmove (ITT  a  (TIT b c)) = pure $ ITT ( TTI  a b) c
fmove (ITT  a  (TTT b c)) = pure $ ITT ( TTT  a b) c

fmove (TTT  a  (TTI b c)) = pure $ TTI ( TTT  a b) c
fmove (TTT  a  (TIT b c)) = pure $ TTT ( TTI  a b) c 


fmove (TIT  a  (TTI b c)) = pure $ TTI ( TIT  a b) c
-- fmove (TIT  a  (TIT b c)) = TTT ( III  a b) c
-- the nontrivial ones have all tau on the leafs and root 
-- internal I
fmove (TTI  a  (ITT b c)) = W [(TIT ( ITT  a b) c, recip tau)         , (TTT ( TTT  a b) c, recip $ sqrt tau)]
-- internal T
fmove (TTT  a  (TTT b c)) = W [(TIT ( ITT  a b) c, recip $ sqrt tau)  , (TTT ( TTT  a b) c,   - recip tau  )]

-- largely just a tranpose of the above case.
fmove' :: FibTree a ((c,d),e) -> Q (FibTree a (c,(d,e)))
fmove' (ITT ( TTI  a b) c) = pure $ (ITT  a  (TIT b c))
--fmoveq (ITT  a  (TIT b c)) = pure $ ITT ( TTI  a b) c

fmove' (ITT ( TTT  a b) c) = pure $  (ITT  a  (TTT b c))
--fmoveq (ITT  a  (TTT b c)) = pure $ 

fmove' (TTI ( TTT  a b) c) = pure $ (TTT  a  (TTI b c))
--fmoveq (TTT  a  (TTI b c)) = pure $ TTI ( TTT  a b) c

fmove' (TTT ( TTI  a b) c ) = pure $ TTT  a  (TIT b c)
--fmoveq (TTT  a  (TIT b c)) = pure $ TTT ( TTI  a b) c 

fmove' (TTI ( TIT  a b) c) = pure $ TIT  a  (TTI b c)
--fmoveq (TIT  a  (TTI b c)) = pure $ TTI ( TIT  a b) c
-- fmove (TIT  a  (TIT b c)) = TTT ( III  a b) c
-- the nontrivial ones have all tau on the leafs and root 

-- internal I

fmove' (TIT ( ITT  a b) c) = W [(TTI  a  (ITT b c), recip tau)         , (TTT  a  (TTT b c) , recip $ sqrt tau)]
--fmoveq (TTI  a  (ITT b c)) = W [(TIT ( ITT  a b) c, recip tau)         , (TTT ( TTT  a b) c, recip $ sqrt tau)]
-- internal T
fmove' (TTT ( TTT  a b) c) = W [(TTI  a  (ITT b c), recip $ sqrt tau)  , (TTT  a  (TTT b c),   - recip tau  )]

--fmoveq (TTT  a  (TTT b c)) = W [(TIT ( ITT  a b) c, recip $ sqrt tau)  , (TTT ( TTT  a b) c,   - recip tau  )]



-- tau**2 + tau == 1
tau :: Complex Double
tau =  (sqrt 5 - 1) / 2 :+ 0 -- 0.618 :+ 0

--test1 = lmap fibswap
-- test2 = lmap $ lmap fibswap
-- test3 = rmap fmove

type Vec1 b r = [(b, r)]

smul :: Num r => r -> Vec1 b r -> Vec1 b r
smul s = map (\(b,s') -> (b, s' * s))

linapply :: Num r => (a -> Vec1 b r) -> (Vec1 a r -> Vec1 b r) -- bind
linapply f [] = []
linapply f ((b,s) : xs) = smul s (f b) ++ linapply f xs


dTTT :: FibTree a (Tau, Tau) -> Q (FibTree a Tau)
dTTT (TTT _ _) = pure TLeaf
dTTT _ = mempty
{-
dITT :: FibTree a (Tau, Tau) -> Q (FibTree a Id)
dTTT (TTT _ _) = pure TLeaf
dTTT _ = mempty
-}
dTTI :: FibTree a (Tau, Id) -> Q (FibTree a Tau) -- A dual tree of type  'tau (tau,'tau)
dTTI (TTI _ _) =  pure TLeaf
dTTI _ = mempty

dTTT'''' = dot' (TTT TLeaf TLeaf)
-- I could do this as a typeclass. NOPE. don't need that garbage

-- I think that incompatible FibTree a' a is empty by default vs unconstructible
-- which is better?
dot' :: FibTree a (b, c) -> FibTree a' (b, c) -> Q (FibTree a' a)
dot' x@(TTI _ _) y@(TTI _ _) | x == y = pure TLeaf
                             | otherwise = mempty
dot' x@(TTT _ _) y@(TTT _ _) | x == y = pure TLeaf
                             | otherwise = mempty
dot' (ITT _ _) (ITT _ _) = pure ILeaf
dot' _ _ = mempty 

test5 = (pure $ TTT TLeaf TLeaf) >>= dTTT
test6 = (pure $ ITT TLeaf TLeaf) >>= dTTT
test7 v = do y <- rmap dTTI v  -- wait. What the hell am I doing?
             lmap dTTT y
test8 v = rmap dTTT v  >>= dTTT
-- Building from the leaves up...?
-- you  hve to apply the deeper stuff before the upper stuff.
--- So this is a dual tree with TTT in the right branch. Then TTT above that.
-- In total (T,(T,T)) -> Tleaf.
-- maybe we need to cps it.
dTTT' :: (Q (FibTree a Tau) -> (Complex Double)) -> (FibTree a (Tau, Tau)) -> Complex Double
dTTT' f = f . dTTT

dTTT'' :: (FibTree a Tau -> Complex Double) -> FibTree a (Tau, Tau) -> Complex Double
dTTT'' f = undefined

-- sort of similar to bind in a way. lift function to a Q function
-- (Q FibTree a ) -> (FibTree a -> Q FibTree b)  -> Q FibTree b
dot :: (FibTree a b -> Complex Double) -> Q (FibTree a b) -> Complex Double
dot f (W v) = sum $ map (\(e,a) -> a * (f e)) v

dTTT''' = dot . dTTT''


pentagon1 ::  FibTree a (e,(d,(c,b))) -> Q (FibTree a (((e,d),c),b))
pentagon1 v =  do 
                 v1 <- fmove v
                 fmove v1

-- type annotations not necessary. For clarity.
pentagon2 :: FibTree a (b,(c,(d,e))) -> Q (FibTree a (((b,c),d),e))
pentagon2 v = do
                v1 :: FibTree a (b,((c,d),e)) <- rmap fmove v
                v2 :: FibTree a ((b,(c,d)),e) <- fmove v1
                lmap fmove v2

hexagon1 :: FibTree a (b,(c,d)) -> Q (FibTree a ((d,b),c))
hexagon1 v = do
             v1 :: FibTree a ((b,c),d) <- fmove v
             v2 :: FibTree a (d,(b,c)) <- braid v1
             fmove v2  

hexagon2 :: FibTree a (b,(c,d)) -> Q (FibTree a ((d,b),c))
hexagon2 v = do
             v1 :: FibTree a (b,(d,c)) <- rmap braid v
             v2 :: FibTree a ((b,d),c) <- fmove v1
             lmap braid v2  

-- hexagon2 :: FibTree a (b,(c,d)) -> Q (FibTree a (c,(d,b)))

-- quickcheck forall v, pentagon1 v == pentagon2 v


-- test9 = test8 $ pure $ TTT (TLeaf) ()
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

-- I can't make a fst. It extracts a skolemized object.
-- So I do need to do complete pattern matching? No. I CAN do this... ? Not always clear I'd want to?
fibfst :: AutoNode a e _ => FibTree a (b,c) -> FibTree e b



-}
class TProd a b c where
  tprod :: Vec1 (FibTree a l) r -> Vec1 (FibTree b l') r -> Vec1 (FibTree c (l,l')) r 

-- tprod :: AutoNode a b c => Vec1 (FibTree a l) r -> Vec1 (FibTree b l') r -> Vec1 (FibTree c (l,l')) r 
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
