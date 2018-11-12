{-# LANGUAGE GADTs, NoImplicitPrelude, TypeFamilies,
InstanceSigs, StandaloneDeriving, UndecidableInstances,
ScopedTypeVariables, FlexibleInstances, DataKinds,
FunctionalDependencies, PolyKinds, 
TypeOperators, RankNTypes, ImpredicativeTypes, MultiParamTypeClasses,
AllowAmbiguousTypes, TypeApplications, FlexibleContexts #-}
--- 
module Main where

import Lib
import qualified Data.Map.Strict as Map
import Prelude hiding ((.), id)
import Linear.Vector
import Data.Tuple
import Data.Complex
import Linear.Epsilon (nearZero)
import Control.Category
import Control.Monad ((<=<))
import GHC.TypeNats
import Data.Proxy
-- import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.PrettyPrint.Boxes as B
-- import Control.Arrow
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
  (W fs) <*> (W xs) = W [(f x, a * b) | (f, a) <- fs, (x, b) <- xs] 
  -- the fs is a a vector of rows kind of.
    -- error "I don't feel like it"

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

trimZero = W . filter (\(k,v) -> not $ nearZero v) . runW
simplify = trimZero . collect
-- filter (not . nearZero . snd)

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

-- pretty printing would be hella nice
--deriving instance Show (FibTree a b)

instance Show (FibTree a b) where
  show = drawTree
deriving instance Eq (FibTree a b)
--deriving instance Ord (FibTree a b)
{-
r = PP.text "\\"
l = PP.text "/"
ls = PP.group $ (PP.text "  /" <> PP.line <> PP.text " /" <> PP.line <>PP.text "/")
rs = PP.group $ (PP.text "\\" <> PP.line <> PP.text " \\" <> PP.line <>PP.text "  \\")
-}

sp = B.char ' '
l = B.char '/'
r = B.char '\\'
h = B.char '-'
v = B.char '|'
t = B.char 'T'
i = B.char 'I'
rs x =  (r  B.<> sp B.<> sp) B.//
      (sp B.<> r  B.<> x) B.//
      (sp B.<> sp  B.<> r)
ls x =  (sp  B.<> sp B.<> l) B.//
      (x B.<> l  B.<> sp) B.//
      (l B.<> sp  B.<> sp)


{-
drawty = v B.// v B.// v B.// (h B.<> h B.<> h B.<> h)
drawb :: FibTree a b -> B.Box
drawb ILeaf = i
drawb TLeaf = t
drawb (TTT l r) = (drawb l) B.vcat   B.// ((drawb r)  B.<> v )
-}
-- PP.nest 2 l <> PP.line <> PP.nest 1 l <> PP.line <> l
-- text "Tau \\"
-- text "Id \\"

-- From Data.Tree

draw :: FibTree a b -> [String]
draw (ITT l r) = "I" : drawSubTrees (l,r)
draw (TIT l r) = "T" : drawSubTrees (l,r)
draw (TTT l r) = "T" : drawSubTrees (l,r)
draw (TTI l r) = "T" : drawSubTrees (l,r)
draw  ILeaf    = "I" : []
draw  TLeaf    = "T" : []

-- drawSubTrees' [] = []
drawSubTrees' t =
    "|" : shift "`- " "   " (draw t)
drawSubTrees (t,ts) =
    "|" : shift "+- " "|  " (draw t) ++ drawSubTrees' ts

shift first other = zipWith (++) (first : repeat other)

{-
draw' ILeaf = ["I"]
draw' TLeaf = ["T"]
draw' (TTT l r) =  " -" ++ ((fmap . fmap) (\s -> s ++ "  |") (draw' l)) ++ (["-+"] ++ draw' r))
-}
-- drawex = drawb (TTT (TTT TLeaf TLeaf) TLeaf)


-- | Neat 2-dimensional drawing of a tree.
drawTree :: FibTree a b -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
-- drawForest :: Forest String -> String
-- drawForest  = unlines . map drawTree

instance Ord (FibTree a b) where
  compare (ITT l r) (ITT l' r') | l < l' = LT
                                | l > l' = GT
                                | otherwise = compare r r'  
  compare (ITT _ _) _ = LT
  compare _ (ITT _ _) = GT
  compare (TTI l r) (TTI l' r') | l < l' = LT
                                | l > l' = GT
                                | otherwise = compare r r' 
  compare (TIT l r) (TIT l' r') | l < l' = LT
                                | l > l' = GT
                                | otherwise = compare r r' 
  compare (TTT l r) (TTT l' r') | l < l' = LT
                                | l > l' = GT
                                | otherwise = compare r r' 

  compare (TTI _ _) _ = LT
  compare _ (TTI _ _) = GT
  compare (TIT _ _) _ = LT
  compare _ (TIT _ _) = GT
  compare (TTT _ _) _ = LT
  compare _ (TTT _ _) = GT
  compare TLeaf TLeaf = EQ
  compare ILeaf ILeaf = EQ
  -- whoa. GHC is smart enough to realize some of these patterns can't happen
  -- because of the types. 

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
braid' :: FibTree a (l,l') -> Q (FibTree a (l',l))
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
fmove (TTI  a  (ITT b c)) = W [(TIT ( ITT  a b) c, tau)         , (TTT ( TTT  a b) c, sqrt tau)]
-- internal T
fmove (TTT  a  (TTT b c)) = W [(TIT ( ITT  a b) c, sqrt tau)  ,   (TTT ( TTT  a b) c, - tau   )]

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

fmove' (TIT ( ITT  a b) c) = W [(TTI  a  (ITT b c), tau)         , (TTT  a  (TTT b c) , sqrt tau)]
--fmoveq (TTI  a  (ITT b c)) = W [(TIT ( ITT  a b) c, recip tau)         , (TTT ( TTT  a b) c, recip $ sqrt tau)]
-- internal T
fmove' (TTT ( TTT  a b) c) = W [(TTI  a  (ITT b c), sqrt tau)  , (TTT  a  (TTT b c),   - tau  )]

--fmoveq (TTT  a  (TTT b c)) = W [(TIT ( ITT  a b) c, recip $ sqrt tau)  , (TTT ( TTT  a b) c,   - recip tau  )]

checkf :: Q (FibTree Tau ((Tau, Tau),Tau))
checkf = simplify $ fmove' (TIT (ITT TLeaf TLeaf) TLeaf) >>= fmove
checkf' = simplify $ fmove' (TTT (TTT TLeaf TLeaf) TLeaf) >>= fmove
-- checkf''' = fmove' (TIT (ITT TLeaf TLeaf) TLeaf)
-- checkf'' = fmove' (ITT (TTT TLeaf TLeaf) TLeaf) >>= fmove

-- tau**2 + tau == 1
tau :: Complex Double
tau =  ((sqrt 5) - 1) / 2 :+ 0 -- 0.618 :+ 0

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


-- TODOS


-- Categorical Interface
-- lmap, rmap vs second first.

{-
pullLeft (Tau,x) = (Tau,x)
pullLeft (Id,x) = (Id,x)
pullLeft (a,b) = fmove ((a',c),b) where (a',c) = pullLeft a  
-- ((a,b),c) = (, (b,c)) where (a',d) = pullLeft a
-}
{-
type family FMove' a where
  FMove' ((a,b),c)  = (a,(b,c))

type family PullLeft a where
  PullLeft Tau = Tau
  PullLeft Id = Id
  PullLeft (Tau,b) = (Tau,b)
  PullLeft (Id,b) =  (Id,b)
  PullLeft ((a,b),c)  = FMove' (PullLeft (a,b), c)

-- Auto F Moves. / auto braid.

pullLeft :: (PullLeft (b,c) ~ (b',c')) => FibTree a (b,c) -> Q (FibTree a (b',c'))
pullLeft x@(TTT TLeaf _) = pure x
pullLeft (TTT l@(TTT _ _ ) r) =  do 
                      l' <- pullLeft l
                      fmove' (TTT l' r)
-}
class PullLeft a b | a -> b where -- | a -> b functional dependency causes errors?
  pullLeft :: FibTree c a -> Q (FibTree c b)

instance PullLeft (Tau,c) (Tau,c) where
  pullLeft = pure

instance PullLeft (Id,c) (Id,c) where
  pullLeft = pure


instance PullLeft Tau Tau where
  pullLeft = pure

instance PullLeft Id Id where
  pullLeft = pure

instance (PullLeft (a,b) (a',b'), r ~ (a',(b',c))) => PullLeft ((a, b),c) r where
	pullLeft t = do 
		       t' <- lmap pullLeft t
		       fmove' t'
{-
instance (PullLeft a a', r ~ (a',(b,c))) => PullLeft (a, (b,c)) r where
  pullLeft t = lmap pullLeft t
-}
{-
instance (PullLeft a a', PullLeft (a',b) (a'',b''), ) => PullLeft (a,b) r where
  pullLeft = lmap pullLeft t
-}

{-  pullLeft (TTT l r) =  do 
                        l' <- pullLeft l
                        fmove' (TTT l' r)
-}

class PullRight a b | a -> b where -- | a -> b functional dependency causes errors?
  pullRight :: FibTree c a -> Q (FibTree c b)
{-
instance (PullRight a a', r ~ ((b,c),a')) => PullRight ((b,c),a) r where
  pullRight t = rmap pullRight t
-}
instance PullRight Tau Tau where
  pullRight = pure

instance PullRight Id Id where
  pullRight = pure


instance PullRight (c,Tau) (c,Tau) where
  pullRight = pure

instance PullRight (c,Id) (c,Id) where
  pullRight = pure

instance (PullRight (a,b) (a',b'), r ~ ((c,a'),b')) => PullRight (c,(a, b)) r where
  pullRight t = do 
           t' <- rmap pullRight t
           fmove t'



type family Count a where
  Count Tau = 1
  Count Id = 1
  Count (a,b) = (Count a) + (Count b)

type family LeftCount a where
	LeftCount (a,b) = Count a

-- The version without the explicit ordering supplied.
class LCA n a b c d | n a c -> b d where
  lcamap :: (forall r. FibTree r b -> Q (FibTree r c)) -> (FibTree e a) -> Q (FibTree e d)
{-
lcamapP :: LCA n a b c d => Proxy n ->  (forall r. FibTree r b -> Q (FibTree r c)) -> (FibTree e a) -> Q (FibTree e d)
lcamapP _ f x = lcamap f x
-}
instance (lc ~ (LeftCount a), 
          gte ~ (CmpNat lc n),
         LCA' n gte a b c d) => LCA n a b c d where
  lcamap f x = lcamap' @n @gte f x

class LCA' n gte a b c d | n gte a c -> b d where
  lcamap' :: (forall r. FibTree r b -> Q (FibTree r c)) -> (FibTree e a) -> Q (FibTree e d)

-- we find b at the lca and pass it back up. c gets passed all the way down, d gets computed by rebuilding out of c.
-- a drives the search.
instance (n' ~ (n - Count l), -- we're searching in the right subtree. Subtract the leaf number in the left subtree
	      lc ~ (LeftCount r), -- dip one left down to order which way we have to go next
	      gte ~ (CmpNat lc n'), -- Do we go left, right or havce we arrived in the next layer?
	      LCA' n' gte r b c d',  -- recurive call
	      d ~ (l,d') -- reconstruct total return type from recurive return type. left tree is unaffected by lcamapping
	      ) => LCA' n 'LT (l,r) b c d where
    lcamap' f x = rmap (lcamap' @n' @gte f) x

instance (lc ~ (LeftCount l),
	        gte ~ (CmpNat lc n),
          LCA' n gte l b c d',
          d ~ (d',r)
          ) => LCA' n 'GT (l,r) b c d where
    lcamap' f x = lmap (lcamap' @n @gte f) x

instance (b ~ a, d ~ c) => LCA' n 'EQ a b c d where
	lcamap' f x = f x



class LeafMap n gte a b c d | n gte a c -> b d where
  leafmap :: (forall r. FibTree r b -> Q (FibTree r c)) -> (FibTree e a) -> Q (FibTree e d)

instance (n' ~ (n - Count l), -- we're searching in the right subtree. Subtract the leaf number in the left subtree
        lc ~ (LeftCount r), -- dip one left down to order which way we have to go next
        gte ~ (CmpNat lc n'), -- Do we go left, right or havce we arrived in the next layer?
        LeafMap n' gte r b c d',  -- recurive call
        d ~ (l,d') -- reconstruct total return type from recurive return type. left tree is unaffected by lcamapping
        ) => LeafMap n 'LT (l,r) b c d where
    leafmap f x = rmap (leafmap @n' @gte f) x

instance (lc ~ (LeftCount l),
          gte ~ (CmpNat lc n),
          LeafMap n gte l b c d',
          d ~ (d',r)
          ) => LeafMap n 'GT (l,r) b c d where
    leafmap f x = lmap (leafmap @n @gte f) x

-- In the equals case, we now continue onward.
instance (lc ~ (LeftCount l),
          gte ~ (CmpNat lc n),
          LeafMap n gte l b c d',
          d ~ (d',r)
          ) => LeafMap n 'EQ (l,r) b c d where
    leafmap f x = lmap (leafmap @n @gte f) x

-- base case
instance (b ~ Tau, d ~ c) => LeafMap 1 'EQ Tau b c d where
  leafmap f x = f x

instance (b ~ Id, d ~ c) => LeafMap 1 'EQ Id b c d where
  leafmap f x = f x

-- split
-- leafmap @3 (const (TTT TLeaf TLeaf)) 
-- need one last arbitrary left or right fmove to put the two on the same stalk
-- neighbormap :: (LCA n a (l,r) (l',r') d, PullRight l (l',x), PullLeft r (y,r') ) => (FibTree.  -> Q FibTree) -> FibTree 

-- I need this to also work for (Tau, Tau)
-- (Tau, yada yada)
-- I.e. I need to do different things depending on 


-- Jesus. What a shitshow. But I don't see how to do better.
-- a is starting tree
-- b is extracted part
-- c is transformed b
-- d is recoustrcted a replacing b with c and rearranging.
class NeighborMap a b c d | a c -> b d where
  nmap :: (forall r. FibTree r b -> Q (FibTree r c)) -> FibTree e a -> Q (FibTree e d)

instance NeighborMap ((l,x),(y,r)) (x,y) c ((l,c),r) where
   nmap f x = do
              x'  <- fmove x -- (((l',x),y),r')
              x'' <- lmap fmove' x' -- ((l',(x,y)),r')
              lmap (rmap f) x''
instance NeighborMap (Tau, (y,r)) (Tau, y)  c  (c,r) where
   nmap f x = fmove x >>= lmap f
instance NeighborMap (Id, (y,r)) (Id, y) c (c,r) where
   nmap f x = fmove x >>= lmap f
instance NeighborMap ((l,x), Tau) (x,Tau) c (l,c) where
   nmap f x = fmove' x >>= rmap f
instance NeighborMap ((l,x), Id) (x,Id) c  (l,c) where
   nmap f x = fmove' x >>= rmap f
instance NeighborMap (Tau, Tau) (Tau,Tau) c  c where
   nmap f x = f x 
instance NeighborMap (Id, Id) (Id,Id) c  c where
   nmap f x = f x 
instance NeighborMap (Tau, Id) (Tau,Id) c  c where
   nmap f x = f x 
instance NeighborMap (Id, Tau) (Id,Tau) c  c where
   nmap f x = f x 


neighbormap :: forall n a b c d l l' r' x y e r z. (LCA n a b c d,
   b ~ (l,r),
   c ~ ((l',z),r'), 
   PullRight l (l',x),
   PullLeft r (y,r')) => 
   (forall r. FibTree r (x,y) -> Q (FibTree r z)) -> FibTree e a -> Q (FibTree e d)
neighbormap f z = lcamap @n @a @b @c @d (helper f) z

helper ::  (c ~ ((l',z),r'), 
   PullRight l (l',x),
   PullLeft r (y,r')) => (forall r. FibTree r (x,y) -> Q (FibTree r z)) -> FibTree e (l,r) -> Q (FibTree e c)
helper f x = do
            x' <- rootneighbor x
            lmap (rmap f) x'                          

rootneighbor :: (PullRight l (l',x), PullLeft r (y,r')) => FibTree e (l,r) -> Q (FibTree e ((l',(x,y)),r'))
rootneighbor x = do 
                x' <- lmap pullRight x
                x'' <- rmap pullLeft x' -- ((l',x),(y,r'))
                x''' <- fmove x'' -- (((l',x),y),r')
                lmap fmove' x''' -- ((l',(x,y)),r')

neighbormap' :: forall n a b c d l l' r r' x y e z b'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') b' z c) => 
   (forall r. FibTree r b' -> Q (FibTree r z)) -> FibTree e a -> Q (FibTree e d)
neighbormap' f z = lcamap @n @a @b @c @d (\x -> do
                                          x'  <- lmap pullRight x
                                          x'' <- rmap pullLeft x' 
                                          nmap f x'') z

neighbormap'' :: forall n a b' z d c l l' r r' x y e b. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') b' z c) => 
   (forall r. FibTree r b' -> Q (FibTree r z)) -> FibTree e a -> Q (FibTree e d)
neighbormap'' f z = lcamap @n @a @b @c @d (\x -> do
                                          x'  <- lmap pullRight x
                                          x'' <- rmap pullLeft x' 
                                          nmap f x'') z


t1 = neighbormap' @2 braid (TTT (TTI TLeaf ILeaf) (TTT TLeaf TLeaf)) 
t2 = neighbormap' @1 braid (TTT (TTI TLeaf ILeaf) (TTT TLeaf TLeaf)) 
t3 = neighbormap' @2 braid (TTT (TTT (TTT TLeaf TLeaf) TLeaf) (TTT TLeaf TLeaf)) 

                                      {-
neighbormap p f z = let helper (x :: FibTree _ (l,r)) = do
                                x' <- rootneighbor x
                                lmap (rmap f) x'
                    in
                    lcamap helper z
                    -}
{-  lcamap 
((\x -> do
                                x' <- rootneighbor x
                                lmap (rmap f) x'
                                ) :: (forall s. FibTree s (l,r) -> Q (FibTree s c))) z
-}

{-(\x -> do 
                            x' <- lmap pullRight x
                            x'' <- rmap pullLeft x' -- ((l',x),(y,r'))
                            x''' <- fmove x'' -- (((l',x),y),r')
                            x4 <- lmap fmove' x''' -- ((l',(x,y)),r')
                            lmap (lmap f) x4 -- ((l',z),r')
                            ) z
-}                         
{-
-- neighbormap f z = lcamap @n (\x -> do 
                            x' <- lmap pullRight x
                            x'' <- rmap pullLeft x'
                            x''' <- fmove x''
                            lmap? f x'''   ) z
-}
{-
abraid :: forall n a b c d l l' r' x y e r z b'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') (x,y) (y,x) c) => Q (FibTree e a) -> Q (FibTree e d)


   -}

abraid :: forall n a b c d l l' r r' x y e z b'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') (x,y) (y,x) c) => 
   Q (FibTree e a) -> Q (FibTree e d)
abraid x = x >>= (neighbormap' @n @a @b @c @d @l @l' @r @r' @x @y @e @(y,x) @(x,y)) braid

abraid' :: forall n a b c d l l' r r' x y e z b'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') (x,y) (y,x) c) => 
   Q (FibTree e a) -> Q (FibTree e d)
abraid' x = x >>= (neighbormap' @n @a @b @c @d @l @l' @r @r' @x @y @e @(y,x) @(x,y)) braid'



{-
abraid
  :: forall n a b c d l l' r r' x y e z b'. (LCA n a (l, r) c d, PullRight l l', PullLeft r r',
      NeighborMap (l', r') (x, y) (y, x) c) =>
     (FibTree e a) -> Q (FibTree e d)
abraid x = neighbormap' @n @a @(l,r) @c @d @l @l' @r @r' braid x
-}
{-
t4 = abraid @2 $
     abraid @4 $
     abraid @3 $
     pure (TTT (TTT (TTT TLeaf TLeaf) TLeaf) (TTT TLeaf TLeaf)) 
-}
{-
ndot' :: forall n a b c d l l' r r' x y e z b' a'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') (x,y) a' c) => 
   FibTree a' (x,y) -> FibTree e a -> Q (FibTree e d)
   -}
-- ndot' x = (neighbormap' @n @a @b @c @d @l @l' @r @r' @x @y @e @a' @(x,y)) (dot' x)

ndot :: forall n a l r c d l' r' x y a' e. (LCA n a (l, r) c d, PullRight l l', PullLeft r r',
      NeighborMap (l', r') (x, y) a' c) =>
     FibTree a' (x, y) -> FibTree e a -> Q (FibTree e d) 
ndot x = neighbormap' @n @a @(l,r) (dot' x)

-- This a b c d pattern is something like a lens. We are replacing 
-- piece of b with c which turns a into d. b is piece of a.
-- s t a b

-- quantified constraints
--  (forall b' c'. (g a b' c' d , f b' b c c') => Compose f g a b c d





-- (forall r. FibTree r b' -> Q (FibTree r z)) ->
{-
abraid' :: forall n a b c d l l' r r' x y e b'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') (x,y) (y,x) c) => 
   FibTree e a -> Q (FibTree e d)
abraid' = (neighbormap'' @n @a @(x,y) @(y,x) @d) braid
-}



{-
abraid' :: forall n a b c d l l' r r' x y e z b'. (LCA n a b c d,
   b ~ (l,r),
   -- c ~ c',
   -- c ~ ((l',z),r'), 
   PullRight l l',
   PullLeft r r',
   NeighborMap (l',r') (x,y) (y,x) c) => 
   FibTree e a -> Q (FibTree e d)
abraid' = (neighbormap' @n @a @b @c @d @l @l' @r @r' @x @y @e @(y,x) @(x,y)) braid'
-}



{-
abraid :: (LCA n a (l1, r) c d, PullRight l1 l'1, PullLeft r r',
      NeighborMap (l'1, r') (l2, l'2) (l'2, l2) c) =>
     FibTree e a -> Q (FibTree e d)
abraid = neighbormap' braid
-}
-- autobraid = neighbormap braid
-- autodot x = neighbormap (dot x)


-- Build state monad to carry along the vector.
-- should be able to do in applicative style, since data does not change structre of computation

-- auto braid
-- find least common ancestor
-- pullLeft, and pullRight
-- then braid


-- stateful 
-- Pretty Print tree





-- Operators.
newtype FibOp a b = FibOp (forall c. FibTree c a -> Q (FibTree c b))
type FibOp' c a b = FibTree c a -> Q (FibTree c b)

instance Category FibOp where
  id = FibOp pure
  (FibOp f) . (FibOp g) = FibOp (f <=< g) 
{-
instance Arrow FibOp where
  arr = error "No. No arr."
  (***) = 
-}
{-
class Monoidal k where
  (***) :: k a b -> k c d -> k (a,c) (b,d)
-}
tensor :: Num r => (b -> c -> a) -> W r b -> W r c -> W r a
tensor f (W xs) (W ys) =  W [ ( f x y , xc * yc ) | (x, xc) <- xs,  (y,yc) <- ys]

-- tensor :: Num r => (b -> c -> a) -> b -> c -> W r a
-- fg f g (TTT l r) = tensor TTT (f l) (g r) 

{-
instance Monoidal FibOp where
  (***) :: FibOp a b -> FibOp c d -> FibOp (a,c) (b,d)
  (FibOp f) *** (FibOp g) = FibOp fg where
                               -- fg ::  (a,c) (b,d)
                               fg :: FibTree e (a,c) -> Q (FibTree e (b,d))
                               fg (TTT l r) = tensor TTT (f l) (g r) 
                               -- fg (ITT l r) = FibOp $ tensor ITT (f l) (g r) 
                               -- fg (TIT l r) = FibOp $ tensor TIT (f l) (g r) 
-}
-- try to remove the issue of being in a instance. Which is kind of silly anyhow
{-
(****) :: FibOp' e a b -> FibOp' e c d -> FibOp' e (a,c) (b,d)
f **** g = fg where
                   -- fg ::  (a,c) (b,d)
                 fg :: FibTree e (a,c) -> Q (FibTree e (b,d))
                 fg (TTT l r) = tensor TTT (f l) (g r)
                 fg (ITT l r) = tensor ITT (f l) (g r)
-}
(***) :: (forall e. FibOp' e a b) -> (forall e'. FibOp' e' c d)  -> FibOp' e'' (a,c) (b,d)
(***) f g (TTT l r) = tensor TTT (f l) (g r)
(***) f g (ITT l r) = tensor ITT (f l) (g r)
(***) f g (TIT l r) = tensor TIT (f l) (g r)

first = lmap
second = rmap
-- braid
-- fmove

-- cup, cap? dot'


-- Is the problem that I'd trying to pattern match on kind of the output?
 -- densification

-- Quickcheck props

-- implement qubit abtraction

-- Did I fall into a way to make end, co-end work as einstein notation?6
-- The bounded meaning of forall and exists in the GADT context

main :: IO ()
main = someFunc



