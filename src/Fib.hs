{-# LANGUAGE GADTs,  TypeFamilies,
 StandaloneDeriving, UndecidableInstances,
ScopedTypeVariables, FlexibleInstances, DataKinds,
FunctionalDependencies, PolyKinds, 
TypeOperators, RankNTypes,  MultiParamTypeClasses,
 TypeApplications, FlexibleContexts, AllowAmbiguousTypes #-}
-- AllowAmbiguousTypes, ImpredicativeTypes, InstanceSigs, NoImplicitPrelude,

module Fib where

import Vec
import Data.Complex
import Control.Monad  ((<=<))
import GHC.TypeNats
import Data.Proxy


-- data FibAnyon = Id | Tau
-- I started using DataKinds and it ended up being a problem.


data Tau
--data Id
type Id = ()
data FibTree root leaves where
   TTT :: FibTree Tau l -> FibTree Tau r -> FibTree Tau (l,r)
   ITT :: FibTree Tau l -> FibTree Tau r -> FibTree Id (l,r) 
   TIT :: FibTree Id l -> FibTree Tau r -> FibTree Tau (l,r)
   TTI :: FibTree Tau l -> FibTree Id r -> FibTree Tau (l,r)
   III :: FibTree Id l -> FibTree Id r -> FibTree Id (l,r)
   TLeaf :: FibTree Tau Tau
   ILeaf :: FibTree Id Id

-- pretty printing would be hella nice
deriving instance Show (FibTree a b)
deriving instance Eq (FibTree a b)

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
    compare (III l r) (III l' r') | l < l' = LT
                                  | l > l' = GT
                                  | otherwise = compare r r' 
  
    compare (TTI _ _) _ = LT
    compare _ (TTI _ _) = GT
    compare (TIT _ _) _ = LT
    compare _ (TIT _ _) = GT
    compare (TTT _ _) _ = LT
    compare _ (TTT _ _) = GT
    compare (III _ _) _ = LT
    compare _ (III _ _) = GT
    compare TLeaf TLeaf = EQ
    compare ILeaf ILeaf = EQ

lmap :: (forall a. FibTree a b -> Q (FibTree a c)) -> (FibTree e (b,d) -> Q (FibTree e (c,d)))
lmap f (ITT l r) = fmap (\l' -> ITT l' r) (f l)
lmap f (TTI l r) = fmap (\l' -> TTI l' r) (f l)
lmap f (TIT l r) = fmap (\l' -> TIT l' r) (f l)
lmap f (TTT l r) = fmap (\l' -> TTT l' r) (f l)
lmap f (III l r) = fmap (\l' -> III l' r) (f l)

rmap :: (forall a. FibTree a b -> Q (FibTree a c)) -> (FibTree e (d,b) -> Q (FibTree e (d,c)))
rmap f (ITT l r) = fmap (\r' -> ITT l r') (f r)
rmap f (TTI l r) = fmap (\r' -> TTI l r') (f r)
rmap f (TIT l r) = fmap (\r' -> TIT l r') (f r)
rmap f (TTT l r) = fmap (\r' -> TTT l r') (f r)
rmap f (III l r) = fmap (\r' -> III l r') (f r)

braid :: FibTree a (l,r) -> Q (FibTree a (r,l))
braid (ITT l r) = W [(ITT r l,  cis $ 4 * pi / 5)]  -- different scalar factors for trivial and non trivial fusion
braid (TTT l r) = W [(TTT r l,  (cis $ - 3 * pi / 5))]
braid (TTI l r) = pure $ TIT r l-- exchange with trivial means nothing
braid (TIT l r) = pure $ TTI r l
braid (III l r) = pure $ III r l

-- The inverse of braid
braid' :: FibTree a (l,r) -> Q (FibTree a (r,l))
braid' = star . braid

tau :: Complex Double
tau =  ((sqrt 5) - 1) / 2 :+ 0 -- 0.618 :+ 0

fmove :: FibTree a (c,(d,e)) -> Q (FibTree a ((c,d),e))
-- fmove (ITT  a  (TTI b c)) = pure $ ITI ( TTT  a b) c -- pure (auto (auto a b) c) -- no maybe not. The internal one isn't auto
fmove (ITT  a  (TIT b c)) = pure $ ITT ( TTI  a b) c
fmove (ITT  a  (TTT b c)) = pure $ ITT ( TTT  a b) c
fmove (ITT  a  (TTI b c)) = pure $ III ( ITT  a b) c



fmove (TIT  a  (TTT b c)) = pure $ TTT ( TIT  a b) c
fmove (TIT  a  (TTI b c)) = pure $ TTI ( TIT  a b) c
fmove (TIT  a  (TIT b c)) = pure $ TIT ( III  a b) c

-- fmove (TIT  a  (TIT b c)) = TTT ( III  a b) c
-- the nontrivial ones have all tau on the leafs and root 
-- internal I
fmove (TTI  a  (III b c)) = pure $ TTI ( TTI  a b) c
fmove (TTI  a  (ITT b c)) = W [(TIT ( ITT  a b) c, tau)         , (TTT ( TTT  a b) c, sqrt tau)]
-- internal T
fmove (TTT  a  (TTT b c)) = W [(TIT ( ITT  a b) c, sqrt tau)  ,   (TTT ( TTT  a b) c, - tau   )]
fmove (TTT  a  (TTI b c)) = pure $ TTI ( TTT  a b) c
fmove (TTT  a  (TIT b c)) = pure $ TTT ( TTI  a b) c 

fmove (III  a  (ITT b c)) = pure $ ITT ( TIT  a b) c
fmove (III  a  (III b c)) = pure $ III ( III  a b) c


-- largely just a tranpose of the above case.
fmove' :: FibTree a ((c,d),e) -> Q (FibTree a (c,(d,e)))
fmove' (ITT ( TTI  a b) c) = pure $ (ITT  a  (TIT b c))
fmove' (ITT ( TTT  a b) c) = pure $  (ITT  a  (TTT b c))
fmove' (ITT ( TIT  a b) c) = pure $  (III  a  (ITT b c))

--fmoveq (ITT  a  (TTT b c)) = pure $ 

fmove' (TTI ( TTT  a b) c) = pure $ (TTT  a  (TTI b c))
fmove' (TTI ( TTI  a b) c) = pure $ (TTI  a  (III b c))
fmove' (TTI ( TIT  a b) c) = pure $ TIT  a  (TTI b c)
--fmoveq (TTT  a  (TTI b c)) = pure $ TTI ( TTT  a b) c



--fmoveq (TTT  a  (TIT b c)) = pure $ TTT ( TTI  a b) c 
fmove' (TIT ( ITT  a b) c) = W [(TTI  a  (ITT b c), tau)         , (TTT  a  (TTT b c) , sqrt tau)]
fmove' (TIT ( III  a b) c ) = pure $ TIT  a  (TIT b c)


fmove' (TTT ( TTI  a b) c ) = pure $ TTT  a  (TIT b c)
fmove' (TTT ( TIT  a b) c ) = pure $ TIT  a  (TTT b c)
fmove' (TTT ( TTT  a b) c) = W [(TTI  a  (ITT b c), sqrt tau)  , (TTT  a  (TTT b c),   - tau  )]

fmove' (III ( III  a b) c ) = pure $ III  a  (III b c)
fmove' (III ( ITT  a b) c ) = pure $ ITT  a  (TTI b c)


rightUnit :: FibTree e (a,Id) -> Q (FibTree e a)
rightUnit (TTI t _) = pure t
rightUnit (III t _) = pure t

rightUnit' :: FibTree e a -> Q (FibTree e (a,Id))
rightUnit' t@(TTT _ _) = pure (TTI  t ILeaf)
rightUnit' t@(TTI _ _) = pure (TTI  t ILeaf)
rightUnit' t@(TIT _ _) = pure (TTI  t ILeaf)
rightUnit' t@(III _ _) = pure (III  t ILeaf)
rightUnit' t@(ITT _ _) = pure (III  t ILeaf)
rightUnit' t@(ILeaf) = pure (III t ILeaf)
rightUnit' t@(TLeaf) = pure (TTI t ILeaf)

leftUnit :: FibTree e (Id,a) -> Q (FibTree e a)
leftUnit = rightUnit <=< braid

-- braid vs braid' doesn't matter, but it has a nice symmettry.
leftUnit' :: FibTree e a -> Q (FibTree e (Id,a))
leftUnit' = braid' <=< rightUnit' 


dot :: FibTree a (b, c) -> FibTree a' (b, c) -> Q (FibTree a' a)
dot x@(TTI _ _) y@(TTI _ _) | x == y = pure TLeaf
                             | otherwise = mempty
dot x@(TIT _ _) y@(TIT _ _) | x == y = pure TLeaf
                             | otherwise = mempty
dot x@(TTT _ _) y@(TTT _ _) | x == y = pure TLeaf
                            | otherwise = mempty
dot x@(III _ _) y@(III _ _) | x == y = pure ILeaf
                            | otherwise = mempty
dot x@(ITT _ _) y@(ITT _ _) | x == y = pure ILeaf
                            | otherwise = mempty
dot _ _ = mempty 




-- Resulting type depends on input
-- I think a typefamily type computation might be necessary? 
-- pullLeft and pullRight might not need a type class.
-- pullLeft (TTT l r) = fmove (TTT pl r)
--                         where pl = pullLeft l
{-
type family Assoc a where
   Assoc ((a,b),c) = (a,(b,c))

type family PullLeft a where
   PullLeft ((a,b),c) = Assoc (PullLeft (a, b), c)
   PullLeft a = a 


type Test1 = PullLeft (((Int,Int),Int),Int)
pullLeft :: forall a b c. (c ~ PullLeft b) => FibTree a b -> Q (FibTree a c)
pullLeft TLeaf = pure Tleaf
pullLeft ILeaf = pure ILeaf
pullLeft t =  do 
            t' <- lmap pullLeft t
            fmove' t'


pullLeft t@(ITT l r) = helper t
pullLeft t@(TTT l r) = helper t
pullLeft t@(TTI l r) = helper t
pullLeft t@(TIT l r) = helper t
pullLeft t@(III l r) = helper t where
                       helper :: (d ~ PullLeft (b,c)) => FibTree a (b,c) -> Q (FibTree a d)
                       helper t = do 
                                t' <- lmap pullLeft t
                                fmove' t'

-}


{-
Spiritually this function is this. Can't write it this way unforturnately
-- There are typing problems
-- this isn't even right
-- we need to pattern match on ((),)
pullLeft TLeaf = pure Tleaf
pullLeft ILeaf = pure ILeaf
pullLeft t =  do 
                t' <- lmap pullLeft t
                fmove' t'

-- but actually a two level tree desctruct
--treeDestruct()

-}
{-
type family Assoc a where
    Assoc ((a,b),c) = (a,(b,c))
 
 type family PullLeft' a where
    PullLeft' ((a,b),c) = Assoc (PullLeft' (a, b), c)
    PullLeft' a = a 
    -}
-- could seperate the typeclass so that I'm using type families instead.

class PullLeftLeaf a b | a -> b where 
  pullLeftLeaf :: FibTree c a -> Q (FibTree c b)
instance PullLeftLeaf (Tau,c) (Tau,c) where
  pullLeftLeaf = pure
instance PullLeftLeaf (Id,c) (Id,c) where
  pullLeftLeaf = pure
instance PullLeftLeaf Tau Tau where
  pullLeftLeaf = pure
instance PullLeftLeaf Id Id where
  pullLeftLeaf = pure
instance (PullLeftLeaf (a,b) (a',b'), 
          r ~ (a',(b',c))) => PullLeftLeaf ((a, b),c) r where
  pullLeftLeaf t = do 
           t' <- lmap pullLeftLeaf t
           fmove' t'


class PullRightLeaf a b | a -> b where -- | a -> b functional dependency causes errors?
    pullRightLeaf :: FibTree c a -> Q (FibTree c b)
{-
instance (PullRightLeaf a a', r ~ ((b,c),a')) => PullRightLeaf ((b,c),a) r where
pullRightLeaf t = rmap pullRightLeaf t
-}
instance PullRightLeaf Tau Tau where
    pullRightLeaf = pure

instance PullRightLeaf Id Id where
    pullRightLeaf = pure


instance PullRightLeaf (c,Tau) (c,Tau) where
    pullRightLeaf = pure

instance PullRightLeaf (c,Id) (c,Id) where
    pullRightLeaf = pure

instance (PullRightLeaf (a,b) (a',b'), r ~ ((c,a'),b')) => PullRightLeaf (c,(a, b)) r where
    pullRightLeaf t = do 
        t' <- rmap pullRightLeaf t
        fmove t'

class RightAssoc a b | a -> b where 
  rightAssoc :: FibTree c a -> Q (FibTree c b)
instance RightAssoc Tau Tau where
  rightAssoc = pure
instance RightAssoc Id Id where
  rightAssoc = pure
instance (PullLeftLeaf (a,b) (a',b'),
          RightAssoc b' b'',
          r ~ (a', b'')) => RightAssoc (a,b) r where
  rightAssoc t = do 
           t' <- pullLeftLeaf t
           rmap rightAssoc t'
-- usually you'll want to force not the root, but the leaves to be some type
-- hence the ordering b c d a for type application
bmove :: forall b c d a. FibTree a (b,(c,d)) -> Q (FibTree a (c,(b,d)))
bmove t = do
           t'  :: FibTree a ((b,c),d) <- fmove t
           t'' :: FibTree a ((c,b),d) <-  lmap braid t'
           fmove' t'' 
bmove' :: forall b c d a. FibTree a (b,(c,d)) -> Q (FibTree a (c,(b,d)))
bmove' = fmove' <=< (lmap braid') <=< fmove

-- Therei s a general pattern for digging into
-- n and a tell us b, the subpiece of a
-- c and a and n tell use what a looks like with b replaced = d
--data Nat = Z | S Nat

rmapN :: forall n gte s t a b e. (RMapN n gte s t a b, gte ~ (CmpNat n 0)) => (forall r. FibTree r a -> Q (FibTree r b)) -> (FibTree e s) -> Q (FibTree e t)
rmapN f t = rmapN' @n @gte f t

class RMapN n gte s t a b | n gte s b -> a t where
    rmapN' :: (forall r. FibTree r a -> Q (FibTree r b)) -> (FibTree e s) -> Q (FibTree e t)
instance (a ~ s, b ~ t) => RMapN 0 'EQ s t a b where
    rmapN' f t = f t -- rmapN = id
instance (RMapN (n-1) gte r r' a b, 
               gte ~ (CmpNat (n-1) 0),
               t ~ (l,r')) => RMapN n 'GT (l,r) t a b where
    rmapN' f t = rmap (rmapN @(n-1) f) t
{-
bmoveN :: forall n a b c d e f. RMapN n a (b, (c, d)) (c, (b, d)) e => FibTree f a -> Q (FibTree f e)
bmoveN t = rmapN @n (bmove @b @c @d) t

-- dotN :: forall n a b c d e f. dot :: FibTree a (b, c) -> FibTree a' (b, c) -> Q (FibTree a' a)
fuseN :: forall n a b c d e f g. RMapN n f (b,(c,d)) (a,d) g => FibTree a (b, c) -> FibTree e f -> Q (FibTree e g)
fuseN q t = rmapN @n f t where
    f :: forall r. FibTree r (b,(c,d)) -> Q (FibTree r (a,d))
    f t' = do 
            t'' <- fmove t'
            lmap (dot q) t''
-}

type family Count a where
    Count Tau = 1
    Count Id = 1
    Count (a,b) = (Count a) + (Count b)
    
type family LeftCount a where
    LeftCount (a,b) = Count a

lcamap ::  forall n s t a b e gte .
           (gte ~ CmpNat (LeftCount s) n,
           LCAMap n gte s t a b)
           => (forall r. FibTree r a -> Q (FibTree r b)) -> (FibTree e s) -> Q (FibTree e t)
lcamap f t = lcamap' @n @gte f t

class LCAMap n gte s t a b | n gte s b -> t a where
    lcamap' :: (forall r. FibTree r a -> Q (FibTree r b)) -> (FibTree e s) -> Q (FibTree e t)
    

instance (n' ~ (n - Count l), -- We're searching in the right subtree. Subtract the leaf number in the left subtree
            lc ~ (LeftCount r), -- dip one level down to order which way we have to go next
            gte ~ (CmpNat lc n'), -- Do we go left, right or have we arrived in the next layer?
            LCAMap n' gte r r' a b,  -- recursive call
            t ~ (l,r') -- reconstruct total return type from recursive return type. Left tree is unaffected by lcamapping
            ) => LCAMap n 'LT (l,r) t a b where
        lcamap' f x = rmap (lcamap @n' f) x
    
instance (lc ~ (LeftCount l),
            gte ~ (CmpNat lc n),
            LCAMap n gte l l' a b,
            t ~ (l',r)
            ) => LCAMap n 'GT (l,r) t a b where
        lcamap' f x = lmap (lcamap @n f) x
    
instance (t ~ b, a ~ s) => LCAMap n 'EQ s t a b where -- Base case
    lcamap' f x = f x



class Twiddle s t a b | s b -> t a where
    twiddle :: (forall r. FibTree r a -> Q (FibTree r b)) -> FibTree e s -> Q (FibTree e t)
    
instance Twiddle ((l,x),(y,r)) ((l,c),r) (x,y) c where
    twiddle f x = do
            x'  <- fmove x -- (((l',x),y),r')
            x'' <- lmap fmove' x' -- ((l',(x,y)),r')
            lmap (rmap f) x''
instance Twiddle (Tau, (y,r)) (c,r) (Tau, y) c where
    twiddle f x = fmove x >>= lmap f
instance Twiddle (Id, (y,r)) (c,r)  (Id, y) c where
    twiddle f x = fmove x >>= lmap f
instance Twiddle ((l,x), Tau) (l,c) (x,Tau) c where
    twiddle f x = fmove' x >>= rmap f
instance Twiddle ((l,x), Id) (l,c) (x,Id) c where
    twiddle f x = fmove' x >>= rmap f
instance Twiddle (Tau, Tau) c (Tau,Tau) c where
    twiddle f x = f x 
instance Twiddle (Id, Id) c (Id,Id)  c where
    twiddle f x = f x 
instance Twiddle (Tau, Id) c (Tau,Id)  c where
    twiddle f x = f x 
instance Twiddle (Id, Tau) c (Id,Tau) c where
    twiddle f x = f x 

nmap :: forall (n :: Nat) s t a b a' b' l l' r r' e gte.
    (gte ~ CmpNat (LeftCount s) n,
    LCAMap n gte s t a' b',
    a' ~ (l,r),
    PullRightLeaf l l',
    PullLeftLeaf r r',
    Twiddle (l',r') b' a b) => 
    (forall r. FibTree r a -> Q (FibTree r b)) -> FibTree e s -> Q (FibTree e t)
nmap f z = lcamap @n @s @t @a' @b' (\x -> do
                                           x'  <- lmap pullRightLeaf x
                                           x'' <- rmap pullLeftLeaf x' 
                                           twiddle f x'') z

t1 = nmap @2 braid (TTT (TTI TLeaf ILeaf) (TTT TLeaf TLeaf)) 
t5 = nmap @2 pure (TTT (TTI TLeaf ILeaf) (TTT TLeaf TLeaf)) >>= nmap @3 pure
t2 = nmap @1 braid (TTT (TTI TLeaf ILeaf) (TTT TLeaf TLeaf)) 
t4 = nmap @1 braid (TTT TLeaf (TTT TLeaf TLeaf)) 
t3 = nmap @2 braid (TTT (TTT (TTT TLeaf TLeaf) TLeaf) (TTT TLeaf TLeaf)) 
t6 = rightAssoc (TTT (TTT (TTT TLeaf TLeaf) TLeaf) (TTT TLeaf TLeaf)) 
t7 = t6 >>= bmove
t8 = t6 >>= rmapN @0 bmove
-- For the category, we probably don't want the association structure.
-- '[Tau, Id, Tau, Id] typelevel list of particles
-- 

ttt = TTT TLeaf TLeaf
example = starttree >>=
        nmap @1 braid >>=
        nmap @2 braid >>=
        nmap @1 (dot ttt) >>=
        nmap @1 braid' >>=
        nmap @2 (dot ttt) >>=
        nmap @1 (dot ttt) where
        starttree = pure (TTT (TTT TLeaf
                              (TTT TLeaf 
                                   TLeaf))
                          TLeaf
                         )
-- would be nice to use an unknot
-- example2 = 
-- I should use neighbormap for this.
-- maybe I should call f fuse and make it global?

-- bmoveN t = rmapN @n (bmove :: forall r. FibTree r (b,(c,d)) -> Q (FibTree r (c,(b,d)))) t
-- bmoveN t = rmapN @n @a @(b, (c, d)) @(c, (b, d)) @e bmove t


{-
bmoveN :: forall n a b c d e f. RMapN n a (b, (c, d)) (c, (b, d)) e => Proxy n -> FibTree f a -> Q (FibTree f e)
bmoveN p t = rmapN p (bmove :: FibTree a (b,(c,d)) -> Q (FibTree a (c,(b,d)))) t
-}
   {- do
            t'  <- fmove t
            t'' <-  lmap braid' t'
            fmove' t'' 
-}

           -- 
{-
pullLeftLeaf
pullRightLeaf


-}
{-
class Standardize a b | a -> b where
    standardize :: FibTree c a -> Q (FibTree c b)

instance Standardize a b where

type family Append a b where
    Append (a,b) c = (a, Append b c)
    Append a c = (a, c)

type family Leftize where
    Leftize (a,b) = Append (Leftize a) (Leftize b)
    Leftize a = a 

fullrightassoc = standardize
completerightassoc
bmove = fmove braid fmove'

standardize ::  FibTree c a -> Q (FibTree c (Leftize a))
standardize (TLeaf) = TLeaf
standardize (ILeaf) = ILeaf
standardize t = do 
                x <- pullLeft t
                rmap standardize x

lcamap :: FibTree a b -> Proxy (a :: Int) -> FibTree a ?
lcamap f n t@(TTT l r) | count l == n  = f t
                       | count l < n   = lcamap f (n - count l) r
                       | otherwise     = lcamap f n l   


pullRight ()




treerecursor :: FibTree a b -> (l -> r -> c) -> (leaf -> c) -> c
treerecursor (TTT l r) f g = f l r
treerecursor (TLeaf) = g Tleaf  

-}