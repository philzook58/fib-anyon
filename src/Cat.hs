{-# LANGUAGE GADTs, StandaloneDeriving, NoImplicitPrelude, FlexibleInstances, RankNTypes, 
TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, 
UndecidableInstances, AllowAmbiguousTypes, TypeFamilies  ,
 ConstraintKinds, TypeOperators, DataKinds, PolyKinds, InstanceSigs, NoMonomorphismRestriction
  #-}

module Cat where
import Control.Category
import Prelude hiding ((.), id)
import Fib
import Vec
import Control.Monad ((<=<))
import GHC.TypeNats
import Control.Arrow ((***))
import Data.Tuple (swap)


{-
-- make the leftmost porton of a match b, turning it into c
class LeftMatch pat a c | pat a -> c where 
    leftmatch :: FibTree e a -> Q (FibTree e c)
class RightMatch pat a c | pat a -> c where 
    rightmatch :: FibTree e a -> Q (FibTree e c)


instance (LeftMatch l a (l',r'), 
          RightMatch l l' l,
          LeftMatch r r' r'') => LeftMatch (l,r) a (l,r'') where -- is this necessary? A larger pattern should be a type error
  leftmatch x = do
                 x' <- leftmatch @l x
                 x'' <- lmap (rightmatch @l) x'
                 rmap (leftmatch @r) x''



-- instance LeftMatch ((a,b),c) 
-- pullLeftLeaf only does fmove' so this can't be enough
-- leftmatch l a, rightmatch r a, 
-- FullMatch l a

instance  (PullLeftLeaf a c) => LeftMatch Tau a c where -- c ~ (Tau,c') I should have a check like this. Sometimes a = Tau
    leftmatch x = pullLeftLeaf x
instance  (PullLeftLeaf a c) => LeftMatch Id a c where
        leftmatch x = pullLeftLeaf x


instance (RightMatch r a (l',r'), 
          LeftMatch  r r' r ,
          RightMatch l l' l'') => RightMatch (l,r) a (l'',r) where -- is this necessary? A larger pattern should be a type error
 rightmatch x = do
                x' <- rightmatch @l x
                x'' <- rmap (leftmatch @l) x'
                lmap (rightmatch @r) x''


instance  (PullRightLeaf a c) => RightMatch Tau a c where -- c ~ (Tau,c') I should have a check like this. Sometimes a = Tau
   rightmatch x = pullRightLeaf x
instance  (PullRightLeaf a c) => RightMatch Id a c where
       rightmatch x = pullRightLeaf x

--class LeftMatch b a b => ReAssoc a b where
-- instance LeftMatch b a b => ReAssoc a b where
-- instead use constraint kinds
type ReAssoc a b = LeftMatch b a b

t1 = leftmatch @(Tau,(Tau,Tau)) (TTT TLeaf (TTT TLeaf TLeaf))
--t2 = leftmatch @((Tau,Tau),Tau) (TTT TLeaf (TTT TLeaf TLeaf))

reassoc :: forall b a e. ReAssoc a b => FibTree e a -> Q (FibTree e b)
reassoc x = leftmatch @b x
-}

class ReAssoc a b where
    reassoc :: FibTree e a -> Q (FibTree e b)
instance (n ~ Count l',
    gte ~ CmpNat n (Count l), 
    LeftCollect n gte (l,r) (l'',r''),    
    ReAssoc l'' l',
    ReAssoc r'' r') => ReAssoc (l,r) (l',r') where
        reassoc x = do 
                    x' <- leftcollect @n x
                    x'' <- rmap reassoc x'
                    lmap reassoc x''
                    
--instance {-# OVERLAPS #-} ReAssoc a a where
 --   reassoc = pure
 --   
    
instance ReAssoc Tau Tau where
    reassoc = pure
instance ReAssoc Id Id where
    reassoc = pure



{-
 \((x,y),z) -> (x, (y,z))
 -- Compiling to monoidal categories.
 -- Could also find ways to pad in or out any () units automatically.
 -- Can I compile a linear function to monoidal categories using the associator?
 
 -- Then I could compile quantum functions. Neat.

 -- finding good swaps is tough though.
linear typed functions are symmettric monoidal categories, pretty sure. Not cartesian.
 
 -- The automatic associator

This right here has a lot of what we need to do compiling to categories of just monoidal categries.

-}

t4 :: Q (FibTree Tau (Tau,(Tau,Tau)))
t4 = reassoc (TTT TLeaf (TTT TLeaf TLeaf))
t5 :: Q (FibTree Tau ((Tau,Tau),Tau))
t5 = reassoc (TTT TLeaf (TTT TLeaf TLeaf))

{-
instance n ~ Count l,
         n' ~  Count 'l,
         gte ~ CmpNat l l'
     ReAssoc'    gte => ReAssoc' ((a,b),r) (l',r') 'GT
    reassoc' x = do
                  
-- Doing it this way is not efficient.
-- We don't need to full right associate to get it over there.
-- lcamap of n and n+1
instance ReAssoc ((l,a),b) (l',r') gte => ReAssoc' (l,(a,b)) (l',r') 'LT
    reassoc' x = do
                 x' <- rmap pullLeftLeaf x
                 x'' <- fmove x' -- now we add 1 to the smaller left side
                 reassoc' @gte x'' -- and try again
instance  ReAssoc l l', ReAssoc r r' => ReAssoc' (l,r) (l',r') 'EQ where
    reassoc' x = do
                 x' <- lmap reassoc x
                 rmap reassoc x'
                
instance ReAssoc' Tau Tau 'EQ where
    ressoc' = pure 
instance ReAssoc' Id Id 'EQ where
    ressoc' = pure 
-}
-- subtask
-- Collect n a b | n a -> b 
-- collect n elements of a into the left subtree.

leftcollect :: forall n gte l r o e. (gte ~ CmpNat n (Count l), LeftCollect n gte (l,r) o) => FibTree e (l,r) -> Q (FibTree e o)
leftcollect x = leftcollect' @n @gte x

class LeftCollect n gte a b | n gte a -> b where
    leftcollect' :: FibTree e a -> Q (FibTree e b)

-- The process is like a binary search.
-- LeftCollect pulls n leaves into the left branch of the tuple

-- If n is greater than the size of l, we recurse into the right branch with a new number of leaves to collect
-- then we do a final reshuffle to put those all into the left tree.
instance (
   k ~ Count l,
   r ~ (l',r'),
   n' ~ (n - k),
   gte ~ CmpNat n' (Count l'), 
   LeftCollect n' gte r (l'',r'')) => LeftCollect n 'GT (l,r) ((l,l''),r'') where
        leftcollect' x = do       
               x' <- rmap (leftcollect @n') x -- (l,(l'',r'')) -- l'' is size n - k
               fmove x'  -- ((l,l''),r'') -- size of (l,l'') = k + (n-k) = n
instance (
    l ~ (l',r'),
    gte ~ CmpNat n (Count l'), 
    LeftCollect n gte l (l'',r'')) => LeftCollect n 'LT (l,r) (l'',(r'',r)) where
        leftcollect' x = do       
                x' <- lmap (leftcollect @n) x -- ((l'',r''),r) -- l'' is of size n
                fmove' x'  -- (l'',(r'',r)

instance LeftCollect n 'EQ (l,r) (l,r) where
    leftcollect' = pure

-- We could define these functions for arbitrary monoidal categorues with reassociators (which is all of them)
-- The Count method requires things to be 
-- It's gonna be ugly. Would need more of the incoherent trick
-- Count (a,b) or Count a = 1 as default case.

t1 = leftcollect @2 (TTT (TTT TLeaf TLeaf) TLeaf)
t2 = leftcollect @1 (TTT (TTT TLeaf TLeaf) TLeaf)

{-
newtype FibOp' a b = FibOp' {runFibOp' :: forall e a' b'. (ReAssoc a a', ReAssoc b' b) => FibTree e a' -> Q (FibTree e b')} 
--newtype FibOp' a b = FibOp' {runFibOp' :: forall e a'. (ReAssoc a a', ReAssoc a' a) => FibTree e a' -> Q (FibTree e b)} 
-- newtype FibOp' a b = FibOp' {runFibOp' :: forall e a' b'. (ReAssoc a a', ReAssoc a' a, ReAssoc b b', ReAssoc b' b) => FibTree e a' -> Q (FibTree e b')} 

instance Category FibOp' where
    id :: forall a. FibOp' a a
    id = FibOp' pure
    (.) :: forall a b c. FibOp' b c -> FibOp' a b -> FibOp' a c
    (FibOp' f) . (FibOp' g) = undefined --  FibOp' $ reassoc <=< f <=< reassoc <=< reassoc <=< g <=< reassoc
    
    {-$ \x -> do
            
            x' <- g x
            x'' <- reassoc x'
            f x'' -}

-}       
    -- (f <=< reassoc <=< g)
-- t3 = leftcollect @3 (TTT (TTT TLeaf TLeaf) TLeaf) -- error
-- 
-- ReAssoc is just a recursive Collect n. Like how RightCaonical is recursive pullLeft
(...) :: ReAssoc b b' => FibOp b' c -> FibOp a b -> FibOp a c
(FibOp f) ... (FibOp g) = FibOp $ f <=< reassoc <=< g
 -- newtype FibOp c a b = FibOp {runFib :: (FibTree c a -> Q (FibTree c b))}
newtype FibOp a b = FibOp {runFib :: (forall c. FibTree c a -> Q (FibTree c b))}
-- type FibOp' c a b = FibTree c a -> Q (FibTree c b)

newtype LinOp a b = LinOp {runLin :: a -> Q b}
-- newtype LinOp a b = LinOp {runLin :: (Eq a, Eq b) => Q (a,b)} -- More matrix like form





{-
       data FibOuter a b where
        FibOuter :: FibTree e a -> FibTree e b -> FibOuter a b
    class FibIdent a where
        expansion :: [(FibOuter a a)]
    
    instance FibExpansion Tau where
        expansion = pure (FibOuter TLeaf TLeaf)
    instance FibExpansion Id where
        expansion = pure (FibOuter ILeaf ILeaf)
    instance (FibExpansion a , FibExpansion b) => FibExpansion (a,b) where
        expansion = pure (FibOuter ILeaf ILeaf) 
                 e1 = expansion @a
                 e2 = expansion @b
FibTree e a -> FibTree f b -> FibTree 
FibOuter a a -> FibOuter b b -> [FibOuter (a,b) (a,b)]
(FibOuter TLeaf TLeaf) (FibOuter TLeaf TLeaf) = [FibOuter (TTT TLeaf TLeaf) (TTT TLeaf TLeaf), (ITT TLeaf TLeaf) (ITT TLeaf TLeaf)]
-}
instance Category LinOp where
    id = LinOp pure
    (LinOp f) . (LinOp g) = LinOp (f <=< g)

instance Category (FibOp) where
  id = FibOp pure
  (FibOp f) . (FibOp g) = FibOp (f <=< g) 

class Category k => Monoidal k where   
    parC :: k a c -> k b d -> k (a,b) (c,d)
    assoc :: k ((a,b),c) (a,(b,c))
    unassoc :: k (a,(b,c)) ((a,b),c)
    leftUnitor :: k ((),a) a
    leftUnitor' :: k a ((),a)
    rightUnitor :: k (a,()) a
    rightUnitor' :: k a (a,())
    
    -- maybe we hsould just ignore these. They are annoying.
    {-
    type I :: *
    idl :: k (a, I) a
    idl' :: k a (a, I)
    idr :: k (I,a) a
    idr' :: k a (I,a) -}




instance Monoidal (FibOp) where
    parC (FibOp f) (FibOp g) = (FibOp (lmap f)) . (FibOp (rmap g))  -- This is where we need c to be forall. We want to be able to par... There isn't a unique way to do Tau?
    assoc = FibOp  fmove'
    unassoc = FibOp fmove
    leftUnitor = FibOp leftUnit
    leftUnitor' = FibOp leftUnit'
    rightUnitor = FibOp rightUnit
    rightUnitor' = FibOp rightUnit'

instance Monoidal LinOp where
    parC (LinOp f) (LinOp g) = LinOp $ \(a,b) -> kron (f a) (g b) -- This is where we need c to be forall. We want to be able to par... There isn't a unique way to do Tau?
    assoc = LinOp  (pure . assoc)
    unassoc = LinOp (pure . unassoc)
    leftUnitor = LinOp (pure . leftUnitor)
    leftUnitor' = LinOp (pure .leftUnitor')
    rightUnitor = LinOp (pure . rightUnitor)
    rightUnitor' = LinOp (pure . rightUnitor')

instance Monoidal (->) where
    parC f g =  f *** g 
    assoc ((x,y),z) = (x,(y,z))
    unassoc (x,(y,z)) = ((x,y),z)
    leftUnitor (_, x) = x 
    leftUnitor' x = ((),x)
    rightUnitor (x, _) = x
    rightUnitor' x = (x,())
    {-    
        type I = Id
         idl = FibOp $ \t -> case t of
                        TTI

    idl' = -}
{- 

Dagger? Dual? Rigid?, Compact closed
cap :: k (a,a) I
cup :: k I (a,a)
-- daulities. CPS, Either <-> TUple, Adjunctions
type Dual v = Q v -> Double
type Dual a = FibOp a I

DualFibOp b a = DualFibOp Dual a -> Dual b

dag :: FibOp a b -> DualFibOp b a
dag' :: uses dot?
-- Dual r a = Dual (a -> r)
-- a -> b ~ CoYoneda (a -> r, r -> b)
-- (a -> (), a) -> ()
-- LinOp a (LinOp b c) = a -> Q (b -> Q c) ~ (a,b) -> Q c
-- yeah I think we can swing that
-- Dual a = LinOp a ()
-- apply :: LinOp (Dual a, a) () -- the dual isn't really on the index though?
-- Bounded Enum a => LinOp () (Dual a, a)
-- 
-- a -> (  , () )
-- in data-category he uses FibOp a a as a stand in for object a
-- in this case FibOp Id Id = ILeaf, FibOp Tau Tau = TLeaf. They are indeed good stand ins
-- what am I doing. Duality is a bog
-- newtype Dagger a b = Dagger {primal :: LinOp a b, dual :: LinOp b a}
-- dag (Dagger f g) = Dagger g f
-- dag = FibOp . dot
-- 

-- We need to also make a category that draws diagrams
http://tex-talk.net/2011/09/the-braids-package/



(lmap  () ) (rmap (ttt) ) ttt

newtype CFibOp a b =  CFibOp {runCFib :: (forall c. FibTree c (RightAssoc a) -> Q (FibTree c (RightAssoc b)))}  
assoc = id
unassoc = id
par = ?

https://arxiv.org/abs/1811.06670

class Monoidal CFibOp where
   type Prod :: * -> * -> *
   Prod = Append
   par f g = rightAssoc >=> nmap @n ((lmap f) >=> (rmap g))


-}
class Monoidal k => Braided k where
    over :: k (a,b) (b,a)
    under :: k (a,b) (b,a)
    {- over . under = id -}

instance Braided FibOp where
    over = FibOp braid
    under = FibOp braid'

instance Braided (->) where
    over = swap
    under = swap

instance Braided (LinOp) where
    over = LinOp (pure . swap)
    under = LinOp (pure . swap)

 -- (Eq a) => Q (a,b) -> LinOp a b
-- LinOp (LinOp a b) () ->
-- () -> (a,a)
-- (a,a) -> () 
-- (Bounded a, Enum a) => LinOp () (a,a)
-- Eq a => LinOp (a,a) () ~ curry dot
-- curry :: k a (k b c) -> k (a,b) c
-- uncurry :: k (a,b) c)-> k a (k b c)
-- Dual k a b = forall r. k (k b r) (k a r)
-- type Dual a = LinOp a ()
-- newtype Dagger a = Dagger a
-- class Dagger k where
    -- type Dual :: * -> *
    -- dag :: k a b -> k (Dual b) (Dual a)
    -- dag' :: k (Dual a) (Dual b) -> k' b a 
--  Dual k
{-
class Monoidal k => Cartesian k where
    fstC :: k (a,b) a 
    sndC :: k (a,b) b 
    dupC :: k a (a,a) 

class Cartesian k => Closed k where
    applyC :: k (k a b,a) b 
    curryC :: k (a,b) c -> k a (k b c)
    uncurryC :: k a (k b c) -> k (a,b) c

fanC f g = (parC f g) . dupC

idC :: Category k => k a a
idC = id

data FreeCat a b where
    Comp :: FreeCat b c -> FreeCat a b -> FreeCat a c
    Id :: FreeCat a a
    Fst :: FreeCat (a,b) a
    Snd :: FreeCat (a,b) b
    Dup :: FreeCat a (a,a)
    Par :: FreeCat a b -> FreeCat c d -> FreeCat (a,c) (b,d)
    Add :: FreeCat (a,a) a
    Mul :: FreeCat (a,a) a
    Apply :: FreeCat (FreeCat a b, a) b
    Curry :: FreeCat (a,b) c -> FreeCat a (FreeCat b c)
    Uncurry :: FreeCat a (FreeCat b c) -> FreeCat (a,b) c
 
instance Closed FreeCat where
    applyC = Apply
    curryC = Curry
    uncurryC = Uncurry

deriving instance Show (FreeCat a b)

instance Category FreeCat where
    (.) = Comp
    id = Id

instance Monoidal FreeCat where
    parC = Par

instance Cartesian FreeCat where
    fstC = Fst
    sndC = Snd
    dupC = Dup
-}