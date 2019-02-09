{-# LANGUAGE GADTs, StandaloneDeriving, NoImplicitPrelude, FlexibleInstances, RankNTypes, 
TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, 
UndecidableInstances, AllowAmbiguousTypes, ConstraintKinds, TypeOperators, DataKinds, PolyKinds  #-}

module Cat where
import Control.Category
import Prelude hiding ((.), id)
import Fib
import Vec
import Control.Monad ((<=<))
import GHC.TypeNats
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


t1 = leftcollect @2 (TTT (TTT TLeaf TLeaf) TLeaf)
t2 = leftcollect @1 (TTT (TTT TLeaf TLeaf) TLeaf)



-- t3 = leftcollect @3 (TTT (TTT TLeaf TLeaf) TLeaf) -- error
-- 
-- ReAssoc is just a recursive Collect n. Like how RightCaonical is recursive pullLeft

-- newtype FibOp c a b = FibOp {runFib :: (FibTree c a -> Q (FibTree c b))}
newtype FibOp a b = FibOp {runFib :: (forall c. FibTree c a -> Q (FibTree c b))}
-- type FibOp' c a b = FibTree c a -> Q (FibTree c b)

instance Category (FibOp) where
  id = FibOp pure
  (FibOp f) . (FibOp g) = FibOp (f <=< g) 

class Category k => Monoidal k where   
    parC :: k a c -> k b d -> k (a,b) (c,d)
    assoc :: k ((a,b),c) (a,(b,c))
    unassoc :: k (a,(b,c)) ((a,b),c)
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
    {-    
        type I = Id
         idl = FibOp $ \t -> case t of
                        TTI

    idl' = -}
{- 

Dagger? Dual? Rigid?, Compact closed
cap :: k (a,a) I
cup :: k I (a,a)

type Dual v = Q v -> Double
type Dual a = FibOp a I

DualFibOp b a = DualFibOp Dual a -> Dual b

dag :: FibOp a b -> DualFibOp b a
dag' :: uses dot?


-- in data-category he uses FibOp a a as a stand in for object a
-- in this case FibOp Id Id = ILeaf, FibOp Tau Tau = TLeaf. They are indeed good stand ins

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