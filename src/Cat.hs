{-# LANGUAGE GADTs, StandaloneDeriving, NoImplicitPrelude, FlexibleInstances, RankNTypes  #-}

module Cat where
import Control.Category
import Prelude hiding ((.), id)
import Fib
import Vec
import Control.Monad ((<=<))

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