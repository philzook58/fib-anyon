{-# LANGUAGE FlexibleContexts, TypeOperators, RankNTypes, GeneralizedNewtypeDeriving, DeriveFunctor, PolyKinds, GADTs #-}

module Qubit where

import Linear.V2
import Linear.V1
import Linear.Vector
import Vec
import Data.Complex
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity
import Cat
import Data.Coerce

type Kron = Compose
type Spin = V2
type QuBit = V2
type C = Complex Double

type DSum = Product

newtype FKron f g a = FKron [(f a, g a)]

type Fock a =  Q [a] -- a "fock space" over a. No assumption of anti symmettric or symmettric. 
newtype Anti a = Anti (Fock a)
-- for antisymmettric space, keep list sorted (canonical form).
-- liftAnti1 :: (a -> Q a) -> ([a] -> Q [a])
-- liftAnti2 :: (a,a) -> Q (a,a) -> ([a] -> Q [a])
-- 

-- this is fock system as a sparse indictaor matrix. We can also do the form of occupation number  Q (V Bool)

vac :: Fock a
vac = pure []

type SHO = Q Int

shovac :: SHO 
shovac = pure 0
adag :: Int -> SHO
adag n = W [(n + 1, sqrt (fromInteger (toInteger n) + 1))]
agad 0 = mempty
agad n = W [(n - 1, sqrt n)]


raise = LinOp adag
lower = LinOp agad

-- boson system can be seen as collection of oscillators. Q (V Int).  Positive integers really.

type Boson v = Q (v Int) -- accepts a functor describing all the oscillators. A Vec for example.
-- bvac = pure zero -- if v is a Vector type, it probably has a zero.
--     


-- traversable ordering. The vector is put into canocnical form with adag appearing first in the traversal coming first.
type Fermion v = Q (v Bool) -- fermion needs to prefix count to figure out signs. This means that v has to be traversable? Foldable?
fvac :: Applicative v => Fermion v
fvac = pure (pure False)
{-
-- what about systems with different types of particles.
particle-hole  Q ([a], [a])


AntiOp a = AntiOp [a] -> Q [a]
-- indexed linear operators. a -> AntiOp a
psi :: Ord a => a -> (a -> Anti a)
psidag :: Ord a => a -> (a -> Anti a)

phi :: Ord a => a -> (a -> Sym a)
phidag :: Ord a => a -> (a -> Sym a)

-- hmm. Something kind of funny about these.
lift1 :: (i -> Double) -> (i -> Fock i)
lift2 :: (i -> i -> Double) -> (i -> Fock i)
symlift -- mention that there is implcit symmettrization
2 natural lifts. cnnected by logarithms. UxUxUxU vs HIII + IHII + IIHI + IIIH


sum1 :: (i -> i -> Q i) -> (i -> Q i) -- takes an indexed operator and turns it into an operator
sum1 :: (i -> AntiOp i) -> (AntiOp i)
sum2
sum :: Addable b => (i -> b) -> b


turn all of this into a typeclass functions. Then do this form, but also full initial form for manipulation.

dyson

-}
-- instance (Additive f, Additive g) => Additive (Compose f g) where
-- instance Num f (g a) => Num (Compose f g a)

-- (Kron (Kron V2 V2) V2) Complex Double  -- a 3 qubit vector space
swap :: (Traversable f, Applicative g) => Kron f g a -> Kron g f a
swap = Compose . sequenceA . getCompose

-- kron :: (Num a, Functor f, Functor g) => f a -> g a -> Kron f g a
-- kron f g = Compose $ fmap (\amp1 -> fmap (\amp2 -> amp1 * amp2) g) f
kron' :: (Num a, Functor f, Functor g) => f a -> g a -> Kron f g a
kron' x y = Compose (outer x y)

densify ::  (Additive (Kron f g), Num a, Functor f, Functor g) => FKron f g a -> Kron f g a
densify (FKron xs) = sumV $ map (uncurry kron') xs


kron''' :: (Applicative f', Applicative g', Traversable f, Traversable g') =>
    (f a -> f' a) -> (g a -> g' a) -> (Kron f g a -> Kron f' g' a)
kron''' lf lg (Compose fga) = Compose $ sequenceA $ (fmap lf) $ sequenceA  $ (fmap lg fga)

type LinOp' f g a = f a -> g a
-- SVD let's you re free
-- but also [indicatro vectro, g a]
-- and perhaps randomize sampling.
    {-
sigx :: Spin C -> Spin C
sigx (V2 up down) = V2 down up

sigz :: Spin C -> Spin C
sigz (V2 (a :+ b) (c :+ d)) = V2 ()

sigz :: Spin C -> Spin C
sigz (V2 up down) = V2 up (-down) 
-}
{-
class Add a where
    (+++) :: a -> a -> a
    negate' :: a -> a

instance Num a => Add a where
    +++ = +
instance    => Additive f
-}


flip' :: Spin a -> Spin a
flip' (V2 up down) = V2 down up

sigx = flip'

sigz :: Num a => Spin a -> Spin a
sigz (V2 up down) = V2 up (negate down) 



-- phase :: Complex a => Spin a -> Spin a
type f ~> g = forall a. (Functor f, Functor g) => f a -> g a
type f *** g = Compose f g
-- (Functor f, Functor g, Functor h, Functor k)
-- we only need g to be a functor
-- see below. If we remove the type signature, this implementation generalizes to another type
par :: Functor g => (forall a. f a -> g a) -> (forall b. h b -> k b) -> (forall c. (Compose f h c) -> (Compose g k c))
par f g = Compose . (fmap g) . f . getCompose -- insufficient though. We can't write linear operators this way

-- I should used V1, not Identity
-- These are all also coerce.
rightUnitor :: Functor f => Compose f Identity a -> f a 
rightUnitor (Compose f) = fmap runIdentity f

rightUnitor' :: f ~> Compose f Identity
rightUnitor' = Compose . fmap Identity

leftUnitor' :: f ~> Compose Identity f
leftUnitor' = Compose . Identity

leftUnitor :: Identity *** f ~> f
leftUnitor = runIdentity . getCompose

-- should be coerce. Confusing role problems
assoc :: Functor f => ((f *** g) *** h) ~> (f *** (g *** h))
assoc = Compose . (fmap Compose) . getCompose . getCompose

assoc' :: Functor f =>  (f *** (g *** h)) ~> ((f *** g) *** h)
assoc' (Compose x)  = Compose $ Compose $ (fmap getCompose x) 


-- if the functor in question happens to be representable
    -- then we can use our previous defitions of monoidal catgoeries.
    -- Reader Functors
-- Num c => prod :: Reader a c -> Reader b c -> Reader (a,b) c
-- Num 
-- newtype ReadCat c a b = ReadCat (Reader a c -> Reader b c)
-- newtype LinOp c a b = LinOp ((a -> c) -> (b -> c))
-- par (LinOp f) (LinOp g) = LinOp \h -> h (\(a,b) ->   )
-- FMapN
-- fmapN @n
{-Compose f g



assoc :: Compose (Compose f g) h a -> Compose f (Compose g h) a

assoc = Compose . (fmap Compose) . getCompose . getCompose

Compose f Id a -> f a

rightunit = (fmap runIdentity) . getCompose

The morphisms of the functor category are natural transformations

par :: (f ~> g) -> (h~> k) -> ((Compose f h) ~> (Compose g k))





-}


newtype Kron' k l f a = Kron' ((k (l f)) a) deriving Functor

-- deriving via compose
{-
newtype V2' f a = V2' (Compose V2 f a)  deriving Functor
newtype V1' f a = V1' (Compose V1 f a) deriving Functor
-}
{-
newtype V2' f a = V2' (V2 (f a))  deriving Functor
newtype V1' f a = V1' (f a) deriving Functor
-}

type V2' f a = Kron V2 f a
type V1' f a = Kron V1 f a

lowerv1 (Compose (V1 f)) = f -- It's a coerce. V1 is assuredly a newtype
{-
exvsa :: (Additive f, Num a) => V2' f a 
exvsa = zero
-}

-- constant vecotrs a morphisms from unit
-- Using f ~ one = (V1 1) 
exconst :: (a ~ Double, Functor f) => V1' f a -> V2' f a
exconst x = let x' = lowerv1 x in Compose $ V2 (1.4 *^ x') (7.3 *^ x')

one = V1 1
attemp = exconst (Compose (V1 one))

-- Applicative composes, so Additive probably should too.
-- Wait is this in the library?
-- Why is applicative g necessary?
{-
Ok. There doesn't appear to be a compose
But V2 etc have a Num instance
and liftU2 has a default implementation that uses Applicative
I probably don't need Applcative g if I define a liftU2 using g's liftU2

-}
-- I should check to make sure the lift makes sense
-- Why isn't this in the library? There must be a good reason.
-- Is there an additive instance for Product?

instance (Additive g, Additive f) => Additive (Compose f g) where
    (Compose v) ^+^ (Compose w) = Compose (liftU2 (^+^) v w) -- I  we might be able to used
    zero = zero -- what?
    liftU2 f (Compose x) (Compose y) = Compose $ liftU2 (liftU2 f) x y
    liftI2 f (Compose x) (Compose y) = Compose $ liftI2 (liftI2 f) x y

type LinOp1 f g a = forall k. Additive k => Kron f k a -> Kron g k a

-- This feels less right to me. But why?
type LinOp1' f g a = forall k. Functor k => Kron k f a -> Kron k g a
{-
It feels less right perhaps because it will change how we do things so little.
We just have to fmap.
No, but saying we always have direct access to the scalar isn't right.
-}


-- reassoc :: Kron f (Kron g k) a -> Kron (Kron f g) k a
-- reassoc = coerce

--reassoc' ::  Kron (Kron f g) k a -> Kron f (Kron g k) a
--reassoc' = -- coerce

-- yikes. coerce will help a bit. Quite a bit.
-- Also Fixing the unnecessary applicative instances.
-- Additive k' is expected. Applicative is coming from Additve (Kron )  constraint.

{-kron'' :: (Applicative g, Applicative k', Additive k', Functor f', Functor f, Functor g') => 
  (forall k. Additive k => Kron f k a -> Kron f' k a) -> (forall k. Additive k => Kron g k a -> Kron g' k a) -> (Kron (Kron f g) k') a -> (Kron (Kron f' g') k') a
-}

kron'' :: (Additive f, Additive g, Additive k, Additive f', Additive g') => 
  LinOp1 f f' a -> LinOp1 g g' a -> Kron (Kron f g) k a -> Kron (Kron f' g') k a
kron'' lf lg fgk = let v = (Qubit.assoc fgk) in Qubit.assoc' (Compose $ fmap lg $ getCompose (lf v))


leftUnit :: Kron V1 (Kron f k) a -> Kron f k a
leftUnit (Compose (V1 x)) = x
{-
(f a -> g a) is a natural transfromation
(Fractional a => f a -> g a) is a numerical transfromation

Horizontal composition in Functor land. Sure.
Even in applicative
The issue is that Good linear ops are going to hold numbers. So ti feels wrong to quanitfy over them
One option is to add a type class constraint
(Num a) or (Fractional a)
This will work because linear lifts these


-- this is the same as par written above.

-- Functor f => (t -> f a) -> (a -> b) -> t -> f b
The oppotite ordering
--  Functor f' => (f' b -> t') -> (a -> b) -> f' a -> t'
The more general type as inferred is
f b ~ t'

(f' a -> f a) -> (a -> b) -> (f' a -> f b)
vs 
(f' b -> f b) -> (a -> b) -> (f' a -> f b)
vs.
forall

kron l l' x = l' (fmap l x)

(forall f. Additive f => Kron V2 f a -> Kron V3 f a) -> () -> (forall p. Kron f (Kron k p) -> Kron f' (Kron k' p) )
We want to allow the field a to unify across them all. 
The scalar is generic in a certain sense, but not in another.

This is the lifted Kron'
We don't want Additive1, something went screwy there.
Additive (Kron' ) 


The problem with Num based Kron: If we define LinOp
(LinOp f g = forall Num a => f a -> g a
Why Num, Why not Fractional. This is a problem.
And really we want to be threading the scalar a throughout the entire system.

kron

And what if we unified the two orderings


Hmm. These are pretty similar to van Laarhoven lenses. This observation is porbably poison
Swtich the arguments
(a -> b) -> Lens 
Functor f => (t -> f a) -> (t -> f b)
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
Lens t a t b

kron :: (Fractional a => f a -> g a) -> (Fractional k) -> LinOp
kron l l' (Compose fk) =  Compose (fmap l' (l fk))


Constrained natural transformation
kron  forall const. (const a => f a -> g a) -> (forall b. const (k b) => k b -> l b)




(f Double -> g Double) ->
(forall f. V2' f Double -> V2' f Double)


kron :: LinOp f g -> LinOp k l -> LinOp (Kron f k) (Kron g l)


-}

{-

-- can't build. Well we can build vzero
exv2' :: (Additive f, Num a) => V2' f a
exv2' = V2' (Compose ( ?  ))  



-}
-- constants are given as morphisms
-- exv2' :: (Additive f, Num a) => V1' f a -> V2' f a
-- exv2' (V1' f) = (V2' (V2   )

{-
There is no additive instance for Compose?
Huh.


class (forall f. Additive f => Additive k f) => Additive1 k where

I'm doing something insane.
Yea, The 1 pattern. I've been here before and decided it was insane.
Something in purescript. Surely it wasn't Additive1 was it?


-}
{-
class Additive1 k where
    vadd :: (Additive f, Num a) => k f a -> k f a -> k f a
    vzero :: (Additive f, Num a) => k f a
instance Additive1 V2' where -- (Functor (V2' f)) => 

    vadd (V2' (V2 v w)) (V2' (V2 x y)) = V2' (V2 (v ^+^ x)  (w ^+^ y)   )
    vzero = V2' (V2 zero zero)

    
    --vadd (V2' v) (V2' w) = V2' (v ^+^ w)
    --vzero = V2' zero
    
instance Additive1 V1' where -- (Functor (V2' f)) => 
    vadd (V1' v) (V1' w) = V1' (v ^+^ w)
    vzero = V1' zero

instance (Additive1 k, Additive1 l) => Additive1 (Kron' k l) where
   vadd (Kron' x) (Kron' y) = Kron' (vadd x y)
   vzero = Kron' vzero
   -}
{-
Deriv functor for V2'
-}



