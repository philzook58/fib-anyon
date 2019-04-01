{-# LANGUAGE FlexibleContexts, TypeOperators, RankNTypes #-}

module Qubit where

import Linear.V2
import Linear.Vector
import Vec
import Data.Complex
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity
import Cat

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
par :: Functor g => (forall a. f a -> g a) -> (forall b. h b -> k b) -> (forall c. (Compose f h c) -> (Compose g k c))
par f g = Compose . (fmap g) . f . getCompose

rightUnitor :: Functor f => Compose f Identity a -> f a 
rightUnitor (Compose f) = fmap runIdentity f

rightUnitor' :: f ~> Compose f Identity
rightUnitor' = Compose . fmap Identity

leftUnitor' :: f ~> Compose Identity f
leftUnitor' = Compose . Identity

leftUnitor :: Identity *** f ~> f
leftUnitor = runIdentity . getCompose

assoc :: Functor f => ((f *** g) *** h) ~> (f *** (g *** h))
assoc = Compose . (fmap Compose) . getCompose . getCompose


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