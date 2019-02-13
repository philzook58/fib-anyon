{-# LANGUAGE FlexibleContexts, TypeOperators, RankNTypes #-}

module Qubit where

import Linear.V2
import Linear.Vector
import Vec
import Data.Complex
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity


type Kron = Compose
type Spin = V2
type QuBit = V2
type C = Complex Double

type DSum = Product

newtype FKron f g a = FKron [(f a, g a)]



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