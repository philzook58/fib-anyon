{-# LANGUAGE FlexibleContexts #-}

module Qubit where

import Linear.V2
import Linear.Vector
import Vec
import Data.Complex
import Data.Functor.Compose
import Data.Functor.Product


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
kron :: (Num a, Functor f, Functor g) => f a -> g a -> Kron f g a
kron x y = Compose (outer x y)

densify ::  (Additive (Kron f g), Num a, Functor f, Functor g) => FKron f g a -> Kron f g a
densify (FKron xs) = sumV $ map (uncurry kron) xs

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


-- FMapN
-- fmapN @n