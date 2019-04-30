{-# LANGUAGE TypeOperators, 
TypeFamilies, DataKinds, UndecidableInstances, 
PolyKinds, RankNTypes #-}

module Vect2 where

import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose
import GHC.TypeLits
-- import Data.Functor.Const
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

type f :+: g = Product f g
type f :*: g = Compose f g

--type V0 a = Const ()

type Test = '[ '[ V0, V1], '[V2, V3]]
type TestV = '[Maybe, V4]
-- type :+: 
{-
type family Dot v v' where
    Dot (x : xs) (y : ys) = Either (x,y) (Dot xs ys) 
-}
type family Dot v v' where
    Dot '[x] '[y] = x :*: y 
    Dot (x : xs) (y : ys) = (x :*: y) :+: (Dot xs ys)
    -- Dot '[x] '[y] = x :*: y -- V0
type family MVMult m v where
    MVMult '[r] v = '[Dot r v]
    MVMult (r : rs) v = (Dot r v) : (MVMult rs v)
type family VMMult m v where
    VMMult v '[c] = '[Dot v c]
    VMMult v (c : cs) = (Dot v c) : (VMMult v cs)
{-
type family MMMult' m m' where
    MMMult' '[r] cs = VMMult r cs
    MMMult' (r : rs) (c : cs) = ((Dot r c) : (VMMult r cs)) : (MMMult' rs (c : cs))
-}
type family MMMult' m m' where
    MMMult' '[r] cs = '[VMMult r cs]
    MMMult' (r : rs) cs = (VMMult r cs) : (MMMult' rs cs)


type family MMMult m m' where
    MMMult m m' = MMMult' m (Transpose m')


type family VApply v a where -- VApply -> f a :+: g a?
    VApply '[x] a = x a
    VApply (x : xs) a = ((x a),  VApply xs a)
type family VSum f where
    VSum '[f] = f
    VSum (f : fs) = f :+: (VSum fs)

type f $$ a = (VSum f) a

type family Transpose m where
    Transpose ((r1 : rs') : rs) = (r1 : (Heads rs)) : (Conss rs' (Transpose (Tails rs)))
    Transpose '[] = '[]
-- some mapped helper functions
-- verrrrrry ugly. Get 'er dun
type family Heads v where
    Heads ((v : vs) : xs) = v : (Heads xs)
    Heads '[] = '[]
type family Tails v where
    Tails ((v : vs) : xs) = vs : (Tails xs)
    Tails '[] = '[]
type family Conss v vs where
    Conss (x : xs) (y : ys) = (x : y) : (Conss xs ys)
    Conss '[] '[] = '[]
type family Index v i where
    Index (x : xs) 0 = x
    Index (x : xs) n = Index xs (n-1)

{-
Alternaitve - define linear maps using type family name per map
Can't go higher order though. 



2Vect - a 2-catgegory
The objects are multi copies of Vect, the category with vector spaces
as objects 
3-2Vect object = (Vect, Vect, Vect)
2-2Vect object = (Vect, Vect) 
etc

we represent this with '[f, g, h]

The morphisms are linear functors. We reprsent this as matrices.
What the hell is a linear functor? I guess functors from Vect to Vect. Basically always kroning with another space?
WHy not a projection? It might not have the functor property
The matrices use the direct sum as sum and tensor produ as product
In haskell, this correspond to the Functor Compose and Product types
We could choose to work in an indexful form also.

The 2-morphisms are linear natural trnasformations
These actually have data associated with them
could avoid the VSum.
Could directly work in tuple space?

forall f g. VSum (MVMult m '[f, g]) a -> VSum (MVMult m' '[f, g]) a

(MVMult m '[f,g]) $$ Double -> (MVMult m' '[f,g]) $$ Double

-}

type NatEx m m'= forall f g. (MVMult m '[f,g]) $$ Double -> (MVMult m' '[f,g]) $$ Double
type NatEx' m m'= forall v. (MVMult m v) $$ Double -> (MVMult m' v) $$ Double

