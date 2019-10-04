{-# LANGUAGE TypeOperators, 
TypeFamilies, DataKinds, UndecidableInstances, 
PolyKinds, RankNTypes, GADTs, TypeSynonymInstances, FlexibleInstances #-}

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
import Fib
import Data.Void
import Data.Functor.Identity
import Data.Proxy
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


type family Append v v' where
    Append '[] v = v 
    Append (x : xs) v = x : (Append  xs v)

-- [(a, f)]
-- [(a,b,f)]
{- A type family for a sparse matrix format [(r,c,val)]
type family MMult2 m v where
    MMult2  ( (y,x,f) : ms)   ((x,g) : xs) = (y, f :*: g) : 
    MMult2 _ '[] = '[]
    MMult2 '[] _ = '[]
-}
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

{-


-}


data FibV :: * -> * -> * -> (* -> *) where
    TTT :: a -> FibV Tau Tau Tau a
    TTI :: a -> FibV Tau Tau Id a
    TII :: FibV Tau Id Id a
    -- and so on
-- This can be desugared further into a sequence of newtypes
-- Which correpsond to partial applications of FibV
-- and has layout benefits probably


type Vect = * -> *

type V2_2 = V2 Vect
type ExV3 = ('V2 V3 V4) --  :: V2 Vect

type M23_2 = V2 Vect -> V3 Vect -- a kind
type M23_2' = V2 (V3 Vect) -- a kind.

type family ExM23 (a :: V2 Vect) :: V3 Vect where -- V2 Vect -> Linear.V3.V3 Vect
    ExM23 ('V2 f g) = 'V3 f g g
data ExM23' :: V2 Vect -> * where
    ExM23' :: Proxy ('V3 f g g) -> ExM23' ('V2 f g) 
data ExM23''' :: V2 Vect -> V3 Vect -> * where
    ExM23''' :: ExM23''' ('V2 f g) ('V3 f g g)
type ExM''' = 'V2 ('V3 V1 V0 V2) ('V3 V3 V1 V0) -- :: V2 (V3 Vect), Kmett style matrix
data IndM :: Nat -> Nat -> Vect -> * where
    IndM00 :: IndM 0 0 V2
    IndM10 :: IndM 1 0 V1
    IndM01 :: IndM 0 1 V3
    IndM11 :: IndM 1 1 V4
-- Yet another style. 
-- This is the style I was suggesting with FibV Tau Tau Id
-- Analogou to a -> b -> Double formulation of matrix.
data IndM' :: Nat -> Nat -> Vect where
    IndM00' :: V1 a -> IndM' 0 0 a
    IndM10' :: V2 a -> IndM' 1 0 a
    IndM01' :: V3 a -> IndM' 0 1 a
    IndM11' :: V4 a -> IndM' 1 1 a
-- IndM' 0 0 ~ V1
-- IndM' 0 1 ~ V2

-- This is the above List style and Kmett vectors are to list vectors

-- MatMul
-- => is also not polykinded, Constraint -> * probably
-- type ExM23_4 v = forall f g. ('V2 f g ~ v) => ('V3 f g g) 

-- Functors. The partially applied kron
newtype V2' f a = V2' ((V2 :*: f) a) 
-- V2' :k  DualV1 Vect -- (* -> *) -> (* -> *)
-- Things likes V2' are the analog of 1-d vectors in 2vect.
-- scalars are * -> *
-- scalars and 1-d vectors are isomorphic, but slightly different.
{-
-- There is an isomorphism between Hom(Fun, Fun) and Vect.
-- This is a theorem / comment in Kitaev
thm :: (forall f. V2' f a -> V2' f a) -> (V2 a -> V2 a)
thm f = fmap (runIdentity) . f . fmap Identity . runCompose
thm2 :: (V2 a -> V2 a) -> (V2' f a) -> (V2' f a)
thm2 f =  
-}

-- My naming conventions are a disaster
{-

Type in Type.
I'm not per say proving anything. I want to calculate/compute. There is a difference.
The kinds and such are guiding correct implementation

-}

-- type ExV3 :: V2 Vect

-- type M ('V2 f g) -> 
{-
-- nope. -> is not polykinded
data PolyV2 :: a -> * where
    PolyV2 :: a -> a -> PolyV2 a




    -}

newtype VNegInf a = VNegInf Void
-- In the functor number, negative infinite because log(0) = -infity

{-
One can construct very raw seeming dual vectors, requiring no data structure facilities    
    -}
type DualV2 a = a -> a -> a
type DualV3 a = a -> a -> a -> a
exv2 :: DualV2 Double
exv2 x y = 3*x + 2*y


-- The advantage of this style is that it seems we don't need anything
-- except type synonyms and newtypes
-- Because of TypeinType DualV2 is both type of value level and typelevel thing 
-- regular * and +
type DualV2'' = DualV2 *
type Two = Either () ()
type ExV2'' a b = Either (Two, a) b 

-- functor level * and +
type DualV3' = DualV3 Vect
type DualV2' = DualV2 Vect -- Vect -> Vect -> Vect
type ExV2 x y = (V3 :*: x) :+: (V2 :*: y)

type DM23 a = DualV2 a -> DualV3 a
exm23 :: DM23 Double
exm23 v2 x y z = v2 (2 * x + 3 * y) z -- matrix = [[2,3,0]. [0,0,1]] 

type DM23' = DualV2 Vect -> DualV3 Vect
type ExM23'' v2 x y z = v2 (V2 :*: x :+: V3 :*: y) z

-- typelevel typeclass polymorphism


type family NV (n :: Nat) :: Vect where
    NV 0 = V0
    NV 1 = V1
    NV 2 = V2
    NV 3 = V3
    NV 4 = V4 -- Well this one won't happen. Ok it might
    NV 5 = V2 :+: V3
    NV 7 = V4 :+: V3
    NV 11 = (NV 4) :+: (NV 7)
-- could certainly automate this.
-- Write in decimal expansion. Me dunno.

-- Writing a functor in it's prime factorization is a very interesting form
-- See conal eliotts parallel paper.
-- makes FFT, sorting network,
-- type familt PrimeFact n

class TNum a where -- a is a kind.
    type (+++) (x :: a) (y :: a) :: a -- (+) is taken by TypeLits.
    type (***) (x :: a) (y :: a) :: a-- star is type. Hmm
    {- fromInteger :: n -> * -} -- associated type family?

instance TNum Vect where
    type f +++ g = f :+: g
    type f *** g = f :*: g
    -- fromInteger n = NV n

instance TNum * where
    type f +++ g = Either f g
    type f *** g = (f, g)
{-
-- Doesn't work

instance TNum b => TNum (a -> b) where
    type (+++) f g i = (f i) +++ (g i)
    type (***) f g i = (f i) *** (g i)

    -}

type Test7 = V2 +++ V2



{-
Using Linear types with V2Dual Vect
It seems like we may be able to enforce linearity?

-}
newtype D2 a = D2 (a -> a -> a -> a)

newtype Dual f a = Dual (f a -> a)
newtype DD2 = DD2 (Dual (D2) Vect)
