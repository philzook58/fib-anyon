{-# LANGUAGE GADTs, StandaloneDeriving, NoImplicitPrelude, FlexibleInstances, RankNTypes, 
TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, 
UndecidableInstances, AllowAmbiguousTypes, TypeFamilies  ,
 ConstraintKinds, TypeOperators, DataKinds, PolyKinds, InstanceSigs, NoMonomorphismRestriction,
 DeriveFunctor,  DeriveFoldable, LambdaCase
  #-}

module Diag where
import Control.Category
import Prelude hiding ((.), id)
import Fib
import Vec
import Control.Monad ((<=<))
import GHC.TypeNats
import Control.Arrow ((***))
import Linear.V2
import Cat
import Data.Foldable
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1
morphism :: Diagram B
morphism = vrule 1 
-- morphism = rect 0.1 1
-- stack run -- -w 100 -h 100 -o out.svg
--mymain = mainWith $ runD ((parC id (parC id id)) . id)
class EmptyTree a where
    emptyTree :: STree' a (P2 Double)
instance (EmptyTree a, EmptyTree b)=> EmptyTree (a,b) where
    emptyTree = SNode' emptyTree emptyTree
instance {-# INCOHERENT #-} (a ~ (P2 Double)) =>  EmptyTree a where
    emptyTree = SLeaf' origin
id' = parC id id
-- d1 :: _
d1  = (runDM $ (id' . id')) emptyTree-- (SNode' (SLeaf' origin) (SLeaf' origin)) -- emptyTree -- $ SNode' (SLeaf origin ) (SNode' (SLeaf' origin) (SLeaf' origin))
mymain = mainWith (fst d1)
--( hsep 1 [morphism, morphism, morphism])

newtype StringD a b = StringD {runD :: Diagram B}

-- data StringD a b = [P2] [P2]
-- [P2] -> (Diagram B, [P2])
-- a -> Writer (Diagram B) b
-- id = \xs -> (zipWith arrowbetween in out, outs) where   outs = map (\(x,y) -> (x, y+1)) in
-- par = \xs -> f (split xs) <> g split xs -- but also need to normalize height 
-- replicate moprhism 3
-- {width, height, diagram} -> {width height diagram}
-- ugh, nevermind this sucks



data STree s where
    SNode :: STree a -> STree b -> STree (a,b)
    SLeaf :: Double -> STree Double
-- A true singletone would be more like STree ('Node a b)


data STree' s c where
    SNode' :: STree' a c -> STree' b c -> STree' (a,b) c
    SLeaf' :: c -> STree' (P2 Double) c
deriving instance Functor (STree' s)
deriving instance Foldable (STree' s)

type family Leftist a where
    Leftist (a,b) = Leftist a
    Leftist a = a

    -- "head" sort of
leftist :: STree s -> Leftist s 
leftist (SNode l r) = leftist l
leftist (SLeaf x) = x

f1 = leftist (SNode (SLeaf 3) (SLeaf 4))

-- tmap
vshift n p = p .+^ (V2 0 n) -- (Point (V2 x y)) = Point $ V2 x (y + n)
hshift n p = p .+^ (V2 n 0)
-- hshift n (Point (V2 x y)) = Point $  V2 (x+n) y
--rightist (SNode l r) = rightist r
--rightist (SLeaf x) = x

newtype DMorph a b = DMorph {runDM :: (STree' a (P2 Double) -> (Diagram B, STree' b (P2 Double))) } 
instance Category DMorph where
    id = DMorph $ \ps -> (mempty, ps) -- let ps' = fmap (vshift 1) ps in  (zipWith arrowBetween (toList ps) (toList ps'), ps')
    (DMorph f) . (DMorph g) =  DMorph $ \x -> let (d1, y) = g x in
                                              let y' = fmap (vshift 1) y in
                                              let d = fold $ zipWith arrowBetween (toList y) (toList y') in
                                              let (d2, z) = f y' in -- or put vshift here?
                                              (d2 <> d1 <> d, z) -- arrowBetween?
x_comp = fst . unp2
instance Monoidal DMorph where
    parC (DMorph f) (DMorph g) = DMorph $ \case (SNode' a b) ->  let (d1, c) = f a in
                                                                 let cmax = maximum $ fmap x_comp c in
                                                                 let amax = maximum $ fmap x_comp a in
                                                                 let xmax = max amax cmax in
                                                                 let b' = fmap (hshift (xmax + 1)) b in
                                                                 let (d2, d) = g b' in
                                                                (d1 <> d2, SNode' c d)
                               -- This is where we need c to be forall. We want to be able to par... There isn't a unique way to do Tau?
    assoc = error "Not implemented"
    unassoc = error "Not implemented"
    leftUnitor  = error "Not imlemented"
    leftUnitor' = error "Not imlemented"
    rightUnitor  = error "Not imlemented"
    rightUnitor'  = error "Not imlemented"
-- id par id par id. But we'll initlize it with space out stuff.
-- par f g = 
-- swap = \ SNode a b -> (SNode b a, vshift shift 
                                              -- maybe take in a single y position?
{-
    newtype STreeOp a b = STreeOp {runSTree :: STree a ->Double ->  (DiagramB, STree b, Double)}

instance Category STreeOp where
  id = STreeOp \x -> (mempty, m) -- id = \xs y -> (xs, y + 1)
  (STreeOp f) . (STreeOp g = STreeOp $ \x -> let (d1, y) = g x in
                                                 (d2, z) = f y in
                                                 (d2 === d1, z) -- arrowBetween?
                      -- BOxing things into Bus wires.
-}
instance Category StringD where
    id = StringD (vrule 1) -- sometimes mempty (when id . f)sometimes vrule 1 (when parc id f)
    (StringD x) . (StringD y) = StringD (x === y)
instance Monoidal StringD where
    parC (StringD f) (StringD g) = StringD (hsep 1 [f,g]) -- This is where we need c to be forall. We want to be able to par... There isn't a unique way to do Tau?
    assoc = (StringD mempty )
    unassoc = (StringD mempty )
    leftUnitor  = (StringD mempty )
    leftUnitor' = (StringD mempty )
    rightUnitor  = (StringD mempty )
    rightUnitor'  = (StringD mempty )
    