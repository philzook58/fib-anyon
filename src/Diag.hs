{-# LANGUAGE GADTs, StandaloneDeriving, NoImplicitPrelude, FlexibleInstances, RankNTypes, 
TypeApplications, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, 
UndecidableInstances, AllowAmbiguousTypes, TypeFamilies  ,
 ConstraintKinds, TypeOperators, DataKinds, PolyKinds, InstanceSigs, NoMonomorphismRestriction
  #-}

module Diag where
import Control.Category
import Prelude hiding ((.), id)
import Fib
import Vec
import Control.Monad ((<=<))
import GHC.TypeNats
import Control.Arrow ((***))

import Cat

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1
morphism :: Diagram B
morphism = vrule 1 
-- morphism = rect 0.1 1

mymain = mainWith $ runD ((parC id (parC id id)) . id)
--( hsep 1 [morphism, morphism, morphism])

newtype StringD a b = StringD {runD :: Diagram B}

-- data StringD a b = [P2] [P2]
-- [P2] -> (Diagram B, [P2])
-- a -> Writer (Diagram B) b
-- id = \xs -> (zipWith arrowbetween in out, outs) where   outs = map (\(x,y) -> (x, y+1)) in
-- par = \xs -> f (split xs) <> g split xs -- but also need to normalize height 
-- replicate moprhism 3
-- {width, height, diagram} -> {width height diagram}
--

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
    