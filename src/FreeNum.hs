{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveTraversable #-}
module FreeNum where

data FreeNum a = Plus (FreeNum a) (FreeNum a) | 
    Times (FreeNum a) (FreeNum a) | 
  --  Minus (FreeNum a) (FreeNum a) | 
    Neg (FreeNum a) | ILit Integer | Lit a | Signum (FreeNum a) | Abs (FreeNum a) deriving (Show, Functor, Traversable, Foldable, Eq) -- structural equality?
instance Num (FreeNum a) where -- could remove minus
    (+) = Plus
    -- (-) = Minus -- defulats to Plus x (Neg y)
    (*) = Times
    negate = Neg
    fromInteger = ILit
    abs = Abs
    signum = Signum

vx :: FreeNum String  
vx = Lit "x"
vy :: FreeNum String  
vy = Lit "y"
vz :: FreeNum String  
vz = Lit "z"


-- if we were going with single rules
doubneg :: FreeNum a -> FreeNum a
doubneg (Neg (Neg x)) = doubneg x
doubneg (Neg (ILit x)) = ILit (negate x)
doubneg (Neg x) = Neg (doubneg x)
doubneg (Plus x y) = Plus (doubneg x) (doubneg y)
doubneg (Times x y) = Times (doubneg x) (doubneg y)
doubneg (Signum x) = Signum (doubneg x)
doubneg (Abs x) = Abs $ doubneg x
doubneg (ILit x) = (ILit x)
doubneg (Lit a) = Lit a


pretty :: Show a => FreeNum a -> String
pretty (Plus x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"
pretty (Times x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"
pretty (Neg x) = "-" ++ pretty x
pretty (Abs x) = "|" ++ pretty x ++ "|"
pretty (ILit x) = show x
pretty (Lit x) = show x
pretty (Signum x) = "signum(" ++ show x ++ ")" 


-- big momma rule
simplify :: FreeNum a -> FreeNum a
-- double neagtion
simplify (Neg (Neg x)) = simplify x
-- abs of neagtive is irrelant
simplify (Abs (Neg x)) = simplify $ Abs x


-- absorb 0, 1
simplify (Plus (ILit 0) y) = simplify y
simplify (Times (ILit 0) _) = ILit 0
simplify (Times (ILit 1) y) = simplify y
simplify (Plus x (ILit 0) ) = simplify x
simplify (Times _ (ILit 0) ) = ILit 0
simplify (Times x (ILit 1) ) = simplify x

-- sum of products
simplify (Times x (Plus y z)) = simplify $ Plus (Times x y) (Times x z)
simplify (Times (Plus x y) z) = simplify $ Plus (Times x x) (Times y z)
simplify (Neg (Plus x y)) = simplify $ Plus (Neg x) (Neg y)

--reassoc to right
simplify (Plus (Plus x y) z) = simplify $ Plus x (Plus y z)
simplify (Times (Times x y) z) = simplify $ Times x (Times y z)

-- absorb into integer constants
simplify (Plus (ILit x) (ILit y)) = ILit (x + y)
simplify (Plus (ILit x) (Plus (ILit y) z)) = simplify $ Plus (ILit (x + y)) z
simplify (Times (ILit x) (ILit y)) = ILit (x * y)
simplify (Times (ILit x) (Times (ILit y) z)) = simplify $ Times (ILit (x * y)) z
simplify (Neg (ILit x)) = ILit (negate x)
simplify (Signum (ILit x)) = ILit (signum x)
simplify (Abs (ILit x)) = ILit (abs x)







-- sort.. uses commute and dist
simplify (Times y (ILit x)) = simplify $ Times (ILit x) y
simplify (Times x (Times (ILit y) z)) = simplify $ Times (ILit y) (Times x z)
simplify (Plus y (ILit x)) = simplify $ Plus (ILit x) y
simplify (Plus x (Plus (ILit y) z)) = simplify $ Plus (ILit y) (Plus x z)
-- simplify (Times (Lit x) (Lit y))


-- collect 


-- catch all cases. recurse
simplify (Plus x y) = Plus (simplify x) (simplify y)
simplify (Times x y) = Times (simplify x) (simplify y)
simplify (Neg x) = Neg (simplify x)
simplify (Signum x) = Signum (simplify x)
simplify (Abs x) = Abs $ simplify x
simplify (ILit x) = (ILit x)
simplify (Lit a) = Lit a


simplify' x = let y = simplify x in if x == y then y else simplify' y

-- forget?
interp :: Num a => FreeNum a -> a
interp (Plus x y) = (interp x) + (interp y)
interp (Times x y) = (interp x) * (interp y)
interp (Neg x) = negate (interp x)
interp (Abs x) = abs (interp x)
interp (Signum x) = signum (interp x)
interp (Lit x) = x
interp (ILit x) = fromInteger x

deriv :: Eq a => FreeNum a -> a -> FreeNum a
deriv (Plus x y) z = (deriv x z) + (deriv y z) 
deriv (Times x y) z = (deriv x z) * y + x * (deriv y z) 
deriv (ILit _) _ = ILit 0
deriv (Neg x) z = Neg (deriv x z)
deriv (Abs x) z = (Signum x) * (deriv x z)
deriv (Signum _) _ = ILit 0
deriv (Lit x) y | x == y = ILit 1
                | otherwise = ILit 0

deriv' x z = simplify (deriv x z)

type SOP a = [(Integer, [a])]

toSOP x = toSOP' $ simplify' x 

-- did i do these right? Very hard to know
toSOP' :: FreeNum a -> SOP a -- [(Integer, [a])]
toSOP' (Plus (Times (ILit n) x) y) = (n, toP x) : toSOP' y -- SOP $ simplify' x 
toSOP' (Plus t@(Times (Lit _) _) y) = (1, toP t) : toSOP' y
toSOP' (Plus (ILit n) y) = (n, []) : toSOP' y
toSOP' (ILit n) = [(n, [])]
toSOP' (Lit x) = [(1, [x])]
toSOP' (Times (ILit n) x) = [(n, toP x)]-- SOP $ simplify' x 
toSOP' t@(Times (Lit _) _) = [(1, toP t)]
toSOP' _ = error "unexpected form in toSOP'"

toP (Lit x) = [x]
toP (Times (Lit x) y) = x : (toP y)
toP _ = error "unexpected form in toP"

fromSOP :: SOP a -> FreeNum a
fromSOP [] = ILit 0
fromSOP ((n, []) : xs) = Plus (ILit n) (fromSOP xs)
fromSOP ((n, t) : xs) = Plus (Times (ILit n) $ foldr1 Times $ map Lit t) (fromSOP xs) -- foldr1 Plus $ map (\(i, t) -> Times (ILit i) $ foldr1 Times $ map Lit t) xs

-- should just derive a num instance for SOP and then interpret FreeNum into it.

-- SOP is to FreeNum as List is to FreeMonoid
-- a rearrangement using the assumed laws.

{-

bohm-berarducci is a first class pattern match.


x -> ( -> r) -> ( -> r ) -> r is kind of like the metric?
x<A,B,C> -> 

    
( -> r) -> r -> r -> x 
a consumer and a producer

deriv
data types a la carte
-- basically fmap -- subst :: Map a b -> FreeNum a -> FreeNum b
infinte series reps of sin / etc
substpartial :: Map a (FreeNum a) -> FreeNum a 

join :: FreeNum (FreeNum a) -> FreeNum a
join (Lit x) = x
join (Plus x y) = (Plus (join x) (join y))
join (Times x y) = (Times (join x) (join y))
join (Times x y) = (Times (join x) (join y))


Num ~ Ring (if we include all identites)
FreeRing ~ Polynomials
Listlike reps of polynomails -- 


data NumF a = Plus a a | 
type FreeNum = Free NumF
data FracF a = 
data FreeFractional = Free (FracF :+: NumF) 
pattern FPlus 
pattern 
instance Num FreeNum where
   (+) = Plus
   (-) = Minus
   (*) = Times
   negate = Neg
   fromInteger = ILit

sop :: FreeNum -> FreeNum
sop (Plus x y) = (sop x) + (sop y)
sop (Times (Plus x y) z) = sop (x * z + y * z)

deriv :: Eq a => FreeNum a -> FreeNum a
deriv (Plus x y) = (deriv x) + (deriv y) 
deriv (ILit _) = 0
deriv (Times x y) = (deriv x) * y + x * (deriv y)
deriv 

-- hmm is we made deriv a logic program?
integrate :: FreeNum -> Maybe FreeNum -- it tries.



simplify ::
simplify ()


newtype Sum a = Sum [a]
newtype Prod a = Prod [a]
newtype SOP a = Sum (Prod a) -- or 

soptofreenum :: 


doubneg :: FreeNum -> FreeNum
doubneg (Neg (Neg x)) = doubneg x

combineLit :: 
combineLit (Times (ILit n) (ILit m)) = ILit (m * n)


divmod ::

sortterms :: Ord a => FreeNum a -> FreeNum a -- sort terms based on order?

sortprod :: Ord a => -- puts terms in order
-}