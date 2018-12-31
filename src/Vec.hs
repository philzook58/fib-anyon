module Vec where
import Linear.Epsilon (nearZero)
import qualified Data.Map.Strict as Map
import Data.Complex

-- http://blog.sigfpe.com/2007/03/monads-vector-spaces-and-quantum.html


data W b a = W { runW :: [(a,b)] } deriving (Eq,Show,Ord)
instance Semigroup (W b a) where
  (W x) <> (W y) = W (x <> y)
instance Monoid (W b a) where
  mempty = W mempty 
mapW f (W l) = W $ map (\(a,b) -> (a,f b)) l
instance Functor (W b) where
    fmap f (W a) = W $ map (\(a,p) -> (f a,p)) a

instance Num b => Applicative (W b) where
  pure x = W [(x,1)]
  (W fs) <*> (W xs) = W [(f x, a * b) | (f, a) <- fs, (x, b) <- xs] 
  -- the fs is a a vector of rows kind of.
    -- error "I don't feel like it"

instance Num b => Monad (W b) where
   return x = W [(x,1)]
   l >>= f = W $ concatMap (\(W d,p) -> map (\(x,q)->(x,p*q)) d) (runW $ fmap f l)

a .* b = mapW (a*) b

instance (Eq a,Show a,Num b) => Num (W b a) where
     W a + W b = W $ (a ++ b)
     a - b = a + (-1) .* b
     _ * _ = error "Num is annoying"
     abs _ = error "Num is annoying"
     signum _ = error "Num is annoying"
     fromInteger a = if a==0 then W [] else error "fromInteger can only take zero argument"

collect :: (Ord a, Num b) => W b a -> W b a
collect = W . Map.toList . Map.fromListWith (+) . runW

trimZero = W . filter (\(k,v) -> not $ nearZero v) . runW
simplify :: Ord a => Q a -> Q a
simplify = trimZero . collect
-- filter (not . nearZero . snd)

type P a = W Double a

type Q a = W (Complex Double) a

star :: Num b => W (Complex b) a -> W (Complex b) a
star = mapW conjugate