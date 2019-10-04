# 2Vect in Haskell - Linear algebra of types, 2-Vect, 


## Linear Algebra of types

It gives my brain a pleasant thrum to learn new mathematics which mimics the algebra I learned in middle school. Basically this means that the system has operations with properties that match those of regular numbers as much as possible. Two pretty important onces are addition and multiplication with the properties of ditrbutivity and associativity. Roughly this corresponds to the mathematical notion of a Ring.

Some examples include
  - min-plus https://en.wikipedia.org/wiki/Tropical_semiring
  - matrices 
  - types

I have talked before about how types also form a semiring, using Either for plus and Tuple for times. These constructions don't obey distributivty or asscociativty "on the nose", but instead are isomorphic to the rearranged type, which when you squint is pretty similar to an equality. I wrote a post on this topic here.

 Matrices are grids of numbers which multiply by "row times column". You can form matrices out of other rings besides just numbers though. One semitrivial but interesting example is block matrices, where the elements of the matrix itself are also matrices. Any matrix can be though of as a block matrix by considered submatrices of the big matrix.

What if we put our peanut butter in our chocolate? We can consider typelevel matrices of types, using the algebra of types semiring.
 
Maybe we do need context of different methods. for vectors. 

The simplest implenentation to show how this could go is using the naive list based implementation of matrices. We can directly lift this representation to the typelevel and the appropriate value-level functions to type families.




### What is the point

Ok. That's kind of neat, but why do it?

Well, what are matrices useful for anyway?
One thing they are useful for is describing transition systems. You can write down a matrix whose entire a_{ij} describes something about the transition from state i to state j. For example the entry is the cost of getting from i to j, the count of ways to get from i to j, the probability of transition from i to j, or the quantum amplitude of going from i to j if we're feeling saucy. If we form a matrix describing a single time step, then multiplying this matrix by itself gives 2 time steps and so on.

One way to fifting this notion to types, we can build a type exactly representing all the possible paths from i to j.

Concretely, consider the following transition system:
You are going between home and work. Every 1 hour period you can make a choice to do a home activity, commute, or work. There are different options of activities at each.

```haskell
data Commute = Drive
data Home = Sleep | Eat
data Work = TPSReport | Bitch | Moan
```

This is described by the following transition diagram

What is the data type that describe all possible 4-hour day?
10 * (Travel + StayHome + Work) is an overestimate. You can't magically `Sleep` at home then appear at work and `Moan` immediately. You have to commute first.



The transitions are decribed by the following matrix.
type T = '[ '[StayHome,  Commute ],
            '[Commute,   Work    ]]

type FourHour = MM T (MM T (MM T T))

There are alternative formulations rather than using type = 'V2 HomeCHoice WorkChoice ?

Now, time to come clean. I don't think this is necessarily the best way to go about this problem. There are alternative ways of representing it.

Here are two data types that describe indefinitely 
data HomeChoice = Stay StayHome HomeChoice | Go Travel WorkChoice
data WorkChoice = Work Work WorkChoice | Go Travel HomeChoice

Another style would hold the current state as a type parameter in the type.
data Path state where
   StayWork :: Work -> Path Work -> Path Work
   CommuteHome :: Commute -> Path Home ->  Path Work
   StayHome :: Home -> Path Home -> Path Home 
   CommuteWork :: Commute -> Path Work ->  Path Home

And then by analogy fron Vec n to [] we could require the lists to be of a particular length.

-- partially apply the state parameter and refactor into two data types.
data PathWork n where
   StayWOrk :: PathWork n -> PathWork
   GoHome :: PathHome n
   Nil :: PathWork 0
data PathHome n where
   Nil :: PathHome 0

List and Vec n, Vec n is in a sense way more challening.
data Path n state where
   StayWork :: Path n 'Home -> Path ('S n) 'Work
   TravelHome :: Path n 'Work -> Travel ->  Path ('S n) 'Home
   StayHome :: Path n 'Home -> Home -> Path 'Home 
   TravelWork ::

## Linear Algebra of Vector Spaces

It is actually a fairly short jump from the algebra of types to considering the linear algebra of vector spaces. And I mean the spaces, not the vectors themselves. This is a confusing point.

Vector spaces have also have two natural operations on them that act like addition and multiplication, the direct sum and kronecker product. These operations do form a semiring, although again not on the nose. The vector space (A \oplus B) \otimes C is isomorphic to a \otimes c \oplus b \otimes c, meaning there is an invertible linear transformation going between the two spaces.

This is simply connected to the above algebra of types picture by considering the index types of these vector spaces. A 2D Vector space has the index type Bool. The simplest way to denote this in haskell is using the free vector space construction as hown in this Dan Piponi post.

type Vec b r = [(b, a)]
type V2D = Vec Bool Double

Then index type of the kronecker product A \otimes B is the tuple of the indivudal indices (a,b), and the index of the direct sum is the (Either a b).
The dimensionality of the vector space is the total count of possible indices, and hence the dimensionality of the kron of two spaces is the product of their dimensionalities, and the dim of the direct sum is the sum of their dimensionalities.

kron :: Num r => Vec a r -> Vec b r -> Vec (a, b) r
kron xs ys = [((x,y),r1 * r2) | (x, r1)<- xs, (y, r2) <- ys]

dsum :: Vec a r -> Vec b r -> Vec (Either a b) r
dsum xs ys = [(Left a, r) | (a,r) <- xs] ++ [(Right b, r) | (b,r) <- xs]  

Before we considered putting types into matrices. What if we put vector spaces into matrices? Going down this road (plus a couple layers of mathematical sophistication) leads to concepts known as 2Vect or Voevodsky-K vector spaces (yes, that voevodsky).



### Functor, Vectors, and String Diagrams
Nix this. It is a seperate blog post. And the Quantum Fourier Transform

Is category theory even good?

In this post, I am going to explain some things that are very concrete and some thngs that are very abstract. That is how I am.

I think the concrete things are necessary to be sure the abstract things mean anything at all. 

The two core runnign examples of this post are Haskell Functors (think containers list list), and Vector spaces. These two interleave and even pun on each other, which is both confusing and explaining. Hopefully mostly the latter.

I personally find something that tickles me about category theory. I'm not necessarily convinced that it sufficiently useful and intuitive that it is morally correct for me to prosletiyze it though.
The going seems rather slow and it is not uncommon for mathematicians to have an asethetic that does not coincide with my own. I like things simple.
Sometimes I am not ready to understand what they are saying. Sometimes they are just delusional, bad communicators, or full of shit. It is hard to tell what situation you're in.


My simple is insanely arcane to others though. It is all subjective.

Post structure
Functor composition
Kronecker product
Vector as shapes / functors / structs
Monoidal categories
String diagrams

What is the central message: Well, that I'm smart of course
What are monoidal categories


## Monoidal Categories
\What are monoidal categories?
Monoidal categories are categories first off with a little monoidal spice. 
Categories are the combo of a collection of objects and arrows between the objects. The arrows can compose as long as the head of one is on the same object as the tail of the other. On every object, there is always an identity arrow, which when composed with do nothing.



Examples of monoidal catoegires
 - Vect - vector spaces = objects, morphisms = matrices, composition = matrix multiplication
 - Haskell - morphisms = function values, objects = types, 
 - Set
 - Rel


One way of thinking about it is that monoidal categories have ordinary composition and some kind of horizontal composition, putting things side to side. Ordinary composition is often doing something kind of sequentially, applying a sequence of functions, or a sequence of matrices. The horizontal composition is often something parallel feeling, somehow applying the two arrows seperately to seperate pieces of the system.

Let us quickly get to an example.

### Monoidal Products on Hask

Hask is a name for the category that has objects as Haskell types and morphisms as Haskell functions. It's a curious mixing of type/value layers of Haskell. The objects are types whereas the function morphisms are Haskell values.

Composition is given by `(.)` and the idneitty morphisms are given by `id`

For Haskell, you can compose functions, but you can also smash functions together side by side. These combinators are held in Control.Arrow

You can smash together types with tuple `(,)` or with `Either`. Both of these are binary operators on types. The corresponding mapping on morphisms are given by `(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)` These are binary operators on morphisms that play nice with the composition sturcture of Haskell.

### String diagrams

String diagrams are a graphical notation for monoidal categories. Morphisms are denoted by boxes.
Composition is shown by plugging arrows together. Monoidal product is denoted by putting the arrows side to side.

When I was even trying to describe what a monoidal category was, I was already using language evocative of string diagrams.

You can see string diagrams in the documentation for the Arrow library. Many diagrmas that people use in various fields can be formalized as the string diagrams for some monoidal category. This is big chunk of the field known as Applied Category Theory.

### Why are they called Monoidal?
A monoid is a binary operations that is associative and has an identity.

Sometimes people are more familiar with the concept of a group. If not, ignore the next sentence. Monoids are like groups without requiring an inverse.

Numbers are seperately monoids under both addition, multiplication and minimization (and more), all of which are associative operations with identity (0, 1, and infinity respectively).
Exponentiation is a binary operation that is not a monoid, as it isn't associative.

The Monoid typeclass in haskell deonstrate this
http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Monoid.html

```haskell
class Semigroup a => Monoid a where
        mempty  :: a
        mappend :: a -> a -> a

```

A common example of a monoid is list, where mempty is the emtpy list and mappend appends the lists.

There are different set-like intuitions for categories. 
One is that the objects in the category are big opaque sets. This is the case for Hask, Rel, Vect.
A different intuitiion is that the category itself is like a set, and the objects are the elements of that set. There just so happens to be some extra structure knocking around in there: the morphisms. This is the often more the feel for the examples of preorders or graphs.
In this second intuition, we want to turn our notion of a monoid on the objects into a 
The word "monoidal" means that they have a binary operation on the objects. But in the category theory aesthetic, you also need that binary operation to "play nice" with the morphisms that are hanging around too. Functors are the first thing that has something like this. It has other properties that come along for the ride. A Functor is a map that takes objects to objects and arrows to arrows in a nice way. A binary functor takes two objects to and object, and two arrows to one arrow in a way that plays nice (commutes) with arrow composition.

A mix of the two is considering the powerset of a set. Now every object is a set, the arrows are set inclusion.

There is funny game category people play where they want to lift ideas from other fields and replace the bits and pieces in such a way that the entire thing is defined in terms of categorical terminology.

### Haskell Functors as a Monoidal Category

Functors in Haskell are a typeclass for containers. They allow you to map functions over all the items in the container. They are related to the categorical notion of functor. I'll try to denote them seperately by capitlaizing the typeclass word Functor. Really, there are interesting depths to be plumbed by blurring and unblurring these lines in your head.

```haskell
:kind Maybe ==> (* -> *)
:kind []   ==> (* -> *)
type Container = * -> *
```

Natural transformations are morphisms between functors. A natural transformation in Haskell looks like
```haskell
type f ~> g = forall a. f a -> g a
```
Because of this polymorphic type signature, these mappings can only rearrange, copy and forget items of their contained type `a`.

You can lift the product and sum of types to the product and sum of Functors. You can denote this with a newtype wrapper.

Functors also have a natural notion of multplication and addition, which you may find in Data.Functor.Product and Data.Functor.Sum. These are very natrual liftings of plus and times of ordinary types to types with a hole. This is analogous to the lifting of ordinary addition and mutplication to the addition and mutlpication of polynomials, which are "numbers with a hole".

```haskell
newtype Product f g a = Product (f a , g a)
newtype Sum f g a = Sum (Either (f a) (g a))
```

Functors compose. A container of containers of `a` is still a container of `a`. We can form composite containers by using the Compose newtype wrapper. 

```haskell
newtype Compose f g a = Compose (f (g a))
```

When you use this Compose newtype, instead of having to address the individual elements by using fmap twice, you can use fmap once.
fmap . fmap = getCompose . fmap . Compose or something

Product, Sum, and Compose are all binary operator on functors. The type constructor has kind

```haskell
{- Enter into ghci -}
:kind Maybe ==> (* -> *)
:kind Compose ==> (* -> *) -> (* -> *) -> (* -> *)
:kind Product ==> (* -> *) -> (* -> *) -> (* -> *)
:kind Sum ==> (* -> *) -> (* -> *) -> (* -> *)
```

There is a category where Functors are objects and morphisms are Natural transformations.
This confusing because Functors really feel morphism-y. They are mappings. And there is a way of reconciling what we're doing here with that impulse, which is to consider the 2-Category Cat, but we'll leave that for another day.

Functor composition obeys the laws necessary to be a monoidal functor on the Funct category.

How it works is kind of neat. Because the natural transformations require polymorphic types, when you apply `ntf` to `fg` the polymorphic variable `a` in the type of `ntf` restricts to `a ~ g a'`.  

```haskell
mon_prod :: Functor f' => (f ~> f') -> (g ~> g') -> (Compose f g ~> Compose f' g')
mon_prod ntf ntg (Compose fg) = Compose (fmap ntg (ntf fg))
-- or equvalently Compose (ntf (fmap ntg fg)) with a (Functor f) typeclass requirement.
```

A monoidal category also has unit objects. This is given by the Identity functor
```haskell
newtype Identity a = Identity a

rightUnitor :: Functor f => Compose f Identity a -> f a 
rightUnitor (Compose f) = fmap runIdentity f

rightUnitor' :: f ~> Compose f Identity
rightUnitor' = Compose . fmap Identity

leftUnitor' :: f ~> Compose Identity f
leftUnitor' = Compose . Identity

leftUnitor :: Identity *** f ~> f
leftUnitor = runIdentity . getCompose
```

There is also a sense of associativity. It is just newtype rearrangement, so it can also be achieved with a `coerce` (although not polymorphically?).

```haskell
assoc :: Functor f => ((f *** g) *** h) ~> (f *** (g *** h))
assoc = Compose . (fmap Compose) . getCompose . getCompose

assoc' :: Functor f =>  (f *** (g *** h)) ~> ((f *** g) *** h)
assoc' (Compose x)  = Compose $ Compose $ (fmap getCompose x) 
```

Similarly, we can define a monoidal category structure using Product or Sum instead of Compose.

### Vector Spaces as Monoidal Categories

Vect is a category with vector spaces as objects and linear maps as morphisms. It has a similar flavor to Hask, where objects are types and morphisms are functions between them.
Vect is a monoidal category in at least two ways.

The most commonly used way is using the Kronecker product as the monoidal product. If you gave me an n and m dimensional space, I can give you back the (n*m) dimensional space of their Kronecker product.

This is a rather abstract operation. What kind of "thing" is a vector space? How can I hold that in my hand? I feel a little bit more like I could hold a vector, it is a little arrow, but the space?
I can take the krnoecker product of two arrows, but this is not what I mean.

```python
vw = np.kron(v, w)
print(vw.shape)
```


It helps me a little bit to concieve that this operation can be repsented in the type system. The type system is a rather abstract place too. At run-time, there isn't really a thing that represents the type strongly either. It is an ephemeral intent.

Because this thing should be a functor, if you gave a me a (pxn) matrix  and (k x m) matrix, I can form a (pk x mn) matrix going between the respective kronecker product spaces.

This operation does actually live in the value level

## The Kronecker product

The Kronecker product gives you a way of naturally lifting naturally lifting small matrices to much larger ones. It is a natural linear algebraic combinator.

I like to think there are 3 sizes of vector spaces:
 - Constant size - O(1). These are often 2,3,4 dimensional vectors with geometrical meaning.
 - Polynomial Sized - O(n^d). These include the grid like tabulations of field values like velcity or temperature that occur in ordinary physical simulations.
 - Exponentially sized (2^n). These include quantum state vectors and probability distributions over many variables.

To convenitely construct vectors and linear operations of the second and third type, it is very convenient to introduce higher order operations that take matrices and lift them into context.

The place I use it the most is when defining linear operations on 2D grids.

If K is the finite difference matrix corresponding to the second derivative, then K\otimes I is the matrxi for \partial_x^2 and I \otimes K is the matrix for \partial_y^2.

It's a quick and clean way to build and think about lifting 1 dimensional constructions to 2 or 3 dimensions.

It is also important for describing multi-piece quantum mechanical or probability ditrbituions. 

The joint transfer matrix is T_1 \otimes T_2.

```python
import numpy as np
N = 10 # We're making a 10x10 grid
row = np.zeros(N)
row[0] = -2
row[1] = 1
K = np.toeplitz(row,row)
I = np.eye(N)
K2 = np.kron(K,I) + np.kron(I,K)
```

Once you've got K2, you can solve the laplace equation by using your linear algebra packages.

When I'm writing code like the above, what am I thinking? I don't think I'm directly contemplating some hard core lowlevel definition of the kronecker product. I'm thinking that it is a way to lift operations naturally into a larger space. What do I mean by that? That somehow this lifted operation is doing the same thing as the original. What do I mean by that? Well, I think what I mean is that I could apply my matrices, then take the outer product, or I could .
(`@` is matrix multiplication btw)


```python
np.kron(A,B) @ np.kron(v,w) == np.kron(A @ v, A @ w)
```

The second is much, much faster, btw.

A framework well built to consider such a condition is that of a Functor between categories. We have a commutiativity condition, which 

Python and numpy in particular are excellent. They let me express succintly and easily rather complex computations.

However, python does not even have an acceptable language in which to formulate some concepts. I think you're still thinking them in some kind of cloudy way.

## Vector Space as Shape / Structs

There are many ways to represent vectors in Haskell, but one of my favorites is Kmett's linear package.
A 4-dimensional vector look like this

```haskell
data V4 a = V4 a a a a
```

A type of kind * -> * can be thoguht of as vector module its held scalar type.
type Vect = * -> * 

The stadard Haskell typeclass hierarchy gives you some of the natrual operations on 
- Functor ~> Scalar Mutiplication
- Applicative ~> Vector addition
- Traversable ~> Tranposition. sequenceA has the type of transposition and works correctly for the appropriate.

The linear library does use Functor for scalar multiplication, but defines a special typeclass for addition `Additive`.

The higher rank kind for vector gives an explciit slot to place the scalar type.

The linear library implements Num, for many of their types. WHich makes sense from a pragmatic point of view, or a array processing point of view, it doesn't feel quite right to me from a linear algerba point of view.


Vector spaces are made of two parts, the shape of the vector space and the scalar.

Before I've spoe about index-ful approaches, like free vector spaces or (Vec n) where we tend to vaguely think about the index type Index = * as denoting the shape, a as the scalar and some type Vec gluing it together. 
However, it seems to jive well with the haskell aesthetic (as we'll show) to consider the shape `type Vect = * -> *` as the vector space. You can transform from the index form to this shape form by partially applying the Vec type.


The following is the entire content of this post.
```haskell
type Kron = Compose
type DSum = Product
```

The functor composition of vector functors gives the kronecker product, and function product gives the direct sum.

What do linear operators look like?
It tickles me to try and pun as much as possible on the Haskell facilities.
Because in the Vectors as shape methodology, Vectors look very much like Functors, we can try to bring our designs in alignment.



type LinOp f g a = f a -> g a)
newtype LinOp' f g a = LinOp' (f (g a))


I have been tempted to lift the natural transformation type above to the following for linear operators.

`type LinOp f g = forall a. (Num a) => f a -> g a`

In a sense this works, because many of the container type (V1, V2, V3, etc) in the linear package implement Num.
However, choosing Num is a problem. Why not Fractional? Why not Floating?
We are also failing to enforce the cohesion of the scalar type.
So the analogy fails I think.

We don't really want to lock away the scalar in a higher rank polymorphic type. We want to ensure that everyone is working in the same scalar type before allowing things to procede.

`type LinOp f g a = f a -> g a`

We can form the kronecker product of vectors.

```haskell
kron :: (Num a, Functor f, Functor g) => f a -> g a -> Kron f g a
kron f g = Compose $ fmap (\amp1 -> fmap (\amp2 -> amp1 * amp2) g) f
```

But can we form the kronecker product of linear operators? Yes. But you're not gonna like it.

```haskell
-- type sig not correct
kron'' :: ( Num a, Functor f, ?) => (f a -> f' a) -> (g a -> g' a) -> (Kron f g a -> Kron f' g' a)
kron'' lf lg (Compose fga) = Compose $ sequenceA $ (fmap lf) $ sequenceA  $ (fmap lg fga)

-- Yeah. This kind of make sense. Rearrange the structure to give access to the scalar
```

This was a head scratcher for me. Follow the types, my friend! I find this particularly true for uses of sequenceA. I find I want the containers swapped in ordering. In that situation sequenceA is usually the right call. It could be called transpose.

There is another possibility for what morphisms should be
```
type LinOp1 f g a = forall k. Additive k => Kron f k a -> Kron g k a
type LinOp1' f g a = forall k. Functor k => Kron k f a -> Kron k g a
```

By a Yoneda transformation analogosu to (a -> c) -> (a -> c)
or 

What we're suggesting is very evocative of a Yoneda-ified/CPS form of a matrix. I don't know if I can show this literally. In those forms, you can often sort of spread the pieces out of an object around some (->) as long as your have some polymorphism to bind them together.



forall k. Additive k? Metric ? => Kron f k a -> Kron g k a  ~~~ Kron f g a which is the ordinary matrix form of a linear operator.

to :: (Kron g f a)  -> (Kron f k a) -> Kron g k a)
to = matrixm utiply

from ::  -> Kron f g
from = Use V1. Use complete basis.
We'd also need this for getting back out (f a -> g a) form

Does this form actually enforce linearity? You may still rearrange objects. Great. You can also now add and scalar multiply them. The issue with the Num a typeclass is we could perform nonlinear operations. liftU2 and liftI2 are powerful operations, so you can still bust out, but I think if we restricted Additive to just a scalar mutiply and vector addition operation, and zero, we'd be good. Funtor is also too strong and allows arbitrary manipulation of 

```haskell
class Vector f where
   smul ::
   vadd ::
   zero ::

instance Additive f => Vector f where
   smul = (*^)
   
```



#### 

What does matrix multiplication look like?

What does kron of spaces look like?

What do kron of vectors look like?

kron of operators?
kron :: LinOp f g a -> LinOp f' g' a -> LinOp (Kron f f') (Kron g g') a
kron (LinOp a) (LinOp b) = LinOp \(Compose v) -> 


Because the type has a slot for the 

Instead the library has the Additive typeclass to 





type Vect = * -> * -- kind

Vector spaces modulo their scalar type are containers.

The generic capabilties of Haskell ease the burden and we can build big vector spaces in this way more conveniently.






Index free methods are often considered to be more elegant in many respects to indexful formlations. 



type Index = * -- kind
type Vect = * -> * -- kind

We can use the above styles in this way by partially applying an index to the vector.
But we can also make new intrinsics that can't be thought of in the other way.



Containers that are basically big product types are also known as representabe/naperian/or logarithmic. 
Representable places emphasis on the isomorphism between such a container type and the type `(->) i` which by the algebra of types correspond is isomoprhic to a^i (i copies of a). 
They are called Naperian/Logrithmic because there is a relationship similar to exponentiaion between the index type a and the container type f.
If you take the Product f g, this container is indexed by (a + b) = Either a b. In a sense Compose f g is indexed by the product (a,b). (f r) ~ r^a
The arrow type is written as an expoential b^a because if you have finite enumerable types a and b, that is the number of possible tabulations avaiable for f.
The Sum of two representable functors is no longer representable. Log(f + g) does not have very good identities associated with it.

I used to think that pure product types (Naperian/representable) were vectors, but there is a reasonable interpetation for container types with sum types in them. These can be thought of as subspaces, different bases, or as choices of sparsity patterns.

-- two bases at 45 degrees to each other.
data V2_45 a = XY a a | XY' a a
data Maybe a = Just a | Notinhg -- a 1-d vectro space with a special marker for the zero vector.
data Maybe2 a = Just2 a a | Nothing2


The names direct sum and direct product are most easily seen the make sense by looking at the algebra of the indices rather than the containers.
The direct product has a dimensionality that is the product of the idnvidiual dimensionality and the direct sum has



Non-naperian functors as sparse representations/ subspace reprenetations. Values like Nothing let you denote the 0 vectors.

The stock definition of Applicative for list [] does not perform vector addition. This demonstrates that the standard tyepclass hierarchy does not necessarily correspond to the correct operations, which is too bad.
The standard typeclass hierarchy is also too strong. Ideally we would want to limit ourselves to only linear operations, whereas the standard typeclasses make no such restriction.
It is basically a hack.



We could instead make a new typeclass hierarchy explicitly for the purpose. 

## 1-D vector spoace ~ Scalar
Maybe this belongs in the 2Vect post
It is useful to pay attention to the trivial stuff.

## Cup and Cap

# Linear Functors


```
-- type V4' :: Vect -> Vect
type V4' f a = Kron V4 f a
```



## The Quantum Fourier Transform

Qubits
```haskell
type QuBit a = V2 a
```

The fourier transform is one of the most important algorithms of all time.

As a warmup, here is an implementation

class FFT f where
instance FFT (V2 ) where
   twiddle = V2 (twiddle) (- twiddle)
   fft (V2 x y)= let x' = fft x
                 let y' = fft y
   
point-free


### Bits and Bobbles

- The vector wrapper and the index type are adjoint.
- Representable functors, Naperian Functors
- More string diagrams - see Art and Dan explain an old trick
- Vectors as structs - 


### References
https://github.com/NickHu/naperian-functors
https://hackage.haskell.org/package/naperian
This Vicary book is great
Coecke book
Marsden talk
Bartosz string diagrams
Naperian Functor Gibbons
Appendix A of compiling to categires paper.
Conal Elliott Parallel.
https://www.researchgate.net/publication/333918631_Quantum_Computing_in_Haskell


### Vect trash

It's confusing because You kind of want objects to be categories and functors to be morphisms. And this is acceptable if you're thinking of things being a 2-category.

If you're trying to categorify something in Haskell, polymorphism is a smell that can indicate a natrual transformation.

Cat is also a monoidal category. In Cat, the objects are categories and the morphisms are functors. There are a couple monoidal products available, but a common one functor composition.

Functor oriented programming

Speciazation of type is a subtle operation.

An N-dimensional vector is something that hold N numbers somehow. Often this means an array. But also we could use structs. This does not feel so odd for the O(1) vector spaces, but does feel unusual for the larger spaces.
The reason for that is structs need to have a name for every entry in order to get the pieces you want. It feels far too burdensome to do this for a 100 entry struct.

There are structs/records available in basically every languages.

Pure product functors correspond to structs in other languages. It seems conceivable that one could write in this style in C, C++, Rust with metaprogramming help. 

Why go through these insane abstract mathemtical simplifications when it all boils down to just a big pile of numbers and structs?
The concrete make the abstract understandable and the abstract simplifies the concrete.

```
struct v4 {
   x :
   y :
   z :
}
```


I really find something rather tickling about the Linear style of vector.
A type of kind * -> * can be thoguht of as a container. When you apply a thing of that kind to the type it is holding, you get back the netire container type. Familiar examples include Maybe, and []. Container have shape. A Vector space is also commonly thought of as a container of numbers of a certain shape. By abstractly using types of kind * -> *, we avoid using labelling vector spaces only by the dimension. 
As pure shape, a concept of kind * -> * is a vector space without a particular scalar. 
If we abstract out the particular scalar involved in a vector space, it feels to me that this is roughly the same concept.



A type of kind * -> * can be thought of as a container. Familiar examples include Maybe, and []. Container have shape. A Vector space is also commonly thought of as a container of numbers of a certain shape. By abstractly using types of kind * -> *, we avoid using labelling vector spaces only by the dimension. 
 You wouldn't want to mix semantically incompatibnle vector spaces just because thir dimension just so happened to be the same. Adding an RGB color space to and XYZ geometrical vector space should never happen. Therefore, it is inelegant and confusing ad error prone we shouldn't define them both as (V 3 Double).
 To name every vector space the same just because they have the same dimensionality is the same as saying every flag type should be a Bool. It makes sense, works, but it creates confusion and allows more programs than you actually want to type check
 There are the 3 sizes of vector spaces.
 Vector spaces can be contructed out of others by two combinators,
 the direct sum and the kronecker product.

### Tensors

### Anyons and Topological Quantum Computation

I've written before a couple blog posts on this topic. The thing that even got me into


The Path type above is very much related to my FibAnyon type. The current "state" once you're inside a the tree is a particle variety. 

it's a vector space of paths. anyons contain a remnant histroy of their "path" (particle type history) and their .


While we have been discussing matrices of vector spaces (a grid with two indices/axes), It is not at all inconceivable to dicuss tensors of vector spaces (grids with arbitrary number of indices).
Anyonic vector spaces are an example of such a thing.

Anyonic vector spaces can be constructed 

Tensors really are their own kettle of fish. The standard way of working with them is using indices. Summation symbols \sigma are binding forms that introduce new dummy indces. Dummy indices can undergo alpha renaming. In order to deal with this, we need to do things very analogous to embeddings of lambda calulsu. The same can be said for forall, exists, min, max,

Indexful structures tend to be more easy for humans to read and write for some reason. Thay are more difficult to manipulate correctly.

We can use the lambda binding forms of Haskell as a stand-in for indices, letting them implement the difficulties of renaming and scoping for us. This is known as Higher Order Abstract Syntax (HOAS).

-- tensor defintiion
t 0 0 0 =3.4
t 0 1 0 = 445.43
t 1 0 1 = 453.45

sum :: BEnum a => Vector b / Num b => (a -> b) -> b


Functional Functional Differentiation

data Expr = X | Times E E | Plus E E | Exp E
data Expr = X | Y | Times E E | Plus E E
data Index = Lit Int | Var String
data Expr = J Index | Plus E E | Times E E | Sum String Expr | Exp E 
-- G Index Index | V Index Index Index | 


-- free indices are implcit
diff (J i) = 
diff (Sum i e) = (Plus (Sum i (diff e)) (diff j e) -- rewrite fresh and rewrite

rewrite i j expr = 


data EHoas ind = J ind | Sum (ind -> EHoad) | Prod E E | Sum E E

diff (J x) | x == y = 1
           | otherwise = 0
diff (Sum f) = Plus (Sum \j -> diff (f j)) (f )


If you want to call Sum Integrate, dat's coo
You may way to finally tagless it all
Also a graph lamnguage. Prod + indices



diff' j (J i) | j == i = 1
              | otherwise = 0


point-free form. much easier to differentiate
data Expr = J | Dot Expr Expr | Kron Expr Expr | Plus E E | DSum | Exp E 

## Contravariant and Covariant

Beckham and Meijer "jam session"


### uses of 2-Vect

2-Hilb

It is rare that playing all these goddamn mathemtical shell games ever somehow magically avoids doing the obvious thing. Taking the inner product amounts to Multiplying and summing. Sorry.

The dual picutre let's you fold this operation into the dual vectors.

Density matrices?

### 2-Vect

It is interesting that this problem is partly what got me into Haskell. It has taken me 50 moons to understand the underpinnings of the various subjects to write this post.

It is strange that the work was in Mathemtica to my mind, because typed functional programming is the best arena to computerize categorical concepts. As we'll see, I think it is a reltively clean fit.

Categories are made of objects and arrows between them. They have a notion of composition that produces an arrow given two others if the tail of one and the head of the other meet on an object . There are always identity arrows that when composed with do nothing.
Haskell can be modelled as a category. The objects are types and the arrows are Haskell functions (which are values). 
Vect is a category. Objects are vector spaces (like V2 or V3) and arrows are linear maps.

2-Categories extend this wth that additional axiom that there are also be 2-arrows that go between arrows. These 2-arrows also have anotion of 2-composition that requires them to meet on the same , and there are always identity 2-arrows.

I have less experience with exmaple 2-categories
One example, in a fit a categroucal narcissism, is Cat. In Cat, the objects are categories, arrows are functors, and 2-arrows are natrual transformations.
Another example is 2-Vect, which is fairly bizarre. Vector mathematics is so incredibly useful and intuitive. Basically every fact or method related to linear algebra is useful. Why are there so few uses of 

Why would you want

2-Vect gives a language

The whole reason I care about 2-Vect is it's relationship to anyons, the exotic particles that underly topological quantum computation.

Applications of 2-categories
Stay - cryptography
Vicary - quantum mechanics.

James Vicary and his student Dan Roberts produced a Mathematica package for 2-Vect which is basically incomprehensbile to me. It seems to me that type functional prgoramming is a more appropraute venue for something of this kind of pursuit, given how evocative/ well modelled by category theory it can be.

It may also be related to 


Kind annotations are extremely clarifying.
Because of Type in Type, Types play double duty as kinds and types. This is both convenient and confusing, just like most weakly typed systems. I do not have the syntax necessary to express my intent, and it becomes confusing even for me.

Kind based typeclass dispatch. We can build typeclasses parametrized not on type, but on kinds. This is already seen in the SIngleton libraries lifted version of typeclasses. PNum I believe.

The basic intuition of 2-Vect is that we want to use Vect as the scalar type, with kronecker product and direct sum replacing ordinary times and plus.
This is so abstract, it is difficult to see what the hell this could even mean. I think the situation is actually clarified by the fact that we are doing typelevel manipulations

Roberts and Vicary show that 0-cells are equivlanet to n copies of Vect, which is specified by a single integer speciffying the number of copies.
1-cells are "linear" maps from tuples of vector spaces to tuples of vector spaces. What the hell does linear mean in this case? What the hell does map mean? Well, map means that it is a functor. The mapping maps objects to new objects. The objects in question are particlar n-dimensional vector spaces. Linear means that F(A \oplus B) ~ F(A) \oplus F(B)  and F(\alpha \otimes A) ~ \alpha \otimes F(A).
In particular, these (~) should also be regular linear maps.

That it is difficult to denote the different layers (value, type, and kind) layer at which things are happening, it is very confusing.


### Ways to represent 2-Vect

One needs to select an index or use dot in order to xtract a type that can actually hold a value.

- MV, MM, VV type family. A fairly direct translation. Structures that hold types
   + Typelevel list
   + V3 / V4

- Indexed GADT
   data IndM' :: Nat -> Nat -> Vect where
    IndM00' :: V1 a -> IndM' 0 0 a
    IndM10' :: V2 a -> IndM' 1 0 a
    IndM01' :: V3 a -> IndM' 0 1 a
    IndM11' :: V4 a -> IndM' 1 1 a
- Written out newtype wrappers. Very similar to GADT execept indices are fused opqauely into the type 
  + newtype V01 = V01 (V4)
- Dual Form newtypes
  + newtype DV2 a b = DV2 ((3 * a) + (2 * b)) -- Vect = *
  + newtype DV2 a b r = DV2 ((V3 * a) + (V2 * b) r ) -- Vect = * -> *

The dual form does not require pattern matching to use and is better suited for the contraints of typelevel programming.


structural isomorphisms of vector spaces.
V2 (V3 a) ~ (V3 (V2 a))

### Ways to do vector spaces


### Bits and Bobbles
  - string diagrams



### A Semicoherent Appendix

You ever write a bunch of shit and then feel too lazy to polish it? Not me. I wonder what that is like. Must be awful.

### Representation of Vector in Haskell

For every formulation of vectors in value level Haskell, there is a typelevel equivalent for these vector space manipulations.

There are lots of ways to encode Vectors into Haskell. There is a tendency to mentally box yourself in to considering only vectors backed by fast low level arrays. It is not clear that these shackles are always warranted. Interface and implentation often should be held more seperate. For my toy concern, anyonic vector spaces, performance is not the overriding factor. I do not have a particular problem in mind, rather I am enjoying just playing with the system.


It is basically ubiquitous to represent the type of the scalar (Double, Int, etc) as a parameter in the type somewhere.

(A side note: I'm basically working in modules, a superset of vectors. Modules are basically vector spaces where the scalar is a ring rather than a field, so it doesn't necessarily need a division operation. Vectors are more commonly used in engineering and physics, modules being largely a subject of discussion for pure mathemtics. I see no reason today to invoke the more obscure name. )

Untyped Vectors
  + `[a]`
  + `Vector a`
  + HMatrix - Closest scipy equivalent
  + Massiv - 
  + Repa -
  + Accelerate - A DSL that compiles via LLVM to GPU or CPU

The list type as a vector is ridiculous from a performance point of view, and yet it gets at the thrust of the capabilities and is writeable in the normal Haskell data type facilities. It is not an uncommon exercise to write a matrix tranpose for lists.

Typed Spaces - These are Vector spaces whose shape is described by the type. Doing things in this way is good from a correctness point of view and from a type drvien design point of view. When one thinks abstractly of vector spaces, I basically think of the dimension, or of what each component represents.


  + Nat Typed
     - `Vec 7 a`
     - Justin Le. Newtype wrapper on Array

The boolean blindness spiel. 
Giving an explicit type the index of the vector space is convenient. The Nat typed vector spaces have a reputation of being more trouble than they are worth, because you have to perform confusing manpipulations to desmntrate simple mathemtical facts to the type system. I find that the index typed don't feel quite the same pain from a psychologoical standpoint. The manipulations do not feel necessarily as obvious and therefore it doesn't feel as exasperating to explain them to the compiler.

They are fairly natural sparse vector types.
The function type is interesting because it works with the primordial soup of functional programming and because it let's you efficiently represent very large vectors so long as you have a good rule for computing them. They become less efficient the more operations you apply however. Caching / Memoizing can help. See Conal Elliott.
Functions are vector occurs in the theory and computation of PDEs (quantum mechanics for example). \psi(x) = sin(n*x / L) is faithfully representable by the Haskell code `psi x = sin (n * x / L)`.

There is an analogy between Nat indexed vectors and the type indexed vectors that is a lifting of the style of describing the naturals as a Peano system vs as the free semiring. (0, 1, plus, times). Rather than the successor operation, type mutiplication and type summation on the index types correspond to the kronecker product and direct sum of vector spaces.



### Index Typed   
    - `b -> r`
    - `Map b r`
    - `[(b,r)]` - See Piponi / My article
    - Conal Elliott's thing?


### Opaque Functor / Index Free 
     - Kmett's Linear
    `data V4 a = V4 a a a a`

Index free methods are often considered to be more elegant in many respects to indexful formlations. 

I really find something rather tickling about the Linear style of vector.
A type of kind * -> * can be thoguht of as a container. When you apply a thing of that kind to the type it is holding, you get back the netire container type. Familiar examples include Maybe, and []. Container have shape. A Vector space is also commonly thought of as a container of numbers of a certain shape. By abstractly using types of kind * -> *, we avoid using labelling vector spaces only by the dimension. 
As pure shape, a concept of kind * -> * is a vector space without a particular scalar. 
If we abstract out the particular scalar involved in a vector space, it feels to me that this is roughly the same concept.

type Vect = * -> *

We can use the above styles in this way by partially applying an index to the vector.
But we can also make new intrinsics that can't be thought of in the other way.

Functors also have a natural notion of multplication and addition, which you may find in Data.Functor.Product and Data.Functor.Sum. These are very natrual liftings of plus and times of ordinary types to types with a hole. This is analogous to the lifting of ordinary addition and mutplication to the addition and mutlpication of polynomials, which are "numbers with a hole".

They are called Naperain/Logrithmic because there is a relationship similar to exponentiaion between the index type a and the container type f.
If you take the Product f g, this container is indexed by (a + b) = Either a b. In a sense Compose f g is indexed by the product (a,b). (f r) ~ r^a
The arrow type is written as an expoential b^a because if you have finite enumerable types a and b, that is the number of possible tabulations avaiable for f.
The Sum of two representable functors is no longer representable. Log(f + g) does not have very good identities associated with it.

Pure product functors correspond to structs in other languages. It seems conceivable that one could write in this style in C, C++, Rust with metaprogramming help. 

The names direct sum and direct product are most easily seen the make sense by looking at the algebra of the indices rather than the containers.
The direct product has a dimensionality that is the product of the idnvidiual dimensionality and the direct sum has

Non-naperian functors as sparse representations/ subspace reprenetations. Values like Nothing let you denote the 0 vectors.

The stock definition of Applicative for list [] does not perform vector addition. This demonstrates that the standard tyepclass hierarchy does not necessarily correspond to the correct operations, which is too bad.
The standard typeclass hierarchy is also too strong. Ideally we would want to limit ourselves to only linear operations, whereas the standard typeclasses make no such restriction.
It is basically a hack.

- Functor - Scalar Mutiplication
- Applicative - Vector addition
- Traversable - Tranposition. sequenceA has the type of transposition and works correctly for the appropriate.


There is an interesting form related to opaque functor style, using partially applied Kron. Now a vector takes two parameters, the scalar type and a held vector type
newtype V4' f a = V4' ((Kron V4 f) a)

exv2 :: forall f. Vector f => V4' f a
exv2 = V4' (Compose (V4 ?)

linear maps between these objects are isomorphic to linear maps on the regular vector space.

This can be domenstrated in one direction by instantating f with V1 and in the other direction by lifting the map by kroning it with `id`.

Why do this?
It adds an interesting polymorphic flavor.
One interesting thing to note is that it appears that we can use linear types to enforce linearity in the numerical sense.

class Vector f where
   vadd :: Num a => f a -> f a -> f a
   smul :: Num a => a -> f a -> f a

exv2 = (Vector f, Num a) => f a -o f a -o f a
exv2 f g = f `vadd ` 3 `smul` g

Why doesn't this work for the other form? Because of the following counterexample

exv2 :: a -o a -o a
exv2 x y = x * y

We need scalars to be capable of being multiplied together, but we cannot allow vectors to do so.

This is not particular a slam dunk

### Duals

Duals to all the above.

classic style stuff needs to be destructued, by the dual style doesn't. This can be seen. There is a rule of thumb that the dual space is easier to work with. It is evocative, related, identical perhaps in the right framework to the rule of thumb that CPS style is the most elegant. I do not understand this point clearly, but I can't help but feel it is correct if only I could find the right way of phrasing it.

newtype Dual f a = Dual (f a -> a)
newtype Dual v a = Dual (v -> a)

A more primitive style. Using container types brings in excess waste. We need type definition facilities, and we need to pattern match out of the data type ultaimtely to do anything with it. Data types do nothing.
Instead we can build a vector type out of functions and numerical operations. This type is very similar to the sgnature of the constructor V2 :: a -> a -> V2 a without the V2 in there.

  

 type DV2 a = a -> a -> a 
 exV2 x y = 2*x + 3*y -- rperesents vector [2,3]
 exV2' x y = dot (V2 2 3) (V2 x y)
 convert :: V2 a -> DV2 a
 convert v x y = dot v (V2 x y)


Levels 0 - 2
Level 0 is the indexing function. You can convert any reprsentable functor to this form via the (!!) operator.
a -> r
Level 1 is the dot product operator. Gien a vector in functor form, tae the do product with it. It's evocative of partially applied fmap.
(a -> r) -> r
Level 2 is
((a -> r) -> r) -> r

Level 2 is 
forall f. (v a -> f a) -- impossible. Can only give 0.
forall f. (Additive f) => (v a -> f a) -> f a -- kron and trace
forall f. ((v a -> f a) -> f a) -> f a
Mixing levels with the parametric vector idea. Maybe we can actually do a yoneda lemma then?
 
Double negation translation.


If you have another representation to add to this list above, I'd love to hear from you.

### Linear Operations
Similar to relations and sets (link to prev post), there are many ways of mixing and matching all of the above to get different styles of representation for matrices.
Well typed matrices

  -  [[a]]
  - HMatrix
  - Accelerate
    V n (V m) a
    [((a,b),r)]
    Map b (Map a r)
    a -> Vec b r - Linear Monad style. Piponi article. Giry monad.
    V4 (V3 a)






## TRASH


 -> as hom / <| |>
(f a, f a) -> a as metrix

(* -> * , *) 
type Dot f x = f x

in 1-d case reduces to product
newtype Dot f g
newtype Dag x f = Dag (f x) 
newtype Dag x y = Dag (x, y)

type family Dot
   Dot ('V2 x y) ('V2 z w) = x * z + y * w
newtype Dot' v w = Dot' (Dot v w)

* -> * -> *
put Dot2 in kindclass. replace 0/1 with ()/Void
newtype Dot2 f g x y = Dot2 (f 1 0) * (g 1 0) + (f 0 1) (g 0 1) 
class Dot k where
  type Dot (a :: k) :: * 

classic style stuff needs to be destructued, by the dual style doesn't. This can be seen. There is a rule of thumb that the dual space is easier to work with. It is evocative, related, identical perhaps in the right framework to the rule of thumb that CPS style is the most elegant. I do not understand this point clearly, but I can't help but feel it is correct if only I could find the right way of phrasing it.


The different formulatios of type level matrices
We have all the same stuff as value level, but we also have choices between GADTs, type, newtype, type families, type classes. 


I feel like we have multiple posts here:
Linear algerba of types 
Opqaure functor exposition
2-Vect
Fib with *->*


it's a vector space of paths. anyons contain a remnant histroy of their "path" (particle type history) and their .
This is held in "internal" state which isn't internal at all. It is actually spread out over the entire goddamn system.
Why does a double slit not contain this structure? The current position and momentum are all that is relevant.
Quantum systems that do not preserve their number of degrees of freedom. How is such a thing possible with unitary dynamics?


like consider the mach zehnder interferometry diagrams. They are basically finite state machines. Well... really they are exactly finite state machines. probalistic transitions

Matrix mathematics. Matrix multiplication. each elements of the Matrix mutiplication AB  is performed by multiplying each row of matrix A times each columns of matrix B. Multiplying a row.
This is ridiculous. I'm not goong to cover matrices in their entirety


Matrix mathetmaics. Matrix multiplication. each elements of the Matrix mutiplication AB  is performed by multiplying each row of matrix A times each columns of matrix B. Multiplying a row. In this row column operation, you sum a mutlpipication of terms. You can overload your conectp of matrix mathematics by allowing matrices to contain things that . One very fruitful example is allowing the elements of a matrix to be matrices themselves. These are known as block matrices. Since any matrix can be blocked, this gives a pleasant recursive definition of matrix multiplication.
Another interesting example is using plus -> min, times -> plus, sometimes called the min-plus semiring. You can encode shortest path and other graph concepts into these matrices.




Just like how numbers had plus-times and min-plus as acceptable rings, Functors have Sum-Product and Product-Compose as available ring structures.

Is there a Min-Plus of types? I don't know what min would be. Maybe in a system with subtyping?

Matrices themselves have an algerba that mimics ordinary algebra. Sqaure amtrices form a ring.

There is another interesting ring related to matrices. One can construct vector spaces out of smaller ones using the operation of direct sum and kronecker product. These operations are associative and commute, again not quite "on the nose" but in the sense that these new spaces are isomorphic to one another.

Polynomials of Vector spaces? type P f a = V1 a :+: V3 :*: f :+: V2 :*: f :*: f

Matrices are a grid of numbers.

### List Representation of Vectors and Matrices?
Since I don't like this one that much

## Linear algebra is good. 

Linear algebra has computational teeth.
Linear algebra forms the core of our computational ability to handle high dimensional spaces. It is also the core of nearly all scientific/numerical calculations. Linear algerba is a also a microcosm  of mathematics.
Matrices are grids of numbers. They multiply by "row times column".


The fact that when counting the total inhabitants of a type, tupling takes the product of the individual counts and Either takes the sum also appeals to me intuitively. The combinatorics of the type constructions.

(you can count types using the bounded typeclass. count = fromEnum . maxBound)
-- don't even need Bounded b if zig zag. It's tough to zig zag a rectangle though.
instance (Enum a, Bounded b,  Enum b) => Enum (a,b) where

instance Bounded a, Bounded b => Bouned (a,b)
instance Bounded a, Bounded b => Bouned (Either a b)
instance (Enum a, Enum b) => Enum (Either a b)

This isn't quite just teh dependent type analog of matrix multication. In that case, we'd use defined functions for plus and times. In this case, we are using rather raw type system constructs, like in my other post.

Here is one example. A probablistic transition system. Or just a dynamical system 
Consider a transition system with two states. If you are on spot A, you can got to spot B in 2 different ways or stay where you are in 3 ways. If you are in spot B you can
Build a data type that describes only possible lenght 10 paths through the system.



This data type may be the analog of my FibAnyon type. The state is the current root.
The n parameter is the tuple tree.

Again, it is easy to build the |_|_|_|_| style anyon tree using matrix like operations.

2-Vect if you want to call it that (I'm not seeing the 2 yet. The 2 get us the braiding and F moves) is a way of building a vector space that had a state machine involved in it's creation. The state is the fib label of edges. Independent branching dynamics, if the thing can branch into two indepdnent subsystems, that is a tree edge.
The fixed labels could perhaps be observations.
|_|_|_|_|_|_ is the diagram of a kalman filter. A prior is the root, objservations are the leaves. Kalman filter is the wrong term. I'm talking a general probablistic filter, not necessarily gaussian. In fact, more likely we are talking about viterbi filtering.

The natural thing to do is marginalization.

probalistic graphical models.


filter duality.

QUantum mechanics is clarified immenesly via considering problability. It has most of the same stuff and is a lot less spooky.

Braiding, F-moves in filtering. Abstraction? Consdering a more simplistic model. A different causality structure unobserved? Braiding - What if we swapped our kalman measurmeents in time?

Joining up edges again is a consistency condition.

type = 'V2 HomeCHoice WorkChoice
-- and project out one.
-- This list like structure is also the fix of applying the trvael matrix.
type = Fix (Apply TravelMatrix)

describe the data type for possible schedules for your day.

If now you wanted to descibre the probability of some paths, you now have a vector space over this base type.

If you wanted to descibre qauntum superopsitions of Sleeping and eating.

Also growing spaces.

Matrix-Graph correspondence. Typelevel shortest path problems
[[ 1,2,3],
 [ 4,5,6]]


Why would you want this. Honestly, I am more driven by how cute it all is rather than by any particular use case I have in mind.

Is this diluting the message. I'm transitioning the indexed version rather than the shape functor style.
Maybe this should be a seperate short post.


Basically a graph example. Basically a finite state machine.

Next Time: I don’t know when I’ll do the next article in this particular series, so let’s sketch out some of the neat things I has intended at least.

String diagrams are a way of drawing monoidal categories. One monoidal category is that of Functors. Each string corresponds to a functor and the diagram is read left to right, with left to right corresponding to functor composition. Natural transformations are nodes, with particular kinds appearing as splitting of lines or bending of lines.

Bending of lines in particular denotes adjunctions. The disappearance of the two functors corresponds to the ability to natural transform that ordering into the identity functor, which you often just leave out of the diagram.

I think that the left adjoint to vectory functors is a tuple of the index type (a,-). This is because one definition of adjunctions is that it is the relationship that makes L a -> b ~ a -> R b. This is closely related to currying.

sequenceA is a way of flipping functors. This may be important for getting the “wrong” cap, the one that adjunction doesn’t get you.

Functor composition of vectory functors corresponds to the Kronecker product. You can look at the the typeclass instance for Representable and see that the composition of functors indices the pairing of their indices.

Likewise, the functor Product of two vectory functors is their Direct Sum. Think about that Representable = Ln relationship.

These Functory String diagrams corresponds very well with the notation in quasi Feynman diagrams and quantum circuits and Penrose notation, where side to side composition corresponds to the Kronecker product and nodes are matrices. Cups and caps correspond to usees of the upper index or lower index metric. A cup and cap can be straightened out into a striaght line because those two metrics are inverse.

I say quasi Feynman diagrams because Feynman diagrams are for bosons and fermions, so they don’t really use the Kronecker product per say, and the left-right up-down ordering means nothing. They are totally topological. Goldstone diagrams are a variant where time is represented in up-down order, so they get closer.

You can index into this composite functor using fmap, but it kills sharing, which is a huge problem. I’m working on it. One basic possibility is to not use ordinary functor composition. Instead use

type Kron f g a = [(f a, g a)]

This is sort of a “Free” Kronecker product that keeps them unexpanded. You can still work with it. It is related to Piponi’s Linear Monad. It is definitely related to doing perturbation theory on top of free particles.

Another important improvement I think is to use sharing by only referencing by integer or something a globally stored vector to prevent dumb duplication of effort. Also Memoizing.

See you next time, hot dog.



We'll often see that there are a cuple monoids hanging around.

Ordinary morphism composition is already an operation with some monoidal flavor. It is associative and always has an identity. So another it is that monoidal categories have ordinary composition and some kind of Horizontal composition, putting things side to side.

I think this intuition is a little helpful for seeing why the 



So we can sort of lift the notion of a monoid to the notion of a monoidal category.

A completely different way of thinking about it is that monoidal categories have ordinary composition and some kind of Horizontal composition, putting things side to side.
Ordinary morphism composition is already an operation with some monoidal flavor. It is associative and always has an identity. So another it is that monoidal categories have ordinary composition and some kind of Horizontal composition, putting things side to side.

The standard categorical model of Haskell is that objects are types and morphisms are functions between them. It is kind of weird the mixing of semantic levels here. The functions are values. In the categorical model, ordinary values like 3 don't have a direct place to be put. `const 3` is a function, hence a morphism, which is a decent standin for the value 3.

Maybe you want to think of this like church encoding or other things, where you can think of basic values being built out of the very simple atoms of your theory.