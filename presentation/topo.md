%A Touch of Topological Quantum Computation in Haskell
% Philip Zucker


# Overview

Where are we going today:

- Quantum Computation
- Haskell
- Vectors
- Anyons
- Category Theory?

# Quantum Mechanics in 5 Minutes
::: notes
most of
normalization
matrix evolution
R+ vs C
probability is "parallel" too
exponential sized
double slit

show some matrices? I like seeing a matrix.

:::

----------------------
Probability                   Quantum                 
-----------                   -------                
$[0,1]$                       $\mathbb{C}$

$p_i$                         $\psi_i$

$\sum_i p_i = 1$              $\sum_i |a_i|^2 =1$

$T_{ij}$                      $U$

$\sum_j T_{ij} = 1$           $U^\dagger U = I$

$sampling$                    $measurement$

$d^n$                         $d^n$

blase                         magic 

----------------------


# Quantum Computation
::: notes
Interesting physics for physics sake,
topological qc is a propsed paradign for fighting decoherence. Like magnets bring physics into classical error correction
the core necessary to even talk aout topological

:::

- Applications in Cryptography, Optimization, Physical Simulations
- Hard to keep quantum quantum. Error Correction
- topological quantum computation
- anyonic vector spaces


# Haskell
::: notes
Haskell  is a statically typed, purely functional programming language with type inference and lazy evaluation.[28][29] Type classes, which enable type-safe operator overloading, originated in Haskell.[30] Its main implementation is the Glasgow Haskell Compiler. It is named after logician Haskell Curry.[1]


Community
Medium weight formal methods
succinteness
Abstractability
optimizaing compiler (fast)
immutability

type classes?

hello world is not actually useful

``` haskell
main = printLn "Hello World"
```

:::

- Programming Language 1990
- Functional
- Statically Typed
- Pure / Immutable
- Lazily evaluated
- Polymorphic
- Type classes



```haskell
factorial :: Num a => a -> a
factorial 0 = 1
factorial n = n * (factorial (n-1))
```

# Mathematical Vectors
- little arrows
- direction and magnitude
- Abstract
- basis elements mean thing
- little, medium, big
- geometrical, 2d, 3d, 4d
- color 3 dimensional BGR RGB
- discretized pde spaces
- probability, and quantum mechanics


# Computer Implemented Vectors 
::: notes
vectors on computers

color you're ocmpiling in your head when the computer hsould be compiling for you
all erors to be cuaght at compile time

Integers are sometimes a natural basis, but often not.
Integers are just things that can be counted
We are programming to the computer or to humans or the domain.

Why do we make everything implciit, only in the mind of the programmer
The dream is to make everything explicit in a pain free manner
That the language is helps you think, and helps to make sure you are making sense
The compiler/ types are your friend. Not your enemmy not a disciplinarian
, available for the compiler to check

What do I call these
unsized?
dynamic?
untyped?
weakly typed


for many scientific applications
crashing isn't the primary concern really



:::

- arrays
- sparse vs dense
- structures

- indexed on integer because computers like integers
- Color
Vectors = Arrays?

- np.array()
- [Double]
- Data.Vector
- HMatrix
- Repa
- Massiv
- accelerate




- Vec n
- Functor Vectors

# Free Vectors
::: notes

Vec n a here if anywhere

:::

- good for sparse
- make the domain part of the vector
historical 
row column formats

- b -> r
- Map b r
- [(b,r)]

```haskell
data W b a = W { runW :: [(a,b)] } deriving (Eq,Show,Ord)
```
# Mathematical Linear Maps
::: notes
Explciit matrix is very useful for inversion, other operations.
But if the only thing you're going to do is matrix multiplication or
matrix vector multiply, the functional representation is flexbiel
identity matrix = id
How do you know you are not squaring
:::
Concept
$A (\alpha \vec{x} + \beta \vec{y}) = \alpha A \vec{x} + \beta A \vec{y}$

Defining how linear maps act on a basis define how they act on any vector via linearity.

# Implementing Linear Maps

- Matrix a 
- (Vec a -> Vec a)

```haskell
vfun :: Vec Double -> Vec Double
vfun = fmap square
```

# The Linear Monad
::: notes
Sorry. I HAD TO
Is Pipework good enough?
Allows lifting of injectiton, projection, and permutation functors
Guarantees we cannot do nonlinear operations
Linear functions are an interesting subset that are very useful.
But how do we constrain this
Vec Double -> Vec Double 
can easily be non linear
piponi

The vector considered as a functor over the basis labels, not the scalars.
a -> m b should be viewed as a -m> b

linear functions are often described as their action on basis elements
And the extended to an arbitrary vector by linearity.

pure indexful functions can be lifted with fmap

:::

$A\hat{e}_i = \sum_j a_{ij}\hat{e}_j$

```haskell
matA Up   = [(Up,1)  ,  (Down,  1)]
matA Down = [(Up,1)  ,  (Down, -1)]
```

```haskell 
instance Num b => Monad (W b) where
   return x = W [(x,1)]
   l >>= f = W $ concatMap (\(W d,p) -> map (\(x,q)->(x,p*q)) d) (runW $ fmap f l)
bs >>= f = [  |  (b, r) <- bs] 
pure
```

# Physics of Fibonacci Anyons
::: notes
Have a diagram of each junction type
:::

- Particles $I$ and $\tau$
- Possible production rules

# Gameplan
describe the basis


# GADTs
::: notes
Generalized Abstract Data Types
Haskell Extension that let's you pick a signature for your constructors.
Constructors are special function in that they are universal. The GADT maintains this universailty.
You don't lose type information by applying a GADT constructor. It is recoverable via pattern matching.

this is not a good example
```haskell```
data MyGadt s where
    MyInt :: Int -> MyGadt Int
    MyBool :: Bool -> MyGadt Bool
```


We can constrain our tree type using GADTs.
:::






# Implementation of Fibonacci Anyons

::: notes
Have a diagram of each junction type
:::

- Possible production rules
- Particles $I$ and $\tau$

```haskell
data Tau
data Id
data FibTree root leaves where
   TTT :: FibTree Tau l -> FibTree Tau r -> FibTree Tau (l,r)
   ITT :: FibTree Tau l -> FibTree Tau r -> FibTree Id (l,r) 
   TIT :: FibTree Id l -> FibTree Tau r -> FibTree Tau (l,r)
   TTI :: FibTree Tau l -> FibTree Id r -> FibTree Tau (l,r)
   III :: FibTree Id l -> FibTree Id r -> FibTree Id (l,r)
   TLeaf :: FibTree Tau Tau
   ILeaf :: FibTree Id Id
```



# References 

https://arxiv.org/pdf/1705.04103.pdf - topological QM review
Nielson and Chuang
Quantum compute review
Moggi and Wadler Monad originals?
Probability monad original and Piponi articles
Wikipedia
Kitaev possibly
Baez and Stay
Preskill notes
http://blog.sigfpe.com/2007/03/monads-vector-spaces-and-quantum.html



my blog posts

http://www.philipzucker.com/a-touch-of-topological-quantum-computation-in-haskell-pt-i/
http://www.philipzucker.com/a-touch-of-topological-quantum-computation-in-haskell-pt-ii-automating-drudgery/
http://www.philipzucker.com/a-touch-of-topological-computation-3-categorical-interlude/



# Quantum Gates
::: notes
get some pics here
:::


- Nand complete for classical circuits
- Complete Set, CNot and 1-spin qubits 
- Build big ole matrices by decomposing them into matrices acting of small kronecker pieces
- Measurement (reading some current or voltage or something)
![](./H_CNOTGate.png){ width=50% }

# Topological Quantum Gates
::: notes
to the best of my understanding of physics, there is no reason to believe ANY particle
you ever talk about is anything more than a quasiparticle. The distincting may be nonsensical

Classical error correction and magnetic disks. Use the physics to do error correction for you
Magnets are a phase of matter with intrinisic error correcting properties.

What is topological about any of this?
Topology is geometry mod wiggles, which leaves only the connectivity structure (well you can also abstract it into oblivion)
Well only the topology of the braiding matters
Also if you put these states of matter onto topological surfaces
The ground state degeneracy depends on the topology of the surface (torii double torii spheres).
This is a related thing. A particle is a certain kind of hole in the ground state. 
There is not enough time to go into this and do I even really understand it?

:::

- Anyons - Quasiparticles.
- If it smells and quacks like a duck
- Baseballs, atoms, nuclei, electrons quarks?
- Braiding of quasiparticles
- Annihilating quasiparticles - Measurement

## ![](./anyon.png){ width=50% }

#Anyons
::: notes
They are a strange beast.
Particle production trees
Only certain kinds of particles can be made from others possibly in distinct ways
The Vector space is labelled by a particle topology of production.
Basis is given by differing inner labels.
OPerations on this vector space can only be implemented by braiding particles.
The space is protected.
Gapped ground state degeneracy. e^E/kT 
Topological quantum computation is implementing via the braiding
Energetic protection. An energy barrier can prevent something from happening
Thermal or perturbative. Ball in a well.
comparison to bosons and fermions

2 dimensional beasts

3d is an unusually rich place

Merely by exisiting.

:::


## Topological Quantum Computation
::: notes
we don't have the space to touch this as a seperate slide
:::

- Anyons. A funky vector space. We'll get to it
- Implement Gates As Braiding of Anyons
- Majorana, FQHE, 


# Representable Functors
Any functor that is isomorphic to `(->) a`
Gibbons Naperian Functors
Connects the Functor Vector to the Free Vector style
One slide? For everything?

# Functor Vectors
::: notes
Vectors as Functor on Scalars
struct vectors
Representable functors makes a bridge between the two concepts.

Ok. I don't think we  have time to get into this.

:::

(,,,,,,) ntuples
Kmett's linear package 
Compose = Kron

Product = Direct Sum
Natural Transforomations
Indexing into with `fmap`

# Hey ho buddi
here we is
```haskell
myprog :: IO ()
myprog = println "foo"
```
- item one
- item two

pandoc -t beamer -V theme:Warsaw topo.md -o topo.pdf



::: notes

Who are the people I'm talking to. Do they know haskell?

Cut the what is haskell slide? If I need space? Put the haskell slide earlier

Give up the quantum computation? Don't want to be asked. 
I don't really plan on getting into quantum computation
It is spicy, 
Implementing infrastructure
Implementing the vector spaces of 
Building
Describing



Factoring, breaking RSA
physical simulations, chemicals, catalysts, pharmaceuticals ("analog" simulation easier?)
Speedup of search algorithms (Grover)
Adiabatic optimization
HHL solving linear eqautions

superposition and entanglement. explores exponentially sized spaces with tiny pinholes in and out

probability ~ chance and correlation. 
The difference is slight and subtle. Sampling is also a tiny pinhole.
This may account for why there are so few quantum alogirhtms in the gap.

Possible implementations - enough to shake a stick at
Superocnding
Ion trapp

Quantum mechanics encodes temporal operations in Unitary matrices
Probability encodes temporal operations into transition matrices (sum of all columns = 1)

Problems 
Decoherence

Error Correction. 


Deutsch-Jones
QFFT
Grovers
Shors
HHL


It IS kind of neato. 
Maybe there are lessons to be learned that apply to classical computers
heuristics
Maybe they will expand the frontier of mankinds infromation processing abilities in my lifetime

Nielsen and Chuang


Quipper

Haskell
Math Vectors 
QM

QC
QM


:::

# Vectors
::: notes

Styles of vector

I do like my distinction  of small medium and big vectors
Not sure it is relevant.
I guess my point that it is particularly relevant for quantum computation
Dense is unacceptable
Sparse isn't really right.
There may be room for complicated things
I do not have the solution,
vsum :: [a] 
smul s = fmap (* s)

As much as it might make you sick, free your mind from performance.


:::


# Automating mechanical aspects
::: notes
not sure I have desire nor time to go into this.
The work is kind of trash anyhow. 
My typeclass style programming is not pleasant.
:::

# Category Theory

Category: Objects Morphisms
Monoidal Category
Braided

Point free programming exposes categorical underpinnings
Vect
Kliesli Arrow of Linear Monad
Physical processes
 
Linear Types ~ Categories don't have to be cartesian (dup and proj).

# 2 Vect anyons

Since I'm not done with this, I am unlikely to talk about it