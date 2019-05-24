%A Touch of Topological Quantum Computation in Haskell
% Philip Zucker


# A

# Quantum Mechanics in 5 Minutes
most of
normalization
matrix evolution
R+ vs C
probability is "parallel" too
exponential sized
double slit

----------------------
Probability                   Quantum                 
-----------                   -------                
$[0,1]$                       $\mathbb{C}$

$\sum p = 1$                  $\sum |a|^2 =1$

$p_i$                         $\psi_i$

$d^n$                         $d^n$

blase                         magic

----------------------

# Free Vectors
Styles of vector
np.array()
List
Vec n
Functor Vectors
Map
[(a,b)]
['=÷–]

# The Linear Monad
Sorry. I HAD TO
Is Pipework good enough?
| Tables        | Are           | Cool  |
| ------------- |:-------------:| -----:|
| col 3 is      | right-aligned | $1600 |
| col 2 is      | centered      |   $12 |
| zebra stripes | are neat      |    $1 |

#category

#GADTs

Generalized Abstract Data Types
Haskell Extension that let's you pick a signature for your constructors
this is not a good example
```haskell```
data MyGadt s where
    MyInt :: Int -> MyGadt Int
    MyBool :: Bool -> MyGadt Bool
```

# Hey ho buddi
here we is
```haskell
myprog :: IO ()
myprog = println "foo"
```
- item one
- item two

pandoc -i -t beamer -V theme:Warsaw topo.md -o topo.pdf