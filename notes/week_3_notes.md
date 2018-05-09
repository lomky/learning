## Week 3 - Data Structures and Types

### Functions on Lists
#### 3.1 Welcome to week 3
Focus:  
Lists in detail,  
Custom Data Types  
the History of Haskell.  

#### 3.2 Recursive Functions on Lists

**Computing with lists**  
Two ways of working with lists:  
  - write a function to do what you want, use recursive definition to traverse the list  
  - write combinations of standard list processing functions  
Generally speaking, second is better. But underneath, everything is initially implemented in the first way.  

**Recursion on lists**  
lists are build from the empty list `[]` and the fn `cons :: a -> [a] -> [a]`.  
In haskell, `:` is `cons`, and is pronounced as such.  

All lists are either `[]` or `(x : xs)` for `x` (the head) and `xs` (the tail).  
`(x : xs)` reads "x cons xs"  
In recusion, `[]` is the base case and recursion happens on `(x : xs)`  

**Recursion Examples**  
_length_  
```haskell
length :: [a] -> Int
length [] = 0                 -- the base case
length (x:xs) = 1 + length xs -- the recursion case
```  

_filter_  
 Given a _predicate_ (func which returns a `bool`) & a list, return the list of elements which satisfy the predicate.  
```haskell
filter :: (a->Bool) -> [a] -> [a]
filter pred [] = []
filter pred (x:xs)    -- Uses guards. More info in week 4
  | pred x        = x : filter pred xs
  | otherwise     = filter pred xs
```  
So given `filter (<5) [ 3,9,2,12,6,4]`, the result is `[3,2,4]`  

**Computations over lists**  
Functionality that would be loop structures in imperitive languages become _list computations_ in functional languages.  
Common cases:  

  - do somethign to each element of a list: `map`  
  - iterate on a list, left->right: `foldl`  
  - iterate on a list, right->left: `foldr`  
Using these functions to build up other functionality is **good practice**  

**Function Composition**  
Expressing larger computation by chaining together functions of smaller computations.  

  1. Given arguemtn of type `a`  
  1. Apply a function `g :: a->b` to the arg, with intermediate result `b`  
  1. Apply a function `f :: b->c` to the intermediate result, getting final result `c`  

  - This computation (first _g_, then _f_) is written `f ∘ g`  
    - Normal mathematical notation - remember to read R to L  
  - In haskell, its written:  

**test** 
```haskell
(.) :: (b->c) -> (a->b) -> a -> c
(f . g) x = f (g x)
```

**`map` function**  
`map` applies a function to each list element.  
`map f [x0,x1,x2] -- > [f x0, f x1, f x2]`  
**Composition of `map`s**  
very common tool in functional programming.  
Common style: given two simple computations, compose them with map:  
`map f (map g xs) = map (f . g) xs`  
**`map` recursive definition**  
```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

**Folding a list (reduction)**  
iterating over a list to produce a singleton: _fold_  
Variations: fold from left, fold from right, and other more adv versions.  
V. powerful and intuitive.  

**Left Fold: `foldl`**  
Fold from the left  
iterate across a list ->  
Looks like: `foldl f z xs`  
`z :: b` - initial value  
`xs :: [a]` arg - list of values to combine with `f`  
Think of `z :: b` as the "accumulator"  
`f` takes the current value of the "accumulator" and a list element, returns the new accumulator val.  
```haskell
foldl :: (b->a->b) -> b -> [a] -> b
```  

**`foldl` Examples: Function Notation**  
```
        𝚏𝚘𝚕𝚍𝚕𝚏𝚣[]  ⇝                      z
      𝚏𝚘𝚕𝚍𝚕𝚏𝚣[𝚡𝟶]  ⇝                 f z x0
   𝚏𝚘𝚕𝚍𝚕𝚏𝚣[𝚡𝟶,𝚡𝟷]  ⇝          f (f z x0) x1
𝚏𝚘𝚕𝚍𝚕𝚏𝚣[𝚡𝟶,𝚡𝟷,𝚡𝟸]  ⇝   f (f (f z x0) x1) x2
```  

**`foldl` Examples: infix Notation**  
`+` represents an arbitrary operator for `f`, not necesarily addition.  
```haskell
foldl (+) z []          -- > z
foldl (+) z [x0]        -- > z + x0
foldl (+) z [x0,x1]     -- > (z + x0) + x1
foldl (+) z [x0,x1,x2]  -- > ((z + x0) + x1) + x2
```  

**`foldl` Recursive Definition**  
```haskell
foldl        :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs
```

**Right Fold: `foldr`**  
Like `foldl`, but <-  
`foldr :: (a -> b -> b) -> b -> [a] -> b`  

**`foldr` Examples: function notation**  
```
        𝚏𝚘𝚕𝚍𝚛 𝚏 𝚣[]  ⇝                     z
      𝚏𝚘𝚕𝚍𝚛 𝚏 𝚣[𝚡𝟶]  ⇝                f x0 z
   𝚏𝚘𝚕𝚍𝚛 𝚏 𝚣[𝚡𝟶,𝚡𝟷]  ⇝         f x0 (f x1 z)
𝚏𝚘𝚕𝚍𝚛 𝚏 𝚣[𝚡𝟶,𝚡𝟷,𝚡𝟸]  ⇝  f x0 (f x1 (f x2 z))
```  

**`foldr` Examples: infix notation**  
```haskell
foldr (+) z []          -- > z
foldr (+) z [x0]        -- > x0 + z
foldr (+) z [x0,x1]     -- > x0 + (x1 + z)
foldr (+) z [x0,x1,x2]  -- > x0 + (x1 + (x2 + z))
```  

**`foldr` Recursive Definition**  
```haskell
foldr            :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys
```  

**Relationship between `foldr` and list structure**  
`[x0,x1;x2]` can be written `x0 : x1 : x2 : []`  
Folding `cons` (`:`) over a list with `[]` as the accumulator:  
```haskel
foldr (:) [] [x0,x1,x2] --> x0 : x1 : x2 : []
```  
Identical to constructing a list using `[]` and `:`!  
Formal relationship:  
_foldr cons [] xs = xs_  

**Applications of Folds**  
```haskell
sum xs = foldr (+) 0 xs
product xs = foldr (*) 1 xs
```  
The `xs` on both sides can be factored out:  
```haskell
sum =     foldr (+) 0
product = foldr (*) 1
```  
Called _point free_ style, data never mentioned directly.

#### 3.3 Functional Maps and Folds Versus Imperative Loops

**Imperitive Equivalents**  
map   : loop over list element-by-element, append new element to new list  
foldl : loop over list element-by-element, update accumulator using current accumulator and element  
foldr : loop over reverse list element-by-element, update accumulator using current accumulator and element  

```haskell
map :: (a -> b) -> [a] -> [b]
foldl :: (b -> a -> b) -> b -> [a] -> b 
foldr :: (a -> b -> b) -> b -> [a] -> b
```


#### 3.4 Do It Yourself: Lists and Recursion
Assignment site is down, no access to assignment to complete locally.  

#### 3.5 Do It Yourself: Function Composition
Assignment site is down, no access to assignment to complete locally.  

#### 3.6 What have we learned about lists? Quiz
Issues thinking through foldl and foldr by hand. Parens!  

#### 3.7 Summary
_Recursion_ is the basic mechanism for computing on data in Haskell.  
All recusion has a _base case_ and an _induction case_  
list's base case: `[]`, induction working with the `x:xs`  

Focus on using _higher-order functions_ on list operations, i.e. `map`, `foldr`, `foldl`.  
aka _list combinators_  

### Custom Data Types
#### 3.8 Define your own Data Types

**Bool**  
True :: Bool  
False :: Bool  

**Int**  
1 :: Int  
42 :: Int  
minBound :: Int  

**Others**  
'a' :: Char  
[1,2,3] :: Int List
("victoria", 1837) :: (String, Int)

**User-Defined Types**  
Create a simple number that can only count 1, 2, Many.  
```
Prelude> data SimpleNum = One | Two | Many
Prelude> One

<interactive>:4:1: error:
    • No instance for (Show SimpleNum) arising from a use of ‘print’
          There are instances for similar types:
          instance [safe] Show Ghci2.SimpleNum
           -- Defined at <interactive>:2:44
     • In a stmt of an interactive GHCi command: print it
Prelude> data SimpleNum = One | Two | Many deriving Show
Prelude> One
One
Prelude> :t One
One :: SimpleNum
```
Defined and made show-able by deriving it off another type.  

**Convert Int to SimpleNum**  
```
Prelude> :set +m
Prelude> let convert 1 = One
Prelude|     convert 2 = Two
Prelude|     convert _ = Many
Prelude|
Prelude> convert 1
One
Prelude> convert 2
Two
Prelude> convert 33
Many
Prelude> map convert [1..5]
[One,Two,Many,Many,Many]
```

That's a custom data type with alternative values, aka _Sum_ data type.

**Algebraic Data Type**  
Stores a portfolio of values.  
```
Prelude> data CricketScore = Score [Char] Int Int deriving Show
Prelude> let x = Score "New Zealand" 350 4
Prelude|
Prelude> x
Score "New Zealand" 350 4
Prelude> :t x
x :: CricketScore
```

**Summary**  
_data_ keyword defines new types  
`deriving Show` allows values to print out  
sum data types use `|` to give alternative values  
Type constructors are used to build _record types_ or _product data types_  

#### 3.9 Grow a Tree

**Trees**  
In computer science, trees from from root to leaves.  
Roots are the unique starting point, leaves are at the bottom.  

**Binary Trees**  
Generally used to store data in a sorted order, allowing for efficient searching.  

**Tree Data Type in Haskell**  
Tree data type to store integers.  
```haskell
data Tree = Leaf | Node Int Tree Tree deriving Show
```  
Tree values might be a `Leaf` or a `Node`. It is a _recursive_ data type!  
`Node` stores an `Int` payload and branches to two subtrees, aka children.  

Simplest tree:  
```Haskell
Leaf
```  

Tree with one Node, val 3, and two leaves.  
```haskell
Node 3 Leaf Leaf
```  

**Compute the depth of a `Tree`**  
```haskell
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) =
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)
```  
Note the `_` value - we don't need to worry about the payload value for our purpose.  
How about a function that sums the node values?  
Write your own:  
```haskell
treeSum :: Tree -> Int
treeSum :: Leaf = 0
treeSum (Node num leftSubtree rightSubtree) = num + (treeSum leftSubtree) + (treeSum rightSubtree)
```  

**Sort the tree**  
`Leaf`s are always sorted,  so each node is what we're sorting.  
Check each value is between min and max, splitting the ranges as we go down.  
```haskell
isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree :: Leaf _ _ = True
isSortedTree ( Node x leftSubtree rightSubtree) minVal maxVal =
  let leftSorted    = isSortedTree leftSubtree minVal x
   rightSorted   = isSortedTree rightSubtree x maxVal
  in x >= minVal && x < maxVal && leftSorted && rightSorted
```

**_Modication_ functions**  
The above have been _traversal_ functions, moving through the tree data structure & computing at each node.  
_modification_ functions generate new `Tree`s by modifying the values of the source `Tree`  
This function will add a new max value to the tree.  
```haskell
addNewMax :: Tree -> Tree
-- add a new max element to tree
addNewMax Leaf = Node 0 Leaf Leaf -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf) -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node (x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree
```

**Practice Problems**  
_Insert a value in place into a tree_  
```haskell
insertValue :: Tree -> Int -> Tree
insertValue Leaf x = Node x Leaf Leaf
insertValue (Node value leftSubtree rightSubtree) x
  | x < value = Node value (insertValue leftSubtree x) rightSubtree
  | x >= value = Node value leftSubtree (insertValue rightSubtree x)
```  

_Convert a Tree into a list_  
```haskell
treeToList :: Tree -> [Int]
treeToList Leaf = []
treeToList (Node value leftSubtree rightSubtree) =
  treeToList leftSubtree ++ ([value]) ++ trreToList rightSubtree
```

#### 3.10 Type Classes

**Number like Types**  
Int, Float, Integers  
Many arithmetic functions can apply to any numeric type.  
```
2+2
4
2+pi
5.141
2*2
4
```  

**Type Classes**  
Establish a family the type belongs to, allowing them to use common functions.  
```
:type (+)
(+) :: Num a => a -> a -> a
```  
The `Num a` is the _context of the type_ establishing type class membership.  

**Other Type Classes**
```
:type (==)
(==) :: Eq a => a -> a -> Bool
1 == 1
True

:type (<)
(<) :: Ord a => a -> a -> Bool
"aaron" < "aardvark"
False
```  

**Show & Read**  
Show means the type can return itself as a string to print.  
Read means you can generate values of such type from a string value.  
```
show 1
"1"
show True
"True"
read "1" :: Int
1
read "True" :: Bool
True
```  

**Specifying Type Class on a Type**  
_deriving_ in the type declaration.  
```
data SimpleNum - One | Two | Many deriving (Show, Read)

show One
"One"
show Two
"Two"
show Many
"Many"
read "One"
*** Exception Prelude.read no parse
read "One" :: SimpleNum
One

One == One
[error message, SimpleNum not eq type class]
data SimpleNum - One | Two | Many deriving (Show, Read, Eq)
One == One
True
One == Two
False
One == "One"
[Type mismatch error]
```  

**Summary**  
A type clas constrains memebertypes (instances) to conform to an API  
Type classes are like interfaces in C# and Java  
Types in the type class are like contrete implementations of interfaces  
Type classes enable operator overloading.  

### Haskell History
#### 3.11 Interview with Simon Peyton Jones
Watched. Notes not taken.  

#### 3.12 Brief History of Haskell
Read. Notes not taken.  

#### 3.13 Course Feedback
Watched. Notes not taken.  

#### 3.14 End of Week 3
Watched. Notes not taken.  

