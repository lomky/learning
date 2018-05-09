## Week 2 - Haskell Building Blocks

### More Basic Haskell
#### 2.1 Welcome
#### 2.2 Do It Yourself: Boolean Values and Expressions
Assignment site is down, no access to assignment to complete locally.  

#### 2.3 Zip that List
**zip**  
`zip` combines two lists into one!  

`["fish", "bread", "salt"]`  
`["chips", "butter", "pepper"]`  
Becomes  
`[("fish","chips"),("bread","butter"),("salt","pepper")]`  

```
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> zip [1,2,3] [4,5,6]
[(1,4),(2,5),(3,6)]
Prelude> zip [1,2,3] "abc"
[(1,'a'),(2,'b'),(3,'c')]
Prelude> zip "glasgow" "beijing" "nairobi"

<interactive>:3:1: error:
    • Couldn't match expected type ‘[Char] -> t’
                  with actual type ‘[(Char, Char)]’
    • The function ‘zip’ is applied to three arguments,
      but its type ‘[Char] -> [Char] -> [(Char, Char)]’ has only two
      In the expression: zip "glasgow" "beijing" "nairobi"
      In an equation for ‘it’: it = zip "glasgow" "beijing" "nairobi"
    • Relevant bindings include it :: t (bound at <interactive>:3:1)
Prelude> zip3 "glasgow" "beijing" "nairobi"
[('g','b','n'),('l','e','a'),('a','i','i'),('s','j','r'),('g','i','o'),('o','n','b'),('w','g','i')]
Prelude> length [1..10]
10
Prelude> length ['a'..'z']
26
Prelude> zip [1..10] ['a'..'z']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e'),(6,'f'),(7,'g'),(8,'h'),(9,'i'),(10,'j')]
```
Thus, `zip` combines two lists into pairs, stopping at the length of the shortest argument.  

**zipWith**  
`zipWith` is the generalized version of `zip`, it takes the function to use on the two lists in order to combine them.  
```
$ ghci
Prelude> zipWith max [1,2,3] [0,2,4]
[1,2,4]
Prelude> zipWith (+) [1,2,3] [0,2,4]
[1,4,7]
```

**zip as a zipWith**  
```
$ ghci
Prelude> zipWith (\x->(\y->(x,y))) [1,2,3] [0,2,4]
[(1,0),(2,2),(3,4)]
```  
This function uses a lambda function

#### 2.4 Do it Yourself: Logical Thinking
Assignment site is down, no access to assignment to complete locally.  

#### 2.5 Nothing but the Truth Quiz
Completed, 100%.

### Input and Output
#### 2.6 Why I/O?
Input and output are how a computer interacts with the world.  
Haskell handles I/O via monads.  
Two things to know about I/O for now:  
Functions using I/O - type _must_ contain IO!  
IO Monad ensures the i/o operations occur in a _fixed sequence_  

#### 2.7 Do It Yourself: Input/Output
Assignment site is down, no access to assignment to complete locally.  

#### 2.8 I/O and a First Encounter with Monads

**Pure Functions**  
_Pure_ functions take values, manipulate them, and return a result value.  
Pure functions _do not depend_ on the state of the world - computation is self-contained.  

**I/O is Impure**  
input and output are _impure_. They influence and interact with the 'outside world'. But it is required to make computer do anything interesting!  
`getLine` reads input form the user and resutnrs it as a special `String` value: `IO String`.  
`putStrLn` takes a `String` input and prints to terminal, returning empty `IO` value.  

**Sequencing Actions**  
```
Prelude> do { planet <- getLine; home <- getLine; putStrLn ("greetings " ++ planet ++ "ling."); putStrLn ("I am from " ++ home ++ "."); putStrLn "Take me to your leader."}
Mars
Jupiter
greetings Marsling.
I am from Jupiter.
Take me to your leader.
```  
Notice, order mattered! We want to get the first `getLine` before the second `getLine`.  
In pure code, order doesn't matter!  
```haskell
let a = reverse "winston"
    b = reverse "churchill"
in "sir " ++ a ++ " " ++ b
```  
Either `reverse` can happen in either order or at the same time.  

Note the `<-` association. This is used to associated IO functions results with names. Pure function results use `=`  

**Monads underneath**  
`do` notation enables _sequence_ actions, but this is only syntactic sugar.  
Below, its a change of function calls, each feeding into the next.  
More discussion of Monads & bind operators later in the course.  

**Summary**  
IO operations are impure  
`do` specifies a sequence of actions  
`<-` associates IO values with names
values/functions that involve IO has `IO` in its type  
sequences of IO action described: "in the IO Monad"  

### Installing GHC
#### 2.9 Installing Haskell for Yourself
Install ghci.

#### 2.10 How to Run GHCI
Invoke via cmd line: `ghci`  
`Prelude>` indicates the std lib is loaded.

```
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> "hi all"
"hi all"
Prelude> :type "hi all"
"hi all" :: [Char]
Prelude> 42
42
Prelude> 1+1
2
Prelude> let x = 3
Prelude> x
3
Prelude> x+1
4
Prelude> length "hi all"
6
Prelude> reverse "hi all"
"lla ih"
Prelude> let fact n = if n == 0 then 1 else n*fact(n-1)
Prelude> fact 0
1
Prelude> fact 3
6
Prelude> fact 55
12696403353658275925965100847566516959580321051449436762275840000000000000
```

**Working with saved files**  

_See [exercises/2-10_factorial.hs](https://github.com/lomky/future-learn-haskell/tree/master/exercises/week_2/2-10_factorial.hs) for examples_

Write fact2 and load it:  
```
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> :load 2-10_factorial.hs
[1 of 1] Compiling Main             ( 2-10_factorial.hs, interpreted )
Ok, modules loaded: Main.
*Main> fact2 3
6
*Main> :type fact2
fact2 :: Int -> Int
*Main> :t fact2
fact2 :: Int -> Int
*Main> map fact2 [1..10]
[1,2,6,24,120,720,5040,40320,362880,3628800]
```
Clean up the factorial (fact3).  
```
*Main> :load 2-10_factorial.hs
[1 of 1] Compiling Main             ( 2-10_factorial.hs, interpreted )
Ok, modules loaded: Main.
*Main> map fact2 [1..10]
[1,2,6,24,120,720,5040,40320,362880,3628800]
*Main> map fact3 [1..10]
[1,2,6,24,120,720,5040,40320,362880,3628800]
*Main> fact3 (-1)
*** Exception: stack overflow
```

`:quit` exits.  

**I/O**  
Note: `:set +m` gives the multiline mode  
```
*Main> :set +m
*Main> do x <- getLine
*Main|    putStrLn ("hello " ++ x)
*Main|
Kat
hello Kat
*Main>
```

See [exercises/2-10_nobles.hs](https://github.com/lomky/future-learn-haskell/tree/master/exercises/week_2/2-10_nobles.hs)  
```
*Main> :load 2-10_nobles.hs
[1 of 1] Compiling Main             ( 2-10_nobles.hs, interpreted )
Ok, modules loaded: Main.
*Main> mknoble "Kat T"
"Sir Kat T"
```

```
*Main> :l 2-10_nobles.hs
[1 of 1] Compiling Main             ( 2-10_nobles.hs, interpreted )
Ok, modules loaded: Main.
*Main> mknoble2 True "Kat T"
"Dame Kat T"
*Main> mknoble2 False "David"
"Sir David"
```

#### 2.11 Guessing Game
See [exercises/2-11_guessing_game.hs](https://github.com/lomky/future-learn-haskell/tree/master/exercises/week_2/2-11_guessing_game.hs)  
#### 2.12 What do you know about Haskell Quiz
Completed, 100%.

#### 2.13 End of Week 2
