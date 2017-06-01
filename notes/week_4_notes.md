## Week 4 - When Programs Get Bigger

### Program Structure
#### 4.1 Week 4 Welcome

Looking at advanced computation patterns with functions as the value.  
Parsing - process of analyzing text of a language according to the formal grammer  
parsec - parsing library in haskell  
QuickCheck - automated testing.  

Program Structure  
with more complication programs, structure adds comprehensibility.  
Scoping & Conditionals help structure programs.  

#### 4.2 Keep Your Programs Tidy

##### Scoping
Scoping is a primary tool for keeping programs legible.  
It limits the regions in which names 'exist' in your program.  

**Scoping with `let`**  
`let` establishes lexical scope. After the `let` keyword, you give a series of equations defining variable values & a final expression following `in` to compute a value with those variables.  
```haskell
let x = 2
in x*x
```  
Not limited to one var:  
```haskell
let x = 2
    y = 3
in x+y
```  
Lining up the variables is good practice _and_ required for the interpreter. In haskell **whitespace is significant**.  
Variables can depend on one another inside a `let`  
```haskell
journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre =
 let milespergallon = 35
     litrespergallon = 4.55
     gallons = miles/milespergallon
 in (gallons*literspergallon*fuelcostperlitre)
```  
```haskell
let diameter = 2*radius
    circumference = pi*diameter -- pi is a constant from Prelude
in (diameter, circmference)
```  

**Scoping with `Where` clauses**  
`where` clauses also introduce a scope with local variables.  
`where` is used inside of an equation to give definitions to variables used in the equation.  
```haskell
squareplusone :: Int -> Int
squareplusone x = xsquared + 1
 where xsquared = x*x
```  
The `where` **must be indented** more than the enclosing equation.  
It can also contain multiple variables.  
```haskell
cel2fahr :: Float -> Float
cel2fahr x = (x*scalingfactor) + freezingpoint
 where scalingfactor = 9.0/5.0
       freezingpoint = 32
```  
Note the whitespace!  

**`let` vs `where`**  
Similarities:  

  - introduce a local scope  
  - allow any number of equations  
  - allow equations in any order  
  - introduced variables can be used in the other equations  

Differeces:  

  - `let`s are _expressions_ and are used in places expecting expressions  
  - `where`s are _clauses_, not expressions. Only usable to provide local variables to a _top level equation_.

#### 4.3 Guards, Guards!
**Guards**  
Guards are haskell's notation for defining functions based on _predicate_ values.  
```haskell
f x
  | predicate1 = expression1
  | predicate2 = expression2
  | predicate3 = expression3
```  

Calculating an absolute value with conditionals:  
```haskell
absolute x = if (x<0) then (-x) else x
```  
Alternatively, with `guard`s  
```haskell
absolute x
  | x<0 = -x
  | otherwise = x
```  
Syntax Notes:  
the first line loses the `-`, which moves to the guard lines  
`otherwise` guard always goes last, as the catch-all. Like the `default` case in C-style `switch` statements.  
Consider golf scoring:  
```haskell
holeScore :: Int -> Int -> String
holeScore strokes par
  | strokes < par = show (par-strokes) ++ " under par"
  | strokes == par = "level par"
  | strokes > par = show(strokes-par) ++ " over par"
```  
This is a bit untidy.  
```haskell
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | score > 0 = show(score) ++ " over par"
 where score = strokes-par
```  
Notice: score is available for all three guards.  

**Case expressions**  
Cases allow you to pick responses for various algebraic data types.  
It looks like a guard, but it is actually pattern matching!  
Given this data: `data Pet = Cat | Dog | Fish`, here is a greeting function:  
```haskell
hello :: Pet -> String
hello x =
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
```  
Syntax Notes:  
the pattern is followed by an arrow & a value.  
align the patterns!  
```haskell
hello :: Pet -> String
hello x =
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name
```  
Now the pattern for Parrot includes the variable name.  
In `case`, the equivalent for the guard's otherwise is `_`, or 'match everything'  

#### 4.4 Dealing with Uncertainty

#### 4.5 Idiomatic Haskell Quiz

### Parsing Text
#### 4.6 Parsing Text Using Higher-Order Functions
#### 4.7 Parsing Using Parsec: A Practical Example
#### 4.8 Parser Puzzles Quiz
#### 4.9 Summary
### Am I Right?
#### 4.10 Check My Program is Correct
#### 4.11 Using QuickCheck
#### 4.12 Talk with a Haskell Teacher
