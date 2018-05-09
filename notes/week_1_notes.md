## Week 1 - Haskell First Steps

### Introduction
#### 1.1 Welcome to the Course

Course instructors are Wim Vanderbauwhede & Jeremy Singer, both from the University of Glasgow.  
The course focuses on Haskell specifically, but also functional programming across languages.  

### Haskell Basics: Expressions and Equations
#### 1.2 Basic Elements By Example

**Expressions**  
i.e. : v = ( b * b + ) / 2  
In Haskell, _everything_ is an expression! No statements.  

**Functions**  
Just like functions in python  
```python
def hello(name):
   return "Hello,"+name
```  
Haskell:  
```haskell
hello name = "Hello, "++name
```  
Function args do not need parens; concat is `++`, not overloaded `+`  

**Types**  
A C example:  
```c
int sq(int x, int y) {
    return x*x+y*y;
}
```  
Specifies that it takes two integers and returns an integer.  
Similar in haskell, but we do the type declaration first, then the function definition.  
```haskell
sq :: Int->Int->Int
sq x y = x*x+y*y
```  

**Anonymous Functions** aka _lambda_ functions  
similar to a javascript anon func:  
```javascript
var f = function(x,y){return x*y+x+y};
```  
Haskell has anon functions called _`lambda` functions_  
```haskell
f = \x y -> x*y+x+y
```  
These form the very basis of the haskell language.  

**Higher-order functions**  
Similar to perl functions, where you apply a function to another function:  
```perl
map sub ($x){$x*2+1}, [1..10]
```  
Haskell has both build in _higher-order functions_ and allows you to build your own.  
```haskell
map (\x -> x*2+1) [1..10]
```  

**Lists**  
```haskell
lst = ["A","list","of","strings"]
```  
Looks much the same as lists in ruby, JS, or Python.  
Very important component in haskell programming.  
Joining lists using the same `++` as earlier:  
```haskell
lst = [1,2] ++ [3,4]
```  

#### 1.3 Introduction to Expressions and Equations
##### Expressions
Unlike imperitive languages (`C`, `Java`) where there are **expressions** which denote computations (`2*x`) and **statements** that handle looping, conditional, and other control stuctures, **pure functional** programming languages have _no statements_ at all.  
There are no assignments, and no jumps. All computation is performed via expression evaluation.  

**Integer expressions**  
Expressions evaluate to a result.  
Our format: `expr` -- > _result_  
`2` -- > _2_  
`3+4` -- > _7_  
`3+4*5` -- > _23_  
`(3+4)*5` -- > _35_  

**Expression Syntax**  
  - Parenthesis group, just like in math  
    - all other uses are optional  
  - Standard operator precedence is followed.  
    - [Full operator precedence](https://www.haskell.org/onlinereport/exps.html)  

**Function applications**  
Expressions can contain function calls.  
Functions take arguments, perform computation, and produce result(s).  
E.g. `abs`  
`abs 5` -- > _5_  
`abs (-6)` -- > _6_  

**Parenthesis should be used only when needed, or to clarify precedence**  

**Multi-argument Functions**  
`min` and `max` take many args.  
They go after the function with whitespace in between. Do not use parens or commas.  
`min 3 8` --> _3_  
`max 3 8` --> _8_  

**Function Precedence**  
Functions bind tighter than anything else.  
`f x + 3` is `(f x) + 3`, **not** `f (x+3)`  
Must use parens to pass an expression to a function.  

##### Equations
**Equations can name values**  
`answer = 42`  
says the left and right side denote the same value.  
Left is the name to give a value to.  
`x = 5*y` - correct  
`2 * x = (3*x)**2` - **incorrect**, no reassignment in functional programming.  

**Equations _are not assignments_**  
Names can only be given _one value_  
'Variables' _do not vary_, in haskell they are _constant_  
```haskell
n = 1      -- yay
x = 3*n    -- just fine!
n = x      -- Wrong!
```  
No side effects in a pure functional language!  

**`n = n+1`?**  
In imperitive languages, this means: "add one to n and store that value back in n"  
_This is not what it means in haskell!_  
Here, it is an _equation_, it means: "compute n such that it is n+1".  
This fails, as it is logically impossible.  

**Install & Use Haskell**  
The interactive browser env is down.  
Installed the haskell interpreter:  

```
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> 3+4
7
Prelude> n = 1
Prelude> n
1
Prelude> f = \x y -> x*x+y*y
Prelude> f

<interactive>:7:1: error:
    • No instance for (Show (a0 -> a0 -> a0))
         arising from a use of ‘print’
         (maybe you haven't applied a function to enough arguments?)
    • In the first argument of ‘print’, namely ‘it’
         In a stmt of an interactive GHCi command: print it
Prelude> f 2 3
13
Prelude> f 1 1
2
```

#### 1.4 Do it Yourself: Expressions, Functions and Equations
Assignment site is down, no access to assignment to complete locally.  

#### 1.5 Quiz
Completed, 100%.

#### 1.6 Summary
_Expressions_ in haskell are similar to those in other langauges. This distinct thing is that _everything_ in haskell is an expression, rather than a mixture of statements and expressions. Unlike other languages where you can update a value (n=n+1), this is not right in haskell. Instead you are just making an always-failing comparision.

### Haskell Basics: Reduction, Functions and Lists
#### 1.7 More Basic Elements by Example
**Lambda functions**  
Anonymous Functions, those not given a name.  
Javascript:  
```javascript
var f = function(x,y){
    return x*x+y*y;
}
```  
Haskell:  
```haskell
f = \x y -> x*x+y*y
```  

**Let expressions**  
Blocks.  
Javascript:  
```javascript
function roots(a,b,c){
    det2 = b*b-4*a*c;
    det = sqrt(det2);
    root1 = (-b + det)/(2*a);
    root2 = (-b - det)/(2*a);
    return [root1, root2];
}
```  
this function defines a block of code where we interact with variables.  
Haskell:  
```haskell
roots a b c = let
    det2 = b*b-4*a*c;
    det = sqrt(det2);
    root1 = (-b + det)/(2*a);
    root2 = (-b - det)/(2*a);
   in [root1, root2];
```  

**Conditionals**  
`if`, `then`, `else`.  
Python:  
```python
def max(x,y):
  if x>y:
      return x
  else:
      return y
```  
Haskell has a similar if/else, but its an expression  
```haskell
max x y = if x > y then x else y
```  

**Case statement**  
ruby:  
```ruby
Red = 1
Blue = 2
Yellow = 3

color = set_color();
action = case color
  when Red then action1()
  when Blue then action2()
  when Yellow then action3()
end
```  
Haskell looks similar:  
```haskell
data Color = Red | Blue | Yellow

color = set_color
action = case color of
    Red -> action1
    Blue -> action2
    Yellow -> action3
```  

**Generics/Templates**  
Aka _template types_. Similar to Java or C++:  
```java
Map<String,Integer> set = new HashMap<String,Integer>();
```  
Haskell:  
```haskell
set :: Data.Map.Map String Integer
set Data.Map.empty
```  
Unlike Java, we get an immutable variable, not an object.  
To interact with this in Java:  
```java
set.put("Answer",42)
```  
Haskell you do:  
```haskell
set' = insert "Answer" 42 set
```  
Which binds the result of the insert call to a new variable, rather than updating in place a la Java.  

**Higher-order functions**  
Functions which take functions as arguments.  
Perl:  
```perl
map sub ($x){2*$x}, [1..10];
```  
Haskell is very similar:  
```haskell
map (\x->2*x) [1..10]
```  
These are used a lot in haskell, more than in imperitive languages.

#### 1.8 Reduction, Functions and Lists
##### Reduction
**Program Execution Model**  
Since we don't have statements, the normal model of execution by processing sequential statements falls down.  
We call the alternative mode used in functional programs _reduction_.  

**Reduction**  
The process of converting an expression to a simplier form.  
Reduce by simplifying one _reducible expression_, or _redex_ at a time.  
Each step is referred to as a reduction.  
Syntax: `expr` -reduction-> `new_expr`  
```haskell
 3 + (4*5)
-- >
 3 + 20
-- >
23
```  
This is the only way to execute functional programs.  

**Unique reduction path**  
Reduciton in which there is only one possible path to reduce!  
```haskell
 3 + (5 * (8-2))
-- >
 3 + (5 * 6)
-- >
 3 + 30
-- >
33
```  
Since each step only has one _redex_ avaiable, we only have one way to process this reduction.  

**Multiple reduction paths**  
Given an expression with multiple _redex_ available, there will be several reduction paths.  
```haskell
 (3+4) * (15-9)
-- >
 7 * (15-9)
-- >
 7 * 6
-- >
42
```  
or  
```haskell
 (3+4) * (15-9)
-- >
 (3+4) * 6
-- >
 7 * 6
-- >
42
```

**Regardless, _result_ doesn't depend on reduction path!**  
the _Church-Rosser theorem_ states:  

> Every terminating reduction path gives the same result

Therefore:  
  - Correctness does not depend on order of eval
  - compiler or programmer can change the order freely to improve performance without affecting the result
  - different expressions can be evaluated in parallel, without affecting the result
    - _As a result, functional languages are leading contenders for programming future parallel systems_

##### Functions
As a functional language, functions are essential to the language.  
A function takes 1+ arguments & computes a result.  
Given the same arguments, the result will _always be the same_, like math functions.  
Thus, in haskell there are no side-effects.  
There are two fundamental operations on functions:  
  - function definition (creates the function)  
  - function application (using the function to compute a results).  

**Function definitions**  
Haskell Standard Library _prelude_ contains many predefined functions for our us.  

**Defining a function**  
Functions are defined by an _equation_  
```haskell
f = \x -> x+1   -- lambda function
-- or
f x = x+1       -- named function
```  
Equivalent to the mathematical notation: `f(x) = x + 1`  

**Function Application**  
How it works:  
  - function definition is an equation
  - lhs gives the name of the function
  - rhs (the body) is an expression of the parameters and the value of application, possibly using the parameters.
  - applications look like `f 31`, where `31` is the argument
  - application is evaluated by replacing it with the body of the function, replacing the parameter(s) with the argument(s).

##### Lists
These are a _key datastructure_ in haskell.  
It is a single value that contains several other values.  
Syntax: `['3', 'a']`, `[2.718, 50.0, -1.0]`

**Functions returning several results**  
A function can only return _one result_, but that can be a _list_!  
`minmax` returns the smaller and larger of two numbers, in order:  
```haskell
minmax = \x y -> [min x y, max x y]
minmax 3 8 --> [3,8]
minmax 8 3 --> [3,8]
```  

**Lazy Evaluation**  
Lists can be constant (`[2,4,6,8]`) or they can contain expressions.  
These expressions will not be evaluated until they are used.  
```haskell
answer = 42
yourlist = [7, answer+1, 7*8]
```  
Until you access the expression `yourlist`, it has not been evaluated!  

**Constructing Lists**  
Append `++` operator takes two lists and gives you a new one containing all the elements.  
```haskell
[23, 29] ++ [48, 41, 44] -- > [23, 29, 48, 41, 44]
```  
The length of the result is always the sun of the original lengths.  
and list appended with the empty list is itself.  
`[] + list = list = list + []`  

**Sequences**  
0, 1, ..., n  
In haskell you notate a sequence as such: `[0 .. 5]` gives `[0,1,2,3,4,5]`  
The increment is `1`.  

**Non numeric sequences**  
Any _enumerable type_ where there is a natural way to increment a value may be sequenced.  
Characters: `['a' .. 'e']` gives `['a','b','c','d','e']`  

**List comprehensions**  
High level notation for specifying the computation of a list.  
compiler transforms list comprehensions into expressions using list functions.  
inspired by _set comprehension_ from mathematics.  
For example:  
A set obtained by multiplying the elements of another set by 3 is {3×x|x←{1,…,10}}.  
`[3*x | x <- [1..10]]` --> `[3,6,9,12,15,18,21,24,27,30]`  
The set of even numbers is {2×x|x←N}  
`[2*x | x <- [0..10]]` -- > `[0,2,4,6,8,10,12,14,16,18,20]`  
The set of odd numbers is {2×x+1|x←N}  
`[2*x + 1 | x <- [0..10]]` -- > `[1,3,5,7,9,11,13,15,17,19,21]`  
The cross product of two sets A and B is {(a,b)|a←A,b←B}  
`[[a,b] | a <- [10,11,12] , b <- [20,21]]` -- > `[[10,20],[10,21],[11,20],[11,21],[12,20],[12,21]]`

**List Operations**  
Indexing a list uses the `!!` operator. Given a list and an index, it returns the element.  
`[5,3,8,7] !! 2` gives `8`  
Zero indexed.  
Negative or too large indexes produce the _undefined_ return  
Robust programming ensures the all expressions are well defined or have the exceptions handled.  

**_head_ and _tail_**  
`head` returns the first and `tail` the rest of a list.  
These are generally best not used, instead using pattern matching.  

**Lists are lazy**  
`[1 .. ]` is a lsit of all possible integers.  
lists of extremely complex expressions are just fine.  
Until we need the value, nothing is evaluated.  
Erroneous expressions, too, will not matter _until evaluation_.

#### 1.9 Do It Yourself: Functions and Lists
Assignment site is down, no access to assignment to complete locally.  

#### 1.10 Quiz
Completed, 100%.

#### 1.11 Summary
Functions take argument(s) and compute a _result_. The same arguments will always produce the same result. No side-effects in haskell.  
Lists are a key data structure. Very similar to lists in other languages, but in haskell they are immutable.

### Finding Out More
#### 1.12 Recommended Reading
[Real World Haskell](http://book.realworldhaskell.org/read/)  
[Learn You a Haskell](http://learnyouahaskell.com/chapters)  
Programming in Haskell - [purchase link](http://www.cambridge.org/9781316626221)  
Haskell: the Craft of Functional Programming - [purchase link](http://www.haskellcraft.com/craft3e/Home.html)  
Haskell Programming from First Principles - [purchase link](http://haskellbook.com/)  
