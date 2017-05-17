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

```bash
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
#### 1.7
#### 1.8
#### 1.9
#### 1.10
#### 1.11
### Finding Out More
#### 1.12
#### 1.13
#### 1.14
