# Functional Programming in Erlang - Week 1: Getting started with Erlang - Notes

## An introduction to Erlang

> Welcome to the course! In this first activity you'll get an opportunity to introduce yourself, and we'll look at the background to functional programming and Erlang. You'll also download and install Erlang to your own machine.


### Section 1.1 - Welcome video

Instructor: Simon Thompson, University of Kent, School of Computing  
Instruction support by Stephen Adams

Week Goal: R/W/X Erlang programs.

Erlang programs consist of modules, each containing functions.  
Run by using the functions in the Erlang shell.  
Iteration vs recursion.  

Other resources:  
  - Armstrong, J. (2013) Programming Erlang: Software for a concurrent world. (2nd ed.) Pragmatic Programmers.
  - Cesarini, F. & Thompson, S. (2009) Erlang Programming. Sebastopol, CA: O’Reilly.
  - Hébert, F. (2013) Learn You Some Erlang for Great Good! San Francisco, CA: No Starch Press.
    - [Link to free online edition and further resources](http://learnyousomeerlang.com/)

### Section 1.2 - Why Erlang? video

Overview of the history of Erlang (1980s, Sweden).

Goals:  
  - Complex HW SW combinations
  - Highly concurrent env, 10K-100K simultaneous calls
  - OS thread too heavy, needed lightweight
  - failure tolerant
    - Hard failure - actual component failures
    - this led to the "let it fail" philosophy which pervades Erlang.
  - system must be up all the time
    - no way to take it down to upgrade

Language of
  - lightweight processes
  - ideas of supervision
    - processes don't deal with their own failure - another seperate component deals with it.
  - 99.9999% availability
  - new code can swap in for a currently running piece

Functional was the best way of programming to build upon for these needs.

Since the 1980s
  - OTP (open telegraph protocol?)
    - can build a server, a Finite State Machine
    - can focus on your single goal

Erlang is open source now! Managed by original team still.

Examples in use across industry settings:
  - online betting
  - grindr
  - WhatsApp servers

#### Further Reading

 - [Erlang.org - official Erlang site](erlang.org)
 - ['Erlang the movie' on YouTube](https://youtu.be/xrIjfIjssLE)
 - ['Inside Erlang' - creator Joe Armstrong tells his story (Ericsson)](https://www.ericsson.com/news/141204-inside-erlang-creator-joe-armstrong-tells-his-story_244099435_c)
 - [Erlang Factory / Erlang User Conference](http://www.erlang-factory.com/)

### Section 1.3 - Erlang and functional programming video

#### Erlang is Functional

Erlang as a functional language, and other functional languages.  
Presumes knowledge of C, Java, etc  

Erlang is functional means that it works _like a calculator_.  
3 + 4 + 4 = 11  
But it's more complicated because instead of just +, -, etc we can have our own defined operations.  
We also have more complicated data types, like lists.

#### Why functional?

  - Because it's sinmpler. values. No references, pointers, etc
  - Functional supports high-level patterns, e.g. map/reduce
  - We don't get side effects

#### Immutability

  - Objects who state doesn't change
    - if you want a diff object, create one
  - Objects = Values in functional languages
  - Thread-safe programming!
    - safe caching and sharing
    - consistency

### Section 1.4 - Erlang in the functional programming landscape video

#### Pragmatic

  - At it's core, Erlang is functional  
    - but it does allow some side effects (e.g. communication)  
    - but not _others_, no state. no Java-style variables
  - Erlang has types
    - but they are ore free-wheeling approach than Java types
    - but it can use tools to check type correctness

#### Functional Programming Languages

  - Strongly-type, rich type languages
    - Haskell, Miranda
      - lazy evaluation 
      - pure functional!
        - cannot abide side effects with lazy
      - allows for infinite complex object structure.
    - ML, Ocaml, F#
      - strict evaluation
      - a little impure
  - Weakly typed, macros, `eval`
    - LISP, Scheme, Racket, Common Lisp
  - "functional" - has a `lambdas` function
    - Java, Javascript, Ruby, C++, ... (most all)
    - anonymous functions have proved so useful

### Section 1.5 - Using the Erlang system video

#### Running Erlang

`$ erl` puts you into the Erlang shell

Calculations

```erlang
> 3+4.
11
```

Need the `.` to end the input!

```erlang
> 3
> +
> 4
> .
11
```

Comparisons

```erlang
> 3<4.
true
> 3>4.
false
```

_Up arrow accesses the history._

Lists

```erlang
> [2,3,4].
[2,3,4]
> hd([2,3,4]).
2
```

Variables

```erlang
> A =[2,3,4].
[2,3,4]
> hd(A).
2
```

Forget a variable

```erlang
> f(A).
ok
> A.
* 1: variable 'A' is unbound
> f(). % Forgets _all bindings_
```

Functions as expressions

```erlang
> (fun (X) -> X+X end)(99).
198
```

To quit!

```erlang
> q().
ok
```


### Section 1.6 - Installing Erlang article

Turns out this is trivially easy on MacOS and on Linux Mint. Yay.

Mac:  
`brew install erlang`

Linux Mint:  
`apt install erlang`

## Programs in Erlang

> In this activity we'll cover the basics of programs in Erlang, and you'll write your first Erlang program. We'll consider types of data in Erlang and the use of variables and patterns.


### 1.7 - The basics of Erlang programming video

How to write programs in Erlang.

  - directly in the shell
  - from file
  - invoking files in the shell

Writing Examples in code. See [Erlang file](https://github.com/lomky/future-learn-erlang/blob/master/examples/week_1_section_1_7.erl)

Don't name files with `-`

load files into the shell:  
`c(file_name_less_dot_erl).`

Functions are invoked as fully named:  
`week_1_section_1_7:double(4)` %  8

Autocompletion works! Yay!

Remember to _export_ the things. If not, you can't invoke them.


```erlang
7> week_1_section_1_7:double(4).
8
8> week_1_section_1_7:mult(2,3).
** exception error: undefined function week_1_section_1_7:mult/2
```

Add the export:  
`export([double/1,mult/2]).`  
This is the name and the _arity_, which says how many args it takes.  

```erlang
11> week_1_section_1_7:mult(2,3).
6
```

### 1.8 - Introducing more complicated functions video

More writing examples. See [Erlang file](https://github.com/lomky/future-learn-erlang/blob/master/examples/week_1_section_1_8.erl)

multiline functions get `,` on each line. end with `.`

compiling erlang produces a `.beam` file.

### 1.9 - My first Erlang program article

#### Assignment 1_9
##### Modifying first.erl

In a text editor of your choice modify first.erl to include functions to square and to treble a value, and test these functions out by calling them from the Erlang shell.

##### Defining second.erl
Using your text editor of choice, define a new module second.erl to include the following functions:

  - Using the function square from first.erl, define a function that gives the size of the hypotenuse of a right-angled triangle given the lengths of the two other sides.
  - Define functions that give the perimeter and area of a right-angled triangle, given the lengths of the two short sides.
  - Which functions can you re-use in making these definitions?
      - For fun, I made square call mult() and treble call mult twice.
      - I reused Square in creating hypotenuse.
      - I reused mult in creating the Right Angle area
      - I used hypotenuse in the perimeter
  - Which existing definitions can you modify to give you the answers?

##### Workings

See workings on [github](https://github.com/lomky/future-learn-erlang/tree/master/assignments/1_9)

### 1.10 - Erlang data: numbers and atoms video

#### Erlng Data - Types

Numbers
**Atoms**
Booleans
**Tuples & lists**
Strings
**Functions**


#### Numbers : Integers and Floats

Integers are "bignums": arbitrarily large with full precision

  - you can make huuuge numbers without issue.

Different base are easy: `base#number`: `2#100.` gives `4`

Numbers use all the expected operators: `+ - * / div rem`

`div` is for integrer division

#### Atoms : "it's just a piece of data"

An atom is just an atom... it stands for itself

```erlang
> foo.
foo
> 'I am an atom'.
'I am an atom'
```

Atoms can be compared for equality, ordering...

```erlang
> foo == 'I am an atom'.
false
> foo > 'I am an atom'.
true
```

Two special atoms, the booleans: `true` and `false`

Atoms can be used in pattern matching.


### 1.11 - Erlang data: tuples, lists and functions video

#### Tuples: putting values together

A tuple has a number of pieces of data

```erlang
{"Joe","Armstrong",55}
{1.2,1.5}
{% raw  %}{{1.2,1.5},{2.4,5.0}}{% endraw %}
```

A common Erlang idion: use the first field to indicate what sort of data in the tuple

```erlang
{rectable,{1.2,1.5},{2.4,5.0}}
{circle,{1.1,2.1},3.0}
```

This is in contrast to other languages that create new objects.

Note that tuples are heterogenous - you can mix types inside it.

#### Lists: collections of values

A list of a number of pieces of data  
```erlang
["Joe","Smith"]
[{1.2,1.5},{2.1,4.2}]
[]
[[1,2,3]]
```

Typically, lists are homogenous: all the elements have the same type.  
A list is like the collection types in Java

#### Tuples _and_ Lists: why?

Both tuples and lists are collections of values, and both _can_ have elements of different types.

```erlang
["Joe","armstrong",21]
{"Joe","armstrong",21}
```

So why do we need both?  

Because we can do _different things_ with lists and tuples.  

Lists are built up one element at a time, they can be iterated through. Tuples are built in one go.  
More on this in Week 2.

#### Strings are just lists

A string is just a list of characters

```erlang
1> "abc".
"abc"
2> [97,98,99].
"abc"
3> [$a,$b,$c].
"abc"
```

The ASCII code for a character, e.g. c, is given by `$c`

#### Functions

Functions can be data themselves.

```erlang
30> fun (x) -> x*2 end.
Fun<erl_wval.6.80484245>
```

For instance, can be arguments of other funcitons

```erlang
31> lists:map(fun (X) -> X*2 end, [1,2,3,4]).
[2,4,6,8]
32> lists:foldr(fun(X,Y)->X*Y end,1,[1,2,3,4,5,6]).
720
```

Very powerful data manipulation: map-reduce (Hadoop) and D3 (JS). More Later.

### 1.12 - Erlang data in practice article

Here we’ll do some programmed interaction with the Erlang shell.

The aim of this exercise is to familiarise you with some of the details of data in Erlang, through trying out a whole lot of expressions in the Erlang shell.

Try evaluating these expressions: remember to follow each expression with a full stop when you type them into the Erlang shell. As you try this out, see if you can predict what should happen before you do it. Does everything behave as you expect? Share your thoughts in the comments.

```erlang
% Command : not true.
% Prediction: false
1> not true
false

% Command : true and false.
% Prediction: false
2> true and false.
false

% Command : length([2,3,4,5]).
% Prediction: 4
3> length([2,3,4,5]).
4

% Command : [2,3]++[[4]].
% Prediction: [2,3,[4]]
4> [2,3]++[[4]]
4> .
[2,3,[4]]

% Command : (fun (X) -> X+2 end)(40).
% Prediction: 42
5> (fun (X) -> X+2 end)(40)
5> .
42

% Command : fun (X) -> X+2 end(40).
% Prediction: error? 
6> fun (X) -> X+2 end(40).
42

% Command : 2#100.
% Prediction: 4
7> 2#100.
4

% Command : 100#2.
% Prediction: 0.2
8> 100#2.
* 1: illegal base '100'

% Command : 34#2.
% Prediction: 2
8> 34#2.
2

% Command : 2#34.
% Prediction: Error
9> 2#34.
* 1: illegal integer

% Command : [97,98,99].
% Prediction: "abc"
9> [97,98,99].
"abc"

% Command : [$h,$e,$l,$l,$o].
% Prediction: "hello"
10> [$h,$e,$l,$l,$o]
10> .
"hello"

% Command : [$h,$e,$l,$l,o].
% Prediction: "hello"
11> [$h,$e,$l,$l,o].
[104,101,108,108,o]

% Command : [34,3,5,36,37].
% Prediction: [34,3,5,36,37]
12> [34,3,5,36,37].
[34,3,5,36,37]

% Command : [34,35,36,37].
% Prediction: [34,35,36,37]
13> [34,35,36,37].
"\"#$%"

% Command : false and (3 == (4 div 0)).
% Prediction: false
14> false and (3 == (4 div 0)).
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  div/2
        called as 4 div 0

% Command : false andalso (3 == (4 div 0)).
% Prediction: error?
16> false andalso (3 == (4 div 0)).
false

% Command : true orelse (3 == (4 div 0)). 
% Prediction: true
17> true orelse (3 == (4 div 0)).
true

```

### 1.13 - Variables in Erlang video

#### Erlang so far

It works like a calculator. Computation is expression evaluation.  
Values come from these built in types: numbers, aroms, lists, tuples, and functions.

#### Erlang has Variables!

```erlang
1> A=2+3.
5
2> B=A-1.
4
```

But not like other variables!

```erlang
3> A=B+1.
5
4> A=B.
** exception error: no match of right hand side value 4
```

What's happening?

#### Erlanf has _single assignment_ variables

If a variable doesn't have a value already - it isn't _bound_ - then `=` gives it a value:  

```erlang
1> A=2+3.
5
2> B=A-1.
4
```

If it _is_ bound, then '=' is a _check on its value_, does the value on the right-hand side (RHS) match the value on the LHS?

```erlang
3> A=B+1.
5
4> A=B.
** exception error: no match of right hand side value 4
```
#### So how are variables used in Erlang?

  - We can use them to name values
    - `A=2+3.` `B=A-1.`
  - And use those variables in defining other values
    - `C=(A,B,A).`
    - Variables in Erlang are _names_. _Definitions_.
  - We can put _patterns_ on the LHS of an assignment and then use those variables in patterns.
    - `{A,B}={2,3}.` defines `B` distinctly
    - `{A,C}={2,5}.`
    - `{A,D}={6,6}.` Fails, as `A` already has a value! `D` gets no value.
  - We can put _repeatd variables_ in patterns
    - `{A,A}={2,2}.`
    - `{B,B}={2,5}.` Fails, `B` can't take two values.

### 1.14 - Pattern matching video

#### No updatable variab;es

In Java one might write:

```java
for (int i = 1; i <= 200; i++)
{
    if (i%2 == 0)
    {
      sum = sum + i;
    }
}
```

But that will not work in Erlang. In particular the statement `sum = sum + i` is nonsense.

But we can recreate this effect in a different way. Next Week.

#### Defining Functions for ourselves

The simplest function definitions look like:

```erlang
double(X) ->
  times(X,2).

times(X,Y) ->
  X*Y.
```

#### Pattern Matching & Multiple funcion clauses

Suppose we want to make a choice in defining a function

Instead of writing (just) a variables as an argment, we can write a literal, or even more complicated things.

```erlang
is_zero(0) ->
  true;  % Clause 1 ends with ;
is_zero(X) ->
  false. % Last Clause ends with .
```

Functions can have multiple clauses, separated by semi-colons.  
They are matched sequentially, use the _first_ that succeeds.

#### Boolean Or

Boolean or usually allows both possibilities to be `true`.  
In a cafe, you can have pizza or pasta, but not both.  
`XOR`!  

Defined like so:

```erlang
xOr(true,false) ->
  true;
xOr(false,true) ->
  true;
xOr(X,Y) ->
  false.
```

#### A common idiom: "don't care"

In the final "catch all" case there, `X` and `Y` aren't used in the definition.  
We don't care about their values, and we can replace them with `_`

```erlang
xOr(true,false) ->
  true;
xOr(false,true) ->
  true;
xOr(_,_) ->
  false.
```

#### Checking for equality

You can also use pattern matching to check equality between parts of patterns.  
In this case, between two arguments, we check if the args are the same (true or false), then they are false, otherwise true.

```erlang
xOr(X,X) ->
  false;
xOr(_,_) ->
  true.
```


### 1.15 - Variables and patterns in practice article

#### Assignment 1.15

The aim of this exercise is to give you experience of writing functions using pattern matching in Erlang, as well as to do some “hand calculation” too.

You can solve all of these questions just using pattern matching – in particular you don’t need any other mechanism for distinguishing between different cases.

We’ll provide some feedback for these exercises in the next step, but when you have completed the step, maybe you would like to discuss your approach, and compare it with what other learners have done, in the comments for this step?

##### Exclusive or

In the previous video step on pattern matching we saw two ways of defining “exclusive or”. Give at least three others. You might find it useful to know that:

  - `=/=` and `==` are the operations for inequality and equality in Erlang;
  - `not` is the Erlang negation function; and,
  - `and` and `or` are the Erlang conjunction and disjunction (infix) operators.

##### Maximum of three

Give a definition of the function maxThree which takes three integers and returns the maximum of the three. You can use the max function, which gives the maximum of two numbers, in writing your definition.

```erlang
maxThree(34,25,36) = 36
```

##### How many equal?

Give a definition of the function howManyEqual which takes three integers and returns an integer, counting how many of its three arguments are equal, so that:

```erlang
howManyEqual(34,25,36) = 0
howManyEqual(34,25,34) = 2
howManyEqual(34,34,34) = 3
```

##### Workings

See workings on [github](https://github.com/lomky/future-learn-erlang/tree/master/assignments/1_15)


### 1.16 - Summing up video

Reviewing the topics covered this week.

Another answer to xOr:

```erlang
xOr(true,X) ->
  not(X);
xOr(false,X) ->
  X.
```

Note that this does not insist on the type of the second argument!

### 1.17 - Programs in Erlang - recap quiz

  - Variables should be Initialcap
  - remember that single quotes `''` make atoms, not strings


## Functions in Erlang

> In this activity we'll consider functions in Erlang. We'll introduce the key concept of recursion - both 'direct' and 'tail' recursion - for defining functions, and further explore the ideas of pattern matching.


### 1.18 - Recursion on integers video

We don't have traditional loops, though we can replicate it.  
We rely on recursion for repetitive behavior.  

#### Getting started with Recursion

A first example of recursion.  
Factorial of `N` is the product of `1*2*3*...*(N-1)*N`  
Factorial of `0` is `1`  

And in Erlang:  

```erlang
fac(0) ->
  1;
fac(N) ->
  fac(N-1)*N.
```

And that's it!

You can write out a calc of this step-by-step yourself:  
`= fac(3)`  
`= fac(2)*3`  
`= (fac(1)*2)*3          ` Use the definition: replace the  
`= ((fac(0)*1)*2)*3      ` left-hand side by right-hand side  
`= ((1*1)*2)*3`  
`= (1*2)*3`  
`= 2*3`  
`= 6`  

What if we put a negative in?  
Oops. It will never reach the base case.  
We need to test for `N>0`  

#### Guards

```erlang
fac(0) ->
  1;
fac(N) when N>0 ->
  fac(N-1)*N.
```

So now what happens if we call `fac(-3)`?  
We get a _failure_ of pattern matching!  
This is an example of the Erlang philosophy: _let it fail!_  

We don't know what to do to a negative to apply factorial, but handling the error isn't up to us. We should just fail.  

You _can't_ put user defined functions in the guard. Why?  
Because patterb matching is desired to always terminate. You should always get a yes or no. So you can only use simple calculations.  
This also makes the guards compute quickly.  
In general, you can use `case` expressions to do this, more later.

#### Let it fail

if we write a function / process o do something, and this fails for some reason, we _let it fail_.

Why?

It's the responsibility f the caller, who called the funciton erroneously, to sort it out. We can't predict what the right option is inside the function.

#### Direct recursion

```erlang
fac(0) ->
  1;
fac(N) when N>0 ->
  fac(N-1)*N.
```

This definition is _direct_ recursion, it fives a direct description of what it does.  
The factorial of `0` _is_ `1`  
The factorial of `N` _is_ the procut of the factorial of `(N-1)` and `N`.  
We are directly describing the process of factorial.

### 1.19 - Recursion examples article

> The following exercises are designed to demonstrate your capability to use recursion and evaluation by hand in a number of practical examples.

#### Fibonacci sequence.

> Give a recursive definition of the function fib/1 computing the Fibonacci numbers, and give a step-by-step evaluation of fib(4).

##### Definition

```erlang
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).
```

##### Eval by Hand of fib(4)

```
  fib(4)
= fib(2) + fib(3)
= (fib(1) + fib(0)) + (fib(1) + fib(2))
= (fib(1) + fib(0)) + (fib(1) + (fib(1) + fib(0)))
= (1 + 1) + (1 + (1 + 1))
= 2 + (1 + 2)
= 2 + 3
= 5
```

#### How Many Pieces?

> Define a function pieces so that pieces(N) tells you the maximum number of pieces into which you can cut a piece of paper with N straight line cuts.

##### Definition

```erlang
howManyPieces(0) ->
  1;
howManyPieces(N) ->
  howManyPieces(N-1)+N;
```

Notes after research:  
  - should have put guards on to catch negative!

##### Expansion to 3D & _N_D

> If you’d like to take this problem further, think about the 3-dimensional case. Into how many pieces can you cut a wooden block with N saw cuts?

Scratch attempts:  

```erlang
howManyPieces3D(0) ->
  1; % 1
howManyPieces3D(1) ->
  2; % howManyPieces(N-1) 
howManyPieces3D(2) ->
  4; %
howManyPieces3D(3) ->
  8; %
howManyPieces3D(4) ->
  16; %

howManyPieces(N) ->
  howManyPieces(N-1)+N;
```

I'm not getting much of anywhere trying to brute force this. 

Research: 

Apparently, this is a really hard problem, in the math sense of hard! A bit mean to set us to work it casually!  
Notes on how to do this [here](http://www.math.toronto.edu/mccann/assignments/199S/cuttingplanes.pdf)  
A plausible solution a coursemate came up with (but no explanation):  

```erlang
pieces(0, _) -> 1;
pieces(X,1) -> X+1;
pieces(X, N) -> pieces(X-1, N) + pieces(X-1,N-1).
```

### 1.20 - Recursion examples - feedback video

Works through the fibonacci series process and the 2D cutting problem logic.  

Note: the definition we did for fib is a bit inefficient (calculate fib(2) twice!)

I'd really love to see the proof for the cut problem.


### 1.21 - Tail recursion video

#### Direct Recursion

```erlang
fac(0) -> 
  1;
fac(N) when N>0 ->
  fac(N-1)*N.
```

This definition is _direct_ recursion. It gives a direct description of what it does.

The factorial of 0 _is_ 1.
The factorial of N _is_ the product of the factorial of (N-1) and N.

#### Recursion revisited

```erlang
fac(N) ->
  fac(N,1).

fac(0, P) ->
  P;
fac(N, P) when N>0 ->
  fac(N-1,P*N).
```

What's going on here?  
`fac(3)`  
`= fac(3, 1)`  
`= fac(2, 3)`  
`= fac(1, 6)`  
`fac(0, 6)`  
`= 0`  

This is called _tail recursion_.  
The recursive call is the last thing in the body of the function (a "tail call").  
Can be implemented very efficiently, no stack involved.  
Just like a _loop_, and the parameters are like _variables_. One of them is collecting the result.

Equivalent to the loop:

```
P,N;
while (N>0) {
  P = P*N;
  N = N-1;
}
return P;
```

#### Looping the loop

```erlang
  loop (N) when N>0 ->
    io:format("~p~n",[N]),
    loop(N-1);
  loop(N) ->
    io:format("bye~n"),
```

What does this do?

```erlang
1>loop(3).
3
2
1
bye
ok
```

`io:format` - each ~p replaced with the corresponding value form the list of values  

#### Documentation 

Documentation of all Erlang modules can be found online at www.erlang.org/doc/  
All modules listed.  
Otherwise, just google erlang and the module name.

#### More examples to try

#### List of numbers

Given a function `F` from integers to integers,

  - find the _sum_ of the values `F(0), F(1),...,F(N-1),F(N)`
  - find the _maximum_ of the values `F(0), F(1),...,F(N-1),F(N)`

#### Fibonacci numbers
The Fibonacci sequence is given by 0, 1, 1, 2, 3, 5, … where subsequent values are given by adding the two previous values in the sequence.

The function fib/1 that we defined earlier is exponentially complex … ouch! Define an efficient Fibonacci function fib/3 using a tail recursion with two accumulating parameters that hold the last two Fibonacci numbers. Give a step-by-step evaluation of fib(4).

#### Perfect numbers
A positive integer is perfect when it is the sum of its divisors, e.g. 6=1+2+3, 28=1+2+4+7+14.

Define a function perfect/1 that takes a positive number N and returns a boolean which indicates whether or not the number is perfect. You may well want to use an accumulating parameter to hold the sum of the divisors “so far”.

#### Thoughts

I find thinking in tail recursion quite hard atm.

### 1.22 - Tail recursion - feedback video

See downloaded_example tail.erl

I will need more practice thinking in this methodology.

### 1.23 - Pattern matching revisited video

### 1.24 - Pulling it all together assignment

### 1.25 - Pulling it all together: review

### 1.26 - Pulling it all together: reflection

### 1.27 - Week one summary video
