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
> f(). //Forgets _all bindings_
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
`week_1_section_1_7:double(4)` // 8

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
{{1.2,1.5},{2.4,5.0}
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

###e Tuples _and_ Lists: why?

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
#Fun<erl_wval.6.80484245>
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

### 1.13 - Variables in Erlang video (07:51)

### 1.14 - Pattern matching video (06:22)

### 1.15 - Variables and patterns in practice article

### 1.16 - Summing up video (07:01)

### 1.17 - Programs in Erlang - recap quiz


## Functions in Erlang

> In this activity we'll consider functions in Erlang. We'll introduce the key concept of recursion - both 'direct' and 'tail' recursion - for defining functions, and further explore the ideas of pattern matching.


### 1.18 - Recursion on integers video (11:45)

### 1.19 - Recursion examples article

### 1.20 - Recursion examples - feedback video (04:42)

### 1.21 - Tail recursion video (06:22)

### 1.22 - Tail recursion - feedback video (06:55)

### 1.23 - Pattern matching revisited video (06:28)

### 1.24 - ### ling it all together assignment -

### 1.25 - Pulling it all together: review review

### 1.26 - Pulling it all together: reflection reflection

### 1.27 - Week one summary video (03:09)
