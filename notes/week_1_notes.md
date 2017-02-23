# Functional Programming in Erlang - Week 1: Getting started with Erlang - Notes

## An introduction to Erlang

> Welcome to the course! In this first activity you'll get an opportunity to introduce yourself, and we'll look at the background to functional programming and Erlang. You'll also download and install Erlang to your own machine.


### Section 1.1 - Welcome video

Instructor: Simon Thompson, University of Kent, School of Computing  
Instruction support by Stephen Adams

Week Goal: R/W/X erlang programs.

Erlang programs consist of modules, each containing functions.  
Run by using the functions in the erlang shell.  
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

`$ erl` puts you into the erlang shell

Calculations

```
> 3+4.
11
```

Need the `.` to end the input!

```
> 3
> +
> 4
> .
11
```

Comparisons

```
> 3<4.
true
> 3>4.
false
```

_Up arrow accesses the history._

Lists

```
> [2,3,4].
[2,3,4]
> hd([2,3,4]).
2
```

Variables

```
> A =[2,3,4].
[2,3,4]
> hd(A).
2
```

Forget a variable

```
> f(A).
ok
> A.
* 1: variable 'A' is unbound
> f(). //Forgets _all bindings_
```

Functions as expressions

```
> (fun (X) -> X+X end)(99).
198
```

To quit!

```
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


```
7> week_1_section_1_7:double(4).
8
8> week_1_section_1_7:mult(2,3).
** exception error: undefined function week_1_section_1_7:mult/2
```

Add the export:  
`export([double/1,mult/2]).`  
This is the name and the _arity_, which says how many args it takes.  

```
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

### 1.10 - Erlang data: numbers and atoms video (05:21)

### 1.11 - Erlang data: tuples, lists and functions video (08:24)

### 1.12 - Erlang data in practice article

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
