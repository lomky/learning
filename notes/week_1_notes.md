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

### Section 1.3 - Erlang and functional programming video (03:39)

### Section 1.4 - Erlang in the functional programming landscape video (05:43)

### Section 1.5 - Using the Erlang system video (05:47)

### Section 1.6 - Installing Erlang article

## Programs in Erlang

> In this activity we'll cover the basics of programs in Erlang, and you'll write your first Erlang program. We'll consider types of data in Erlang and the use of variables and patterns.


### 1.7 - The basics of Erlang programming video (06:03)

### 1.8 - Introducing more complicated functions video (05:43)

### 1.9 - My first Erlang program article

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
