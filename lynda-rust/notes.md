# Rust

## Install & Hello World

`hello_world` files.

Install with:  
`curl https://sh.rustup.rs -sSf | sh`  
`rustup update`

Cargo automatically installed. Cargo is the pkg mng.

Rust files end in `.rs`.

Compile with `rustc`. 

## Cargo
`hello_cargo` files.
Build System & Package Manager

Cargo builds code, dl's dependency libs, 

Build a new binary app:  
`cargo new hello_cargo --bin`

`Cargo.toml` - Cargo config file

Source code goes in `/src`. Top level is for READMEs,
configs, etc.

`cargo build`

Creates the built project with code in `./target/debug`

`cargo run`

Build and then runs the code all in one.

`cargo check`

Test build but don't produce the compiled output.

## Variables & Mutability

`variables` proj

Variables values are set once declared, unless make mutable.

Interpolation is `{}`, also called a 'template'.

Attempting to reassign a variable results in compile error.

Mutable allows for the change with keywoir `mut`

This clarifies whether a value plans to change.

## Data Types
### Scalar
#### Ints
`integers.rs` file

Rust is statically typed, so all var types must be known at compile time.  
This can be _inferred_.

The scalar types are:  
Integer, Floating point numbers, booleans, characters.

Int can store 32b by default. If we store the largest value and try to add one,
we get a run time panic
```
thread 'main' panicked at 'attempt to add with overflow', integers.rs:3:9
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

Expicit typing can be done as well.
`let mut a:i64 = 3415345233415`

This gives us control of memory usage.

#### Floats, Bool

f32 and f64 floats available. f64 is the default.

Bool has `true` and `false`, explicit typed with `bool`.

#### Char
Unicode scalar. Char use "'".  
Can store emoji and extended characters by default, thank goodness.

### Compound
#### Tuples
`tuples.rs` file  

Groups together a variety of types.  
Comma seperated list in parens.   
Destructing breaks a tuple via pattern matching  
Element access via "."  

### Arrays
`arrays.rs` file

Groups together all the same type.  
The length is fixed.  
Accessed via `[]`  

Accessing outside the array compiletime errors. (used to be warn?)  

Array use case:  
  - Allocates to stack instead of heap
  - want fixed num of elements

Safety in Rust  
Rust _wants_ to have this level of strictness. Better to exit than allow outside bound memory access.

## Functions
`functions` proj

rust uses the `main` function as the entry point.  
`fn` is the keyword.  
snake_case is the preferred naming.  

order of declaration is not important.

Parameters are declard in the sig.

### Return values

Type must be specified.  
The final value is the returned val  

_leave off the semicolon on this line_  
Adding a semicolon turns the line into a statement,
making it return an empty tuple instead of the result.

## Control Flow

### If/else
`branches` proj

```rust
if a % 4 == 0 {
    println!("A is divisible by 4");
} else if a % 3 == 0 {
    println!("A is divisible by 3");
} else if a % 2 == 0 {
    println!("A is divisible by 2");
} else {
    println!("A is not divisible by 4, 3, or 2");
}
```

### Loops
`loops` proj

Keywords `loop`, `break`, `while`, `for`.

`for`  
```rust
    for element in a.iter() {
        unimplemented!();
    }
```
`array.iter()` creates an object which can be stepped through one at a time.

`for` does not deal with indexing, but loops through nicely.

## Ownership in Rust

### Memory Management review

Stack is LIFO stack of known-size values.  
Push on, pop off.  
Very fast, no searching for data.

Heap is unorganized, and the spaced required must be asked for.  
Accessed via a pointer to a memory value. Slower to get.  

Ints, Bools, & Strings are stored in the stack. More complicated ones go into the heap.

Pointers & References in C++ & C. Manually managing pointers is dangerous.  
Garbage collection algoritms (Java, Python, Ruby), heavy & complicated.  

Rust: *Ownership* - something in the middle.

### Ownership Rules
`ownership` proj

  1. Each value in Rust has a variable that's called its owner
  2. There can only be one owner at a time.
  3. When hte owner goes out of scope, the value will be dropped.

Scope is determined via the curly blocks.  
Out of scope variable access is a _compile time_ error.  

### Ownership: Memory allocation
`memory` proj

`let mut s = String::from("Hello");` - creates a string on the heap. Memory size not known at compile  
Manipulated with `push_str`  

Memory is requested from the OS at runtime.  
How do we return the memory when we're done with it?

We request the space ourselves. How is it returned?  

Allocates & free need to pair perfectly.  
**Memory is automatically freed when it leaves scope.**  
`drop` is called for us automatically.  

let s1 = String::from("hello"); //string on the heap
let s2 = s1;                    //_reference_ on the stack. ptr, length, and capacity.

s1 is a value on the stack, consisting of a `pointer` to the heap (addr), the `length`, and the `capacity`.
s2 is another value on the stack, with a copy of the pointer, length, and capacity. the 'string' values are copied, but the heap data is not.

Once s2 has been assigned the heap memory from s1, you _cannot access_ it through s1 anymore. Compile time error.  
This prevents double-free errors.  
s1 was _moved_ to s2.  
This is a _shallow_ copy. Deep copies, or the heap itself, are done with `clone`.

### References & Borrowing
`ownership-function` proj

To avoid losing ownership of a value, we can use a *reference*.  
Done by prepending `&` to the value.  

You may only have one mutable reference to a value in a given scope.

### Data race
`race-conditions` proj

Conditions:  

  1. Two or more pointers access the same data at the same time
  2. At least one of the pointers is being used to write to the data
  3. There's no mechanism being used to sync access to teh data

Rust prevents this from happening by making data races a compile tiem errors.

### Slices
`slices` proj

```rust
let s = String::from("Hello, World!");
let hello = &s[0..5]
```
from index up to but not including last index

In memory a slice is a pointer to the later address in the heap for the selected section.  

String literals are slices.

let s = "Hellos World";

&str is an immutable reference.

For non strings, you can also slice.

Arrays:  
`let a = [1,2,3,4,5];`  
`let slice = &a[1..3]; // type is &[i32]`  

## Structs
`structs` proj

### Uses of Structs

Custom data type.  
Groups together data in a meaningful group.  
Builds new data types.

Similar to javascript?

Similar to tuples, can have different types in it.  

Access is not ordered.

Compiler points out dead, unused code.

### Building struct from the values

See code.

Shortcut if hte param and field name match.

If you want to use the same as before, specify the distinct values and pass `..user1` to use the rest matching it.

### Real life struct
`rectangles` proj

## Methods
`methods` proj

Similar to functions. Also declared with `fn`, belong to structs. First param is `&self`.  

Methods are added via the keyword `impl`

## Enums
`enums` proj

Enumerations of various options.  
`option` enum allows for null.

Good over structs when one and only one.

Enums can hold various types:  
```rust
enum Message {
    Quit,
    Move {x: i32, y: i32},
    Write(String),
    ChangeColor(i32,i32,i32),
}
```

Enums can also havbe methods.

`option` - used in the situation where a value could be something or nothing.  
```rust
enum Option<T> {
    Some(T),
    None,
}
```

Can be used to handle failure at times, instead of panic.  

`T` is a generic type parameter.

## Pattern Matching
`match` compares agains patterns.

Default match character is `_`, placed last.  


## Error Handling: Unrecoverable Errors

Two major categories in rust for errors are recoverable (i.e. File not found).
Unrecoverable errors are bugs, like accessing outside of the memory.  
Recoverable use type `Result<T,E>`, Unrecoverable `panic!` & stop execution.  

The panic macro causes the program to print a failure message, unwind, clean up the stack, and then quit.  
Most commonly caused by bugs.  


## Error Handling: Recoverable Errors

Errors where it's reasonable to report to the user instead of killing the program.  

Result enum handles these.

```rust
enum Result<T,E> {
    Ok(T),
    Err(E),
}
```

```rust
let f = File::open("hello.txt").unwrap();
```
`unwrap` means:
```rust
    let foo = match f {
        Ok(file) => file,
        Err(error) => {
            panic!("File was not found!");
        },
    };
```
Need a better message? use `expect`  
```rust
let f = File::open("hello.txt").expect("We don't have the file yet!");
```

## Guessing Game

To do IO?  
```rust
use std::io
let mut guess = String::new();
io::stdin().read_line(&mut guess)
           .expect("failed to read line!");
```

Random numbers? Not in std libs.  
```rust
extern crate rand;

use rand::Rng;

let target = rand::thread_rng().gen_range(1,101);
```

Documentation of the project & its dependencies:  
`cargo doc --open`

Comparing library?  
```rust
use std::cmp::Ordering;

```