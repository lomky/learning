# Rust Language Book Notes - Chapter 2

## Guessing Game Tutorial

Generated the game `cargo new guessing_game --bin`

### Prompt for a guess

```rust
use std::io;

fn main() {
    println!("Guess the number!");
    println!("Please input your guess.");

    let mut guess = String::new();

    io::stdin().readline(&mut guess)
        .expect("Failed to read line");

    println!("You guessed: {}", guess");
}
```

`use std::io;` is the library allowing for input/output. Some things are always imported by default, called _the 'prelude'_. Everything else needs to be used explicitly.

`fn main()` defines the entry point into the program, `println!` lines are printing to screen.

`let mut guess = String::new();`  
This is a let statement, creating a binding and assigning it a value. They are by default immutable, but `mut` makes it mutable. `guess` is not a name, but rather a _pattern_, to be explained later.  
`String` is a string type from the standard library. It is a growable UTF-8 encoded bit of text.  
`::new()` - `::` syntax is an 'associated function' of a particular type, meaning `new` is invoked on the concept of String, rahther than an instance of String. (aka static).  
This creates an empty string values to bind to `guess`

`io::stdin().read_line(&mut guess).expect("Failed to read line.");`  
`io::stdin()` calls stdin function from the io library. Returns a handle to the std in for the terminal (std::io::Stdin).  
`.read_line(&mut guess)` is a method on the handle. These are called on particular instances of an object (compare to assocaited functions). Here we have called on the handle with the argument `&mut guess`.  
`&mut guess` read_line doesn't take a string, but rather a `&mut String`. & references, like C. To get a reference to a mutable string, we call as &mut String, since &String would be immutable (default behavior). The argument must be mutable, allowing the method to set the value in the argument to return back. It also returns a value, `io::Result` to indicate success/failure.  
`.expect("Failed to read line");` is a method on the io::Result. given a failure it will `panic!` (crash) the program. If the result isn't checked, our compiler will warn us.

`//` initiates comments.

`println!("You guessed: {}", guess);` print macro. `{}` are placeholders for the argument `guess`.

And run it:

```
Wed May 10 16:17:37 ~/repo/rust_book/projects/guessing_game $ cargo run
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
    Finished dev [unoptimized + debuginfo] target(s) in 0.78 secs
     Running `target/debug/guessing_game`
Guess the number!
Please input your guess.
4
You guessed: 4
```

### Gen a Random Secret Number

#### Getting the Rand crate

Std lib doesn't have a rand, but there is a rand crate.

Add the crate to your project like so:

`Cargo.toml`:  
```
...
[dependencies]

rand="0.3.0"
```

##### Cargo & Versioning

Cargo uses SemVer. Yay.  
running `cargo build`: fetches the dependency `rand` and `libc`, which `rand` requires.  
Versioning: `Cargo.lock` will keep is on the exact same 9.9.X version, both here and everywhere else.  
To update the third digit version, we have to do so explicitly: `cargo update`. To update the first or second digit version, we must change the `Cargo.toml` file itself.

#### Updating our code with rand

```rust
extern crate rand;

use std::io;
use rand::Rng;

fn main() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1, 101);
    println!("The secret numebr is: {}", secret_number);

    println!("Please input your guess.");

    let mut guess = String::new();

    io::stdin().read_line(&mut guess)
        .expect("Failed to read line");

    println!("You guessed: {}", guess);
}
```

`extern crate rand;`: tells use we will be using this crate. Implicitly does a `use rand;`, which means we can use `rand::` to invoke rand items.

`use rand::Rng;` our methods require `Rng` to be in scope.  
methods are defined on '`traits`', and the method requires the `trait` to be in scope. More info needed. TODO


`let secret_number = rand::thread_rng().gen_range(1, 101);` & `println!("The secret numebr is: {}", secret_number);`  
`rand::thread_rng()` gives us a random number generator local to our execution thread.  
`gen_range` method is in scope because of the `use rand::Rng;`. the arguments 1 and 101 say give me a number between inclusive & exclusive, so this gives 1-100.  
The print line is for dev purposes.

##### Check it is working

```
Thu May 11 08:25:50 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
    Finished dev [unoptimized + debuginfo] target(s) in 0.40 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 75
Please input your guess.
3
You guessed: 3

Thu May 11 08:35:52 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 70
Please input your guess.
2
You guessed: 2

Thu May 11 08:35:57 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 64
Please input your guess.
4
You guessed: 4
```

### Comparing Guesses

#### Code changes

```rust
//snip

use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main () {
//snip

    printlin!("You guessed: {}", guess);

    match guess.cmp(&secret_number) {
        Ordering::Less    => println!("Too small!"),
        Ordering::Greater => println!("Too big!"),
        Ordering::Equal   => println!("You win!"),
    }
}
```

New use line to bring in Ordering.

`match...` lines. `cmp()` can be called on anything comparable, taking a reference to the thing to compare to and returns the Ordering type.  
Ordering is an enum of Less, Equal, Greater.  
Since we have three options, we check them each giving us three branches.  

#### Compiling?

This is not yet compilable:  
```
Thu May 11 08:36:00 [master !?]~/repo/rust_book/projects/guessing_game $ cargo build
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
error[E0308]: mismatched types
  --> src/main.rs:22:21
   |
22 |     match guess.cmp(&secret_number) {
   |                     ^^^^^^^^^^^^^^ expected struct `std::string::String`, found integral variable
   |
   = note: expected type `&std::string::String`
              found type `&{integer}`

error: aborting due to previous error

error: Could not compile `guessing_game`.

To learn more, run the command again with --verbose.
```

Our issue is type! guess is a string and secret_number was _assumed_ to be a `i32` number by the compiler.  
This became a problem when we wanted to _compare_ them. Rust can't compare different things.

To fix this, we add a line:

```rust
//snip

fn main () {
//snip
    io::stdin().read_line(&mut guess)
        .expect("Failed to read line");

    let guess: u32 = guess.trim().parse()
        .expect("Please type a number!");

    printlin!("You guessed: {}", guess);
//snip
}
```

with the new line, we are shadowing guess into itself as a new type.  
`guess.trim().parse()`, `trim` removes white space (like our newline). `parse` on a string looks for a number, but not a specific one.  
We tell rust we want a u32 in the left half: `let guess: u32`.  
The `:` prepares rust for a type annotation. `u32` says unsigned 32bit int.  
Expect causes a crash on bad `parse`.  

#### Current working progress?

```
Thu May 11 08:53:45 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
    Finished dev [unoptimized + debuginfo] target(s) in 0.76 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 44
Please input your guess.
40
You guessed: 40
Too small!
Thu May 11 08:54:43 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 56
Please input your guess.
56
You guessed: 56
You win!
```

### Making it loop

#### Add a loop

`loop` causes infinite looping:

```rust
//snip

fn main () {
//snip
    printlin!("The secret number is: {}", secret_number);

    loop {
        printlin!("Please input your guess");

        let mut guess = String::new();

        //snip
    }
}
```

#### Endless game:

The only way to get out now is a non-number panic!.

```
Thu May 11 08:54:47 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
    Finished dev [unoptimized + debuginfo] target(s) in 1.3 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 70
Please input your guess.
22
You guessed: 22
Too small!
Please input your guess.
99
You guessed: 99
Too big!
Please input your guess.
70
You guessed: 70
You win!
Please input your guess.
44
You guessed: 44
Too small!
Please input your guess.
asdf
thread 'main' panicked at 'Please type a number!: ParseIntError { kind: InvalidDigit }', src/libcore/result.rs:859
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

#### Make it quit on win

```rust 
//snip


#### Make it quit on win

```rust 
//snip


#### Make it quit on win

```rust 
//snip


#### Make it quit on win

```rust 
//snip
fn main() {
    //snip
    loop {
       //snip

       match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal   => {
                println!("You win!");
                break;
            }
        }
    }
}
```

Now on correct guess we break the `loop`, and then exit as its the last command.

Let's also ignore non-number entries:

```rust 
//snip
fn main() {
    //snip
    loop {
       //snip

       let guess: u32 = match guess.trim().parse() {
           Ok(num) => num,
           Err(_) => continue,
       };

       //snip
    }
}
```

So the `Result` from parse is also an enum, `Ok` and `Err` for good and bad, respectively.  
For an Ok num, set the number.  
for an Err case, we don't care about the type, so ask for `_` catch all.  
`continue` says go to the top of the loop immediately.


#### Final Dev Game:

```
Thu May 11 09:07:07 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
    Finished dev [unoptimized + debuginfo] target(s) in 0.40 secs
     Running `target/debug/guessing_game`
Guess the number!
The secret numebr is: 67
Please input your guess.
1
You guessed: 1
Too small!
Please input your guess.
99
You guessed: 99
Too big!
Please input your guess.
foo
Please input your guess.
67
You guessed: 67
You win!
Thu May 11 09:07:44 [master !?]~/repo/rust_book/projects/guessing_game $
```

#### Final Game

No more cheating!

```
Thu May 11 09:07:44 [master !?]~/repo/rust_book/projects/guessing_game $ cargo run
   Compiling guessing_game v0.1.0 (file:///Users/37580/Documents/GitHub_Repos/rust_book/projects/guessing_game)
    Finished dev [unoptimized + debuginfo] target(s) in 0.38 secs
     Running `target/debug/guessing_game`
Guess the number!
Please input your guess.
50
You guessed: 50
Too big!
Please input your guess.
25
You guessed: 25
Too small!
Please input your guess.
35
You guessed: 35
Too small!
Please input your guess.
42
You guessed: 42
Too small!
Please input your guess.
47
You guessed: 47
Too small!
Please input your guess.
48
You guessed: 48
You win!
Thu May 11 09:08:49 [master !?]~/repo/rust_book/projects/guessing_game $
```
