# Rust Language Book Notes - Chapter 2

## Guessing Game Tutorial

Generated the game `cargo new guessing_game --bin`

Creating the main file:

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
