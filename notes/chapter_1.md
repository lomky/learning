# Rust Language Book Notes - Chapter 1

[Reference Book](https://doc.rust-lang.org/book/getting-started.html)

Installed rust, no issues. Sourced path.

## Hello World

[Program](../projects/hello_world/main.rs):

```rust
fn main() {
        println!("Hello, world!");
}
```

Compile with: `rustc main.rs`, execute generated binary: `./main`  
As expected from a C-like

### Breaking it down

`fn main () { }` declares a function main with no args that returns nothing. Args go in parens. Uncertain where the return type goes, but probably in front? TODO

`    println!("Hello, World!");`  

  - indents with four spaces
  - `println!` is a _macro_, indicated by the `!`.
    - functions look similar, just no `!`
    - more info on what a macro vs function is to come.
  - `"hello, world!"` is a _string_
    - specifically, it is a _statically allocated~ string.
  - `;` ends the expression

Compiling and Running are distinct steps. `rustc` is similar to `gcc`, `g++`. Good for compiliing single files.  
Cargo is the tool for real-world situations.

## Cargo

Rust's build system & Package Manager.

(Rustaceans is a cute moniker).

Cargo tasks:  
  - Building the code
  - Downloading any libraries (dependencies) your code uses
  - building those libraries

Comes installed with rust.

## Moving our Hello World to Cargo

Three steps:
  - place the source file in the right dir
  - remove old exec
  - create cargo config file

```
$ cd projects/hello_world
$ mkdir src
$ mv main.rs src/
$ rm main
```

Cargo expects to find source files inside the `src` directory. top level is for READMEs, licenses, or other code-unrelated items.  
`main.rs` is the convention for creating an executable. `lib.rs` would create a library. (What if you have multiple libs?)

For the config, we create `Cargo.toml` in the `hello_world` dir.  
This is written in [TOML](https://github.com/toml-lang/toml)

```
[package]

name = "hello_world"
version = "0.0.1"
authors = [ "Kathryn Tipton <kathryn.tipton@gmail.com>" ]
```

First line declares it a package config. Next three are required info for Cargo.

## Build & Run Cargo Projects

`cargo build` to compile. `./target/debug/hello_world` to run.  
Or compile & run: `cargo run`

Cargo only recompiles on changes. Cool.  

Cargo is a bit much for simple projects, but really shines for building crates (packages, libraries).

Building for release: `cargo build --release` optimizes the code to run faster, at the cost of slower compiling.

## Other Cargo notes

`Cargo.lock` tracks the dependencies of the app. No manual intervention needed.

All of this can be generated as a skeleton with: `cargo new hello_world --bin`

```
Wed May 10 14:51:37 ~/repo/rust_book/projects $ cargo new hello_world_new --bin
     Created binary (application) `hello_world_new` project
Wed May 10 14:52:18 ~/repo/rust_book/projects $ ls -R hello_world_new
Cargo.toml      src

hello_world_new/src:
main.rs
```

Generated config values are based on the arguments and your global git config (used my work email).  
Also initialized the git repo and a hello world .rs file
