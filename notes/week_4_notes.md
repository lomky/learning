# Coursera HTML, CSS, and Javascript for WebDevs - Week 4 Notes

## Javascript Basics

### Lecture 40 - Part 1: Adjusting the Dev Env for JS Dev

Rearranging the workspace for sublime concerns.  
Sublime tries to help with autocompletion, but it's going to get our various lecture files confused. Opening only the Lecture we're on in the workplace is the way to avoid this.  
Oh, he just means 'Opening a Folder'. But do it at the individual lecture instead of the examples folder.  

Start browser sync in the all files mode:  
`browser-sync start --server --directory --files "**/*"`  

We no longer care as much about the page. But in chrome just hit `ctrl`+`alt`+`i` and go to the console tab.  
This tab shows the javascript and any errors. Go ahead and pop the console out completely for future use.  

Blah blah dev env trivia. Probably super useful for new devs.

### Lecture 40 - Part 2: Where to place JS code

Where does JS go for your site? Several options.

  - Inside the `head` tab, create a `script` tag. Everything in the `script` tag is the JS.
  - Also, you can include an external script. `script` tag again, but with an attribute `src` pointing to your file. 
  - **NB**: script tabs must be closed.
  - **NB**: JS is processed sequentially

Try out some JS:

Inside the external script file, set `var x="Hello World!"`
Inside the `script` section, write `console.log(x)`

And now, the browser console prints "Hello World" when you load the page

**NB**: JS is processed in a single thread.

You can also define JS in the `body`, in another `script` tag.

At the end let's put `console.log("Right before body close tag")`  
And now the log shows both messages, in order

### Lecture 41: Defining Variables, Function, and Scope

#### Variables

`var message = "hi";`

  - no types, JS is dynamically typed.
    - type is determined at RunTime
    - type can change

#### Functions

```function a () { ... }```  
or  
```var a = function () { ... }```

 - Both of these declare a function to be invoked by the name `a`
 - **NB**: in the latter case, it's the _function_ assigned to `a`, not the result of the function execution!

Calling a function:  
`a();`

  - parens are required.
  - AKA 'invoking' the function

Arguments are defined in the definition:

```
function compare (x, y) {
  return x > y;
}
```

Various legal ways to use functions:

  - `function compare (x, y) { ... }` # declaration
  - `var a = compare(4, 5);`          # invoke and store result in a
  - `compare( 4, "a");`               # invoke and throw away the answer
  - `compare();`                      # all arguments are optional in JS

#### Scope

JS has two scopes:

  - global
    - variables and functions declared globally are available everywhere
  - functions aka lexical
    - variables _and functions_ only available within the functions
    - in JS, only function boundaries define scope.
    - **NB**: NO BLOCK SCOPE IN JS.

#### Scope Chain

  - Everything in JS is executed in an _Execution Context_
  - Function invocation creates a new _Execution Context_
  - Each _Execution Context_ has:
    - its own _Variable Environment
    - special '`this`' object
    - Reference to its _Outer Environment_
  - Global scope lacks an _Outer Environment_, as it is the outmost.

The Scope Chain Process:

  - Referenced (NOT defined) variable will be search for in its current scope first.
  - If not found, the Outer Reference will be searched.
  - If not found, the Outer Reference's Outer Reference will be searched
  - If not found, ... 
  - If not found, the Global scope will be searched
  - If not found, the variable is `undefined`.

So, scoping is nested like russian dolls. Onions. Ogres.  
```
    ______________________________________
    | _Global_    ____________________   |
    | var x = 2;  |_Function A_      |   |
    | A();        | var x = 5;       |   |
    |             | B();             |   |
    |             |__________________|   |
    |             _____________________  |
    |             |_Function B_       |  |
    |             | console.log(x);   |  |
    |             |___________________|  |
    |____________________________________|
```

What is printed to the screen? 5 from Function A's scope, or 2 from Global Scope?
  - 2!
  - B() is _called from_ A(), but B() is _defined within_ global scope.
  - **Execution does not matter for scope**, only where it is **defined**

#### Hands on lesson in the browser

  - define `var message = "in global"` in script.js
  - and right below:  
    `console.log( "Global: message = " + message);`

As expected, it prints "global: message = in global"

In the browser console, type `this`, and then `window`  
You get the same set of key value pairs.

  - define: 
    ```
    var a = function () {
        var message = "inside a";
        console.log("a: message = " + message);
    }
    a();
    ```

Now in the browser we see:
"global: message = in global"
"a: message = inside a"

And if we call B inside A?

  - update the code: 
    ```
    var a = function () {
        var message = "inside a";
        console.log("a: message = " + message);
        b();
    }
    function b() {
        console.log("b: message = " + message);
    }
    a();
    ```

Now in the browser we see:
"global: message = in global"
"a: message = inside a"
"b: message = in global"

And what if we define B inside A?

  - update the code:
    ```
    var a = function () {
        var message = "inside a";
        console.log("a: message = " + message);
        function b() {
            console.log("b: message = " + message);
        }
        b();
    }
    a();
    ```

Now in the browser we see:
"global: message = in global"
"a: message = inside a"
"b: message = inside a"


## Javascript Types and Common Language Constructs

### Lecture 42: Javascript Types

### Lecture 43: Common Language Constructs

### Lecture 44: Handling Default Values

## Objects and Functions in Javascript

### Lecture 45: Creating Objects Using 'new Object()' Syntax

### Lecture 46: Functions Explained

### Lecture 47: Passing Variables by Value vs by Reference

### Lecture 48: Function Constructors, prototype, and the 'this' Keyword

### Lecture 49: Object Literals and the 'this' Keyword

## Arrays, Closures, and Namespaces

### Lecture 50: Arrays

### Lecture 51: Closures

### Lecture 52 - Part 1: Fake Namespace

### Lecture 52 - Part 2: Immediately Invoked Function Expressions (IIFEs)

