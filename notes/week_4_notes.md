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

#### Types

  - a particular data structure
  - all languages come with some built in types
  - these types can be used to build other data structures (types)
  - JS has 7 built in types; 6 primitive types and 1 Object type

#### Object Type

`Object` is a collection of name/value pairs

e.g.:

    ```
    firstName: "Kat",
    lastName: "T",
    social: {
      linkedin: "foobar",
      twitter: "hahNo",
      facebook: "yup"
    }
    ```
name is a string, but the value can be anything, including another object

#### Primitive Types

Primitive type represents a _single, immutable_ value

  - Single value, i.e. __not__ an object.
  - Once set, the value cannot be changed.
     - Read only. Memory space is set.

##### Primitive Type: Boolean

Can only have two valies, `true` or `false`.  
`true` and `false` are reserved words in JS

##### Primitive Type: Undefined

Undifined disnifies that no value has ever been set.

  - This is hte value set when JS sets up the variable.
  - Only valid value is `undefined`
  - You _can_ set a variable to `undefined`, but you _should NEVER do it_
    - it should mean the item has never been defined, so you are undermining the core meaning.

##### Primitive Type: Null

Null signifies hte lack of value

  - As opposed to `undefined` which is a lack of definition
  - Can only have one value: `null`
  - Perfectly okay to explicitly set things to `null`

##### Primitive Type: Number

Number is the only numeric type in JS

  - Always represented under the hoo as double-precision 64-bit floating point
  - JS does _not_ have an integer type.
    - integers are a subset of doubles

##### Primitive Type: String

String is a sequence of characters used to represent text.

  - can use either single or double quotes to define it
    - 'string' and "string" are fine

##### Primitive Type: Symbol

Symbol is new to ES6(ECMA Script 6) - Not covered in this class.

ES6 was released in 2015 and isn't widely supported/used yet.

Goolging for a real answer:

A symbol value may be used as an identifier for object properties; this is the data type's only purpose

Similar to Ruby symbols?

#### Examples of use

Went through examples in the console:

  - after decalring `var x;`, `x == undefined` returns `true`
  - after assigning a value to `x`, that returns `false` and x has a value

What if we remove the `var x`? We get an error `not defined`.

  - `not defined` is distinct from `undefined`
  - `not defined` means the variable hasnt even been declared.
  - `undefined` means the variable has been declared - space has been made in memory for it.

### Lecture 43: Common Language Constructs

#### String Concat

  - accessed via the `+` operator
  - can be done via `var1 + var2` or `var1 += var2`
  - the `+=` stores the value in `var1`, whereas the `+` operator leave the strings alone


#### Math Operators

Very straightforward and expected as programming langauges go.

  - normal operators:
    - `+ - * /` all valid here
    - `()` change precedence as expected
  - `NaN` is a special symbol indicating "Not a Number"
    - occurs when you attempt to do numeric things to non numbers

#### Equality

  - `=` is the assignment operator. No comparison.
    - `var x = 4, y = 4;` -- valid
  - `==` is a comparison operator
    - `if ( x == y )` returns `true` or `false
    - `if ( "4" == y )` comparing across types?
      - this does _type conversion_
      - returns `true`
  - `===` is _strict_ equality
    - `4 == 4` is `true`
    - `"4" == 4` is `false`


#### JS `false` and `true`

  - what things are `false` in Javascript?
    - `false`
    - `null`
    - `undefined`
    - `""`               # empty string
    - `0`
    - `NaN`
  - what things are `true` in Javascript?
    - `true`
    - "hello"            # nonempty string
    - `1`
    - `-1`
    - `"false"`          # _string_, not boolean
  - You can check whether something is `true` or `false` in the console
    - `Boolean()`
      - `Boolean(null);` evals to `false`
      - `Boolean("Hi!");` evals to `true`
      - this is actually just javascript type converting the value to boolean.

#### Best Practives for `{}`

  - Same line or New line? Is this just a style choice?
    - may cause religious wars in other languages
    - but in JS, it _is a best practice_
  - **Same Line**
  - the JS engine can accidentally return `undefined` because the `;` at the end is optional


#### `for` loop

in js they look like so:

```
var sum = 0;
for ( var i = 0; i < 10; i++) {
  sum = sum + i;
}
console.log("sum of 0 thru 9 is: " + sum);
```

blah blah c-style for loops.

### Lecture 44: Handling Default Values

```
function orderChickenWith(sideDish) {
  console.log("Chicken with " + sideDish);
}

orderChickenWith("noodles");
orderChickenWith();
```

> Chicken with noodles
> Chicken with undefined


```
function orderChickenWith(sideDish) {
  if ( sideDish === undefined) {
    sideDish = "whatever!"
  }
  console.log("Chicken with " + sideDish);
}

orderChickenWith("noodles");
orderChickenWith();
```

> Chicken with noodles
> Chicken with whatever!


```
function orderChickenWith(sideDish) {
  sideDish = sideDish || "something";
  console.log("Chicken with " + sideDish);
}

orderChickenWith("noodles");
orderChickenWith();
```

> Chicken with noodles
> Chicken with something

Standard `||` assignment handling. Lazy.

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

