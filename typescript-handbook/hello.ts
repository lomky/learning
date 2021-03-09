// https://www.typescriptlang.org/docs/handbook/intro.html
//******************************************
//The Basics
//******************************************
//greets the world
//console.log("Hello world!");

//This is an industrial-grade general-purpose greeter function:
function greet(person: string, date: Date) {
  console.log(`Hello ${person}, today is ${date}!`);
}

greet("Madison", new Date());
//
////vim-ts protip - keybind to show docs is K
//let msg = "hello there!";


//******************************************
//Everyday Types
//******************************************

let obj: any = { x: 0 };
// None of these lines of code are errors
obj.foo();
obj();
obj.bar = 100;
obj = "hello";
const n: number = obj;

function getFavoriteNumber(): number {
  return 26;
}

// The parameter's type annotation is an object type
function printCoord1(pt: { x; y: number }) {
  console.log("The coordinate's x value is " + pt.x);
  console.log("The coordinate's y value is " + pt.y);
}
printCoord1({ x: 3, y: 7 });


function printName(obj: { first: string; last?: string }) {
  // ...
}
// Both OK
printName({ first: "Bob" });
printName({ first: "Alice", last: "Alisson" });


function printName2(obj: { first: string; last?: string }) {
  //Object is possibly 'undefined'.
  console.log(obj.last.toUpperCase());

  if (obj.last !== undefined) {
    console.log(obj.last.toUpperCase());
  }

  // A safe alternative using modern JavaScript syntax:
  console.log(obj.last?.toUpperCase());
}


function printId(id: number | string) {
  console.log("Your ID is: " + id);
}
// OK
printId(101);
// OK
printId("202");
// Error
//printId({ myID: 22342 });


//function printId2(id: number | string) {
//console.log(id.toUpperCase());
//}


function printId3(id: number | string) {
  if (typeof id === "string") {
    // In this branch, id is of type 'string'
    console.log(id.toUpperCase());
  } else {
    // Here, id is of type 'number'
    console.log(id);
  }
}

function welcomePeople(x: string[] | string) {
  if (Array.isArray(x)) {
    // Here: 'x' is 'string[]'
    console.log("Hello, " + x.join(" and "));
  } else {
    // Here: 'x' is 'string'
    console.log("Welcome lone traveler " + x);
  }
}


// Return type is inferred as number[] | string
function getFirstThree(x: number[] | string) {
  return x.slice(0, 3);
}


type Point = {
  x: number;
  y: number;
};

// Exactly the same as the earlier example
function printCoord(pt: Point) {
  console.log("The coordinate's x value is " + pt.x);
  console.log("The coordinate's y value is " + pt.y);
}

printCoord({ x: 100, y: 100 });



//interface Window {
//  title: string
//}
//
//interface Window {
//  ts: TypeScriptAPI
//}
//
//const src = 'const a = "Hello World"';
//window.ts.transpileModule(src, {});

//use interface until you need a type's features

const myCanvas1 = document.getElementById("main_canvas");
const myCanvas2 = document.getElementById("main_canvas") as HTMLCanvasElement;
//HTMLElement vs HTMLCanvasElement

function printText(s: string, alignment: "left" | "right" | "center") {
  // ...
}
printText("Hello, world", "left");
//printText("G'day, mate", "centre")

function compare(a: string, b: string): -1 | 0 | 1 {
  return a === b ? 0 : a > b ? 1 : -1;
}

interface Options {
  width: number;
}
function configure(x: Options | "auto") {
  // ...
}
configure({ width: 100 });
configure("auto");
//configure("automatic");


const req = { url: "https://example.com", method: "GET" };
//handleRequest(req.url, req.method);


function liveDangerously(x?: number | undefined) {
  // No error
  console.log(x!.toFixed());
}


//******************************************
//Narrowing
//******************************************

function padLeft(padding: number | string, input: string): string {
  if (typeof padding === "number") {
    return new Array(padding + 1).join( " ") + input;
  }
  return padding + input;
}
// smart engine! once you create the narrow, it understands the narrowing of the type scope
// also understands some JS quirks, like null is obj


//lookit this truthy pretty
// = fixes indent
function printAll(strs: string | string[] | null) {
  if (strs && typeof strs === "object") {
    for (const s of strs) {
      console.log(s);
    }
  } else if (typeof strs === "string") {
    console.log(strs);
  }
}

function multiplyAll(
  values: number[] | undefined,
  factor: number
): number[] | undefined {
  if (!values) {
    return values;
  } else {
    return values.map((x) => x * factor);
  }
}


