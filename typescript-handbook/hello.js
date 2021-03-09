//******************************************
//The Basics
//******************************************
//greets the world
//console.log("Hello world!");
//This is an industrial-grade general-purpose greeter function:
function greet(person, date) {
    console.log(`Hello ${person}, today is ${date}!`);
}
greet("Madison", new Date());
//
////vim-ts protip - keybind to show docs is K
//let msg = "hello there!";
//******************************************
//Everyday Types
//******************************************
let obj = { x: 0 };
// None of these lines of code are errors
obj.foo();
obj();
obj.bar = 100;
obj = "hello";
const n = obj;
function getFavoriteNumber() {
    return 26;
}
// The parameter's type annotation is an object type
function printCoord1(pt) {
    console.log("The coordinate's x value is " + pt.x);
    console.log("The coordinate's y value is " + pt.y);
}
printCoord1({ x: 3, y: 7 });
function printName(obj) {
    // ...
}
// Both OK
printName({ first: "Bob" });
printName({ first: "Alice", last: "Alisson" });
function printName2(obj) {
    //Object is possibly 'undefined'.
    console.log(obj.last.toUpperCase());
    if (obj.last !== undefined) {
        console.log(obj.last.toUpperCase());
    }
    // A safe alternative using modern JavaScript syntax:
    console.log(obj.last?.toUpperCase());
}
function printId(id) {
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
function printId3(id) {
    if (typeof id === "string") {
        // In this branch, id is of type 'string'
        console.log(id.toUpperCase());
    }
    else {
        // Here, id is of type 'number'
        console.log(id);
    }
}
function welcomePeople(x) {
    if (Array.isArray(x)) {
        // Here: 'x' is 'string[]'
        console.log("Hello, " + x.join(" and "));
    }
    else {
        // Here: 'x' is 'string'
        console.log("Welcome lone traveler " + x);
    }
}
// Return type is inferred as number[] | string
function getFirstThree(x) {
    return x.slice(0, 3);
}
// Exactly the same as the earlier example
function printCoord(pt) {
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
const myCanvas2 = document.getElementById("main_canvas");
//HTMLElement vs HTMLCanvasElement
function printText(s, alignment) {
    // ...
}
printText("Hello, world", "left");
//printText("G'day, mate", "centre")
function compare(a, b) {
    return a === b ? 0 : a > b ? 1 : -1;
}
function configure(x) {
    // ...
}
configure({ width: 100 });
configure("auto");
//configure("automatic");
const req = { url: "https://example.com", method: "GET" };
//handleRequest(req.url, req.method);
function liveDangerously(x) {
    // No error
    console.log(x.toFixed());
}
//******************************************
//Narrowing
//******************************************
function padLeft(padding, input) {
    if (typeof padding === "number") {
        return new Array(padding + 1).join(" ") + input;
    }
    return padding + input;
}
