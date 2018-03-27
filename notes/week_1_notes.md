## Week 1 Notes

Objectives

  - Describe the use of Node for server-side development
  - Create a Node application
  - Demonstrate the use of Node modules in developing a Node application
  - Develop a simple REST API server using the Express framework

### Welcome to Server-Side Development with NodeJS, Express and MongoDB

#### Welcome to the Course

  - Overview of the course. Previous courses, etc.
  - Expected knowledge:
   - Javascript, ES5, ES 2015+
  - Angular Course to understand frontend
    - oh well! maybe later.

**Web Design & Dev**

Design:  
UI/UX  
Prototypes, graphics, etc  
Not this course  

Development, Building * Deployment  
  - Web UI Frameworks
  - Javascript framework
  - Hybrid Mobile Frameworks
  - *Server side Development* - This Course

**Covered in this Course**

Node.js & Node modules  
Express Framework  
MongoDB  
Backend as a Service (BaaS)  

**Module 1: Intro to Server-side Dev**

  - Big Picture
  - Setup Git (skip)
  - Intro to Node.js & NPM
  - Node Modules
  - Node & HTTP
  - Intro to Express
  - Assignment

**Module 2: Data**

  - Express Generator
  - Intro MongoDB (skim?)
  - Node <-> MongoDB
  - Mongoose ODM (Object Data Model)
  - REST API with Express, MongoDB, and Mongoose
  - Assignment 2

**Module 3: Authentication**

  - Basic Auth
  - Cookies & Express Sessions
  - User Auth w/ Passport Module
  - Mongoose Population
  - Assignment 3

**Module 4: Backend as a Service (BaaS)**

  - HTTPS and Secure Communication
  - Uploading Files
  - Cross-Origin Resource Sharing
  - OAuth & User Auth (example- Facebook)
  - Backend as a Service
  - Assignment 4

#### Learning Resources

*skimmed for new content*

#### Welcome - Additional Resources

Useful links:

[NodeJS](https://nodejs.org/)  
[NPM](https://www.npmjs.com/)  
[ExpressJS](http://expressjs.com/)  
[MongoDB](http://www.mongodb.org/)  
[Mongoose](http://mongoosejs.com/)  


### Full-Stack Web Development: The Big Picture
#### Objectives & Outcomes

Big picture view of FullStack WebDev

  - Understand full stack web dev conceptually
  - Distinguish front end, back end, & full stack

#### What is Full-Stack Web Development?

**Front end and Back end**

 - Front end / Client side
   - html, css, js
  - Back end / Server side
    - various tech & approaches
    - PHP, Java, ASP.NET (ew), Ruby, Python. (AND PERL >:( )

**Three Tier Arch**

Full Stack:
```
 Presentation layer    - concerned with UI related issues

 Business Logic layer  - Data validation, dynamic content process, generating content

 Data Access layer     - Dat apersistence, data access via API
```

**Traditional Web Dev**

Presentation Layer - HTML, CSS, JS  
  ^ *server side rendering*  
Business Logic Layer - Ruby, Python, etc  
  ^v  
Data Access layer - DBMS  

  - Requires specialists for all three layers

**Full Stack JavaScript Dev**

Presentation Layer - Single page Apps w/ JS framework, ie Angular  
  ^ *REST API serving JSON*  
Business Logic Layer - NodeJS and NodeJS modules  
  ^v  
Data Access layer - MongoDB of JSON Documents  

**NB**: Maybe I should dip back to the Angular course (course 2)?

### Setting up your Development Environment: Git

*skipped*

### Introduction to Node.js and NPM

#### Objectives

 - download & install Node & NPM

#### Node.js & NPM

JS was originally designed as a scripting language for browsers.
Nodejs has shifted this.

Node.js is a JS runtime built on Chrone V8 JS Engine  
Event-driven, non-blocking I/O model, making it lightwieght & efficient

**Node Architecture**   
Node Code / Standard Lib (JS)  
Node Bindings (C++)  
Chrome V8 (C++) + libuv (C)  

**Node.js use cases**  
Utilities: Bower, Frunt, Gulp, Yeoman  
Server-side Dev: Web server, business logic, DB access

**Node Package Manager (NPM)**  
NPM: manages ecosystem of node modules/pkgs  
Packages contain: 
  - JS files
  - package.json (manifest)

#### Setting up Node.js & NPM

Installed [Node Version Manager](https://github.com/creationix/nvm).  
Installed Node v8.4.0 to match coursera, set Coursera alias  
Confirmed versions match course.

#### Additional Links

[Nodejs.org](https://nodejs.org/en/)  
[Npmjs.com](https://www.npmjs.com/)  
[Node API Documentation](https://nodejs.org/api/)  
[NPM Documentation](https://docs.npmjs.com/)  

### Node Modules

#### Objectives and Outcomes

  - Write basic node apps & run them
  - Dev node modules & use them in the app
  - learn callbacks & error handling

#### Node Modules

**JavaScript Modules**

  - JS has no stdlib at the language level
  - CommonJS API fills the stdlib gap with defined APIs for common app needs
    - defines a module format
    - Node follows CommonJS module specs

**Node Modules**

  - every file is a module
  - the *module* var gives acces to teh current module definition in a file
  - the *module.exports* var determines the export from the current module
  - the *require* function is used to import a module

**Types**

  - File-based Modules
  - Core Modules
    - part of core Node
    - kept intentionally small
    - Ex: path, fs, os, util
  - External Node Modules
    - third party modules
    - installed via NPM
    - `node_modules` folder in the Node app

**Using Node Modules**

  - include using the require function
  - File-based modules:
    - `require('./module_name')`
    - specify the *relative* path
  - Core & External modules:
    - `require('module_name')`
    - core is found by automatically
    - looks for external modules in:
      - ./node_modules, ../node_modules, ../../node_modules, ...
      - up the dirtree until module is found

**Example**

`rectangle.js`
```
exports.perimeter = (x,y) => (2*(x+y));
exports.area = (x,y) => (x*y);
```

**NB**: `exports` is an alias for `module.exports`

`index.js`
```
var rect = require('./rectangle');

...
```

#### Exercise: Understanding Node Modules

[See exercise](https://github.com/lomky/coursera-NodeJS/tree/master/exercises/week_1/understanding-node-modules)

Node supports both JS and TypeScript

#### Node Modules: Callbacks and Error Handling

**JS: Two Salient Features**

  - *First-class functions*: A function can be treated the same as any other var
  - *Closures*:
    - a function _defined_ inside another function has access to the outside functions scope.
    - the inner function will continue to have access to the variables from the outer scope _even after the outer function has returned._

**Async Programming**

Assume Comp2 relies on the IO result, but Comp 3 isn't.  
Async allows us to have comp 3 happen sooner  
The function after the IO is called a _Callback_

```
Sync:
Comp 1 -> Longrunning comp / IO -> Comp 2 -> Comp 3

Async:
Comp 1 -> Longrunning comp / IO  -> Callback
       -> Comp 3
```

**Node, Async I/O, and Callbacks**

The Event loop runs the entire process.  
When an I/O request is made, it's sent back to the request stack

```
                             I/O req
     Requests                 ___----> File /
--> ||||||||||    EVENT LOOP /       Database /
 ^                 (single          Processing
  \__Callback_____ thread)  <---_______/
                             I/O req completed
```

**Event Loop**

```
  ___
 |   |
 |   v
 |  timers
 |  I/O callbacks
 |  idle, prepage
 |  poll   <-------- incoming connections, data, etc
 |  check
 |  close callbacks
 |  |
 ----
```

*timers*: executes callbacks scheduled by `setTimeout()` and `setInterval()`  
*I/O callbacks*: executes almost all callbacks with the exception of close callbacks (timers and check)  
*idle, prepare*: only used internally  
*poll*: retrieve new I/O events; node will block here as appropriate  
*check*: `setImmediate()` callbacks are invoked here  
*close callbacks*: eg `socket.on('close',...)`  

Each phrase has its own queue.

#### Exercise: Node Modules: Callbacks and Error Handling

[See exercise](https://github.com/lomky/coursera-NodeJS/tree/master/exercises/week_1/callback-error-handling)

setTimeout takes (error, return value)  

the pattern of calling a module with a passed in callback, and then handling the callback is very common in node.

#### Node Modules: Additional Resoures

[Node Modules](https://nodejs.org/api/modules.html)  
[The Node.js Event Loop, Timers, and process.nextTick()](https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick/)  

[CommonJS](http://www.commonjs.org/)  
[CommonJS Module Format](http://wiki.commonjs.org/wiki/Modules/1.1.1)  
[RequireJS](http://requirejs.org/)  

### Node and HTTP

#### Objectives and Outcomes

  - create simple HTTP server with Node HTTP code module
  - create a web servier with static HTML served from dir

#### Networking Essentials

Broad overview of how computer networks work.  
_skimming PDF_  


**Client and Server**

Web applications are not stand alone  
Many of them have a “Cloud” backend

**Client - Server Communication**

  - Network operations cause unexpected delays
  - You need to write applications recognizing the asynchronous nature of communication
    - Data is not instantaneously available

_Overview of HTTP verbs, req, response, codes, etc_

XML vs JSON. JSON overview.

#### Node and the HTTP Module

**Node HTTP Module**  

  - core networking module supporting high-perf HTTP stack
  - Use the module: `const http = require('http');`
  - Create a server: `const server = http.createServer(function(req,res){...});`
  - Start the server: `server.listen(port,...);`
  - Incoming request message info available through `req`
    - `req.headers`, `req.body`,...
  - Response message is constructed on `res`
    - `res.setHeader("Content-Type", "text/html");`
    - `res.statusCode = 200;`
    - `res.write('Hello World!');`
    - `res.end('<html><body><h1>Hello World</h1></body></html>');`

**Node `path` Module**

  - Using path Module: `const path = require('path');`
  - path examples:
    - `path.resolve('./public' + fileURL);`
    - `path.extname(filePath);`

**Node `fs` Module**

  - Using fs Module `const fs = require('fs');`
  - fs examples:
    - `fs.exists(filePath, function(exists){...});`
    - `fs.createReadStream(filePath).pipe(res);`

#### Exercise: Node and the HTTP Module

[See exercise](https://github.com/lomky/coursera-NodeJS/tree/master/exercises/week_1/node-http)

Intro'd to Postman - investigate more.

#### Node and HTTP: Additional Resources

### Introduction to Express

### Assignment 1
