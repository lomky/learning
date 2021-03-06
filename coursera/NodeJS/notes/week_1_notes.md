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

[Node HTTP](https://nodejs.org/api/http.html)  
[Anatomy of an HTTP Transaction](https://nodejs.org/en/docs/guides/anatomy-of-an-http-transaction/)  
[`fs` Module](https://nodejs.org/api/fs.html)  
[`path` Module](https://nodejs.org/api/path.html)  


[Hypertext Transfer Protocol](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)  
[List of HTTP Status Codes](https://en.wikipedia.org/wiki/List_of_HTTP_status_codes)  
[Postman](http://getpostman.com/)  

### Introduction to Express

#### Objectives and Outcomes

  - web server with Express framework
  - web server supporting REST API
  - Express router to implement support for REST API

#### Introduction to Express

**What is Express**  
A third party module, an HTTP webserver framework  

  - Express: Fast, unopinionated, minimalist web framework for Node.js
  - Web app framework that provides a robust set of features
  - Many third part middleware that extends functionality of express
  - Installing express: `npm install express --save`

**Express Middleware**

  - Middleware provides a lot of plugin functionality that can be used within your Express app
  - Example: `morgan` for logging
    ```
    var morgan = require('morgan');
    app.use(morgan('dev'));
    ```
  - Serving static web resources:
    `app.use(express.static(__dirname + '/public'));`
    - Note: `__filename` and `__dirname` give you the full path to the file and dir of the current module

**A Brief Tour of a Node Module**

  - Examine package.json file
  - Semantic Versioning [SemVer](semver.org)
  - npm can install specific versions:
    - Exact       : `npm install express@4.0.0`
    - Patch ok    : `npm install express@"~4.0.0"`
    - Minor ver ok: `npm install express@"^4.0.0"`

_package.json_

  - `dependencies` - specify modules the package depends on

_.gitignore_

  - add `node_modules` to your gitignore to avoid commiting your third party modules

_`node_modules`_

  - contains all of the third party modules
  - and their dependencies...

_package-lock.json_

  - auto generated by npm
  - the exact tree generated by the node modules install


#### Exercise Introduction to Express

#### Brief Representational State Transfer (REST)

#### Express Router

**Express Application Routes**  

  - given REST, we identify endpoints with an URI and apply verb to that URI
  - app routes:
  ```
  app.all('/dishes', function(req,res,next) {...});
  app.get('/dishes', function(req,res,next) {...});
  app.post('/dishes', function(req,res,next) {...});
  app.put('/dishes', function(req,res,next) {...});
  app.delete('/dishes', function(req,res,next) {...});
  ```
  - Example:
  ```
  app.get('/dishes/:dishId',(req,res,next) => {
      res.end('Will send detauils of the dish: '
          + req.params.dishId + 'to you!');
  });
  ```

**Body Parser**  

  - Middleware to parse the body of the message
  - Use:
  ```
  var bodyParser = require('body-parser');
  app.use(bodyParser.json()); // parse the JSON in the vbody
  ```
  - Parses the body of the message and populates the req.body property

**Express Router**  

  - Express Router creates a mini-Express app:
  ```
  var dishRouter = express.Router();
  dishRouter.use(bodyParser.json());
  
  dishRouter.route('/')
    .all(...);
    .get(...);
    ...
  ```

#### Exercise Express Router

[See exercise](https://github.com/lomky/coursera-NodeJS/tree/master/exercises/week_1/express-router)

#### Introduction to Express: Additional Resources

[ExpressJS](http://expressjs.com/)  
[Connect](https://github.com/senchalabs/connect)  
[Express Wiki](https://github.com/expressjs/express/wiki)  
[morgan](https://github.com/expressjs/morgan)  
[body-parser](https://github.com/expressjs/body-parser)  
[Understanding Express.js](http://evanhahn.com/understanding-express/)  
[A short guide to Connect Middleware](https://stephensugden.com/middleware_guide/)  

[Node Modules](https://nodejs.org/api/modules.html)  
[ExpressJS](http://expressjs.com/)  

### Assignment 1

#### Step-By-Step Assignment InstructionsStep-By-Step Assignment Instructions

**Assignment Overview**

At the end of this assignment, you should have completed the following tasks to update the server:

  - Created a Node module using Express router to support the routes for the dishes REST API.
  - Created a Node module using Express router to support the routes for the promotions REST API.
  - Created a Node module using Express router to support the routes for the leaders REST API.

**Assignment Requirements**

The REST API for our Angular and Ionic/Cordova application that we built in the previous courses requires us to support the following REST API end points:

  1. http://localhost:3000/dishes/:dishId
  1. http://localhost:3000/promotions and http://localhost:3000/promotions/:promoId
  1. http://localhost:3000/leaders and http://localhost:3000/leaders/:leaderId

We need to support GET, PUT, POST and DELETE operations on each of the endpoints mentioned above, including supporting the use of route parameters to identify a specific promotion and leader. We have already constructed the REST API for the dishes route in the previous exercise.

This assignment requires you to complete the following three tasks. Detailed instructions for each task are given below.

**Task 1**

In this task you will create a separate Node module implementing an Express router to support the REST API for the dishes. You can reuse all the code that you implemented in the previous exercise. To do this, you need to complete the following:

  - Update the Node module named dishRouter.js to implements the Express router for the /dishes/:dishId REST API end point.

**Task 2**

In this task you will create a separate Node module implementing an Express router to support the REST API for the promotions. To do this, you need to complete the following:

  - Create a Node module named promoRouter.js that implements the Express router for the /promotions and /promotions/:promoId REST API end points.
  - Require the Node module you create above within your Express application and mount it on the /promotions route.

**Task 3**

In this task you will create a separate Node module implementing an Express router to support the REST API for the leaders. To do this, you need to complete the following:

  - Create a Node module named leaderRouter.js that implements the Express router for the /leaders and /leaders/:leaderId REST API end points.
  - Require the Node module you create above within your Express application and mount it on the /leaders route.

**Review criteria**

Upon completion of the assignment, your submission will be reviewed based on the following criteria:

**Task 1:**

  - The REST API supports GET, PUT, POST and DELETE operations on /dishes/:dishId end point.

**Task 2:**

  - The new Node module, promoRouter is implemented and used within your server to support the /promotions end point.
  - The REST API supports GET, PUT, POST and DELETE operations on /promotions and GET, PUT, POST and DELETE operations on /promotions/:promoId end points.

**Task 3:**

  - The new Node module, leaderRouter is implemented and used within your server to support the /leaders end point.
  - The REST API supports GET, PUT, POST and DELETE operations on /leadership and GET, PUT, POST and DELETE operations on /leaders/:leaderId end points.


