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

[See exercise](../exercises/week_1/understanding-node-modules)

Node supports both JS and TypeScript

#### Node Modules: Callbacks and Error Handling

#### Exercise: Node Modules: Callbacks and Error Handling

#### Node Modules: Additional Resoures

### Node and HTTP

### Introduction to Express

### Assignment 1
