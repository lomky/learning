# Coursera HTML, CSS, and Javascript for WebDevs - Week 5 Notes

## Document Object Model Manipulation
### Lecture 53 - DOM Manipulation

 - See examples folder for the index.html we're referencing.

Document Object Model Manipulation  

The special document called `document`

The `window` object has a `document` object that contains our entire page. We use this to access elements in the page.

For example in the console:

```
document.getElementById("title");
  <h1 id="title">Lecture 53</h1>
```

We can access the `document` variable inside of our scripts, because it is in the `global` scope.

```javascript
console.log(document.getElementById("title"));
```

We get null? Why?  
Because of when the line is executed - where it is mentioned! In the script tag inside the header.  
The item "title" has not yet been rendered. So it's null. How do we solve it? Severla ways.  

  - special method that listens for an event in the lifecycle of hte page loading, and triggers the execution
  - place the javascript at the very _end_ of the document, so it executes after everything in the DOM is in browser memory.

The same code _after_ the JS script tag is moved to the end, produces the element just like the console.

```javascript
console.log(document instanceof HTMLDocument); // true
```

In our `index.html`, we tell the button to do something `onClick`:

```html
<button onclick="sayHello()">Say it!</button>
```

Now it will call the function sayHello in our JS on clicking the button.

The function:

```javascript
function sayHello () {
    var name = 
      document
        .getElementByID("name")
        .value; // gives us the value instead of the element.
    var message = "Hello " + name " "!";
    docuemnt
      .getElementByID("content")
      .textContent = message;     // This puts the message Into the div with id 'content'!
}
```

What if we want to put HTML styling in? If we put it into the `message` variable, it will not render. (Good!)

We need a different way to say to render HTML styling.

```javascript
function sayHello () {
    var name = 
      document
        .getElementByID("name")
        .value; // gives us the value instead of the element.
    var message = "<h2>Hello " + name " "!</h2>";
    docuemnt
      .getElementByID("content")
      .innerHTML = message;     // This puts the message Into the div with id 'content'!
}
```

How can we add intelligence?
Let's make the Title change for specific inputs.

```javascript
function sayHello () {
    var name = 
      document
        .getElementByID("name")
        .value; // gives us the value instead of the element.
    var message = "<h2>Hello " + name " "!</h2>";
    docuemnt
      .getElementByID("content")
      .innerHTML = message;     // This puts the message Into the div with id 'content'!

    if ( name == "student") {
      var title = 
        document
          .querySelector("#title") //specify via CSS selector syntax
          .textContent;
      title += " & Lovin' it!";   //we haven't change the actual value, just a copy.
      document
        .querySelector("h1") //can also query on the h1!
        .textContent = title;
    }
}
```

### Lecture 54 - Handling Events

Continunig from the last lecture, the "onClick" is an example of an event handler.

Event Handlers are functions that you bind with specific methods to certain events that happen in the browser.

The events may be triggered by 

  - the browser life cycle, i.e. page loaded
  - a user action, i.e. clicking or typing a character.

The easiest way to envoke is with hte `onSomething` attribute in html.

Examples:

  - `onblur` - when this element loses focus
  - `onclick`
  - `ondblclick`
  - `onfocus`
  - `onkeydown`
  - `onkeypress`
  - `onkeyup`
  - `onmousedown`

But you don't have to place things straight on the HTML. Dirties your html a bit, this isn't content.

Instead, we can bing the function `sayHello` to the onclick event itself

#### Unobstrusive event binding

Remove the onclick from the html.

In the script.js:

```javascript
document.querySelector("button")
  .addEventListener("click", sayHello);
```

Now when buttons are clicked, the will call `sayHello()`!  
This also means `this` will point to the button element instead of the `window`  


Alternatively, you can do:

```javascript
document.querySelector("button")
  .onclick = sayHello;
```

Exactly the same code-wise with the previous.

This method gives us more flexibility with the `this` pointing at the button itself.

Now a Lifecycle event example:

These scripts can go in the head, instead of the body bottom


```javascript
document.addEventListener("DOMContentLoaded",
  function(event) {
    function sayHello () {
        var name = 
          document
            .getElementByID("name")
            .value; // gives us the value instead of the element.
        var message = "<h2>Hello " + name " "!</h2>";
        docuemnt
          .getElementByID("content")
          .innerHTML = message;     // This puts the message Into the div with id 'content'!

        if ( name == "student") {
          var title = 
            document
              .querySelector("#title") //specify via CSS selector syntax
              .textContent;
          title += " & Lovin' it!";   //we haven't change the actual value, just a copy.
          document
            .querySelector("h1") //can also query on the h1!
            .textContent = title;
        }
    }
  }
}
```

Now since the JS loads only after the DOM loads, we can place the JS at the top. This assures the other elements exist

### Lecture 55 - The 'event' Argument

The JS engine passes the `event` argument into every single event handler.  

When you log the `event` out you get, for example, a `mouseEvent`.  
Includes a bunch of helpful info about how the mouse event happened.

Can get more info on the Mozzila Developer Network about events.

Can be related to keyboard, mouse movement, mouse actions, zoom, input. Many options.

All of these have properties, which are defined there as well.

Let's define another event function based on mouse movement and shift.

In our script.js, in the `document.addEventListener` section.

```javascript
document.querySelector("body")
  .addEventListener("mousemove", //on mouse movement
    function (event) {           //but only if Shift is pressed
      if (event.shiftKey) {
        console.log("x: " + event.clientX); //print x
        console.log("y: " + event.clientY); //print y
      }
    }
  );
```

Now in the console we see out X and Y coordinates as we move the mouse.

## Introduction to Ajax

### Lecture 56 - HTTP Basics

Before diving into Ajax, let's talk about the HTTP.

#### What is HTTP?

HyperText Transfer Protocol.

  -based on request/response stateless protocol
    - stateless - no state is remembered by the protocol. All requests are distinct
  - Normal connection process:
    - client opens the connection to the server
    - client sends HTTP req for a resource
    - server sends HTTP response to the client (w/ resource if no error)
    - client closes connection to server

#### Identifying Resources on the Web

**URN: Uniform Resource Name**

  - Uniquely identifier resource or name of resource
  - Does not tell us hot to get the resource

Example: 

"HTML/CSS/Javascipt/Web Developer/Yaakov/Chaikin"  
  Could be a unique name for the course that we are taking.
  But not unique _worldwide_, just on coursera.

**URI: Uniform Resource Identifier**

  - Uniquely identifies resource of location of resource
  - Does not necessarily tell us how to get the resource
    - the context is usually missing. More like a directory

Example:  
`/official_web_site/index.html` - only works in context of knowing the main site.

Personal note: This is an incorrect definition of URI. See [Wikipedia](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier). Or the RFC. Or W3...


**URL: Uniform Resource Locator**

  - Form of URI that provides info on how to get the resource

Example:
  `http://www.mysite.com/path/index.html`

#### HTTP Request Structure (GET)

`GET /index.html?firstName=Yaakov HTTP/1.1`

  - `GET               ` - the method
  - `/index.html       ` - the URI string
  - `?firstName=Yaakov ` - optional QueryString (?name=value&name=value)
  - `HTTP/1.1          ` - HTTP Version

Where is the hostname? Not here. This is only issued after we have connected to a server. So every request is related to the server we are connected to.


#### HTTP Methods

  - GET
    - retrieves the resource
    - data is passed to server as part of the URI
      - i.e. query string
  - POST
    - sends data to server in order to be processed
    - data is sent in the message body
  - More methods not covered here
    - well, sort of.

#### HTTP Request Structure (POST)

```
POST /index.html HTTP/1.1  //
Host: coursera.org         // Request Headers
Accept-Charset: urf-8      //
firstName=Yaakov...    //
...                    //  Message Body
```


#### HTTP Response Structure

`HTTP/1.1 200 OK`

  - `HTTP/1.1 ` - the HTTP version
  - `200      ` - response status code
  - `OK       ` - English phrase describing status code
  - all are separated by a single space

#### Typical HTTP Response

```
HTTP/1.1 200 OK
Date: Tue, 11 Aug 2004 19:00:01 GMT
Content-Type: text/html
<html>
<body>
  <h1>WEEEE</h1>
  <p> I am the BEST EXAMPLE HTML RESPONSE </p>
</body>
</html>
```

#### Some Response Status Codes

  - 200 OK
    - Okay, here is the content you requested
  - 404 Not Found
    - Server can't find the resource requested
  - 403 Forbidden
    - Unauthenticated client tried to access a secure resource
  - 500 Internal Server Error
    - Some unhandled error was raised on the server

### Lecture 57 - Ajax Basics

#### What is Ajax?

stands for:  
**Asynchronous Javascript and XML**

  - while ajax started with XML, very few apps use it now
  - Plain text, HTML, and JSON are used instead

#### Traditional Web App Flow

  - Starts with a web page.
  - Press a button, sending an HTTP request to the server
  - Server responds with a new page with a mostly-identical page

#### Ajax Web App Flow

  - Starts with a web page 
  - Press a button, sending HTTP request to the server
  - Server responds with a small piece of data that is inserted into the current page

This is:

  - faster
  - takes less bandwidth
  - nicer for the user

#### Synchronous Execution

The execution of one instruction at a time.  
Can't start execution of another instruction until the first finishes its execution

#### Asynchronous Execution

Execution of more than one instruction at a time.  
Asynchronous instruction returns right away.  
The actual execution is done in a separate thread/process  

How can Ajax be async when JS is sync?

#### How Does Ajax Work?

  - the browser
    - Javascript Engine runs inside it
    - Event Queue runs in the browser - speaks to JS Eng
    - HTML Rendering engine - handles the display
    - webGL - lets JS reach out and do some high end graphics work
    - HTTP Requestor - responsible for reaching out to make requests

While JS is Synchronous, the HTTP Requestor is _asynchronous_.

#### Ajax Process

```
Javascript Engine                   HTTP Requestor
_____________________                   _____________________
|js code line        |                 |                     |
|js code line        | provides JS fun |                     |
|make Ajax req  ---------------------->|                     |
|js code line        | response handler|                     |
|js code line        |                 |                     |
|js code line        |                 |                     |
|js code line        |                 |                     |
|Handle server resp  <------------------ executes the JS func|
|    //callback      |                 | provided            |
|    //function      |                 |                     |
|____________________|                 |_____________________|
```

_See lecture examples for 57 for code examples_

The Ajax utility script:

```ajax
(function (global) { 

//set up a namespace for our utility
var ajaxUtils = {};

// returns an HTTP request object
function getRequestObject() {
  if (window.XMLHttpRequest) {
    return (new XMLHttpRequest());
  }
  else if (window.ActiveXObject) {
    //for very old IE browsers, optional
    return (new ActiveXObject("Microsoft.XMLHTTP"));
  }
  else {
    global.alert("Ajax is not supported!");
    return(null);
  }
}

// Makes an Ajax GET request to 'requestUrl'
ajaxUtils.sendGetRequest = 
  function(requestUrl, responseHandler) {
    var request = getRequestObject(); //DO NOT MAKE THIS GLOBAL. Breaks Async with a race condition
    request.onreadystatechange =
      function() {
        handleResponse(request, responseHandler);
      };
      request.open("GET", requestUrl, true); //true means async
      request.send(null); //for POST only
  };

//Only calls uer provided 'responseHandler'
// function if resposne is ready
// and not an error
function handleResponse(request, responseHandler) {
  if ((request.readyState == 4) && (request.status == 200)) {
    responseHandler(request);
  }
}

//Expose utility to the global object
global.$ajaxUtils = ajaxUtils;

})(window);
```

And the corresponding Script.js

```javascript
//Event handling
document.addEventListener ("DOMContentLoaded",
  function (event) {

    //Unobstrusive event binding
    document.querySelector("button")
      .addEventListener("click", function() {

        //Call server to get the name
        $ajaxUtils                                             // access the ajax utility
          .sendGetRequest("/data/name.txt",                    // our URL
            function (request) {                               // our handler function. The response object is still called request in JS.
              var name = request.responseText;                 // the responseText of the request object holds the response from the server
              document.querySelector("#content")               //this _must go_ in here. putting after means it will execute before ajax returns. (async!)
                      .innerHTML = "<h2>Hello " + name + "!";  //and this does our content replacement like we learned last week
          });
    });
  }
);
```

### Lecture 58 - Processing JSON

### What is JSON?

JavaScript Object Notation

  - Lightweight data-interchange format
    - simple textual representation of data
  - _Easy for humans_ to read and write
  - _Easy for machines_ to parse and generate
  - __Completely independent of any language__

### JSON Syntax Rules

  - Subset of Javascript object literal syntax
    - Exceptions:
    - Property names must be in _double_ quotes
    - String values must be in _double_ quotes

### JSON Example

```JSON
  "firstName": "Yaakov",  //note the double quotes
  "lastName: "Chaikin",
  "likesChineseFood: false, //literals can be unquoted
  "numberOfDisplays": 2
}
```

Can be made into a Javascript string by surrounding it with single quotes and assigning it.

### Common Misconception

  - JSON is NOT a Javascript Object literal
  - JSON is just a string
  - The syntax of JSON is based on object literal, though
  - Need to convert JSON into a JS object if you want to treat it as such

### Converting JSON to String & Back to JSON

Converts from json string to object:  
`var obj = JSON.parse(jsonString);`

Converts from object to json string:  
`var str = JSON.stringify(obj);`

### The previous example, but now in JSON

See the example files for Lecture 58.

name.json contains:

```JSON
  "firstName": "Yaakov",
  "lastName: "Chaikin",
  "likesChineseFood: false,
  "numberOfDisplays": 2
}
```

ajax-utils.js gets updated.  
Our `sendGetRequest` function gets a third argument, `isJsonResponse`.  
This gets a `true` or `false` (default `true`) to expect json.  
This is further passed into `handleResponse` as a third arg.  

`handleResponse` now has the `isJsonResponse`  
if undefined, we set it to `true`.  
if `true`, our `responseHandler` passes in `JSON.parse(request.responseText)`  
else we just give them the `request.responseText`  

our script.js is updated:  
in our `sendGetRequest` our `res` is now a JS object since we converted it in ajax.  
we can use it via dot notation, `res.likesChineseFood` works just fine.  
we can use if/else on those values to display varies text options.  

## Using Ajax to Connect Restaurant Site with Real Data

### Lecture 59 - Fixing Mobile Nav Menu Automatic Collapse

This problem:  In our small screen the menu extends on clikcing hte button, but when we click off it it does not disappear.  
This is the `onBlur` event.  

When do we want to do this?  
Not with the big buttons, only the collapsable button version.  
This size is around the ~760px, which is one of our breakpoints.  

See examples in the Lecture 59 `after` file.

In our `script.js`:

```javascript
$(function () { // Same as document.addEventListener("DOMContentLoaded...)

  $("navbarTogle")  // Same as document.querySelector("navbarToggle").addEventListener("blur",...)
    .blur(function (event) {
      var screenWidth = window.innerWidth;      //get hte width of hte inner window
      if (screenWidth < 768) {                  // only on the smaller screen size!
        $("#collapsable-nav").collapse('hide'); //select the collapsable nav, call collapse with value hide. This is bootstrap magic mixed with jQuery.
      }
  });
});
```

Now it works!

### Lecture 60 - Dynamically Loading Home View Content

We want all the front page content to load dynamically. Why?  
We're moving towards a single page web application (SPA).  
Popular today. Heavily reliant on Ajax. Avoid reloading the header/footer every page.  

To start, we wipe out the main content from the html.  
Instead, we have things stored in separate files called snippets.  

In the script we add functions that know how to load the content properly.  
We set up our namespace for the web page functions.  
Create a convinience function for inserting innerHTML for a selector  
Another function showLoading places a nice loading gif to reassure the user content is loading.  
Now the page loads properly! But our menu categories are broken now.

### Lecture 61 - Dynamic Loading Menu Categories View

Now the goal is to have the menu categories load dynamically on click with all the proper content.  

#### Heroku & CORS

We offer our restaurant data on a separate ruby on rails app hosted on heroku. This is a bit of a problem, as we are breaking same origin loading javascript. CORS is the answer. Wikipedia goes into detail, but basically there is an HTTP header that whitelists the domain name we will get our data from.

#### Examining the Snippets 

  - Menu Categories title
    - Just contains the heading of the Menu Categories 
  - Menu Categories category
    - Contains a snippet for One cateory.
    - We can loop over this to show each new category.
    - So for each category we will have events to populate them.

#### Script.js

Contains paths to our heroku paths to get data for allCategories
and paths for our snippets for the home page, category titles, and category snippets.

New function `insertProperty` replaces the `{{word}}` items in our snippets with the proper values.

For our menu categories, we create the function `dc.loadMenuCategories`. Starts with the loading image and the invokes a `sendGetRequest` with the `allCategoriesUrl` and `buildAndShowCategoriesHTML` (the callback)  as arguments.

In `buildAndShowCategoriesHTML` we send out another ajax request to get the categoryTitle. Inside that we call for the categoryHTML. 

Now we have all the information we need to build the category with the function `buildCategoriesViewHTML` which takes all the pieces and puts them together. Finally we use insertHTML to replace the main-content.

And we're done with the menu categories!

### Lecture 62 - Dynamically Loading Single Category View

We have another REST API endpoint for the menu items for each category. Given the short name of the category, we get back the category itself and the items inside the given category.

So we will set that base URL for our query, and use the short name as the key.

In our script.js we add that URL and the new snippets for the menu item title snippet, and then menu items snippet.

we have a new function `loadMenuItems` which looks a lot like the `loadCategoryItems` except now we pass in an argument for the short_name.

It callsback to `buildAndShowMenuItemsHTML` which does the double Ajax request to get the title html and the menu item html that we will loop over.

That calls to the `buildMenuItemsViewHTML` which builds the HTML, and finally we do the insert of the content.

### Lecture 63 - Changing 'active' Button Style Through Javascript

Our active class that highlighted our place in the header no longer works with our Ajax setup. Let's fix that.

script.js, new function `switchMenuToActive`

It tells the home button to de-highlight and alerts the menu button to highlight.

We are functionally complete!
