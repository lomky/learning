# Visit with the client & Setup Overview

## Lecture 27: Visit with the Client

### Ground Rules
 - most clients don't know what they want  
   - not for their business, but for technical needs  
   - Ask questions to figure out what they want  
   - bring examples to show and ask questions about, "why do you like this? why don't you like that?"  
 - most clients want to put a lot of info on their page  
   - it's all important, which makes nothing important  
   - Less is More  
   - less info makes that info more impactful  
   - you need to help determine what's the post important  
 - find a way for the client to invest in the project  
   - they need skin in the game, so you aren't a side project  
   - doubly true if its a free task.  
   - get them to commit to paying for the photos, creating product for photos, etc.  
 - make sure only one person is designated as responsible for the decisions for the business  
   - all other persons should funnel through them  
 - limit the number of revisions UPFRONT  
   - write it into the contract  
   - if it is a paid job, limit the number of free revisions  
 - google: web development client questionnaire  
 - involve others if needed  
   - if you need help with graphics, photography, etc, find help  
 - find out what they have already  


## Lecture 28: Design Overview

### Create the design

 - this goes Before the programming  
 - Mock up designing in Balsamiq Mockups  
 - get full mock ups from professional designer  
   - for showing the client  

## Lecture 29: Some Ground Rules & Overview of Setup

### Some Ground Rules
 
 - use the video features to replay, speed up, etc  
 - grab the available source code  
 - from now on, the current lectures "before" contains the previous chapters "after"  
 - some things will be done offscreen, but will be explained  
 - code & browser switching will happen a lot  
 
### Set browser-sync running

`browser-sync start --server --directory --files "*"`

 - notice alongside you localhost:3000, there is a 3001 UI.  
   - Explore this on your own.  

### Setting up our base style

See example/Lecture29

 - starting with standard meta tags and the title.  
 - want to set up our `styles.css` with global styles  
 - `body` - `font-size: 16`; `color:#fff`; `background-color: #61122f`;   
 - add our font:  
    - `font-family: Oxygen`  
 - this font doesn't work!  
 - need to get it. go to `google.com/fonts`, search for it, and hit `quick use`  
 - copy the `link` tag and paste it into your `head`  
 - specify the `font-family` how the google page directs  
    - `font-family: 'Oxygen', sans-serif;`  

# Coding the Navigation Bar of the Site

## Lecture 30: Coding the Basics of Navbar Header

In the command line:  
 - Start up browser sync to watch all folders: `browser-sync start --server --directory --files "**/*"`  

Adding the bar in:

 - Semantic tag `<header>` we should use.
 - create the nav bar with the `<nav` element, an id `header-nav` and classes for bootstrap: `navbar` and `navbar-default`
 - In the browser we now have a white navbar element at the top
   - Note the rounded corners - to fix later

Adding a container:

 - `div class="container"` 
   - we don't want fluid, as we don't have a lot of content, so stretching it out will look bad. constraining the width controls the appearance.
   - This goes _inside_ the `nav` tag, so that the yellow background stretches behind the container but the container is constrained.

Adding some CSS:

`#header-nav` gets a `background-color: yellow`, `border-radius: 0`, and `border:0`;
  - this removes the rounded edges, and sets the color properly.

Place the logo in the top left.

 - add a new `div` with class `navbar-header`
    - this class helps position it properly.
    - don't be afraid to open bootstrap.css and read the code
    - but before that, RTFM for bootstrap!
      - it has lots of examples with implementation to guide you
 - surround the logo with `a` to make it a link to the homepage
 - create a `div` with id `logo-img` and `alt` text  
 - In the CSS, create the `#logo-img` and set it's `background: url("../link/to/image.png") no-repeat`, and set the `width`, `height` and `margin` to space it properly. 

Now we have a yellow header area with the logo placed!

Putting the Name into the Header

 - create a `div` with class `navbar-brand`
 - place the text inside an `a` to link to the index, and an `h1` to mark it as the Most Important Thing on the page.
 - place a `p` and below to contain the 'Kosher Certified' logo (in an `img`) and the words Kosher Certified inside a `span`

 - It's in the wrong place! Sinking down below the logo because they are both `div`s and they stack
 - Need to make the logo float right, but in a _bootstrap_ way

Add the _bootstrap position_ to the logo

  - on the `a` tag for the logo, add the class `pull-left`

Styling the Name

 - import the font (from google)
 - CSS style:
   - give `.navbar-brand` gets some `padding-top: 25px`
   - give `.navbar-brand h1` its `font-family`, `color`, `font-size`, `text-transform: uppercase`, `font-weight: bold`, `text-shadow`, `margin-top:0`, `margin-bottom:0`, `line-height: .75`
     - can look up the attributes yourself
   - Remove the underlink link hover: 
     - `.navbar-brand a:hover, .navbar-bran a:focus` get `text-decoration: none`
  - style the Kosher certified by giving `.navbar-brand p` the attrs `color`, `text-transform`, `font-size` and `margin-top`
  - style the Kosher text by giving `.navbar-brand p span` a `vertical-align: middle`

Hide the logo on small screens

 - Bootstrap has classes to assert this
 - on the logo's `a` ref add the classes `visible-md visible-lg`
   - now the logo will only be shown on medium and large screens


## Lecture 31: Coding Button for Future Collapsible Menu

Before we add any more, let's take care of the mobile view and its hamburger element. The bootstrap docs 

Adding the mobile hamburger

 - stay inside the `navbar-header` `div`
 - add the code from the bootstrap docs.
 - starts with a semantic `button` tag with the classes `navbar-toggle` and `collapse`
   - navbar-toggle is bootstrap how to display and handle it
   - collapse is a plugin to bootstrap that signifies the section has not been pressed
   - other attributes: `data-toggle: "collapse"; data-target="#collapsable-nav" aria-expanded="false"`
     - the data-* allow javascript hooks to open and close the menu
     - aria-expanded is for screen readers.
 - next are `span`s
    - first has class `sr-only` with text for the screen reader.
    - next three have class `icon-bar`

Now when the screen is small the button shows up, hover highlights, but nothing happens on click. to make the click work, we will hook into the value in `data-target` We also need to style the button.

## Lecture 32: Coding the Nav Menu Buttons

Focusing on the Menu buttons

Going just below the navbar-header! Because this won't be part of the navbar header when it collapses.

 - First, a `div` containing all the rest. 
   - id `collapsable-nav` matches the `data-target` from earlier, making this the target
   - classes `collapse` and `navbar-collapse` based on BS docs
 - Create an ordered list (`ol`) with id `nav-list` and classes `nav navbar-nav navbar-right`
   - these classes set up the proper display with bootstrap
 - `li` items with `a` elements pointing to a new page in the website. 
   - Inside the `a` we have `span`s with special classes `glyphicon glyphicon-icon-sign` which give special icon of a sign (glyphicon-icon-cutlery would give a fork & knife)
   - also add a `br`, but only in some sizes! so the `br` looks like: `br class="hidden-xs"`
   - glyphicons come with bootstrap
 - final `li` has the phone number
   - has an `a` field with `href="tel:999-999-9999"` which sets it up to be treated like a phone number by the browser.
   - this `li` also has class `hidden-xs` so it isn't shown in super small layouts - we will put it somewhere else then

Now we can see the menu, but needs some styling.

 - `#nav-list` gets a `margin-top`
 - `#nav-list a` gets a color and centered
 - `#nav-list a:hover` gets a background color
 - `#nav-list a span` gets larger text
 - `#phone` gets a margin top
 - `#phone a` gets right align right and no bottom padding
 - `#phone div` gets color, text-align right, and some right padding.

And now the buttons are styled.

As we squeeze the browser, the middle style is slightly broken, but the smallest form works perfecly with the hamburger menu.

Styling the Collapse button

 - `.navbar-header button.navbar-toggle, .navbar-header .icon-bar` gets styled a `border` of 1px, solid, and colored maroon.

Now pretty - thought it doesnt automatically unselect.

Need to fix the navbar background color by applying hte clear property.

 - `.navbar-header button.navbar-toggle` gets `clear: both;`

Now the backgrounds right, but the Name is a bit too big in the small view.

 - `.navbar-header button.navbar-toggle` add `margin-top: -30px`

now it has made the header smaller, but that's not as nice looking. We'll shrink the name and kosher later to fit.


## Lecture 33: Fixing Navbar Layout, Text, Dropdown Menus

Fixing the medium screen

The problem is both the left div and the right div are too big to fit, so the second div is being pushed down.

We want to shrink the logo on the medium screens.

 - add a style based on media queries.
 - Large: -1200; Medium 992-1199; Small 768-991; XSmall: -768
    - based on bootstrap
 - in the medium `@media`, target the `#logo-img` and swap it for a different image, "restaurant-logo_medium.png" and shrink the padding.

Small, Medium, and Large now look good with their logo handling (big, medium, gone).

Xsmall needs work

 - in the Xsmall `@media` range: 
   - `.navbar-brand` gets less `padding-top` and `height`
   - `.navbar-brand h1` gets less `padding-top` and a `font-size: 5vw`
     - `5vw` means '5% of the viewport width', responsive sizing
   - `.navbar-brand p`'s' font-size and margin shrink.
   - `.navbar-brand p img`'s' gets a `height` limit.

Styling the dropdown menus

At the moment, the glyphicon is way bigger than the corresponding text.

Why is it like that? Inspect the text in the browser an look at the styles. 

 - The text is currently getting it from styles.css getting 16px from the `body` specification. 
 - The glyph is getting it from `#nav-list a span` in styles.css. We want to override that.

 - In our Xsmall `@media` section:
   - `#collapsable-nav a` gets `font-size: 1.2em` 

Now _both_ got 20% bigger! No good. We want the span to inherit the same thing, so set it's font-size to `1em`

  - In our Xsmall `@media` section:
   - `#collapsable-nav a span` gets `font-size: 1em` 

Same size! but let's add a margin

  - In our Xsmall `@media` section:
   - `#collapsable-nav a span` adds `margin-right: 5px`




# Coding the Homepage and the Footer

## Lecture 34: Coding the Jumbotron

## Lecture 35: Coding Navigation Tiles

## Lecture 36: Coding the Footer

# Coding the Restaurant Menu Pages

## Lecture 37: Coding the Menu Categories

## Lecture 38: Coding the Single Menu Category Page

## Lecture 39: Testing the Mobile Version on a Real Phone








