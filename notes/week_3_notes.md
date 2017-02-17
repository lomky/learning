# Coursera HTML, CSS, and Javascript for WebDevs - Week 3 Notes

## Visit with the client & Setup Overview

### Lecture 27: Visit with the Client

#### Ground Rules
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


### Lecture 28: Design Overview

#### Create the design

  - this goes Before the programming  
  - Mock up designing in Balsamiq Mockups  
  - get full mock ups from professional designer  
    - for showing the client  

### Lecture 29: Some Ground Rules & Overview of Setup

#### Some Ground Rules
 
  - use the video features to replay, speed up, etc  
  - grab the available source code  
  - from now on, the current lectures "before" contains the previous chapters "after"  
  - some things will be done offscreen, but will be explained  
  - code & browser switching will happen a lot  
 
#### Set browser-sync running

`browser-sync start --server --directory --files "*"`

  - notice alongside you localhost:3000, there is a 3001 UI.  
    - Explore this on your own.  

#### Setting up our base style

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

## Coding the Navigation Bar of the Site

### Lecture 30: Coding the Basics of Navbar Header

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


### Lecture 31: Coding Button for Future Collapsible Menu

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

### Lecture 32: Coding the Nav Menu Buttons

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


### Lecture 33: Fixing Navbar Layout, Text, Dropdown Menus

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




## Coding the Homepage and the Footer

### Lecture 34: Coding the Jumbotron

This is the giant picture of the restuarant. This is called a "Jumbotron" in bootstrap.

Need a top-level element to keep everything aligned nicely. Let's make a container

  - in our `index.html`, down below the `call-btn`
  - `div` with class `contaier`
    - emmet shortcut: `div.container`+TAB
    - give it the id `main-content`
  - now working inside this div.

We have several sizes of the image. We want to use the proper resolution based on the screen size. Small screen sizes are probably mobile, and band-width limited.

  - create a `div` with class `jumbotron`
  - if we set the img as a background, stretching won't work.
  - instead, `img` tag of the smallest image, add some alt text, and add the classes `img-responsive visible-xs`
    - `img-responsive` is bootstrap, as the browser width changes the image changes proportionally.
 
Now in the css, load the images in the proper media queries.

  - in the large @media, create css for `.jumbotron`
    - `background: url(../img-lg) no-repeat`, `height: 675px`, `box-shadow`, `border`
  - for medium `@media`, `.jumbotron`:
    - `background: url(../img-md) no-repeat`, `height: 558px`, `box-shadow`, `border`
  - for small `@media`, `.jumbotron`:
    - `background: url(../img-sm) no-repeat`, `height: 432px`, `box-shadow`, `border`
  - `box-shadow` and `border` could be shared outside of `@media`, as they do not change

This way of doing it always loads the smallest image (xs), but won't load the larger images unless the larger size is met. Good for saving mobile data.

XSmall screen has bad jumbotron sizing still, let's fix that.

  - in it's `@media`, give `.jumbotron` a `margin-top`, `padding`, `box-shadow` and `border`

Still has white bars on the size. Why? Inspect the div in the browser. oh, we have padding still. Why isn't our `padding: 0` applying? Because we have a more specific one applying from bootstrap (two classes).

  - Change the xs `@media` to target `.contrainer .jumbotron`
  - actually, go ahead and do that on our other `@media` `.jumbotron` as well, to avoid future headaches.

Let's move our `box-shadow` and `border` out of the `@media` queries. Move them to `.container .jumbotron` in the normal level

### Lecture 35: Coding Navigation Tiles

Now working on the Menu, the Specials, and the Map.

First, look over how the behavior changes in smaller views.

Desktop and Tablet are the same, 3 in a row  
Small is different, now we have Menu and Specials in one row, map below.  
Xsmall also different, now just stack them.  

We want to stay inside our `main-content` `div`. So we already have our required `container` class item.
 
  - create a row to hold it `div id="home-tiles" class="row"` (emmet: `div.row`+TAB)
    - Menu & Specials: `div` of class `col-md-4 col-sm-6 col-xs-12`
      - an `a` tag to the page (menu categories page or specials page)
        - a `div` with id `menu-tile` or `specials-tile`
          - a `span` containing the text
        - The `div` will be styled as a square with an image. The `span` will be absolutely positioned within it.
    - Map: `div` of class `col-md-4 col-sm-12 col-xs-12`
      - the rest of the structure is mostly the same
        - we give a `target="_blank"`
  
Doesn't look like much yet. Let's style it.

  - `#menu-tile, #specials-tile, #map-tile`
    - `height: 250px`, `border`
    - `width: 100%` /** 100% _of the div_ **/
    - `margin-bottom: 15px`
    - `postition: relative` /** allows you to anchor child elements absolutely **/
    - `overflow: hidden`  /** chop off any overflow **/
  - `#menu-tile:hover, #specials-tile:hover, #map-tile:hover`
    - `box-shadow: 0 1px 5px 1px #cccccc;`
       - shows the user they are above it

now it looks better! sized properly and you can see your hover.

Now we need to add the pictures and style the labels.

Placing the images

If you don't have a ready placeholder, you can get one from placehold.it

  - `#menu-tile`
    - `background: url(../images/menu-tile.jpg) no-repeat`
    - `background-position: center`
      - we know our image content is centered, so this works
      - as the div shrinks, this should still look fine in this use case
  - `#specials-tile`
    - `background: url(../images/specials-tile.jpg) no-repeat`
    - `background-position: center`

And the titles

  - `#menu-tile span, #specials-tile span, #map-tile span`
    - `position: absolute`, `bottom: 0`, `right: 0`, `width: 100%`, `text-align: center`, `font-size`, `text-transform: uppercase`, `background-color: #000`, `color: #fff`, `opacity: .8`

Embedding the map

  - go to google maps and search for the location.
  - go to the menu and hit "share or embed map"
  - take the share link and put that into your href for the map tile
  - go to the embed map and take that `iframe` to paste into you `div id="map-tile"`
    - need to style the `iframe`
      - adjust it's inline styling to match the tiles
      - `height: 250px`, `width: 100%`

All looks good. How is the responsiveness? Large good, medium good. Small hops the map down nicely. Xsmall: stacking is nice, but the menu & specials looks weird

Try to fix it in the browser developer tools!

  - Change it's width to 360px?
  - good, but now it's over to the left
  - add `margin: 0 auto 15px auto`

Looks good! paste that new css into the right `@media` section. Target the IDs and boom, fixed!

You can simulate real devices in chrome. iPhone 4 - the menu still looks quite too big. Let's make an even smaller `@media`.

  - `@media (max-width: 479px)`
    - `#menu-tile, #specials-tile`: `width` and `margin` shrink

Restarant name should be bigger.

  - in that smallest `@media` query: `.navbar-brand h1` gets `padding` (a bit less) and `font-size` (a bit bigger)

### Lecture 36: Coding the Footer

Tip: on your `</div>` add a comment of what div you are closing!

We want to position below the `main-content`

Use the semantic tag `footer` class `panel-footer` with a `div` inside it with class `container` (for bootstrap & alignment) and finally a `div` of class `row`.

Three `section`s:

  - hours
  - address
  - testimonials

We want it to stack when it gets smaller than the small screen size, so we add class `col-sm-4` to the `section` tags. In small and larger, it's in three sections. Below that, they stack.

Inside the `sections`:

  - a `span` with "Hours:" or "Address:" in it
  - and the hours/address listed with `<br>`s
  - finally, a `<br class="visible-xs">
    - this makes it stack nicely when they change to stacking.
  - testimonials is the same but no span.

Finally, a copyright notice at the bottom

Now we look at it. The colors are bad, but responsible behavior is okay!

In the styles, new comment section for footer.

  - `.panel-footer`
    - `margin-top`, `padding-top`, `padding-bottom`, `background-color`, `border-top: 0`
  - notice this is a bootstrap class. that's okay! cascading!
  - `.panel-footer div.row` get `margin-bottom` to space away the copyright.
  - `#hours, #address` gets `line-height: 2` for legibility
  - `#hours > span, #address > span` gets `font-size: 1.3em`
  - `#address p` gets `color`, `font-size`, `line-height`
  - `#testimonials` gets `font-style: italics`
  - `#testimonials p:nth-child(2)` gets `margin-top: 25px` /** separates the testimonials **/

Looks good in desktop. Now how about the smaller screens?

Large looks fine, medium good. small fine.

Xsmall stacks them nicely, but maybe if they were spaced better and centered?

  - `@media` query for xsmall
    - `.panel-footer section` 
      - `margin-bottom: 30px` and `text-align: center`
    - `.panel-footer section:nth-child(3)` 
      - `margin-bottom: 0px` /* already exists on row */
    - `.panel-footer section hr` 
      - `width: 50%`

Now looks pretty nice, even in the smallest settings. Main page is now complete!











## Coding the Restaurant Menu Pages

### Lecture 37: Coding the Menu Categories

### Lecture 38: Coding the Single Menu Category Page

### Lecture 39: Testing the Mobile Version on a Real Phone








