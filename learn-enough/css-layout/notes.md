# Learn Enough CSS & Layout

## Introduction to CSS

### Intro 

Goal is to show how to design a real web site, not
a reference to all teh CSS properties. See the [MDN reference](https://developer.mozilla.org/en-US/docs/Web/CSS)
for that.

AKA? Front end web design!

Builds directly on [Learn Enough HTML](../html).

  - Ch1: basics of CSS declarations & values, staying DRY
  - Ch2: good naming & managing complexity in a project
  - Ch3: CSS values: colors & sizes
  - Ch4: the _box model_, fitting page elements together
  - Ch5&6: _Jekyll_ templating system
  - Ch7: flexible page layouts with _flexbox_
  - Ch8: creating a blog
  - Ch9: mobile friendly sites with media queries
  - Ch10: polishing a site for real use

### CSS Overview & History

Cascading: style flows into child elements  
Style sheet: collect it all!  

Browers support of the spec can preceed and lag at times.  
Can include browser specific implementations.  
Check for browser support: [CanIUse](http://www.caniuse.com/)  

**CSS Roots**  

The web began plaintext, then HTML.  
Originally, styling was user controlled.  
A bunch of ways to style came to be, but none of them are direct CSS ancestors.  

CSS came about ~1996 as a thesis. W3C adopted it.  
Browser support was slower - first full support ~2000.  
Spotty and varying support (*cough*IE6*cough*)  
Standard, reliable support is _very recent_. Around _2012_  

Very often, theres More Than One Way To Do It with CSS.  

Best practices are sometimes fads rather than facts.  
_Consistency_ is always best!  

### Starting Styling

Working in [the sample site](./sample_webpage/index.html).  

Building inline stylesheets via `style` tags.  
These can go anywhere, but `head` is conventional.

```
...
a {
  color: red;
}
div {
  border: 1px solid black;
}
div a {
  color: green;
}
li a {
  color: purple;
}
li {
  border: 1px solid black;
}
...
```
 

### CSS Selectors

`id` and `class`.  
individual vs group.  

Suggested best practices:  

  - only one `id` per element
  - numbers should start the name
  - `-`, `_`, & `CamelCase` to multiword
  - spaces are invalid
  - consistency!

## Style of Style

CSS Considerations:
  - from the browser: no real distinction between inline and well formatted.
  - from the text editor: huge difference in maintainability

Making good choices wrt naming & structure to 
improve maintainability moving forward

### Naming Things

  - Name by intent
    + `box2` - bad, too generic
    + `bio-box` - good, refers to intent (box for biographies)
  - Avoid naming by appearance. Use _functional_ naming.
    + class `red` to highlight, - no! what if you want purple later?
    + class `alert` instead
    + use `collapse` over `small`
    + use `disabled` rather than `grey`
  -  See some strict naming conventions:
    +  [BEM](http://csswizardry.com/2013/01/mindbemding-getting-your-head-round-bem-syntax/)
    +  [OOCSS](https://www.smashingmagazine.com/2011/12/an-introduction-to-object-oriented-css-oocss/)
    +  [SMACSS](https://smacss.com/)

### When & Why: Classes vs IDs


**ID**: Unique. One allowed per object.  
**Class**: multi. Many allowed per object.  

Browsers _treat them differently_!

**ID**s have higher _specificity_, and thus override 
classes. This makes the handling hard to override with
classes, and the styling ends up full of hacks.

LE suggestion:

> You should strive to use ids only when you absolutely have 
> to (for example, if you are using JavaScript, and then 
> use them only for JavaScript).

Classes: Machine guns spraying style.  
IDs: _Rocket Launcher_

### Priority & Specificity

CSS designed to allow multiple sheets with overriding 
properties.

Final declarations win:

```
  .bio-box {
    width: 75%;
  }
  .bio-box {
    width: 50%;
  }
```

Results in `width` being `50%`.

Full priority list:

  1. Importance (`!important`) (NEVER use this)
  2. Inline
  3. Media Type
  4. User defined (local, accessibility)
  5. Selector specificity (`class` and `id` overwrite generic)
  6. Rule order (last wins)
  7. Parent inherit 
  8. CSS (`style` blocks for generic elements)
  9. Browser default


What happens with priority ties? **Specificity**

The more specifically you target an element, the
greater the strength.

```
# Not very specific
a {
  color: gray;
}

# More specific
h1 a {
  color: green;
}

```

Less to More Specific, _ish_:

 - Simple HTML selector  `em {color: #fff;}`  
 - HTML selector targeting element inside another element  `h1 em {color:  - #00ff00;}`  
 - CSS class name  `.alert {color: #ff0000;}`  
 - HTML element with a class name  `p.safe {color: #0000ff;}`  
 - CSS id  `#thing {color: #823706;}`  
 - CSS id with a class name  `#thing .property {color: #823706;}`  
 - Inline style  `style="color: transparent;"`   

### Good Styling Citizen

Build your classes like they're legos, easy to snap
together. Modular styles!

Multiple classes on a selectior is fine! But don't
overdo it.

Two categories of styling:  

  - global styles
    + affect many different places
    + create consistency
  - individual sections
    + self contained modules of functionality or content

Three selector Limit - don't create style rules with 
more than three selectors.

**Group your styles & Add Comments!**

## CSS Values: Color & Size

### CSS Color

Names: `red`, `green`, `lightgray`.  
CSS supports lots of these. [Ref list](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value)

Hex: `#1379bd`  
`red` is `#ff0000`.  
short form of above: `#f00`  
A few more to remember:
  - `#000` is black
  - `#fff` is white
  - `#00f` is blue
  - `#0f0` is green

rgb: `rgb(255, 255, 255)` is `#fff`.  
Also gives access to _transparency_ via `rgba`.  
the _a_ is for _alpha_ (as in alpha level).  
`0` is transparent, `1` is opaque. 
Decimals indicate percent. `0.5` is 50% transparent.

There are _even more ways_ to set color in CSS.  
HSL, HSLa.  
Not covering here.

### Intro to Sizing

_so many ways_ to set size.  
Browser implementation is super kludged.

Pixels: `px`.  
great, if everyone had the same size resolution.  
varying pixel density throws this out.  

Absolute sizes are a way of the past.

_relative_ sizes: size based on other elements.  
allows you to scale various screens, 
and deal with resizing easily.

### Pixels & Points

Pixel: `px`  
Point: `pt`

Absolutely measurements defined as `1/96` of an
inch for `px` and `1/72` of an inch for `pt`.

Now ignore `pt`. Like everyone else.  
Some people _hate_ `px`.  
LE stance: use `px` if it makes sense, makes the
work easier, and doesn't break the layout. Otherwise,
use relative.  

Don't use pixel sizes for fonts!

_DO_ use pixels for border width.

### Percentages

Can be useful for relative sizing.  
Caveats!!: 
  - percentage sizing is based on _parent container_, not browser or page size
  - percentage heights are weird as they require the parent container to have a _set_ height.

You cannot use percentage for borders.

Percentage height requires the parent 
to have a defined absolute height. Without it
the browser makes it just big enough to hold the
content. Don't use it.

We'll find a better solution to height sizing
later on.

**Percentage fonts**  
Works! But remember: it based the size on whatever
the `font-size` the container has inherited. Not
on the container size. A box `1000px` tall with
parent font size `16px` and child font size `50%`
will have `8px` font!  

This weird sourcing is why percentage font size is
rarely used. Most people think in sizes of the 
containers of the box shaped things.

### `em`

`em` is a relative size unit commonly used to
size text. (Preferred, even).
Named because of the width of an 'm'.
In CSS, one `em` is the number of pixels equal to
the current font size of the given elements parent
container. Fallback is default page font size.  
Plaintext default is `16px`.  
so, `0.5em` would be `8px`.  
and, `2.25em` would be `36px`.  


If you use `em` throughout your site, changing the
base font size will adjust the whole site accordingly.

`em` changes are _cumulative_. 0.5 inside another 0.5
will result in a 0.25 sized font. Use properly.

You can size things like margins with `em`s. Maybe you want
to base your sizing on the fontsize. Maybe you don't. Be aware
of flame wars and such, do what is right for the project and
be consistent.  

### `rem`

Like `em`, but instead of parsing its parent tree, `rem` goes
back to the font size defined on the `html` tag - the font size
for the page.

Best practice is to use a `rem` unit for the font-size at the
top of your 'module', and then style all inside with `em`. Now
you can move that module without worrying about cumulative explosions.

### `vh` & `vw`

Newer mobile-friendly units.  
Two-dimensional.  

`vh` - viewport height  
`vw` - viewport width  

Sizes based on the actual size of the browser window / mobile screen.  
1 vh/vw is 1% of the screen dimension.  
`3vh` is 3% of the height.  
`100vw` is 100% of the width.  

Not affected by parent element sizing. No cumulative effects.
Everything is based on the window size.  

These are newer, so some older browsers lack support. Be aware.

Space _will be left_ for the browsers default margin. These defaults
can be adjusted with a _reset_.

Also works on fonts!

All of this is nice - expect mobile screens and desktop screens are
so distinct as to render a good style in one awful in the other.  
*cough* media queries! *cough*


### Pleasing fonts

The goal is readable text.

  - Use relative units for text
  - don't use the 62.5% trickt
  - pick random numbers that set the font close to your design needs.

Don't seek pixel perfect. Seek good design and maintainable styles.














