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

















