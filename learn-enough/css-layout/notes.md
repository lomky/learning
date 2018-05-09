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

```css
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




