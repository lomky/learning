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