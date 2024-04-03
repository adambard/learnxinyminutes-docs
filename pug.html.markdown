---
language: Pug
contributors:
  - ["Michael Warner", "https://github.com/MichaelJGW"]
filename: index.pug
---

## Getting Started with Pug

Pug is a little language that compiles into the HTML. It has cleaner syntax 
with additional features like if statements and loops. It can also be used 
as a server side templating language for server languages like NodeJS.

### The Language
```pug

//- Single Line Comment

//- Multi Line
    Comment

//- ---TAGS---
//- Basic
div
//- <div></div>
h1
//- <h1></h1>
my-customTag
//- <my-customTag></my-customTag>

//- Sibling
div
div
//- <div></div>
    <div></div>

//- Child
div
  div
//- <div>
      <div></div>
    </div>

//- Text
h1 Hello there
//- <h1>Hello there</h1>

//- Multi Line Text
div.
  Hello
  There
//- <div>
      Hello
      There
    </div>

//- ---ATTRIBUTES---
div(class="my-class" id="my-id" my-custom-attrs="data" enabled)
//- <div class="my-class" id="my-id" my-custom-attrs="data" enabled></div>

//- Short Hand
span.my-class
//- <span class="my-class"></span>
.my-class
//- <div class="my-class"></div>
div#my-id
//- <div id="my-id"></div>
div#my-id.my-class
//- <div class="my-class" id="my-id"></div>


//- ---JS---
- const lang = "pug";

//- Multi Line JS
-
  const lang = "pug";
  const awesome = true;

//- JS Classes
- const myClass = ['class1', 'class2', 'class3']
div(class=myClass)
//- <div class="class1 class2 class3"></div>

//- JS Styles
- const myStyles = {'color':'white', 'background-color':'blue'}
div(styles=myStyles)
//- <div styles="{&quot;color&quot;:&quot;white&quot;,&quot;background-color&quot;:&quot;blue&quot;}"></div>

//- JS Attributes
- const myAttributes = {"src": "photo.png", "alt": "My Photo"}
img&attributes(myAttributes)
//- <img src="photo.png" alt="My Photo">
- let disabled = false
input(type="text" disabled=disabled)
//- <input type="text">
- disabled = true
input(type="text" disabled=disabled)
//- <input type="text" disabled>

//- JS Templating
- const name = "Bob";
h1 Hi #{name}
h1= name
//- <h1>Hi Bob</h1>
//- <h1>Bob</h1>

//- ---LOOPS---

//- 'each' and 'for' do the same thing we will use 'each' only.

each value, i in [1,2,3]
  p=value
//-
  <p>1</p>
  <p>2</p>
  <p>3</p>

each value, index in [1,2,3]
  p=value + '-' + index
//-
  <p>1-0</p>
  <p>2-1</p>
  <p>3-2</p>

each value in []
  p=value
//- 

each value in []
  p=value
else
  p No Values are here

//- <p>No Values are here</p>

//- ---CONDITIONALS---

- const number = 5
if number < 5
  p number is less then 5
else if number > 5
  p number is greater then 5
else
  p number is 5
//- <p>number is 5</p>

- const orderStatus = "Pending";
case orderStatus
  when "Pending"
    p.warn Your order is pending
  when "Completed"
    p.success Order is Completed.
  when -1
    p.error Error Occurred
  default
    p No Order Record Found
//- <p class="warn">Your order is pending</p>

//- --INCLUDE--
//- File path -> "includes/nav.pug"
h1 Company Name
nav
  a(href="index.html") Home
  a(href="about.html") About Us

//- File path -> "index.pug"
html
  body
    include includes/nav.pug
//-
  <html>
    <body>
      <h1>Company Name</h1>
      <nav><a href="index.html">Home</a><a href="about.html">About Us</a></nav>
    </body>
  </html>

//- Importing JS and CSS
script
  include scripts/index.js
style
  include styles/theme.css

//- ---MIXIN---
mixin basic
  div Hello
+basic
//- <div>Hello</div>

mixin comment(name, comment)
  div
    span.comment-name= name
    div.comment-text= comment
+comment("Bob", "This is Awesome")
//- 
  <div>
    <span class="comment-name">Bob</span>
    <div class="comment-text">This is Awesome</div>
  </div>

```


### Additional Resources
- [The Site](https://pugjs.org/)
- [The Docs](https://pugjs.org/api/getting-started.html)
- [Github Repo](https://github.com/pugjs/pug)
