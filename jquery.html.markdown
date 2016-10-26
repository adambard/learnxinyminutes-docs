---
category: tool
tool: jquery
contributors:
    - ["Sawyer Charles", "https://github.com/xssc"]
filename: jquery.js
---

jQuery is a JavaScript library that helps you "do more, write less". It makes many common JavaScript tasks and makes them easier to write. jQuery is used by many big companies and developers everywhere. It makes AJAX, event handling, document manipulation, and much more, easier and faster.

Because jQuery is a JavaScript library you should [learn JavaScript first](https://learnxinyminutes.com/docs/javascript/)

```js


///////////////////////////////////
// 1. Selectors

// Selectors in jQuery are used to select an element
var page = $(window); // Selects the whole viewport

// Selectors can also be CSS selector
var paragraph = $('p'); // Selects all paragraph elements
var table1 = $('#table1'); // Selects element with id 'table1'
var squares = $('.square'); // Selects all elements with the class 'square'
var square_p = $('p.square') // Selects paragraphs with the 'square' class


///////////////////////////////////
// 2. Events and Effects
// jQuery is very good at handling what happens when an event is triggered
// A very common event used is the ready event on the document
// You can use the 'ready' method to wait until the element has finished loading
$(document).ready(function(){
  // Code won't execute until the document is loaded
});
// You can also use defined functions
function onAction() {
  // This is executed when the event is triggered
}
$('#btn').click(onAction); // Invokes onAction on click

// Some other common events are:
$('#btn').dblclick(onAction); // Double click
$('#btn').hover(onAction); // Hovering over
$('#btn').focus(onAction); // On focus
$('#btn').blur(onAction); // Losses focus
$('#btn').submit(onAction); // On submit
$('#btn').select(onAction); // When an element is selected
$('#btn').keydown(onAction); // When a key is pushed down
$('#btn').keyup(onAction); // When a key is released
$('#btn').keypress(onAction); // When a key is pressed
$('#btn').mousemove(onAction); // When the mouse is moved
$('#btn').mouseenter(onAction); // Mouse enters the element
$('#btn').mouseleave(onAction); // Mouse leaves the element


// These can all also trigger the event instead of handling it
// by simply not giving any parameters
$('#btn').dblclick(); // Fires double click on the element

// You can handle multiple events while only using the selector once
$('#btn').on(
  {dblclick: myFunction1} // Triggered on double click
  {blur: myFunction1} // Triggered on blur
);

// You can move and hide elements with some effect methods
$('.table').hide(); # Hides the element(s)

// Note: calling a function in these methods will still hide the element
$('.table').hide(function(){
    // Element hidden then function executed
});

// You can store selectors in variables
var tables = $('.table');

// Some basic document manipulation methods are:
tables.hide(); // Hides element(s)
tables.show(); // Shows (un-hides) element(s)
tables.toggle(); // Changes the hide/show state
tables.fadeOut(); // Fades out
tables.fadeIn(); // Fades in
tables.fadeToggle(); // Fades in or out
tables.fadeTo(0.5); // Fades to an opacity (between 0 and 1)
tables.slideUp(); // Slides up
tables.slideDown(); // Slides down
tables.slideToggle(); // Slides up or down

// All of the above take a speed (milliseconds) and callback function
tables.hide(1000, myFunction); // 1 second hide animation then function

// fadeTo has a required opacity as its second parameter
tables.fadeTo(2000, 0.1, myFunction); // 2 sec. fade to 0.1 opacity then function

// You can get slightly more advanced with the animate method
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// The animate method takes an object of css and values to end with,
// optional options parameter to tune the animation,
// and of course the callback function

///////////////////////////////////
// 3. Manipulation

// These are similar to effects but can do more
$('div').addClass('div'); // Adds class div to all div taming-slim-20

// Common manipulation methods
$('p').append('Hello world'); // Adds to end of element
$('p').attr('class'); // Gets attribute
$('p').attr('class', 'content'); // Sets attribute
$('p').hasClass('div'); // Returns true if it has the class
$('p').height(); // Gets height of element or sets height


// For many manipulation methods, getting info on an element
// will ONLY get the first matching element
$('p').height(); // Gets only the first 'p' tag's height

// You can use each to loop through all the elements
var heights = [];
$('p').each(function() {
  heights.push($(this.height)); // Adds all 'p' tag heights to array
});


``
