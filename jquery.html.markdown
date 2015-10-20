---
category: library
+library: jquery
+contributors:
+    - ["Sawyer Charles", "https://github.com/xssc"]
+filename: jquery.js
+---

jQuery is a JavaScript library that helps you "do more, write less". It makes many common JavaScript tasks and makes them easier to write. jQuery is used by many big companies and developers everywhere. It makes AJAX, event handling, document manipulation, and much more, easier and faster.

Because jQuery is a JavaScript library you should [learn JavaScript first](https://github.com/adambard/learnxinyminutes-docs/blob/master/javascript.html.markdown)

```js
// Basic jQuery syntax is $(selector).action()
$(this).click(); // Clicks the current element

///////////////////////////////////
// 1. Selectors

// Selectors can also be CSS selector
var paragraph = $('p'); // Selects all paragraph elements
var table1 = $('#table1'); // Selects element with id 'table1'
var squares = $('.square'); // Selects all elements with the class 'square'
var square_p = $('p.square') // Selects paragraphs with the 'square' class


///////////////////////////////////
// 2. Events and Methods

// A very common event used is the ready event on the document
// You can use the 'ready' method to wait until the element has finished loading
$(document).ready(function(){
  // Code won't execute until the document is loaded
});

// The document ready can be shortened to this:
$(function(){
  // Same as $(document).ready();
});

// jQuery is very good at triggering events
// and also handling what happens when an event is triggered
$('#button').click(); // Fires a click event on $('#button')
$('#button').click(function(){
  // Code here gets executed when the #button element is clicked
});

function onAction() {
  // This is executed when the event is triggered
}

// Some common events are:
$('#btn').dblclick(onAction); // Double click
$('#btn').mouseenter(onAction); // Mouse enters the element
$('#btn').mouseleave(onAction); // Mouse leaves the element
$('#btn').hover(onAction); // Hovering over
$('#btn').focus(onAction); // On focus
$('#btn').blur(onAction); // Losses focus
$('#btn').submit(onAction); // On submit

// You can also use an anonymous function
$('#btn').hover(function(){
  // Executed on hover
});

// These can all also trigger the event instead of handling it
// by simply not giving any parameters
$('#btn').dblclick(); // Fires double click on the element


``
