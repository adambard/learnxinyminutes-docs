---
language: html
filename: learnhtml.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Dimitri Kokkonis", "https://github.com/kokkonisd"]
---

HTML stands for Hypertext Markup Language.

It is a language which allows us to write pages for the World Wide Web.
It is a markup language, it enables us to write webpages using code to indicate
how text and data should be displayed.  In fact, HTML files are simple text
files.

What is this markup? It is a method of organising the page's data by
surrounding it with opening tags and closing tags.  This markup serves to give
significance to the text that it encloses.  Like other computer languages, HTML
has many versions. Here we will talk about HTML5.

**NOTE :**  You can test the different tags and elements as you progress through
the tutorial on a site like [codepen](http://codepen.io/pen/) in order to see
their effects, understand how they work and familiarise yourself with the
language.  This article is concerned principally with HTML syntax and some
useful tips.


```html
<!-- Comments are enclosed like this line! -->

<!--
	Comments
	can
	span
	multiple
	lines!
-->

<!-- #################### The Tags #################### -->

<!-- Here is an example HTML file that we are going to analyse. -->


<!doctype html>
	<html>
		<head>
			<title>My Site</title>
		</head>
		<body>
			<h1>Hello, world!</h1>
			<a href="http://codepen.io/anon/pen/xwjLbZ">
				Come look at what this shows
			</a>
			<p>This is a paragraph.</p>
			<p>This is another paragraph.</p>
			<ul>
				<li>This is an item in a non-enumerated list (bullet list)</li>
				<li>This is another item</li>
				<li>And this is the last item on the list</li>
			</ul>
		</body>
	</html>

<!--
	An HTML file always starts by indicating to the browser that the page is HTML.
-->
<!doctype html>

<!-- After this, it starts by opening an <html> tag. -->
<html>

<!-- that will be closed at the end of the file with </html>. -->
</html>

<!-- Nothing should appear after this final tag. -->

<!-- Inside (between the opening and closing tags <html></html>), we find: -->

<!-- A header defined by <head> (it must be closed with </head>). -->
<!--
	The header contains some description and additional information which are not
	displayed; this is metadata.
-->

<head>
	<!--
		The tag <title> indicates to the browser the title to show in browser
		window's title bar and tab name.
	-->
	<title>My Site</title>
</head>

<!-- After the <head> section, we find the tag - <body> -->
<!-- Until this point, nothing described will show up in the browser window. -->
<!-- We must fill the body with the content to be displayed. -->

<body>
	<!-- The h1 tag creates a title. -->
	<h1>Hello, world!</h1>
	<!--
		There are also subtitles to <h1> from the most important (h2) to the most
		precise (h6).
	-->

	<!-- a hyperlink to the url given by the attribute href="" -->
	<a href="http://codepen.io/anon/pen/xwjLbZ">
		Come look at what this shows
	</a>

	<!-- The tag <p> lets us include text in the html page. -->
	<p>This is a paragraph.</p>
	<p>This is another paragraph.</p>

	<!-- The tag <ul> creates a bullet list. -->
	<!--
		To have a numbered list instead we would use <ol> giving 1. for the first
		element, 2. for the second, etc.
	-->
	<ul>
		<li>This is an item in a non-enumerated list (bullet list)</li>
		<li>This is another item</li>
		<li>And this is the last item on the list</li>
	</ul>
</body>

<!-- And that's it, creating an HTML file can be simple. -->

<!-- But it is possible to add many additional types of HTML tags. -->

<!-- The <img /> tag is used to insert an image. -->
<!--
	The source of the image is indicated using the attribute src=""
	The source can be an URL or even path to a file on your computer.
-->
<img src="http://i.imgur.com/XWG0O.gif"/>

<!-- It is also possible to create a table. -->

<!-- We open a <table> element. -->
<table>

	<!-- <tr> allows us to create a row. -->
	<tr>

		<!-- <th> allows us to give a title to a table column. -->
		<th>First Header</th>
		<th>Second Header</th>
	</tr>

	<tr>

		<!-- <td> allows us to create a table cell. -->
		<td>first row, first column</td>
		<td>first row, second column</td>
	</tr>

	<tr>
		<td>second row, first column</td>
		<td>second row, second column</td>
	</tr>
</table>
```

## Usage

HTML is written in files ending with `.html` or `.htm`. The mime type is
`text/html`.
**HTML is NOT a programming language**
## To Learn More

* [Wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML Tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3Schools](http://www.w3schools.com/html/html_intro.asp)
