---
language: html
filename: learnhtml.html
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
  - ["Martin Naumann", "http://www.geekonaut.de"]
---

HTML, for HyperText Markup Language, is a declarative language used for creating and visually representing a webpage. Web browsers can read HTML files and render them into visible or audible web pages.

HTML is a subset of Standard Generalized Markup Language (SGML) and is specified by the World Wide Web Consortium (W3C).

```html

<!DOCTYPE html>

<!--  Comments are between arrows. -->

<!-- <!DOCTYPE> informs the browser which version of HTML you used.
	 You cannot write valid HTML before it. -->

<!--  HTML is written in the form of nested HTML elements consisting of
      tags enclosed in angle brackets : <tag>. A lot of tags exist.
      We cannot describe all of them. -->

<!--  Always start with the tag <html>. It represents the root of your page. -->
<html>

	<!--  An HTML page is divide in two singles parts.
    		<head> provides general information about the document.
    		<body> represents the content of an HTML document. -->
  <head>
      <!-- We write in utf-8. -->
    <meta charset="utf-8" />

    <title>Here we write the Document title</title>
  </head>
  <body>
  	<!-- Here we write the Document content -->
  	<!-- HTML is a semantic langage. Use the rightest possible element. -->

		<!-- <div> is a basic block element. -->
    <div>
    	<!-- There are 6 levels of heading. <h1> is the largest
           and <h6> is the smallest. A heading element briefly
           describes the topic of the section it introduces.
           It adds a newline character automatically at the end of each one. -->
      <h1>Main heading in this document</h1>

      <p>This is a paragraph.</p>

			<!-- HTML supports attributes. -->
			<!-- The id global attribute defines a unique identifier which must be
					 unique in the whole document. Its purpose is to identify the element
					 when linking, scripting, or styling. -->
      <p id="information">
        Paragraphs are a one or multiple adjacent lines of text
        separated by one or multiple blank lines.
      </p>
      <p>
        This is another paragraph. This is <strong>important</strong>.
        To break the line, use <br />.

		    <!-- With hyperlink we can navigate anywhere! As you can see, there are more attributes, like "href" to point to the target of the link or "title" to tell the browser what the link shall be described as when hovering-->
		    <a href="example.org" title="example link">Go to example!</a>

        <!-- Hyperlinks can also go to anchors or elements with a specific id in the same document -->
        <a href="#bottom">Go to bottom</a>

		    <!-- <span> is a basic inline element. -->
        <span class="alertText">Congratulations!</span>

        <!-- you can also have custom attributes, by the way. That's often handy for scripting or styling elements
             they are ignored by browsers, though and must always begin with "data-" -->
        <span data-translation="Hello!">Hola!</span>
      </p>

		  <!-- HTML supports image, audio, video and more. -->
      <img src="example.jpg" alt="this is an example" />

      <!-- HTML can also do lists -->

      <!-- either with numbering -->
      <ol>
        <li>First item</li>
        <li>Second item</li>
        <li>Third item</li>
        <li>and so on...</li>
      </ol>

      <!-- or without numbering -->
      <ul>
        <li>First item</li>
        <li>Second item</li>
        <li>Third item</li>
        <li>and so on...</li>
      </ul>
    </div>
    <!-- anchors can also be used to mark particular points in the document instead of other documents.
         The following link can be used to jump to the bottom, either from another link in this document or by appending #bottom to the address of this document -->
    <a name="bottom" />
  </body>
  <!--  Never forget to close your tag! -->
</html>

```

## Additional resources

- [W3C](http://www.w3.org/TR/html/) - Official specification of HTML
- [W3C Validator](https://validator.w3.org/) - Official HTML Validator
- [WebPlatform.org](http://www.webplatform.org/) - Authoritative resource for documentation
- [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTML/) - Information about Open Web technologies
- [CSS-Tricks](https://css-tricks.com/) - Tips, Tricks, and Techniques on using CSS.
- [HTML5 Rocks](http://www.html5rocks.com/en/) - A resource for open web HTML5 developer
- [Encoded characters table](http://dev.w3.org/html5/html-author/charref) -
- [Can I use ... ?](http://caniuse.com/) - Website about web browser compatibility
