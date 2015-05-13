---
language: html
filename: learnhtml.html
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
---

HTML, for HyperText Markup Language, is a declarative language used for creating and visually representing a webpage. Web browsers can read HTML files and render them into visible or audible web pages.
 
HTML is a subset of Standard Generalized Markup Language (SGML) and is specified by the World Wide Web Consortium (W3C).

```html

<!--  Comments are between arrows. -->

<!--  HTML is written in the form of nested HTML elements consisting of
      tags enclosed in angle brackets : <tag>. -->
  
<!-- <!DOCTYPE> informs the browser which version of HTML you used. -->
<!DOCTYPE html>
  
<!--  Always start with the tag <html>. It represents the root of your page. -->
<html>
<!--  An HTML page is divide in two singles parts.
      <head> provides general information about the document.
      <body> represents the content of an HTML document. -->
  <head>
    <title>Here we write the Document title</title>
    <!-- A lot of metadata tags exist.  -->
  </head>
  <body>
    <!-- Here we write the Document content -->
    <!-- HTML is a semantic langage. Use the rightest possible element. -->

    
    <div id="main">
    <!--  There are 6 levels of heading. <h1> is the largest
          and h6 is the smallest) -->
      <h1>Main heading in my document</h1>
      
      <p>This is a paragraph.</p>
      
      <p>
        Paragraphs are a one or multiple adjacent lines of text
        separated by one or multiple blank lines.
      </p>
  
      <p>
        This is another paragraph. This is <strong>important</strong>.
        To break the line, use <br />
        
        <span class="alertText">Congratulations!</span
      </p>
      
      <img src="example.jpg" alt="this is an example" />
    </div>
  </body>
  <!--  Never forget to close your tag! -->
</html>

<!--  Less than 2 minutes and you have already created your first page! -->

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
