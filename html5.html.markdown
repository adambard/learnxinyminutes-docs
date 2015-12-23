---
language: HTML/HTML5
contributors:
    - ["Walter Cordero", "http://waltercordero.com"]
translators:
    - ["Walter Cordero", "http://waltercordero.com"]     
filename: learnhtml.html
---
HTML5 Tutorial.

In the late 80s, Tim Berners-Lee was working as a physicist at CERN
(CERN). He devised a system for
scientists to share documents over the Internet. Before their invention,
Internet communications only allowed to transmit plain text, using technologies
like email, FTP (File Transfer Protocol). Html the invention allowed the use of
a model content stored on a central server, which could be displayed in a
local terminal using a browser. This simplified and enabled Access to content
the ability to display content "rich" as a sophisticated formatted text
and viewing images.

HTML stands for HyperText Markup Language
which makes high benchmark to markup language for developing web pages. Is a
It is serving as a reference standard for making web pages in different
versions (the latest HTML5). This defines a basic structure and a code for
the definition of contents of a web page, they can be text, images, videos,
among others.

The standard is in charge of the W3C, which is a standardization organization Dedicace
technologies related to web development, primarily in writing and
interpretation. Notably, during their versions they have been added and removed
certain labels that new technologies have become completely obsolete.

Feedback is welcome! You can find me in:
[@WalterC_87](https://twitter.com/WalterC_87), o
[me@waltercordero.com](mailto:me@waltercordero.com).

```HTML
// HTML documents have a .html extension indicating that it is a hypertext file
helloworld.html

// when you start to work a website, the main file called index
index.html

// HTML is a tag-based language, these tags are enclosed in the
// less than (<) and greater than (>) signs
<tag>

/*
  * in most HTML tags have an opening tag and closing the closing tag after the sign (>) 
  * takes a slash (/), telling the interpreter that the label is closing 
  * and there among them ran in hypertext
*/
<tag> Here are content to be intrepreted </tag>

///////////////////////////////////
// 1. Creating HTML Documents

// All new HTML document must be labeled! DOCTYPE followed by "html" indicating
// which is a hypertext document the browser, this label does not have a closing tag
<!DOCTYPE html>

// the structure of an HTML document is made up of three basic tags, html, head and body
// these tags if they have closing tag and opening, it is noteworthy that these 
// tags are unique, this means that there must be a single tag html, head and body
// for each web page
<html>
    <head>
    </head>

    <body>
    </body>
</html>

// Within the <head> is information that is not visible in the body of
// website, in other words is the same configuration, are added here
// tags as the page title, description, writing format, styles and more.

<head>
    <title>title of the web page</title>
    <meta charset="utf-8">
    <style>
        // styles for the site
    </style>
<head>

// it should be clear that the HTML documents are providing the structure that will
// the website, the styles are provided through CSS syntax, this syntax
// It can be stored in external files .css or may be incorporated into
// the <head>. External file to call the <link> is used
<head>
    <link rel="stylesheet" type="text/css" href="url or paht to/file.css"/>
</head>

// within the <body> content will be visible in the browser is
// the visitor, she can go inside paragraph tags, titles, articles, headlines, and more
<body>
    <h1>this is te principal title</h1>
    <p>this represents a paragraph</p>
    <article>this space to add content for an article </article>
</body>

// HTML5 provides specialized tags to create a better structure of each document
// the <nav> tells the interpreter which is a navigation menu.
<nav>
    // Menu elements
</nav>

// the <header> tag indicates the header of a page, an article or a section
<header>
    //is headed, here you can go any tag
</header>

// the <section> tag indicates the creation of diferents sections within 
// a web page, mainly used to divide different part of content
//

// la etiqueta <section> es utilizada para la creacion de diferentes secciones dentro de 
// una página web, utilizada principalmente para dividir diferentes partes de contenido
// inside this tag can be any tag
<section>
    // this represents a section
</section>

<section>
    // this represents a new secction
</section>

// the <article> can be used to represent small portions of HTML that can go inside a <section>
<section>
    <article>
        // here goes the content of the article or information to be displayed
    </article>
</section>

// the <aside> is used for creating sections with extra information that they may be 
// located to the left, right or top of our main content.
<aside>
    <section>
        <p>e.g. extra content for a web page</p>
    </section>
<aside>

// Just as there exists a label for a label headers footers called <footer>, 
// which is used for the footer to any section or to the bottom of a web page.
<footer>
    <p>This content anger beneath a web page</p>
</footer>

///////////////////////////////////
// 2. Text and HTML Typography

// There are certain special characters which have HTML code that can be written in the right way

* (<) code: &lt;
* (>) code: &gt;
* (á) code: &aacute;
* (Á) code: &Aacute;
* (é) code: &eacute;
* (É) code: &Eacute;
* (í) code: &iacute;
* (Í) code: &Iacute;
* (ó) code: &oacute;
* (Ó) code: &Oacute;
* (ú) code: &uacute;
* (Ú) code: &Uacute;
* (ñ) code: &ntilde;
* (Ñ) code: &Ntilde;
* (™) code: &#153;
* (€) code: &euro;
* (ç) code: &ccedil;
* (Ç) code: &Ccedil;
* (ü) code: &uuml;
* (Ü) code: &Uuml;
* (&) code: &amp;
* (¿) code: &iquest;
* (¡) code: &iexcl;
* (") code: &quot;
* (·) code: &middot;
* (º) code: &ordm;
* (ª) code: &ordf;
* (¬) code: &not;
* (©) code: &copy;
* (®) code: &reg;

// Sometimes you need lots of HTML comment to tell other people who find in that section, 
// for it is done using <!-- -->
<!-- This is an example of comment in HTML you will not be seen in the browser -->

// Line breaks are used when you want to make a space between content and another, 
// or to appear in online content area right next to it the label used <br>
<section>
    <p>
        section <br> 1
    </p>
</section>

// in the web browser this looks like:
section
1

// to ensure that the text appears as it has been created you can use the preformatted 
// text tag <pre> </ pre>
<pre>Hi, WELCOME
THIS IS MY WEBSITE
      and this preformatted text</pre>

// which resulted in the browser:
Hi, WELCOME
THIS IS MY WEBSITE
      and this preformatted text

// an element that is often used for the separation of the separating sections is <hr> 
// This tag has certain settings such as alignment, width, size
Start<hr align="left" width="300%" size="5" noshade>Welcome to my web site.

// Sangria is a kind of margin which is established on both sides of the text. 
// to display bleeding, the <blockquote> </ blockquote> insert
Dear users;
<blockquote>
  <blockquote>
  I am pleased to announce that a new section.
  </blockquote>
</blockquote>

// obteniendo como resultado en el navegador:
Dear users;
    I am pleased to announce that a new section.

// The text of a page can be grouped into paragraphs. To do this, the text of each 
// paragraph should be inserted between the <p> and </ p>.  
<p>Welcome to my web site</p>
<p>Here you will find very interesting training courses.</p>     

// There are a number of headers that are used to establish titles within a page. 
// The difference between the different types of header is the font size, the type of 
// highlight, and the spacing between the text and the elements above it and below it

* <h1>
* <h2>
* <h3>
* <h4>
* <h5>
* <h6>

///////////////////////////////////
// 3. Hyperlinks or Links

//A hyperlink, or link, is just a link, which when clicked takes you to a page, file or section. 
//Those elements (text, images, etc.) on which you want to insert a link must be between the <a> 
//and </a> labels. Through the href attribute to the page that is associated with the link, 
//the page displayed is specified when the user clicks on the link.
<a href="http://waltercordero.com">Visítame http://waltercordero.com</a>

// The link target determines which window will be open the linked page is specified 
// through the target attribute that can be assigned the following values:
**_blank: 
    Opens the linked document in a new browser window.

**_parent: 
    Opens the linked document in the frame window that contains the link or parent frameset.

**_self:
    This is the default option. Opens the linked document in the same frame or window as the link.

// There are other types of links that lead to another website, we will see below:
// Email: Open Outlook Express application to write an email, the recipient shall be as specified in the link. 
// For this the reference link should be
// be "mailto:direcciondecorreo".
<a href="mailto:me@waltercordero.com">e-mail to Walter Cordero</a>

// Link to files to download: The href attribute value will normally be a website, 
// but can also be a compressed file, an Excel spreadsheet, Word document, 
// a document with pdf extension. When the link is to a web page dialog box us in the browser prompts 
// the user for permission to download the file on your computer will appear.
<a href="carta.doc" tarjet=_blank >click here; to download the file</a>

///////////////////////////////////
// 4. Image Management

// All pages usually have a number of images that can improve their appearance, 
//or give it more visual information. 
//To insert a picture is necessary to insert the <img>. This label does not need a closing tag. 
// The name of the image must be specified through the src attribute.
<img src="imagenes/logo_animales.gif">

// When an image can not be displayed in the browser, which can occur when you specify 
// the wrong value of the src attribute, a white frame appears with an X in place, along with 
// the name of the imagen.Se you can do that instead of display the name of the custom text image appears, 
// thanks to alt attribute.
<img src="folder/kitty.gif" alt="Cat Image" >

///////////////////////////////////
// 5. Tables

// Before starting with the issue of tables, it should be mentioned that many years ago 
// the pages are created only with tables, a table is created and then were creating the 
// columns and rows within the cells was inserted content.

// Now the use of tables is given exclusively for displaying information, 
// reports or separation and comparison tables, but should never be used 
// to create the structure of a web page.

// The tables are made up of cells, which are the frames obtained as 
// a result of the intersection of a row and a column.

// To create a table you have to insert the <table> and </ table>. 
// Such tags will have to specify the rows and columns that create the table.

// You need to insert the <tr> and </ tr> for each of the rows in the table. 
// These tags must be inserted between the etiqetas <table> and </ table>.
<table>
  <tr>...</tr>
  <tr>...</tr>
  <tr>...</tr>
  <tr>...</tr>
  <tr>...</tr>
</table>

// To create a table is not enough to specify the number of rows, you must also specify 
//the number of columns. You need to insert the <td> and </ td> for each of the cells 
// that make up each of the rows in the table. Therefore, we must insert these tags 
// between the <tr> and </ tr>. Between the <td> and </ td> you can specify the contents of each cell.

<table border="1">
  <tr>
    <td>Saturday</td>
    <td>Sunday</td>
  </tr>
  <tr>
    <td>HTML Course</td>
    <td>JavaScript Course</td>
  </tr>
  <tr>
    <td>AngularJS Course</td>
    <td>ReactJS Course</td>
  </tr>
</table>

// The <td> and </ td> tags are used to define the cells of each of the rows, 
// but instead the <th> and </ th>. For the <th> you can specify the same attributes 
// for the <td>, but this new label makes the cell text appears centered and in bold, 
// so is used to define the headers or titles columns.
<table border="1">
  <tr>
    <th>Saturday</th>
    <th>Sunday</th>
  </tr>
  <tr>
    <td>HTML Course</td>
    <td>JavaScript Course</td>
  </tr>
  <tr>
    <td>AngularJS Course</td>
    <td>ReactJS Course</td>
  </tr>
</table>

// Not only can set titles for the columns, or you can set a title for the table 
// using the <caption> and </ caption>. These labels have to go after the <table>, 
// and can specify the value of align attributes (with the bottom values, center, left, right and top) 
// and valign (with the bottom and top values).
<table border="1">
  <caption align="right" valign="top">Title of the table<tr>
  <tr>
    <th>Saturday</th>
    <th>Sunday</th>
  </tr>
  <tr>
    <td>HTML Course</td>
    <td>JavaScript Course</td>
  </tr>
  <tr>
    <td>AngularJS Course</td>
    <td>ReactJS Course</td>
  </tr>
</table>


// For the <td> and <th> there are colspan and rowspan attributes, which are used to merge cells. 
// Through colspan attribute the number of columns for which the cell is extended is specified, 
// and through rowspan attribute the number of rows that extend the cell specified.

<table width="575" border="2" cellspacing="2">
  <tr align="center" valign="middle"> 
    <th colspan="4">DIFFERENCES BETWEEN DOG AND MAN</th>
  </tr>
  <tr align="center" valign="middle"> 
    <th rowspan="2">DIFFERENCES</th>
    <th colspan="2">DOG</th>
    <th rowspan="2">MAN</th>
  </tr>
  <tr align="center" valign="middle"> 
    <th>Tiny</th>
    <th>Tall</th>
  </tr>
  <tr align="center" valign="middle"> 
    <td>growth duration</td>
    <td>10 months</td>
    <td>18 to 24 months</td>
    <td>16 years</td>
  </tr>
  <tr align="center" valign="middle"> 
    <td>gestational age</td>
    <td colspan="2">58 to 63 days</td>
    <td>9 months</td>
  </tr>
  <tr align="center" valign="middle"> 
    <td>Hair lifespan</td>
    <td colspan="2">1 year</td>
    <td>2 to 7 years</td>
  </tr>
</table>

///////////////////////////////////
// 6. Forms

// A form is an element that can collect data entered by the user.

// The forms are used for the views, concerns, and another set of data about
// users to enter orders through the network, they have many applications.

// A form is made, among other things, labels, text fields, drop down menus, and buttons.

// The forms are inserted through the <form> and </ form>. Such labels will have to 
// insert different objects that make up the form. the <form> has the following attributes:

**action 
indicates an e-mail address to which to send the form, or the direction of the 
program it will process the contents of the form.

**enctype It indicates how the information will be encrypted for shipment. 
Default is set to application / x-www-form-urlencoded.

**method It indicates the method by which the form variables will be transferred. 
Its value can be get or post.

**get It used when they will not produce changes in any document or program 
that is not the user's browser that aims to send the form, 
as when consultations are held on a database.

**post It is used when another they will produce changes, as when the user sends 
data to be stored in a database.

// e.g.
<form action="mailto:formularios@aulaclic.com" method="post" enctype="text/plain" >
  ...
</form>

// Text areas allow users to add multiple lines of text. Therefore, often used to include comments.

// To insert a text area is necessary to include the <textarea> and </ textarea> 
//between the <form> and </ form> tags form
<textarea name="egarea" cols="30" rows="3">Enter the text you want</textarea>

//To insert an input is necessary to include the <input> between the <form> and </ form> form.

// The name attribute specifies the name to be given to the input, through which 
// will be evaluated, and the type attribute indicates the type of input element.

// Let's see the different types of inputs, and other attributes that can be defined for each

**Text field:
// To insert a text field, the type attribute should be the text value.

// The size attribute specifies the number of characters that can be displayed in the
// text field determines the width of the box.

// The maxlenght attribute indicates the number of characters that can be inserted into the text field.

// The value attribute specifies the initial value of the text field
<input name="field" type="text" value="text field" size="20" maxlength="15">

**Password field
// To insert a password field, the type attribute value must have the password.

// The other attributes are the same as for a normal text field. The only difference 
// is that all the letters written in the password field will be displayed as asterisks.
<input name="pass" type="password" value="password" size="20" maxlength="15">

**Button:
// To insert a button, the type attribute must have the value submit, restore or button.

// If the value is to submit, by clicking on the button the form is sent.

// If the value is restore, pressing the button on the form is reset, 
// erasing all the form fields that have been modified and acquiring its initial value.

// If the value is button, pressing the button no action will be performed.

// The value attribute indicates the text that will show the button.
<input name="boton" type="submit" value="Send">

**Checkbox:
// To insert a check box, the type attribute must have the value checkbox.

// The value attribute indicates the value associated with the checkbox.
// You need to put this attribute, but the user can not see its value. It is the value to be sent.

// The appearance of the checked attribute indicates that the box will be initially activated.
// This attribute takes values.
<input name="box" type="checkbox" value="accept" checked>

**Radio Button:
// To insert a radio button, the type attribute value must be the radius.

// The value attribute indicates the value associated with the radio button. 
// You need to put this attribute, but the user can not see its value. It is the value to be sent.

// The appearance of the checked attribute indicates the button is activated initially.
// This attribute takes values.

// Radio buttons are used when you want a form variable can take one of several possible value. 
// To do this, several buttons are inserted with the same name (indicating the variable) and with different values.
// Only one of these buttons can be activated, which is activated when the form is submitted, 
// its value will be the one variable
<input name="prefiere" type="radio" value="estudiar" checked>
<input name="prefiere" type="radio" value="trabajar">


// The selection fields are used to insert menus and drop-down lists.
// To insert one of these menus or lists you need to insert the <select> and </ select> on a form.
// The name attribute specifies the name of the menu or list will be the name of the variable containing the selected value.
// The size attribute indicates the number of items in the list that can be displayed simultaneously,
// Determine the top of the list.

// The emergence of multiple attribute indicates that the user can select multiple items
// The list at the same time helping the Ctrl key. This attribute does not take values.

// The appearance of the disabled attribute indicates that the list is disabled, so that the user can not be
// Select elements. This attribute also takes values.

// Each of the items in the list be inserted between the <option> and </ option>.

// The value attribute indicates the value to send if the item is selected. If this attribute is not specified,
// The text of the option, which is located between the <option> and </ option> will be sent.

// The appearance of the selected attribute indicates the item is selected. This attribute takes values
<select name="animal" size="3" multiple>
  <option selected>---animals---</option>
  <option value="ave">Parrot</option>
  <option>Dog</option>
  <option>Cat</option>   
  <option>Fish</option>
</select>

## Source & References

The [Network Develores from Mozilla](https://developer.mozilla.org/es/docs/Web/Guide/HTML/Introduction_alhtml) 
I provides excellent documentation to HTML for browsers. Moreover, it is in wiki format, 
so while you learn you can help others with your experience.

[HTML course](http://www.aulaclic.es/html) 
an interesting guide that provides some challenges for students to apply what they learned.

[HTML teamtreehouse course](https://teamtreehouse.com/library/html) with Nick Pettit you will learn how 
to spell HTML syntax to create great documents.
