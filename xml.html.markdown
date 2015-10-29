---
language: xml
filename: learnxml.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
  - ["Rachel Stiyer", "https://github.com/rstiyer"]
  - ["John Tyndall", "https://github.com/jbtyndall"]
---

XML is a markup language designed to describe data.

Unlike HTML, XML does not specify how to display or format data; it is a representation of data that allows it to be stored, transported, and shared via independent means.

## XML Syntax

An XML document is an *ordered*, labeled tree comprised of **nodes** (i.e., **elements**).

```xml
<?xml version="1.0" encoding="UTF-8"?>
<bookstore>
  <book category="COOKING">
    <title lang="en">Everyday Italian</title>
    <author>Giada De Laurentiis</author>
    <year>2005</year>
    <price>30.00</price>
  </book>
  <book category="CHILDREN">
    <title lang="en">Harry Potter</title>
    <author>J K. Rowling</author>
    <year>2005</year>
    <price>29.99</price>
  </book>
  <book category="WEB">
    <title lang="en">Learning XML</title>
    <author>Erik T. Ray</author>
    <year>2003</year>
    <price>39.95</price>
  </book>
</bookstore>

<!-- This is a comment in XML -->
```
In the XML above, the first line is the  **declaration**, which gives an XML parser information about the XML document. Declarations are optional, but must be the first line if included.

Each XML document has a unique **root** element, which is the **parent** element of all other elements.
* e.g., `<bookstore>` is the root element

Elements can have **child** elements.
* e.g., `<bookstore>` has 3 children (of type `<book>`); these children have their own children (`<title>`, `<author>`, etc.).

### Elements and Attributes

Data can be represented in one of two ways:
* **Elements** &mdash; named tags that can contain text, attributes, other elements, or nothing (i.e., an empty element).
* **Attributes** &mdash; data related to a specific element.

```xml
<title lang="en">Everyday Italian</title>
```
This element has an opening (`<title>`) and closing (`</title>`) tag; its value is everything between these tags (`Everyday Italian`).

The element has one attribute, `lang`; the attribute's value (always denoted by quotes) is `en`.

## Well-Formed and Valid XML

An XML document is **well-formed** if it conforms to basic XML syntax rules:
```xml
<bookstore>
  <book category="COOKING">
    <title>Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>

<!-- This XML document is well-formed because it has a single root as well as opening and closing tags nested in the proper order. -->
```

Well-formed XML can still contain semantic errors or inconsistencies:

```xml
<bookstore>
  <book category="COOKING">
    <title>Everyday Italian</title>
    <price>30.00</price>
  </book>
  <book category="CHILDREN">
    <title lang="en">Harry Potter</title>
    <author>J K. Rowling</author>
    <price>29.99</price>
    <year>2005</year>
  </book>
</bookstore>

<!-- This XML document is well-formed; however, the book elements are inconsistent. -->
```
The structure of XML documents can be standardized (or defined), allowing them to be transferred and stored in an expected format. Well-formed XML documents are **valid** if they follow the rules of these definitions.

###Document Type Definition (DTD)

The following simplified `<bookstore>` XML document uses a Document Type Definition (DTD) to define its structure: 
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE bookstore [
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>
<bookstore>
  <book category="COOKING">
    <title>Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>
```
The DTD specifies the following about `<bookstore>` XML documents:
* The root element must be `<bookstore>`
* The `<bookstore>` element can have 1 or more `<book>` child elements
* Each `<book>` element must contain exactly 1 `<title>` and `<price>` element
* Each `<book>` element must have a `<category>` attribute; the default value is `Literature`
* The `<title>` and `<price>` elements contain *parsed character data*.

The DTD is considered **internal** since it is defined within the `<bookstore>` XML document.

An **external** DTD means that the DTD is declared in a separate file (e.g., `bookstore.dtd`):

```xml
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>

```
The DTD is referenced in the XML document as follows:
```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE bookstore SYSTEM "bookstore.dtd">
<bookstore>
  <book category="COOKING">
    <title>Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>
```

## Resources
* [Validate an XML File](http://www.xmlvalidation.com)

## Further Reading
* [XML Tutorial](http://www.w3schools.com/xml)
* [Extensible Markup Language (XML)](http://www.w3.org/TR/xml)
* [XML Path Language (XPath)](http://www.w3.org/TR/xpath)
