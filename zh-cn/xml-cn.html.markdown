---
language: xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Zach Zhang", "https://github.com/checkcheckzz"]
filename: learnxml-cn.xml
lang: zh-cn
---

XML是一种标记语言，被设计用来存储数据和传输数据。

不像HTML, XML不指定怎样显示或格式化数据，只是携带它。


* XML 语法

```xml
<!-- XML中的注解像这样 -->

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

<!-- 上面是一个典型的XML文件。
  它以一个声明开始，通知一些元数据（自选的）
  
  XML使用一个树的结构。上面的文件中，根节点是'bookstore'，它有三个孩子节点，
  所有的'books'。那些节点有更多的孩子节点，等等。。。
  
  节点用开放/关闭标签创建， 并且孩子就是在开发和关闭标签之间的节点。-->



<!-- XML 携带两类信息:
  1 - 属性 -> 那是关于一个元素的元数据。
      通常，XML解析器使用这些信息去正确地存储数据。
	  它通过在开放标签里出现在插入语中来表示。
  2 - 元素 -> 那是纯数据。
      那就是解析器将从XML文件提取的东西。
	  元素出现在开放和关闭标签之间，没插入语。-->
      
  
<!-- 下面, 一个有两个属性的元素-->
<file type="gif" id="4293">computer.gif</file>


```

* 良好格式的文件 x 验证

一个XML文件是良好格式的如果它是语法正确的。
但是， 使用文件定义，比如DTD和XML概要，在文件中插入更多的限制是可能的。

一个遵守一个文件定义的XML文件被叫做有效的，对于那个文件来说。

有了这个工具，你能够在应用逻辑之外检查XML数据。

```xml

<!-- 下面, 你能够看到一个简化版本的增加了DTD定义的bookstore文件。-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Bookstore.dtd">
<bookstore>
  <book category="COOKING">
    <title >Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>

<!-- 这个DTD可能是像这样的:-->

<!DOCTYPE note
[
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>


<!-- 这个DTD以一个声明开始。
  接下来, 根节点被声明， 它需要一个或多个孩子节点'book'。 
  每个 'book' 应该准确包含一个 'title' 和 'price' 和
  一个被叫做'category'的缺省值为"Literature"的属性。
  这个'title' 和 'price'节点包含一个解析过的字符数据。-->

<!-- 这个DTD可以在XML文件中本身被声明。-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>

<bookstore>
  <book category="COOKING">
    <title >Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>
```