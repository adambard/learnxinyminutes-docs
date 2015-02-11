---
language: xml
filename: learnxml-ru.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Dmitry Bessonov", "https://github.com/TheDmitry"]
lang: ru-ru
---

XML - это язык разметки, предназначеный для хранения и передачи данных.

В отличие от HTML, XML не определяет, как отображать или формировать данные, он только содержит их.

* XML Синтаксис

```xml
<!-- Комментраии в XML выглядят так -->

<?xml version="1.0" encoding="UTF-8"?>
<bookstore>
  <book category="КУЛИНАРИЯ">
    <title lang="en">Everyday Italian</title>
    <author>Giada De Laurentiis</author>
    <year>2005</year>
    <price>30.00</price>
  </book>
  <book category="ДЕТИ">
    <title lang="ru">Гарри Поттер</title>
    <author>Дж. К. Роулинг</author>
    <year>2005</year>
    <price>29.99</price>
  </book>
  <book category="ВСЕМИРНАЯ ПАУТИНА">
    <title lang="ru">Изучаем XML</title>
    <author>Эрик Рэй</author>
    <year>2003</year>
    <price>39.95</price>
  </book>
</bookstore>

<!-- Вышеописанный документ - типичный XML-файл.
  Он начинается с определения, информирующее о некоторых метаданных (необязательно).
  
  XML использует древовидную структуру. У вишеописанного документа корневой узел - 'bookstore', который содержит
  три дочерних узла - все 'books'. Такие узлы содержат много дочерних узлов и т.д. 
  
  Узлы создаются с помощью открывающего/закрывающего тегов, а дочерние - это узлы между
  открывающимися и закрывающимися тегами.-->


<!-- XML содержит в себе два типа данных:
  1 - Атрибуты -> Это метаданные узлов.
      Обычно, XML-парсер использует эту информацию, чтобы хранить свойства данных.
      Атрибут изображается путем вписывания его в скобки в пределах открывающегося тега
  2 - Элементы -> Это чистые данные.
      Это то, что парсер извлечет из XML-файла.
      Элементы идут между открывающим и закрывающим тегами без скобок. -->
      
  
<!-- Внизу элемент с двумя атрибутами -->
<file type="gif" id="4293">компьютер.gif</file>


```

* Well-Formated Document x Validation

A XML document is well-formated if it is syntactically correct.
However, it is possible to inject more constraints in the document,
using document definitions, such as DTD and  XML Schema.

A XML document which follows a document definition is called valid, 
regarding that document. 

With this tool, you can check the XML data outside the application logic.

```xml

<!-- Below, you can see an simplified version of bookstore document, 
  with the addition of DTD definition.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Bookstore.dtd">
<bookstore>
  <book category="COOKING">
    <title >Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>

<!-- This DTD could be something like:-->

<!DOCTYPE note
[
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>


<!-- The DTD starts with a declaration.
  Following, the root node is declared, requiring 1 or more child nodes 'book'.
  Each 'book' should contain exactly one 'title' and 'price' and an attribute
  called 'category', with "Literature" as its default value.
  The 'title' and 'price' nodes contain a parsed character data.-->

<!-- The DTD could be declared inside the XML file itself.-->

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
