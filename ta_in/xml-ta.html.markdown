---
language: xml
filename: learnxml-ta.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Rasendran Kirushan", "https://github.com/kirushanr"]
lang: in-ta
---


XML ஆனது ஒரு கட்டமைப்பு மொழி ஆகும் இது தகவலை சேமிக்கவும்
தகவலை பரிமாறவும் உருவாக்கபட்டுள்ளது


HTML போல் அன்றி , XML ஆனது தகவலை மட்டும் கொண்டு செல்ல்கிறது 
* XML வாக்கிய அமைப்பு


```xml
<!-- இது ஒரு XML குறிப்பு -->

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


  <!--
  
	மேல காட்டப்பட்டுள்ளது ஒரு xml file இன் உதாரணம் ஆகும்
	அது metadata உடன் ஆரம்பமாகிறது
	XML  ஆனது ஒரு மரத்தை போன்ற கட்டமைப்பை ஒத்தது. 
	இங்கு root node (கொப்பு)  `bookstore`  இது மூன்று கிளைகள்  (child nodes)
	கொண்டுள்ளது. இந்த கிளைகள் மேலும் சில கிளைகளை கொண்டு இருக்கலாம்
	ஒவொரு node கட்டமைப்பும்  ஒரு `<` ஆரம்பாமாகி `>` முடிவடையும்
	கிளைகள் இந்த கட்டமைப்புக்கு இடையில் நிறுவப்படும்
  -->


<!--
XML இரண்டு வகையான தகவல்களை கொண்டு செல்லக்கூடியது
1- Attributes -> ஒரு  கணு(node) பற்றிய metadata 
பொதுவாக   XML Parser இந்த தகவலை பயன்படுத்தியே தகவலை
சரியான முறையில் சேமிக்க.
இது xml கட்டமைப்பின் ஆரம்பத்தில் உள்ள name="value"
தீர்மானிக்கபடுகிறது.

2-Elements ->இவற்றில் முற்றிலும் தகவல்களே சேமிக்கபட்டு இருக்கும்
Elements  ஒரு `<` ஆரம்பாமாகி `>` முடிவடையும் காணப்படும்


-->

<!-- கிழே உள்ள element இரண்டு பெறுமானங்களை கொண்டுள்ளது  -->
<file type="gif" id="4293">computer.gif</file>


```

* சரியான முறையில் ஒழுகுபடுத்தபட்ட X document


ஒரு XML document ஆனது சரியான முறையில் எழுத பட்டிருப்பின் மட்டுமே அது 
சிறந்த வகையில்  வடிவமைக்கபட்டுள்ளது,எனினும் மேலும் பல கட்டுபாடுகளை
நாம் ஒரு xml document உக்கு இட முடியும் உ.ம்:-DTD மற்றும்  XML Schema.


ஒரு xml document ஆனது ஒரு வரையறுக்கபட்டிருப்பின் மட்டுமே 
அது சரி என கொள்ளப்படும்


With this tool, you can check the XML data outside the application logic.
இந்த கருவியை உபயோகித்து xml தகவல்களை சோதிக்க முடியும் 

```xml

<!-- கீழே bookstore html document இன் எளிமையான வடிவம் 
    DTD வரையறைகளுடன்
-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Bookstore.dtd">
<bookstore>
  <book category="COOKING">
    <title >Everyday Italian</title>
    <price>30.00</price>
  </book>
</bookstore>

<!-- DTD ஆனது பின்வருமாறு  அமையும் :-->

<!DOCTYPE note
[
<!ELEMENT bookstore (book+)>
<!ELEMENT book (title,price)>
<!ATTLIST book category CDATA "Literature">
<!ELEMENT title (#PCDATA)>
<!ELEMENT price (#PCDATA)>
]>


<!-- DTD ஆனது root node ஐ உருவாக்கிய பின் நிறுவ படுகிறது ,இது ஒன்று அல்லது 
ஒன்றிக்கு மேற்பட்ட child node களை எதிர்பார்க்கிறது.
 ஒவ்வொரு 'book' உம் கட்டாயமாக ஒரு 'title' , 'price','category', with "Literature"
 ஆகிய பெறுமானங்களை கொண்டிருத்தல் அவசியம்.
--> 

<!-- DTD ஆனது xml file ஒன்றினுள் உருவாக்கபடுகிறது-->

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
