---
language: xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Kevin Peters", "https://github.com/igeligel"]
filename: xml-de.html.markdown
lang: de-de
---

XML ist eine Auszeichnungssprache, die dafür kreiert wurde, um Daten zu speichern und transportieren.

Im Gegensatz zu HTML, spezifiert XML nicht, wie Daten dargestellt oder formatiert werden soll, sondern beinhaltet nur die Daten.

* XML Syntax

```xml
<!-- Comments in XML are like this -->

<?xml version="1.0" encoding="UTF-8"?>
<buecherei>
  <buch kategorie="KOCHEN">
    <titel sprache="en">Everyday Italian</titel>
    <autor>Giada De Laurentiis</autor>
    <jahr>2005</jahr>
    <preis>30.00</preis>
  </buch>
  <buch kategorie="KINDER">
    <titel sprache="en">Harry Potter</titel>
    <autor>J K. Rowling</autor>
    <jahr>2005</jahr>
    <preis>29.99</preis>
  </buch>
  <buch kategorie="WEB">
    <titel sprache="en">Learning XML</titel>
    <autor>Erik T. Ray</autor>
    <jahr>2003</jahr>
    <preis>39.95</preis>
  </buch>
</buecherei>

<!-- Über diesem Kommentar ist eine typische XML-Datei.
  Sie startet mit einer Deklaration, welche (optional) über Metadaten informiert.

  XML benutzt eine Baumstruktur. Der Wurzelknoten ist "buecherei", welcher drei
  Kinderknoten besitzt (Alle "buecher"). Diese Knoten haben weitere Kinderknoten, 
  die jeweils weiter Kinderknoten besitzen. Dies führt sich so weiter...

  Knoten werden durch Kennzeichnungen eröffnet (Beispiel: <...>, 
  bzw. abgeschlossen (Beispiel: </...>). 
  Kinderknoten sind zwischen diesen Kennzeichnungen. -->


<!-- XML beinhalten zwei Arten von Daten:
  1 - Attribute -> Dies sind Metadaten des Knotens.
  	  Normalerweise benutzt der XML Parser diese Informationen, um Daten ordentlich zu speichern.
  	  Dies wird mit dem Format name="wert" in der eröffnenden Kennzeichnung festgelegt.
  2 - Elemente -> Das sind echte Daten.
  	  Dies sind Daten, die der Parser von der XML-Datei erhält.
  	  Elemente erscheinen zwischen den eröffnenden und schließenden Kennzeichnungen.
-->


<!-- Unter diesem Kommentar befindet sich ein Element mit zwei Attributen -->
<datei typ="gif" id="4293">computer.gif</datei>

```

* Gut formatiertes Dokument x Validierung

Ein XML-Dokument ist gut formatiert, wenn es syntaktisch korrekt ist.
Es ist möglich mehr Nebenbedingungen in das Dokument zu bringen, indem
man Dokumentdefinitionen, wie DTD oder das XML-Schema, benutzt.

Ein XML-Dokument, welches die Dokumentdefinion verfolgt, ist zulässig für 
die Dokumentendefinition.

Mit diesem Werkzeug, kannst du die XML-Daten, auch außerhalb der Programm 
Logik, überprüfen.

```xml

<!-- Unter diesem Kommentar, kann man eine vereinfachte Fassung des Buechereidokuments
  mit hinzugefügten DTD-Definitionen sehen. -->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Buecherei.dtd">
<buecherei>
  <buch kategorie="KOCHEN">
    <titel>Everyday Italian</titel>>
    <preis>30.00</preis>>
  </buch>>
</buecherei>>

<!-- Dieses DTD kann etwas sein, wie: -->

<!DOCTYPE note
[
<!ELEMENT buecherei (buch+)>
<!ELEMENT buch (titel,preis)>
<!ATTLIST buch kategorie CDATA "Literatur">
<!ELEMENT titel (#PCDATA)>
<!ELEMENT preis (#PCDATA)>
]>


<!-- Das DTD startet mit einer Deklaration.
  Vorrausgesetzt der Wurzelknoten ist deklariert, benötigt er 1 oder mehr 
  Kinderknoten des Typs "buch". Jedes "buch" muss genau einen "titel" und 
  "preis" und ein Attribut "kategorie" enthalten, welches den Standardwert 
  "Literatur" besitzt.
  Die "Titel"- und "Preis"-Knoten besitzen jeweils analysierte Zeichenfolgen. -->

<!-- Das DTD kann auch innerhalb der XML-Datei deklariert werden.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT buecherei (buch+)>
<!ELEMENT buch (titel,preis)>
<!ATTLIST buch kategorie CDATA "Literatur">
<!ELEMENT titel (#PCDATA)>
<!ELEMENT preis (#PCDATA)>
]>

<buecherei>
  <buch kategorie="KOCHEN">
    <titel>Everyday Italian</titel>>
    <preis>30.00</preis>>
  </buch>>
</buecherei>>
```
