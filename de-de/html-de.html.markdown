---
language: html
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
filename: learnhtml-de.html
lang: de-de
---

HTML steht für HyperText Markup Language (Hypertext-Auszeichnungssprache).
Sie ist eine Sprache, um Seiten für das World Wide Web zu schreiben..
Es ist eine Auszeichnugssprache, die es uns ermöglicht Webseiten mithilfe des Codes zu schreiben, der kennzeichnet  wie Text und Daten angezeigt werden sollen. Eigentlich sind HTML Dateien nur einfache Textdateien.
Was sind das für Auszeichnungen? Es ist eine Methode, um die Daten der Website zu organisieren mithilfe von Start- und Endtags.
Diese Auszeichnung dient dazu dem Text Bedeutung zu geben, welchen sie umschließt.
Wie viele andere Computersprachen auch, besitzt HTML viele Versionen. Wir werden hier über HTML5 reden.

**NOTE :**  Du kannst die unterschiedlichen Tags und Elemente, während des Tutorials auf Seiten, wie [codepen](http://codepen.io/pen/) testen, um deren Effekte zu sehen und wie diese funktionieren. Auch kannst du dich damit besser mit der Sprache vertraut machen.
Dieser Artikel ist bedacht darauf, nur HTML Syntax und nützliche Tipps zu geben.


```html
<!-- Kommentare werden wie in dieser Zeile geschrieben -->

<!-- #################### Die Tags #################### -->

<!-- Hier ist eine Beispiel HTML Datei, die wir analysieren werden -->

<!doctype html>
	<html>
		<head>
			<title>Meine Website</title>
		</head>
		<body>
			<h1>Hallo Welt!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">Komm schau was das hier anzeigt</a>
			<p>Das ist ein Paragraf.</p>
			<p>Das ist ein weiterer Paragraf.</p>
			<ul>
				<li>Das ist eine Item mit einer nicht-nummerierten Liste (Aufzählungsliste)</li>
				<li>Das ist ein weiteres Item</li>
				<li>Und das ist das letzte Item in der Liste</li>
			</ul>
		</body>
	</html>

<!-- Jede HTML Datei startet damit dem Browser zu sagen, dass die Seite aus HTML besteht. -->
<!doctype html>

<!-- Danach startet sie mit einem Öffnungtag <html>. -->
<html>

<!-- Dieser wird am Ende der Datei mit</html> geschlossen. -->
</html>

<!-- Nichts sollte nach diesen finalen Tag erscheinen. -->

<!-- Dazwischen (Zwischen dem Öffnungs- und Schließungstag <html></html>) finden wir: -->

<!-- Ein Kopf wird definiert mit <head> (er muss mit </head> geschlossen werden). -->
<!-- Der Header beinhaltet Beschreibungen und zusätzliche Informationen, welche nicht dargestellt werden. Das sind Metadaten. -->

<head>
	<title>Meine Seite</title><!-- Der <title> kennzeichnet dem Browser den Titel im Browserfenster und im Tabnamen anzuzeigen. -->
</head>

<!-- Nach dem <head> Bereich findet sich der <body> Tag -->
<!-- Bis zu diesen Punkt wird nichts im Browerfenster angezeigt. -->
<!-- Wir müssen den Body mit dem Inhalt füllen der angezeigt werden soll. -->

<body>
	<h1>Hallo, Welt!</h1> <!-- Der h1 Tag erstellt einen Titel. -->
	<!-- Es gibt auch Untertitel für <h1> von den wichtigsten <h2> zu den Unwichtigsten (h6). -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">Komm, schaue was das zeigt</a> <!-- Eine URL wird zum Hyperlink, wenn es das Attribut href=""  -->
	<p>Das ist ein Absatz.</p> <!-- Der Tag <p> lässt uns Text auf die HTML Seite hinzufügen. -->
	<p>Das ist ein anderer Absatz.</p>
	<ul> <!-- Der <ul> Tag erstellt eine Aufzählungsliste. -->
	<!-- Für eine nummerierte Liste sollten wir stattdessen <ol> verwenden. Das erste Element bekommt 1., das zweite 2. usw. -->
		<li>Das ist ein Element in einer nicht Aufzählungsliste</li>
		<li>Das ist ein anderes Item</li>
		<li>Und das ist das letzte Element in der List</li>
	</ul>
</body>

<!-- Und das war es. Eine HTML Datei kann so simpel sein. -->

<!-- Aber es ist möglich viele weitere zusätzliche HTML tags hinzuzufügen. -->

<!-- Um ein Bild hinzuzufügen. -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- Die Quelle des Bildes wird gezeigt durch das Attribut src="" -->
<!-- Die Quelle kann eine URL sein oder ein Pfad zu deinem Computer. -->

<!-- Es ist ebenso möglich eine Tabelle zu erstellen. -->

<table> <!-- Wir öffnen ein <table> Element. -->
	<tr> <!-- <tr> erlaubt es uns Reihe zu erstellen. -->
		<th>Erster Tabellenkopf</th> <!-- <th> erlaubt es uns der Tabelle einen Titel zu geben. -->
		<th>Zweiter Tabllenkopf</th>
	</tr>
	<tr>
		<td>Erste Zeile, erste Spalte</td> <!-- <td> erlaubt es eine Tabellenzelle zu erstellen. -->
		<td>Erste Zeile, zweite Spalte</td>
	</tr>
	<tr>
		<td>Zweite Zeile, erste Spalte</td>
		<td>Zweite Zeile, zweite Spalte</td>
	</tr>
</table>

```

## Verwendung

HTML Dateien enden mit `.html`.

## Um mehr zu lernen

* [wikipedia (EN)](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial (EN)](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School (EN)](http://www.w3schools.com/html/html_intro.asp)
