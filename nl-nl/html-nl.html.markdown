---
language: html
filename: learnhtml-nl.html
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Jeroen Deviaene", "https://github.com/jerodev"]
lang: nl-nl
---

HTML staat voor HyperText Markup Language.
Het is een taal die je toe staat pagina's te maken op het World Wide Web.
Het is een opmaak taal, dit staat de gebruiker toe om webpagina's te maken in code en zo aan te duiden hoe de pagina moet weergegeven worden.
Eigenlijk zijn html files zelfs simpele tekst bestanden.
Wat is deze opmaak? Het is een manier om de structuur en data op de pagina weer te geven met speciale html tags.
Deze tags dienen om de betekenis te geven aan de tekst die het bevat.
Zoals vele computer talen heeft html vele verschillende versies. Hier zullen we HTML5 bespreken.

**Merk op:** Je kan de verschillende tags en elementen testen terwijl je door de tutorial gaat met een website zoals [codepen](http://codepen.io/pen/), zo kan je de effecten hier van live zien.
Dit artikel gaat vooral over de HTML syntax en enkele handige tips


```html
<!-- Commentaren worden toegevoegd zoals deze lijn -->

<!-- #################### De Tags #################### -->

<!-- Hier is een voorbeeld HTML bestand dat we zullen analyseren. -->

<!doctype html>
	<html>
		<head>
			<title>Mijn Website</title>
		</head>
		<body>
			<h1>Hello, world!</h1>
			<a href="http://codepen.io/anon/pen/xwjLbZ">Neem een kijkje op deze link</a>
			<p>Dit is een paragraaf.</p>
			<p>Dit is nog een paragraaf.</p>
			<ul>
				<li>Dit is een item in een niet genummerde lijst</li>
				<li>Dit is nog zo een item</li>
				<li>En dit is het laatste item van de lijst</li>
			</ul>
		</body>
	</html>

<!-- Een HTML bestand start altijd met een tag die aan de browser laat weten dat we HTML gebruiken -->
<!doctype html>

<!-- Daarna openen we de root van het bestand met de <html> tag -->
<html>

<!-- Deze tag moet ook gesloten worden op het einde van het bestand -->
</html>

<!-- Niets mag nog na deze tag komen! -->

<!-- Binnenin (tussen de html tags <html></html>) vinden we: -->

<!-- Een header, gedefigneerd door <head> (Deze moet gesloten worden met </head>) -->
<!-- De header bevat beschrijvingen en externe data die niet zichtbaar is op de website; Dit is metadata -->

<head>
	<title>Mijn Website</title><!-- De <title> tag geeft de tekst aan die in de titelbar van de browser moet weergegeven worden. -->
</head>

<!-- Achter de <head> sectie vinden we bijna altijd <body> -->
<!-- Tot op dit punt is nog niets verschenen in het browser venster. -->
<!-- In de body plaatsen we wat zichtbaar moet zijn in de browser -->

<body>
	<h1>Hello, world!</h1> <!-- De h1 tag maakt een titel. -->
	<!-- Er zijn ook sub titels voor <h1> van belangrijk <h2> tot minder belangrijk <h6>. -->
	<a href="http://codepen.io/anon/pen/xwjLbZ">Neem een kijkje op deze link</a> <!-- een hyperlink naar de aangegeven link waar op geklikt kan worden in de browser -->
	<p>This is a paragraph.</p> <!-- De tag <p> laat ons tekst toevoegen. -->
	<p>This is another paragraph.</p>
	<ul> <!-- De tag <ul> maakt een lijst met puntjes. -->
	<!-- Om een genummerde lijst te hebben gebruik je <ol>, hiermee worden de elementen <li> automatisch genummerd -->
		<li>This is an item in a non-enumerated list (bullet list)</li>
		<li>This is another item</li>
		<li>And this is the last item on the list</li>
	</ul>
</body>

<!-- En dat is het! Zo gemakkelijk is het om een html bestand te maken. -->
```

## Gebruik

HTML wordt altijd opgeslagen in bestanden die eindigen in `.html`.

## Meer weten

* [wikipedia](https://nl.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
