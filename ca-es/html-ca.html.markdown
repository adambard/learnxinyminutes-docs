---
language: html
filename: html-ca.md
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Marc Auledas", "https://github.com/Auledas"]
lang: ca-es
---

HTML significa llenguatge de marques d'hipertext (HyperText Markup Language).

És un llenguatge que permet escriure pàgines pel sistema web (World Wide Web).
És un llenguatge de marques que permet escriure pàgines web fent servir codi per indicar
com s'ha de visualitzar el text i les dades. De fet, els fitxers html són simples
fitxers de text.

Què són les 'marques'? Són un mètode d'organitzar les dades d'una pàgina mitjançant
etiquetes d'obertura i tancament. Aquestes marques serveixen per donar
significat al text que hi ha entre les etiquetes. Com d'altres llenguatges de marques,
hi ha moltes versions de l'HTML. Aquí es desenvoluparà l'HTML5.

**NOTA:** Pots provar les diferents etiquetes i elements a mesura que progressis a través
del tutorial a pàgines web com [codepen](http://codepen.io/pen/). D'aquesta manera podràs
veure'n els efectes, entendre com funcionen i familiaritzar-te amb el llenguatge. Aquest
article tracta principalment la sintaxi de l'HTML i alguns consells útils.


```html
<!-- Els comentaris s'escriuen com aquesta línia! -->

<!--
	Els comentaris
	poden
	ocupar
	múltiples
	línies!
-->

<!-- #################### Les etiquetes #################### -->

<!-- Aquí hi ha un exemple de fitxer HTML que analitzarem. -->


<!doctype html>
	<html>
		<head>
			<title>El meu lloc web</title>
		</head>
		<body>
			<h1>Hola, món!</h1>
			<a href="http://codepen.io/anon/pen/xwjLbZ">
				Fes una ullada a com es veu això.
			</a>
			<p>Això és un paràgraf.</p>
			<p>Això és un altre paràgraf.</p>
			<ul>
				<li>Això és un element d'una llista no enumerada (llista de punts).</li>
				<li>Això és un altre element.</li>
				<li>I aquest és l'últim element de la llista.</li>
			</ul>
		</body>
	</html>

<!--
	Un fitxer HTML sempre comença indicant al navegador que la pàgina és HTML.
-->
<!doctype html>

<!-- Després d'això, es comença obrint l'etiqueta <html>. -->
<html>

<!-- Aquesta etiqueta s'ha de tancar al final del fitxer amb </html>. -->
</html>

<!-- No s'ha d'escriure res més després d'aquesta etiqueta final. -->

<!-- Entremig (entre les etiquetes d'obertura i tancament <html></html>), trobem: -->

<!-- Una capçalera definida per <head> (que s'ha de tancar emprant </head>). -->
<!--
	La capçalera conté descripcions i informació addicional que no es mostra,
        això són les metadades.
-->

<head>
	<!--
		L'etiqueta <title> indica al navegador el títol que s'ha de visualitzar 
                a la finestra del buscador, a la barra del títol, i al nom de la pestanya.
	-->
	<title>El meu lloc web</title>
</head>

<!-- Després de la secció <head>, trobem l'etiqueta <body> -->
<!-- Fins a aquest punt, res del que s'ha fet apareixerà a la finestra del navegador. -->
<!-- Hem d'emplenar el cos amb el contingut per visualitzar. -->

<body>
	<!-- L'etiqueta h1 crea el títol. -->
	<h1>Hola, món!</h1>
	<!--
		També es poden fer subtítols per <h1>, que van des del més important <h2>
                fins al més precís <h6>.
	-->

	<!-- També es poden crear enllaços fent servir l'atribut href="" -->
	<a href="http://codepen.io/anon/pen/xwjLbZ">
		Fes una ullada a com es veu això.
	</a>

	<!-- L'etiqueta <p> permet incloure text a una pàgina HTML. -->
	<p>Això és un paràgraf.</p>
	<p>Això és un altre paràgraf.</p>

	<!-- L'etiqueta <ul> crea una llista de punts. -->
	<!--
		Per tenir una llista enumerada s'hauria de fer servir <ol>, en comptes d'<ul>.
                El primer element seria 1, el segon element 2, etc.
	-->
	<ul>
		<li>Això és un element d'una llista no enumerada (llista de punts).</li>
		<li>Això és un altre element.</li>
		<li>I aquest és l'últim element de la llista.</li>
	</ul>
</body>

<!-- I això és tot, crear un fitxer HTML pot ser molt simple. -->

<!-- Però és possible afegir molts altres tipus d'etiquetes HTML addicionals. -->

<!-- L'etiqueta <img /> es fa servir per afegir imatges. -->
<!--
	L'origen de la imatge s'indica fent servir l'atribut src=""
	L'origen pot ser una adreça URL o la ruta a un arxiu del teu ordinador.
-->
<img src="http://i.imgur.com/XWG0O.gif"/>

<!-- També es poden crear taules. -->

<!-- L'etiqueta <table> obre la taula. -->
<table>

	<!-- <tr> permet crear files. -->
	<tr>

	<!-- <th> permet posar títol a la columna d'una taula. -->
		<th>Primera capçalera</th>
		<th>Segona capçalera</th>
	</tr>

	<tr>

	<!-- <td> permet crear cel·les d'una taula. -->
		<td>Primera fila, primera columna</td>
		<td>Primera fila, segona columna</td>
	</tr>

	<tr>
		<td>Segona fila, primera columna</td>
		<td>Segona fila, segona columna</td>
	</tr>
</table>

```

## Ús

Els arxius HTML acaben amb les extensions `.html` o `.htm`. El tipus MIME és `text/html`.

**HTML NO és un llenguatge de programació**

## Per aprendre'n més

* [Viquipèdia](https://ca.wikipedia.org/wiki/Hyper_Text_Markup_Language)
* [Tutorial HTML](https://developer.mozilla.org/ca/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
