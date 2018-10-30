---
language: html
filename: learnhtml-it.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Ale46", "http://github.com/Ale46/"]
lang: it-it
---

HTML sta per HyperText Markup Language (linguaggio a marcatori per ipertesti).
È un linguaggio che consente di scrivere pagine web per il world wide web.
È un linguaggio di markup, che permette di scrivere pagine web usando del codice che indica come il testo ed i dati devono essere mostrati.
Infatti, i files html sono semplici file di testo.
Cos'è il markup? È un metodo per organizzare i dati della pagina circondandoli con tag di apertura e tag di chiusura.
Questo markup serve a dare significato al testo che racchiude.
Come altri linguaggi di programmazione, HTML ha molte versioni. Qui discuteremo di HTML5.

**NOTA :**  Puoi testare i differenti tags ed elementi man mano che prosegui nel tutorial in un sito come [codepen](http://codepen.io/pen/) per vedere i loro effetti, capire come lavorano e familiarizzare con il linguaggio.
Questo articolo riguarda principalmente la sintassi HTML ed alcuni suggerimenti utili.


```html
<!-- I commenti sono racchiusi come in questa riga! -->

<!-- #################### I Tags #################### -->

<!-- Ecco un esempio di file HTML che andremo ad analizzare. -->

<!doctype html>
	<html>
		<head>
			<title>Il mio sito</title>
		</head>
		<body>
			<h1>Ciao, mondo!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">Vieni a vedere ciò che mostra</a>
			<p>Questo è un paragrafo.</p>
			<p>Questo è un altro paragrafo.</p>
			<ul>
				<li>Questo è un elemento di un elenco non numerato (elenco puntato)</li>
				<li>Questo è un altro elemento</li>
				<li>E questo è l'ultimo elemento dell'elenco</li>
			</ul>
		</body>
	</html>

<!-- Un file HTML inizia sempre indicando al browser che la pagina è HTML. -->
<!doctype html>

<!-- Dopo questo, inizia aprendo un tag <html>. -->
<html>

<!-- che sarà chiuso alla fine del file con </html>. -->
</html>

<!-- Nulla dovrebbe apparire dopo questo tag finale. -->

<!-- All'interno (tra i tag di apertura e chiusura <html> </html>) troviamo: -->

<!-- Un'intestazione definita da <head> (deve essere chiusa con </head>). -->
<!-- L'intestazione contiene alcune descrizioni e informazioni aggiuntive non visualizzate; questi sono i metadati. -->

<head>
	<title>Il mio sito</title> <!-- Il tag <title> indica al browser il titolo da mostrare nella barra del titolo della finestra del browser e nel nome della scheda. -->
</head>

<!-- Dopo la sezione <head>, troviamo il tag - <body> -->
<!-- Fino a questo punto, niente di ciò che abbiamo descritto verrà visualizzato nella finestra del browser. -->
<!-- Dobbiamo riempire il corpo con il contenuto da visualizzare. -->

<body>
	<h1>Ciao, mondo!</h1> <!-- Il tag h1 crea un titolo. -->
	<!-- Ci sono anche sottotitoli a <h1> dal più importante (h2) al più preciso (h6). -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">Vieni a vedere ciò che mostra</a> <!-- un collegamento ipertestuale all'URL fornito dall'attributo href="" -->
	<p>Questo è un paragrafo.</p> <!-- Il tag <p> ci permette di includere del testo nella pagina html. -->
	<p>Questo è un altro paragrafo.</p>
	<ul> <!-- Il tag <ul> crea un elenco puntato. -->
	<!-- Per avere un elenco numerato, invece, usiamo <ol> che restituisce 1. per il primo elemento, 2. per il secondo, etc. -->
		<li>Questo è un elemento in un elenco non elencato (elenco puntato)</li>
		<li>Questo è un altro elemento</li>
		<li>E questo è l'ultimo elemento dell'elenco</li>
	</ul>
</body>

<!-- E questo è tutto, creare un file HTML può essere molto semplice. -->

<!-- Ma è possibile aggiungere molti altri tipi di tag HTML. -->

<!-- Per inserire un'immagine. -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- La fonte dell'immagine viene indicata usando l'attributo src="" -->
<!-- La fonte può essere un URL o persino un percorso di un file sul tuo computer. -->

<!-- È anche possibile creare una tabella. -->

<table> <!-- Apriamo un elemento <table>. -->
	<tr> <!-- <tr> ci permette di creare una riga. -->
		<th>Prima intestazione</th> <!-- <th> ci permette di dare un titolo ad una colonna della tabella. -->
		<th>Seconda intestazione</th>
	</tr>
	<tr>
		<td>prima riga, prima colonna</td> <!-- <td> ci permette di creare una cella della tabella. -->
		<td>prima riga, seconda colonna</td>
	</tr>
	<tr>
		<td>seconda riga, prima colonna</td>
		<td>seconda riga, seconda colonna</td>
	</tr>
</table>

```

## Uso

HTML è scritto in files che finiscono con `.html` o `.htm`. Il "MIME type" è `text/html`.

## Per saperne di più

* [wikipedia](https://it.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/it/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
