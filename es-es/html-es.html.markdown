---
language: html
filename: learnhtml-es.html
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
lang: es-es
---

HTML significa Lenguaje de marcado de hipertexto (HyperText Markup Language).
Este es un lenguaje usado para escribir páginas en la web (WWW). 
Este es un lenguaje de marcado, es usado para escribir páginas web usando código para indicar cómo se debe mostrar el texto y los datos.
En efecto, los archivos html son simples archivos de texto.
Qué es esto de marcado? es un método para organizar los datos de la página encerrandolos con etiquetas de apertura y cierre.
Este marcado sirve para darle significancia al texto que éste encierra.
Como en otros lenguajes computacionales, HTML tiene varias versiones. Aquí hablaremos acerca de HTML5.

**Nota :**  Puedes probrar las diferentes etiquetas y elementos a medida que progresas en un tutorial en un sitio como  [codepen](http://codepen.io/pen/) con el fin de ver sus efectos, entender como funcionan y familiarizarse con el lenguaje.
Este artículo está centrado principalmente en la sintaxis HTML y algunos tips de importancia.


```html
<!-- los comentarios están encerrados como en esta línea! -->

<!-- #################### Las Etiquetas #################### -->
   
<!-- Este es un ejemplo de un archivo HTML que analizaremos! -->

<!doctype html>
	<html>
		<head>
			<title>Mi Sitio</title>
		</head>
		<body>
			<h1>Hola, Mundo!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">ven mira lo que esto muestra. </a>
			<p>Esto es un párrafo</p>
			<p>Este es otro párrafo</p>
			<ul>
				<li>Este es un elemento de una lista no numerada (lista de viñetas)</li>
				<li>Este es otro ítem</li>
				<li>Y este es el último ítem de la lista</li>
			</ul>
		</body>
	</html>

<!-- En un archivo HTML siempre inicia indicando le al buscador que esta es una página HTML. -->
<!doctype html>

<!-- Después de esto, iniciamos abriendo una etiqueta html <html> -->
<html>

<!-- Cuando termine el archivo cerraremos la etiqueta así </html>. -->
</html>

<!-- Después de la etiqueta final nada aparecerá o podrá aparecer -->

<!-- Dentro (Entre las etiquetas de apertura y cierre <html></html>), encontraremos: -->

<!-- Un encabezado definido por <head> (Este debe ser cerrado por </head>). -->

<!-- El encabezado contiene alguna descripción y información adicional que no se muestra; estos son los metadatos. -->

<head>
	<title>Mi Sitio</title><!-- La etiqueta <title> Indica al buscador el título a mostrar en la ventana del buscador en la barra de título y en el nombre de la pestaña. -->
</head>

<!-- Después de la sección del encabezado <head> , Encontraremos la etiqueta de cuerpo - <body> -->
<!-- Hasta este punto. no hay nada descrito para que se muestre en la ventana del navegador -->
<!-- Debemos llenar el cuerpo con el contenido que se mostrará -->

<body>
	<h1>Hola, Mundo!</h1> <!-- La etiqueta <h1> crea un título. -->
	<!-- También tenemos subtítulos para <h1> desde la más importante <h2> a la más precisa <h6> -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">ven mira lo que esto muestra.</a> <!-- Un hipervínculo a la URL dada por el atributo href="" -->
	<p>Esto es un párrafo.</p> <!-- La etiqueta <p> nos permite incluir texto en nuestra página HTML -->
	<p>Este es otro párrafo.</p>
	<ul> <!-- La etiqueta <ul> crea una lista de viñetas -->
	<!-- Para tener una lista numerada usamos la etiqueta <ol> dando 1. para el primer elemento, 2. para el segundo, etc. -->
		<li>Este es un elemento de una lista no numerada (lista de viñetas)</li>
		<li>Este es otro ítem</li>
		<li>Y este es el último ítem de la lista</li>
	</ul>
</body>

<!-- Y esto es todo, la creación de un archivo HTML puede ser muy simple. -->

<!-- Sin embargo, es posible añadir muchos otros tipos de etiquetas HTML  -->

<!-- Para insertar una imagen -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- La fuente donde se localiza la imagen se indica utilizando el atributo src=""-->
<!-- La fuente puede ser una URL o incluso una ruta a una archivo en tu computador. -->

<!-- También es posible crear una tabla -->

<table> <!-- Abrimos una etiqueta o elemento tabla <table> -->
	<tr> <!-- <tr> Nos permite crear una fila. -->
		<th>Primer encabezado</th> <!-- <th> Nos permite dar un título a una columna de una tabla -->
		<th>Segundo encabezado</th>
	</tr>
	<tr>
		<td>Primera fila, primera columna</td> <!-- <td> nos permite crear una celda  -->
		<td>Primera fila, segunda columna</td>
	</tr>
	<tr>
		<td>Segunda fila, primera columna</td>
		<td>Segunda fila, segunda columna</td>
	</tr>
</table>

```

## Uso

HTML es escrito en archivos que terminan con (extensión) `.html`.

## Para aprender más! 

* [wikipedia](https://es.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/es/docs/Web/HTML)
* [W3School (EN)](http://www.w3schools.com/html/html_intro.asp)
