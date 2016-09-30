---
lenguaje: html
nombre del archivo: learnhtml.html
contribuyentes:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
traductor:
    - ["Gino Amaury", "https://github.com/ginoamaury"]
---

HTML significa Lenguaje de marcado de hipertexto (HyperText Markup Language).
Este es un lenguaje usado para escribir paginas en la web (WWW). 
Este es un lenguaje de marcado, es usado para escribir paginas web usando codigo para indicar como se debe mostrar el texto y los datos.
En efecto, los archivos html son simples archivos de texto.
Que es esto de marcado? es un metodo para organizar los datos de la pagina encerrandolos con etiquetas de apertura y cierre.
Este marcado sirve para darle significancia al texto que este encierra.
Como en otros lenguajes computacionales, HTML tiene varias verciones. Aqui hablaremos acerca de HTML5.

**Nota :**  Puedes probrar las diferentes etiquetas y elementos a medida que progresas en un tutorial en un sitio como  [codepen](http://codepen.io/pen/) con el fin de ver sus efectos, entender como funcionan y familiarizarse con el lenguaje.
Este articulo es ta centrado principalmente en la sintaxis HTML y algunos tips de importancia.


```html
<!-- los comentarios estan encerrados como en esta linea! -->

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
			<p>Esto es un parrafo</p>
			<p>Este es otro parrafo</p>
			<ul>
				<li>Este es un elemento de una lista no numerada (lista de viñetas)</li>
				<li>Este es otro item</li>
				<li>Y este es el ultimo item de la lista</li>
			</ul>
		</body>
	</html>

<!-- En un archivo HTML siempre inicia indicandole al buscador que esta es una pagina HTML. -->
<!doctype html>

<!-- Despues de esto, iniciams abriendo una etiqueta html <html> -->
<html>

<!-- Cuando termine el archivo cerraremos la etiqueta asi </html>. -->
</html>

<!-- despues de la etiqueta final nada aparecera o podra aparecer -->

<!-- Dentro (Entre las etiquetas de apertura y cierre <html></html>), encontraremos: -->

<!-- Un encabezado definido por <head> (Este debe ser cerrado por </head>). -->

<!-- El encabezado contiene alguna descripcion y informacion adicional que no se muestra; estos son los metadatos. -->

<head>
	<title>Mi Sitio</title><!-- La etiqueta <title> Indica al buscador el titulo a mostrar en la ventana del buscador en la barra de titulo y en el nombre de la pestaña. -->
</head>

<!-- Despues de la seccion del encabezado <head> , Encontraremos la etiqueta de cuerpo - <body> -->
<!-- Hasta este punto. no hay nada descrito para que se muestre en la ventana del navegador -->
<!-- Debemos llenar el cuerpo con el contenido que se mostrara -->

<body>
	<h1>Hola, Mundo!</h1> <!-- La etiqueta <h1> crea un titulo. -->
	<!-- Tambien tenemos subtitulos para <h1> desde la mas importante <h2> a la mas precisa <h6> -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">ven mira lo que esto muestra.</a> <!-- Un hipervinculo a la URL dada por el atributo href="" -->
	<p>Esto es un parrafo.</p> <!-- La etiqueta <p> nos permite incluir texto en nuestra pagina HTML -->
	<p>Este es otro parrafo.</p>
	<ul> <!-- La etiqueta <ul> crea una lista de viñetas -->
	<!-- Para tener una lista numerada usariamos la etiqueta <ol> dando 1. para el primer elemento, 2. para el segundo, etc. -->
		<li>Este es un elemento de una lista no numerada (lista de viñetas)</li>
		<li>Este es otro item</li>
		<li>Y este es el ultimo item de la lista</li>
	</ul>
</body>

<!-- Y esto es todo, la creacion de un archivo HTML puede ser muy simple. -->

<!-- Sin embargo, es posible añadir muchos otros tipos de etiquetas HTML  -->

<!-- Para insertar una imagen -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- La fuente donde se localiza la imagen se indica utilizando el atributo src=""-->
<!-- La fuente puede ser una URL o incluso una ruta a una arhivo en tu computador. -->

<!-- Tambien es posible crear una tabla -->

<table> <!-- Abrimos una etiqueta o elemento tabla <table> -->
	<tr> <!-- <tr> Nos permite crear una fila. -->
		<th>Primer encabezado</th> <!-- <th> Nos permite dar un titulo a una columna de una tabla -->
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

HTML es escrito en archivos que terminan con (extencion) `.html`.

## Para aprender mas! 

* [wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
