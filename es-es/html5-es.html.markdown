---
language: HTML/HTML5
contributors:
    - ["Walter Cordero", "http://waltercordero.com"]
filename: learnhtml-es.html
lang: es-es
---
Tutorial de HTML5 en español.

A finales de la década de los 80s, Tim Berners-Lee estuvo trabajando como físico en CERN
(Organización Europea para la Investigación Nuclear). Ideó un sistema para que los 
científicos pudieran compartir documentos a través de internet. Antes de su invención, las
comunicaciones por internet sólo permitían transmitir texto plano, empleando tecnologías
como el email, FTP (File Transfer Protocol). La invención del HTML permitió el uso de
un modelo contenido almacenado en un servidor central, que podía ser mostrado en un
terminal local mediante un navegador. Esto simplificó el accesso al contenido y habílitó
la posibilidad de mostrar contenido "enriquecido" como un sofisticado texto con formato
y visualización de imágenes.

HTML, son las siglas para HyperText Markup Languaje (Lenguaje de marcas de Hipertexto),
la cual hace referncia al lengua de marcado para la elaboración de páginas web. Es un
estándar que sirve de referencia para la elaboración de páginas web en sus diferentes
versiones (la más actual HTML5). Esta define una estructura básica y un código para 
la definición de contenido de una página web, estos pueden ser: texto, imágenes, videos,
entre otros.

El estándar esta a cargo de la W3C, que es la organización dedicaca a la estandarización
de las tecnologías ligadas al desarrollo web, principalmente en su escritura e 
interpretación. Cabe destacar que durante sus versiones se han añadido y eliminado 
ciertas etiquetas que por las nuevas tecnologías han quedado completamente obsoletas.

¡La retroalimentación es bienvenida! Puedes encontrarme en: 
[@WalterC_87](https://twitter.com/WalterC_87), o
[me@waltercordero.com](mailto:me@waltercordero.com).

```HTML
// los documentos HTML tienen una extensión .html indicando que es un archivo de hipertexto
holamundo.html

// cuando se inicia a trabajar un sitio web, el archivo principal recibe el nombre de index
index.html

// HTML es un lenguaje basado en etiquetas (tags), estas etiquetas se encierran entre los 
// signos menor que (<) y mayor que (>)
<etiqueta>

// en HTML la mayoría de las etiquetas tienen una etiqueta de apertura y una de cierre
// la etiqueta de cierre despues del signo (>) lleva una diagonal (/), indicando al 
// interprete que la etiqueta se esta cerrando y en medio de ellas hay contendio en hipertexto
<etiqueta> Acá hay contenido para ser interpretado </etiqueta>

///////////////////////////////////
// 1. Creación de Documentos HTML

// Todo nuevo documento HTML debe llevar la etiqueta !DOCTYPE seguido de "html" indicando
// que es un documento de hipertexto al navegador, esta etiqueta no tiene etiqueta de cierre
<!DOCTYPE html>

// la estructura de un documento HTML esta dado por tres etiquetas básicas, html, head y body
// estas etiquetas si poseen etiqueta de cierre y de apertura, cabe mencionar que estas 
// etiquetas son únicas, esto quiere decir que solo debe existir una etiqueta html, head y body
// para cada página web
<html>
    <head>
    </head>

    <body>
    </body>
</html>

// Dentro de la etiqueta <head> se encuentra información que no es visible en el cuerpo del
// página web, en otras palabras se encuentra la configuración del mismo, aca se agregan 
// etiquetas como el titulo de la página, la descripción, el formato de escritura, estilos y más.

<head>
    <title>Titulo de la página web</title>
    <meta charset="utf-8">
    <style>
        //Aca van los estilos
    </style>
<head>

// se debe tener claro que los documentos HTML son los que brindan la estructura que tendra
// la página web, los estilos son proporcionados por medio de sintaxis CSS, esta sintaxis
// puede estar almacenada en archivos externos .css o bien pueden estar incorporados dentro
// de la etiqueta <head>. Para llamar archivos externos se utiliza la etiqueta <link>
<head>
    <link rel="stylesheet" type="text/css" href="la url o dirección donde esta el archivo/file.css"/>
</head>

// dentro de la etiqueta <body> se encuentra el contenido que será visible en el navegador para
// el visitante, dentro de ella pueden ir etiquetas de parrafo, titulos, articulos, encabezados y más
<body>
    <h1>Esto es el titulo principal</h1>
    <p>esto representa un párrafo</p>
    <article>Este es un espacio para agregar contendio de articulos</article>
</body>

// HTML5 brinda etiquetas especializadas para crear una mejor estructura de cada documento
// la etiqueta <nav> indica al interprete que es una menú de navegación.
<nav>
    //Elementos del menu
</nav>

// la etiqueta <header> indica el encabezado de una página, un artículo o una sección
<header>
    //representa encabezados, aca pueden ir cualquier tipo de etiqueta
</header>

// la etiqueta <section> es utilizada para la creacion de diferentes secciones dentro de 
// una página web, utilizada principalmente para dividir diferentes partes de contenido
// dentro de esta etiqueta puede ir cualquer etiqueta
<section>
    // esto representa una seccion
</section>

<section>
    // esto representa una nueva seccion diferente a la anterior
</section>

// la etiqueta <article> puede utilizarse para representar pequeñas porciones de HTML
// que pueden ir dentro de una etiqueta <section>
<section>
    <article>
        // aca va el contenido del articulo o información que se desea mostrar
    </article>
</section>

// la etiqueta <aside> es utilizada para la creacion de secciones con información extra
// que bien pueden estar ubicada a la izquierda, derecha o arriba de nuestro contenido 
// principal.
<aside>
    <section>
        <p>ejemplo de contenido extra para una página web</p>
    </section>
<aside>

// asi como existe una etiqueta para encabezados existe una etiqueta para pies de pagina
// llamada <footer>, la cual es utilizada para el pie de página para cualquier sección o
// bien para el pie de una página web.
<footer>
    <p>este contenido ira al pie de una página web</p>
</footer>

///////////////////////////////////
// 2. Texto y Tipografía en HTML

// Existen ciertos carácteres especiales en HTML los cuales tienen códigos para que puedan
// ser escritos de la manera correcta

* (<) código: &lt;
* (>) código: &gt;
* (á) código: &aacute;
* (Á) código: &Aacute;
* (é) código: &eacute;
* (É) código: &Eacute;
* (í) código: &iacute;
* (Í) código: &Iacute;
* (ó) código: &oacute;
* (Ó) código: &Oacute;
* (ú) código: &uacute;
* (Ú) código: &Uacute;
* (ñ) código: &ntilde;
* (Ñ) código: &Ntilde;
* (™) código: &#153;
* (€) código: &euro;
* (ç) código: &ccedil;
* (Ç) código: &Ccedil;
* (ü) código: &uuml;
* (Ü) código: &Uuml;
* (&) código: &amp;
* (¿) código: &iquest;
* (¡) código: &iexcl;
* (") código: &quot;
* (·) código: &middot;
* (º) código: &ordm;
* (ª) código: &ordf;
* (¬) código: &not;
* (©) código: &copy;
* (®) código: &reg;

// En ciertas ocasiones es necesario comentar porciones de HTML para indicar a otras personas
// que encontrará en dicha sección, para ello se hace uso de <!-- -->
<!-- Este es un ejemplo de comentario dentro de HTML que no será visto en el navegador -->

// Los saltos de línea son utilizados cuando se desea marcar un espacio en blanco entre
// un contenido y otro, o bien para que aparezca en la siguiente línea cierto contendio
// para ello se utiliza la etiqueta <br>
<section>
    <p>
        sección <br> 1
    </p>
</section>

// esto en el navegador se verá así:
sección
1

// para asegurarnos de que el texto aparezca tal como ha sido creado se puede utilizar la
// etiqueta de texto preformateado <pre></pre>
<pre>Hola,     BIENVENIDOS
esta     ES MI PÁGINA WEB
     y esto un texto preformateado</pre>

// obteniendo como resultado en el navegdor:
Hola,     BIENVENIDOS
esta     ES MI PÁGINA WEB
     y esto un texto preformateado

// un elemento que suele utilizarse para la separación de secciones es la etiqueta separadora <hr>
// esta etiqueta posee ciertas configuraciones como su alineación, ancho, tamaño
Inicio<hr align="left" width="300%" size="5" noshade>Bienvenidos a mi p&aacute;gina.

// La sangría es una especia de margen que se establece a ambos lados del texto. para que
// aparezca sangrado, se insertan las etiquetas <blockquote> </blockquote>
Queridos usuarios,
<blockquote>
  <blockquote>
  tengo el placer de comunicaros que hay una nueva secci&oacuten.
  </blockquote>
</blockquote>

// obteniendo como resultado en el navegador:
Queridos usuarios,
    tengo el placer de comunicaros que hay una nueva secci&oacuten.

// El texto de una página puede agruparse en párrafos. Para ello, el texto de cada uno de los 
// párrafos debe insertarse entre las etiquetas <p> y </p>.   
<p>Bienvenidos a mi p&aacutegina.</p>
<p>Aqu&iacute encontrar&eacuteis cursos de formaci&oacuten muy interesantes.</p>     

//Existen una serie de encabezados que suelen utilizarse para establecer títulos dentro de 
// una página. La diferencia entre los distintos tipos de encabezado es el tamaño de la letra, 
// el tipo de resaltado, y la separación existente entre el texto y los elementos que 
// tiene encima y debajo de él

* <h1>
* <h2>
* <h3>
* <h4>
* <h5>
* <h6>

///////////////////////////////////
// 3. Hipervínculos o Links

//Un hiperenlace, hipervínculo, o vínculo, no es más que un enlace, que al ser pulsado lleva 
// a una página, archivo o seccion. Aquellos elementos (texto, imágenes, etc.) sobre los que se 
// desee insertar un enlace han de encontrarse entre las etiquetas <a> y </a>.
// A través del atributo href se especifica la página a la que está asociado el enlace, la página que 
// se visualizará cuando el usuario haga clic en el enlace.
<a href="http://waltercordero.com">Visítame http://waltercordero.com</a>

// El destino del enlace determina en qué ventana va a ser abierta la página vinculada, se especifica 
// a través del atributo target al que se le puede asignar los siguientes valores:
**_blank: 
    Abre el documento vinculado en una ventana nueva del navegador.

**_parent: 
    Abre el documento vinculado en la ventana del marco que contiene el vínculo o en el 
    conjunto de marcos padre.

**_self:
    Es la opción predeterminada. Abre el documento vinculado en el mismo marco o ventana que el vínculo.

// Existen otros tipos de enlaces que no conducen a otra página web, los veremos a continuación:
// Correo electrónico: Abre la aplicación Outlook Express para escribir un correo electrónico, cuyo 
// destinatario será el especificado en el enlace. Para ello la referencia del vínculo debe 
// ser "mailto:direcciondecorreo".
<a href="mailto:me@waltercordero.com">e-mail para Walter Cordero</a>

// Vínculo a ficheros para descarga: El valor del atributo href normalmente será una página web,
// pero también puede ser un fichero comprimido, una hoja de Excel, un documento Word, un documento 
// con extensión pdf. Cuando el enlace no es a una página Web nos aparecerá el cuadro de diálogo 
// en el que el navegador le pide al usuario permiso para descargar el fichero en su ordenador.
<a href="carta.doc" tarjet=_blank >haz clic aqu&iacute; para descargarte el fichero</a>

///////////////////////////////////
// 4. Manejo de Imágenes

// Todas las páginas web acostumbran a tener un cierto número de imágenes, que permiten mejorar 
// su apariencia, o dotarla de una mayor información visual.
// Para insertar una imagen es necesario insertar la etiqueta <img>. Dicha etiqueta no necesita 
// etiqueta de cierre. El nombre de la imagen ha de especificarse a través del atributo src.
<img src="imagenes/logo_animales.gif">

//Cuando una imagen no puede ser visualizada en el navegador, cosa que puede ocurrir al especificar 
// mal el valor del atributo src, aparece un recuadro blanco con una X en su lugar, junto con el nombre de la imagen.
// Se puede hacer que en lugar de mostrarse el nombre de la imagen aparezca el texto personalizado, 
// gracias al atributo alt.
<img src="carpeta/gatito.gif" alt="Imagen gato" >

///////////////////////////////////
// 5. Tablas

// Antes de iniciar con el tema de tablas, cabe mencionar que hace muchos años las páginas se
// creaban solo con tablas, se creaba una tabla y luego se iban creando las columnas y filas, 
// dentro de las celdas se insertaba el contenido.

// Ahora el uso de las tablas se da única y exclusivamente para mostrar información, ya separación
// reportes o tablas de comparación, pero jamás se deben utilizar para crear la estructura de una
// página web.

// Las tablas están formadas por celdas, que son los recuadros que se obtienen como resultado de 
// la intersección entre una fila y una columna.

// Para crear una tabla hay que insertar las etiquetas <table> y </table>. Entre dichas etiquetas
// habrá que especificar las filas y columnas que formarán la tabla.

// Es necesario insertar las etiquetas <tr> y </tr> por cada una de las filas de la tabla. 
// Estas etiquetas deberán insertarse entre las etiqetas <table> y </table>.
<table>
  <tr>...</tr>
  <tr>...</tr>
  <tr>...</tr>
  <tr>...</tr>
  <tr>...</tr>
</table>

// Para crear una tabla no basta con especificar el número de filas, es necesario también especificar
// el número de columnas.
// Es necesario insertar las etiquetas <td> y </td> por cada una de las celdas que compongan cada una 
// de las filas de la tabla. Por lo tanto, habrá que insertar esas etiquetas entre las etiquetas <tr> y </tr>.
// Entre las etiquetas <td> y </td> se podrá especificar el contenido de cada una de las celdas.

<table border="1">
  <tr>
    <td>Sabado</td>
    <td>Domingo</td>
  </tr>
  <tr>
    <td>Curso HTML</td>
    <td>Curso JavaScript</td>
  </tr>
  <tr>
    <td>Curso AngularJS</td>
    <td>Curso ReactJS</td>
  </tr>
</table>

// Las etiquetas <td> y </td> se utilizan para definir las celdas de cada una de las filas, 
// pero en su lugar las etiquetas <th> y </th>.

// Para la etiqueta <th> es posible especificar los mismos atributos que para la etiqueta <td>, 
// pero esta nueva etiqueta hace que el texto de la celda aparezca centrado y en negrita, 
// por lo que se utiliza para definir los encabezados o títulos de las columnas.
<table border="1">
  <tr>
    <th>Sabado</th>
    <th>Domingo</th>
  </tr>
  <tr>
    <td>Curso HTML</td>
    <td>Curso JavaScript</td>
  </tr>
  <tr>
    <td>Curso AngularJS</td>
    <td>Curso ReactJS</td>
  </tr>
</table>

// No solamente es posible establecer títulos para las columnas, también es posible establecer un 
// título para la tabla mediante las etiquetas <caption> y </caption>.

//Estas etiquetas han de ir después de la etiqueta <table>, y puede especificarse el valor de los 
// atributos align (con los valores bottom, center, left, right y top) y valign (con los valores bottom y top).
<table border="1">
  <caption align="right" valign="top">Titulo de la tabla<tr>
  <tr>
    <th>Sabado</th>
    <th>Domingo</th>
  </tr>
  <tr>
    <td>Curso HTML</td>
    <td>Curso JavaScript</td>
  </tr>
  <tr>
    <td>Curso AngularJS</td>
    <td>Curso ReactJS</td>
  </tr>
</table>

// Para las etiquetas <td> y <th> existen los atributos colspan y rowspan, que se utilizan para combinar celdas.
// A través del atributo colspan se especifica el número de columnas por las que se extenderá la 
// celda, y a través del atributo rowspan se especifica el número de filas por las que se extenderá la celda.
<table width="575" border="2" cellspacing="2">
  <tr align="center" valign="middle"> 
    <th colspan="4">DIFERENCIAS ENTRE EL PERRO Y EL HOMBRE</th>
  </tr>
  <tr align="center" valign="middle"> 
    <th rowspan="2">DIFERENCIAS</th>
    <th colspan="2">PERRO</th>
    <th rowspan="2">HOMBRE</th>
  </tr>
  <tr align="center" valign="middle"> 
    <th>PEQUE&Ntilde;O</th>
    <th>GRANDE</th>
  </tr>
  <tr align="center" valign="middle"> 
    <td>Duraci&oacute;n crecimiento</td>
    <td>10 meses</td>
    <td>18 a 24 meses</td>
    <td>16 a&ntilde;os</td>
  </tr>
  <tr align="center" valign="middle"> 
    <td>Tiempo de gestaci&oacute;n</td>
    <td colspan="2">58 a 63 d&iacute;as</td>
    <td>9 meses</td>
  </tr>
  <tr align="center" valign="middle"> 
    <td>Duraci&oacute;n de vida del pelo/cabello</td>
    <td colspan="2">1 a&ntilde;o</td>
    <td>2 a 7 a&ntilde;os</td>
  </tr>
</table>

///////////////////////////////////
// 6. Formularios

// Un formulario es un elemento que permite recoger datos introducidos por el usuario.

// Los formularios se utilizan para conocer las opiniones, dudas, y otra serie de datos sobre los usuarios, 
// para introducir pedidos a través de la red, tienen multitud de aplicaciones.

// Un formulario está formado, entre otras cosas, por etiquetas, campos de texto, menús desplegables, y botones.

// Los formularios se insertan a través de las etiquetas <form> y </form>. Entre dichas etiquetas 
// habrá que insertar los diferentes objetos que formarán el formulario. la etiqueta <form> tiene los siguientes atributos:

**action 
indica una dirección de correo electrónico a la que mandar el formulario, o la dirección del programa 
que se encargará de procesar el contenido del formulario.

**enctype indica el modo en que será cifrada la información para su envío. 
Por defecto tiene el valor application/x-www-form-urlencoded.

**method indica el metodo mediante el que se transferirán las variables del formulario. 
Su valor puede ser get o post.

**get se utiliza cuando no se van a producir cambios en ningún documento o programa que no sea el 
navegador del usuario que pretende mandar el formulario, como ocurre cuando se realizan consultas 
sobre una base de datos.

**post se utiliza cuando sí se van a producir cambios, como ocurre cuando el usuario manda 
datos que deben ser almacenados en una base de datos.

// por ejemplo
<form action="mailto:formularios@aulaclic.com" method="post" enctype="text/plain" >
  ...
</form>

// Las áreas de texto permiten a los usuarios insertar varias líneas de texto. Por ello, 
// suelen utilizarse para que incluyan comentarios.

// Para insertar un área de texto es necesario incluir las etiquetas <textarea> y </textarea> 
// entre las etiquetas <form> y </form> del formulario
<textarea name="ejemploarea" cols="30" rows="3">Escribe el texto que quieras</textarea>

//Para insertar un elemento de entrada es necesario incluir la etiqueta <input> entre las 
etiquetas <form> y </form> del formulario.

// El atributo name indica el nombre que se desea dar al elemento de entrada, mediante el cual 
// será evaluado, y el atributo type indica el tipo de elemento de entrada.

// Vamos a ver los diferentes tipos de elementos de entrada, y el resto de atributos que pueden 
// definirse para cada uno de ellos

**Campo de texto:
// Para insertar un campo de texto, el atributo type debe tener el valor text.

// El atributo size indica el número de caracteres que podrán ser visualizados en el campo 
// de texto, determina el ancho de la caja.

// El atributo maxlenght indica el número de caracteres que podrán ser insertados en el campo de texto.

// El atributo value indica el valor inicial del campo de texto.
<input name="campo" type="text" value="Campo de texto" size="20" maxlength="15">

**Campo de contraseña
// Para insertar un campo de contraseña, el atributo type debe tener el valor password.

// El resto de atributos son los mismos que para un campo de texto normal. La única diferencia es 
// que todas las letras escritas en el campo de contraseña serán visualizadas como asteriscos.
<input name="contra" type="password" value="contraseña" size="20" maxlength="15">

**Botón:
// Para insertar un botón, el atributo type debe tener el valor submit, restore o button.

// Si el valor es submit, al pulsar sobre el botón se enviará el formulario.

// Si el valor es restore, al pulsar sobre el botón se restablecerá el formulario, 
// borrándose todos los campos del formulario que hayan sido modificados y adquiriendo su valor inicial.

// Si el valor es button, al pulsar sobre el botón no se realizará ninguna acción.

// El atributo value indica el texto que mostrará el botón.
<input name="boton" type="submit" value="Enviar">

**Casilla de verificación:
// Para insertar una casilla de verificación, el atributo type debe tener el valor checkbox.

// El atributo value indica el valor asociado a la casilla de verificación. 
// Es necesario poner este atributo, aunque el usuario no pueda ver su valor. Es el valor a enviar.

// La aparición del atributo checked indica que la casilla aparecerá activada inicialmente. 
// Este atributo no toma valores.
<input name="casilla" type="checkbox" value="acepto" checked>

**Botón de opción:
//Para insertar un botón de opción, el atributo type debe tener el valor radio.

// El atributo value indica el valor asociado al botón de opción. Es necesario poner este atributo, 
// aunque el usuario no pueda ver su valor. Es el valor a enviar.

// La aparición del atributo checked indica que el botón aparecerá activado inicialmente. 
// Este atributo no toma valores.

// Los botones de opción se utilizan cuando se desea que una variable del formulario pueda tomar 
// un solo valor de entre varios posibles. Para ello, se insertan varios botones de opción 
// con el mismo nombre (que indica la variable) y con distintos valores. 
// Sólamente uno de estos botones podrá estar activado, el que esté activado cuando se envia el 
// formulario, su valor será el que tendrá la variable.
<input name="prefiere" type="radio" value="estudiar" checked>
<input name="prefiere" type="radio" value="trabajar">


// Los campos de selección se utilizan para insertar menús y listas desplegables.
// Para insertar uno de estos menús o listas es necesario insertar las etiquetas <select> y </select> en un formulario.
// El atributo name indica el nombre del menú o lista será el nombre de la variable que contendrá el valor seleccionado.
// El atributo size indica el número de elementos de la lista que pueden ser visualizados al mismo tiempo, 
// determina el alto de la lista.

// La aparición del atributo multiple indica que el usuario podrá seleccionar varios elementos 
// de la lista al mismo tiempo, ayudándose de la tecla Ctrl. Este atributo no toma valores.

// La aparición del atributo disabled indica que la lista estará desactivada, por lo que el usuario no podrá 
// seleccionar sus elementos. Este atributo tampoco toma valores.

// Cada uno de los elementos de la lista ha de insertarse entre las etiquetas <option> y </option>.

// El atributo value indica el valor a enviar si se selecciona el elemento. Si no se especifica este atributo,
// se enviará el texto de la opción, que se encuentra entre las etiquetas <option> y </option>.

// La aparición del atributo selected indica que el elemento aparecerá seleccionado. Este atributo no toma valores.
<select name="animal" size="3" multiple>
  <option selected>---Elige animales---</option>
  <option value="ave">Loro</option>
  <option>Perro</option>
  <option>Gato</option>   
  <option>Pez</option>
</select>

## Fuentes y Referencias

La [Red para Desarroladores de Mozilla](https://developer.mozilla.org/es/docs/Web/Guide/HTML/Introduction_alhtml) 
proveé excelente documentación para HTML para navegadores. Además, está en formato de wiki,
por lo que mientras vayas aprendiendo podrás ayudar a los demás con tu experiencia.

[Curso de HTML](http://www.aulaclic.es/html) 
una guía interesante que proveé ciertos retos para que el estudiante aplique lo aprendido.

[Curso de HTML de TreeHouse](https://teamtreehouse.com/library/html) junto a Nick Pettit aprenderás como escribir
correctamente sintaxis para crear excelentes documentos HTML.
