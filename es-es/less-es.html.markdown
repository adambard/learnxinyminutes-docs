---
language: less
filename: learnless-es.less
lang: es-es
contributors:
  - ["Saravanan Ganesh", "http://srrvnn.me"]
translators:
  - ["César Suárez", "https://github.com/csuarez"]
---

Less es un pre-procesador CSS, que añade características como variables, anidación, mixins y más.
Less (y otros pre-procesadores como [Sass](http://sass-lang.com/) ayudan a los desarrolladores a escribir código mantenible y DRY (Don't Repeat Yourself).

```css


//Los comentarios de una línea son borrados cuando Less es compilado a CSS.

/* Los comentarios multi-línea se mantienen. */



/*Variables
==============================*/


/* Puedes almacenar un valor CSS (como un color) en una variable.
Usa el símbolo '@' para crear una variable. */

@primary-color: #a3a4ff;
@secondary-color: #51527f;
@body-font: 'Roboto', sans-serif;

/* Puedes usar las variables por toda tu hoja de estilos.
Ahora, si quieres cambiar un color, sólo lo tienes que hacer una vez.*/

body {
	background-color: @primary-color;
	color: @secondary-color;
	font-family: @body-font;
}

/* Esto compilará en: */

body {
	background-color: #a3a4ff;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Esto es mucho más mantenible que tener que cambiar el color
   cada vez que aparece en tu hoja de estilos. */



/* Mixins
==============================*/


/* Si tienes el mismo código para más de un elemento, puede que
   quieras reutilizarlo fácilmente. */

.center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Puedes usar un mixin simplemente añadiendo el selector como un estilo. */

div {
	.center;
	background-color: @primary-color;
}

/* Esto compilará en: */

.center {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}

div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #a3a4ff;
}

/* Puedes omitir que se compile el código del mixin añadiendo un
   paréntesis después del selector. */

.center() {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}

div {
  .center;
  background-color: @primary-color;
}

/* Esto compilará en: */
div {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  background-color: #a3a4ff;
}



/* Anidación
==============================*/


/* Less te permite anidar selectores dentro de otros selectores. */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #f00;
	}
}

/* '&' es replazado por el selector padre. */
/* También se pueden anidar seudo clases. */
/* Ten en cuenta que anidar demasiado puede hacer tu código menos
   mantenible. Las buenas prácticas recomiendan no tener más de 3 niveles de
	 anidación. Por ejemplo: */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: red;

		&:hover {
		  background-color: blue;
		}

		a {
		  color: white;
		}
	}
}

/* Compila en: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



/* Funciones
==============================*/


/* Less ofrece funciones que pueden ser usadas para cumplir una gran variedad
   de tareas. Considera lo siguiente: */

/* Las funciones pueden ser llamadas usando su nombre y pasándole los argumentos
   requeridos. */

body {
  width: round(10.25px);
}

.header {
	background-color: lighten(#000, 0.5);
}

.footer {
  background-color: fadeout(#000, 0.25)
}

/* Compila en: */

body {
  width: 10px;
}

.header {
  background-color: #010101;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* Puedes definir tus propias funciones. Las funciones son muy similares a
   los mixins. Cuando tengas que elegir entre una función y un mixin, recuerda
   que los mixins son mejores para generar CSS, mientras que las funciones son
   mejores para la lógica que puedas necesitar en tu código Sass. Los ejemplos
   de la sección 'Operadores matemáticos' son candidatos ideales para ser
   usados como una función reusable. */

/* Esta función calcula la media de dos números. */

.average(@x, @y) {
  @average-result: ((@x + @y) / 2);
}

div {
  .average(16px, 50px);        // aquí se llama a la función
  padding: @average-result;    // y aquí se usa el valor "devuelto"
}

/* Compila en: */

div {
  padding: 33px;
}



/* Extender (Herencia)
==============================*/


/* Extend es una manera de compartir propiedades de un selector con otro. */

.display {
  height: 50px;
}

.display-success {
  &:extend(.display);
	border-color: #22df56;
}

/* Compila en: */
.display,
.display-success {
  height: 50px;
}
.display-success {
  border-color: #22df56;
}

/* Extender una declaración CSS es preferible a crear un mixin
   debido a la manera en la que Sass agrupa las clases que comparten
   los mismos estilos base. Si esto fuese hecho con un mixin, el ancho,
   alto y el borden aparecerían duplicados para cada una de las declaraciones
   que usasen el mixin. Esto no afectará a tu workflow, pero infla
   innecesariamente los ficheros generados por el compilador Less. */


/*Partials and Imports
==============================*/



/* Less allows you to create partial files. This can help keep your Less
   code modularized. Partial files conventionally begin with an '_',
   e.g. _reset.less. and are imported into a main less file that gets
   compiled into CSS */

/* Consider the following CSS which we'll put in a file called _reset.less */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

 /* Less ofrece @import para poder importar parciales a un fichero. Esto se
 		diferencia del @import de CSS en que no hace otra petición HTTP para
		importar el fichero, sino que combina el código importado en el código
		compilado. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* Compila en: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* Operaciones matemáticas
==============================*/


/* Less ofrece los siguientes operadores: +, -, *, / y %. Estos son útiles
   para calcular valores directamente en tu código Less en vez de usar valores
   calculados a mano. Mira el siguiente ejemplo que prepara un sencillo diseño
   de dos columnas. */

@content-area: 960px;
@main-content: 600px;
@sidebar-content: 300px;

@main-size: @main-content / @content-area * 100%;
@sidebar-size: @sidebar-content / @content-area * 100%;
@gutter: 100% - (@main-size + @sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: @main-size;
}

.sidebar {
  width: @sidebar-size;
}

.gutter {
  width: @gutter;
}

/* Compila en: */

body {
  width: 100%;
}

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}

.gutter {
  width: 6.25%;
}


```

## Practica Less

Si quieres probar Less en tu navegador, prueba:
* [Codepen](http://codepen.io/)
* [LESS2CSS](http://lesscss.org/less-preview/)

## Compatibilidad

Sass puede ser usado en cualquier proyecto mientras tengas un programa que lo compile en CSS. Quizás quieras comprobar si el CSS que estás usando es compatible con tus navegadores objetivo.

[QuirksMode CSS](http://www.quirksmode.org/css/) y [CanIUse](http://caniuse.com) son buenos recursos para comprobar la compatibilidad de navegadores.

## Más información
* [Documentación oficial (EN)](http://lesscss.org/features/)
* [Less CSS - Guía para principiantes (EN)](http://www.hongkiat.com/blog/less-basic/)
