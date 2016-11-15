---
language: sass
filename: learnsass.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
  - ["Kyle Mendes", "https://github.com/pink401k"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
translators:
  - ["César Suárez", "https://github.com/csuarez"]
lang: es-es
---

Sass es un lenguaje que extiende CSS y que añade características tales como variables, anidación, mixins y más. Sass (y otros preprocesadores tales como [Less](http://lesscess.org/)) ayudan a los desarrolladores a escribir código mantenible y DRY (Don't Repeat Yourself).

Sass tiene dos sintaxis para elegir: SCSS, que usa la misma que CSS pero con las características añadidas de Sass, y Sass (la sintaxis original) que usa identación en vez de llaves y puntos y comas. Este tutorial está escrito en SCSS.

Si ya estás familiarizado con CSS3, vas a entender Sass relativamente rápido. Sass no ofrece nuevas propiedades de estilo, si no que añade herramientas para escribir tus CSS de manera más eficiente, haciendo su mantenimiento mucho más sencillo.

```scss


//Los comentarios en una sola línea son eliminados cuando Sass es compilado a CSS.

/* Los comentarios multi-línea se mantienen. */



/* Variables
============================== */


/* Puedes almacenar valores CSS (como un color) en una variable.
Usa el símbolo '$' para crear una variable */

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* Puedes usar las variables a lo largo de tu hoja de estilos.
Ahora, si quieres cambiar el color, sólo lo tienes que hacer una vez. */

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* Este código compilará en: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}

/* El resultado es mucho más mantenible que tener que cambiar el color
cada vez que aparece en la hoja de estilos. */



/* Directivas de control
============================== */


/* Sass permite usar @if, @else, @for, @while, y @each para controlar la
   compilación de tu código en CSS. */

/* Los bloques @if/@else se comportan tal como es de esperar */

$debug: true !default;

@mixin debugmode {
	@if $debug {
		@debug "Modo debug activado";

		display: inline-block;
	}
	@else {
		display: none;
	}
}

.info {
	@include debugmode;
}

/* Si $debug es true, .info es mostrado; si es false entonces
no se muestra.

Nota: @debug mostrará información de depuración en la consola.
Es muy útil para ver el contenido de tus variables cuando estás depurando. */

.info {
	display: inline-block;
}

/* @for es un bucle que itera un conjunto de valores.
Es particularmente útil para dar estilos a una colección de objetos.
Hay dos formas "through" y "to". El primero incluye el último valor
mientras que el segundo para antes del último valor. */

@for $c from 1 to 4 {
	div:nth-of-type(#{$c}) {
		left: ($c - 1) * 900 / 3;
	}
}

@for $c from 1 through 3 {
	.myclass-#{$c} {
		color: rgb($c * 255 / 3, $c * 255 / 3, $c * 255 / 3);
	}
}

/* Esto compila en: */

div:nth-of-type(1) {
	left: 0;
}

div:nth-of-type(2) {
	left: 300;
}

div:nth-of-type(3) {
	left: 600;
}

.myclass-1 {
	color: #555555;
}

.myclass-2 {
	color: #aaaaaa;
}

.myclass-3 {
	color: white;
// SASS convierte automáticamente #FFFFFF a white
}

/* @while es bastante sencillo: */

$columns: 4;
$column-width: 80px;

@while $columns > 0 {
	.col-#{$columns} {
		width: $column-width;
		left: $column-width * ($columns - 1);
	}

	$columns: $columns - 1;
}

/* Esto se convierte en el siguiente CSS: */

.col-4 {
	width: 80px;
	left: 240px;
}

.col-3 {
	width: 80px;
	left: 160px;
}

.col-2 {
	width: 80px;
	left: 80px;
}

.col-1 {
	width: 80px;
	left: 0px;
}

/* @each funciona parecido a @for, pero usa una lista en ver del valor ordinal
Nota: puedes especificar listas como cualquier otra variable usando espacios
como delimitadores. */

$social-links: facebook twitter linkedin reddit;

.social-links {
	@each $sm in $social-links {
		.icon-#{$sm} {
			background-image: url("images/#{$sm}.png");
		}
	}
}

/* Esto resultará en: */

.social-links .icon-facebook {
	background-image: url("images/facebook.png");
}

.social-links .icon-twitter {
	background-image: url("images/twitter.png");
}

.social-links .icon-linkedin {
	background-image: url("images/linkedin.png");
}

.social-links .icon-reddit {
	background-image: url("images/reddit.png");
}



/* Mixins
==============================*/


/* Si te encuentras con que estás escribiendo el mismo código en más de un
elemento, puede que quieras almacenarlo en un mixin.

Usa la directiva '@mixin', más un nombre para tu mixin. */

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Puedes usarlo con '@include' y el nombre del mixin. */

div {
	@include center;
	background-color: $primary-color;
}

/* Esto compilará en: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}

/* Puedes usar mixins para crear una propiedad shorthand. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/* La que puedes invocar pasándole los argumentos width y height. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* Compila en: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}



/* Funciones
============================== */


/* Sass ofrece funciones que pueden ser usadas para realizar una gran variedad
   de tareas. Por ejemplo: */

/* Se pueden invocar funciones usando su nombre y pasándole los
   argumentos requeridos. */
body {
  width: round(10.25px);
}

.footer {
  background-color: fade_out(#000000, 0.25);
}

/* Compila en: */

body {
  width: 10px;
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

/* Esta función toma un tamaño objetivo y el tamaño de un padre y
  devuelve el porcentaje. */

@function calculate-percentage($target-size, $parent-size) {
  @return $target-size / $parent-size * 100%;
}

$main-content: calculate-percentage(600px, 960px);

.main-content {
  width: $main-content;
}

.sidebar {
  width: calculate-percentage(300px, 960px);
}

/* Compila en: */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}



/* Extender (Herencia)
============================== */


/* Extend es una manera de compartir propiedades de un selector con otro. */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* Compila en: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.display-success {
  border-color: #22df56;
}

/* Extender una declaración CSS es preferible a crear un mixin
   debido a la manera en la que Sass agrupa las clases que comparten
   los mismos estilos base. Si esto fuese hecho con un mixin, el ancho,
   alto y el borden aparecerían duplicados para cada una de las declaraciones
   que usasen el mixin. Esto no afectará a tu workflow, pero infla
   innecesariamente los ficheros generados por el compilador Sass. */



/* Anidación
============================== */


/* Sass permite anidar selectores dentro de otros selectores. */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
	}
}

/* '&' será reemplazado por el selector del padre. */

/* También puedes anidar seudo clases. */

/* Ten en cuenta que anidar demasiado hará tu código menos mantenible.
Como buena práctica, se recomienda no tener más de 3 niveles de anidación.
Por ejemplo: */

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



/* Parciales e importaciones
============================== */


/* Sass permite que crees ficheros parciales. Esto te puede ayudar a mantener
   tu código Sass modularizado. Los ficheros parciales deben comenzar por '_',
   p.e. _reset.css.
   Los parciales no son convertidos en CSS. */

/* Mira este al que vamos a añadir un fichero llamado _reset.css */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Con @import puedes importar parciales a un fichero. Este se diferencia del
   @import de CSS en que no hace otra petición HTTP para importar el fichero.
   Sass, sino que combina el código importado en el código compilado. */

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



/* Placeholders
============================== */


/* Los placeholders son útiles cuando estás creando una declaración CSS a
   extender. Si quieres crear una declaración que sólo va a ser usada con @extend,
   puedes hacerlo mediante un placeholder. Los placeholders comienzan con '%'
   en vez de '.' o '#'. Esto no aparecen en el código CSS compilado. */

%content-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  @extend %content-window;
  background-color: #0000ff;
}

/* Compila en: */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}



/* Operaciones matemáticas
============================== */


/* Sass provee los siguientes operadores: +, -, *, / y %. Estos son útiles
   para calcular valores directamente en tu código Sass en vez de usar valores
   calculados a mano. Mira el siguiente ejemplo que prepara un sencillo diseño
   de dos columnas. */

$content-area: 960px;
$main-content: 600px;
$sidebar-content: 300px;

$main-size: $main-content / $content-area * 100%;
$sidebar-size: $sidebar-content / $content-area * 100%;
$gutter: 100% - ($main-size + $sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: $main-size;
}

.sidebar {
  width: $sidebar-size;
}

.gutter {
  width: $gutter;
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

## ¿SASS o Sass?
¿Alguna vez has pensado si Sass es un acrónimo o no? Seguramente no, pero te lo vamos a explicar de todas maneras. "Sass" es una palabra, no un acrónimo.
Como todo el mundo lo escribe como "SASS", el creador del lenguaje lo ha llamado de broma "Syntactically Awesome StyleSheets" (Hojas de estilo sintácticamente increíbles).


## Practica Sass
Si quieres probar Sass en tu navegador, prueba  [SassMeister](http://sassmeister.com/).
Puedes usar cualquier sintaxis, o elegir en la configuración entre Sass y SCSS.

## Compatibilidad
Sass puede ser usado en cualquier proyecto mientras tengas un programa que lo compile en CSS. Quizás quieras comprobar si el CSS que estás usando es compatible con tus navegadores objetivo.

[QuirksMode CSS](http://www.quirksmode.org/css/) y [CanIUse](http://caniuse.com) son buenos recursos para comprobar la compatibilidad de navegadores.


## Más información
* [Documentación oficial (EN)](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way (EN)](http://thesassway.com/) tiene tutoriales (para principiantes y avanzandos) and artículos.
