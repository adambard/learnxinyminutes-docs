---
language: css
filename: learncss-es.css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
translators:
    - ["Daniel Zendejas","https://github.com/DanielZendejas"]
lang: es-es
---

Tutorial de CSS en español

En los primeros días de la web no había elementos visuales, todo
era texto plano. Pero después, con el desarrollo de los navegadores,
las páginas con contenido visual empezaron a ser más comunes.
CSS es el lenguaje estándar que existe para separar el contenido
(HTML) y el aspecto visual de las páginas web.

Lo que CSS hace es proveer con una sintaxis que te permite apuntar a distintos 
elementos HTML y asignarles diferentes propiedades visuales.

CSS, como cualquier otro lenguaje, tiene múltiples versiones. Aquí nos enfocamos
en CSS 2.0. No es la versión más reciente pero sí la más soportada y compatible.

**NOTA:** Como los resultados de CSS son efectos visuales, para aprenderlo, 
necesitarás probar todo tipo de cosas en ambientes como 
[dabblet](http://dabblet.com/). Este artículo se enfoca, principalmente, en
la sintaxis y consejos generales.

```css
/* ¡Los comentarios aparecen dentro de diagonal-asterisco, justo como esta línea! */

/* ####################
   ## SELECTORES
   ####################*/

/* Generalmente, la sentencia principal en CSS es muy simple. */
selector { propiedad: valor; /* más propiedades separados por punto y coma...*/ }

/* El selector es usado para apuntar a (seleccionar) un elemento en la página.

¡Puedes apuntar a todos los elementos en la página con el asterisco! */
* { color:red; }

/*
Dado un elemento como este en la página:

<div class='una-clase clase2' id='unaId' attr='valor' />
*/

/* puedes seleccionar el <div> por el nombre de su clase */
.una-clase { }

/*¡O por sus dos clases! */
.una-clase.clase2 { }

/* O por el nombre de su elemento */
div { }

/* O por su Id */
#unaId { }

/* ¡O por el hecho de que tiene un atributo! */
[attr] { font-size:smaller; }

/* O por el hecho de que el atributo tiene un valor determinado */
[attr='valor'] { font-size:smaller; }

/* Empieza con un valor ('val' en este caso)*/
[attr^='val'] { font-size:smaller; }

/* O termina con un valor ('or' en este caso) */
[attr$='or'] { font-size:smaller; }

/* O incluso contiene un valor ('lo' en este caso) */
[attr~='lo'] { font-size:smaller; }

/*Más importante, puedes combinar estos criterios de búsqueda entre sí.
No debe existir ningún espacio entre estas partes porque hace que el 
significado cambie.*/
div.una-clase[attr$='or'] { }

/* También puedes seleccionar un elemento HTML basándote en sus padres*/

/* Un elemento que es hijo directo de otro elemento (Seleccionado de la forma que
vimos anteriormente) */

div.un-padre > .nombre-clase {}

/* O cualquiera de sus ancestros en la jerarquía*/
/* La siguiente sentencia selecciona a cualquier elemento que tenga una clase
"nombre-clase" y sea hijo de un div con clase "un-padre" EN CUALQUIER PROFUNDIDAD*/
div.un-padre .nombre-clase {}

/* advertencia: el mismo selector sin espacio tiene otro significado. ¿Puedes
identificar la diferencia?*/

/* También puedes seleccionar un elemento basado en su hermano inmediato previo*/
.yo-estoy-antes + .este-elemento { }

/*o cualquier hermano previo */
.yo-soy-cualquiera-antes ~ .estes-elemento {}

/* Existen algunas pseudo-clases que permiten seleccionar un elemento
basado en el comportamiendo de la página (a diferencia de la estructura de
la página) */

/* Por ejemplo, para cuando pasas el mouse por encima de un elemento */
:hover {}

/* o una liga visitada*/
:visited {}

/* o una liga no visitada aún*/
:link {}

/* o un elemento de un formulario que esté seleccionado */
:focus {}


/* ####################
   ## PROPIEDADES
   ####################*/

selector {
    
    /* Unidades */
    width: 50%; /* en porcentaje */
    font-size: 2em; /* dos veces el tamaño de la fuente actual */
    width: 200px; /* en pixeles */
    font-size: 20pt; /* en puntos */
    width: 5cm; /* en centimetros */
    width: 50mm; /* en milimetros */
    width: 5in; /* en pulgadas */
    
    /* Colores */
    background-color: #F6E;  /* en hexadecimal corto */
    background-color: #F262E2; /* en hexadecimal largo */
    background-color: tomato; /* puede ser un color con nombre */
    background-color: rgb(255, 255, 255); /* en rgb */
    background-color: rgb(10%, 20%, 50%); /* en rgb percent */
    background-color: rgba(255, 0, 0, 0.3); /* en rgb semi-transparente (con valor alfa)*/
    
    /* Imagenes */
    background-image: url(/ruta-a-la-imagen/imagen.jpg);
    
    /* Fuentes */
    font-family: Arial;
    font-family: "Courier New"; /* si el nombre contiene espacios, debe ir entre comillas */
    font-family: "Courier New", Trebuchet, Arial; /* si la primera fuente no se encontró 
    entonces se busca la seguna, o la tercera, así recursivamente*/
}

```

## Uso

Guarda cualquier CSS que quieras en un archivo con extensión `.css`.

```xml
<!-- Necesitas incluir tu archivo CSS en el elemento <head> de tu HTML: -->
<link rel='stylesheet' type='text/css' href='ruta/archivoDeEstilos.css' />

<!--
también puedes incluir CSS dentro del archivo HTML. Esta no es una buena práctica
y debe ser evitada.
-->
<style>
   selector { propiedad:valor; }
</style>

<!--
También se pueden aplicar propiedades al elemento directamente.
Esta práctica también debe ser evitada a toda costa
-->
<div style='propiedad:valor;'>
</div>

```

## Preferencia y orden

Como te habrás dado cuenta un elemento puede ser seleccionado por más
de un selector. En este caso alguna de las reglas cobra preferencia
sobre las otras:

Dado el siguiente CSS:

```css
/*A*/
p.clase1[attr='valor']

/*B*/
p.clase1 {}

/*C*/
p.clase2 {}

/*D*/
p {}

/*E*/
p { propiedad: valor !important; }

```

Y el siguiente HTML:

```xml
<p style='/*F*/ propiedad:valor;' class='clase1 clase2' attr='valor'>
</p>
```

El orden respetado es el siguiente:  
Recuerda, la preferencia es por cada **property**, no para el bloque completo.

* `E` tiene la preferencia más elevada gracias a la palabra `!important`.  
	Es recomendado evitar esto a menos que sea estrictamente necesario incluirlo.
* `F` le sigue, porque es estilo incrustado directamente en el HTML.
* `A` le sigue, porque es más específico que cualquier otra opción.  
	más específico = más especificadores. Aquí hay tres especificadores: elemento `p` +   
	nombre de la clase `clase1` + un atributo `attr='valor'`
* `C` le sigue. Aunque tiene el mismo número de especificadores como `B`  
	pero aparece después.
* Luego va `B`
* y al final  `D`.

## Compatibilidad

La mayoría de las funcionalidades de CSS2 (y gradualmente de CSS3) son compatibles 
en todos los navegadores y dispositivos. Pero siempre es vital tener en mente la
compatibilidad y disponibilidad del CSS que uses con respecto a los navegadores
y dispositivos para los que desarrolles.

[QuirksMode CSS](http://www.quirksmode.org/css/) es una excelente referencia para esto.

## Recursos

* Para ejecutar un test de compatibilidad, revisa [CanIUse](http://caniuse.com).
* CSS Playground [Dabblet](http://dabblet.com/).
* [Mozilla Developer Network's CSS documentation](https://developer.mozilla.org/en-US/docs/Web/CSS).
* [Codrops' CSS Reference](http://tympanus.net/codrops/css_reference/).

## Otras lecturas

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/).
* [Selecting elements using attributes](https://css-tricks.com/almanac/selectors/a/attribute/).
* [QuirksMode CSS](http://www.quirksmode.org/css/).
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) y [LESS](http://lesscss.org/) para preprocesamiento CSS.
* [CSS-Tricks](https://css-tricks.com).

