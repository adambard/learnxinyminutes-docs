---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
translators:
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
lang: es-es
filename: asciidoc-es.md
---

AsciiDoc es un lenguaje de marcas similar a Markdown que puede ser usado para cualquier uso, desde libros a blogs.
Creado en 2002 por Stuart Rackham, el lenguaje es simple pero permite un gran nivel de personalización.

Cabecera de documento

La cabecera es opcional y no puede contener lineas vacías. Debe estar separada del contenido por al menos una línea en blanco.

Solo título

```
= Título de documento

Primer contenido del documento.
```

Título y autor

```
= Título del documento
Nombre Apellido(s) <nombre.apellido@learnxinyminutes.com>

Inicio de este documento.
```

Múltiples autores

```
= Título del documento
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Inicio de un documento con múltiples autores.
```

Linea de versión (requiere línea de autor)

```
= Título del documento V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

Este artículo sobre patatas fritas será genial.
```

Párrafo

```
No necesitas nada especial para un párrafo.

Inserta una línea vacía entre cada párrafo para separarlos.

Para insertar un salto de línea, solo añade un +
y ya lo tienes!
```

Dando formato al texto

```
_guión bajo para cursiva_
*asteriscos para negrita*
*_combínalos y verás_*
`usa comillas invertidas para monospace`
`*combina para negrita monospace*`
```

Títulos de sección

```
= Nivel 0 (úsalo solo para el título del documento)

== Nivel 1 <h2>

=== Nivel 2 <h3>

==== Nivel 3 <h4>

===== Nivel 4 <h5>
```

Listas

Para crear una lista sin orden usa asteriscos.

```
* foo
* bar
* baz
```

Para crear una lista numerada usa puntos.

```
. item 1
. item 2
. item 3
```

Puedes crear hasta 5 subniveles en las listas añadiendo asteriscos o puntos.

```
* foo 1
** foo 2
*** foo 3
**** foo 4
***** foo 5

. foo 1
.. foo 2
... foo 3
.... foo 4
..... foo 5
```

## Referencias

Existen dos herramientas para procesar documentación en AsciiDoc:

1. [AsciiDoc](http://asciidoc.org/): implementación original para Python, disponible en las principales distribuciones Linux. Versión estable actualmente en modo mantenimiento.
2. [Asciidoctor](http://asciidoctor.org/): implementación alternativa para Ruby, usable también desde Java y JavaScript. Implementación completa en evolución, su objetivo es ampliar AsciiDoc con nuevas funcionalidades y conversores de salida.

Los siguientes enlaces pertenecen a `Asciidoctor` (documentación en inglés):

* [Comparación de sintaxis Markdown - AsciiDoc](http://asciidoctor.org/docs/user-manual/#comparison-by-example): comparativa de elements comunes entre Markdown y AsciiDoc.
* [Primeros pasos](http://asciidoctor.org/docs/#get-started-with-asciidoctor): manuales de instalación e inicio para convertir documentos simples.
* [Manual de usuario de Asciidoctor](http://asciidoctor.org/docs/user-manual/): referencia completa en un único documento, contiene ejemplos, guías de herramientas, etc.
