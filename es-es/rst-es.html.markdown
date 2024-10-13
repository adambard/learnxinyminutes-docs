---
language: restructured text (RST)
contributors:
    - ["DamienVGN", "https://github.com/martin-damien"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Oscar Maestre", "http://www.github.com/OscarMaestre"]
filename: restructuredtext-es.rst
lang: es-es
---


RST, de Restructured Text, es un formato de fichero creado por la comunidad Python para escribir documentación. Es parte del paquete [Docutils](https://docutils.sourceforge.io/rst.html).

RST es un lenguaje de marcas similar a HTML pero mucho más ligero y fácil de leer.

## Instalación.

Para utilizar Restructured Text, tendrás que instalar [Python](http://www.python.org) y el paquete `docutils`.

`docutils` puede instalarse con el siguiente comando.

```bash
$ easy_install docutils
```

Si tu sistema tiene `pip`, también puedes utilizarlo para instalar `docutils`.

```bash
$ pip install docutils
```


## Sintaxis.

Un ejemplo simple de sintaxis:

```
.. Las líneas que empiezan por un punto seguido de otro punto son comandos especiales. Si no se encuentra ningún comando, se considerará que esa línea es un comentario.

========================================================================
Los títulos principales se escriben usando el signo igual arriba y abajo
========================================================================

Observa que cada caracter, incluyendo los espacios, necesita un signo igual por encima y por debajo.

Los títulos de nivel medio también usan el signo igual, pero solo por debajo
=============================================================================


Títulos de nivel más bajo con guiones
-------------------------------------


Puedes poner texto en *cursiva* o en **negrita.**  También puedes "marcar" texto como código usando la doble comilla inversa, como ``print()``.

Los caracteres especiales pueden "escaparse" usando el backslash, como \\ o \*.

Las listas son similares a las de Markdown, pero un poquito más sofisticadas.

Recuerda alinear los símbolos de lista (como - o \*) al margen izquierdo del anterior bloque de texto. Recuerda también usar líneas en blanco para separar listas nuevas de las listas padre:


- Primer elemento
- Segundo elemento

  - Subelemento
    
- Tercer elemento

o

* Primer elemento
* Segundo elemento
    
  * Subelemento

* Tercer elemento

Las tablas son muy fáciles de escribir.

=========== ========
País        Capital
=========== ========
Francia     París
Japón       Tokyo
=========== ========

Se pueden elaborar fácilmente tablas más complejas (con columnas y/o filas fusionadas) pero para esto es recomendable leer el documento completo. :)

Hay varias formas de construir enlaces:

- Añadiendo un guión bajo al final de una palabra: GitHub_ y poniendo despues del texto la URL (esto tiene la ventaja de no insertar URLs innecesarias en el texto visible)
- Tecleando un URL completa :  https://github.com/ (se convertirá automáticamente en enlace)
- Creando un link al estilo Markdown: `GitHub <https://github.com/>`_ .

.. _GitHub: https://github.com/
```


## Como utilizarlo

RST viene con el paquete `docutils` dentro del cual está el comando `rst2html`, por ejemplo:

```bash
$ rst2html myfile.rst output.html
```

*Nota : En algunos sistemas el comando podría ser rst2html.py*

Hay aplicaciones más complejas que usan el formato RST:

- [Pelican](http://blog.getpelican.com/), un generador de sitios web estáticos.
- [Sphinx](http://sphinx-doc.org/), un generador de documentación.
- y muchos otros.


## Otras lecturas

- [Referencia rápida oficial](http://docutils.sourceforge.net/docs/user/rst/quickref.html)
