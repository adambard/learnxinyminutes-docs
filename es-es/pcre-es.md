---
language: PCRE
filename: pcre-es.txt
contributors:
    - ["Sachin Divekar", "http://github.com/ssd532"]
translators:
    - ["Roberto R", "https://github.com/rrodriguze"]
lang: es-es
---

Una expresión regular (regex o regexp para abreviar) es una cadena especial
utilizada para definir un patrón, por ejemplo, buscar una secuencia de
caracteres; por ejemplo, `/^[a-z]+:/` se puede usar para extraer `http:`
desde la URL `http://github.com/`.

PCRE (Pearl Compatible Regular Expressions) es una biblioteca para expresiones
muy similar a la Perls, desde ahí el nombre. Se trata de una de las sintaxis
más comunes para escribir expresiones regulares.

Hay dos tipos de metacaracteres (caracteres con una función especial):

* Caracteres reconocidos en todas partes excepto corchetes

```
  \      caracter de escape
  ^      buscar al principio de la cadena (o línea, en modo multilínea)
  $      busca al final de la cadena (o línea, en modo multilínea)
  .      cualquier caracter exceptoo las nuevas líneas
  [      inicio de clase de caracter
  |      condiciones alternativas del separador
  (      inicio del subpatrón
  )      fin del subpatrón
  ?      cuantificador "0 o 1"
  *      quantificatore "0 o más"
  +      quantificatore "1 o más"
  {      inicio de cuantificador numérico
```

* Caracteres reconocidos entre corchetes

```
  \      caracter de escape
  ^      negar la clase si es el primer caracter
  -      indica una serie de caracteres
  [      clase de caracteres POSIX (si sigue la sintaxis POSIX)
  ]      termina la clase de caracteres
```

PCRE también proporciona clases de caracteres predefinidas

```
  \d     cifra decimal
  \D     cifra NO decimal
  \h     espacio horizontal vacío
  \H     espacio horizontal NO vacío
  \s     espacio
  \S     NO esoacui
  \v     espacio vertical vacío
  \V     espacio vertical NO vacío
  \w     palabra
  \W     "NO palabra"
```

## Ejemplos

Usaremos la siguiente cadena para nuestras pruebas:

```
66.249.64.13 - - [18/Sep/2004:11:07:48 +1000] "GET /robots.txt HTTP/1.0" 200 468 "-" "Googlebot/2.1"
```

Se trata de una línea de log del servidor web Apache.

| Regex | Resultado          | Comentario |
| :---- | :-------------- | :------ |
| `GET`   | GET | Busque exactamente la cadena "GET" (distingue entre mayúsculas y minúsculas) |
| `\d+.\d+.\d+.\d+` | 66.249.64.13 | `\d+` identifica uno o más (cuantificador `+`) números [0-9], `\.` identifica el caracter `.` |
| `(\d+\.){3}\d+` | 66.249.64.13 | `(\d+\.){3}` busca el grupo (`\d+\.`) exactamente 3 veces. |
| `\[.+\]` | [18/Sep/2004:11:07:48 +1000] | `.+` identifica cualquier caracter, excepto las nuevas líneas; `.` indica cualquier carácter |
| `^\S+` | 66.249.64.13 | `^` buscar al inicio de la cadena, `\S+` identifica la primera cadena de caracteres que no sea espacio |
| `\+[0-9]+` | +1000 | `\+` identifica el caracter `+`. `[0-9]` indica una cifra de 0 a 9. La expresión es equivalente a `\+\d+` |

## Otros recursos
[Regex101](https://regex101.com/) - probador de expresiones regulares
