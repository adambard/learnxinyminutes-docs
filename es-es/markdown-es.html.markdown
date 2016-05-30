---
language: markdown
filename: markdown-es.md
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Daniel Zendejas", "https://github.com/DanielZendejas"]
lang: es-es
---

Markdown fue creado por John Gruber en 2004. Su propósito es ser una sintaxis fácil de leer y escribir que se convierta
fácilmente a HTML (y, actualmente, otros formatos también).

¡Denme toda la retroalimentación que quieran! / ¡Sientanse en la libertad de hacer forks o pull requests!


```markdown
<!-- Markdown está basado en HTML, así que cualquier archivo HTML es Markdown
válido, eso significa que podemos usar elementos HTML en Markdown como, por
ejemplo, el comentario y no serán afectados por un parseador Markdown. Aún 
así si creas un elemento HTML en tu archivo Markdown no podrás usar sintaxis
Markdown dentro de él. -->

<!-- La implementación de Markdown cambia de acuerdo al parseador. Esta
guía servirá para clarificar cuales características son universales y
cuales son específicas de cada parseador-->

<!-- Headers -->
<!-- Puedes crear headers HTML fácilmente precediendo al texto con una serie
de símbolos de números (#)-->

# Esto es un <h1>
## Esto es un <h2>
### Esto es un <h3>
#### Esto es un <h4>
##### Esto es un <h5>
###### Esto es un <h6>

<!-- Markdown también nos proveé con dos alternativas para indicar h1 y h2 -->
Esto es un h1
=============

Esto es un h2
-------------

<!-- Estilos para texto plano -->
<!-- El texto puede ser fácilmente estilizado con italicas, negritas o tachado 
usando markdown -->

*Este texto está en itálicas.*
_Al igual que este texto._

**Este texto está en negritas.**
__Al igual que este texto.__

***Este texto tiene ambos estilos.***
**_Al igual que este!_**
*__¡Y este!__*

<!-- En GitHub Flavored Markdown, el cual es usado para mostrar archivos 
Markdown en GitHub, también tenemos: -->

~~Este texto está tachado.~~

<!-- Los párrafos son una o múltiples líneas de texto adyacentes separadas por 
una o múltiples líneas en blanco-->

Este es un párrafo. Estoy escribiendo un párrafo, ¿No es divertido?

Ahora estoy en el párrafo dos.
¡Sigo en el párrafo dos!

¡Estoy en el párrafo tres!

<!-- Si en algún momento quieres insertar un break HTML <br />, puedes terminar
un párrafo con dos o más espacios y luego empieza un párrafo nuevo-->

Termino con dos espacios (selecciona esta línea completa para que los veas).  

¡Hay un <br /> arriba de mí!

<!-- Las citas de bloque son fáciles y se pueden hacer con el caracter >. -->

> Esta es una cita de bloque. Puedes
> envolver tus líneas manualmente y poner un `>` antes de cada línea o puedes dejar que tus líneas sean muy largas y que se envuelvan solas.
> No hay diferencia, siempre y cuando empiecen con `>`.

> ¿También puedes usar más de un nivel
>> de indentación?
> Esto es muy útil ¿No?

<!-- Listas -->
<!-- Las listas desordenadas se hacen usando asteriscos, símbolos de más,
 o guiones -->

* Item
* Item
* Otro item

o

+ Item
+ Item
+ Un item más

o

- Item
- Item
- El último item

<!-- Las listas ordenadas se logran con un número seguido de un punto -->

1. Item uno
2. Item dos
3. Item tres

<!-- Aunque Markdown mostrará los items correctamente en orden, esto no
es una buena idea -->

1. Item uno
1. Item dos
1. Item tres
<!-- (Esto muestra lo mismo que el ejemplo de arriba) -->

<!-- También puedes usar sub-listas -->

1. Item uno
2. Item dos
3. Item tres
    * Sub-item
    * Sub-item
4. Item cuatro

<!-- Bloques de código -->
<!-- Puedes indicar un bloque de código (usan los elementos <code>) indentando 
una línea con cuatro espacios o un tab-->

    Esto es código
    Esto también

<!-- También puedes insertar dos tabs (o cuatro espacios adicionales)
para indentar dentro del código -->

    my_array.each do |item|
        puts item
    end

<!-- Código dentro de la línea puede ser escrito usando la comilla ` -->

¡John no sabía lo que la función `go_to()` hacía!

<!-- Con GitHub Flavored Markdown, puedes usar una sintaxis especial para código -->

\`\`\`ruby <!-- quita esas comillas cuando lo hagas, deja sólo ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- aquí también, sin comillas, sólo ``` -->

<!-- El texto de arriba no necesita indentación, aparte GitHub usará
resaltará la sintaxis del lenguaje que especifiques después de ``` -->

<!-- Regla horizontal (<hr />) -->
<!-- Las reglas horizontales se agregan fácilmente con tres o más asteriscos o guiones,
con o sin espacios. -->

***
---
- - - 
****************

<!-- Ligas -->
<!-- Una de las mejores cosas de Markdown es la facilidad para hacer ligas. Pon
el texto a mostrar en corchetes [] seguidos por la URL en paréntesis () -->

[¡Haz click!](http://test.com/)

<!-- También puedes agregar el titulo de la liga usando comillas dentro de los paréntesis -->

[¡Haz click!](http://test.com/ "Liga al test.com")

<!-- También funcionan las rutas relativas. -->

[Ir a la música](/music/).

<!-- Markdown también soporta ligas con estilo de referencia -->

¡[Has click a esta liga][liga1] para más información!
[También mira esta liag][foobar] si quieres.




<!-- El título también puede estar en comillas simples o dentro de paréntesis,
también se pueden omitir completamente. Las referencias pueden estar en cualquier
lugar en tu documento y los IDs de referencia pueden ser lo que sea mientras sean únicos. -->

<!-- También hay "nombramiento implicito" el cual te permite usar el texto de la liga como id -->

[Esta][] es una liga.



<!-- Pero no se usa comúnmente. -->

<!-- Imagenes -->
<!-- Las imagenes se hacen de la misma forma que las ligas pero con un símbolo de exclamaciónal frente! -->

![Esta es una etiqueta (texto alternativo) para mi imagen](http://imgur.com/myimage.jpg "Un titulo opcional")

<!-- Y el estilo de referencia funciona como se espera -->

![Esta es una etiqueta.][myimage]



<!-- Misceláneos -->
<!-- Auto-ligas -->

<http://testwebsite.com/> equivale a
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-ligas para correos electrónicos -->

<foo@bar.com>

<!-- Escapando caracteres -->

Quiero escribir *este texto rodeado por asteriscos* pero no quiero que esté en itálicas,
así que hago esto: \*Este texto está rodeado de asteriscos\*.

<!-- Tablas -->
<!-- Las tablas sólo están disponibles en GitHub Flavored Markdown y son un poco pesadas,
pero si de verdad las quieres: -->

| Col1         | Col2     | Col3          |
| :----------- | :------: | ------------: |
| Izquierda | Centrado | Derecha |
| blah         | blah     | blah          |

<!-- o, para los mismos resultados -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Ugh esto es feo | has que | pare.

<!-- ¡El fin! -->

```

Para más información, mira el post oficial de John Gruber's [aquí](http://daringfireball.net/projects/markdown/syntax) y la gran referencia de Adam Pritchard's [aquí](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
