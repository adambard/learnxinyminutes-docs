---
language: haml
filename: learnhaml-es.haml
contributors:
  - ["Simon Neveu", "https://github.com/sneveu"]
translators:
    - ["Camilo Garrido", "http://www.twitter.com/hirohope"]
lang: es-es
---

Haml es un lenguage de marcas principalmente usado con Ruby, que de forma simple y limpia describe el HTML de cualquier documento web sin el uso de código en linea. Es una alternativa popular respecto a usar el lenguage de plantilla de Rails (.erb) y te permite embeber código Ruby en tus anotaciones.

Apunta a reducir la repetición en tus anotaciones cerrando los tags por ti, basándose en la estructura de identación de tu código. El resultado es una anotación bien estructurada, que no se repite, lógica y fácil de leer.

También puedes usar Haml en un proyecto independiente de Ruby, instalando la gema Haml en tu máquina y usando la línea de comandos para convertirlo en html.

$ haml archivo_entrada.haml archivo_salida.html


```haml
/ -------------------------------------------
/ Identación
/ -------------------------------------------

/
  Por la importancia que la identación tiene en cómo tu código es traducido,
  la identación debe ser consistente a través de todo el documento. Cualquier
  diferencia en la identación lanzará un error. Es una práctica común usar dos
  espacios, pero realmente depende de tí, mientras sea consistente.


/ -------------------------------------------
/ Comentarios
/ -------------------------------------------

/ Así es como un comentario se ve en Haml.

/
  Para escribir un comentario multilínea, identa tu código a comentar de tal forma
  que sea envuelto por por una barra.


-# Este es un comentario silencioso, significa que no será traducido al código en absoluto


/ -------------------------------------------
/ Elementos Html
/ -------------------------------------------

/ Para escribir tus tags, usa el signo de porcentaje seguido por el nombre del tag
%body
  %header
    %nav

/ Nota que no hay tags de cierre. El código anterior se traduciría como
  <body>
    <header>
      <nav></nav>
    </header>
  </body>

/ El tag div es un elemento por defecto, por lo que pueden ser escritos simplemente así
.foo

/ Para añadir contenido a un tag, añade el texto directamente después de la declaración
%h1 Headline copy

/ Para escribir contenido multilínea, anídalo.
%p
  Esto es mucho contenido que podríamos dividirlo en dos
  líneas separadas.

/
  Puedes escapar html usando el signo ampersand y el signo igual ( &= ).
  Esto convierte carácteres sensibles en html a su equivalente codificado en html.
  Por ejemplo

%p
  &= "Sí & si"

/ se traduciría en 'Sí &amp; si'

/ Puedes desescapar html usando un signo de exclamación e igual ( != )
%p
  != "Así es como se escribe un tag párrafo <p></p>"

/ se traduciría como 'Así es como se escribe un tag párrafo <p></p>'

/ Clases CSS puedes ser añadidas a tus tags, ya sea encadenando .nombres-de-clases al tag
%div.foo.bar

/ o como parte de un hash Ruby
%div{:class => 'foo bar'}

/ Atributos para cualquier tag pueden ser añadidos en el hash
%a{:href => '#', :class => 'bar', :title => 'Bar'}

/ Para atributos booleanos asigna el valor verdadero 'true'
%input{:selected => true}

/ Para escribir atributos de datos, usa la llave :dato  con su valor como otro hash
%div{:data => {:attribute => 'foo'}}


/ -------------------------------------------
/ Insertando Ruby
/ -------------------------------------------

/
  Para producir un valor Ruby como contenido de un tag, usa un signo igual
  seguido por código Ruby

%h1= libro.nombre

%p
  = libro.autor
  = libro.editor


/ Para correr un poco de código Ruby sin traducirlo en html, usa un guión
- libros = ['libro 1', 'libro 2', 'libro 3']

/ Esto te permite hacer todo tipo de cosas asombrosas, como bloques de Ruby
- libros.shuffle.each_with_index do |libro, indice|
  %h1= libro

  if libro do
    %p Esto es un libro

/
  Nuevamente, no hay necesidad de añadir los tags de cerrado en el código, ni siquiera para Ruby
  La identación se encargará de ello por tí.


/ -------------------------------------------
/ Ruby en linea / Interpolación de Ruby
/ -------------------------------------------

/ Incluye una variable Ruby en una línea de texto plano usando #{}
%p Tu juego con puntaje más alto es #{mejor_juego}


/ -------------------------------------------
/ Filtros
/ -------------------------------------------

/
  Usa un signo dos puntos para definir filtros Haml, un ejemplo de filtro que
  puedes usar es :javascript, el cual puede ser usado para escribir javascript en línea.

:javascript
  console.log('Este es un <script> en linea');

```

## Recusros adicionales

- [¿Qué es HAML? (en inglés)](http://haml.info/) - Una buena introducción que hace mejor el trabajo de explicar los beneficios de usar haml.
- [Documentación Oficial (en inglés)](http://haml.info/docs/yardoc/file.REFERENCE.html) - Si deseas ir un poco más profundo.
