---
language: xml
filename: learnxml.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Daniel Zendejas", "https://github.com/DanielZendejas"]
lang: es-es
---
XML es un lenguaje diseñado para guardar y transportar datos

A diferencia de HTML, XML no especifica cómo desplegar la información,
sólo la guarda.

* Sintaxis XML

```xml
<!-- Los comentarios en XML son de esta forma -->

<?xml version="1.0" encoding="UTF-8"?>
<tiendaDeLibros>
  <libro categoria="COCINA">
    <titulo lenguaje="en">Everyday Italian</titulo>
    <autor>Giada De Laurentiis</autor>
    <anio>2005</anio>
    <precio>30.00</precio>
  </libro>
  <libro categoria="INFANTES">
    <titulo lenguaje="en">Harry Potter</titulo>
    <autor>J K. Rowling</autor>
    <anio>2005</anio>
    <precio>29.99</precio>
  </libro>
  <libro categoria="WEB">
    <titulo lenguaje="en">Learning XML</titulo>
    <autor>Erik T. Ray</autor>
    <anio>2003</anio>
    <precio>39.95</precio>
  </libro>
</tiendaDeLibros>

<!-- Este es un archivo típico de XML.
  Empieza con una declaración de metadatos (opcional).
  
  XML usa una estructura de árbol. El nodo raíz es 'tiendaDeLibros', el cual
  tiene tres nodos hijos, todos llamados 'libros'. 
  Esos nodos tienen más nodos hijos, y así continúa... 
  
  Los nodos son creados usando tags que abren y cierran, y los hijos 
  son sólo nodos entre estas tags.-->


<!-- XML guarda dos tipos de datos:
  1 - Atributos -> Son los metadatos de un nodo.
	  Usualmente el parseador XML usa esta información para guardar los datos
	  apropiadamente. Aparecen con el formato (nombre="valor") dentro de la
	  tag que abre.
  2 - Elementos -> Ese es el dato puro.
  	  Eso es lo que el parseador recuperará del archivo XML.
  	  Los elementos aparecen entre las tags que abren y cierran.-->
      
  
<!-- Debajo, un elemento con dos atributos. -->
<archivo tipo="gif" id="4293">computer.gif</archivo>


```

* Documentos con buen formato x Validación

Un documento XML está bien formado cuando es sintácticamente correcto.
Aún esto, es posible inyectar más restricciones en el documento,
usando definiciones de documento, así como DTD o XML Schemas.

Un documento XML que sigue a una definición de documento (un esquema) es
válida. 

Con esta herramienta puedes validar datos XML fuera de la aplicación

```xml

<!-- Debajo puedes encontrar una versión simplificada del documento 
  tiendaDeLibros en adición a la definición DTD.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "tiendaDeLibros.dtd">
<tiendaDeLibros>
  <libro categoriq="COCINA">
    <titulo>Everyday Italian</titulo>
    <precio>30.00</precio>
  </libro>
</tiendaDeLibros>

<!-- El DTD de este documento podría verse algo así:-->

<!DOCTYPE note
[
<!ELEMENT tiendaDeLibros (libro+)>
<!ELEMENT libro (titulo,precio)>
<!ATTLIST libro categoria CDATA "Literatura">
<!ELEMENT titulo (#PCDATA)>
<!ELEMENT precio (#PCDATA)>
]>

<!--El DTD empieza con una declaración.
	Después el nodo raíz es declarado, requiriendo 1 o más nodos 'libro'
	Cada 'libro' debe contener exactamente un 'titulo' y un 'precio' y
	un atributo llamado 'categoria', con "Literatura" como su valor 
	default.
	Los nodos 'titulo' y 'precio' contienen datos de caracteres
	parseados.
	
	El DTD puede ser declarado dentro del XML mismo.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT tiendaDeLibros (libro+)>
<!ELEMENT libro (titulo,precio)>
<!ATTLIST libro categoria CDATA "Literatura">
<!ELEMENT titulo (#PCDATA)>
<!ELEMENT precio (#PCDATA)>
]>
<tiendaDeLibros>
  <libro categoriq="COCINA">
    <titulo>Everyday Italian</titulo>
    <precio>30.00</precio>
  </libro>
</tiendaDeLibros>
```
