---
language: yaml
lang: es-es
filename: learnyaml-es.yaml
contributors:
  - ["Adam Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Daniel Zendejas","https://github.com/DanielZendejas"]
---
Tutorial de YAML en español.

YAML es un lenguaje de serialización de datos diseñado para ser
leído y escrito por humanos.

Basa su funcionalidad en JSON, con la adición de líneas nuevas
e indentación inspirada en Python. A diferencia de Python, YAML
no permite tabs literales.

```yaml
# Los comentarios en YAML se ven así.

###################
# TIPOS ESCALARES #
###################

# Nuestro objeto raíz (el cual es el mismo a lo largo de todo el
# documento) será un mapa, equivalente a un diccionario, hash,
# u objeto en otros lenguajes.

llave: valor
otra_llave: Otro valor
un_valor_numerico: 100
notacion_cientifica: 1e+12
booleano: true
valor_nulo: null
llave con espacios: valor
# Nótese que los strings no deben estar entre comillas, aunqué también es válido.
llave: "Un string, entre comillas."
"Las llaves tambien pueden estar entre comillas.": "valor entre comillas"

# Los strings de líneas múltiples pueden ser escritos 
# como un 'bloque literal' (usando pipes |)
# o como un 'bloque doblado' (usando >)

bloque_literal: |
	Este bloque completo de texto será preservado como el valor de la llave
	'bloque_literal', incluyendo los saltos de línea.
	
	Se continúa guardando la literal hasta que se cese la indentación. 
		Cualquier línea que tenga más indentación, mantendrá los espacios dados
		(por ejemplo, estas líneas se guardarán con cuatro espacios)

nloque_doblado: >
	De la misma forma que el valor de 'bloque_literal', todas estas
	líneas se guardarán como una sola literal, pero en esta ocasión todos los
	saltos de línea serán reemplazados por espacio.

	Las líneas en blanco, como la anterior, son convertidos a un salto de línea.

        Las líneas con mayor indentación guardan sus saltos de línea.
        Esta literal ocuparán dos líneas.

########################
# TIPOS DE COLECCIONES #
########################

# La indentación se usa para anidar.
un_mapa_indentado:
    llave: valor
    otra_llave: otro valor
    otro_mapa_indentado:
        llave_interna: valor_interno

# Las llaves de los mapas no deben ser strings necesariamente
0.25: una llave numérica

# Las llaves también pueden ser objetos de multi línea, usando ? para indicar
# el inicio de una llave
? |
    Esto es una llave
    que tiene múltiples líneas
: y este es su valor

# YAML tambien permite colecciones como llaves, pero muchos lenguajes de 
# programación se quejarán.

# Las secuencias (equivalentes a listas o arreglos) se ven así:
una_secuencia:
    - Item 1
    - Item 2
    - 0.5 # las secuencias pueden tener distintos tipos en su contenido.
    - Item 4
    - llave: valor
      otra_llave: otro_valor
    -
        - Esta es una secuencia
        - ...dentro de otra secuencia

# Dado que todo JSON está incluído dentro de YAML, también puedes escribir 
# mapas con la sintaxis de JSON y secuencias: 
mapa_de_json: {"llave": "valor"}
secuencia_de_json: [3, 2, 1, "despegue"]

##################################
# CARACTERÍSTICAS EXTRAS DE YAML #
##################################

# YAML tiene funciones útiles llamadas 'anchors' (anclas), que te permiten
# duplicar fácilmente contenido a lo largo de tu documento. En el ejemplo
# a continuación, ambas llaves tendrán el mismo valor:
contenido_anclado: &nombre_del_ancla Este string será el valor de las llaves
otra_ancla: *nombre_del_ancla

# YAML también tiene tags, que puedes usar para declarar tipos explícitamente.
string_explícito: !!str 0.5
# Algunos parseadores implementar tags específicas del lenguaje, como el 
# que se muestra a continuación, encargado de manejar números complejos en
# Python:
numero_complejo_python: !!python/complex 1+2j

########################
# TIPOS EXTRAS EN YAML #
########################

# Stirngs y números no son los únicos escalares que YAML puede entener.
# YAML también puede parsear fechas en formato ISO .
fechaHora: 2001-12-15T02:59:43.1Z
fechaHora_con_espacios: 2001-12-14 21:59:43.10 -5
fecha: 2002-12-14

# La tag !!binary indica que un string es, en realidad, un blob
# representado en base-64.
archivo_gif: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML también tiene un tipo set, que se ve de la siguiente forma:
set:
    ? item1
    ? item2
    ? item3

# Al igual que Python, los sets sólo son mapas con valores nulos.
# El ejemplo de arriba es equivalente a: 
set2:
    item1: null
    item2: null
    item3: null
```
