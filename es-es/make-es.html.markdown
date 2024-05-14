---
category: tool
tool: make
filename: Makefile
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Stephan Fuhrmann", "https://github.com/sfuhrm"]
translators:
    - ["Andrés Perdomo", "https://github.com/andres7293"]
lang: es-es
---

Un archivo Makefile define un conjunto de reglas para crear un objetivo (o
varios objetivos). Su propósito es hacer la mínima cantidad de trabajo necesaria
para actualizar un objetivo a la versión más reciente de la fuente. Escrito
famosamente en un fin de semana por Stuart Feldman en 1976, todavía se utiliza
ampliamente (especialmente en Unix y Linux) a pesar de muchos competidores y
críticas.

Existen muchas variedades de Make en existencia, no obstante, este artículo
asume que estamos utilizando GNU Make, que es el estándar en Linux.

```make
# Los comentarios se pueden escribir de esta forma.

# El fichero debe tener el nombre de Makefile y luego puede ser ejecutado
# como `make <objetivo>`.
# De lo contrario, se utiliza `make -f "nombre_archivo" <objetivo>`

# Advertencia: ¡solo use TABULACIONES para la identación en Makefiles, nunca
# espacios!

#-----------------------------------------------------------------------
# Fundamentos
#-----------------------------------------------------------------------

# Las reglas tienen el formato
# objetivo: <prerrequisito>
# donde prerrequisito es opcional.

# Una regla - esta regla solamente se ejecutará si file0.txt no existe.
file0.txt:
	echo "foo" > file0.txt
	# Incluso los comandos en esta sección de 'receta' se pasan a la shell.
	# Prueba `make file0.txt` o  simplemente 'make' - La primera regla es la
	# predeterminada.

# Esta regla se ejecutará solo si file0.txt es más reciente que file1.txt.
file1.txt: file0.txt
	cat file0.txt > file1.txt
	# Use las mismas reglas de comillas que en la shell.
	@cat file0.txt >> file1.txt
	# @ evita que el comando se muestre en stdout.
	-@echo 'hello'
	# - Quiere decir que make continuará en caso de error.
	# Pruebe 'make file1.txt` en la línea de comandos.

# Una regla puede tener múltiples objetivos y múltiples prerrequisitos
file2.txt file3.txt: file0.txt file1.txt
	touch file2.txt
	touch file3.txt

# Make se quejará de múltiples recetas para la misma regla. Sin embargo,
# las reglas vacías no cuentan y se pueden utilizar para agregar nuevas
# dependencias

#-----------------------------------------------------------------------
# Objetivos ficticios (Phony Targets)
#-----------------------------------------------------------------------

# Un objetivo ficticio (phony target). Cualquier objetivo que no sea un archivo.
# Nunca estará actualizado, por lo que make siempre tratará de ejecutarlo.
all: make process

# Podemos declarar cosas sin orden.
maker:
	touch ex0.txt ex1.txt

# Se puede evitar que las reglas ficticias (phony) se rompan cuando un archivo
# real tiene el mismo nombre usando:
.PHONY: all maker process
# Esto es un objetivo especial. Hay varios otros.

# Una regla con una dependencia en un objetivo ficticio (phony target)
# se ejecutara siempre:
ex0.txt ex1.txt: maker

# Los objetivos ficticios (phony target) más comunes son: 
# all make clean install...

#-----------------------------------------------------------------------
# Variables automáticas y Wildcards
#-----------------------------------------------------------------------

process: file*.txt	# usa un wildcard para coincidir con los nombres de archivos.
	@echo $^	# $^ es una variable que contiene una lista de todos los prerrequisitos
	@echo $@	# imprime el nombre del objetivo
	#(para reglas con múltiples objetivos, $@ es el que hizo que se ejecutara la regla)
	@echo $<	#  el primer prerrequisito listado
	@echo $?	# solo las dependencias que están desactualizadas
	@echo $+	# todas las dependencias incluyendo las duplicadas (a diferencia de lo normal)
	#@echo $|	# solo los 'prerrequisitos solicitados'

# Incluso si dividimos las definiciones de las dependencias, de las reglas, $^
# las encontrará
process: ex1.txt file0.txt
# ext1.xt se encontrará pero file0.txt se duplicará.

#-----------------------------------------------------------------------
# Patrones
#-----------------------------------------------------------------------

# Se puede instruir a make sobre como convertir ciertos tipos de archivos en
# otros archivos.

%.png: %.svg
	inkscape --export-png $^

# Las reglas de patrones solo harán algo si make decide crear el objetivo.

# Los directorios no suelen tenerse en cuenta al coincidir con reglas de
# patrones.
# Pero make intentará usar la regla más apropiada disponible.

small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

# make usará la última versión de una regla de patrón que encuentre.
%.png: %.svg
	@echo esta regla es elegida

# Sin embargo make usará la primera regla de patrón que pueda construir el
# objetivo.
%.png: %.ps
	@echo esta regla no es elegida si *.svg y *.ps están ambas presentes

# Make ya tiene algunas reglas de patrón integradas.
# Por ejemplo, sabe cómo convertir archivos *.c en archivos *.o.

# En makefiles antiguos se solían utilizar las reglas de sufijo en lugar de las
# reglas de patrón
.png.ps:
	@echo esta regla es similar a una regla de patrón.

# Instruye a make sobre una regla de sufijo
.SUFFIXES: .png

#-----------------------------------------------------------------------
# Variables
#-----------------------------------------------------------------------
# también conocidas como macros.

# Las variables son básicamente de tipo cadena (string)

name = Ted
name2="Sarah"

echo:
	@echo $(name)
	@echo ${name2}
	@echo $name    # Esto no funcionará, se tratará como $(n)name.
	@echo $(name3) # Variables desconocidas se tratarán como cadenas vacías.

# Hay 4 lugares donde se pueden definir variables.
# En orden de prioridad de mayor a menor:
# 1: argumentos de línea de comando.
# 2: Makefile.
# 3: variables de entorno de la shell - make las importa automáticamente.
# 4: make tiene algunas variables predefinidas.

name4 ?= Jean
# Solo establece la variable si la variable de entorno no está aún definida.

override name5 = David
# Detiene que los argumentos de línea de comandos modifiquen esta variable.

name4 +=grey
# Añade valores a la variable (incluye un espacio).

# Valores de variables específicos de patrones (Extension de GNU).
echo: name2 = Sara # Verdadero dentro de la regla coincidente
	# y también dentro de sus dependencias recursivas rehechas
	# (¡excepto que puede romperse cuando el grafo se complica demasiado!)

# Algunas variables son definidas automáticamente por make.
echo_inbuilt:
	echo $(CC)
	echo ${CXX}
	echo $(FC)
	echo ${CFLAGS}
	echo $(CPPFLAGS)
	echo ${CXXFLAGS}
	echo $(LDFLAGS)
	echo ${LDLIBS}

#-----------------------------------------------------------------------
# Variables 2
#-----------------------------------------------------------------------

# El primer tipo de variables se evalúan cada vez que se usan.
# Esto puede ser costoso, por lo que existe un segundo tipo de variable que se
# evalúa solo una vez. (Esta es una extensión de GNU make)

var := hello
var2 ::=  $(var) hello
#:= y ::= son equivalentes

# Estas variables se evalúan de manera procedimental (en el orden en que
# aparecen), ¡rompiendo así con el resto del lenguaje!

# Esto no funciona
var3 ::= $(var4) and good luck
var4 ::= good night

#-----------------------------------------------------------------------
# Funciones
#-----------------------------------------------------------------------

# make tiene muchas funciones disponibles.

sourcefiles = $(wildcard *.c */*.c)
objectfiles = $(patsubst %.c,%.o,$(sourcefiles))

# El formato es $(func arg0,arg1,arg2...)

# Algunos ejemplos
ls:	* src/*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# Directrices (Directives)
#-----------------------------------------------------------------------

# Incluye otros makefiles, útil para código de plataformas específicas
include foo.mk

sport = tennis
# Compilación condicional
report:
ifeq ($(sport),tennis)
	@echo 'game, set, match'
else
	@echo "They think it's all over; it is now"
endif

# También existe ifneq, ifdef, ifndef

foo = true

ifdef $(foo)
bar = 'hello'
endif
```

### Más recursos (en inglés)

+ [GNU Make documentation](https://www.gnu.org/software/make/manual/)
+ [Software carpentry tutorial](http://swcarpentry.github.io/make-novice/)
