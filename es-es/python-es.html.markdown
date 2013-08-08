---
language: python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["Raúl Ascencio", "http://rscnt.github.io"]
filename: learnpython.py
lang: es-es
---

Python fue creado por Guido Van Rossum a inicios de los años 90. Se ha
convertido en uno de los lenguajes de programación mas populares en existencia.
Me enamoré de Python debido a su claridad sintáctica. Básicamente es
pseudo-código ejecutable.

Cualquier recomendación sera enormemente agradecida!, puedes contactarme en
[@louiedinh](http://twitter.com/louiedinh) o louiedinh [at] [google's email
service].

Nota: Este articulo aplica específicamente para Python 2.7, pero podría ser
aplicable a cualquier versión dentro de la rama Python 2.x.

> ```python
# Los comentarios de una linea comienzan con un signo numeral.
""" Para escribir cadenas de multiples lineas, se utilizan tres comillas (")
    en su apertura y cierre, son  mayoritariamente usadas como comentarios.
"""

####################################################
## 1. Tipos de datos primitivos y Operadores
####################################################

# Números
3 #=> 3

# Simple definición de operaciones matemáticas.
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# La división puede ser un poco complicada. Si es una división de enteros, se
# aproxima el resultado automáticamente.
5 / 2 #=> 2

# Para "arreglar" la división necesitamos aprender a usar "Floats"
2.0     # Esto es un "float"
11.0 / 4.0 #=> 2.75 mucho mejor c:

# Utiliza los paréntesis para definir la precedencia en las operaciones.
(1 + 3) * 2 #=> 8

# Los valores "Boolean" son primitivos
True
False

# Niega utilizando "not"
not True #=> False
not False #=> True

# Igualdad es "=="
1 == 1 #=> True
2 == 1 #=> False

# Desigualdad es "!="
1 != 1 #=> False
2 != 1 #=> True

# Mas comparaciones.
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# Las comparaciones pueden conjugarse.
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Las cadenas puede ser creadas con " o '
"Esto es una cadena."
'Esto tambien es una cadena.'

# Uniendo cadenas.
"Hola " + "mundo!" #=> "Hola Mundo!"

# Una cadena puede ser creada como una lista de caracteres.
"Esto es una cadena."[0] #=> 'E'

# % puede ser usado para formatear cadenas, por ejemplo:
"%s puede ser %s" % ("las cadenas", "interpoladas")

# Una nueva manera de formatear cadenas es el método de formato.
# Método es el preferido.
"{0} pueden ser {1}".format("las cadenas", "formateadas")
# También puedes utilizar palabras claves, si no deseas contar.
"{nombre} quiere comer {food}".format(name="Juan", food="pupusas")

# None es un objeto.
None #=> None

# No utilices el símbolo de igualdad '=='  para comparar objetos con None.
# en cambio debes utilizar el operador 'is'.
"etc" is None #=> False
None is None  #=> True

# El operador 'is' identifica la identidad del objeto. Es útil cuando se
# trata de valores primitivos, pero es de mayor utilidad cuando se utilizan
# objetos.

# None, 0, y cadenas/listas vacías son evaluadas a False.
# Todos los demás valores son True
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Variables y Colecciones
####################################################

# Imprimir es sencillo.
print "Hola mundo!"
print("Deberia de pensarse en algo diferente al Hola Mundo!")

# No existe necesidad de declarar variables antes de asignarlas.
una_variable = 5    # Por convencion (PEP8) se utiliza el guion bajo.
una_variable #=> 5

# Intentar acceder a una variable no asignada genera una excepción.
# Mira la sección "Control de Flujos" para aprender mas sobre el control de
# excepciones.
una_variable_sin_asignar  # Devuelve la excepcion NameError.

# El operador "if" puede ser usado como expresión.
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

# Las listas almacenan secuencias.
lista = []
# Puedes predefinir una lista al crearla.
otra_lista = [4, 5, 6]

# Añade elementos al final de una lista con la función "append"
lista.append(1)    #lista es ahora [1]
lista.append(2)    #lista es ahora [1, 2]
lista.append(4)    #lista es ahora [1, 2, 4]
lista.append(3)    #lista es ahora [1, 2, 4, 3]
# Remueve el ultimo elemento con "pop"
lista.pop()        #=> 3 y lista es ahora [1, 2, 4]
# Añadiéndolo de nuevo.
lista.append(3)    # lista es ahora [1, 2, 4, 3] de nuevo.

# Accede a una lista como lo harías con un arreglo.
lista[0] #=> 1
# Muestra el ultimo elemento.
lista[-1] #=> 3

# Buscar un elemento fuera de los limites devuelve la excepción IndexError
lista[4] # Retorna un IndexError

# Puedes acceder a un rango de elementos con la sintaxis de corte.
# (Representa un intervalo abierto/cerrado matemáticamente)
lista[1:3] #=> [2, 4]
# Omite el inicio.
lista[2:] #=> [4, 3]
# Omite el final.
lista[:3] #=> [1, 2, 4]

# Eliminando elementos de una lista arbitrariamente con del.
del lista[2] # lista es ahora [1, 2, 3]

# Puedes agregar listas,
lista + otra_lista #=> [1, 2, 3, 4, 5, 6] - Nota: lista y otra_lista no se modifican.

# Es posible concatenar las listas haciendo uso de la función extend.
lista.extend(otra_lista) #  Ahora lista es [1, 2, 3, 4, 5, 6]

# Revisa si un elemento existe en una lista.
1 in li #=> True

# Examina la longitud con len
len(li) #=> 6


# Tuplas, como listas pero inmutables.
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # Devuelve la excepcion TypeError

# Puedes utilizar las acciones de listas con las tuplas.
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Puedes desempacar las tuplas (o listas) en variables.
a, b, c = (1, 2, 3)     # a es igual a 1, b es 2 y c es ahora 3
# Las tuplas son creadas por defecto si no colocas los paréntesis.
d, e, f = 4, 5, 6
# Observa como es de fácil intercambiar dos valores.
e, d = d, e     # d es ahora 5 and e equivale a 4


# Los diccionarios almacenan mapas.
diccionario_vacio = {}
# Los diccionarios se pueden llenar al definirse.
diccionario_lleno = {"uno": 1, "dos": 2, "tres": 3}

# Accede a los valores con []
diccionario_lleno["uno"] #=> 1

# Obten todas las llaves como una lista.
diccionario_lleno.keys() #=> ["tres", "dos", "uno"]
# Nota - El orden de las llaves no esta garantizado.
# Los resultados podrian variar.

# Obten todos los valores como una lista.
diccionario_lleno.values() #=> [3, 2, 1]
# Nota - Igual que arriba, no se garantiza el orden.

# Revisa la existencia de una llave en un diccionario con in.
"uno" in diccionario_lleno #=> True
1 in diccionario_lleno #=> False

# Buscar por una llave no existente retorna una excepción KeyError.
diccionario_lleno["cuatro"] # KeyError

# Usa el método get para evadir la excepción KeyError.
diccionario_lleno.get("uno") #=> 1
diccionario_lleno.get("cuatro") #=> None
# El método get permite asignar un valor por defecto cuando la llave no existe.
diccionario_lleno.get("uno", 4) #=> 1
diccionario_lleno.get("cuatro", 4) #=> 4

# El método setdefault es una manera segura de agregar nuevas elementos a un
# diccionario.
diccionario_lleno.setdefault("cinco", 5) #diccionario_lleno["cinco"] es igualado a 5
diccionario_lleno.setdefault("cinco", 6) #diccionario_lleno["cinco"] mantiene el valor de 5


# Los Sets almacenan... Bueno, sets.
set_vacio = set()
# Inicializa un set con un montón de valores.
set_x = set([1,2,2,3,4]) # algun_set es ahora set([1,2,3,4])

# Desde Python 2.7, los {} puede ser utilizados para declarar un set.
set_lleno = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Agrega mas elementos a un set.
set_lleno.add(5) # set_lleno es ahora {1, 2, 3, 4, 5}

# Realiza intersecciones en los sets con &
otro_set = {3, 4, 5, 6}
set_lleno & otro_set #=> {3, 4, 5}

# Las uniones se realizan con |
set_lleno | otro_set #=> {1, 2, 3, 4, 5, 6}

# Obtén los elementos diferentes con -
{1,2,3,4} - {2,3,5} #=> {1, 4}

# Revisa la existencia en un set con in
2 in set_lleno #=> True
10 in set_lleno #=> False


####################################################
## 3. Control de Flujos.
####################################################

# Definamos una_variable.
una_variable = 5

# He aquí una sentencia if. La acentuación es sumamente importante en Python!

if una_variale > 10:
    print "una_variale es extremandamente mayor a 10."
elif una_variale < 10:    # La clausula elif es opcional.
    print "una_variale es menor a 10."
else:           # else, tambien es opcional.
    print "una_variale es igual a 10."


"""
for itera sobre listas.
imprime:
    perro es un mamifero
    gato es un mamifero
    raton es un mamifero
"""
for animal in ["perro", "gato", "raton"]:
    # Puedes usar % para interpolar las cadenas.
    print "%s es un mamifero" % animal

"""
`range(numero)` retorna una lista de numeros.
desde 0 hasta el numero pasado.
imprime:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
while itera hasta que la condicion no se cumpla.
imprime:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # sinonimo de x = x + 1

# Maneja las excepciones con un bloque try/except

# Fue implementado en Python 2.6, por lo que funciona en todas las versiones posteriores a esta.
try:
    # Utiliza raise para llamar una excepcion.
    raise IndexError("Esta es un error de indices")
except IndexError as e:
    pass    # pass, no realiza ninguna accion. Usualmente se trata la
            # recuperacion en esta aqui.


####################################################
## 4. Funciones
####################################################

# Utiliza def para crear una función.
def add(x, y):
    print "x es %s,  y es %s" % (x, y)
    return x + y    # retorna los valores.

# Llamando funciones con parámetros.
add(5, 6) #=> imprime "x es 5, y es 6" y retorna 11

# También es posible indicar los parámetros por nombre.
add(y=6, x=5)   # Los parametros pasados por nombre, no necesitan un orden.

# Puedes definir funciones que tomen un numero variable de argumentos,
# mediante su posicionamiento
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# También puedes definir argumentos que tomen un numero variables de argumentos,
# mediante nombres.
def keyword_args(**kwargs):
    return kwargs

# Invoquemosle para ver que ocurre
keyword_args(pie="grande", lago="ness") #=> {"pie": "grande", "lago": "ness"}

# Puedes realizar ambos de una sola vez.
def todos_los_argumentos(*args, **kwargs):
    print args
    print kwargs
"""
todos_los_argumentos(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Cuando se llaman funciones, puedes hacer lo opuesto de varargs/kwargs!
# Usa * para expandir tuplas y usa ** para expandir kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
todos_los_args(*args) # equivalente a foo(1, 2, 3, 4)
todos_los_args(**kwargs) # equivalente a foo(a=3, b=4)
todos_los_args(*args, **kwargs) # equivalente a foo(1, 2, 3, 4, a=3, b=4)

# Python posee funciones de primera clase
def crear_agregador(x):
    def agregador(y):
        return x + y
    return agregador

agregador_10 = crear_agregador(10)
agregador_10(3) #=> 13

# Existen también funciones anonimas
(lambda x: x > 2)(3) #=> True

# Existen funciones integradas de mayor orden.
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# Puedes utilizar la comprensión de listas para obtener mapas y filtros.
[agregador_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. Clases
####################################################

# Usamos subclases de un objecto para obtener una clase.
class Humano(object):

     # Un atributo de clase. Es compartido por todas las instancias de la clase.
    especies = "H. sapiens"

    # Inicializador.
    def __init__(self, nombre):
        # Asigna el argumento nombre al atributo nombre de la instancia.
        self.nombre = nombre

    # Un metodo de instancia. Todos los metodos toman self, como el primero
    # argumento.
    def dice(self, msg):
       return "%s: %s" % (self.nombre, msg)

    # Un metodo de clase es compartido por todas las instancias.
    # Estos son llamados con la clase invocada como primer argumento.
    @classmethod
    def obtener_especies(cls):
        return cls.especies

    # Un metodo estatico es invocado sin una clase o referencia de instacia.
    @staticmethod
    def grunt():
        return "*grunt*"


# Instanciando una clase.
ian = Humano(nombre="Ian")
print ian.dice("hola")     # imprime "Ian: hola"

joel = Humano("Joel")
print joel.dice("que dice?")  #  imprime "Joel: que dice?"

# Invocando un método de clase.
ian.obtener_especies() #=> "H. sapiens"

# Cambien el atributo compartido
Humano.especies = "H. neandentales"
ian.obtener_especies() #=> "H. neandentales"
joel.obtener_especies() #=> "H. neandentales"

# Invocando un método estático.
Humano.grunt() #=> "*grunt*"


####################################################
## 6. Módulos
####################################################

# Importando módulos.
import math
print math.sqrt(16) #=> 4

# Puedes obtener solamente funciones especificas de un modulo.
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# También puedes importar todas las funciones de un modulo.
# Advertencia: Esto No Es Recomendado! (referirse a PEP8)
from math import *

# Puedes crear un alias del nombre de un modulo.
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Los módulos no son mas que archivos ordinarios de Python. Puedes escribir
# tus propios módulos e importarlos. El nombre del modulo es el mismo que el
# nombre del archivo.

# Para descubrir que funciones y atributos posee un modulo.
import math
dir(math) #[..., 'sqrt', ...]


```

## Listo para mas?

### Contenido gratuito:

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### Dead Tree

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

