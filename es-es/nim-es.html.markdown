---
language: Nim
category: language
filename: learnNim.nim
contributors:
    - ["Jason J. Ayala P.", "http://JasonAyala.com"]
    - ["Dennis Felsing", "http://felsin9.de/nnis/"]
translators:
    - ["Juan Carlos", "https://github.com/juancarlospaco"]
lang: es-es
---

Nim es un lenguaje de programación imperativo de tipado estático,
que le da al programador mucho poder sin comprometer la eficiencia.

Nim es eficiente, expresivo y elegante.

```nim
# Los comentatios de una linea comienzan con un #
## La Documentacion comienza con un ##

#[
  Este es un comentario multi-linea.
  En Nim, los comentarios multi-linea pueden estar anidados, empezando con un #[
  ... y terminando con un ]#
]#

var                      # Declarar (y asignar) variables,
  letra: char = 'n'      # con o sin anotaciones de Tipado.
  lenguaje = "N" & "im"  # Concatenar strings con &
  nLongitud: int = len(lenguaje)
  flotante: float
  booleano: bool = false

let               # Usa let para declarar y asignar variables.
  piernas = 400   # piernas es immutable por que usa let.
  brazos = 2_000  # _ es ignorado y es util para numeros largos.
  acercaDePi = 3.15

const           # Las Constantes som computadas en tiempo de compilacion. This provides
  debug = true  # Esto provee performance y es util para expresiones en tiempo de compilacion.
  compilarCodigoMalo = false

when compilarCodigoMalo:           # `when` es un `if` en tiempo de compilacion.
  piernas = piernas + 1            # Esto es un error y no compilara.
  const entrada = readLine(stdin)  # Los valores de las const deben ser conocidos en tiempo de compilacion.

discard 1 > 2 # Nota: El compilador se quejara si el resultado de una expresion
              # no es usado. `discard` puede evitar esto.


#
# Estructuras de Datos
#

# Tuplas

var
  chicos: tuple[nombre: string, edad: int]     # Tuplas tienen *ambos* nombres de campo
  hoy: tuple[pronostico: string, temp: float]  # *y* orden.

chicos = (nombre: "Pepe", edad: 2)  # Asignar todo de una sola vez es posible con el literal ()
hoy.pronostico = "Nublado"          # o campos individuales.
hoy.temp = 22.1

# Secuencias

var bebidas: seq[string]

bebidas = @["Agua", "Jugo", "Chocolatada"] # @[V1,..,Vn] es la secuencia literal

bebidas.add("Leche")

if "Leche" in bebidas:
  echo "Tenemos Leche y ", bebidas.len - 1, " otras bebidas."

let miBebida = bebidas[2]

#
# Definiendo Tipos
#

# Definiendo tus propios tipos pone al compilador a trabajar por ti.
# Es lo que hace el tipado estatico poderoso y util.

type
  Nombre = string # Un alias de tipo te da un nuevo tipo que es intercambiable
  Edad = int      # con el tipo viejo original pero es mas descriptivo.
  Persona = tuple[nombre: Nombre, edad: Edad] # Puedes definir estructuras de datos tambien.
  OtraSintaxis = tuple
    campoUno: string
    campoDos: int

var
  juan: Persona = (nombre: "Juan", edad: 17)
  nuevaEdad = 18  # Deberia ser mejor usar Age en lugar de int.

juan.edad = nuevaEdad  # Pero igualmente funciona por que int y Age son sinonimos.

type
  Efectivo = distinct int  # `distinct` hace que un nuevo tipo sea intencionalmente incompatible con su
  Desc = distinct string   # tipo base.

var
  dinero: Efectivo = 100.Efectivo  # `.Efectivo` convierte el int a nuetro tipo Efectivo.
  descripcion: Desc = "Interesting".Desc

when compilarCodigoMalo:
  juan.edad = dinero         # Error! edad es de tipo int y dinero es Cash.
  john.nombre = descripcion  # El compilador dice: "No!"

#
# Mas Tipos y Estructuras de Datos
#

# Enumeraciones permiten un tipo tener uno de un numero limitado de valores.

type
  Color = enum cRojo, cAzul, cVerde
  Direccion = enum # Formato alternativo de como escribir el Enum.
    dNorte
    dOeste
    dEste
    dSur

var
  orientacion = dNorte # `orientacion` es de tipo Direction, con el valor `dNorte`.
  pixel = cVerde # `pixel` es de tipo Color, con el vlor `cVerde`.

discard dNorte > dEste  # Enums son usualmente un tipo "ordinal".

# Subranges especifican un rango valido limitado.

type CarasDeUnDado = range[1..20]  # Solamente un int desde 1 hasta 20 es un valor valido.
var tirar_dado: CarasDeUnDado = 13

when compilarCodigoMalo:
  tirar_dado = 23 # Error!

# Arrays

type
  ContadorDado = array[CarasDeUnDado, int]  # Los Array tienen longitud fija y
  NombresDeDir = array[Direccion, string]   # son indexados por cualquier tipo ordinal.
  Verdades = array[42..44, bool]

var
  contador: ContadorDado
  direcciones: NombresDeDir
  posibilidades: Verdades

posibilidades = [false, false, false] # Los Array literal son creados con [V1,..,Vn]
posibilidades[42] = true

direcciones[dNorte] = "Ahh. El norte recuerda!."
direcciones[dOeste] = "No, no podes ir ahi."

tirar_dado = 13
contador[tirar_dado] += 1
contador[tirar_dado] += 1

var otroArray = ["El Indice por defecto", "comienza en", "0"]

# Mas estructuras de datos estan disponibles, incluyendo tablas, sets, listas, colas,
# y arboles crit bit.
# http://nim-lang.org/docs/lib.html#collections-and-algorithms

#
# IO y Control de flujo
#

# `case`, `readLine()`

echo "Leiste algun libro bueno ultimamente?"
case readLine(stdin)
of "no", "No":
  echo "Ve a tu biblioteca local."
of "yes", "Yes":
  echo "Bien, sigue asi."
else:
  echo "Eso es genial; Supongo."

# `while`, `if`, `continue`, `break`

import strutils as str # http://nim-lang.org/docs/strutils.html

echo "Estoy pensando un numero entre 41 y 43. Adivina cual!."
let numero = 42

var
  adivinado_crudo: string
  adivinado: int

while adivinado != numero:
  adivinado_crudo = readLine(stdin)
  if adivinado_crudo == "": continue  # continue saltea esta iteracion.
  adivinado = str.parseInt(adivinado_crudo)
  if adivinado == 1001:
    echo "AAAAAAGGG!"
    break
  elif adivinado > numero:
    echo "Nope. Muy grande."
  elif adivinado < numero:
    echo adivinado, " es muy chico."
  else:
    echo "Yeeeeeehaw!"

#
# Iteracion
#

for i, elem in ["Si", "No", "Tal vez"]:  # O solamente `for elem in`.
  echo elem, " esta en el indice: ", i

for k, v in items(@[(persona: "Vos", poder: 100), (persona: "Yo", poder: 9000)]):
  echo v

let miString = """
an <example>
`string` to
play with
"""  # string multi-linea

for line in splitLines(miString):
  echo(line)

for i, c in miString:        # Indice y letra. O `for j in` para solamente la letra.
  if i mod 2 == 0: continue  # Un `if` compacto.
  elif c == 'X': break
  else: echo c

#
# Procedimientos
#

type Respuesta = enum rSi, rNo

proc preguntar(question: string): Respuesta =
  echo question, " (s/n)"
  while true:
    case readLine(stdin)
    of "s", "S", "si", "Si":
      return Respuesta.rSi  # Los Enums pueden ser completamente calificados.
    of "n", "N", "no", "No":
      return Respuesta.rNo
    else: echo "Por favor se claro: si o no."

proc agregarAzucar(amount = 2) =  # El valor por defecto de amount es 2, No retorna nada.
  assert amount > 0 and amount < 9000, "Azucar muy loco"
  for a in 1..amount:
    echo a, " azucar..."

case preguntar("Queres azucar con tu cafe?")
of aSi:
  agregarAzucar(3)
of aNo:
  echo "Oh, por favor usa un poco!"
  agregarAzucar()
# No hay necesidad de un `else` aqui. Unicamente `yes` o `no` son los valores posibles.

#
# FFI
#

# Nim compila a C, por tal motivo el FFI es muy facil:

proc strcmp(a, b: cstring): cint {.importc: "strcmp", nodecl.}

let cmp = strcmp("C?", "Facil!")
```

Adicionalmente, Nim se diferencia del resto por su metaprogramacion,
performance, y caracteristicas de ejecucion de codigo en tiempo de compilacion.


## Otras lecturas

* [Pagina Web Oficial](http://nim-lang.org)
* [Descargas](http://nim-lang.org/download.html)
* [Comunidad](http://nim-lang.org/community.html)
* [Preguntas Frecuentes](http://nim-lang.org/question.html)
* [Documentacion](http://nim-lang.org/documentation.html)
* [Manual](http://nim-lang.org/docs/manual.html)
* [Libreria Standard](http://nim-lang.org/docs/lib.html)
* [Rosetta Code](http://rosettacode.org/wiki/Category:Nim)
