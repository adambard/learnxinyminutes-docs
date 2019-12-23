---
language: Scala
filename: learnscala-es.scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Ha-Duong Nguyen", "http://reference-error.org"]
translators:
    - ["Pablo Arranz Ropero", "http://arranzropablo.com"]
lang: es-es
---

Scala - El lenguaje escalable

```scala

/////////////////////////////////////////////////
// 0. Básicos
/////////////////////////////////////////////////
/*
  Configurar Scala:

  1) Descarga Scala - http://www.scala-lang.org/downloads
  2) Unzip/untar a tu carpeta elegida y pon la subcarpeta bin en tu variable de entorno `PATH`
*/

/*
  Prueba REPL

  Scala tiene una herramienta llamada REPL (Read-Eval-Print Loop, en español: Bucle de lectura-evaluación-impresión) que es analogo a interpretes de la linea de comandos en muchos otros lenguajes. 
  Puedes escribir cualquier expresión en Scala y el resultado será evaluado e impreso.  

  REPL es una herramienta muy práctica para testear y verificar código.
  Puedes usarla mientras lees este tutorial para explorar conceptos por tu cuenta.
*/

// Inicia Scala REPL ejecutando `scala` en tu terminal. Deberías ver:
$ scala
scala>

// Por defecto cada expresión que escribes es guardada como un nuevo valor numerado:
scala> 2 + 2
res0: Int = 4

// Los valores por defecto pueden ser reusados. Fíjate en el tipo del valor mostrado en el resultado...
scala> res0 + 2
res1: Int = 6

// Scala es un lenguaje fuertemente tipado. Puedes usar REPL para comprobar el tipo sin evaluar una expresión.
scala> :type (true, 2.0)
(Boolean, Double)

// Las sesiones REPL pueden ser guardadas
scala> :save /sites/repl-test.scala

// Se pueden cargar archivos en REPL
scala> :load /sites/repl-test.scala
Loading /sites/repl-test.scala...
res2: Int = 4
res3: Int = 6

// Puedes buscar en tu historial reciente
scala> :h?
1 2 + 2
2 res0 + 2
3 :save /sites/repl-test.scala
4 :load /sites/repl-test.scala
5 :h?

// Ahora que sabes como jugar, aprendamos un poco de Scala...

/////////////////////////////////////////////////
// 1. Básicos
/////////////////////////////////////////////////

// Los comentarios de una linea comienzan con dos barras inclinadas

/*
  Los comentarios de varias lineas, como ya has visto arriba, se hacen de esta manera.
*/

// Así imprimimos forzando una nueva linea en la siguiente impresión
println("Hola mundo!")
println(10)
// Hola mundo!
// 10

// Así imprimimos sin forzar una nueva linea en la siguiente impresión
print("Hola mundo")
print(10)
// Hola mundo10

// Para declarar valores usamos var o val.
// Valores decalrados con val son inmutables, mientras que los declarados con var son mutables. 
// La inmutabilidad es algo bueno.
val x = 10 // x es 10
x = 20     // error: reassignment to val
var y = 10
y = 20     // y es 20

/*
  Scala es un lenguaje tipado estáticamente, aunque se puede ver en las expresiones anteriores que no hemos especificado un tipo. 
  Esto es debido a una funcionalidad del lenguaje llamada inferencia. En la mayoría de los casos, el compilador de Scala puede adivinar cual es el tipo de una variable, así que no hace falta escribirlo siempre.
  Podemos declarar explicitamente el tipo de una variable de la siguiente manera:
*/
val z: Int = 10
val a: Double = 1.0

// Observa la conversión automática de Int a Double, el resultado será 10.0, no 10
val b: Double = 10

// Valores Booleanos
true
false

// Operaciones Booleanas
!true         // false
!false        // true
true == false // false
10 > 5        // true

// Las operaciones matemáticas se realizan como siempre
1 + 1   // 2
2 - 1   // 1
5 * 3   // 15
6 / 2   // 3
6 / 4   // 1
6.0 / 4 // 1.5
6 / 4.0 // 1.5


// Evaluar una expresión en REPL te da el tipo y valor del resultado

1 + 7

/* La linea superior tienen como resultado:

  scala> 1 + 7
  res29: Int = 8

  Esto quiere decir que el resultado de evaluar 1 + 7 es un objeto de tipo Int con valor 8

  Observa que "res29" es un nombre de variable secuencialmente generado para almacenar los resultados de las expresiones escritas, la salida que observes puede diferir en este sentido.
*/

"Las cadenas en Scala están rodeadas por comillas dobles"
'a' // Un caracter en Scala
// 'Las cadenas con comillas simples no existen' <= Esto causa un error

// Las cadenas tienen los los típicos metodos de Java definidos
"hello world".length
"hello world".substring(2, 6)
"hello world".replace("C", "3")

// También tienen algunos métodos extra de Scala. Ver: scala.collection.immutable.StringOps
"hello world".take(5)
"hello world".drop(5)

// Interpolación de cadenas: Observa el prefijo "s"
val n = 45
s"Tengo $n manzanas" // => "Tengo 45 manzanas"

// Es posible colocar expresiones dentro de cadenas interpoladas
val a = Array(11, 9, 6)
s"Mi segunda hija tiene ${a(0) - a(2)} años."         // => "Mi segunda hija tiene 5 años."
s"Hemos doblado la cantidad de ${n / 2.0} manzanas."  // => "Hemos doblado la cantidad de 22.5 manzanas."
s"Potencia de 2: ${math.pow(2, 2)}"                   // => "Potencia de 2: 4"

// Podemos formatear cadenas interpoladas con el prefijo "f"
f"Potencia de 5: ${math.pow(5, 2)}%1.0f"         // "Potencia de 5: 25"
f"Raiz cuadrada de 122: ${math.sqrt(122)}%1.4f" // "Raiz cuadrada de 122: 11.0454"

// Las cadenas puras ignoran caracteres especiales.
raw"Nueva linea: \n. Retorno: \r." // => "Nueva linea: \n. Retorno: \r."

// Algunos caracteres necesitn ser escapados, por ejemplo unas comillas dobles dentro de una cadena:
"Se quedaron fuera de \"Rose and Crown\"" // => "Se quedaron fuera de "Rose and Crown""

// Las triples comillas dobles dejan que las cadenas se expandan por multiples filas y contengan comillas dobles o simples
val html = """<form id="daform">
                <p>Press belo', Joe</p>
                <input type="submit">
              </form>"""


/////////////////////////////////////////////////
// 2. Funciones
/////////////////////////////////////////////////

// Las funciones se definen de la siguiente manera:
//
//   def nombreFuncion(argumentos...): TipoRetorno = { cuerpo... }
//
// Si estás acostumbrado a lenguajes más tradicionales, observa la omisión de la palabra return.
// En Scala, la última expresión en el bloque de función es el valor de retorno.
def sumaDeCuadrados(x: Int, y: Int): Int = {
  val x2 = x * x
  val y2 = y * y
  x2 + y2
}

// Los { } pueden omitirse si el cuerpo de la función es una única expresión:
def sumaDeCuadradosCorta(x: Int, y: Int): Int = x * x + y * y

// La sintaxis para llamar funciones es familiar:
sumaDeCuadrados(3, 4)  // => 25

// Puedes usar los nombres de los parámetros para llamarlos en orden diferente
def restar(x: Int, y: Int): Int = x - y

restar(10, 3)     // => 7
restar(y=10, x=3) // => -7

// En la mayoría de los casos (siendo las funciones recursivas la excepción más notable),
// el tipo de retorno de la función puede ser omitido, y la misma inferencia de tipos que vimos con las variables
// funcionará con los valores de retorno de las funciones:
def sq(x: Int) = x * x  // El compilador puede adivinar que el tipo de retorno es Int

// Las funciones pueden tener parametros por defecto:
def sumarConDefecto(x: Int, y: Int = 5) = x + y
sumarConDefecto(1, 2) // => 3
sumarConDefecto(1)    // => 6


// Las funciones anónimas se escriben así:
(x: Int) => x * x

// Al contrario que los defs, incluso el tipo de entrada de las funciones anónimas puede ser omitido si
// el contexto lo deja claro. Observa el tipo "Int => Int" que significa que es una función
// que recibe Int y retorna Int.
val sq: Int => Int = x => x * x

// Las funciones anónimas pueden ser llamadas como las demás:
sq(10)   // => 100

// Si cada argumento en tu función anónima es usado solo una vez,
// Scala te da una manera incluso más corta de definirlos.
// Estas funciones anónimas son extremadamente comunes, 
// como será obvio en la sección de estructuras de datos.
val sumarUno: Int => Int = _ + 1
val sumaRara: (Int, Int) => Int = (_ * 2 + _ * 3)

sumarUno(5)       // => 6
sumaRara(2, 4)    // => 16


// La palabra return existe en Scala, pero solo retorna desde la función más interna que la rodea.
// ADVERTENCIA: Usar return en Scala puede inducir a errores y debe ser evitado
// No tiene efecto en funciones anónimas. Por ejemplo:
def foo(x: Int): Int = {
  val funcAnon: Int => Int = { z =>
    if (z > 5)
      return z // Esta línea hace que z sea el valor de retorno de foo!
    else
      z + 2    // Esta línea es el valor de retorno de funcAnon
  }
  anonFunc(x)  // Esta línea es el valor de retorno de foo
}


/////////////////////////////////////////////////
// 3. Control del flujo
/////////////////////////////////////////////////

1 to 5
val r = 1 to 5
r.foreach(println)

r foreach println
// Nota: Scala es un lenguaje muy permisivo cuando se trata de puntos y parentesis - estudia las
// reglas separadamente. Esto ayuda a escribir DSLs y APIs que se lean en lenguaje natural.

// Por qué `println` no necesita parámetros aquí?
// Presta atención a las funciones de primera clase en la sección de Programación Funcional más abajo!
(5 to 1 by -1) foreach (println)

// Un bucle while
var i = 0
while (i < 10) { println("i " + i); i += 1 }

while (i < 10) { println("i " + i); i += 1 }   // Si, de nuevo. Qué ocurrió? Por qué?

i    // Muestra el valor de i. Observa que while es un loop en el sentido clásico -
     // se ejecuta secuencialmente mientras cambia la variable del bucle. while es muy
     // rápido, pero los combinadores y comprensiones anteriores son más sencillos
     // de entender y paralelizar

// Un bucle do-while
i = 0
do {
  println("i es aún menor que 10")
  i += 1
} while (i < 10)

// La recursion es la manera idiomática de repetir una acción en Scala (como en la mayoría de
// lenguajes funcionales).
// Las funciones recursivas necesitan un tipo de retorno explicito, el compilador no puede inferirlo.
// En Scala tenemos Unit, que es análogo al tipo de retorno `void` en Java
def enseñaNumerosEnUnRango(a: Int, b: Int): Unit = {
  print(a)
  if (a < b)
    enseñaNumerosEnUnRango(a + 1, b)
}
enseñaNumerosEnUnRango(1, 14)


// Condicionales

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"


/////////////////////////////////////////////////
// 4. Estructuras de datos
/////////////////////////////////////////////////

val a = Array(1, 2, 3, 5, 8, 13)
a(0)     // Int = 1
a(3)     // Int = 5
a(21)    // Lanza una excepción

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")         // java.lang.String = tenedor
m("spoon")        // java.lang.String = cuchara
m("bottle")       // Lanza una excepción

val mapaSeguro = m.withDefaultValue("no lo se")
mapaSeguro("bottle")   // java.lang.String = no lo se

val s = Set(1, 3, 7)
s(0)      // Boolean = false
s(1)      // Boolean = true

/* Hecha un vistazo a la documentación de Map aquí -
 * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map
 * y asegúrate de que puedes leerla
 */


// Tuplas

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// Por qué tener esto?
val dividirEnteros = (x: Int, y: Int) => (x / y, x % y)

// La función dividirEnteros te da el resultado y el resto
dividirEnteros(10, 3)    // (Int, Int) = (3,1)

// Para acceder a los elementos de una tupla, usa _._n donde n es el indice (comenzando por 1)
// del elemento
val d = dividirEnteros(10, 3)    // (Int, Int) = (3,1)

d._1    // Int = 3
d._2    // Int = 1

// Alternativamente puedes asignar multiples variables desde una tupla, lo que
// resulta más conveniente y legible en muchos casos.
val (div, mod) = dividirEnteros(10, 3)

div     // Int = 3
mod     // Int = 1


/////////////////////////////////////////////////
// 5. Programación Orientada a Objetos
/////////////////////////////////////////////////

/*
  Nota: Todo lo que hemos hecho hasta ahora en este tutorial han sido
  simples expresiones (valores, funciones, etc). Estas expresiones son validas 
  para hacer pruebas rapidas en el interprete de la linea de comandos, 
  pero no pueden existir por si solas en un archivo de Scala. Por ejemplo, 
  no puedes tener simplemente "val x = 5" en un archivo Scala. En lugar de eso,
  las únicas construcciones de alto nivel en Scala son:

  - Objetos
  - Clases
  - Case clases
  - Traits

  Y ahora explicaremos lo que son estas.
*/

// Las clases son similares a las clases de otros lenguajes. Los argumentos del constructor 
// son declarados despues del nombre de la clase, y la inicialización se hace en el cuerpo de la clase.
class Perro(r: String) {
  // Código del constructor aquí
  var raza: String = r

  // Define un método llamado ladrar, que devuelva un String
  def ladrar = "Woof, woof!"

  // Los valores y métodos son asumidos como públicos. 
  // Las palabras "protected" y "private" también son válidas.
  private def dormir(horas: Int) =
    println(s"Estoy durmiendo $horas horas")

  // Los métodos abstractos son simplemente métodos sin cuerpo. 
  // Si descomentamos la linea de debajo, la clase Perro necesitaría ser abstracta:
  //   abstract class Perro(...) { ... }
  // def perseguir(algo: String): String
}

val miperro = new Dog("greyhound")
println(mydog.raza) // => "greyhound"
println(mydog.ladrar)  // => "Woof, woof!"


// La palabra "object" crea un tipo y una instancia singleton de ese tipo.
// Es común que las clases en Scala tengan un "companion object", de manera que 
// el comportamiento por instancia es controlado por las clases y el comportamiento 
// relacionado a todas las instancias de esa clase es controlado por el objeto
// La relación es similar a los métodos de las clases con los métodos estáticos 
// en otros lenguajes. Observa que los objetos y clases pueden tener el mismo nombre.
object Perro {
  def todasLasRazasConocidas = List("pitbull", "shepherd", "retriever")
  def crearPerro(raza: String) = new Dog(breed)
}


// Case clases son clases que tienen funcionalidad extra añadida. Una pregunta
// común para los principiantes en Scala es cuando usar case clases y cuando usar 
// clases. La linea no está bien definida, pero en general, las clases tienden a
// enfocarse en la encapsulación, polimorfismo y comportamiento. Los valores en
// estas clases tienden a ser privados, y solo se exponen los métodos. 
// El propósito principal de las case clases es tener datos inmutables.
// A menudo tienen pocos métodos, y los métodos raramente tienen efectos secundarios.
case class Persona(nombre: String, telefono: String)

// Para crear instancia nuevas, observa que las case clases no necesitan "new"
val george = Persona("George", "1234")
val kate = Persona("Kate", "4567")

// Con las case clases tienes unas pocas ventajas, como el acceso a los campos:
george.telefono  // => "1234"

// Para la igualdad de campos no necesitas sobreescribir el método equals
Persona("George", "1234") == Persona("Kate", "1236")  // => false

// Manera fácil de copiar
// otroGeorge == Persona("George", "9876")
val otroGeorge = george.copy(telefono = "9876")

// Y muchas otras. Las case clases también tienen comparación de patrones, que veremos más abajo.

// Traits
// De manera similar a las interfaces Java, los traits definen un tipo de objeto y métodos.
// Scala permite la implementación parcial de dichos métodos.
// No se permiten parámetros de constructor. Los traits pueden heredar de otros traits o
// clases sin parámetros.

trait Perro {
	def raza: String
	def color: String
	def ladra: Boolean = true
	def muerde: Boolean
}
class SanBernardo extends Perro {
	val raza = "San Bernardo"
	val color = "marrón"
	def muerde = false
}  

scala> b  
res0: SanBernardo = SanBernardo@3e57cd70  
scala> b.raza  
res1: String = San Bernardo  
scala> b.ladra
res2: Boolean = true  
scala> b.muerde
res3: Boolean = false  

// Un trait tambien puede ser usado mezclado con otros traits. 
// La clase extiende el primer trait, pero la palabra "with" 
// puede añadir traits adicionales.

trait Ladra {
	def ladra: String = "Guau"
}
trait Perro {
	def raza: String
	def color: String
}
class SanBernardo extends Perro with Ladra {
	val raza = "San Bernardo"
	val color = "marrón"
}

scala> val b = new SanBernardo
b: SanBernardo = SanBernardo@7b69c6ba
scala> b.ladra
res0: String = Guau


/////////////////////////////////////////////////
// 6. Comparación de patrones
/////////////////////////////////////////////////

// La comparación de patrones es una poderosa función de Scala.
// Ahora veremos como comparar patrones en una case clase. 
// Nota: A diferencia de otros lenguajes, Scala "cases" no necesitan
// "break", porque no ejecuta los "case" posteriores.

def comparaPersona(persona: Persona): String = persona match {
  // Aqui especificas los patrones:
  case Persona("George", telefono) => "Hemos encontrado a George! Su número es " + telefono
  case Persona("Kate", telefono)   => "Hemos encontrado a Kate! Su número es " + telefono
  case Persona(nombre, telefono)     => "Hemos encontrado alguien : " + nombre + ", teléfono : " + telefono
}

// Las expresiones regulares también están incorporadas.
// Creas una expresión regular con el método `r` en una cadena:
val email = "(.*)@(.*)".r

// La comparación de patrones puede parecerse al bloque switch en la familia de lenguajes de C,
// pero aquí es mucho más poderosa. En Scala, puedes hacer más comparaciones:
def comparaTodo(obj: Any): String = obj match {
  // Puedes comparar valores:
  case "Hola mundo" => "Tengo la cadena Hola mundo"

  // Puedes comparar tipos:
  case x: Double => "Tengo un double: " + x

  // Puedes especificar condiciones:
  case x: Int if x > 10000 => "Tengo un número muy grande!"

  // Puedes comparar case clases como antes:
  case Persona(nombre, telefono) => s"Tengo la información de contacto de $nombre!"

  // Puedes comparar expresiones regulares:
  case email(nombre, dominio) => s"Tengo la dirección de correo $nombre@$dominio"

  // Puedes comparar tuplas:
  case (a: Int, b: Double, c: String) => s"Tengo la tupla: $a, $b, $c"

  // Puedes comparar estructuras:
  case List(1, b, c) => s"Tengo un alista con tres elementos que empieza con 1: 1, $b, $c"

  // Puedes anidar patrones:
  case List(List((1, 2, "YAY"))) => "Tengo una lista de listas de tuplas"

  // Comparar cualquier case (default) si todos los anteriores no han coincido
  case _ => "Tengo un objeto desconocido"
}

// De hecho puedes comparar un patrón con cualquier objeto con el método "unapply". 
// Esta función es tan poderosa que Scala te deja definir funciones enteras como patrones:
val funcPatron: Person => String = {
  case Persona("George", telefono) => s"Teléfono de George: $telefono"
  case Persona(nombre, telefono) => s"Teléfono de una persona aleatoria: $telefono"
}


/////////////////////////////////////////////////
// 7. Programación funcional
/////////////////////////////////////////////////

// Scala permite a los métodos y funciones devolver o 
// recibir como parámetros otras funciones o métodos

val suma10: Int => Int = _ + 10 // Una función que recibe y devuelve un Int
List(1, 2, 3) map suma10 // List(11, 12, 13) - suma10 es aplicado a cada elemento

// Las funciones anónimas pueden ser usadas en vez de funciones con nombre:
List(1, 2, 3) map (x => x + 10)

// Y la barra baja puede ser usada si solo hay un argumento en la función anónima.
// Se usa como la variable. 
List(1, 2, 3) map (_ + 10)

// Si el bloque anónimo Y la función que estás usando usan los dos un argumento, 
// puedes incluso omitir la barra baja.
List("Dom", "Bob", "Natalia") foreach println


// Combinadores
// Usando s de arriba:
// val s = Set(1, 3, 7)

s.map(sq)

val sCuadrado = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)

// La función filter toma un predicado (una función A -> Boolean) y
// selecciona todos los elementos que satisfacen el predicado.
List(1, 2, 3) filter (_ > 2) // List(3)
case class Persona(nombre: String, edad: Int)
List(
  Persona(nombre = "Dom", edad = 23),
  Persona(nombre = "Bob", edad = 30)
).filter(_.edad > 25) // List(Persona("Bob", 30))


// Ciertas colecciones (como List) en Scala tienen un método `foreach`,
// que toma como argumento un tipo que devuelva Unit (un método void)
val unaListaDeNumeros = List(1, 2, 3, 4, 10, 20, 100)
unaListaDeNumeros foreach (x => println(x))
unaListaDeNumeros foreach println

// Para comprensiones

for { n <- s } yield sq(n)

val nCuadrado2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared

/* Nota: Esos no son bucles. La semántica de un bucle es repetir, mientras que un for de comprension define una relación entre dos conjuntos de datos.*/


/////////////////////////////////////////////////
// 8. Implicitos
/////////////////////////////////////////////////

/* ATENCIÓN ATENCIÓN: Los implicitos son un conjunto de poderosas características de Scala
 * y es fácil abusar de ellos. Si eres principiante en Scala deberías resistir la tentación
 * de usarlos hasta que entiendas no solo como funcionan, sino también las mejores prácticas
 * con ellos. Nosotros solo incluiremos esta sección en el tutorial porque son tan comunes
 * en las librerias de Scala que es imposible hacer algo significativo sin usar una librería
 * que tenga implicitos. Esto es para que entiendas como funcionan los implicitos, no para 
 * que definas los tuyos propios.
 */

// Cualquier valor (val, funciones, objetos, etc) puede ser declarado como implicito usando
// la palabra "implicit". Observa que usamos la clase Perro de la sección 5.
implicit val miEnteroImplicito = 100
implicit def miFunciónImplicita(raza: String) = new Perro("Golden " + raza)

// Por si misma, la palabra implicit no cambia el comportamiento de un valor,
// así que estos valores pueden ser usados como siempre.
miEnteroImplicito + 2              // => 102
miFunciónImplicita("Pitbull").raza // => "Golden Pitbull"

// La diferencia es que estos valores ahora pueden ser usados cuando otra pieza de código
// necesite un valor implicito. Una situación así puede darse con argumentos implicitos de función:
def enviaSaludos(aQuien: String)(implicit cuantos: Int) =
  s"Hola $aQuien, $cuantos saludos a ti y a los tuyos!"

// Si proporcionamos un valor para "cuantos", la función se comporta como siempre
enviaSaludos("John")(1000)  // => "Hola John, 1000 saludos a ti y a los tuyos!"

// Pero si omitimos el parámetro implicito, un valor implicito del mismo tipo es usado,
// en este caso, "miEnteroImplicito":
enviaSaludos("Jane")  // => "Hello Jane, 100 blessings to you and yours!"

// Los parámetros de función implicit nos permiten simular clases tipo en otros lenguajes funcionales. 
// Es usado tan a menudo que tiene su propio atajo. Las dos siguientes lineas significan lo mismo:
// def foo[T](implicit c: C[T]) = ...
// def foo[T : C] = ...


// Otra situación en la que el compilador busca un implicit es si tienes
//   obj.método(...)
// pero "obj" no tiene "método" como un método. En este caso, si hay una conversión 
// implicita de tipo A => B, donde A es el tipo de obj y B tiene un método llamado
// "método", esa conversión es aplicada. Así que teniendo miFunciónImplicita, podemos decir:
"Retriever".raza // => "Golden Retriever"
"Sheperd".ladra    // => "Woof, woof!"

// Aquí la cadena es convertida primero a Perro usando nuestra función miFunciónImplicita, 
// y entonces el método apropiado es llamado. Esta es una herramienta extremadamente poderosa
// pero de nuevo, no puede usarse con ligereza. De hecho, cuando definiste la función implicita, 
// tu compilador debería haber mostrado una advertencia, diciendo que no deberías hacer esto 
// a no ser que realmente sepas lo que estás haciendo.

/////////////////////////////////////////////////
// 9. Misc
/////////////////////////////////////////////////

// Importando cosas
import scala.collection.immutable.List

// Importando todos los "sub paquetes"
import scala.collection.immutable._

// Importando multiples clases en una línea
import scala.collection.immutable.{List, Map}

// Renombrar un import usando '=>'
import scala.collection.immutable.{List => ImmutableList}

// Importar todas las clases, excepto algunas. La siguiente linea excluye Map y Set:
import scala.collection.immutable.{Map => _, Set => _, _}

// Las clases de Java pueden ser importadas también con sintaxis de Scala:
import java.swing.{JFrame, JWindow}

// El punto de entrada de tus programas está definido en un fichero scala usando un object, 
// con un solo método, main:
object Application {
  def main(args: Array[String]): Unit = {
    // Aquí va tu código.
  }
}

// Los ficheros pueden contener multiples clases y objetos. Compila con scalac


// Salida y entrada

// Leer un fichero línea por línea
import scala.io.Source
for(line <- Source.fromFile("miarchivo.txt").getLines())
  println(line)

// Para escribir un archivo usa el PrintWriter de Java
val writer = new PrintWriter("miarchivo.txt")
writer.write("Escribiendo linea por linea" + util.Properties.lineSeparator)
writer.write("Otra linea" + util.Properties.lineSeparator)
writer.close()

```

## Más recursos

* [Scala para los impacientes](http://horstmann.com/scala/)
* [Escuela de Scala en Twitter](http://twitter.github.io/scala_school/)
* [La documentación de Scala](http://docs.scala-lang.org/)
* [Prueba Scala en tu navegador](http://scalatutorials.com/tour/)
* Unete al [grupo de usuarios de Scala](https://groups.google.com/forum/#!forum/scala-user)
