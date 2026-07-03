---
name: Gleam
contributors:
    - ["Antonio Ognio", "https://github.com/aognio/"]
filename: learngleam.gleam
---

Gleam es un nuevo lenguaje para la máquina virtual BEAM de Erlang que se basa en
el poder de un sistema de tipos robusto, la expresividad de la programación funcional,
y el tiempo de ejecución Erlang altamente concurrente y tolerante a fallas que utiliza
sintaxis moderna inspirada en lenguajes como OCaml, Rust y Elixir.

Al ser un desarrollo bastante moderno, Gleam viene con un compilador, una herramienta de compilación,
un formateador de código, varias integraciones de editores y un administrador de paquetes.

Al formar parte del ecosistema BEAM en general, los programas creados con Gleam
también pueden aprovechar miles de paquetes publicados escritos en Erlang o Elixir.

El diseño del lenguaje es muy conciso por lo que no presenta valores nulos.
sin excepciones, mensajes de error claros y un sistema de tipos práctico.

JavaScript también se admite como destino de compilación, por lo que puedes ejecutar Gleam
código en el navegador o cualquier otro tiempo de ejecución habilitado para JS. Al utilizar esta función,
Se crean definiciones de TypeScript para que pueda interactuar con su código Gleam
con confianza, incluso desde fuera.

Para ejecutar este código, primero cree un proyecto Gleam:

```sh
gleam new my_project
cd my_project
```

Reemplace el código generado con este ejemplo y ejecute:

```sh
gleam run
```


```gleam
//// Este comentario con cuatro barras es a nivel de módulo.
//// Este tipo de comentarios se utilizan para describir todo el módulo.

import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/dict

// El nombre de un tipo siempre comienza con una letra mayúscula, a diferencia de las variables.
// y funciones, que comienzan con una letra minúscula.

//Cuando se utiliza la palabra clave pub, el alias de tipo es público y se puede hacer referencia a él.
// por otros módulos.

pub type UserId =
  Int

pub fn main() {
  io.println("Hello from learnxinmyminutes.com!")
  // io.println ("¡Esta declaración fue comentada con dos barras diagonales!")

  // Los módulos son las unidades en las que se organiza todo el código de Gleam.
  // En un módulo encontrará un montón de definiciones de tipos, funciones, etc.
  // que parecen ir juntos.
  // Por ejemplo, el módulo gleam/io contiene una variedad de funciones para
  // imprimiendo, como println.

  // Todo el código de gleam está en algún módulo u otro, cuyo nombre proviene del nombre
  // del archivo en el que se encuentra.
  // Por ejemplo, gleam/io está en un archivo llamado io.gleam dentro de un directorio
  // llamado gleam.

  // Gleam tiene un robusto sistema de tipo estático que te ayuda mientras escribes y editas
  // código, detectando errores y mostrándole dónde realizar cambios.
  // io.println(10)
  // Si descomentas la línea anterior, obtendrás un error en tiempo de compilación.
  // ya que la función io.println solo funciona con cadenas, no con enteros.

  // La compilación generará un error similar a este:
  // error: Type mismatch
  //  ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:21:14
  //  │
  // 21 │ io.println(10)
  //  │              ^^
  //
  // Expected type:
  //
  //     String
  //
  // Found type:
  //
  //     Int

  // trabajando con números

  //Cuando se ejecuta en la máquina virtual Erlang, las entradas no tienen máximo ni mínimo
  // tamaño.
  // Cuando se ejecuta en tiempos de ejecución de JavaScript, los ints se representan utilizando JavaScript
  // Números de coma flotante de 64 bits.

  // Podemos usar `echo` para depurar imprimir un valor.

  // Int arithmetic
  echo 1 + 1
  echo 5 - 1
  echo 5 / 2
  echo 3 * 3
  echo 5 % 2

  // Int comparisons using variables to prevent compiler static analysis warnings
  let two = 2
  let one = 1
  let same_one = 1
  let same_two = 2
  let _ = echo two > one
  let _ = echo two < one
  let _ = echo two >= one
  let _ = echo two <= one

  // La igualdad sirve para cualquier tipo y se controla estructuralmente, lo que significa que dos
  // Los valores son iguales si tienen la misma estructura y no si están en
  // la misma ubicación de memoria.
  let _ = echo one == same_one
  // True
  let _ = echo two != same_two
  // False

  // Funciones int de biblioteca estándar
  echo int.min(142, 137)
  // 137
  echo int.clamp(-80, min: 0, max: 100) // abrazadera (valor, mínimo, máximo) especifica el valor que debe estar dentro del rango dado
  // 0
  let _ = echo int.base_parse("10", 2) // analiza una cadena binaria como un entero de base 2. 
  // Ok(2) : 10 in binary equals to 2 in integer

  // Literales Int binarios, octales y hexadecimales
  echo 0b00001111
  echo 0o17
  echo 0xF

  // Utilice guiones bajos para mejorar la legibilidad de los números enteros
  echo 1_000_000

  // Los operadores numéricos de Gleam no están sobrecargados, por lo que hay dedicados
  // Operadores para trabajar con flotadores.

  // Float arithmetic
  echo 1.0 +. 1.5
  echo 5.0 -. 1.5
  echo 5.0 /. 2.5
  echo 3.0 *. 3.5

  // Float comparisons using variables to prevent compiler static analysis warnings
  let two_point_two = 2.2
  let one_point_three = 1.3
  let _ = echo two_point_two >. one_point_three
  let _ = echo two_point_two <. one_point_three
  let _ = echo two_point_two >=. one_point_three
  let _ = echo two_point_two <=. one_point_three

  // Los flotantes se representan como números de punto flotante de 64 bits tanto en Erlang
  // y tiempos de ejecución de JavaScript.
  // El comportamiento de punto flotante es nativo de sus respectivos tiempos de ejecución, por lo que
  // su comportamiento exacto será ligeramente diferente en los dos tiempos de ejecución.

  // Bajo el tiempo de ejecución de JavaScript, exceder el máximo (o mínimo)
  //El valor representable para un valor de punto flotante dará como resultado Infinito.
  // (o -Infinito). Si intentas dividir dos infinitos obtendrás NaN
  // como resultado.

  // Cuando se ejecuta en BEAM, cualquier desbordamiento generará un error. Entonces no hay
  // Valor flotante NaN o Infinity en el tiempo de ejecución de Erlang.

  // La división por cero no es un error y se define como cero
  echo 3.14 /. 0.0
  // 0.0

  // Funciones flotantes de biblioteca estándar
  echo float.max(2.0, 9.5)
  // 9.5
  echo float.ceiling(5.4)
  // 6.0

  // También se admiten guiones bajos para flotantes.
  echo 10_000.01

  // Trabajando con cadenas
  echo "⭐ Gleam ⭐ - 별"
  echo "this
    is
    a
    multi
    line
    string"

  echo "\u{1F600}"
  // Sale un emoticón 😀

  // Se pueden escapar las comillas dobles
  io.println("\"X\" marks the spot")

  // String concatenation
  echo "One " <> "Two"

  // String functions
  echo string.reverse("1 2 3 4 5")
  echo "abc" <> "def"

  io.println(string.reverse("!desrever tog gnirts sihT"))
  // Genera "¡Esta cadena se invirtió!"

  // Se admiten varias secuencias de escape:

  // \" - comillas dobles
  // \\ - barra invertida
  // \f - avance de formulario
  // \n - nueva línea
  // \r - retorno de carro
  // \t - pestaña

  // Bool operators
  // El || y los operadores && funcionan mediante cortocircuito

  echo True && False
  // False

  echo True && True
  // True

  echo False || False
  // False

  echo False || True
  // True

  // Bool functions
  echo bool.to_string(True)
  // "True"

  //Asignaciones
  let x = "Original value"
  echo x

  // Asigna `y` al valor de `x`
  let y = x
  echo y

  // Asigne `x` a un nuevo valor
  let x = "New value"
  echo x

  // La `y` todavía se refiere al valor original.
  echo y

  // En Gleam, los nombres de variables y funciones están escritos en Snake_case.
  let answer_to_the_universe = 42
  echo answer_to_the_universe

  let and_everything = answer_to_the_universe
  // Ahora usar una variable produce una advertencia.

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │   let and_everything = answer_to_the_universe
  //     │       ^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_and_everything`.

  // Escriba anotaciones

  let _name: String = "Gleam"

  let _is_cool: Bool = True

  let _version: Int = 1
  // Útil para fines de documentación, pero no cambia la forma en que el compilador
  // verifica el código más allá de asegurarse de que la anotación coincida con el tipo;
  // de lo contrario, obtendrás un error.

  // let _has_wrong_type_annotation: Int = True

  //  error: Type mismatch
  //      ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:219:41
  //      │
  //  219 │ let _has_wrong_type_annotation: Int = True
  //      │                                         ^^^^
  //
  //  Expected type:
  //
  //      Int
  //
  //  Found type:
  //
  //      Bool

  // Escriba alias
  let one: UserId = 1
  // Consulte el principio del archivo para conocer la definición del tipo UserId.

  let two: Int = 2

  // Los alias sirven sólo para crear código más legible y más preciso.
  // documentación.
  // En el fondo siguen siendo valores del mismo tipo, por lo que las operaciones
  // todavía trabajo
  echo one + two
  // 3

  // Bloques: alcance y valor
  let radius = {
    let value = 100.0
    value
  }
  //valor de eco // <- Esto no se compilará porque "valor" está fuera de alcance

  let area = 3.14159 *. radius *. radius
  echo area

  // Utilice bloques para agrupar operaciones en lugar de paréntesis
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  echo n1 != n2
  // True

  // Listas

  // Sobrinos de Scrooge McDuck
  let nephews = ["Huey", "Dewey", "Louie"]
  echo nephews
  // ["Huey", "Dewey", "Louie"]

  // Anteponer inmutablemente para que la lista original no cambie
  echo ["Donald", ..nephews]
  // ["Donald", "Huey", "Dewey", "Louie"]

  // Algunas funciones de biblioteca estándar para listas

  list.each(nephews, io.println)
  // huey
  // Dewey
  // Luis

  echo list.drop(nephews, 2)
  // ["Louie"]

  more_examples()
  more_function_examples()
  generic_typing_examples()
  beloved_pipelines_demo()
  labels_in_function_calls()
  showcase_flow_control()
  more_on_recursion()
  more_on_pattern_matching()
  showcase_types()
  more_on_types()
  more_on_callbacks()
  showcase_externals()
  showcase_constants()
  showcase_dicts()
  showcase_bit_arrays()
  showcase_opaque_types()
  showcase_panic()
}

// La palabra clave fn se utiliza para definir nuevas funciones.
fn multiply(a: Int, b: Int) -> Int {
  // Sin devolución explícita
  // Se devuelve la última expresión.
  a * b
}

// Las funciones duplicar y multiplicar se definen sin la palabra clave pub.
// Esto las convierte en funciones privadas, solo se pueden utilizar dentro de este módulo.
// Si otro módulo intentara usarlos, se produciría un error del compilador.
fn double(a: Int) -> Int {
  multiply(a, 2)
}

// Solo se exportan funciones públicas y se pueden llamar desde fuera del módulo.

// Las anotaciones de tipo son opcionales para los argumentos de funciones y los valores de retorno.
// pero se consideran buenas prácticas para mayor claridad y para fomentar
// diseño intencional y reflexivo.

pub fn is_leap_year(year: Int) -> Bool {
  { year % 4 == 0 } && { { year % 100 != 0 } || { year % 400 == 0 } }
}

fn more_examples() {
  //La depuración también devuelve un valor, por lo que su salida es el valor de retorno de
  // esta función
  echo double(10)
  // 20
  echo is_leap_year(2000)
  // True
}

// Gleam admite funciones de orden superior:
// Se pueden asignar a variables y pasar como argumentos a otras funciones.
// o incluso ser devuelto como valores de bloques u otras funciones
fn call_func_on_int(func: fn(Int) -> Int, value: Int) -> Int {
  func(value)
}

fn more_function_examples() -> Int {
  echo call_func_on_int(double, 2)
  // 4

  let square = fn(x: Int) -> Int { x * x }
  echo square(3)
  // 9

  // Llamar a una función anónima inmediatamente después de definirla
  echo fn(x: Int) { x + 1 }(1)

  // Ejemplo de cierre
  let make_adder = fn(n: Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { argument + n }
  }

  let adder_of_fives = make_adder(5)
  echo adder_of_fives(10)
  // 15

  // Las funciones anónimas se pueden utilizar indistintamente con funciones con nombre.
  echo call_func_on_int(fn(x: Int) -> Int { x + 100 }, 900)
  // 1000

  // Creemos un decorador de funciones.
  let twice = fn(wrapped_func: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argument: Int) -> Int { wrapped_func(wrapped_func(argument)) }
  }
  let quadruple = twice(double)
  echo quadruple(1)

  let quadruple_2 = fn(a: Int) -> Int { multiply(4, a) }
  echo quadruple_2(2)
  // 8

  // Una captura de función es una sintaxis abreviada para crear funciones anónimas.
  // que toma un argumento e inmediatamente llama a otra función con ese
  // argumento
  let quadruple_3 = multiply(4, _)
  echo quadruple_3(4)
  // 16
}

// Las funciones genéricas se admiten mediante variables de tipo.
fn generic_twice(func: fn(value) -> value, argument: value) -> value {
  func(func(argument))
}

// En generic_twice el valor era la variable de tipo.
// En generic_twice_decorator the_type es la variable de tipo.
// Como en cualquier otra variable, puedes elegir el nombre.
fn generic_twice_decorator(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argument: the_type) -> the_type { func(func(argument)) }
}

fn generic_typing_examples() {
  let double_integers = fn(a: Int) -> Int { a * 2 }
  let double_floats = fn(a: Float) -> Float { a *. 2.0 }
  echo generic_twice(double_integers, 3)
  echo generic_twice(double_floats, 3.0)

  let quadruple_integers = generic_twice_decorator(double_integers)
  let quadruple_floats = generic_twice_decorator(double_floats)
  echo quadruple_integers(1)
  // 4
  echo quadruple_floats(1.0)
  // 4.0
}

// El operador de tubería de Gleam |> toma el resultado de la expresión a su izquierda
// y lo pasa como argumento a la función de su derecha.
fn beloved_pipelines_demo() {
  // Seamos honestos: quieres usar Gleam solo para este genial operador, ¿verdad?
  ["hello", "world"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> echo

  // Mucho más limpio que esto, ¿verdad?
  echo 
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["hello", "world"], " "), ["!"]),
      ),
    )

  //Solución al primer problema del Proyecto Euler:
  // URL: https://projecteuler.net/problem=1
  // Description: Find the sum of all the multiples of 3 and 5 below 1000.
  // Usando int.range de gleam/int para iterar de forma recursiva.
  int.range(from: 1, to: 999, with: 0, run: fn(acc, n) {
    case n % 3 == 0 || n % 5 == 0 {
      True -> acc + n
      False -> acc
    }
  })
  |> int.to_string
  |> fn(sum_as_text: String) {
    "Solution to Project Euler's problem #1: " <> sum_as_text
  }
  |> echo
  // Solution to Project Euler's problem #1: 233168
}

// Se pueden agregar etiquetas antes de cada argumento.
fn call_func_on_int_with_labels(
  func passed_func: fn(Int) -> Int,
  value n: Int,
) -> Int {
  passed_func(n)
}

// La etiqueta y el argumento pueden tener el mismo nombre.
fn add_one(number number: Int) -> Int {
  number + 1
}

fn add_two_integers(first n: Int, second m: Int) -> Int {
  n + m
}

fn labels_in_function_calls() -> Int {
  // Como estamos etiquetando los argumentos, podemos cambiar el orden.
  // si queremos
  echo call_func_on_int_with_labels(value: 8, func: double)
  echo add_one(number: 1)
  // 2
  echo string.contains(does: "theme", contain: "the")
  // True
  // Los argumentos sin etiquetar deben ir primero
  echo add_two_integers(2, second: 2)
  // 4
}

fn showcase_flow_control() {
  // Caso de uso si desea utilizar la coincidencia de patrones para
  // seleccione qué código ejecutar.
  // Gleam se asegurará de que todos los valores posibles estén cubiertos
  // mediante la realización de controles exhaustivos.
  // De lo contrario, obtendrá errores de compilación.
  let puppies = ["Bear", "Frisco", "Ranger"]
  let count = list.length(of: puppies)
  {
    "We have "
    <> int.to_string(count)
    <> " "
    <> // El guión bajo coincide con cualquier otro valor.
    case count {
      1 -> "puppy"
      _ -> "puppies"
    }
  }
  |> echo

  // Gleam permite que los patrones en expresiones de caso también asignen variables.
  {
    "Puppy count: "
    <> case list.length(puppies) {
      0 -> "None."
      1 -> "Just one."
      other -> "As many as " <> int.to_string(other) <> " puppies."
    }
  }
  |> echo

  // Considere que los lenguajes BEAM son funcionales en el diseño y Gleam no es una excepción.
  // por lo que no hay construcciones if, for o while disponibles.

  // Utilice la coincidencia de patrones para condicionales
  let answer = 42
  case answer == 42 {
    True -> {
      echo "This is the answer to the universe."
    }
    False -> {
      echo "This is the answer to something else."
    }
  }

  // Utilice recursividad en lugar de bucle
  from_one_to_ten(1)
}

// función recursiva
fn from_one_to_ten(n: Int) {
  echo n
  case n {
    10 -> Nil
    _ -> from_one_to_ten(n + 1)
  }
}

// Para evitar el agotamiento de la memoria debido a la creación excesiva
//apilar marcos al llamar funciones de forma recursiva, Gleam admite
// "optimización de llamada final", lo que significa que el compilador puede reutilizar
// el marco de pila para la función actual si se realiza una llamada a la función
// lo último que hace la función.

pub fn fib(x: Int) -> Int {
  // La función pública llama a la función recursiva de cola privada.
  fib_loop(x, 1)
}

fn fib_loop(x: Int, accumulator: Int) -> Int {
  case x {
    1 -> accumulator

    // Lo último que hace esta función es llamarse a sí misma.
    // En la lección anterior lo último que hizo fue multiplicar dos enteros
    _ -> fib_loop(x - 1, accumulator + x)
  }
}

// Gleam admite la coincidencia de patrones del primer elemento y el resto
// de una lista con el patrón [x, ..y] dentro de una expresión de caso.
fn reverse_list(the_list: List(value)) -> List(value) {
  case the_list {
    [head, ..tail] -> list.flatten([reverse_list(tail), [head]])
    [] -> []
  }
}

fn more_on_recursion() {
  echo fib(10)
  // 55
  echo reverse_list([1, 2, 3])
}

fn more_on_pattern_matching() {
  // Cuando se comparan patrones en cadenas, el operador <> coincide en cadenas
  // con un prefijo específico y asigna el recordatorio a una variable
  let lucy = "Hello, Lucy"
  let _ = echo case lucy {
    "Hello, " <> name -> "Greetings for " <> name
    _ -> "Potentially no greetings"
  }

  // Se admiten patrones alternativos, por lo que se utiliza la misma cláusula.
  // para múltiples valores
  let month = 2
  let year = 2024
  let number_of_days = case month {
    2 ->
      case is_leap_year(year) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  echo "Number of days: " <> int.to_string(number_of_days)
  // 29

  // Guardias en coincidencia de patrones:
  // Cuando se utiliza la palabra clave if, una expresión debe evaluarse como Verdadera
  // para que el patrón coincida.
  let list_starts_with = fn(the_list: List(value), the_value: value) -> Bool {
    case the_list {
      [head, ..] if head == the_value -> True
      _ -> False
    }
  }
  echo list_starts_with([10, 20, 30], 10)
  // True
}

pub type Gender {
  Male
  Female
  Other
}

// Registros:
// - Variantes de soporte
// - Cada variante es similar a una estructura con campos.
pub type Shape {
  Rectangle(base: Float, height: Float)
  Triangle(base: Float, height: Float)
}

// Los registros con una variante se parecen a estructuras
pub type Point {
  Point(x: Float, y: Float)
}

fn showcase_types() {
  //Tuplas:
  // - Puede mezclar elementos de diferentes tipos.
  // - Su tipo es implícito, p.e. #{1, "Hola"} es de tipo #{Int, String}
  // - Se puede acceder a sus elementos mediante índices numéricos.
  let tuple_01 = #(1, "Ferris", "rustacean", True)
  let tuple_02 = #(1, "Lucy", "starfish", True)
  echo tuple_01
  echo tuple_01.0
  // 1
  echo tuple_02.1
  // lucía
  let #(_, name, species, _) = tuple_01
  echo name <> " the " <> species

  // Coincidencia de patrones con tuplas, incluida la asignación de variables
  let _ = print_mascot_status(tuple_02)

  // Usar un tipo personalizado con coincidencia de patrones
  let gender = Other
  let _ = echo gender_to_string(gender)

  // Usando registros
  let rectangle_1 = Rectangle(base: 10.0, height: 20.0)
  echo rectangle_1.height
  // 10.3

  let point_1 = Point(x: 3.2, y: 4.3)
  echo point_1

  // Actualizar un registro
  let point_2 = Point(..point_1, y: 5.7)
  echo point_2

  // En Gleam, los valores no admiten valores NULL.
  // Nil es el único valor de este tipo.
  let some_var = Nil
  let result = io.println("Hello!")
  echo some_var == result
  // True
}

fn print_mascot_status(mascot: #(Int, String, String, Bool)) {
  case mascot {
    #(_, name, _, True) -> echo name <> " is a mascot."
    #(_, name, _, False) -> echo name <> " is not a mascot."
  }
}

fn gender_to_string(gender: Gender) -> String {
  case gender {
    Male -> "Boy"
    Female -> "Girl"
    Other -> "Undetermined"
  }
}

pub type Mineral {
  Gold
  Silver
  Copper
}

// Tipos personalizados genéricos con tipos contenidos como parámetros
pub type Purity(inner_type) {
  Pure(inner_type)
  Impure(inner_type)
}

pub type Beverage {
  Water
  Juice
}

// Tipos personalizados existentes de los módulos gleam/option y gleam/result
// facilitar el trabajo con valores que aceptan valores NULL y el manejo de posibles errores
pub type Person {
  Person(name: String, nickname: Option(String))
}

pub type DiceError {
  DiceValueOutOfRange
}

fn checked_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(value)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn double_dice_value(value: Int) -> Result(Int, DiceError) {
  case value {
    1 | 2 | 3 -> Ok(value * 2)
    _ -> Error(DiceValueOutOfRange)
  }
}

fn more_on_types() {
  let mineral_sample_01: Purity(Mineral) = Pure(Gold)
  let mineral_sample_02 = Impure(Silver)
  echo mineral_sample_01
  echo mineral_sample_02

  // Un vaso puede estar vacío o no.
  let glass_01: Option(Beverage) = Some(Water)
  let glass_02 = None
  echo glass_01
  echo glass_02

  // Una persona puede tener un apodo o no.
  let person_01 = Person(name: "John", nickname: Some("The Ripper"))
  let person_02 = Person(name: "Martin", nickname: None)
  echo person_01
  echo person_02

  // Trabajar con funciones que devuelven valores de tipo Resultado
  let dice_01 = 5
  case checked_dice_value(dice_01) {
    Ok(checked_value) ->
      echo "The value of " <> int.to_string(checked_value) <> " is OK."
    Error(DiceValueOutOfRange) ->
      echo "The value of the dice is out of range"
  }

  // Intentemos duplicar el valor si el valor resultante sigue siendo
  // un número en cualquiera de los lados del dado.
  // De lo contrario, pongamos el valor máximo.
  2
  |> checked_dice_value
  |> result.try(double_dice_value)
  |> result.unwrap(or: 6)
  |> echo
}

pub fn throw_dice_as_result() {
  Ok(int.random(6) + 1)
}

pub fn sum_dice_values(a: Int, b: Int) {
  Ok(a + b)
}

//Apostando por funciones de primer nivel y coincidencia de patrones
// puede provocar fácilmente toneladas de sangrías
fn roll_two_dices_without_use() {
  result.try(throw_dice_as_result(), fn(first_dice) {
    result.try(throw_dice_as_result(), fn(second_dice) {
      sum_dice_values(first_dice, second_dice)
    })
  })
}

// La expresión use aún nos permite escribir código que usa devoluciones de llamada.
// pero limpia la sangría excesiva:
// - Una llamada a una función de orden superior va al lado derecho del operador <-
// - Los nombres de los argumentos para la función de devolución de llamada van en el lado izquierdo de
//   el operador <-
// - Todo el código restante en el bloque {} adjunto se convierte en el cuerpo del
//   función de devolución de llamada.
fn roll_two_dices_with_use() {
  use first_dice <- result.try(throw_dice_as_result())
  use second_dice <- result.try(throw_dice_as_result())
  sum_dice_values(first_dice, second_dice)
}

fn more_on_callbacks() {
  let _ = echo roll_two_dices_without_use()
  let _ = echo roll_two_dices_with_use()
  Nil
}

// Desde v1.14.0, la anotación @external también se puede utilizar en tipos externos para
// apúntelos a los tipos Erlang o TypeScript.
pub type DateTime

// Las funciones externas deben anotar un tipo de devolución.
// Esta función sólo está definida en el objetivo de Erlang; compilando en JavaScript
// requeriría un cuerpo alternativo o una anotación @external(javascript, ...).
@external(erlang, "calendar", "local_time")
pub fn now() -> DateTime {
  todo
}

fn showcase_externals() {
  let _ = echo now()
  // #(#(2024, 4, 6), #(14, 4, 16))
  Nil
}

// Constantes a nivel de módulo (compatibles desde v1.0.0, con lista precedida desde v1.16.0)
pub const prime_numbers = [2, 3, 5]
pub const more_primes = [7, 11, ..prime_numbers]

fn showcase_constants() {
  let _ = echo prime_numbers
  let _ = echo more_primes

  // Desde v1.17.0, también puedes usar `todo` dentro de declaraciones constantes:
  // pub const next_constant = todo
  Nil
}

fn showcase_dicts() {
  // Los diccionarios son colecciones de valores clave (stdlib v0.33.0)
  let scores = dict.new()
    |> dict.insert("Alice", 10)
    |> dict.insert("Bob", 15)

  let _ = echo dict.get(scores, "Alice")
  // Ok(10)
  let _ = echo dict.get(scores, "Charlie")
  // Error(Nil)
  Nil
}

fn showcase_bit_arrays() {
  // Las matrices de bits representan secuencias de datos binarios (stdlib v0.32.0)
  let binary_data = <<0x41, 0x42, 0x43>> //"ABC" en ASCII
  let _ = echo binary_data

  // En Gleam, la coincidencia de patrones con `let` debe ser exhaustiva (debe cubrir todos
  // valores posibles). Si un patrón no coincide en tiempo de ejecución, debemos usar
  // `let assert` en lugar de `let`. Si la coincidencia falla, el programa entra en pánico:
  let assert <<first_byte, rest:bytes>> = binary_data
  let _ = echo first_byte // 65
  let _ = echo rest // <<66, 67>>
  Nil
}

// Los tipos opacos son tipos cuyos constructores son privados para el módulo que los define,
// permitiendo la encapsulación y ocultando detalles de implementación.
pub opaque type EncryptedData {
  EncryptedData(value: String)
}

pub fn encrypt(data: String) -> EncryptedData {
  EncryptedData(data <> "_encrypted")
}

fn showcase_opaque_types() {
  let secret = encrypt("my_password")
  // Podemos transmitir el secreto, pero no podemos hacer coincidir el patrón en EncryptedData.
  // o acceda a su campo .value desde fuera de este módulo.
  let _ = echo secret
  Nil
}

fn showcase_panic() {
  // Podemos abortar deliberadamente la ejecución utilizando la palabra clave pánico
  // para hacer que nuestro programa falle inmediatamente
  let three = 3
  let two = 2
  case three == two {
    True -> panic as "The equality operator is broken!"
    False -> "Equality operator works for integers"
  }
  // Llamar a una función que utiliza la palabra clave todo también falla
  // tarea()
}

pub fn homework() {
  todo as "This function is not implemented yet"
}
```

## Lectura adicional

* [Sitio web oficial de Gleam](https://gleam.run/)
* [Tour de idiomas](https://tour.gleam.run/) - Incluye editor de código en vivo
* [Documentación oficial](https://gleam.run/documentation/)
* [Lista increíble de Gleam](https://github.com/gleam-lang/awesome-gleam)
* [Pista de ejercicio para Gleam](https://exercism.org/tracks/gleam)

Los documentos oficiales tienen hojas de referencia para personas familiarizadas con:

* [Elixir](https://gleam.run/cheatsheets/gleam-for-elixir-users)
* [Olmo](https://gleam.run/cheatsheets/gleam-for-elm-users)
* [Erlang](https://gleam.run/cheatsheets/gleam-for-erlang-users)
* [PHP](https://gleam.run/cheatsheets/gleam-for-php-users)
* [Python](https://gleam.run/cheatsheets/gleam-for-python-users)
* [Óxido](https://gleam.run/cheatsheets/gleam-for-rust-users)
