---
nombre: Gleam
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

pub type IdUsuario =
  Int

pub fn main() {
  io.println("Hola desde learnxinyminutes.com!")
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

  // Aritmetica de enteros
  echo 1 + 1
  echo 5 - 1
  echo 5 / 2
  echo 3 * 3
  echo 5 % 2

  // Comparaciones de enteros usando variables para evitar advertencias del compilador
    let dos = 2
    let uno = 1
    let mismo_uno = 1
    let mismo_dos = 2
  let _ = echo dos > uno
  let _ = echo dos < uno
  let _ = echo dos >= uno
  let _ = echo dos <= uno

  // La igualdad sirve para cualquier tipo y se controla estructuralmente, lo que significa que dos
  // Los valores son iguales si tienen la misma estructura y no si están en
  // la misma ubicación de memoria.
  let _ = echo uno == mismo_uno
  // True
  let _ = echo dos != mismo_dos
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

  // Aritmetica de flotantes
  echo 1.0 +. 1.5
  echo 5.0 -. 1.5
  echo 5.0 /. 2.5
  echo 3.0 *. 3.5

  // Comparaciones de flotantes usando variables para evitar advertencias del compilador
    let dos_punto_dos = 2.2
    let uno_punto_tres = 1.3
  let _ = echo dos_punto_dos >. uno_punto_tres
  let _ = echo dos_punto_dos <. uno_punto_tres
  let _ = echo dos_punto_dos >=. uno_punto_tres
  let _ = echo dos_punto_dos <=. uno_punto_tres

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
  io.println("\"X\" marca el lugar")

  // Concatenacion de cadenas
  echo "One " <> "Two"

  // Funciones de cadenas
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

  // Operadores booleanos
  // El || y los operadores && funcionan mediante cortocircuito

  echo True && False
  // False

  echo True && True
  // True

  echo False || False
  // False

  echo False || True
  // True

  // Funciones booleanas
  echo bool.to_string(True)
  // "True"

  //Asignaciones
  let x = "Original valor"
  echo x

  // Asigna `y` al valor de `x`
  let y = x
  echo y

  // Asigne `x` a un nuevo valor
  let x = "New valor"
  echo x

  // La `y` todavía se refiere al valor original.
  echo y

  // En Gleam, los nombres de variables y funciones están escritos en Snake_case.
    let respuesta_del_universo = 42
  echo respuesta_del_universo

    let y_todo_lo_demas = respuesta_del_universo
  // Ahora usar una variable produce una advertencia.

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │     let y_todo_lo_demas = respuesta_del_universo
  //     │       ^^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_y_todo_lo_demas`.

  // Escriba anotaciones

    let _nombre: String = "Gleam"

    let _es_genial: Bool = True

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
    let uno: IdUsuario = 1
  // Consulte el principio del archivo para conocer la definición del tipo UserId.

    let dos: Int = 2

  // Los alias sirven sólo para crear código más legible y más preciso.
  // documentación.
  // En el fondo siguen siendo valores del mismo tipo, por lo que las operaciones
  // todavía trabajo
  echo uno + dos
  // 3

  // Bloques: alcance y valor
  let radio = {
    let valor = 100.0
    valor
  }
  //valor de eco // <- Esto no se compilará porque "valor" está fuera de alcance

  let area = 3.14159 *. radio *. radio
  echo area

  // Utilice bloques para agrupar operaciones en lugar de paréntesis
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  echo n1 != n2
  // True

  // Listas

  // Sobrinos de Scrooge McDuck
    let sobrinos = ["Hugo", "Paco", "Luis"]
  echo sobrinos
  // ["Hugo", "Paco", "Luis"]

  // Anteponer inmutablemente para que la lista original no cambie
  echo ["Donald", ..sobrinos]
  // ["Donald", "Hugo", "Paco", "Luis"]

  // Algunas funciones de biblioteca estándar para listas

  list.each(sobrinos, io.println)
  // Hugo
  // Paco
  // Luis

  echo list.drop(sobrinos, 2)
  // ["Luis"]

  mas_ejemplos()
  mas_ejemplos_funciones()
  ejemplos_tipos_genericos()
  demo_tuberias()
  etiquetas_en_llamadas()
  mostrar_control_de_flujo()
  mas_sobre_recursion()
  mas_sobre_coincidencia_patrones()
  mostrar_tipos()
  mas_sobre_tipos()
  mas_sobre_callbacks()
  mostrar_externos()
  mostrar_constantes()
  mostrar_dicts()
  mostrar_bit_arrays()
  mostrar_tipos_opacos()
  mostrar_panic()
}

// La palabra clave fn se utiliza para definir nuevas funciones.
fn multiplicar(a: Int, b: Int) -> Int {
  // Sin devolución explícita
  // Se devuelve la última expresión.
  a * b
}

// Las funciones duplicar y multiplicar se definen sin la palabra clave pub.
// Esto las convierte en funciones privadas, solo se pueden utilizar dentro de este módulo.
// Si otro módulo intentara usarlos, se produciría un error del compilador.
fn duplicar(a: Int) -> Int {
  multiplicar(a, 2)
}

// Solo se exportan funciones públicas y se pueden llamar desde fuera del módulo.

// Las anotaciones de tipo son opcionales para los argumentos de funciones y los valores de retorno.
// pero se consideran buenas prácticas para mayor claridad y para fomentar
// diseño intencional y reflexivo.

pub fn es_bisiesto(anio: Int) -> Bool {
  { anio % 4 == 0 } && { { anio % 100 != 0 } || { anio % 400 == 0 } }
}

fn mas_ejemplos() {
  //La depuración también devuelve un valor, por lo que su salida es el valor de retorno de
  // esta función
  echo duplicar(10)
  // 20
  echo es_bisiesto(2000)
  // True
}

// Gleam admite funciones de orden superior:
// Se pueden asignar a variables y pasar como argumentos a otras funciones.
// o incluso ser devuelto como valores de bloques u otras funciones
fn llamar_func_con_entero(func: fn(Int) -> Int, valor: Int) -> Int {
  func(valor)
}

fn mas_ejemplos_funciones() -> Int {
  echo llamar_func_con_entero(duplicar, 2)
  // 4

  let cuadrado = fn(x: Int) -> Int { x * x }
  echo cuadrado(3)
  // 9

  // Llamar a una función anónima inmediatamente después de definirla
  echo fn(x: Int) { x + 1 }(1)

  // Ejemplo de cierre
  let crear_sumadora = fn(n: Int) -> fn(Int) -> Int {
    fn(argumento: Int) -> Int { argumento + n }
  }

  let sumadora_de_cincos = crear_sumadora(5)
  echo sumadora_de_cincos(10)
  // 15

  // Las funciones anónimas se pueden utilizar indistintamente con funciones con nombre.
  echo llamar_func_con_entero(fn(x: Int) -> Int { x + 100 }, 900)
  // 1000

  // Creemos un decorador de funciones.
  let dos_veces = fn(funcion_envuelta: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argumento: Int) -> Int { funcion_envuelta(funcion_envuelta(argumento)) }
  }
    let cuadruple = dos_veces(duplicar)
    echo cuadruple(1)

    let cuadruple_2 = fn(a: Int) -> Int { multiplicar(4, a) }
  echo cuadruple_2(2)
  // 8

  // Una captura de función es una sintaxis abreviada para crear funciones anónimas.
  // que toma un argumento e inmediatamente llama a otra función con ese
  // argumento
    let cuadruple_3 = multiplicar(4, _)
  echo cuadruple_3(4)
  // 16
}

// Las funciones genéricas se admiten mediante variables de tipo.
fn doble_generico(func: fn(valor) -> valor, argumento: valor) -> valor {
  func(func(argumento))
}

// En doble_generico el valor era la variable de tipo.
// En decorador_doble_generico the_type es la variable de tipo.
// Como en cualquier otra variable, puedes elegir el nombre.
fn decorador_doble_generico(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argumento: the_type) -> the_type { func(func(argumento)) }
}

fn ejemplos_tipos_genericos() {
  let duplicar_enteros = fn(a: Int) -> Int { a * 2 }
  let duplicar_flotantes = fn(a: Float) -> Float { a *. 2.0 }
  echo doble_generico(duplicar_enteros, 3)
  echo doble_generico(duplicar_flotantes, 3.0)

  let cuadruple_enteros = decorador_doble_generico(duplicar_enteros)
  let cuadruple_flotantes = decorador_doble_generico(duplicar_flotantes)
  echo cuadruple_enteros(1)
  // 4
  echo cuadruple_flotantes(1.0)
  // 4.0
}

// El operador de tubería de Gleam |> toma el resultado de la expresión a su izquierda
// y lo pasa como argumento a la función de su derecha.
fn demo_tuberias() {
  // Seamos honestos: quieres usar Gleam solo para este genial operador, ¿verdad?
  ["hola", "mundo"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> echo

  // Mucho más limpio que esto, ¿verdad?
  echo 
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["hola", "mundo"], " "), ["!"]),
      ),
    )

  //Solución al primer problema del Proyecto Euler:
  // URL: https://projecteuler.net/problem=1
  // Descripcion: Encuentra la suma de todos los multiplos de 3 y 5 por debajo de 1000.
  // Usando int.range de gleam/int para iterar de forma recursiva.
  int.range(from: 1, to: 999, with: 0, run: fn(acum, n) {
    case n % 3 == 0 || n % 5 == 0 {
      True -> acum + n
      False -> acum
    }
  })
  |> int.to_string
  |> fn(suma_como_texto: String) {
    "Solucion al problema #1 de Project Euler: " <> suma_como_texto
  }
  |> echo
  // Solucion al problema #1 de Project Euler: 233168
}

// Se pueden agregar etiquetas antes de cada argumento.
fn llamar_func_con_entero_etiquetas(
  func funcion_pasada: fn(Int) -> Int,
  valor n: Int,
) -> Int {
  funcion_pasada(n)
}

// La etiqueta y el argumento pueden tener el mismo nombre.
fn sumar_uno(numero numero: Int) -> Int {
  numero + 1
}

fn sumar_dos_enteros(primero n: Int, segundo m: Int) -> Int {
  n + m
}

fn etiquetas_en_llamadas() -> Int {
  // Como estamos etiquetando los argumentos, podemos cambiar el orden.
  // si queremos
  echo llamar_func_con_entero_etiquetas(valor: 8, func: duplicar)
  echo sumar_uno(numero: 1)
  // 2
  echo string.contains(does: "tema", contain: "te")
  // True
  // Los argumentos sin etiquetar deben ir primero
  echo sumar_dos_enteros(2, segundo: 2)
  // 4
}

fn mostrar_control_de_flujo() {
  // Caso de uso si desea utilizar la coincidencia de patrones para
  // seleccione qué código ejecutar.
  // Gleam se asegurará de que todos los valores posibles estén cubiertos
  // mediante la realización de controles exhaustivos.
  // De lo contrario, obtendrá errores de compilación.
  let cachorros = ["Bear", "Frisco", "Ranger"]
  let conteo = list.length(of: cachorros)
  {
    "Tenemos "
    <> int.to_string(conteo)
    <> " "
    <> // El guión bajo coincide con cualquier otro valor.
    case conteo {
      1 -> "cachorro"
      _ -> "cachorros"
    }
  }
  |> echo

  // Gleam permite que los patrones en expresiones de caso también asignen variables.
  {
    "Cantidad de cachorros: "
    <> case list.length(cachorros) {
      0 -> "Ninguno."
      1 -> "Solo uno."
      otra_conteo -> "Tantos como " <> int.to_string(otra_conteo) <> " cachorros."
    }
  }
  |> echo

  // Considere que los lenguajes BEAM son funcionales en el diseño y Gleam no es una excepción.
  // por lo que no hay construcciones if, for o while disponibles.

  // Utilice la coincidencia de patrones para condicionales
  let respuesta = 42
  case respuesta == 42 {
    True -> {
      echo "Esta es la respuesta del universo."
    }
    False -> {
      echo "Esta es la respuesta de otra cosa."
    }
  }

  // Utilice recursividad en lugar de bucle
  del_uno_al_diez(1)
}

// función recursiva
fn del_uno_al_diez(n: Int) {
  echo n
  case n {
    10 -> Nil
    _ -> del_uno_al_diez(n + 1)
  }
}

// Para evitar el agotamiento de la memoria debido a la creación excesiva
//apilar marcos al llamar funciones de forma recursiva, Gleam admite
// "optimización de llamada final", lo que significa que el compilador puede reutilizar
// el marco de pila para la función actual si se realiza una llamada a la función
// lo último que hace la función.

pub fn fibonacci(x: Int) -> Int {
  // La función pública llama a la función recursiva de cola privada.
  bucle_fibonacci(x, 1)
}

fn bucle_fibonacci(x: Int, acumulador: Int) -> Int {
  case x {
    1 -> acumulador

    // Lo último que hace esta función es llamarse a sí misma.
    // En la lección anterior lo último que hizo fue multiplicar dos enteros
    _ -> bucle_fibonacci(x - 1, acumulador + x)
  }
}

// Gleam admite la coincidencia de patrones del primer elemento y el resto
// de una lista con el patrón [x, ..y] dentro de una expresión de caso.
fn invertir_lista(la_lista: List(valor)) -> List(valor) {
  case la_lista {
    [cabecera, ..cola] -> list.flatten([invertir_lista(cola), [cabecera]])
    [] -> []
  }
}

fn mas_sobre_recursion() {
  echo fibonacci(10)
  // 55
  echo invertir_lista([1, 2, 3])
}

fn mas_sobre_coincidencia_patrones() {
  // Cuando se comparan patrones en cadenas, el operador <> coincide en cadenas
  // con un prefijo específico y asigna el recordatorio a una variable
    let lucia = "Hola, Lucia"
  let _ = echo case lucia {
    "Hola, " <> nombre -> "Saludos para " <> nombre
    _ -> "Puede que no haya saludos"
  }

  // Se admiten patrones alternativos, por lo que se utiliza la misma cláusula.
  // para múltiples valores
    let mes = 2
    let anio = 2024
  let numero_de_dias = case mes {
    2 ->
      case es_bisiesto(anio) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  echo "Numero de dias: " <> int.to_string(numero_de_dias)
  // 29

  // Guardias en coincidencia de patrones:
  // Cuando se utiliza la palabra clave if, una expresión debe evaluarse como Verdadera
  // para que el patrón coincida.
  let lista_empieza_con = fn(la_lista: List(valor), el_valor: valor) -> Bool {
    case la_lista {
      [cabecera, ..] if cabecera == el_valor -> True
      _ -> False
    }
  }
  echo lista_empieza_con([10, 20, 30], 10)
  // True
}

pub type Genero {
  Masculino
  Femenino
  Otro
}

// Registros:
// - Variantes de soporte
// - Cada variante es similar a una estructura con campos.
pub type Forma {
  Rectangulo(base: Float, altura: Float)
  Triangulo(base: Float, altura: Float)
}

// Los registros con una variante se parecen a estructuras
pub type Punto {
  Punto(x: Float, y: Float)
}

fn mostrar_tipos() {
  //Tuplas:
  // - Puede mezclar elementos de diferentes tipos.
  // - Su tipo es implícito, p.e. #{1, "Hola"} es de tipo #{Int, String}
  // - Se puede acceder a sus elementos mediante índices numéricos.
  let tupla_01 = #(1, "Ferris", "rustaceo", True)
  let tupla_02 = #(1, "Lucia", "estrella_de_mar", True)
  echo tupla_01
  echo tupla_01.0
  // 1
  echo tupla_02.1
  // lucía
  let #(_, nombre, especie, _) = tupla_01
  echo nombre <> " el " <> especie

  // Coincidencia de patrones con tuplas, incluida la asignación de variables
  let _ = imprimir_estado_mascota(tupla_02)

  // Usar un tipo personalizado con coincidencia de patrones
    let genero = Otro
  let _ = echo genero_a_texto(genero)

  // Usando registros
  let rectangulo_1 = Rectangulo(base: 10.0, altura: 20.0)
  echo rectangulo_1.altura
  // 10.3

  let punto_1 = Punto(x: 3.2, y: 4.3)
  echo punto_1

  // Actualizar un registro
  let punto_2 = Punto(..punto_1, y: 5.7)
  echo punto_2

  // En Gleam, los valores no admiten valores NULL.
  // Nil es el único valor de este tipo.
  let alguna_variable = Nil
    let resultado = io.println("Hola!")
  echo alguna_variable == resultado
  // True
}

fn imprimir_estado_mascota(mascota: #(Int, String, String, Bool)) {
  case mascota {
    #(_, nombre, _, True) -> echo nombre <> " es una mascota."
    #(_, nombre, _, False) -> echo nombre <> " no es una mascota."
  }
}

fn genero_a_texto(genero: Genero) -> String {
  case genero {
    Masculino -> "Hombre"
    Femenino -> "Mujer"
    Otro -> "Indeterminado"
  }
}

pub type Mineral {
  Oro
  Plata
  Cobre
}

// Tipos personalizados genéricos con tipos contenidos como parámetros
pub type Pureza(tipo_interno) {
  Puro(tipo_interno)
  Impuro(tipo_interno)
}

pub type Bebida {
  Agua
  Jugo
}

// Tipos personalizados existentes de los módulos gleam/option y gleam/result
// facilitar el trabajo con valores que aceptan valores NULL y el manejo de posibles errores
pub type Persona {
  Persona(nombre: String, apodo: Option(String))
}

pub type ErrorDado {
  ValorDadoFueraDeRango
}

fn valor_dado_verificado(valor: Int) -> Result(Int, ErrorDado) {
  case valor {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(valor)
    _ -> Error(ValorDadoFueraDeRango)
  }
}

fn duplicar_valor_dado(valor: Int) -> Result(Int, ErrorDado) {
  case valor {
    1 | 2 | 3 -> Ok(valor * 2)
    _ -> Error(ValorDadoFueraDeRango)
  }
}

fn mas_sobre_tipos() {
    let muestra_mineral_01: Pureza(Mineral) = Puro(Oro)
    let muestra_mineral_02 = Impuro(Plata)
  echo muestra_mineral_01
  echo muestra_mineral_02

  // Un vaso puede estar vacío o no.
    let vaso_01: Option(Bebida) = Some(Agua)
    let vaso_02 = None
  echo vaso_01
  echo vaso_02

  // Una persona puede tener un apodo o no.
    let persona_01 = Persona(nombre: "Juan", apodo: Some("El Destripador"))
    let persona_02 = Persona(nombre: "Martin", apodo: None)
  echo persona_01
  echo persona_02

  // Trabajar con funciones que devuelven valores de tipo Resultado
    let dado_01 = 5
  case valor_dado_verificado(dado_01) {
    Ok(valor_verificado) ->
      echo "El valor de " <> int.to_string(valor_verificado) <> " es correcto."
    Error(ValorDadoFueraDeRango) ->
      echo "El valor del dado esta fuera de rango"
  }

  // Intentemos duplicar el valor si el valor resultante sigue siendo
  // un número en cualquiera de los lados del dado.
  // De lo contrario, pongamos el valor máximo.
  2
  |> valor_dado_verificado
  |> result.try(duplicar_valor_dado)
  |> result.unwrap(or: 6)
  |> echo
}

pub fn lanzar_dado_como_resultado() {
  Ok(int.random(6) + 1)
}

pub fn sumar_valores_dados(a: Int, b: Int) {
  Ok(a + b)
}

//Apostando por funciones de primer nivel y coincidencia de patrones
// puede provocar fácilmente toneladas de sangrías
fn lanzar_dos_dados_sin_use() {
    result.try(lanzar_dado_como_resultado(), fn(primer_dado) {
        result.try(lanzar_dado_como_resultado(), fn(segundo_dado) {
            sumar_valores_dados(primer_dado, segundo_dado)
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
fn lanzar_dos_dados_con_use() {
    use primer_dado <- result.try(lanzar_dado_como_resultado())
    use segundo_dado <- result.try(lanzar_dado_como_resultado())
        sumar_valores_dados(primer_dado, segundo_dado)
}

fn mas_sobre_callbacks() {
  let _ = echo lanzar_dos_dados_sin_use()
  let _ = echo lanzar_dos_dados_con_use()
  Nil
}

// Desde v1.14.0, la anotación @external también se puede utilizar en tipos externos para
// apúntelos a los tipos Erlang o TypeScript.
pub type FechaHora

// Las funciones externas deben anotar un tipo de devolución.
// Esta función sólo está definida en el objetivo de Erlang; compilando en JavaScript
// requeriría un cuerpo alternativo o una anotación @external(javascript, ...).
@external(erlang, "calendar", "local_time")
pub fn ahora() -> FechaHora {
  todo
}

fn mostrar_externos() {
  let _ = echo ahora()
  // #(#(2024, 4, 6), #(14, 4, 16))
  Nil
}

// Constantes a nivel de módulo (compatibles desde v1.0.0, con lista precedida desde v1.16.0)
pub const numeros_primos = [2, 3, 5]
pub const mas_primos = [7, 11, ..numeros_primos]

fn mostrar_constantes() {
  let _ = echo numeros_primos
  let _ = echo mas_primos

  // Desde v1.17.0, también puedes usar `todo` dentro de declaraciones constantes:
  // pub const next_constant = todo
  Nil
}

fn mostrar_dicts() {
  // Los diccionarios son colecciones de valores clave (stdlib v0.33.0)
  let puntajes = dict.new()
    |> dict.insert("Alicia", 10)
    |> dict.insert("Beto", 15)

    let _ = echo dict.get(puntajes, "Alicia")
  // Ok(10)
    let _ = echo dict.get(puntajes, "Carlos")
  // Error(Nil)
  Nil
}

fn mostrar_bit_arrays() {
  // Las matrices de bits representan secuencias de datos binarios (stdlib v0.32.0)
  let datos_en_binario = <<0x41, 0x42, 0x43>> //"ABC" en ASCII
  let _ = echo datos_en_binario

  // En Gleam, la coincidencia de patrones con `let` debe ser exhaustiva (debe cubrir todos
  // valores posibles). Si un patrón no coincide en tiempo de ejecución, debemos usar
  // `let assert` en lugar de `let`. Si la coincidencia falla, el programa entra en pánico:
  let assert <<primer_byte, resto:bytes>> = datos_en_binario
  let _ = echo primer_byte // 65
  let _ = echo resto // <<66, 67>>
  Nil
}

// Los tipos opacos son tipos cuyos constructores son privados para el módulo que los define,
// permitiendo la encapsulación y ocultando detalles de implementación.
pub opaque type DatoCifrado {
  DatoCifrado(valor: String)
}

pub fn cifrar(dato: String) -> DatoCifrado {
  DatoCifrado(dato <> "_cifrado")
}

fn mostrar_tipos_opacos() {
    let secreto = cifrar("mi_contrasena")
  // Podemos transmitir el secreto, pero no podemos hacer coincidir el patrón en DatoCifrado.
  // o acceda a su campo .valor desde fuera de este módulo.
  let _ = echo secreto
  Nil
}

fn mostrar_panic() {
  // Podemos abortar deliberadamente la ejecución utilizando la palabra clave pánico
  // para hacer que nuestro programa falle inmediatamente
    let tres = 3
    let dos = 2
    case tres == dos {
    True -> panic as "El operador de igualdad esta roto!"
    False -> "El operador de igualdad funciona para enteros"
  }
  // Llamar a una función que utiliza la palabra clave todo también falla
  // tarea()
}

pub fn tarea() {
  todo as "Esta funcion aun no esta implementada"
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
