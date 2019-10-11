---
language: dart
contributors:
    - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
translators:
    - ["Jorge Antonio Atempa", "http://www.twitter.com/atempa09"]
filename: dart-es.md
lang: es-es
---

Dart es un recién llegado al ámbito de los lenguajes de programación.
Toma prestado mucho de otros lenguajes principales, con el objetivo de no desviarse demasiado de
su hermano JavaScript. Tal como JavaScript, Dart tiene como objetivo una gran integración en el navegador.

La característica más controvertida de Dart debe ser su escritura opcional.

```dart
import "dart:collection";
import "dart:math" as DM;

// Bienvenido a Aprende Dart en 15 minutos. http://www.dartlang.org/
// Este es un tutorial ejecutable. Puedes ejecutarlo con Dart o en
// el sitio de ¡Try Dart! solo copiando y pegando en http://try.dartlang.org/

// La declaración de función y de método tienen el mismo aspecto. 
// Las funciones pueden estar anidadas. 
// La declaración toma la forma name() {} o name() => expresionEnUnaLinea;
// La declaración de la función de flecha gorda, tiene un retorno implícito 
// para el resultado de la expresión.
example1() {
  nested1() {
    nested2() => print("example1 anidado 1 anidado 2");
    nested2();
  }
  nested1();
}

// Las funciones anónimas no incluyen un nombre.
example2() {
  nested1(fn) {
    fn();
  }
  nested1(() => print("example2 anidado 1"));
}

// Cuando se declara un parámetro de función, la declaración puede incluir el
// número de parámetros que toma la función especificando los nombres de los
// parámetros que lleva.
example3() {
  planA(fn(informSomething)) {
    fn("example3 plan A");
  }
  planB(fn) { // O no declarar el número de parámetros.
    fn("example3 plan B");
  }
  planA((s) => print(s));
  planB((s) => print(s));
}

// Las funciones tienen acceso de cierre a variables externas.
var example4Something = "Example4 anidado 1";
example4() {
  nested1(fn(informSomething)) {
    fn(example4Something);
  }
  nested1((s) => print(s));
}

// La declaración de la clase con un método sayIt, el cual también tiene acceso de cierre
// a la variable exterior como si fuera una función como se ha visto antes.
var example5method = "example5 sayIt";
class Example5Class {
  sayIt() {
    print(example5method);
  }
}
example5() {
  // Crear una instancia anónima de Example5Class y la llamada del método sayIt
  new Example5Class().sayIt();
}

// La declaración de clase toma la forma NombreDeClase { [cuerpoDeClase] }.
// Donde cuerpoDeClase puede incluir métodos de instancia y variables, pero también
// métodos y variables de clase.
class Example6Class {
  var instanceVariable = "Example6 variable de instancia";
  sayIt() {
    print(instanceVariable);
  }
}
example6() {
  new Example6Class().sayIt();
}

// Los métodos y variables de clase son declarados con términos "static".
class Example7Class {
  static var classVariable = "Example7 variable de clase";
  static sayItFromClass() {
    print(classVariable);
  }
  sayItFromInstance() {
    print(classVariable);
  }
}
example7() {
  Example7Class.sayItFromClass();
  new Example7Class().sayItFromInstance();
}

// Las literales son geniales, pero hay una restricción para lo que pueden ser las literales
// fuera de los cuerpos de función/método. Literales en el ámbito exterior de clase
// o fuera de clase tienen que ser constantes. Las cadenas de caracteres y los números
// son constantes por defecto. Pero los arreglos y mapas no lo son. 
// Ellos pueden hacerse constante anteponiendo en la declaración el término "const".
var example8Array = const ["Example8 arreglo constante"],
    example8Map = const {"algunaKey": "Example8 mapa constante"};
example8() {
  print(example8Array[0]);
  print(example8Map["algunaKey"]);
}

// Los bucles en Dart toman la forma estándar para for () {} o ciclos while () {} ,
// ligeramente más moderno for (.. in ..) {}, o llamadas funcionales con muchas
// características soportadas, comenzando con forEach.
var example9Array = const ["a", "b"];
example9() {
  for (var i = 0; i < example9Array.length; i++) {
    print("example9 ciclo for '${example9Array[i]}'");
  }
  var i = 0;
  while (i < example9Array.length) {
    print("example9 ciclo while '${example9Array[i]}'");
    i++;
  }
  for (var e in example9Array) {
    print("example9 ciclo for-in '${e}'");
  }
  example9Array.forEach((e) => print("example9 ciclo forEach '${e}'"));
}

// Para recorrer los caracteres de una cadena o para extraer una subcadena.
var example10String = "ab";
example10() {
  for (var i = 0; i < example10String.length; i++) {
    print("example10 Recorrido de caracteres en la cadena '${example10String[i]}'");
  }
  for (var i = 0; i < example10String.length; i++) {
    print("example10 ciclo de subcadena '${example10String.substring(i, i + 1)}'");
  }
}

// Formato de números Int y double son soportados.
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  print("example11 int ${i}");
  print("example11 double ${d}");
}

// DateTime ofrece aritmética de fecha/hora.
example12() {
  var now = new DateTime.now();
  print("example12 ahora '${now}'");
  now = now.add(new Duration(days: 1));
  print("example12 manana '${now}'");
}

// Expresiones regulares son soportadas.
example13() {
  var s1 = "alguna cadena", s2 = "alguna", re = new RegExp("^s.+?g\$");
  match(s) {
    if (re.hasMatch(s)) {
      print("example13 regexp embona '${s}'");
    } else {
      print("example13 regexp no embona '${s}'");
    }
  }
  match(s1);
  match(s2);
}

// Las expresiones booleanas admiten conversiones implícitas y tipos dinámicos. 
example14() {
  var a = true;
  if (a) {
    print("true, a is $a");
  }
  a = null;
  if (a) {
    print("true, a es $a");
  } else {
    print("false, a es $a"); // corre aquí
  }

  // el tipado dinámico null puede convertirse a bool
  var b; // b es de tipo dinámico
  b = "abc";
  try {
    if (b) {
      print("true, b es $b");
    } else {
      print("false, b es $b");
    }
  } catch (e) {
    print("error, b es $b"); // esto podría ser ejecutado pero consiguió error
  }
  b = null;
  if (b) {
    print("true, b es $b");
  } else {
    print("false, b es $b"); // corre aquí
  }

  // tipado estático null no puede ser convertido a bool
  var c = "abc";
  c = null;
  // compilación fallida
  // if (c) {
  //   print("true, c is $c");
  // } else {
  //   print("false, c is $c");
  // }
}

// try/catch/finally y throw son utilizados para el manejo de excepciones.
// throw toma cualquier objeto como parámetro;
example15() {
  try {
    try {
      throw "Algun error inesperado.";
    } catch (e) {
      print("example15 una excepcion: '${e}'");
      throw e; // Re-throw
    }
  } catch (e) {
    print("example15 atrapa la excepcion que ha sido relanzada: '${e}'");
  } finally {
    print("example15 aún ejecuta finally");
  }
}

// Para ser eficiente cuando creas una cadena larga dinámicamente, usa
// StringBuffer. O podrías unir un arreglo de cadena de caracteres.
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) { sb.write(e); }
  print("example16 cadena de caracteres dinamica creada con "
    "StringBuffer '${sb.toString()}'");
  print("example16 union de arreglo de cadena de caracteres '${a.join()}'");
}

// Las cadenas de caracteres pueden ser concatenadas contando solo 
// con literales una después de la otra sin algún otro operador necesario.
example17() {
  print("example17 "
      "concatenar "
      "cadenas "
      "asi");
}

// Las cadenas de caracteres utilizan comilla simple o comillas dobles como delimitadores
// sin ninguna diferencia entre ambas. Esto proporciona flexibilidad que puede ser efectiva
// para evitar la necesidad de 'escapar' el contenido. Por ejemplo,
// las dobles comillas de los atributos HTML.
example18() {
  print('Example18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

// Las cadenas de caracteres con triple comilla simple o triple comillas dobles
// dividen múltiples lineas e incluyen como delimitador el salto de línea.
example19() {
  print('''Example19 <a href="etc">
Example19 Don't can't I'm Etc
Example19 </a>''');
}

// Las cadenas de caracteres cuentan con una extraordinaria característica
// para la interpolación de caracteres utilizando el operador $
// Con $ { [expresion] }, devolvemos la expresion interpolada.
// $ seguido por el nombre de una variable interpola el contenido de dicha variable.
// $ puede ser escapado con \$ para solo agregarlo a la cadena.
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 \$ interpolation ${s1} or $s2 works.");
}

// Hasta ahora no hemos declarado ningún tipo de dato y los programas
// han funcionado bien. De hecho, los tipos no se toman en cuenta durante 
// el tiempo de ejecución.
// Los tipos incluso pueden estar equivocados y al programa todavía se le dará 
// el beneficio de la duda y se ejecutará como si los tipos no importaran.
// Hay un parámetro de tiempo de ejecución que comprueba los errores de tipo que es
// el modo de verificación, el cuál es útil durante el tiempo de desarrollo,
// pero que también es más lento debido a la comprobación adicional y, por lo tanto
// se evita durante el tiempo de ejecución de la implementación.
class Example21 {
  List<String> _names;
  Example21() {
    _names = ["a", "b"];
  }
  List<String> get names => _names;
  set names(List<String> list) {
    _names = list;
  }
  int get length => _names.length;
  void add(String name) {
    _names.add(name);
  }
}
void example21() {
  Example21 o = new Example21();
  o.add("c");
  print("example21 nombres '${o.names}' y longitud '${o.length}'");
  o.names = ["d", "e"];
  print("example21 nombres '${o.names}' y longitud '${o.length}'");
}

// La herencia de clases toma la forma NombreDeClase extends OtraClase {}.
class Example22A {
  var _name = "¡Algun Nombre!";
  get name => _name;
}
class Example22B extends Example22A {}
example22() {
  var o = new Example22B();
  print("example22 herencia de clase '${o.name}'");
}

// La mezcla de clases también esta disponible y toman la forma de
// NombreDeClase extends AlgunaClase with OtraClase {}.
// Es necesario extender de alguna clase para poder mezclar con otra.
// La clase de plantilla de mixin no puede en este momento tener un constructor.
// Mixin se utiliza principalmente para compartir métodos con clases distantes,
// por lo que la herencia única no interfiere con el código reutilizable.
// Mixins se colocan despues de la palabra "with" durante la declaración de la clase.
class Example23A {}
class Example23Utils {
  addTwo(n1, n2) {
    return n1 + n2;
  }
}
class Example23B extends Example23A with Example23Utils {
  addThree(n1, n2, n3) {
    return addTwo(n1, n2) + n3;
  }
}
example23() {
  var o = new Example23B(), r1 = o.addThree(1, 2, 3),
    r2 = o.addTwo(1, 2);
  print("Example23 addThree(1, 2, 3) results in '${r1}'");
  print("Example23 addTwo(1, 2) results in '${r2}'");
}

// El método constructor de la clase utiliza el mismo nombre de la clase
// y toma la forma de AlgunaClase() : super() {}, donde la parte ": super()"
// es opcional y es utilizado para delegar parametros constantes
// al método constructor de la clase padre o super clase.
class Example24A {
  var _value;
  Example24A({value: "algunValor"}) {
    _value = value;
  }
  get value => _value;
}
class Example24B extends Example24A {
  Example24B({value: "algunOtroValor"}) : super(value: value);
}
example24() {
  var o1 = new Example24B(),
    o2 = new Example24B(value: "aunMas");
  print("example24 llama al método super desde el constructor '${o1.value}'");
  print("example24 llama al método super desde el constructor '${o2.value}'");
}

// Hay un atajo para configurar los parámetros del constructor en el caso de clases más simples.
// Simplemente use el prefijo this.nombreParametro y establecerá el parámetro 
// en una variable de instancia del mismo nombre.
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}
example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("example25 atajo para el constructor '${o.value}' y "
    "'${o.anotherValue}'");
}

// Los parámetros con nombre están disponibles cuando se declaran entre {}.
// El orden de los parámetros puede ser opcional cuando se declara entre {}.
// Los parámetros pueden hacerse opcionales cuando se declaran entre [].
example26() {
  var _name, _surname, _email;
  setConfig1({name, surname}) {
    _name = name;
    _surname = surname;
  }
  setConfig2(name, [surname, email]) {
    _name = name;
    _surname = surname;
    _email = email;
  }
  setConfig1(surname: "Doe", name: "John");
  print("example26 name '${_name}', surname '${_surname}', "
    "email '${_email}'");
  setConfig2("Mary", "Jane");
  print("example26 name '${_name}', surname '${_surname}', "
  "email '${_email}'");
}

// Las variables declaradas con final solo se pueden establecer una vez.
// En el caso de las clases, las variables de instancia final se pueden establecer 
// a través de la constante del parámetro constructor.
class Example27 {
  final color1, color2;
  // Un poco de flexibilidad para establecer variables de instancia finales con la sintaxis
  // que sigue a :
  Example27({this.color1, color2}) : color2 = color2;
}
example27() {
  final color = "orange", o = new Example27(color1: "lilac", color2: "white");
  print("example27 color es '${color}'");
  print("example27 color es '${o.color1}' y '${o.color2}'");
}

// Para importar una librería utiliza la palabra reservada import "rutaLibrería" o si es una biblioteca central,
// import "dart:NombreLibrería". También está el "pub" administrador de paquetes con
// su propia convensión import "package:NombrePaquete".
// Ve import "dart:collection"; al inicio. Las importaciones deben venir antes
// de la delcaración de algún otro código. IterableBase proviene de dart:collection.
class Example28 extends IterableBase {
  var names;
  Example28() {
    names = ["a", "b"];
  }
  get iterator => names.iterator;
}
example28() {
  var o = new Example28();
  o.forEach((name) => print("example28 '${name}'"));
}

// Para el control de flujo tenemos:
// * estandard switch
// * if-else if-else y el operador ternario ..?..:..
// * closures y funciones anonimas
// * sentencias break, continue y return
example29() {
  var v = true ? 30 : 60;
  switch (v) {
    case 30:
      print("example29 sentencia switch");
      break;
  }
  if (v < 30) {
  } else if (v > 30) {
  } else {
    print("example29 sentencia if-else");
  }
  callItForMe(fn()) {
    return fn();
  }
  rand() {
    v = new DM.Random().nextInt(50);
    return v;
  }
  while (true) {
    print("example29 callItForMe(rand) '${callItForMe(rand)}'");
    if (v != 30) {
      break;
    } else {
      continue;
    }
    // Nunca llega aquí.
  }
}

// La sentencia int.parse, convierte de tipo double a int, o simplemente mantener int cuando se dividen los números
// utilizando ~/ como operación. Vamos a jugar un juego de adivinanzas también.
example30() {
  var gn, tooHigh = false,
    n, n2 = (2.0).toInt(), top = int.parse("123") ~/ n2, bottom = 0;
  top = top ~/ 6;
  gn = new DM.Random().nextInt(top + 1); // +1 porque nextInt top es exclusivo
  print("example30 Adivina un número entre 0 y ${top}");
  guessNumber(i) {
    if (n == gn) {
      print("example30 ¡Adivinaste correctamente! El número es ${gn}");
    } else {
      tooHigh = n > gn;
      print("example30 Número ${n} es demasiado "
        "${tooHigh ? 'high' : 'low'}. Intenta nuevamente");
    }
    return n == gn;
  }
  n = (top - bottom) ~/ 2;
  while (!guessNumber(n)) {
    if (tooHigh) {
      top = n - 1;
    } else {
      bottom = n + 1;
    }
    n = bottom + ((top - bottom) ~/ 2);
  }
}

// Los programas tienen un solo punto de entrada en la función principal.
// No se espera que se ejecute nada en el ámbito externo antes de que un programa
// comience a funcionar con su función principal.
// Esto ayuda con una carga más rápida e incluso con una carga lenta 
// de lo que necesita el programa para iniciar.
main() {
  print("Learn Dart in 15 minutes!");
  [example1, example2, example3, example4, example5, example6, example7,
    example8, example9, example10, example11, example12, example13, example14,
    example15, example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25, example26,
    example27, example28, example29, example30
    ].forEach((ef) => ef());
}

```

## Lecturas adicionales

Dart tiene un sitio web muy completo. Cubre referencias de API, tutoriales, artículos y más, incluyendo una
útil sección en línea Try Dart.
[https://www.dartlang.org](https://www.dartlang.org)
[https://try.dartlang.org](https://try.dartlang.org)
