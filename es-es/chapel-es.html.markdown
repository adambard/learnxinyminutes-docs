---
language: chapel
filename: learnchapel.chpl
contributors:
    - ["Ian J. Bertolacci", "https://www.cs.arizona.edu/~ianbertolacci/"]
    - ["Ben Harshbarger", "https://github.com/benharsh/"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
---

Puede leer todo sobre Chapel en [el sitio web oficial de Chapel de Cray](https://chapel-lang.org).
En resumen, Chapel es un lenguaje de programación paralela, código abierto, de alta productividad
desarrolladp en Cray Inc. y está diseñado para ejecutarse en PC multi-nucleos,
así como en supercomputadoras multi-kilocore.

Puede encontrar más información y asistencia al final de este documento.

```chapel
// Los comentarios son de estilo de la familia C

// comentario de una línea
/*
 comentario de múltiples lineas
*/

// Impresión básica

write("Hola, ");
writeln("Mundo!");

// write y writeln pueden tomar una lista de cosas para imprimir.
// Cada cosa está impresa justo al lado de las demás, ¡así que incluye espacios!
writeln("hay ", 3, " comas (\",\") en esta línea de código");

// Diferentes canales de salida:
stdout.writeln("Esto va a la salida estándar, al igual que lo hace writeln()");
stderr.writeln("Esto va al error estándar");


// Las variables no tienen que escribirse explícitamente
// mientras el compilador pueda determinar el tipo que contendrá.

// 10 es un entero, asi que myVar es explícitamente un entero
var myVar = 10;
myVar = -10;
var mySecondVar = myVar;
// var anError; sería un error en tiempo de compilación

// Podemos (y debemos) escribir cosas explícitamente.
var myThirdVar: real;
var myFourthVar: real = -1.234;
myThirdVar = myFourthVar;

// Tipos

// Hay varios tipos básicos.
var myInt: int = -1000; // Enteros firmados
var myUint: uint = 1234; // Enteros sin-firmar
var myReal: real = 9.876; // Números de punto flotante
var myImag: imag = 5.0i; // Números imaginarios
var myCplx: complex = 10 + 9i; // Números complejos
myCplx = myInt + myImag; // Otra manera de formar números complejos
var myBool: bool = false; // Booleanos
var myStr: string = "Una cadena..."; // Cadenas
var singleQuoteStr = 'Otra cadena...'; // Cadena literal con comillas simples

// Algunos tipos pueden tener tamaños.
var my8Int: int(8) = 10; // Entero de 8 bit (one byte);
var my64Real: real(64) = 1.516; // Real de 64 bit (8 bytes)

// Conversion de tipos.
var intFromReal = myReal : int;
var intFromReal2: int = myReal : int;

// Alias de tipo.
type chroma = int;        // Tipo de un solo tono
type RGBColor = 3*chroma; // Tipo que representa un color completo
var black: RGBColor = (0,0,0);
var white: RGBColor = (255, 255, 255);

// Constantes y Parámetros

// una variable const es una constante y no se puede cambiar después de
// establecerla en tiempo de ejecución.
const almostPi: real = 22.0/7.0;

// Un parámetro es una constante cuyo valor debe conocerse estáticamente
// en tiempo de compilación.

param compileTimeConst: int = 16;

// El modificador de configuración permite establecer valores en la línea de comando.
// Establece valores con --varCmdLineArg=Value o --varCmdLineArg Value en tiempo de ejecución.
config var varCmdLineArg: int = -123;
config const constCmdLineArg: int = 777;

// config param se puede configurar en tiempo de compilación.
// Establece valores con --set paramCmdLineArg=value  en tiempo de compilación.
config param paramCmdLineArg: bool = false;
writeln(varCmdLineArg, ", ", constCmdLineArg, ", ", paramCmdLineArg);

// Referencias

// ref funciona de manera muy similar a una referencia en C ++. En Chapel,
// no se puede hacer una referencia como alias a una variable distinta
// de la variable con la que se inicializa.

// Aquí, refToActual se refiere a actual.
var actual = 10;
ref refToActual = actual;
writeln(actual, " == ", refToActual); // imprime el mismo valor
actual = -123; // modificar actual (a lo que refToActual se refiere)
writeln(actual, " == ", refToActual); // imprime el mismo valor
refToActual = 99999999; //  modificar a qué se refiere refToActual (que es actual)
writeln(actual, " == ", refToActual); // imprime el mismo valor

// Operadores

// Operadores matemáticos:
var a: int, thisInt = 1234, thatInt = 5678;
a = thisInt + thatInt;  // Adicción
a = thisInt * thatInt;  // Multiplicación
a = thisInt - thatInt;  // Substracción
a = thisInt / thatInt;  // División
a = thisInt ** thatInt; // Exponenciación
a = thisInt % thatInt;  // residuo (módulo)

// Operadores logicos:
var b: bool, thisBool = false, thatBool = true;
b = thisBool && thatBool; // Lógico y
b = thisBool || thatBool; // Lógico o
b = !thisBool;            // Lógico negación

// Operadores relacionales:
b = thisInt > thatInt;           // Mas grande que
b = thisInt >= thatInt;          // Mas grande o igual que
b = thisInt < a && a <= thatInt; // Menor que, y, Menor o igual que
b = thisInt != thatInt;          // No es igual a
b = thisInt == thatInt;          // es igual a

// Operadores bit a bit:
a = thisInt << 10;     // Desplazamiento de bit izquierdo por 10 bits;
a = thatInt >> 5;      // Desplazamiento de bit derecho por 5 bits;
a = ~thisInt;          // Negación bit a bit
a = thisInt ^ thatInt; // bit a bit exclusivo o

// Operadores de asignación compuesta:
a += thisInt;          // Adición-igual (a = a + thisInt;)
a *= thatInt;          // Multiplicación-igual (a = a * thatInt;)
b &&= thatBool;        // Lógico e igual (b = b && thatBool;)
a <<= 3;               // Desplazamiento a la izquierda igual (a = a << 10;)

// A diferencia de otros lenguajes de familia C, no hay operadores de
// pre / post-incremento / decremento, tales como:
//
// ++j, --j, j++, j--

// Operador de intercambio:
var old_this = thisInt;
var old_that = thatInt;
thisInt <=> thatInt; // Intercambia los valores de thisInt y thatInt
writeln((old_this == thatInt) && (old_that == thisInt));

// También se pueden definir sobrecargas del operador, como veremos con los procedimientos.

// Tuplas

// Las tuplas pueden ser del mismo tipo o de diferentes tipos.
var sameTup: 2*int = (10, -1);
var sameTup2 = (11, -6);
var diffTup: (int,real,complex) = (5, 1.928, myCplx);
var diffTupe2 = (7, 5.64, 6.0+1.5i);

// Se puede acceder a las tuplas usando corchetes o paréntesis,
// y están indexadas en base 1.
writeln("(", sameTup[1], ",", sameTup(2), ")");
writeln(diffTup);

// Las tuplas también se pueden escribir.
diffTup(1) = -1;

// Los valores de tupla se pueden expandir a sus propias variables.
var (tupInt, tupReal, tupCplx) = diffTup;
writeln(diffTup == (tupInt, tupReal, tupCplx));

// También son útiles para imprimit una lista de variables,
// como es común en la depuración.
writeln((a,b,thisInt,thatInt,thisBool,thatBool));

// Flujo de control

// if - then - else funciona como cualquier otro lenguaje de la familia C.
if 10 < 100 then
  writeln("All is well");

if -1 < 1 then
  writeln("Continuando creyendo en la realidad");
else
  writeln("¡Envia un matemático!, algo está mal");

// Puedes usar paréntesis si lo prefieres.
if (10 > 100) {
  writeln("El Universo está roto, Por favor reinicie el universo.");
}

if a % 2 == 0 {
  writeln(a, " es par.");
} else {
  writeln(a, " es impar.");
}

if a % 3 == 0 {
  writeln(a, " es divisible entre 3.");
} else if a % 3 == 1 {
  writeln(a, " es divisible entre 3 con un residuo de 1.");
} else {
  writeln(b, " es divisible entre 3 con un residuo de 2.");
}

// Ternario:  if - then - else en una declaración.
var maximum = if thisInt < thatInt then thatInt else thisInt;

// las declaraciones select son muy parecidas a las declaraciones switch
// en otros idiomas. Sin embargo, las declaraciones select no caen
// en cascada como en C o Java.
var inputOption = "anOption";
select inputOption {
  when "anOption" do writeln("Escoge 'anOption'");
  when "otherOption" {
    writeln("Escoge 'otherOption'");
    writeln("Que tiene un cuerpo");
  }
  otherwise {
    writeln("Cualquier otra entrada");
    writeln("El caso otherwise no necesita hacerse si el cuerpo es de una línea");
  }
}

// Los bucles while y do-while también se comportan como sus contrapartes en C.
var j: int = 1;
var jSum: int = 0;
while (j <= 1000) {
  jSum += j;
  j += 1;
}
writeln(jSum);

do {
  jSum += j;
  j += 1;
} while (j <= 10000);
writeln(jSum);

// Los bucles for son muy parecidos a los de Python porque iteran en un rango.
// Los rangos (como la expresión 1..10 a continuación) son un objeto de primera clase
// en Chapel, y como tal pueden almacenarse en variables.
for i in 1..10 do write(i, ", ");
writeln();

var iSum: int = 0;
for i in 1..1000 {
  iSum += i;
}
writeln(iSum);

for x in 1..10 {
  for y in 1..10 {
    write((x,y), "\t");
  }
  writeln();
}

// Rangos y Dominios

// Los bucles y matrices utilizan rangos y dominios para definir un conjunto de índices
// que se pueden iterar. Los rangos son índices enteros unidimensionales, mientras
// que los dominios pueden ser multidimensionales y representan índices
// de diferentes tipos.

// Son tipos ciudadanos de primera clase y pueden asignarse a variables.
var range1to10: range = 1..10;  // 1, 2, 3, ..., 10
var range2to11 = 2..11; // 2, 3, 4, ..., 11
var rangeThisToThat: range = thisInt..thatInt; // usando variables
var rangeEmpty: range = 100..-100; // esto es válido pero no contiene índices

// Los rangos pueden ser ilimitados.
var range1toInf: range(boundedType=BoundedRangeType.boundedLow) = 1.. ; // 1, 2, 3, 4, 5, ...
var rangeNegInfTo1 = ..1; // ..., -4, -3, -2, -1, 0, 1

// Los rangos se pueden andar (y revertir) utilizando el operador by.
var range2to10by2: range(stridable=true) = 2..10 by 2; // 2, 4, 6, 8, 10
var reverse2to10by2 = 2..10 by -2; // 10, 8, 6, 4, 2

var trapRange = 10..1 by -1; // No te dejes engañar, esto sigue siendo un rango vacío
writeln("Size of range ", trapRange, " = ", trapRange.length);

// Note: range(boundedType= ...) and range(stridable= ...) solo son necesarios
// si escribimos explícitamente la variable.

// El punto final de un rango se puede determinar utilizando el operador de conteo (#).
var rangeCount: range = -5..#12; // intervalo de -5 to 6

// Los operadores pueden ser mixtos.
var rangeCountBy: range(stridable=true) = -5..#12 by 2; // -5, -3, -1, 1, 3, 5
writeln(rangeCountBy);

// Se pueden consultar las propiedades del rango.
// En este ejemplo, imprime el primer índice, el último índice, el número de índices,
// el paso y si 2 se incluye en el rango.
writeln((rangeCountBy.first, rangeCountBy.last, rangeCountBy.length,
           rangeCountBy.stride, rangeCountBy.member(2)));

for i in rangeCountBy {
  write(i, if i == rangeCountBy.last then "\n" else ", ");
}

// Los dominios rectangulares se definen usando la misma sintaxis de rango,
// pero se requiere que estén delimitados (a diferencia de los rangos).
var domain1to10: domain(1) = {1..10};        // 1D domain from 1..10;
var twoDimensions: domain(2) = {-2..2,0..2}; // 2D domain over product of ranges
var thirdDim: range = 1..16;
var threeDims: domain(3) = {thirdDim, 1..10, 5..10}; // using a range variable

// Los dominios también pueden ser redimensionados
var resizedDom = {1..10};
writeln("antes, resizedDom = ", resizedDom);
resizedDom = {-10..#10};
writeln("despues, resizedDom = ", resizedDom);

// Los índices pueden iterarse como tuplas.
for idx in twoDimensions do
  write(idx, ", ");
writeln();

// Estas tuplas también pueden ser deconstruidas.
for (x,y) in twoDimensions {
  write("(", x, ", ", y, ")", ", ");
}
writeln();

// Los dominios asociativos actúan como conjuntos.
var stringSet: domain(string); // empty set of strings
stringSet += "a";
stringSet += "b";
stringSet += "c";
stringSet += "a"; // Redundant add "a"
stringSet -= "c"; // Remove "c"
writeln(stringSet.sorted());

// Los dominios asociativos también pueden tener una sintaxis literal
var intSet = {1, 2, 4, 5, 100};

// Tanto los rangos como los dominios se pueden dividir para producir un rango
// o dominio con la intersección de los índices.
var rangeA = 1.. ; // range from 1 to infinity
var rangeB =  ..5; // range from negative infinity to 5
var rangeC = rangeA[rangeB]; // resulting range is 1..5
writeln((rangeA, rangeB, rangeC));

var domainA = {1..10, 5..20};
var domainB = {-5..5, 1..10};
var domainC = domainA[domainB];
writeln((domainA, domainB, domainC));

// Matrices

// Las matrices son similares a otros lenguajes.
// Sus tamaños son definidos usndo dominions que repretsenten sus indices.
var intArray: [1..10] int;
var intArray2: [{1..10}] int; // equivalent

// Pueden ser accedidos usando brackets o paréntesis
for i in 1..10 do
  intArray[i] = -i;
writeln(intArray);

// No podemos acceder a intArray[0] porque existe fuera del conjunto de índices,
// {1..10}, que definimos al principio.
// intArray [11] es ilegal por la misma razón.
var realDomain: domain(2) = {1..5,1..7};
var realArray: [realDomain] real;
var realArray2: [1..5,1..7] real;   // equivalent
var realArray3: [{1..5,1..7}] real; // equivalent

for i in 1..5 {
  for j in realDomain.dim(2) {   // Solo use la segunda dimensión del dominio
    realArray[i,j] = -1.61803 * i + 0.5 * j;  // Acceso usando la lista de índice
    var idx: 2*int = (i,j);                   // Nota: 'índice' es una palabra reservada
    realArray[idx] = - realArray[(i,j)];      // Indice usando tuplas
  }
}

// Las matrices tienen dominios como miembros y pueden ser iterados de manera normal.
for idx in realArray.domain {  // De nuevo, idx es una tupla 2*int
  realArray[idx] = 1 / realArray[idx[1], idx[2]]; // Acceso por tupla y lista
}

writeln(realArray);

// Los valores de una matriz también se pueden iterar directamente.
var rSum: real = 0;
for value in realArray {
  rSum += value; // Read a value
  value = rSum;  // Write a value
}
writeln(rSum, "\n", realArray);

// Las matrices asociativas (diccionarios) se pueden crear utilizando dominios asociativos.
var dictDomain: domain(string) = { "one", "two" };
var dict: [dictDomain] int = ["one" => 1, "two" => 2];
dict["three"] = 3; // Adiciona 'three' a 'dictDomain' implícitamente
for key in dictDomain.sorted() do
  writeln(dict[key]);

// Las matrices se pueden asignar entre sí de diferentes maneras.
// Estos arreglos se usarán en el ejemplo.

var thisArray : [0..5] int = [0,1,2,3,4,5];
var thatArray : [0..5] int;

// Primero, simplemente asigna uno al otro. Esto copia esta matriz en
// thatArray, en lugar de simplemente crear una referencia. Por lo tanto, modificando
// thisArray tampoco modifica thatArray.
thatArray = thisArray;
thatArray[1] = -1;
writeln((thisArray, thatArray));

// Asigna un segmento de una matriz a un segmento (del mismo tamaño) en el otro.
thatArray[4..5] = thisArray[1..2];
writeln((thisArray, thatArray));

// Las operaciones también se pueden promover para trabajar en arreglos.
// 'thisPlusThat' también es una matriz.
var thisPlusThat = thisArray + thatArray;
writeln(thisPlusThat);

// Continuando, las matrices y los bucles también pueden ser expresiones, donde
// la expresión del cuerpo del bucle es el resultado de cada iteración.
var arrayFromLoop = for i in 1..10 do i;
writeln(arrayFromLoop);

// Una expresión puede resultar en nada, como cuando se filtra con una expresión if.
var evensOrFives = for i in 1..10 do if (i % 2 == 0 || i % 5 == 0) then i;

writeln(arrayFromLoop);

// Las expresiones de matriz también se pueden escribir con una notación de paréntesis.
// Nota: esta sintaxis utiliza el concepto paralelo forall discutido más adelante.
var evensOrFivesAgain = [i in 1..10] if (i % 2 == 0 || i % 5 == 0) then i;

// They can also be written over the values of the array.
arrayFromLoop = [value in arrayFromLoop] value + 1;


// Procedimientos

// Los procedimientos de Chapel tienen funciones de sintaxis similares en otros idiomas.
proc fibonacci(n : int) : int {
  if n <= 1 then return n;
  return fibonacci(n-1) + fibonacci(n-2);
}

// Los parámetros de entrada pueden estar sin tipo para crear un procedimiento genérico.
proc doublePrint(thing): void {
  write(thing, " ", thing, "\n");
}

// Se puede inferir el tipo de retorno, siempre que el compilador pueda resolverlo.
proc addThree(n) {
  return n + 3;
}

doublePrint(addThree(fibonacci(20)));

// También es posible tomar un número variable de parámetros.
proc maxOf(x ...?k) {
  // x se refiere a una tupla de un tipo, con k elementos
  var maximum = x[1];
  for i in 2..k do maximum = if maximum < x[i] then x[i] else maximum;
  return maximum;
}
writeln(maxOf(1, -10, 189, -9071982, 5, 17, 20001, 42));

// Los procedimientos pueden tener valores de parámetros predeterminados, y
// los parámetros pueden nombrarse en la llamada, incluso fuera de orden.
proc defaultsProc(x: int, y: real = 1.2634): (int,real) {
  return (x,y);
}

writeln(defaultsProc(10));
writeln(defaultsProc(x=11));
writeln(defaultsProc(x=12, y=5.432));
writeln(defaultsProc(y=9.876, x=13));

// El operador ? se llama operador de consulta y se usa para tomar valores
// indeterminados como tuplas o tamaños de matriz y tipos genéricos.
// Por ejemplo, tomar matrices como parámetros.

// El operador de consulta se utiliza para determinar el dominio de A.
// Esto es útil para definir el tipo de retorno, aunque no es obligatorio.
proc invertArray(A: [?D] int): [D] int{
  for a in A do a = -a;
  return A;
}

writeln(invertArray(intArray));

// Podemos consultar el tipo de argumentos a los procedimientos genéricos.
// Aquí definimos un procedimiento que toma dos argumentos del mismo tipo,
// pero no definimos cuál es ese tipo.
proc genericProc(arg1 : ?valueType, arg2 : valueType): void {
  select(valueType) {
    when int do writeln(arg1, " and ", arg2, " are ints");
    when real do writeln(arg1, " and ", arg2, " are reals");
    otherwise writeln(arg1, " and ", arg2, " are somethings!");
  }
}

genericProc(1, 2);
genericProc(1.2, 2.3);
genericProc(1.0+2.0i, 3.0+4.0i);

// También podemos imponer una forma de polimorfismo con la cláusula where
// Esto permite que el compilador decida qué función usar.

// Nota: Eso significa que toda la información debe conocerse en tiempo de compilación.
// El modificador param en el argumento se usa para imponer esta restricción.
proc whereProc(param N : int): void
 where (N > 0) {
  writeln("N is greater than 0");
}

proc whereProc(param N : int): void
 where (N < 0) {
  writeln("N is less than 0");
}

whereProc(10);
whereProc(-1);

// whereProc(0) daría lugar a un error del compilador porque no hay funciones
// que satisfagan la condición de la cláusula where.
// Podríamos haber definido un whereProc sin una cláusula where que
// hubiera servido como captura para todos los demás casos (de los cuales solo hay uno).

// Las cláusulas where también se pueden usar para restringir según el tipo de argumento.
proc whereType(x: ?t) where t == int {
  writeln("Inside 'int' version of 'whereType': ", x);
}

proc whereType(x: ?t) {
  writeln("Inside general version of 'whereType': ", x);
}

whereType(42);
whereType("hello");

// Intenciones

/* Los modificadores de intención en los argumentos transmiten cómo esos argumentos se pasan al procedimiento.

     * in: copia arg adentro, pero no afuera
     * out: copia arg, pero no dentro
     * inout: copia arg adentro, copia arg afuera
     * ref: pasa arg por referencia
*/
proc intentsProc(in inarg, out outarg, inout inoutarg, ref refarg) {
  writeln("Adentro antes: ", (inarg, outarg, inoutarg, refarg));
  inarg = inarg + 100;
  outarg = outarg + 100;
  inoutarg = inoutarg + 100;
  refarg = refarg + 100;
  writeln("Adentro después: ", (inarg, outarg, inoutarg, refarg));
}

var inVar: int = 1;
var outVar: int = 2;
var inoutVar: int = 3;
var refVar: int = 4;
writeln("Afuera antes: ", (inVar, outVar, inoutVar, refVar));
intentsProc(inVar, outVar, inoutVar, refVar);
writeln("Afuera después: ", (inVar, outVar, inoutVar, refVar));

// Del mismo modo, podemos definir intentos en el tipo de retorno.
// refElement devuelve una referencia a un elemento de la matriz. Esto tiene más sentido
// práctico para los métodos de clase donde las referencias a elementos en una estructura
// de datos se devuelven a través de un método o iterador.
proc refElement(array : [?D] ?T, idx) ref : T {
  return array[idx];
}

var myChangingArray : [1..5] int = [1,2,3,4,5];
writeln(myChangingArray);
ref refToElem = refElement(myChangingArray, 5); // Almacena una referencia al elemento en variable de referencia
writeln(refToElem);
refToElem = -2; // modifica referencia que, a su vez, modifica el valor real en la matriz
writeln(refToElem);
writeln(myChangingArray);

// Definiciones del operador

// Chapel permite que los operadores se sobrecarguen.
// Podemos definir los operadores unarios:
// + - ! ~
// y los operadores binarios:
// + - * / % ** == <= >= < > << >> & | ˆ by
// += -= *= /= %= **= &= |= ˆ= <<= >>= <=>

// Exclusivo u operador booleano.
proc ^(left : bool, right : bool): bool {
  return (left || right) && !(left && right);
}

writeln(true  ^ true);
writeln(false ^ true);
writeln(true  ^ false);
writeln(false ^ false);

// Define un operador * en cualquiera de los dos tipos que devuelve una tupla de esos tipos.
proc *(left : ?ltype, right : ?rtype): (ltype, rtype) {
  writeln("\tIn our '*' overload!");
  return (left, right);
}

writeln(1 * "a"); // Utiliza nuestro * operador.
writeln(1 * 2);   // Utiliza el operador predeterminado *.

//  Note: Podrías romper todo si te descuidas con tus sobrecargas.
//  Esto aquí lo romperá todo. No lo hagas

/*
    proc +(left: int, right: int): int {
      return left - right;
    }
*/

// Iteradores

// Los iteradores son hermanas del procedimiento, y casi todo lo relacionado
// con los procedimientos también se aplica a los iteradores. Sin embargo, en lugar de
// devolver un solo valor, los iteradores pueden generar múltiples valores en un bucle.

// Esto es útil cuando se necesita un conjunto u orden complicado de iteraciones,
// ya que permite que el código que define las iteraciones
// se separe del cuerpo del bucle.
iter oddsThenEvens(N: int): int {
  for i in 1..N by 2 do
    yield i; // yield values instead of returning.
  for i in 2..N by 2 do
    yield i;
}

for i in oddsThenEvens(10) do write(i, ", ");
writeln();

// Los iteradores también pueden ceder condicionalmente, cuyo resultado puede ser nada
iter absolutelyNothing(N): int {
  for i in 1..N {
    if N < i { // Always false
      yield i;     // Yield statement never happens
    }
  }
}

for i in absolutelyNothing(10) {
  writeln("Woa there! absolutelyNothing yielded ", i);
}

// Podemos comprimir dos o más iteradores (que tienen el mismo número de iteraciones)
// usando zip () para crear un solo iterador comprimido, donde cada iteración
// del iterador comprimido produce una tupla de un valor de cada iterador.
for (positive, negative) in zip(1..5, -5..-1) do
  writeln((positive, negative));

// La iteración de la cremallera es bastante importante en la asignación de matrices,
// segmentos de matrices y expresiones de matriz / bucle.
var fromThatArray : [1..#5] int = [1,2,3,4,5];
var toThisArray : [100..#5] int;

// Algunas operaciones de cierre implementan otras operaciones.
// La primera declaración y el bucle son equivalentes.
toThisArray = fromThatArray;
for (i,j) in zip(toThisArray.domain, fromThatArray.domain) {
  toThisArray[i] = fromThatArray[j];
}

// Estos dos pedazos también son equivalentes.
toThisArray = [j in -100..#5] j;
writeln(toThisArray);

for (i, j) in zip(toThisArray.domain, -100..#5) {
  toThisArray[i] = j;
}
writeln(toThisArray);

/*
 Esto es muy importante para entender por qué esta declaración
 exhibe un error de tiempo de ejecución.
*/

/*
  var iterArray : [1..10] int = [i in 1..10] if (i % 2 == 1) then i;
*/

// Aunque el dominio de la matriz y la expresión de bucle son del mismo tamaño,
// el cuerpo de la expresión puede considerarse como un iterador.
// Debido a que los iteradores pueden producir nada, ese iterador produce un número
// diferente de cosas que el dominio de la matriz o bucle, que no está permitido.

// Clases

// Las clases son similares a las de C ++ y Java, asignadas en el montón.
class MyClass {

// Variables miembro
  var memberInt : int;
  var memberBool : bool = true;

// Inicializador definido explícitamente.
// También obtenemos el inicializador generado por el compilador, con un argumento por campo.
// Tenga en cuenta que pronto no habrá un inicializador generado por el compilador
// cuando definamos los inicializadores explícitamente.
  proc init(val : real) {
    this.memberInt = ceil(val): int;
  }

// Desinicializador explícitamente definido.
// Si no escribiéramos uno, obtendríamos el desinicializador generado por el compilador,
// que tiene un cuerpo vacío.
  proc deinit() {
    writeln("MyClass deinitializer called ", (this.memberInt, this.memberBool));
  }

// Métodos de clase.
  proc setMemberInt(val: int) {
    this.memberInt = val;
  }

  proc setMemberBool(val: bool) {
    this.memberBool = val;
  }

  proc getMemberInt(): int{
    return this.memberInt;
  }

  proc getMemberBool(): bool {
    return this.memberBool;
  }
} // termina MyClass

// Llame al inicializador generado por el compilador,
// utilizando el valor predeterminado para memberBool.
var myObject = new MyClass(10);
    myObject = new MyClass(memberInt = 10); // Equivalente
writeln(myObject.getMemberInt());

// Same, but provide a memberBool value explicitly.
var myDiffObject = new MyClass(-1, true);
    myDiffObject = new MyClass(memberInt = -1, memberBool = true); // Equivalente
writeln(myDiffObject);

// Llame al inicializador que escribimos.
var myOtherObject = new MyClass(1.95);
    myOtherObject = new MyClass(val = 1.95); // Equivalente
writeln(myOtherObject.getMemberInt());

// También podemos definir un operador en nuestra clase,
// pero la definición tiene que estar fuera de la definición de la clase.
proc +(A : MyClass, B : MyClass) : MyClass {
  return new MyClass(memberInt = A.getMemberInt() + B.getMemberInt(),
                      memberBool = A.getMemberBool() || B.getMemberBool());
}

var plusObject = myObject + myDiffObject;
writeln(plusObject);

// Destrucción.
delete myObject;
delete myDiffObject;
delete myOtherObject;
delete plusObject;

// Las clases pueden heredar de una o más clases primarias
class MyChildClass : MyClass {
  var memberComplex: complex;
}

// Aquí hay un ejemplo de clases genéricas.
class GenericClass {
  type classType;
  var classDomain: domain(1);
  var classArray: [classDomain] classType;

// Constructor explícito.
  proc GenericClass(type classType, elements : int) {
    this.classDomain = {1..#elements};
  }

// Copiar constructor.
// Nota: Todavía tenemos que poner el tipo como argumento, pero podemos usar
// el operador de consulta (?) como predeterminado para el tipo del otro objeto.
// Además, podemos aprovechar esto para permitir a nuestro constructor de copias
// copiar clases de diferentes tipos y emitir sobre la marcha.
  proc GenericClass(other : GenericClass(?otherType),
                     type classType = otherType) {
    this.classDomain = other.classDomain;
    // Copiar y Convertir
    for idx in this.classDomain do this[idx] = other[idx] : classType;
  }

// Defina la notación de corchetes en un objeto GenericClass
// para que pueda comportarse como una matriz normal
// i.e. objVar[i] or objVar(i)
  proc this(i : int) ref : classType {
    return this.classArray[i];
  }

// Definir un iterador implícito para que la clase produzca
// valores de la matriz a un bucle
// i.e. for i in objVar do ...
  iter these() ref : classType {
    for i in this.classDomain do
      yield this[i];
  }
} // end GenericClass

// Podemos asignar a la matriz de miembros del objeto usando la notación de
// corchete que definimos.
var realList = new GenericClass(real, 10);
for i in realList.classDomain do realList[i] = i + 1.0;

// Podemos iterar sobre los valores en nuestra lista con el iterador
// que definimos.
for value in realList do write(value, ", ");
writeln();

// Haga una copia de realList usando el constructor de copias.
var copyList = new GenericClass(realList);
for value in copyList do write(value, ", ");
writeln();

// Haga una copia de realList y cambie el tipo, también utilizando el constructor de copias.
var copyNewTypeList = new GenericClass(realList, int);
for value in copyNewTypeList do write(value, ", ");
writeln();


// Módulos

// Los módulos son la forma en que Chapel administra los espacios de nombres.
// Los archivos que contienen estos módulos no necesitan ser nombrados después
// de los módulos (como en Java), pero los archivos implícitamente nombran módulos.
// Por ejemplo, este archivo nombra implícitamente el módulo learnChapelInYMinutes

module OurModule {

// Podemos usar módulos dentro de otros módulos.
// Time es uno de los módulos estándar.
  use Time;

// Usaremos este procedimiento en la sección de paralelismo.
  proc countdown(seconds: int) {
    for i in 1..seconds by -1 {
      writeln(i);
      sleep(1);
    }
  }

// Es posible crear nidos de módulos arbitrariamente profundos.
// i.e. submódulos de OurModule
  module ChildModule {
    proc foo() {
      writeln("ChildModule.foo()");
    }
  }

  module SiblingModule {
    proc foo() {
      writeln("SiblingModule.foo()");
    }
  }
} // end OurModule

// Usando OurModule también usa todos los módulos que usa.
// Como OurModule usa Time, nosotros también usamos Time.
use OurModule;

// En este punto no hemos usado ChildModule o SiblingModule, por lo que sus símbolos
// (es decir, foo) no están disponibles para nosotros. Sin embargo, los nombres de
// los módulos están disponibles y podemos llamar explícitamente a foo () a través de ellos.
SiblingModule.foo();
OurModule.ChildModule.foo();

// Ahora usamos ChildModule, que permite llamadas no calificadas.
use ChildModule;
foo();

// Paralelismo

// En otros idiomas, el paralelismo generalmente se realiza con librerias complicadas
// y extrañas jerarquías de estructura de clases.
// Chapel lo tiene directamente en el idioma.

// Podemos declarar un procedimiento principal, pero todo el código anterior
// a main todavía se ejecuta.
proc main() {

// Una declaración de inicio hará girar el cuerpo de esa declaración en una nueva tarea.
// Una declaración de sincronización garantizará que el progreso de la tarea principal
// no avance hasta que los hijos hayan sincronizado nuevamente.

  sync {
    begin { // Inicio del cuerpo de la nueva tarea.
      var a = 0;
      for i in 1..1000 do a += 1;
      writeln("Done: ", a);
    } // Fin del nuevo cuerpo de tareas
    writeln("escindió una tarea!");
  }
  writeln("De nuevo juntos");

  proc printFibb(n: int) {
    writeln("fibonacci(",n,") = ", fibonacci(n));
  }

// Una declaración de cobegin girará cada declaración del cuerpo en una nueva tarea.
// Observe aquí que las impresiones de cada declaración pueden ocurrir en
// cualquier orden.
  cobegin {
    printFibb(20); // nueva tarea
    printFibb(10); // nueva tarea
    printFibb(5);  // nueva tarea
    {
      // Este es un cuerpo de declaración anidado y, por lo tanto, es una
      // declaración única para la declaración principal, ejecutada
      // por una sola tarea.
      writeln("esto se ");
      writeln("ejecuta");
      writeln("como un todo");
    }
  }

// Un bucle coforall creará una nueva tarea para CADA iteración.
// Nuevamente vemos que las impresiones suceden en cualquier orden.
// NOTA: ¡coforall debe usarse solo para crear tareas!
// ¡Usarlo para iterar sobre una estructura es una muy mala idea!
  var num_tasks = 10; // Number of tasks we want
  coforall taskID in 1..#num_tasks {
    writeln("Hola de tarea# ", taskID);
  }

// los bucles forall son otro bucle paralelo, pero solo crean un número
// menor de tareas, específicamente --dataParTasksPerLocale = número de tareas.
  forall i in 1..100 {
    write(i, ", ");
  }
  writeln();

// Aquí vemos que hay secciones que están en orden, seguidas de una sección
// que no seguiría (por ejemplo, 1, 2, 3, 7, 8, 9, 4, 5, 6,).
// Esto se debe a que cada tarea está asumiendo un fragmento del rango 1..10
// (1..3, 4..6 o 7..9) haciendo ese fragmento en serie, pero cada tarea ocurre en paralelo.
// Sus resultados pueden depender de su máquina y configuración

// Para los bucles forall y coforall, la ejecución de la tarea principal
// no continuará hasta que todos los hijos se sincronicen.

// los bucles forall son particularmente útiles para la iteración paralela sobre matrices.
// Hagamos un experimento para ver qué tan rápido es un ciclo paralelo.

  use Time; // Importe el módulo Time para usar objetos de Timer
  var timer: Timer;
  var myBigArray: [{1..4000,1..4000}] real; // Gran matriz en la que escribiremos

// Experimento en serie:
  timer.start(); // Iniciar temporizador
  for (x,y) in myBigArray.domain { // Iteración en serie
    myBigArray[x,y] = (x:real) / (y:real);
  }
  timer.stop(); // Detener temporizador
  writeln("Serial: ", timer.elapsed()); // Imprimir tiempo transcurrido
  timer.clear(); // Limpia el temporizador para bucle paralelo

// Experimento Paralelo:
  timer.start(); // Iniciar temporizador
  forall (x,y) in myBigArray.domain { // Iteración paralela
    myBigArray[x,y] = (x:real) / (y:real);
  }
  timer.stop(); // Detener temporizador
  writeln("Parallel: ", timer.elapsed()); // Imprimir tiempo transcurrido
  timer.clear();

// Puede que hayas notado que (dependiendo de cuántos núcleos tengas)
// el ciclo paralelo fue más rápido que el ciclo serial.

// La expresión de bucle estilo corchete descrita mucho antes utiliza
// implícitamente un bucle forall.
  [val in myBigArray] val = 1 / val; // Operación paralela

// Las variables atómicas, comunes a muchos idiomas, son aquellas cuyas operaciones
// ocurren sin interrupciones. Por lo tanto, varios subprocesos pueden modificar
// las variables atómicas y pueden saber que sus valores son seguros.
// Las variables atómicas de la capilla pueden ser de tipo bool, int, uint y real.
  var uranium: atomic int;
  uranium.write(238);      // escribir atómicamente una variable
  writeln(uranium.read()); // leer atómicamente una variable

// Las operaciones atómicas se describen como funciones, por lo que puede definir
// las suyas propias.
  uranium.sub(3); // restar atómicamente una variable
  writeln(uranium.read());

  var replaceWith = 239;
  var was = uranium.exchange(replaceWith);
  writeln("El uranio era", was, ", pero ahora es ", replaceWith);

  var isEqualTo = 235;
  if uranium.compareExchange(isEqualTo, replaceWith) {
    writeln("El uranio era igual a ", isEqualTo,
             " pero valor reemplazado por", replaceWith);
  } else {
    writeln("uranio no era igual a ", isEqualTo,
             " así que el valor permanece igual... sea lo que sea");
  }

  sync {
    begin { // Tarea del lector
      writeln("Lector: esperando que el uranio sea ", isEqualTo);
      uranium.waitFor(isEqualTo);
      writeln("Lector: el uranio fue configurado (por alguien) para ", isEqualTo);
    }

    begin { // Tarea de escritor
      writeln("Escritor: establecerá uranio en el valor ", isEqualTo, " en...");
      countdown(3);
      uranium.write(isEqualTo);
    }
  }

// las variables de sincronización tienen dos estados: vacío y lleno.
// Si lee una variable vacía o escribe una variable completa,
// se espera hasta que la variable esté llena o vacía nuevamente.
  var someSyncVar$: sync int; // varName$ Es una convención, no una ley.
  sync {
    begin { // Tarea del lector
      writeln("Lector: esperando leer.");
      var read_sync = someSyncVar$;
      writeln("Lector: el valor es ", read_sync);
    }

    begin { // Tarea de escritor
      writeln("Escritor: escribirá en...");
      countdown(3);
      someSyncVar$ = 123;
    }
  }

// las variales individuales solo se pueden escribir una vez.
// Una lectura en un solo no escrito da como resultado una espera,
// pero cuando la variable tiene un valor, puede leerse indefinidamente.
  var someSingleVar$: single int; // varName$ Es una convención, no una ley.
  sync {
    begin { // Tarea del lector
      writeln("Lector: esperando leer.");
      for i in 1..5 {
        var read_single = someSingleVar$;
        writeln("Lector: iteración ", i,", y el valor es ", read_single);
      }
    }

    begin { // Tarea de escritor
      writeln("Escritor: escribirá en ...");
      countdown(3);
      someSingleVar$ = 5; // primero y único escrito.
    }
  }

// Aquí hay un ejemplo usando atómica y una variable de sincronización
// para crear un mutex de cuenta regresiva
//  (también conocido como multiplexor).
  var count: atomic int; // nuestro mostrador
  var lock$: sync bool;   // la cerradura mutex

  count.write(2);       // Solo deje dos tareas a la vez.
  lock$.writeXF(true);  // Establezca lock$ en completo (desbloqueado)
  // Nota: el valor en realidad no importa, solo el estado
  // (completo: desbloqueado / vacio: bloqueado)
  // Además, writeXF() llena (F) la variable de sincronización independientemente de su estado (X)

  coforall task in 1..#5 { // Generar tareas
    // Create a barrier
    do {
      lock$;                 // Leer lock$ (espera)
    } while (count.read() < 1); // Sigue esperando hasta que se abra un lugar

    count.sub(1);          //disminuir el contador
    lock$.writeXF(true); // Establezca lock$ en completo (señal)

    // 'Trabajo' actual
    writeln("Tarea #", task, " trabajando");
    sleep(2);

    count.add(1);        // Incrementa el contador
    lock$.writeXF(true); // Establezca lock$ en completo (señal)
  }

// Podemos definir las operaciones + * & | ^ && || min max minloc maxloc
// sobre una matriz completa usando escaneos y reducciones.
// Las reducciones aplican la operación en toda la matriz
// y dan como resultado un valor escalar.

  var listOfValues: [1..10] int = [15,57,354,36,45,15,456,8,678,2];
  var sumOfValues = + reduce listOfValues;
  var maxValue = max reduce listOfValues; // 'max' da solo el valor máximo

// maxloc proporciona el valor máximo y el índice del valor máximo.
// Nota: Tenemos que comprimir la matriz y el dominio junto con el iterador zip.
  var (theMaxValue, idxOfMax) = maxloc reduce zip(listOfValues,
                                                  listOfValues.domain);

  writeln((sumOfValues, maxValue, idxOfMax, listOfValues[idxOfMax]));

// Los escaneos aplican la operación de forma incremental y devuelven una matriz
// con los valores de la operación en ese índice a medida que avanza a través
// de la matriz desde array.domain.low hasta array.domain.high.
  var runningSumOfValues = + scan listOfValues;
  var maxScan = max scan listOfValues;
  writeln(runningSumOfValues);
  writeln(maxScan);
} // end main()
```

## ¿Para quién es este tutorial?

Este tutorial es para personas que desean aprender las cuerdas de chapel sin tener
que escuchar sobre qué mezcla de fibras son las cuerdas, o cómo fueron trenzadas,
o cómo las configuraciones de trenzas difieren entre sí. No le enseñará cómo
desarrollar código increíblemente eficaz, y no es exhaustivo.
Referirse a [especificación de idioma](https://chapel-lang.org/docs/latest/language/spec.html)(en) y
a [documentación del módulo](https://chapel-lang.org/docs/latest/)(en) para más detalles.

Ocasionalmente, vuelva aquí en el [website de Chapel](https://chapel-lang.org)
para ver si se han agregado más temas o se han creado más tutoriales.

### Lo que le falta a este tutorial:

* Exposición de los [módulos estándar](https://chapel-lang.org/docs/latest/modules/standard.html)
* Múltiples configuraciones regionales (sistema de memoria distribuida)
* Registros
* Iteradores paralelos

## ¡Sus comentarios, preguntas y descubrimientos son importantes para los desarrolladores!

El lenguaje Chapel todavía está en desarrollo activo, por lo que
ocasionalmente hay problemas con el rendimiento y
las características del lenguaje.

Cuanta más información brinde al equipo de desarrollo de Chapel
sobre los problemas que encuentre o las características que le gustaría ver,
mejor será el lenguaje.

Hay varias formas de interactuar con los desarrolladores:

* [Chat de Gitter](https://gitter.im/chapel-lang/chapel)
* [lista de emails de Sourceforge](https://sourceforge.net/p/chapel/mailman)

Si está realmente interesado en el desarrollo del compilador o en contribuir al proyecto,
[consulte el repositorio maestro de GitHub](https://github.com/chapel-lang/chapel).
Está bajo el [La licencia Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).

## Instalar el compilador

[La documentación oficial de Chapel detalla cómo descargar y compilar el compilador de Chapel.](https://chapel-lang.org/docs/usingchapel/QUICKSTART.html)

Chapel se puede construir e instalar en su máquina promedio 'nix (y cygwin).
[Descargue la última versión de lanzamiento](https://github.com/chapel-lang/chapel/releases/)
y es tan fácil como

1. `tar -xvf chapel-<VERSION>.tar.gz`
2. `cd chapel-<VERSION>`
3. `source util/setchplenv.bash # or .sh or .csh or .fish`
4. `make`
5. `make check # optional`

You will need to `source util/setchplenv.EXT` from within the Chapel directory
(`$CHPL_HOME`) every time your terminal starts so it's suggested that you drop
that command in a script that will get executed on startup (like .bashrc).

Necesitará `source util/setchplenv.EXT` desde el directorio de Chapel (`$CHPL_HOME`)
cada vez que se inicie su terminal, por lo que se sugiere que suelte ese comando
en un script que se ejecutará al inicio (como .bashrc).

Chapel se instala fácilmente con Brew para OS X

1. `brew update`
2. `brew install chapel`

## Compilando Código

Construye como otros compiladores:

`chpl myFile.chpl -o myExe`

Argumentos notables:

* `--fast`: habilita varias optimizaciones y deshabilita las comprobaciones
  de los límites de la matriz Solo debe habilitarse cuando la aplicación es estable.
* `--set <Nombre del Symbolo>=<Valor>`: establece el param de configuracion
  `<Nombre del Symbolo>` a `<Valor>`en tiempo de compilación.
* `--main-module <Nombre del módulo>`: use el procedimiento main() que se encuentra en el módulo
  `<Nombre del módulo>` como principal del ejecutable.
* `--module-dir <Directorio>`: incluye `<Directorio>` en la ruta de búsqueda del módulo.
