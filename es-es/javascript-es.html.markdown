---
language: javascript
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
    - ["Daniel Zendejas","https://github.com/DanielZendejas"]
filename: javascript-es.js
lang: es-es
---
Tutorial de JavaScript en español.

JavaScript fue creado por Brendan Eich en 1995 mientras trabajaba en Netscape. 
Su intención original era crear un lenguaje simple para sitios web, complementándolo
con Java para aplicaciones más complejas. Debido a su integracion estrecha con sitios
web y soporte por defecto de los navegadores modernos se ha vuelto mucho más común 
para front-end que Java.

Sin embargo, JavaScript no sólo se limita a los navegadores web: Node.js, un proyecto que proporciona un entorno de ejecución independiente para el motor V8 de Google Chrome, se está volviendo más y más popular.

¡La retroalimentación es bienvenida! Puedes encontrarme en: 
[@ExcitedLeigh](https://twitter.com/ExcitedLeigh), o
[l@leigh.net.au](mailto:l@leigh.net.au).

```js
// Los comentarios en JavaScript son los mismos como comentarios en C. 

//Los comentarios de una sola línea comienzan con //,
/* y los comentarios multilínea comienzan
   y terminan con */

// Cada sentencia puede ser terminada con punto y coma ;
hazAlgo();

// ... aunque no es necesario, ya que el punto y coma se agrega automáticamente
// cada que se detecta una nueva línea, a excepción de algunos casos.
hazAlgo()

// Dado que esta práctica puede llevar a resultados inesperados, seguiremos agregando
// punto y coma en esta guía.

///////////////////////////////////
// 1. Números, Strings y Operadores

// JavaScript tiene un solo tipo de número (doble de 64-bit IEEE 754).
// Así como con Lua, no te espantes por la falta de enteros: los dobles tienen 52 bits
// de mantisa, lo cual es suficiente para guardar enteros de hasta 9✕10¹⁵.
3; // = 3
1.5; // = 1.5

// Toda la aritmética básica funciona como uno esperaría.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Incluyendo divisiones con resultados no enteros.
5 / 2; // = 2.5

// Las operaciones con bits también funcionan; cuando ejecutas una operación con bits
// el número flotante se convierte a entero con signo *hasta* 32 bits.
1 << 2; // = 4

// La jerarquía de las operaciones se aplica con paréntesis.
(1 + 3) * 2; // = 8

// Hay tres casos especiales de valores con los números:
Infinity; // por ejemplo: 1/0
-Infinity; // por ejemplo: -1/0
NaN; // por ejemplo: 0/0

// También hay booleanos:
true;
false;

// Los Strings se pueden crear con ' o ".
'abc';
"Hola, mundo";

// La negación se aplica con la expresión ! 
!true; // = false
!false; // = true

// Para comprobar una igualdad se usa ===
1 === 1; // = true
2 === 1; // = false

// Para comprobar una desigualdad se usa !==
1 !== 1; // = false
2 !== 1; // = true

// Más comparaciones
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Los Strings se concatenan con +
"¡Hola " + "mundo!"; // = "¡Hola mundo!"

// y se comparan con < y con >
"a" < "b"; // = true

// Los tipos no importan con el operador ==...
"5" == 5; // = true
null == undefined; // = true

// ...a menos que uses ===
"5" === 5; // = false
null === undefined; // false

// Los Strings funcionan como arreglos de caracteres
// Puedes acceder a cada caracter con la función charAt()
"Este es un String".charAt(0);  // = 'E'

// ...o puedes usar la función substring() para acceder a pedazos más grandes
"Hola Mundo".substring(0, 4); // = "Hola"

// length es una propiedad, así que no uses ()
"Hola".length; // = 4

// También hay null y undefined
null; // usado para indicar una falta de valor deliberada
undefined; // usado para indicar que un valor no está presente actualmente
           // (aunque undefined es un valor en sí mismo)

// false, null, undefined, NaN, 0 y "" es false; todo lo demás es true.
// Note que 0 es false y "0" es true, a pesar de que 0 == "0".
// Aunque 0 === "0" sí es false.

///////////////////////////////////
// 2. Variables, Arrays y Objetos

// Las variables se declaran con la palabra var. JavaScript cuenta con tipado dinámico,
// así que no se necesitan aplicar tipos. La asignación se logra con el operador =.
var miPrimeraVariable = 5;

// si no escribes la palabra var no se marcará ningún error...
miSegundaVariable = 10;

// ...pero tu variable se declarará en el ámbito global, no en el ámbito
// en el que se definió.

// Las variables que no están aún asignadas tienen el valor undefined.
var miTerceraVariable; // = undefined

// Existen atajos para realizar operaciones aritméticas:
miPrimeraVariable += 5; // equivalente a miPrimeraVariable = miPrimeraVariable + 5;
						// miPrimeraVariable ahora es 10
miPrimeraVariable *= 10; // ahora miPrimeraVariable es 100

// Y atajos aún más cortos para sumar y restar 1
miPrimeraVariable++; // ahora miPrimeraVariable es 101
miPrimeraVariable--; // de vuelta a 100

// Los arreglos son listas ordenadas de valores, de cualquier tipo.
var miArreglo = ["Hola", 45, true];

// Los miembros de un arreglo pueden ser accesados con la sintaxis 
// de indices dentro de corchetes [].
// Los índices empiezan en cero.
miArreglo[1]; // = 45

// Los arreglos son mutables y pueden cambiar de longitud.
miArreglo.push("Mundo");
miArreglo.length; // = 4

// Agregar/Modificar en un determinado índice
miArreglo[3] = "Hola";

// Los objetos en JavaScript son equivalentes a los 'diccionarios' o 'mapas' en otros
// lenguajes: una colección de pares llave/valor desordenada.
var miObjeto = {llave1: "Hola", llave2: "Mundo"};

// Las llaves son strings, pero no se necesitan las comillas si son un identificador
// válido de JavaScript. Los valores pueden ser de cualquier tipo.
var miObjeto = {miLlave: "miValor", "mi otra llave": 4};

// Los atributos de los objetos también pueden ser accesadas usando
//  la sintaxis de corchetes,
miObjeto["mi otra llave"]; // = 4

// ... o usando la sintaxis de punto, dado que la llave es un identificador válido.
miObjeto.miLlave; // = "miValor"

// Los objetos son mutables; los valores pueden ser cambiados y se pueden
// agregar nuevas llaves.
miObjeto.miTerceraLlave = true;

// Si intentas acceder con una llave que aún no está asignada tendrás undefined.
miObjeto.miCuartaLlave; // = undefined

///////////////////////////////////
// 3. Lógica y estructura de control

// La sintaxis de esta sección es casi idéntica a la de Java. 

// La estructura if funciona de la misma forma.
var contador = 1;
if (contador == 3){
    // evaluar si contador es igual a 3
} else if (contador == 4){
    // evaluar si contador es igual a 4
} else {
    // evaluar si contador no es igual a 3 ni a 4
}

// De la misma forma la estructura while.
while (true){
    // ¡Loop infinito!
}

// La estructura Do-while es igual al while, excepto que se ejecuta al menos una vez.
var input
do {
    input = conseguirInput();
} while (!esValido(input))

// la esctructura for es la misma que la de C y Java:
// inicialización; condición; iteración.
for (var i = 0; i < 5; i++){
    // correrá cinco veces
}

// && es un "y" lógico, || es un "o" lógico
if (casa.tamano == "grande" && casa.color == "azul"){
    casa.contiene = "oso";
}
if (color == "rojo" || color == "azul"){
    // el color es rojo o azul
}

// && y || "corto circuito", lo cual es útil para establecer valores por defecto.
var nombre = otroNombre || "default";


// la estructura switch usa === para sus comparaciones
// usa 'break' para terminar cada caso 
// o los casos después del caso correcto serán ejecutados también. 
calificacion = 'B';
switch (calificacion) {
  case 'A':
    console.log("Excelente trabajo");
    break;
  case 'B':
    console.log("Buen trabajo");
    break;
  case 'C':
    console.log("Puedes hacerlo mejor");
    break;
  default:
    console.log("Muy mal");
    break;
}


///////////////////////////////////
// 4. Funciones, ámbitos y closures

// Las funciones en JavaScript son declaradas con la palabra clave "function".
function miFuncion(miArgumentoString){
    return miArgumentoString.toUpperCase(); //la funcion toUpperCase() vuelve todo
    // el String a mayúsculas
}
miFuncion("foo"); // = "FOO"

// Note que el valor a ser regresado debe estar en la misma línea que la
// palabra clave 'return', de otra forma la función siempre regresará 'undefined' 
// debido a la inserción automática de punto y coma.
function miFuncion()
{
    return // <- punto y coma insertado aquí automáticamente
    {
        estaEsUna: 'propiedad'
    }
}
miFuncion(); // = undefined al mandar a llamar la función

// Las funciones en JavaScript son de primera clase, así que pueden ser asignadas
// a variables y pasadas a otras funciones como argumentos - por ejemplo:
function miFuncion(){
    // este código será llamado cada cinco segundos
}
setTimeout(miFuncion, 5000);
// Note: setTimeout no es parte de JS, pero lo puedes obtener de los browsers
// y Node.js.

// Es posible declarar funciones sin nombre - se llaman funciones anónimas
// y se definen como argumentos de otras funciones.
setTimeout(function(){
    // este código se ejecuta cada cinco segundos
}, 5000);

// JavaScript tiene ámbitos de funciones; las funciones tienen su propio ámbito pero
// otros bloques no.
if (true){
    var i = 5;
}
i; // = 5 - en un lenguaje que da ámbitos por bloque esto sería undefined, pero no aquí.

// Este conlleva a un patrón de diseño común llamado "ejecutar funciones anónimas 
//inmediatamente", que preveé variables temporales de fugarse al ámbito global
(function(){
    var temporal = 5;
    // Podemos acceder al ámbito global asignando al 'objeto global', el cual
    // en un navegador siempre es 'window'. El objeto global puede tener
    // un nombre diferente en ambientes distintos, por ejemplo Node.js .
    window.permanente = 10;
})();
temporal; // da ReferenceError
permanente; // = 10

// Una de las características más útiles de JavaScript son los closures.
// Si una función es definida dentro de otra función, la función interna tiene acceso
// a todas las variables de la función externa, incluso aunque la función 
// externa ya haya terminado.
function decirHolaCadaCincoSegundos(nombre){
    var texto = "¡Hola, " + nombre + "!";
    // Las funciones internas son puestas en el ámbito local por defecto
    // como si fueran declaradas con 'var'.
    function interna(){
        alert(texto);
    }
    setTimeout(interna, 5000);
    // setTimeout es asíncrono, así que la función decirHolaCadaCincoSegundos 
    // terminará inmediatamente, y setTimeout llamará a interna() a los cinco segundos
    // Como interna está "cerrada dentro de" decirHolaCadaCindoSegundos, interna todavía tiene
    // acceso a la variable 'texto' cuando es llamada.
}
decirHolaCadaCincoSegundos("Adam"); // mostrará una alerta con "¡Hola, Adam!" en 5s

///////////////////////////////////
// 5. Más sobre objetos; constructores y prototipos

// Los objetos pueden contener funciones.
var miObjeto = {
    miFuncion: function(){
        return "¡Hola Mundo!";
    }
};
miObjeto.miFuncion(); // = "¡Hola Mundo!"

// Cuando las funciones de un objeto son llamadas, pueden acceder a las variables 
// del objeto con la palabra clave 'this'.
miObjeto = {
    miString: "¡Hola Mundo!",
    miFuncion: function(){
        return this.miString;
    }
};
miObjeto.miFuncion(); // = "¡Hola Mundo!"

// Las funciones de un objeto deben ser llamadas dentro del contexto de ese objeto.
var miFuncion = myObj.miFuncion;
miFuncion(); // = undefined

// Una función puede ser asignada al objeto y ganar acceso a él gracias a esto,
// incluso si no estaba dentro del objeto cuando este se definió.
var miOtraFuncion = function(){
    return this.miString.toUpperCase();
}
miObjeto.miOtraFuncion = myOtherFunc;
miObjeto.miOtraFuncion(); // = "¡HOLA MUNDO!"

// Podemos especificar el contexto en el que una función será llamada con los comandos
// 'call' o 'apply'.

var otraFuncion = function(otroString){
    return this.miString + otroString;
}
otraFuncion.call(miObjeto, " y hola Luna!"); // = "¡Hola Mundo! y hola Luna!"

// 'apply' es casi idéntico, pero recibe un arreglo como argumento.

otraFuncion.apply(miObjeto, [" y hola Sol!"]); // = "¡Hola Mundo! y hola Sol!"

// Esto es útil cuando estás trabajando con una función que acepta una secuencia de 
// argumentos y quieres pasar un arreglo.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Pero 'call' y 'apply' sólo son temporales. Cuando queremos que se quede, usamos bind.

var funcionUnida = otraFuncion.bind(miObjeto);
funcionUnida(" y hola Saturno!"); // = "¡Hola Mundo! y hola Saturno!"

// Bind también puede ser usada para aplicar parcialmente (curry) una función.

var producto = function(a, b){ return a * b; }
var porDos = producto.bind(this, 2);
porDos(8); // = 16

// Cuando llamas a una función con la palabra clave 'new' un nuevo objeto es creado.
// Se hace disponible a la función. Las funciones diseñadas para ser usadas así se
// llaman constructores.

var MiConstructor = function(){
    this.miNumero = 5;
}
miNuevoObjeto = new MiConstructor(); // = {miNumero: 5}
miNuevoObjeto.miNumero; // = 5

// Todos los objetos JavaScript tienen un 'prototipo'. Cuando vas a acceder a una
// propiedad en un objeto que no existe en el objeto el intérprete buscará en
// el prototipo.

// Algunas implementaciones de JavaScript te permiten acceder al prototipo de 
// un objeto con la propiedad __proto__. Mientras que esto es útil para explicar
// prototipos, no es parte del estándar; veremos formas estándar de usar prototipos
// más adelante.

var miObjeto = {
    miString: "¡Hola Mundo!"
};
var miPrototipo = {
    sentidoDeLaVida: 42,
    miFuncion: function(){
        return this.miString.toLowerCase()
    }
};

miObjeto.__proto__ = miPrototipo;
miObjeto.sentidoDeLaVida; // = 42

// Esto funcionan también para funciones.
miObjeto.miFuncion(); // = "hello world!"

// Por supuesto, si la propiedad que buscas no está en el prototipo, 
// se buscará en el prototipo del prototipo.
miPrototipo.__proto__ = {
    miBoolean: true
};
miObjeto.miBoolean; // = true

// Esto no involucra ningún copiado, cada objeto guarda una referencia a su 
// prototipo. Esto significa que podemos alterar el prototipo y nuestros
// cambios serán reflejados en todos lados.
miPrototipo.sentidoDeLaVida = 43;
miObjeto.sentidoDeLaVida; // = 43

// Mencionabamos anteriormente que __proto__ no está estandarizado, y que no 
// existe una forma estándar de acceder al prototipo de un objeto. De todas formas.
// hay dos formas de crear un nuevo objeto con un prototipo dado.

// El primer método es Object.create, el cual es una adición reciente a JavaScript,
// y por lo tanto, no disponible para todas las implementaciones aún.
var miObjeto = Object.create(miPrototipo);
miObjeto.sentidoDeLaVida; // = 43

// El segundo método, el cual trabaja en todos lados, tiene que ver con los 
// constructores. Los constructores tienen una propiedad llamada prototype.
// Este NO ES el prototipo de la función constructor; es el prototipo que
// se le da a los nuevos objetos cuando son creados con la palabra clave
// new.

MiConstructor.prototype = {
    miNumero: 5,
    getMiNumero: function(){
        return this.miNumero;
    }
};
var miNuevoObjeto2 = new MiConstructor();
miNuevoObjeto2.getMiNumero(); // = 5
miNuevoObjeto2.miNumero = 6
miNuevoObjeto2.getMiNumero(); // = 6

// Los tipos que vienen por defecto en JavaScript (como Strings y números)
// también tienen constructores que crean objetos equivalentes.
var miNumero = 12;
var miNumeroObjeto = new Number(12);
miNumero == miNumeroObjeto; // = true

// No son exactamente iguales.
typeof miNumero; // = 'number'
typeof miNumeroObjeto; // = 'object'
miNumero === miNumeroObjeyo; // = false
if (0){
    // Este código no se ejecutará porque 0 es false.
}

// Aún así, los objetos que envuelven y los prototipos por defecto comparten
// un prototipo. así que puedes agregar funcionalidades a un string de la 
// siguiente forma:
String.prototype.primerCaracter = function(){
    return this.charAt(0);
}
"abc".primerCaracter(); // = "a"

// Este hecho se usa normalmente en "polyfilling", lo cual es implementar
// nuevas funciones a JavaScript en un JavaScript más viejo, así que pueda ser
// compatible con ambintes más viejos de JavaScript (por ejemplo, navegadores viejos).

// Por ejemplo, mencionabamos que Object.create no está aún disponible en todas
// las implementaciones, pero podemos hacerlo con polyfill:
if (Object.create === undefined){ // esta validación sirve para no sobreescribir
    Object.create = function(proto){
    	// hace un constructor temporal con el prototipo correcto
        var Constructor = function(){};
        Constructor.prototype = proto;
        // y luego lo usamos para hacer un objeto con el prototipo
        // correcto.
        return new Constructor();
    }
}
```

## Fuentes y Referencias

La [Red para Desarroladores de Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript) 
proveé excelente documentación para JavaScript para navegadores. Además, está en formato de wiki,
por lo que mientras vayas aprendiendo podrás ayudar a los demás con tu experiencia.

MDN [Una re-introducción a JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
cubre muchos de los conceptos que vimos aquí pero a mayor detalle. Esta guía cubre, más que nada, 
el lenguaje JavaScript solo. Si quieres aprender a cómo usarlo en un ambiente web empieza aprendiendo
sobre el [DOM](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Aprende JavaScript con ejemplos y retos](http://www.learneroo.com/modules/64/nodes/350) es una 
variante de esta guía pero con retos.

[Jardín JavaScript](http://bonsaiden.github.io/JavaScript-Garden/) es una guía para todas las
funciones y características contra-intuitivas del lenguaje.

[JavaScript: La guía definitiva](http://www.amazon.com/gp/product/0596805527/) es una guía clásica / libro de referencia. 

Aparte de las contribuciones directas para este artículo, algo del contenido se adaptó
del tutorial de Python por Louie Dinh en este sitio. y el [Tutorial JS](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript) en la Red de Desarrolladores de Mozilla.
