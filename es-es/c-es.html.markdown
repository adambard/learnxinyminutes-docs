---
language: c
filename: learnc-es.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Francisco García", "http://flaskbreaker.tumblr.com/"]
    - ["Heitor P. de Bittencourt", "https://github.com/heitorPB/"]
lang: es-es
---

¡Ah!, C. Aun hoy en día sigue siendo el lenguaje por excelencia de la 
computación moderna de alto rendimiento.

C es el lenguaje de más bajo nivel que la mayoría de los programadores
llegarán a usar, pero lo compensa de sobra con pura velocidad. Solo
ten en cuenta el manejo manual de memoria y te llevará tan lejos como
necesites.

```c
// Los comentarios de una sola línea comienzan con //

/*
Los comentarios multilínea tienen este aspecto.
*/

// Importa cabeceras con #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Declara por adelantado las armaduras de las funciones en un archivo .h,
// o al principio de tu archivo .c .
void function_1();
void function_2();

// El punto de entrada de tu programa es una función llamada main con
// retorno de tipo entero (integer).
int main() {

// Muestra la salida usando printf, para el "formato print"
// %d es un entero, \n es una nueva línea
printf("%d\n", 0); // => Muestra 0
// Todas las sentencias deben terminar con un punto y coma.

///////////////////////////////////////
// Tipos
///////////////////////////////////////

// Tienes que declarar una variable antes de usarla. La declaración de una
// variable necesites que especifiques su tipo; el tipo de una variable
// determina su tamaño en bytes.

// 'ints' (enteros) son normalmente de 4 bytes
int x_int = 0;

// 'shorts' son normalmente de 2 bytes
short x_short = 0;

// 'chars' son fijo de 1 byte
char x_char = 0;
char y_char = 'y'; // Los caracteres literales se entrecomillan con ''

// 'longs' son a menudo de 4 a 8 bytes; 'long longs' son fijo de por lo
// menos 64 bits
long x_long = 0;
long long x_long_long = 0; 

// 'floats' son normalmente números de coma flotante de 32 bits
float x_float = 0.0;

// 'doubles' son normalmente números de coma flotante de 64 bits
double x_double = 0.0;

// Todos los tipos enteros pueden ser 'unsigned'. Esto significa que no
// pueden ser negativos, pero el valor máximo de una variable 'unsigned'
// es mayor que el de una no 'unsigned' del mismo tamaño.
unsigned char ux_char;
unsigned short ux_short;
unsigned int ux_int;
unsigned long long ux_long_long;

// Todos menos 'char', que es siempre de 1 byte, varían el tamaño 
// dependiendo de tu máquina. sizeof(T) te dice el tamaño de una variable
// de tipo T en bytes por lo que podemos expresar el tamaño de estos tipos
// portatilmente.
// Por ejemplo,
printf("%lu\n", sizeof(int)); // => 4 (en máquinas con 'words' de 4 bytes)

// Los arrays deben ser inicializados con un tamaño concreto.
char my_char_array[20]; // Este array ocupa 1 * 20 = 20 bytes
int my_int_array[20]; // Este array ocupa 4 * 20 = 80 bytes
                      // (suponiendo que tenemos 'words' de 4-byte)


// Puedes inicializar un array a 0 así:
char my_array[20] = {0};

// Indexar un array es como en otros lenguajes -o, más bien, otros
// lenguajes son como C-
my_array[0]; // => 0

// Los arrays varían; ¡son sólo memoria!
my_array[1] = 2;
printf("%d\n", my_array[1]); // => 2

// Las cadenas (strings) son sólo arrays de 'chars' (caracteres)
// terminados en un byte NUL (0x00), representado en las cadenas como el
// carácter especial '\0'.
// (No tenemos porqué añadir el byte nulo en cadenas literales; el
// compilador lo añade al final por nosotros.)
char a_string[20] = "Esto es una cadena";
printf("%s\n", a_string); // %s se sutituye por una cadena.

/*
Te habrás dado cuenta de que a_string es solo de 18 caracteres.
El 'char' #19 es el byte nulo. 
El 'char' #20 es de valor indefinido.
*/

printf("%d\n", a_string[18]); // => 0

///////////////////////////////////////
// Operadores
///////////////////////////////////////

int i1 = 1, i2 = 2; // Forma corta de declaración múltiple
float f1 = 1.0, f2 = 2.0;

// La aritmética es sencilla
i1 + i2; // => 3
i2 - i1; // => 1
i2 * i1; // => 2
i1 / i2; // => 0 (0.5, pero es truncado tras el 0)

f1 / f2; // => 0.5, más o menos épsilon
// Módulo está también
11 % 3; // => 2

// Los operadores de comparación te resultaran familiares, pero no hay
// booleanos en C. Usamos enteros (ints) en su lugar. 0 es falso,
// cualquier otra cosa es verdadero. (Los operadores de comparación 
// siempre devuelven 0 o 1)
3 == 2; // => 0 (Falso)
3 != 2; // => 1 (Verdadero)
3 > 2; // => 1
3 < 2; // => 0
2 <= 2; // => 1
2 >= 2; // => 1

// La lógica funiona en enteros
!3; // => 0 (not lógico)
!0; // => 1
1 && 1; // => 1 (and lógico)
0 && 1; // => 0
0 || 1; // => 1 (or lógico)
0 || 0; // => 0

// ¡Operadores de bits!
~0x0F; // => 0xF0 (Negación)
0x0F & 0xF0; // => 0x00 (AND)
0x0F | 0xF0; // => 0xFF (OR)
0x04 ^ 0x0F; // => 0x0B (XOR)
0x01 << 1; // => 0x02 (desplazar hacia la izquierda (por 1))
0x02 >> 1; // => 0x01 (desplazar hacia la derecha (por 1))

///////////////////////////////////////
// Estructuras de Control
///////////////////////////////////////

if (0) {
  printf("Yo nunca ocurro\n");
} else if (0) {
  printf("Yo tampoco ocurro nunca\n");
} else {
  printf("Yo me muestro\n");
}

// Mientras el bucle exista
int ii = 0;
while (ii < 10) {
    printf("%d, ", ii++); // ii++ incrementa ii en uno, después de usar su valor.
} // => muestra "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

int kk = 0;
do {
    printf("%d, ", kk);
} while (++kk < 10); // ++kk incrementa kk en uno, antes de usar su valor.
// => muestra "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

// Bucles 'for' también
int jj;
for (jj=0; jj < 10; jj++) {
    printf("%d, ", jj);
} // => muestra "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

///////////////////////////////////////
// Cambios de Tipo
///////////////////////////////////////

// Cada valor en C tiene un tipo,  pero tu puedes ingresar un valor en
// otro tipo si quieres.

int x_hex = 0x01; // Puedes asignar hexadecimales a variables

// El cambio de tipos intentará mantener sus valores numéricos
printf("%d\n", x_hex); // => Muestra 1
printf("%d\n", (short) x_hex); // => Muestra 1
printf("%d\n", (char) x_hex); // => Muestra 1

// Los tipos se desbordan sin aviso
printf("%d\n", (char) 257); // => 1 (El valor máximo de un 'char' es 255)

// Los tipos enteros puden cambiarse a tipos de coma flotante, y viceversa
printf("%f\n", (float)100); // %f se sustituye por un 'float'
printf("%lf\n", (double)100); // %lf se sustituye por un 'double'
printf("%d\n", (char)100.0);

///////////////////////////////////////
// Punteros
///////////////////////////////////////

// Un puntero es una variable declarada para almacenar una dirección de 
// memoria. Su declaración además nos dirá el tipo de dato al que apunta. 
// Puedes obtener la dirección de memoria de tus variables, y después
// enlazarlas con ellos.

int x = 0;
printf("%p\n", &x); // Usa & para obtener la dirección de una variable.
// (%p se sustituye por un puntero)
// => Muestra alguna dirección de memoria;

// Los tipos de puntero terminan con * en su declaración
int* px; // px es un puntero a un 'int'
px = &x; // Almacena la dirección de x en px
printf("%p\n", px); // => Muestra alguna dirección de memoria

// Para obtener el valor de la dirección a la que apunta un puntero, pon
// * delante para desreferenciarle. 
printf("%d\n", *px); // => Muestra 0, el valor de x y de la dirección a la
                     //    que apunta px

// También puedes cambiar el valor al que está apuntando el puntero.
// Tenemos que meter la desreferencia entre paréntesis porque ++ tiene
// prioridad frente a *.
(*px)++; // Incrementa el valor al que apunta px en 1
printf("%d\n", *px); // => Muestra 1
printf("%d\n", x); // => Muestra 1

int x_array[20]; // Los arrays son una buena manera de distribuir bloques
int xx;          // continuos de memoria.
for (xx=0; xx<20; xx++) {
    x_array[xx] = 20 - xx;
} // Inicializa x_array a 20, 19, 18,... 2, 1

// Declara un puntero de tipo 'int' y lo inicializa para apuntar a x_array
int* x_ptr = x_array;
// x_ptr ahira apunta al primer elemento del 'array' (el entero 20).
// Esto funciona porque las 'arrays' actualmente son solo punteros a su
// primer elemento.

// Los 'arrays' son punteros a su primer elemento.
printf("%d\n", *(x_ptr)); // => Muestra 20
printf("%d\n", x_array[0]); // => Muestra 20

// Los punteros aumentan y disminuyen en función de su tipo.
printf("%d\n", *(x_ptr + 1)); // => Muestra 19
printf("%d\n", x_array[1]); // => Muestra 19

// Puedes también asigner dinamicamente bloques contiguos de memoria con
// la función malloc de la librería estándard, que toma un entero como
// argumento representando el número de bytes a asignar de la pila.
int* my_ptr = (int*) malloc(sizeof(int) * 20);
for (xx=0; xx<20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx funcionaría también aquí
} // Inicializa la memoria a 20, 19, 18, 17... 2, 1 (como 'ints')

// Desreferenciando la memoria que no has asignado te dará resultados
// impredecibles
printf("%d\n", *(my_ptr + 21)); // => Prints who-knows-what?

// Cuando hayas acabado con el bloque de memoría malloc, necesitas 
// liberarlo o sino nadie más podrá usarlo hasta que tu programa se cierre
free(my_ptr);

// Las cadenas pueden ser 'arrays' de chars, pero normalmente se
// representan con punteros 'char':
char* my_str = "This is my very own string";

printf("%c\n", *my_str); // => 'T'

function_1();
} // fin de la función main

///////////////////////////////////////
// Funciones
///////////////////////////////////////

// Sintexis de la declaración de funciones:
// <tipo de retorno> <nombre>(<argumentos>)

int add_two_ints(int x1, int x2){
    return x1 + x2; // Usa 'return' para dar una salida
}

/*
Las funciones son de paso por valor, pero puedes hacer tus propias 
referencias con punteros de manera que las funciones puedan cambiar sus 
valores.

Ejemplo: invertidor de cadenas in-situ
*/

// Una función 'void' no retorna valor
void str_reverse(char* str_in){
    char tmp;
    int ii=0, len = strlen(str_in); // Strlen es parte de la librería 
    for(ii=0; ii<len/2; ii++){      // estándard
        tmp = str_in[ii];
        str_in[ii] = str_in[len - ii - 1]; // ii-th último 'char'
        str_in[len - ii - 1] = tmp;
    }
}

/*
char c[] = "Esto es una prueba.";
str_reverse(c);
printf("%s\n", c); // => ".abeurp anu se otsE"
*/

///////////////////////////////////////
// Definición de tipos y estructuras
///////////////////////////////////////

// Los 'Typedefs' pueden ser utilizados para crear alias de tipos.
typedef int my_type;
my_type my_type_var = 0;

// Las estructuras son sólo grupos de datos.
struct rectangle {
    int width;
    int height;
};


void function_1(){

    struct rectangle my_rec;

    // Utiliza los miembros de una estructura con .
    my_rec.width = 10;
    my_rec.height = 20;

    // Puedes declarar punteros a estructuras
    struct rectangle* my_rec_ptr = &my_rec;

    // Usa la desreferencia para modificar sus miembros...
    (*my_rec_ptr).width = 30;

    // ... o usa la abreviatura ->
    my_rec_ptr->height = 10; // Lo mismo que (*my_rec_ptr).height = 10;
}

// Puedes aplicar un 'typedef' a una estructura por conveniencía.
typedef struct rectangle rect;

int area(rect r){
    return r.width * r.height;
}

///////////////////////////////////////
// Punteros a Funciones
///////////////////////////////////////
/*
En tiempo de ejecución,  las funciones se localizan en unas direcciones de
memoria concretas. Los punteros a funciones son como cualquier otro 
puntero (almacenan una dirección de memoria), pero pueden ser usados para 
utilizar funciones directamente, o para pasar 'handlers' (o funciones 
'callback') por todos lados.
Sin embargo, la sintaxis de definición parecera confusa al principio.

Ejemplo: usar str_reverse desde un puntero
*/
void str_reverse_through_pointer(char * str_in) {
    // Define un puntero a una función, llamado f.
    void (*f)(char *);
    // La armadura debe coincidir exactamente con al función objetivo.

    // Assigna la dirección de la función (determinado en tiempo de ejecuión)
    f = &str_reverse;

    // Llamando la función desde el puntero
    (*f)(str_in);

    // Esta es una alternativa para llamarla pero con una sintaxis igual de válida.
    // f(str_in);
}

/*
Tanto tiempo como las armaduras de las funciones coincidan, podrás asignar
cualquier función al mismo puntero.
Los punteros a funciones  son normalmente envueltos en 'typedef' para
simplificar su legibilidad, como sigue:
*/

typedef void (*my_fnp_type)(char *);

// Es usado para declarar la variable puntero actual:
// ...
// my_fnp_type f; 

```

## Otras lecturas

Lo mejor que puedes encontrar es una copia de [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language). Es *el*
libro de C, escrito por Dennis Ritchie, creador de C y Brian Kernighan. Aún así,
se cuidadoso, es antiguo, contiene algunas inexactitudes, y algunas prácticas 
han cambiado.

Otro buen recurso es [Learn C the hard way](http://learncodethehardway.org/c/).

Si tienes una pregunta, lee [compl.lang.c Frequently Asked Questions](http://c-faq.com).

Es muy importante utilizar el espaciado y la sangría apropiados y ser coherente 
con su estilo de codificación en general. El código legible es mejor que el 
código rápido. Para adoptar un buen estilo de codificación, vea el
[Estilo de codificación del kernel Linux] (https://www.kernel.org/doc/Documentation/CodingStyle).

Aparte de eso, Google es tu amigo.
