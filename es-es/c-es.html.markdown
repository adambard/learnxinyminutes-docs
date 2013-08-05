---
language: c
filename: learnc.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Francisco Garc�a", "http://flaskbreaker.tumblr.com/"]
lang: es-es
---

�Ah!, C. Aun hoy en d�a sigue siendo el lenguaje por excelencia de la 
computaci�n moderna de alto rendimiento.

C es el lenguaje de m�s bajo nivel que la mayor�a de los programadores
llegar�n a usar, pero lo compensa de sobra con pura velocidad. Solo
ten en cuenta el manejo manual de memoria y te llevar� tan lejos como
necesites.

```c
// Los comentarios de una sola l�nea comienzan con //

/*
Los comentarios multil�nea tienen este aspecto.
*/

// Importa cabeceras con #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Declara por adelantado las armaduras de las funciones en un archivo .h,
// o al principio de tu archivo .c .
void function_1();
void function_2();

// El punto de entrada de tu programa es una funci�n llamada main con
// retorno de tipo entero (integer).
int main() {

// Muestra la salida usando printf, para el "formato print"
// %d es un entero, \n es una nueva l�nea
printf("%d\n", 0); // => Muestra 0
// Todas las sentencias deben terminar con un punto y coma.

///////////////////////////////////////
// Tipos
///////////////////////////////////////

// Tienes que declarar una variable antes de usarla. La declaraci�n de una
// variable necesites que especifiques su tipo; el tipo de una variable
// determina su tama�o en bytes.

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

// 'floats' son normalmente n�meros de coma flotante de 32 bits
float x_float = 0.0;

// 'doubles' son normalmente n�meros de coma flotante de 64 bits
double x_double = 0.0;

// Todos los tipos enteros pueden ser 'unsigned'. Esto significa que no
// pueden ser negativos, pero el valor m�ximo de una variable 'unsigned'
// es mayor que el de una no 'unsigned' del mismo tama�o.
unsigned char ux_char;
unsigned short ux_short;
unsigned int ux_int;
unsigned long long ux_long_long;

// Todos menos 'char', que es siempre de 1 byte, var�an el tama�o 
// dependiendo de tu m�quina. sizeof(T) te dice el tama�o de una variable
// de tipo T en bytes por lo que podemos expresar el tama�o de estos tipos
// portatilmente.
// Por ejemplo,
printf("%lu\n", sizeof(int)); // => 4 (en m�quinas con 'words' de 4 bytes)

// Los arrays deben ser inicializados con un tama�o concreto.
char my_char_array[20]; // Este array ocupa 1 * 20 = 20 bytes
int my_int_array[20]; // Este array ocupa 4 * 20 = 80 bytes
                      // (suponiendo que tenemos 'words' de 4-byte)


// Puedes inicializar un array a 0 as�:
char my_array[20] = {0};

// Indexar un array es como en otros lenguajes -o, m�s bien, otros
// lenguajes son como C-
my_array[0]; // => 0

// Los arrays var�an; �son s�lo memoria!
my_array[1] = 2;
printf("%d\n", my_array[1]); // => 2

// Las cadenas (strings) son s�lo arrays de 'chars' (caracteres)
// terminados en un byte NUL (0x00), representado en las cadenas como el car�cter especial '\0'.
// (No tenemos porqu� a�adir el byte nulo en cadenas literales; el
//  compilador lo a�ade al final por nosotros.)
char a_string[20] = "Esto es una cadena";
printf("%s\n", a_string); // %s se sutituye por una cadena.

/*
Te habr�s dado cuenta de que a_string es solo de 18 caracteres.
El 'char' #19 es el byte nulo. 
El 'char' #20 es de valor indefinido.
*/

printf("%d\n", a_string[18]); // => 0

///////////////////////////////////////
// Operadores
///////////////////////////////////////

int i1 = 1, i2 = 2; // Forma corta de declaraci�n m�ltiple
float f1 = 1.0, f2 = 2.0;

// La aritm�tica es sencilla
i1 + i2; // => 3
i2 - i1; // => 1
i2 * i1; // => 2
i1 / i2; // => 0 (0.5, pero es truncado tras el 0)

f1 / f2; // => 0.5, m�s o menos �psilon
// M�dulo est� tambi�n
11 % 3; // => 2

// Los operadores de comparaci�n te resultaran familiares, pero no hay
// booleanos en C. Usamos enteros (ints) en su lugar. 0 es falso,
// cualquier otra cosa es verdadero. (Los operadores de comparaci�n 
// siempre devuelven 0 o 1)
3 == 2; // => 0 (Falso)
3 != 2; // => 1 (Verdadero)
3 > 2; // => 1
3 < 2; // => 0
2 <= 2; // => 1
2 >= 2; // => 1

// La l�gica funiona en enteros
!3; // => 0 (not l�gico)
!0; // => 1
1 && 1; // => 1 (and l�gico)
0 && 1; // => 0
0 || 1; // => 1 (or l�gico)
0 || 0; // => 0

// �Operadores de bits!
~0x0F; // => 0xF0 (Negaci�n)
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
    printf("%d, ", ii++); // ii++ incrementa ii en uno, despu�s de usar su valor.
} // => muestra "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

int kk = 0;
do {
    printf("%d, ", kk);
} while (++kk < 10); // ++kk incrementa kk en uno, antes de usar su valor.
// => muestra "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

printf("\n");

// Bucles 'for' tambi�n
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

// El cambio de tipos intentar� mantener sus valores num�ricos
printf("%d\n", x_hex); // => Muestra 1
printf("%d\n", (short) x_hex); // => Muestra 1
printf("%d\n", (char) x_hex); // => Muestra 1

// Los tipos se desbordan sin aviso
printf("%d\n", (char) 257); // => 1 (El valor m�ximo de un 'char' es 255)

// Los tipos enteros puden cambiarse a tipos de coma flotante, y viceversa
printf("%f\n", (float)100); // %f se sustituye por un 'float'
printf("%lf\n", (double)100); // %lf se sustituye por un 'double'
printf("%d\n", (char)100.0);

///////////////////////////////////////
// Punteros
///////////////////////////////////////

// Un puntero es una variable declarada para almacenar una direcci�n de 
// memoria. Su declaraci�n adem�s nos dir� el tipo de dato al que apunta. 
// Puedes obtener la direcci�n de memoria de tus variables, y despu�s
// enlazarlas con ellos.

int x = 0;
printf("%p\n", &x); // Usa & para obtener la direcci�n de una variable.
// (%p se sustituye por un puntero)
// => Muestra alguna direcci�n de memoria;

// Los tipos de puntero terminan con * en su declaraci�n
int* px; // px es un puntero a un 'int'
px = &x; // Almacena la direcci�n de x en px
printf("%p\n", px); // => Muestra alguna direcci�n de memoria

// Para obtener el valor de la direcci�n a la que apunta un puntero, pon
// * delante para desreferenciarle. 
printf("%d\n", *px); // => Muestra 0, el valor de x y de la direcci�n a la
                     //    que apunta px

// Tambi�n puedes cambiar el valor al que est� apuntando el puntero.
// Tenemos que meter la desreferencia entre par�ntesis porque ++ tiene
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

// Los punteros aumentan y disminuyen en funci�n de su tipo.
printf("%d\n", *(x_ptr + 1)); // => Muestra 19
printf("%d\n", x_array[1]); // => Muestra 19

// Puedes tambi�n asigner dinamicamente bloques contiguos de memoria con
// la funci�n malloc de la librer�a est�ndard, que toma un entero como
// argumento representando el n�mero de bytes a asignar de la pila.
int* my_ptr = (int*) malloc(sizeof(int) * 20);
for (xx=0; xx<20; xx++) {
    *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx funcionar�a tambi�n aqu�
} // Inicializa la memoria a 20, 19, 18, 17... 2, 1 (como 'ints')

// Desreferenciando la memoria que no has asignado te dar� resultados
// impredecibles
printf("%d\n", *(my_ptr + 21)); // => Prints who-knows-what?

// Cuando hallas acabado con el bloque de memor�a malloc, necesitas 
// liberarlo o sino nadie m�s podr� usarlo hasta que tu programa se cierre
free(my_ptr);

// Las cadenas pueden ser 'arrays' de chars, pero normalmente se
// representan con punteros 'char':
char* my_str = "This is my very own string";

printf("%c\n", *my_str); // => 'T'

function_1();
} // fin de la funci�n main

///////////////////////////////////////
// Funciones
///////////////////////////////////////

// Sintexis de la declaraci�n de funciones:
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

// Una funci�n 'void' no retorna valor
void str_reverse(char* str_in){
    char tmp;
    int ii=0, len = strlen(str_in); // Strlen es parte de la librer�a 
    for(ii=0; ii<len/2; ii++){      // est�ndard
        tmp = str_in[ii];
        str_in[ii] = str_in[len - ii - 1]; // ii-th �ltimo 'char'
        str_in[len - ii - 1] = tmp;
    }
}

/*
char c[] = "Esto es una prueba.";
str_reverse(c);
printf("%s\n", c); // => ".abeurp anu se otsE"
*/

///////////////////////////////////////
// Definici�n de tipos y estructuras
///////////////////////////////////////

// Los 'Typedefs' pueden ser utilizados para crear alias de tipos.
typedef int my_type;
my_type my_type_var = 0;

// Las estructuras son s�lo grupos de datos.
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

// Puedes aplicar un 'typedef' a una estructura por convenienc�a.
typedef struct rectangle rect;

int area(rect r){
    return r.width * r.height;
}

///////////////////////////////////////
// Punteros a Funciones
///////////////////////////////////////
/*
En tiempo de ejecuci�n,  las funciones se localizan en unas direcciones de
memoria concretas. Los punteros a funciones son como cualquier otro 
puntero (almacenan una direcci�n de memoria), pero pueden ser usados para 
utilizar funciones directamente, o para pasar 'handlers' (o funciones 
'callback') por todos lados.
Sin embargo, la sintaxis de definici�n parecera confusa al principio.

Ejemplo: usar str_reverse desde un puntero
*/
void str_reverse_through_pointer(char * str_in) {
    // Define un puntero a una funci�n, llamado f.
    void (*f)(char *); // La armadura debe coincidir exactamente con al funci�n objetivo.
    f = &str_reverse; // Assigna la direcci�n de la funci�n (determinado en tiempo de ejecui�n)
    (*f)(str_in); // Llamando la funci�n desde el puntero
    // f(str_in); // Esta es una alternativa para llamarla pero con una sintaxis igual de v�lida.
}

/*
Tanto tiempo como las armaduras de las funciones coincidan, podr�s asignar
cualquier funci�n al mismo puntero.
Los punteros a funciones  son normalmente envueltos en 'typedef' para
simplificar su legibilidad, como sigue:
*/

typedef void (*my_fnp_type)(char *);

// Es usado para declarar la variable puntero actual:
// ...
// my_fnp_type f; 

```

## Otras lecturas

Lo mejor que puedes en contrar es una copia de [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language)

Otro buen recurso es [Learn C the hard way](http://c.learncodethehardway.org/book/)

Aparte de eso, Google es tu amigo.
