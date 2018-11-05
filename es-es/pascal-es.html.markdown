---
language: Pascal
filename: learnpascal-es.pas
contributors:
    - ["Ganesha Danu", "http://github.com/blinfoldking"]
    - ["Keith Miyake", "https://github.com/kaymmm"]
translators:
    - ["Ivan Alburquerque", "https://github.com/AlburIvan"]
lang: es-es
---


>Pascal es un lenguaje de programación imperativo y de procedimiento, que Niklaus Wirth diseñó en 1968–69 y publicó en 1970, como un lenguaje pequeño y eficiente destinado a fomentar las buenas prácticas de programación utilizando programación estructurada y estructuración de datos. Se nombra en honor al matemático, filósofo y físico francés Blaise Pascal. fuente: [wikipedia](https://es.wikipedia.org/wiki/Pascal_(lenguaje_de_programación)))

Para compilar y ejecutar un programa pascal puede usar un compilador pascal gratuito. [Descargar aquí](https://www.freepascal.org/)

```pascal
//Anatomía de un programa en Pascal 
//Esto es un comentario
{
    Esto es un
    comentario multilínea
}

//nombre del programa
program learn_pascal; //<-- no olvides el punto y coma

const
    {
        Aquí es donde se debe declarar valores constantes.
    }
type
    {
       Aquí es donde se debe declarar un tipo de datos personalizado
    }
var
    {
        aquí es donde se debe declarar una variable
    }

//área principal del programa
begin
    {
        área para declarar su instrucción
    }
end. // El final de un área principal del programa debe requerir un símbolo "." 
```

```pascal
//declarando variable
//puedes hacer esto
var a:integer;
var b:integer;
//o esto
var 
    a : integer;
    b : integer;
//o esto
var a,b : integer;
```

```pascal
program Learn_More;
//Aprendamos sobre los tipos de datos y sus operaciones.

const
    PI = 3.141592654;
    GNU = 'GNU No Es Unix';
        // las constantes se nombran convencionalmente usando CAPS (mayúscula)
        // sus valores son fijos y no se pueden cambiar durante el tiempo de ejecución
        // tiene cualquier tipo de datos estándar (enteros, reales, booleanos, characteres, cadenas)

type
    ch_array : array [0..255] of char;
        // los  son nuevos 'tipos' que especifican la longitud y el tipo de datos
        // esto define un nuevo tipo de datos que contiene 255 caracteres
        // (esto es funcionalmente equivalente a una variable string[256])
    md_array : array of array of integer;
        // los arreglos anidados son equivalentes a los arreglos multidimensionales
        // puede definir arreglos de longitud cero (0) que son de tamaño dinámico
        // esta es una matriz bidimensional de enteros

//Declarando variables
var
    int, c, d  : integer;
           // Tres variables que contienen números enteros.
           // los enteros son de 16 bits y están limitados al rango [-32,768..32,767]
    r    : real;
           // una variable que contiene un número real como tipos de datos
           // el rango de los reales pueden variar entre [3.4E-38..3.4E38]
    bool : boolean;
           // una variable que contiene un valor booleano (True/False)
    ch   : char;
           // una variable que contiene un valor de carácter
           // Las variables char se almacenan como tipos de datos de 8 bits, por lo que no hay UTF
    str  : string;
           // una variable no estándar que contiene un valor de cadena
           // Las cadenas son una extensión incluida en la mayoría de los compiladores de Pascal.
           // se almacenan como una matriz de caracteres con una longitud predeterminada de 255.
    s    : string[50];
           // una cadena con longitud máxima de 50 caracteres.
           // puede especificar la longitud de la cadena para minimizar el uso de memoria
    my_str: ch_array;
           // Puedes declarar variables de tipos personalizados.
    my_2d : md_array;
           // Las matrices de tamaño dinámico deben dimensionarse antes de que puedan usarse.

    // tipos de datos enteros adicionales
    b    : byte;     // rango [0..255]
    shi  : shortint; // rango [-128..127]
    smi  : smallint; // rango [-32,768..32,767] (entero estándar)
    w    : word;     // rango [0..65,535]
    li   : longint;  // rango [-2,147,483,648..2,147,483,647]
    lw   : longword; // rango [0..4,294,967,295]
    c    : cardinal; // longword
    i64  : int64;    // rango [-9223372036854775808..9223372036854775807]
    qw   : qword;    // rango [0..18,446,744,073,709,551,615]

    // tipos reales adicionales
    rr   : real;     // rango depende de la plataforma (i.e., 8-bit, 16-bit, etc.)
    rs   : single;   // rango [1.5E-45..3.4E38]
    rd   : double;   // rango [5.0E-324 .. 1.7E308]
    re   : extended; // rango [1.9E-4932..1.1E4932]
    rc   : comp;     // rango [-2E64+1 .. 2E63-1]

Begin
    int := 1;// como asignar un valor a una variable
    r   := 3.14;
    ch  := 'a';
    str := 'manzana';
    bool := true;
    //pascal no es un lenguaje sensible a mayúsculas y minúsculas
    //operación aritmética
    int := 1 + 1; // int = 2 sobrescribiendo la asignacion anterior
    int := int + 1; // int = 2 + 1 = 3;
    int := 4 div 2; //int = 2 operación de división donde el resultado será redondeado.
    int := 3 div 2; //int = 1
    int := 1 div 2; //int = 0

    bool := true or false; // bool = true
    bool := false and true; // bool = false
    bool := true xor true; // bool = false

    r := 3 / 2; // un operador de división para reales
    r := int; // Puede asignar un entero a una variable real pero no a la inversa

    c := str[1]; // asigna la primera letra de str a c
    str := 'hola' + 'mundo'; // combinando cadenas

    my_str[0] := 'a'; // asignación de matriz necesita un índice

    setlength(my_2d,10,10); // inicializa matrices de tamaño dinámico: matriz 10 × 10
    for c := 0 to 9 do // los arreglos comienzan en 0 y terminan en longitud - 1
        for d := 0 to 9 do // Para los contadores de bucle hay que declarar variables.
        my_2d[c,d] := c * d;
          // aborda las matrices multidimensionales con un único conjunto de corchete

End.
```

```pascal
program Functional_Programming;

Var
    i, dummy : integer;

function recursion_factorial(const a: integer) : integer;
{ calcula recursivamente el factorial del parámetro entero a }

// Declare variables locales dentro de la función.
// e.g.:
// Var
//    local_a : integer;

Begin
    If a >= 1 Then
    // devuelva valores de las funciones asignando un valor al nombre de la función
        recursion_factorial := a * recursion_factorial(a-1)
    Else
        recursion_factorial := 1;
End; // termine una función usando un punto y coma después de la instrucción End.

procedure obtener_entero(var i : integer; dummy : integer);
{ obten la entrada del usuario y almacenarla en el parámetro entero i. 
  los parámetros que preceden a 'var' son variables, lo que significa que su valor 
  puede cambiar fuera del parámetro. Los parámetros de valor (sin 'var') como 'dummy' 
  son estáticos y los cambios realizados dentro del alcance de la función/procedimiento 
  no afectan la variable que se pasa como parámetro }

Begin
    write('Escriba un entero: ');
    readln(i);
    dummy := 4; // dummy no cambiará el valor fuera del procedimiento
End;

Begin // bloque de programa principal
    dummy := 3;
    obtener_entero(i, dummy);
    writeln(i, '! = ', recursion_factorial(i));
    // muestra i!
    writeln('dummy = ', dummy); // siempre muestra '3' ya que dummy no ha cambiado.
End.

```

