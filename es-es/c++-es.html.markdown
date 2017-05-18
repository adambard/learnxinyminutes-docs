---
language: c++
filename: learncpp-es.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Connor Waters", "http://github.com/connorwaters"]
translators:
    - ["Gerson Lázaro", "https://gersonlazaro.com"]
lang: es-es
---

C++ es un lenguaje de programación de sistemas que,
[de acuerdo a su inventor Bjarne Stroustrup](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote),
fue diseñado para

- ser un "mejor C"
- soportar abstracción de datos
- soportar programación orientada a objetos
- soportar programación genérica

Aunque su sintaxis puede ser más difícil o compleja que los nuevos lenguajes,
es ampliamente utilizado, ya que compila instrucciones nativas que pueden ser
directamente ejecutadas por el procesador y ofrece un estricto control sobre
el hardware (como C), mientras ofrece características de alto nivel como
genericidad, excepciones, y clases. Esta combinación de velocidad y
funcionalidad hace de C ++ uno de los lenguajes de programación más utilizados.

```c++
////////////////////
// Comparación con C
////////////////////

// C ++ es _casi_ un superconjunto de C y comparte su sintaxis básica para las
// declaraciones de variables, tipos primitivos y funciones.

// Al igual que en C, el punto de entrada de tu programa es una función llamada
// main con un retorno de tipo entero.
// Este valor sirve como código de salida del programa.
// Mira http://en.wikipedia.org/wiki/Exit_status para mayor información.
int main(int argc, char** argv)
{
	// Los argumentos de la línea de comandos se pasan por argc y argv de la
	// misma manera que en C.
	// argc indica el número de argumentos,
	// y argv es un arreglo de strings de estilo C (char*)
	// representando los argumentos.
	// El primer argumento es el nombre con el que el programa es llamado.
	// argc y argv pueden omitirse si no te preocupan los argumentos,
	// dejando la definición de la función como int main ()

	// Un estado de salida 0 indica éxito.
    return 0;
}

// Sin embargo, C ++ varía en algunas de las siguientes maneras:

// En C++, los caracteres literales son caracteres
sizeof('c') == sizeof(char) == 1

// En C, los caracteres literales son enteros
sizeof('c') == sizeof(int)


// C++ tiene prototipado estricto
void func(); // función que no acepta argumentos

// En C
void func(); // función que puede aceptar cualquier número de argumentos

// Use nullptr en lugar de NULL en C++
int* ip = nullptr;

// Las cabeceras (headers) estándar de C están disponibles en C ++,
// pero tienen el prefijo "c" y no tienen sufijo .h.
#include <cstdio>

int main()
{
    printf("Hola mundo!\n");
    return 0;
}

//////////////////////////
// Sobrecarga de funciones
//////////////////////////

// C++ soporta sobrecarga de funciones
// siempre que cada función tenga diferentes parámetros.

void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("Mi entero es %d", myInt);
}

int main()
{
    print("Hello"); // Resolves to void print(const char*)
    print(15); // Resolves to void print(int)
}

////////////////////////////////////
// Argumentos de función por defecto
////////////////////////////////////

// Puedes proporcionar argumentos por defecto para una función si no son
// proporcionados por quien la llama.

void doSomethingWithInts(int a = 1, int b = 4)
{
    // Hacer algo con los enteros aqui
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// Los argumentos predeterminados deben estar al final de la lista de argumentos.

void invalidDeclaration(int a = 1, int b) // Error!
{
}

/////////////////////
// Espacios de nombre
/////////////////////

// Espacios de nombres proporcionan ámbitos separados para variable, función y
// otras declaraciones.
// Los espacios de nombres se pueden anidar.

namespace First {
    namespace Nested {
        void foo()
        {
            printf("Esto es First::Nested::foo\n");
        }
    } // fin del nombre de espacio Nested
} // fin del nombre de espacio First

namespace Second {
    void foo()
    {
        printf("Esto es Second::foo\n")
    }
}

void foo()
{
    printf("Este es global: foo\n");
}

int main()
{

	// Incluye todos los símbolos del espacio de nombre Second en el ámbito
	// actual. Tenga en cuenta que simplemente foo() no funciona, ya que ahora
	// es ambigua si estamos llamando a foo en espacio de nombres Second o en
	// el nivel superior.
    using namespace Second;

    Second::foo(); // imprime "Esto es Second::foo"
    First::Nested::foo(); // imprime "Esto es First::Nested::foo"
    ::foo(); // imprime "Este es global: foo"
}

/////////////////
// Entrada/Salida
/////////////////

// La entrada y salida de C++ utiliza flujos (streams)
// cin, cout, y cerr representan a stdin, stdout, y stderr.
// << es el operador de inserción >> es el operador de extracción.


#include <iostream> // Incluir para el flujo de entrada/salida

using namespace std; // Los streams estan en std namespace (libreria estandar)

int main()
{
   int myInt;

   // Imprime a la stdout (o terminal/pantalla)
   cout << "Ingresa tu número favorito:\n";
   // Toma una entrada
   cin >> myInt;

   // cout puede también ser formateado
   cout << "Tu número favorito es " << myInt << "\n";
   // imprime "Tu número favorito es <myInt>"

    cerr << "Usado para mensajes de error";
}
////////////////////
// Cadenas (Strings)
////////////////////

// Las cadenas en C++ son objetos y tienen muchas funciones
#include <string>

using namespace std; // Strings también estan en namespace std

string myString = "Hola";
string myOtherString = " Mundo";

// + es usado para concatenar.
cout << myString + myOtherString; // "Hola Mundo"

cout << myString + " Tu"; // "Hola Tu"

// Las cadenas en C++ son mutables y tienen valor semántico.
myString.append(" Perro");
cout << myString; // "Hola Perro"


//////////////
// Referencias
//////////////

// Además de punteros como los de C,
// C++ tiene _references_.
// Estos tipos de puntero no pueden ser reasignados una vez establecidos
// Y no pueden ser nulos.
// También tienen la misma sintaxis que la propia variable:
// No es necesaria * para eliminar la referencia y
// & (dirección) no se utiliza para la asignación.

using namespace std;

string foo = "Yo soy foo";
string bar = "Yo soy bar";

string& fooRef = foo; // Crea una referencia a foo.
fooRef += ". Hola!"; // Modifica foo través de la referencia
cout << fooRef; // Imprime "Yo soy foo. Hola!"

// No trate de reasignar "fooRef". Esto es lo mismo que "foo = bar", y
//   foo == "Yo soy bar"
// después de esta linea.
fooRef = bar;

const string& barRef = bar; // Crea una referencia constante a bar.
// Como en C, los valores constantes (y punteros y referencias) no pueden ser
// modificados.
barRef += ". Hola!"; // Error, referencia constante no puede ser modificada.

// Sidetrack: Antes de hablar más sobre referencias, hay que introducir un
// concepto llamado objeto temporal. Supongamos que tenemos el siguiente código:
string tempObjectFun() { ... }
string retVal = tempObjectFun();

// Lo que pasa en la segunda línea es en realidad:
// - Un objeto de cadena es retornado desde tempObjectFun
// - Una nueva cadena se construye con el objeto devuelto como argumento al
// constructor
// - El objeto devuelto es destruido
// El objeto devuelto se llama objeto temporal. Objetos temporales son
// creados cada vez que una función devuelve un objeto, y es destruido en el
// fin de la evaluación de la expresión que encierra (Bueno, esto es lo que la
// norma dice, pero los compiladores están autorizados a cambiar este
// comportamiento. Busca "return value optimization" para ver mas detalles).
// Así que en este código:
foo(bar(tempObjectFun()))

// Suponiendo que foo y bar existen, el objeto retornado de tempObjectFun es
// pasado al bar, y se destruye antes de llamar foo.

// Ahora, de vuelta a las referencias. La excepción a la regla "en el extremo
// de la expresión encerrada" es si un objeto temporal se une a una
// referencia constante, en cuyo caso su vida se extiende al ámbito actual:

void constReferenceTempObjectFun() {
  // ConstRef obtiene el objeto temporal, y es válido hasta el final de esta
  // función.
  const string& constRef = tempObjectFun();
  ...
}

// Otro tipo de referencia introducida en C ++ 11 es específicamente para
// objetos temporales. No se puede tener una variable de este tipo, pero tiene
// prioridad en resolución de sobrecarga:

void someFun(string& s) { ... }  // Referencia regular
void someFun(string&& s) { ... }  // Referencia a objeto temporal

string foo;
someFun(foo);  // Llama la función con referencia regular
someFun(tempObjectFun());  // Llama la versión con referencia temporal

// Por ejemplo, puedes ver estas dos versiones de constructores para
// std::basic_string:
basic_string(const basic_string& other);
basic_string(basic_string&& other);

// La idea es que si estamos construyendo una nueva cadena de un objeto temporal
// (que va a ser destruido pronto de todos modos), podemos tener un constructor
// mas eficiente que "rescata" partes de esa cadena temporal. Usted verá este
// Concepto denominado "movimiento semántico".

////////////////////////////////////////////
// Clases y programación orientada a objetos
////////////////////////////////////////////

// Primer ejemplo de clases
#include <iostream>

// Declara una clase.
// Las clases son usualmente declaradas en archivos de cabeceras (.h o .hpp)
class Dog {
    // Variables y funciones de la clase son privados por defecto.
    std::string name;
    int weight;

// Todos los miembros siguientes de este son públicos
// Hasta que se encuentre "private" o "protected".
// All members following this are public
// until "private:" or "protected:" is found.
public:

    // Constructor por defecto
    Dog();

	// Declaraciones de funciones de la clase (implementaciones a seguir)
    // Nota que usamos std::string aquí en lugar de colocar
    // using namespace std;
    // arriba.
    // Nunca ponga una declaración "using namespace" en un encabezado.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);
	// Funciones que no modifican el estado del objeto
	// Deben marcarse como const.
	// Esto le permite llamarlas si se envia una referencia constante al objeto.
	// También tenga en cuenta que las funciones deben ser declaradas
	// explícitamente como _virtual_ para que sea reemplazada en las clases
	// derivadas.
	// Las funciones no son virtuales por defecto por razones de rendimiento.
    virtual void print() const;

    // Las funciones también se pueden definir en el interior
    // del cuerpo de la clase.
	// Funciones definidas como tales están entre líneas automáticamente.
    void bark() const { std::cout << name << " barks!\n"; }

    // Junto a los constructores, C++ proporciona destructores.
	// Estos son llamados cuando un objeto se elimina o está fuera del ámbito.
	// Esto permite paradigmas potentes como RAII
	// (mira abajo)
	// El destructor debe ser virtual si una clase es dervada desde el;
	// Si no es virtual, entonces la clase derivada destructor
	// No será llamada si el objeto se destruye a través de una referencia de
	// la clase base o puntero.
    virtual ~Dog();



}; // Un punto y coma debe seguir la definición de clase.

// Las funciones de una clase son normalmente implementados en archivos .cpp.
Dog::Dog()
{
    std::cout << "Un perro ha sido construido\n";
}

// Objetos (tales como cadenas) deben ser pasados por referencia
// Si los estas modificando o referencia constante en caso contrario.
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// Nota que "virtual" sólo se necesita en la declaración, no en la definición.
void Dog::print() const
{
    std::cout << "El perro es " << name << " y pesa " << weight << "kg\n";
}

Dog::~Dog()
{
    std::cout << "Adiós " << name << "\n";
}

int main() {
    Dog myDog; // imprime "Un perro ha sido construido"
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.print(); // imprime "El perro es Barkley y pesa 10 kg"
    return 0;
} // imprime "Adiós Barkley"

// Herencia:

// Esta clase hereda todo lo público y protegido de la clase Dog
class OwnedDog : public Dog {

    void setOwner(const std::string& dogsOwner);

	// Reemplaza el comportamiento de la función de impresión
	// de todos los OwnedDogs. Mira
	// http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping
	// Para una introducción más general si no está familiarizado con el
	// polimorfismo de subtipo.
	// La palabra clave override es opcional, pero asegura que estás
	// reemplazando el método de una clase base.
    void print() const override;

private:
    std::string owner;
};

// Mientras tanto, en el archivo .cpp correspondiente:

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // Llama a la función de impresión en la clase base Dog
    std::cout << "El perro es de " << owner << "\n";
    // Imprime "El perro es <name> y pesa <weight>"
    //         "El perro es de <owner>"
}

////////////////////////////////////////////
// Inicialización y sobrecarga de operadores
////////////////////////////////////////////

// En C ++ se puede sobrecargar el comportamiento
// de los operadores como +, -, *, /, etc.
// Esto se hace mediante la definición de una función que es llamada
// cada vez que se utiliza el operador.

#include <iostream>
using namespace std;

class Point {
public:
    // Las variables de la clase pueden dar valores por defecto de esta manera.
    double x = 0;
    double y = 0;

	// Define un constructor por defecto que no hace nada
    // pero inicializa el punto al valor por defecto (0, 0)
    Point() { };

    // The following syntax is known as an initialization list
    // and is the proper way to initialize class member values
    Point (double a, double b) :
        x(a),
        y(b)
    { /* No hace nada excepto inicializar los valores */ }

    // Sobrecarga el operador +
    Point operator+(const Point& rhs) const;

    // Sobrecarga el operador +=
    Point& operator+=(const Point& rhs);

    // También tendría sentido añadir los operadores - y -=,
   	// Pero vamos a omitirlos por razones de brevedad.
};

Point Point::operator+(const Point& rhs) const
{
    // Crea un nuevo punto que es la suma de este y rhs.
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Point up (0,1);
    Point right (1,0);
    // Llama al operador + de Point
    // Point llama la función + con right como parámetro
    Point result = up + right;
    // Prints "Result is upright (1,1)"
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

/////////////////////////
// Plantillas (Templates)
/////////////////////////

// Las plantillas en C++ se utilizan sobre todo en la programación genérica,
// a pesar de que son mucho más poderoso que los constructores genéricos
// en otros lenguajes. Ellos también soportan especialización explícita y
// parcial y clases de tipo estilo funcional; de hecho, son un lenguaje
// funcional Turing-completo incrustado en C ++!

// Empezamos con el tipo de programación genérica que podría estar
// familiarizado.
// Para definir una clase o función que toma un parámetro de tipo:
template<class T>
class Box {
public:
    // En este caso, T puede ser usado como cualquier otro tipo.
    void insert(const T&) { ... }
};

// Durante la compilación, el compilador realmente genera copias de cada
// plantilla con parámetros sustituidos, por lo que la definición completa
// de la clase debe estar presente en cada invocación.
// Es por esto que usted verá clases de plantilla definidas
// Enteramente en archivos de cabecera.

//Para crear una instancia de una clase de plantilla en la pila:
Box<int> intBox;

y puedes utilizarlo como era de esperar:
intBox.insert(123);

// Puedes, por supuesto, anidar plantillas:
Box<Box<int> > boxOfBox;
boxOfBox.insert(intBox);

// Hasta C++11, había que colocar un espacio entre los dos '>'s,
// de lo contrario '>>' serían analizados como el operador de desplazamiento
// a la derecha.


// A veces verás
//   template<typename T>
// en su lugar. La palabra clave "class" y las palabras clave "typename" son
// mayormente intercambiables en este caso. Para la explicación completa, mira
//   http://en.wikipedia.org/wiki/Typename
// (sí, esa palabra clave tiene su propia página de Wikipedia).

// Del mismo modo, una plantilla de función:
template<class T>
void barkThreeTimes(const T& input)
{
    input.bark();
    input.bark();
    input.bark();
}

// Observe que no se especifica nada acerca de los tipos de parámetros aquí.
// El compilador generará y comprobará cada invocación de la plantilla,
// por lo que la función anterior funciona con cualquier tipo "T"
// que tenga un método 'bark' constante!


Dog fluffy;
fluffy.setName("Fluffy")
barkThreeTimes(fluffy); // Imprime "Fluffy barks" 3 veces.

Los parámetros de la plantilla no tienen que ser las clases:
template<int Y>
void printMessage() {
  cout << "Aprende C++ en " << Y << " minutos!" << endl;
}

// Y usted puede especializar explícitamente plantillas
// para código más eficiente.
// Por supuesto, la mayor parte del mundo real que utiliza una especialización
// no son tan triviales como esta.
// Tenga en cuenta que usted todavía tiene que declarar la función (o clase)
// como plantilla incluso si ha especificado de forma explícita todos
// los parámetros.

template<>
void printMessage<10>() {
  cout << "Aprende C++ rapido en solo 10 minutos!" << endl;
}

printMessage<20>();  // Prints "Aprende C++ en 20 minutos!"
printMessage<10>();  // Prints "Aprende C++ rapido en solo 10 minutos!"


/////////////////////
// Manejador de excepciones
/////////////////////

// La biblioteca estándar proporciona algunos tipos de excepción
// (mira http://en.cppreference.com/w/cpp/error/exception)
// pero cualquier tipo puede ser lanzado como una excepción
#include <exception>
#include <stdexcept>

//Todas las excepciones lanzadas dentro del bloque _try_ pueden ser
// capturados por los siguientes manejadores _catch_.
try {
    // No asignar excepciones en el heap usando _new_.
    throw std::runtime_error("Ocurrió un problema");
}

// Captura excepciones por referencia const si son objetos
catch (const std::exception& ex)
{
    std::cout << ex.what();
}
********************************************************************************
// Captura cualquier excepción no capturada por bloques _catch_ anteriores
catch (...)
{
    std::cout << "Excepción desconocida capturada";
    throw; // Re-lanza la excepción
}

///////
// RAII
///////

// RAII significa "Resource Acquisition Is Initialization"
// (Adquisición de recursos es inicialización).
// A menudo se considera el paradigma más poderoso en C++
// Y el concepto es simple: un constructor de un objeto
// Adquiere recursos de ese objeto y el destructor les libera.

// Para entender cómo esto es útil,
// Considere una función que utiliza un identificador de archivo C:
void doSomethingWithAFile(const char* filename)
{
    // Para empezar, asuma que nada puede fallar.

    FILE* fh = fopen(filename, "r"); // Abre el archivo en modo lectura

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // Cierra el manejador de archivos
}

// Por desgracia, las cosas se complican rápidamente por el control de errores.
// Supongamos que fopen puede fallar, y que doSomethingWithTheFile y
// DoSomethingElseWithIt retornan códigos de error si fallan.
// 	(Excepciones son la mejor forma de manejar los fallos,
// 	 pero algunos programadores, especialmente los que tienen un fondo C,
// 	 estan en desacuerdo sobre la utilidad de las excepciones).
// Ahora tenemos que comprobar cada llamado por fallos y cerrar el manejador
// del archivo si se ha producido un problema.
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Abre el archivo en modo lectura
    if (fh == nullptr) // El puntero retornado es nulo o falla.
        return false; // Reporta el fallo a quien hizo el llamado.

    // Asume que cada función retorna falso si falla
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // Cierre el manejador de archivo para que no se filtre.
        return false; // Propaga el error.
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // Cierre el manejador de archivo para que no se filtre.
        return false; // Propaga el error.
    }

    fclose(fh); // Cierre el archivo.
    return true; // Indica que todo funcionó correctamente.
}

// Programadores C suelen limpiar esto un poco usando goto:
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // Cierre el archivo.
    return true; // Indica que todo funcionó correctamente.

failure:
    fclose(fh);
    return false; // Propagate el error
}

// Si las funciones indican errores mediante excepciones,
// Las cosas son un poco más claras, pero pueden optimizarse mas.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Abrir el archivo en modo lectura
    if (fh == nullptr)
        throw std::runtime_error("No puede abrirse el archivo.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) {
        fclose(fh); // Asegúrese de cerrar el archivo si se produce un error.
        throw; // Luego vuelve a lanzar la excepción.
    }

    fclose(fh); // Cierra el archivo
}

// Compare esto con el uso de la clase de flujo de archivos de C++ (fstream)
// fstream utiliza su destructor para cerrar el archivo.
// Los destructores son llamados automáticamente
// cuando un objeto queda fuera del ámbito.
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream es la abreviatura de el input file stream
    std::ifstream fh(filename); // Abre el archivo

    // Hacer algo con el archivo
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // El archivo se cierra automáticamente aquí por el destructor


// Esto tiene ventajas _enormes_:
// 1. No importa lo que pase,
//    El recurso (en este caso el manejador de archivo) será limpiado.
//    Una vez que escribes el destructor correctamente,
//    Es _imposible_ olvidar cerrar el identificador y permitir
//    fugas del recurso.
// 2. Tenga en cuenta que el código es mucho más limpio.
//    El destructor se encarga de cerrar el archivo detrás de cámaras
//    Sin que tenga que preocuparse por ello.
// 3. El código es seguro.
//    Una excepción puede ser lanzado en cualquier lugar de la función
//    y la limpieza ocurrirá.

// Todo el código idiomático C++ utiliza RAII ampliamente para todos los
// recursos.
// Otros ejemplos incluyen
// - Memoria usando unique_ptr y shared_ptr
// - Contenedores (Containers) - la biblioteca estándar linked list,
//   vector (es decir, array con auto-cambio de tamaño), hash maps, etc.
//   Destruimos todos sus contenidos de forma automática
//   cuando quedan fuera del ámbito.
// - Mutex utilizando lock_guard y unique_lock


/////////////////////
// Cosas divertidas
/////////////////////

// Aspectos de C ++ que pueden sorprender a los recién llegados
// (e incluso algunos veteranos).
// Esta sección es, por desgracia, salvajemente incompleta;
// C++ es uno de los lenguajes con los que mas facil te disparas en el pie.

// Tu puedes sobreescribir métodos privados!
class Foo {
  virtual void bar();
};
class FooSub : public Foo {
  virtual void bar();  // Sobreescribe Foo::bar!
};


// 0 == false == NULL (La mayoria de las veces)!
bool* pt = new bool;
*pt = 0; // Establece los puntos de valor de 'pt' en falso.
pt = 0;  // Establece 'pt' al apuntador nulo. Ambas lineas compilan sin error.

// nullptr se supone que arregla un poco de ese tema:
int* pt2 = new int;
*pt2 = nullptr; // No compila
pt2 = nullptr;  // Establece pt2 como null.

// Hay una excepción para los valores bool.
// Esto es para permitir poner a prueba punteros nulos con if (!ptr),
// pero como consecuencia se puede asignar nullptr a un bool directamente!
*pt = nullptr;  // Esto todavía compila, a pesar de que '*pt' es un bool!

// '=' != '=' != '='!
// Llama Foo::Foo(const Foo&) o alguna variante (mira movimientos semanticos)
// copia del constructor.
Foo f2;
Foo f1 = f2;

// Llama Foo::Foo(const Foo&) o variante, pero solo copia el 'Foo' parte de
// 'fooSub'. Cualquier miembro extra de 'fooSub' se descarta. Este
// comportamiento horrible se llama "Corte de objetos."
FooSub fooSub;
Foo f1 = fooSub;

// Llama a Foo::operator=(Foo&) o variantes.
Foo f1;
f1 = f2;


// Cómo borrar realmente un contenedor:
class Foo { ... };
vector<Foo> v;
for (int i = 0; i < 10; ++i)
  v.push_back(Foo());
// La siguiente línea establece el tamaño de v en 0,
// pero los destructores no son llamados y los recursos no se liberan!

v.empty();
v.push_back(Foo());  // Nuevo valor se copia en el primer Foo que insertamos

// En verdad destruye todos los valores en v.
// Consulta la sección acerca de los objetos temporales para la
// explicación de por qué esto funciona.
v.swap(vector<Foo>());

```
Otras lecturas:

Una referencia del lenguaje hasta a la fecha se puede encontrar en
<http://cppreference.com/w/cpp>

Recursos adicionales se pueden encontrar en <http://cplusplus.com>
