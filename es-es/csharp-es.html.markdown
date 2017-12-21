---
language: C#(C Sharp)
filename: LearnCSharp-es.cs
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
translators:
    - ["Olfran Jiménez", "https://twitter.com/neslux"]
lang: es-es

---

C# es un lenguaje orientado a objetos elegante y de tipado seguro que
permite a los desarrolladores construir una variedad de aplicaciones
seguras y robustas que se ejecutan en el Framework .NET.

[Lee más aquí.](http://msdn.microsoft.com/es-es/library/vstudio/z1zx9t92.aspx)

```c#
// Los comentarios de una sola línea comienzan con //
/*
Los comentarios de múltiples líneas son de esta manera
*/
/// <summary>
/// Este es un comentario de documentación XML
/// </summary>

// Especifica el espacio de nombres que estará usando la aplicación
using System;
using System.Collections.Generic;


// Define un ambito para organizar el código en "paquetes"
namespace Learning
{
	// Cada archivo .cs debe contener al menos una clase con el mismo nombre que el archivo
    // Se permite colocar cualquier nombre, pero no deberías por cuestiones de consistencia.
    public class LearnCSharp
    {
		// Una aplicación de consola debe tener un método main como punto de entrada
        public static void Main(string[] args)
        {
			// Usa Console.WriteLine para imprimir líneas
            Console.WriteLine("Hello World");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

			// Para imprimir sin una nueva línea, usa Console.Write
            Console.Write("Hello ");
            Console.Write("World");


            ///////////////////////////////////////////////////
            // Variables y Tipos
            //
            // Declara una variable usando <tipo> <nombre>
            ///////////////////////////////////////////////////

            // Sbyte - Entero de 8 bits con signo
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - Entero de 8 bits sin signo
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - Entero de 16 bits con signo
            // (-32,768 <= short <= 32,767)
            short fooShort = 10000;

            // Ushort - Entero de 16 bits sin signo
            // (0 <= ushort <= 65,535)
            ushort fooUshort = 10000;

            // Integer - Entero de 32 bits con signo
            // (-2,147,483,648 <= int <= 2,147,483,647)
            int fooInt = 1;

            // Uinteger - Entero de 32 bits sin signo
            // (0 <= uint <= 4,294,967,295)
            uint fooUint = 1;

            // Long - Entero de 64 bits con signo
            // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            long fooLong = 100000L;
            // L es usado para indicar que esta variable es de tipo long o ulong
            // un valor sin este sufijo es tratado como int o uint dependiendo del tamaño.

            // Ulong - Entero de 64 bits sin signo
            // (0 <= ulong <= 18,446,744,073,709,551,615)
            ulong fooUlong = 100000L;

            // Float - Precisión simple de 32 bits. IEEE 754 Coma flotante
            // Precisión: 7 dígitos
            float fooFloat = 234.5f;
			// f es usado para indicar que el valor de esta variable es de tipo float
            // de otra manera sería tratado como si fuera de tipo double.

            // Double - Doble precisión de 32 bits. IEEE 754 Coma flotante
            // Precisión: 15-16 dígitos
            double fooDouble = 123.4;

            // Bool - true & false (verdadero y falso)
            bool fooBoolean = true;
            bool barBoolean = false;

			// Char - Un solo caracter Unicode de 16 bits
            char fooChar = 'A';

            // Strings
            string fooString = "My string is here!";
            Console.WriteLine(fooString);

            // Formato de cadenas
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);
            Console.WriteLine(fooFormattedString);

            // Formato de fechas
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

			// \n es un caracter de escape que comienza una nueva línea
            string barString = "Printing on a new line?\nNo Problem!";
            Console.WriteLine(barString);

            // Puede ser escrito mejor usando el símbolo @
            string bazString = @"Here's some stuff
    on a new line!";
            Console.WriteLine(bazString);

            // Las comillas deben ser escapadas
            // usa \" para escaparlas
            string quotedString = "some \"quoted\" stuff";
            Console.WriteLine(quotedString);

			// usa "" cuando las cadenas comiencen con @
            string quotedString2 = @"some MORE ""quoted"" stuff";
            Console.WriteLine(quotedString2);

			// Usa const o readonly para hacer las variables inmutables
			// los valores const son calculados en tiempo de compilación
            const int HOURS_I_WORK_PER_WEEK = 9001;

            // Tipos que aceptan valores NULL (Nullable)
			// cualquier tipo de dato puede ser un tipo nulo añadiendole el sufijo ?
            // <tipo>? <variable> = <valor>
            int? nullable = null;
            Console.WriteLine("Nullable variable: " + nullable);

			// Para usar valores nulos, tienes que usar la propiedad Value
			// o usar conversión explícita
            string? nullableString = "not null";
            Console.WriteLine("Nullable value is: " + nullableString.Value + " or: " + (string) nullableString );

			// ?? is una manera corta de especificar valores por defecto
            // en caso de que la variable sea null
            int notNullable = nullable ?? 0;
            Console.WriteLine("Not nullable variable: " + notNullable);

			// var - el compilador escogerá el tipo de dato más apropiado basado en el valor
            var fooImplicit = true;

            ///////////////////////////////////////////////////
            // Estructura de datos
            ///////////////////////////////////////////////////
            Console.WriteLine("\n->Data Structures");

            // Arreglos
			// El tamaño del arreglo debe decidirse al momento de la declaración
            // El formato para declarar un arreglo es el siguiente:
            // <tipo_de_dato>[] <nombre_variable> = new <tipo_de_dato>[<tamaño>];
            int[] intArray = new int[10];
            string[] stringArray = new string[1];
            bool[] boolArray = new bool[100];

			// Otra forma de declarar e inicializar un arreglo
            int[] y = { 9000, 1000, 1337 };

			// Indexar arreglos - Acceder a un elemento
            Console.WriteLine("intArray @ 0: " + intArray[0]);

			// Los arreglos son de índice cero y son mutables.
            intArray[1] = 1;
            Console.WriteLine("intArray @ 1: " + intArray[1]); // => 1

            // Listas
			// Las listas son usadas más frecuentemente que los arreglos ya que son más flexibles
			// El formato para declarar una lista es el siguiente:
            // List<tipo_de_dato> <nombre_variable> = new List<tipo_de_dato>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();

            // Otra forma de declarar e inicializar una lista
            List<int> z = new List<int> { 9000, 1000, 1337 };

            // Indexar una lista - Acceder a un elemento
            // Las listas son de índice cero y son mutables.
            Console.WriteLine("z @ 0: " + z[2]);

            // Las listas no tienen valores por defecto;
            // Un valor debe ser añadido antes de acceder al índice
            intList.Add(1);
            Console.WriteLine("intList @ 0: " + intList[0]);


            // Otras estructuras de datos a chequear:
            //
            // Pilas/Colas
            // Diccionarios
            // Colecciones de sólo lectura
            // Tuplas (.Net 4+)


            ///////////////////////////////////////
            // Operadores
            ///////////////////////////////////////
            Console.WriteLine("\n->Operators");

            int i1 = 1, i2 = 2; // Modo corto para múltiples declaraciones

            // La aritmética es sencilla
            Console.WriteLine("1+2 = " + (i1 + i2)); // => 3
            Console.WriteLine("2-1 = " + (i2 - i1)); // => 1
            Console.WriteLine("2*1 = " + (i2 * i1)); // => 2
            Console.WriteLine("1/2 = " + (i1 / i2)); // => 0 (0.5 truncated down)

            // Módulo
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // Operadores de comparación
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // Operadores a nivel de bits
            /*
            ~       Complemento a nivel de bits
            <<      Desplazamiento a la izquierda con signo
            >>      Desplazamiento a la derecha con signo
            >>>     Desplazamiento a la derecha sin signo
            &       AND a nivel de bits
            ^       XOR a nivel de bits
            |       OR a nivel de bits
            */

            // Incremento
            int i = 0;
            Console.WriteLine("\n->Inc/Dec-remento");
            Console.WriteLine(i++); //i = 1. Posincrementación
            Console.WriteLine(++i); //i = 2. Preincremento
            Console.WriteLine(i--); //i = 1. Posdecremento
            Console.WriteLine(--i); //i = 0. Predecremento


            ///////////////////////////////////////
            // Estructuras de control
            ///////////////////////////////////////
            Console.WriteLine("\n->Control Structures");

            // Las condiciones if son como en lenguaje c
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("I get printed");
            }
            else if (j > 10)
            {
                Console.WriteLine("I don't");
            }
            else
            {
                Console.WriteLine("I also don't");
            }

            // Operador ternario
			// Un simple if/else puede ser escrito de la siguiente manera;
            // <condición> ? <true> : <false>
            string isTrue = (true) ? "True" : "False";
            Console.WriteLine("Ternary demo: " + isTrue);


            // Bucle while
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                //Console.WriteLine(fooWhile);
                //Incrementar el contador
                //Iterar 99 veces, fooWhile 0->99
                fooWhile++;
            }
            Console.WriteLine("fooWhile Value: " + fooWhile);

            // Bucle Do While
            int fooDoWhile = 0;
            do
            {
                //Console.WriteLine(fooDoWhile);
                //Incrementar el contador
                //Iterar 99 veces, fooDoWhile 0->99
                fooDoWhile++;
            } while (fooDoWhile < 100);
            Console.WriteLine("fooDoWhile Value: " + fooDoWhile);

            // Bucle For
            int fooFor;
            //Estructura del bucle for => for(<declaración_inicial>; <condición>; <incremento>)
            for (fooFor = 0; fooFor < 10; fooFor++)
            {
                //Console.WriteLine(fooFor);
                //Iterated 10 times, fooFor 0->9
            }
            Console.WriteLine("fooFor Value: " + fooFor);

            // Switch Case
			// El switch funciona con los tipos de datos byte, short, char e int
            // También funciona con las enumeraciones (discutidos en in Tipos Enum),
            // la clase string y algunas clases especiales que encapsulan
            // tipos primitivos: Character, Byte, Short, Integer.
            int month = 3;
            string monthString;
            switch (month)
            {
                case 1:
                    monthString = "January";
                    break;
                case 2:
                    monthString = "February";
                    break;
                case 3:
                    monthString = "March";
                    break;
                default:
                    monthString = "Some other month";
                    break;
            }
            Console.WriteLine("Switch Case Result: " + monthString);


            ////////////////////////////////
            // Conversión de tipos de datos
            ////////////////////////////////

            // Convertir datos

            // Convertir String a Integer
            // esto generará una excepción al fallar la conversión
            int.Parse("123");//retorna una versión entera de "123"

            // TryParse establece la variable a un tipo por defecto
            // en este caso: 0
            int tryInt;
            int.TryParse("123", out tryInt);

            // Convertir Integer a String
            // La clase Convert tiene algunos métodos para facilitar las conversiones
            Convert.ToString(123);

            ///////////////////////////////////////
            // Clases y Funciones
            ///////////////////////////////////////

            Console.WriteLine("\n->Classes & Functions");

            // (Definición de la clase Bicycle (Bicicleta))

            // Usar new para instanciar una clase
            Bicycle trek = new Bicycle();

            // Llamar a los métodos del objeto
            trek.speedUp(3); // Siempre deberías usar métodos setter y métodos getter
            trek.setCadence(100);

            // ToString es una convención para mostrar el valor del objeto.
            Console.WriteLine("trek info: " + trek.ToString());

            // Instanciar otra nueva bicicleta
            Bicycle octo = new Bicycle(5, 10);
            Console.WriteLine("octo info: " + octo.ToString());

            // Instanciar un Penny Farthing (Biciclo)
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.ToString());

            Console.Read();
        } // Fin del método main


    } // Fin de la clase LearnCSharp

    // Puedes incluir otras clases en un archivo .cs


    // Sintaxis para la declaración de clases:
    // <public/private/protected> class <nombre_de_clase>{
    //    //campos, constructores, funciones todo adentro de la clase.
    //    //las funciones son llamadas métodos como en java.
    // }

    public class Bicycle
    {
        // Campos/Variables de la clase Bicycle
        public int cadence; // Public: Accesible desde cualquier lado
        private int _speed;  // Private: Sólo es accesible desde dentro de la clase
        protected int gear; // Protected: Accesible desde clases y subclases
        internal int wheels; // Internal: Accesible en el ensamblado
        string name; // Todo es privado por defecto: Sólo es accesible desde dentro de esta clase

        // Enum es un tipo valor que consiste un una serie de constantes con nombres
        public enum Brand
        {
            AIST,
            BMC,
            Electra,
            Gitane
        }
        // Definimos este tipo dentro de la clase Bicycle, por lo tanto es un tipo anidado
        // El código afuera de esta clase debería referenciar este tipo como Bicycle.Brand

        public Brand brand; // Declaramos un tipo enum, podemos declarar un campo de este tipo

        // Los miembros estáticos pertenecen al tipo mismo, no a un objeto en específico.
        static public int bicyclesCreated = 0;
        // Puedes acceder a ellos sin referenciar ningún objeto:
        // Console.WriteLine("Bicycles created: " + Bicycle.bicyclesCreated);

        // Los valores readonly (Sólo lectura) son establecidos en tiempo de ejecución
        // sólo pueden ser asignados al momento de la declaración o dentro de un constructor
        readonly bool hasCardsInSpokes = false; // privado de sólo lectura

        // Los constructores son una forma de crear clases
        // Este es un constructor por defecto
        private Bicycle()
        {
            gear = 1;
            cadence = 50;
            _speed = 5;
            name = "Bontrager";
            brand = Brand.AIST;
            bicyclesCreated++;
        }

        // Este es un constructor específico (contiene argumentos)
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, Brand brand)
        {
            this.gear = startGear; // La palabra reservada "this" señala el objeto actual
            this.cadence = startCadence;
            this._speed = startSpeed;
            this.name = name; // Puede ser útil cuando hay un conflicto de nombres
            this.hasCardsInSpokes = hasCardsInSpokes;
            this.brand = brand;
        }

        // Los constructores pueden ser encadenados
        public Bicycle(int startCadence, int startSpeed, Brand brand) :
            this(startCadence, startSpeed, 0, "big wheels", true)
        {
        }

        // Sintaxis para Funciones:
        // <public/private/protected> <tipo_retorno> <nombre_funcion>(<args>)

        // Las clases pueden implementar getters y setters para sus campos
        // o pueden implementar propiedades

        // Sintaxis para la declaración de métodos:
        // <ámbito> <tipo_retorno> <nombre_método>(<argumentos>)
        public int GetCadence()
        {
            return cadence;
        }

        // Los métodos void no requieren usar return
        public void SetCadence(int newValue)
        {
            cadence = newValue;
        }

        // La palabra reservada virtual indica que este método puede ser sobrescrito
        public virtual void SetGear(int newValue)
        {
            gear = newValue;
        }

        // Los parámetros de un método pueden tener valores por defecto.
		// En este caso, los métodos pueden ser llamados omitiendo esos parámetros
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // Propiedades y valores get/set
        // Cuando los datos sólo necesitan ser accedidos, considera usar propiedades.
        // Las propiedades pueden tener get, set o ambos
        private bool _hasTassles; // variable privada
        public bool HasTassles // acceso público
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // Las propiedades pueden ser auto implementadas
        public int FrameSize
        {
            get;
            // Puedes especificar modificadores de acceso tanto para get como para set
            // esto significa que sólo dentro de la clase Bicycle se puede modificar Framesize
            private set;
        }

        //Método para mostrar los valores de atributos de este objeto.
        public override string ToString()
        {
            return "gear: " + gear +
                    " cadence: " + cadence +
                    " speed: " + _speed +
                    " name: " + name +
                    " cards in spokes: " + (hasCardsInSpokes ? "yes" : "no") +
                    "\n------------------------------\n"
                    ;
        }

        // Los métodos también pueden ser estáticos. Puede ser útil para métodos de ayuda
        public static bool DidWeCreateEnoughBycles()
        {
            // Dentro de un método esático,
			// Sólo podemos hacer referencia a miembros estáticos de clases
            return bicyclesCreated > 9000;
        }   // Si tu clase sólo necesita miembros estáticos,
		    // considera establecer la clase como static.

    } // fin de la clase Bicycle

    // PennyFarthing es una subclase de Bicycle
    class PennyFarthing : Bicycle
    {
        // (Penny Farthings son las bicicletas con una rueda delantera enorme.
        // No tienen engranajes.)

        // llamar al constructor de la clase padre
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "PennyFarthing", true)
        {
        }

        public override void SetGear(int gear)
        {
            gear = 0;
        }

        public override string ToString()
        {
            string result = "PennyFarthing bicycle ";
            result += base.ToString(); // Llamar a la versión base del método
            return reuslt;
        }
    }

    // Las interfaces sólo contienen las declaraciones
	// de los miembros, sin la implementación.
    interface IJumpable
    {
        void Jump(int meters); // todos los miembros de interfaces son implícitamente públicos
    }

    interface IBreakable
    {
		// Las interfaces pueden contener tanto propiedades como métodos, campos y eventos
        bool Broken { get; }
    }

	// Las clases sólo heredan de alguna otra clase, pero pueden implementar
	// cualquier cantidad de interfaces
    class MountainBike : Bicycle, IJumpable, IBreakable
    {
        int damage = 0;

        public void Jump(int meters)
        {
            damage += meters;
        }

        public void Broken
        {
            get
            {
                return damage > 100;
            }
        }
    }
} // Fin del espacio de nombres

```

## Temas no cubiertos

 * Flags
 * Attributes
 * Generics (T), Delegates, Func, Actions, lambda expressions
 * Static properties
 * Exceptions, Abstraction
 * LINQ
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)



## Lecturas recomendadas

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/es-es/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/tutorials)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)



[Convenciones de código de C#](http://msdn.microsoft.com/es-es/library/vstudio/ff926074.aspx)
