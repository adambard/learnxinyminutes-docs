---
language: java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["Camilo Garrido", "http://www.twitter.com/hirohope"]
lang: es-es
filename: LearnJava-es.java
---

Java es un lenguage de programación de propósito general, concurrente, basado en clases y
orientado a objetos.
[Lee más aquí.](http://docs.oracle.com/javase/tutorial/java/index.html)

```java
// Comentarios de una sóla línea comienzan con //
/*
Comentarios multilínea lucen así
*/
/**
Comentarios JavaDoc lucen así. Suelen describir la clase o varios atributos
de una clase.
*/

// Importa la clase ArrayList dentro del paquete java.util
import java.util.ArrayList;
// Importa todas las clases dentro del paquete java.security
import java.security.*;

// Cada archivo .java contiene una clase pública, con el mismo nombre del archivo.
public class AprendeJava {

    // Un programa debe tener un método 'main' como punto de entrada
    public static void main (String[] args) {

        // Usa System.out.println para imprimir líneas
        System.out.println("¡Hola mundo!");
        System.out.println(
            "Entero (int): " + 10 +
            " Doble (double): " + 3.14 +
            " Booleano (boolean): " + true);

        // Para imprimir sin el salto de línea, usa System.out.print
        System.out.print("Hola ");
        System.out.print("Mundo");


        ///////////////////////////////////////
        // Tipos & Variables
        ///////////////////////////////////////

        // Declara una variable usando <tipo> <nombre> [
        // Byte - Entero complemento a dos con signo de 8-bit
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - Entero complemento a dos con signo de 16-bit
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - Entero complemento a dos con signo de 32-bit
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - Entero complemento a dos con signo de 64-bit
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L es usado para denotar que el valor de esta variable es del tipo Long;
        // cualquier cosa sin ella es tratado como un entero por defecto.

        // Nota: Java no tiene tipos sin signo

        // Float - Número de coma flotante IEEE 754 de precisión simple de 32-bit
        float fooFloat = 234.5f;
        // f es usado para denotar qeu el valor de esta variable es del tipo float;
        // de otra manera es tratado como un double.

        // Double - Número de coma flotante IEEE 754 de precisión doble de 64-bit
        double fooDouble = 123.4;

        // Boolean - true & false
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - Un simple carácter unicode de 16-bit
        char fooChar = 'A';

        // Usa 'final' para hacer inmutable las variables
        final int HORAS_QUE_TRABAJO_POR_SEMANA = 9001;

        // Strings
        String fooString = "¡Mi String está aquí!";

        // \n es un carácter escapado que inicia una nueva línea
        String barString = "¿Imprimiendo en una nueva linea?\n¡Ningun problema!";
        // \t es un carácter escapado que añade un carácter tab
        String bazString = "¿Quieres añadir un 'tab'?\t¡Ningun problema!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Arreglos
        //El tamaño del arreglo debe decidirse en la declaración
        //El formato para la declaración de un arreglo es la siguiente:
        //<tipo_de_dato> [] <nombre_variable> = new <tipo_de_dato>[<tamaño_arreglo>];
        int [] arreglo_de_enteros = new int[10];
        String [] arreglo_de_strings = new String[1];
        boolean [] arreglo_de_booleanos = new boolean[100];

        // Otra forma de declarar & inicializar un arreglo
        int [] y = {9000, 1000, 1337};

        // Indexación de un arreglo - Accediendo un elemento
        System.out.println("arreglo_de_enteros @ 0: " + arreglo_de_enteros[0]);

        // Arreglos comienzan su indexación en cero y son mutables
        arreglo_de_enteros[1] = 1;
        System.out.println("arreglo_de_enteros @ 1: " + arreglo_de_enteros[1]); // => 1

        // Otros para echar un vistazo
        // ArrayLists - Son como arreglos excepto que ofrecen más funcionalidades
        //              y el tamaño es mutable
        // LinkedLists
        // Maps
        // HashMaps

        ///////////////////////////////////////
        // Operadores
        ///////////////////////////////////////
        System.out.println("\n->Operadores");

        int i1 = 1, i2 = 2; // Abreviación para múltiples declaraciones

        // La aritmética es directa
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (0.5 truncado)

        // Módulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Operadores de comparación
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // ¡Operaciones a nivel de bits!
        /*
        ~       Complemento unario bit a bit
        <<      Deplazamiento hacia la izquierda con signo
        >>      Deplazamiento hacia la derecha con signo
        >>>     Deplazamiento hacia la derecha sin signo
        &       AND lógico
        ^       OR lógico exclusivo
        |       OR lógico inclusivo
        */

        // Incrementos
        int i = 0;
        System.out.println("\n->Incrementos y reducciones");
        System.out.println(i++); //i = 1. Post-incremento
        System.out.println(++i); //i = 2. Pre-incremento
        System.out.println(i--); //i = 1. Post-reducción
        System.out.println(--i); //i = 0. Pre-reducción

        ///////////////////////////////////////
        // Estructuras de Control
        ///////////////////////////////////////
        System.out.println("\n->Estructuras de Control");

        // Condiciones 'if' son como en c
        int j = 10;
        if (j == 10){
            System.out.println("Me imprimieron");
        } else if (j > 10) {
            System.out.println("A mi no");
        } else {
            System.out.println("A mi tampoco");
        }

        // Ciclos 'while'
        int fooWhile = 0;
        while(fooWhile < 100)
        {
            //System.out.println(fooWhile);
            //Incrementar el contador
            //Iteró 99 veces, fooWhile 0->99
            fooWhile++;
        }
        System.out.println("Valor fooWhile: " + fooWhile);

        // Ciclos 'do while'
        int fooDoWhile = 0;
        do
        {
            //System.out.println(fooDoWhile);
            //Incrementar el contador
            //Iteró 99 veces, fooDoWhile 0->99
            fooDoWhile++;
        }while(fooDoWhile < 100);
        System.out.println("Valor fooDoWhile: " + fooDoWhile);

        // Ciclos 'for'
        int fooFor;
        //Estructura del ciclo 'for' => for(<declaración_de_inicio>; <condicional>; <paso>)
        for(fooFor=0; fooFor<10; fooFor++){
            //System.out.println(fooFor);
            //Iteró 10 veces, fooFor 0->9
        }
        System.out.println("Valor fooFor: " + fooFor);

        // Switch Case
        // Un 'switch' funciona con un tipo de dato byte, short, char e int
        // También funciona con tipos enumerados (discutido en tipos Enum),
        // la clase String y unas pocas clases especiales que envuelven
        // tipos primitivos: Character, Byte, Short e Integer.
        int mes = 3;
        String mesString;
        switch (mes){
            case 1:
                    mesString = "Enero";
                    break;
            case 2:
                    mesString = "Febrero";
                    break;
            case 3:
                    mesString = "Marzo";
                    break;
            default:
                    mesString = "Algun otro mes";
                    break;
        }
        System.out.println("Resultado switch Case: " + mesString);


        ///////////////////////////////////////
        // Convirtiendo Tipos de Datos y Conversión de Tipos
        ///////////////////////////////////////

        // Convirtiendo datos

        // Convertir String a Integer
        Integer.parseInt("123");//retorna una versión entera de "123"

        // Convertir Integer a String
        Integer.toString(123);//retorna una versión string de 123

        // Para otras conversiones fíjate en las siguientes clases
        // Double
        // Long
        // String

        // Conversión de tipos
        // También puedes convertir objetos java, hay muchos detalles
        // con unos pocos conceptos intermedios
        // No dudes en verlos acá
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Clases y Funciones
        ///////////////////////////////////////

        System.out.println("\n->Clases & Funciones");

        // (A continuación la definición de una clase Bicicleta)

        // Usa 'new' para instanciar una clase
        Bicicleta excursion = new Bicicleta();

        // Llama métodos del objeto
        excursion.aumentarVelocidad(3); // Siempre deberías usar metodos 'set' (establecer) y 'get' (obtener)
        excursion.setRitmo(100);

        // 'toString' es una convención para mostrar los valores de este objeto.
        System.out.println("informacion de la excursion: " + excursion.toString());

        ///////////////////////////////////////
        // Genéricos
        ///////////////////////////////////////

        // Utilizando genéricos (a partir de Java 1.5) es posible detectar en tiempo de
        // compilación errores de tipado (en versiones anteriores se detectarían como error
        // de ejecución)

        List<String> v = new ArrayList<String>();
        v.add("test");
        String s = v.get(0); // Si intentamos recuperar s como otro tipo diferente a String
                             // (por ejemplo, un Integer) obtendríamos un error de compilación

    } // Fin del método 'main'
} // Fin de la clase AprendeJava


// Puedes incluir otras clases no públicas en un archivo .java


// Sintaxis de declaración de clases:
// <public/private/protected> class <nombre_de_la_clase>{
//    //variables_de_clase, constructores, todas las funciones.
//    //las funciones son llamadas como métodos en Java.
// }

class Bicicleta {

    // Campos/Variables de Bicicleta
    public int ritmo; // Public: Puede ser accedido desde cualquier parte
    private int velocidad;  // Private: Accesible sólo desde esta clase
    protected int engranaje; // Protected: Accesible desde esta clases y sus subclases
    String nombre; // default: Sólo accesible desde este paquete

    // Constructores son la manera de crear clases
    // Este es un constructor por defecto
    public Bicicleta() {
        engranaje = 1;
        ritmo = 50;
        velocidad = 5;
        nombre = "Bontrager";
    }

    // Este es un constructor específico (contiene argumentos)
    public Bicicleta(int ritmoInicial, int velocidadInicial, int engranajeInicial, String nombre) {
        this.engranaje = engranajeInicial;
        this.ritmo = ritmoInicial;
        this.velocidad = velocidadInicial;
        this.nombre = nombre;
    }

    // Sintaxis de función:
    // <public/private/protected> <tipo_de_retorno> <nombre_funcion>(<argumentos>)

    // Las clases de Java usualmente implementan métodos 'get' (obtener) y 'set' (establecer) para sus campos

    // Sintaxis de declaración de métodos
    // <alcance> <tipo_de_retorno> <nombre_metodo>(<argumentos>)
    public int getRitmo() {
        return ritmo;
    }

    // Métodos void no requieren retornar
    public void setRitmo(int nuevoValor) {
        ritmo = nuevoValor;
    }

    public void setEngranaje(int nuevoValor) {
        engranaje = nuevoValor;
    }

    public void aumentarVelocidad(int incremento) {
        velocidad += incremento;
    }

    public void disminuirVelocidad(int reduccion) {
        velocidad -= reduccion;
    }

    public void setNombre(String nuevoNombre) {
        nombre = nuevoNombre;
    }

    public String getNombre() {
        return nombre;
    }

    //Método para mostrar los valores de los atributos de este objeto.
    @Override
    public String toString() {
        return "engranaje: " + engranaje +
                " ritmo: " + ritmo +
                " velocidad: " + velocidad +
                " nombre: " + nombre;
    }
} // fin clase Bicicleta

// PennyFarthing es una subclase de Bicicleta
class PennyFarthing extends Bicicleta {
    // (Penny Farthings son esas bicicletas con una gran rueda forntal.
    // No tienen engranajes.)

    public PennyFarthing(int ritmoInicial, int velocidadInicial){
        // Llama al constructor del padre con super
        super(ritmoInicial, velocidadInicial, 0, "PennyFarthing");
    }

    // Deberías marcar un método que estás sobre escribiendo con una @anotacion
    // Para aprender más sobre que son y el propósito de las anotaciones
    // echa un vistazo acá: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setEngranaje(int engranaje) {
        engranaje = 0;
    }

}

```

## Más Lectura

Estos links son sólo para tener un entendimiento del tema, no dudes en
usar Google y encontrar ejemplos más específicos

Otros temas a investigar:

* [Java Tutorial Trail from Sun / Oracle](http://docs.oracle.com/javase/tutorial/index.html)

* [Java Access level modifiers](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Object-Oriented Programming Concepts](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](http://www.oracle.com/technetwork/java/codeconv-138413.html)
