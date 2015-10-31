---
language: java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Jakukyo Friel", "http://weakish.github.io"]
    - ["Madison Dickson", "http://github.com/mix3d"]
    - ["Simon Morgan", "http://sjm.io/"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["Cameron Schermerhorn", "http://github.com/cschermerhorn"]
    - ["Rachel Stiyer", "https://github.com/rstiyer"]
translators:
	- ["Oleksandr Tatarchuk", "http://github.com/tatarchuk"]
filename: LearnJava.java
lang: uk-ua
---

Java є об'єктно-орієнтованою мовою програмування загального призначення з підтримкою паралельного програмування, яка базується на класах.
[Read more here.](http://docs.oracle.com/javase/tutorial/java/)

```java
// Однорядковий коментар починається з //
/*
Багаторядковий коментар виглядає так.
*/
/**
JavaDoc коментар виглядає так. Використовується для опису класу та членів класу.
*/

// Імпорт класу ArrayList з пакету java.util
import java.util.ArrayList;
// Імпорт усіх класів з пакету java.security 
import java.security.*;

// Кожний .java файл містить один зовнішній публічний клас, ім'я якого співпадає
// з і менем файлу.
public class LearnJava {

    // Для запуску програма, написана на java, повинна мати точку входу у вигляді методу main.
    public static void main (String[] args) {

        // Використання System.out.println() для виводу на друк рядків.
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // Для друку з нового рядкка використовується System.out.print().
        System.out.print("Hello ");
        System.out.print("World");

        // Використання System.out.printf() для простого форматованого виводу на друк.
        System.out.printf("pi = %.5f", Math.PI); // => pi = 3.14159

        ///////////////////////////////////////
        // Змінні
        ///////////////////////////////////////

        /*
        *  Оголошення змінних
        */
        // Для оголошення змінних використовується формат <тип> <змінна>
        int fooInt;
        // Оголошення декількох змінних одного типу <тип> <ім'я1>, <ім'я2>, <ім'я3>
        int fooInt1, fooInt2, fooInt3;

        /*
        *  Ініціалізація змінних
        */

        // Ініціалізація змінної з використанням формату <тип> <ім'я> = <значення>
        int fooInt = 1;
        // Ініціалізація декількох змінних одного типу з одним значенням <тип> <ім'я1>, <ім'я2>, <ім'я3> = <значення>
        int fooInt1, fooInt2, fooInt3;
        fooInt1 = fooInt2 = fooInt3 = 1;

        /*
        *  Типи змінних
        */
        // Байт - 8-бітне ціле число зі знаком
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Short - 16-бітне ціле число зі знаком
        // (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - 32-бітне ціле число зі знаком
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int fooInt = 1;

        // Long - 64-бітне ціле число зі знаком
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L використовується для позначення того, що число має тип Long;
        // інакше число буде трактуватись як integer.

        // Примітка: Java не має беззнакових типів.

        // Float - 32-бітне число з плаваючою комою одиничної точності за стандартом IEEE 754 
        // 2^-149 <= float <= (2-2^-23) * 2^127
        float fooFloat = 234.5f;
        // f or F використовується для позначення того, що змінна має тип float;
        // інакше трактується як double.

        // Double - 64-бітне число з плаваючою комою подвійної точності за стандартом IEEE 754 
        // 2^-1074 <= x <= (2-2^-52) * 2^1023
        double fooDouble = 123.4;

        // Boolean - true & false (істина чи неправда)
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - 16-бітний символ Unicode
        char fooChar = 'A';

        // final - посилання на такі змінні не можуть бути присвоєні іншим об'єктам,
        final int HOURS_I_WORK_PER_WEEK = 9001;
        // але вони можуть мати відкладену ініціалізацію.
        final double E;
        E = 2.71828;


        // BigInteger -Незмінні знакові цілі числа довільної точності
        //
        // BigInteger є типом даних, який дає можливість розробнику виконувати операції з
        // з цілими числами, розрядність яких більша за 64 біти. Числа зберігаються у масиві
        // байтів, операції над ними виконуються функціями, які має клас BigInteger
        //
        // BigInteger можна ініціалізувати, використовуючи масив байтів чи рядок.
        
        BigInteger fooBigInteger = new BigInteger(fooByteArray);


        // BigDecimal - Незмінні знакові дробові числа довільної точності
        //
        // BigDecimal складається з двох частин: цілого числа довільної точності 
        // з немасштабованим значенням та 32-бітного масштабованого цілого числа
        //
        // BigDecimal дозволяє розробника контролювати десяткове округлення.
        // Рекомндовано використовувати BigDecimal зі значеннями валют
        // і там, де необхідна точність дробових обчислень.
        //
        // BigDecimal може бути ініціалізований типами даних int, long, double or String
        // чи немасштабованим значенням (BigInteger) і масштабованим значенням (int).

        BigDecimal fooBigDecimal = new BigDecimal(fooBigInteger, fooInt);
        
        // Для дотримання заданої точності рекомендується використовувати
        // конструктор, який приймає String 
        
        BigDecimal tenCents = new BigDecimal("0.1");


        // Рядки
        String fooString = "Це мій рядок!";

        // \n символ переходу на новий рядок
        String barString = "Друк з нового рялка?\nНема питань!";
        // \t символ табуляції
        String bazString = "Хочете додати табуляцію?\tТримайте!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Масиви
        // Розмір масиву має бути визначений перед ініціалізацією
        // Наведений формат ілюструє ініціалізацію масивів
        // <тип даних>[] <ім'я змінної> = new <тип даних>[<розмір масиву>];
        // <тип даних> <ім'я змінної>[] = new <тип даних>[<розмір масиву>];
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];

        // Інший шлях оголошення та ініціалізації масиву
        int[] y = {9000, 1000, 1337};
        String names[] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = new boolean[] {true, false, false};

        // Індексація масиву - доступ за елементами
        System.out.println("intArray @ 0: " + intArray[0]);

        // Масиви є змінними та мають нульовий елемент.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Додатково
        // ArrayLists - Схожі на масив, але мають більший функціонал та змінний розмір.
        // LinkedLists - Реалізація двозв'язного списку. Всі операції
        //               виконуються так, як очікується від
        //               двозв'язного списку.
        // Maps - Множина об'єктів, які пов'язують ключ зі значенням. Map є
        //        інтерфейсом, тому не може бути успадкований.
        //        Типи ключів і значень, які зберігаються в Map мають
        //        вказуватись у класі, який його реалізує.
		//		  Ключ не може повторюватись і пов'язаний лише з одним значенням        
        // HashMaps - Цей клас використовує хеш-таблицю для реалізації інтерфейсу Map.
        //            Це дозволяє виконувати певні операції,
        //            такі як отримання та вставка елемента,
        //            за сталий час для будь-якої кількості значень.

        ///////////////////////////////////////
        // Оператори
        ///////////////////////////////////////
        System.out.println("\n->Operators");

        int i1 = 1, i2 = 2; // Коротка форма присвоєння

        // Арифметичні операції виконуються
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (int/int повертається як int)
        System.out.println("1/2 = " + (i1 / (double)i2)); // => 0.5

        // Ділення з остачею
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Оператори порівняння
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Логічні оператори
        System.out.println("3 > 2 && 2 > 3? " + ((3 > 2) && (2 > 3))); // => false
        System.out.println("3 > 2 || 2 > 3? " + ((3 > 2) || (2 > 3))); // => true
        System.out.println("!(3 == 2)? " + (!(3 == 2))); // => true

        // Бітові оператори!
        /*
        ~      Унарне бітове доповнення
        <<     Знаковий зсув уліво
        >>     Знаковий/Арифметичний зсув управо
        >>>    Беззнаковий/Логічний зсув управо
        &      Бітове І
        ^      Бітови виключне АБО
        |      Бітове АБО
        */

        // Інкремнт
        int i = 0;
        System.out.println("\n->Інкремент/Декремент");
        // Оператори ++ і -- здійснюють інкремент та декремент ретроспективно.
        // Якщо вони розташовані перед змінною, операція виконається перед поверненням;
        // після - повернеться інкремент або декремент.
        System.out.println(i++); // i = 1, prints 0 (post-increment)
        System.out.println(++i); // i = 2, prints 2 (pre-increment)
        System.out.println(i--); // i = 1, prints 2 (post-decrement)
        System.out.println(--i); // i = 0, prints 0 (pre-decrement)

        ///////////////////////////////////////
        // Control Structures
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // If statements are c-like
        int j = 10;
        if (j == 10) {
            System.out.println("I get printed");
        } else if (j > 10) {
            System.out.println("I don't");
        } else {
            System.out.println("I also don't");
        }

        // While loop
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Increment the counter
            // Iterated 100 times, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // Do While Loop
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Increment the counter
            // Iterated 99 times, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // For Loop
        // for loop structure => for(<start_statement>; <conditional>; <step>)
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // Iterated 10 times, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);
        
        // Nested For Loop Exit with Label
        outer:
        for (int i = 0; i < 10; i++) {
          for (int j = 0; j < 10; j++) {
            if (i == 5 && j ==5) {
              break outer;
              // breaks out of outer loop instead of only the inner one
            }
          }
        }
        
        // For Each Loop
        // The for loop is also able to iterate over arrays as well as objects
        // that implement the Iterable interface.
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // for each loop structure => for (<object> : <iterable>)
        // reads as: for each element in the iterable
        // note: the object type must match the element type of the iterable.

        for (int bar : fooList) {
            System.out.println(bar);
            //Iterates 9 times and prints 1-9 on new lines
        }

        // Switch Case
        // A switch works with the byte, short, char, and int data types.
        // It also works with enumerated types (discussed in Enum Types), the
        // String class, and a few special classes that wrap primitive types:
        // Character, Byte, Short, and Integer.
        int month = 3;
        String monthString;
        switch (month) {
            case 1: monthString = "January";
                    break;
            case 2: monthString = "February";
                    break;
            case 3: monthString = "March";
                    break;
            default: monthString = "Some other month";
                     break;
        }
        System.out.println("Switch Case Result: " + monthString);
        
        // Starting in Java 7 and above, switching Strings works like this:
        String myAnswer = "maybe";
        switch(myAnswer) {
            case "yes":
                System.out.println("You answered yes.");
                break;
            case "no":
                System.out.println("You answered no.");
                break;
            case "maybe":
                System.out.println("You answered maybe.");
                break;
            default:
                System.out.println("You answered " + myAnswer);
                break;
        }

        // Conditional Shorthand
        // You can use the '?' operator for quick assignments or logic forks.
        // Reads as "If (statement) is true, use <first value>, otherwise, use
        // <second value>"
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Prints A, because the statement is true


        ////////////////////////////////////////
        // Converting Data Types And Typecasting
        ////////////////////////////////////////

        // Converting data

        // Convert String To Integer
        Integer.parseInt("123");//returns an integer version of "123"

        // Convert Integer To String
        Integer.toString(123);//returns a string version of 123

        // For other conversions check out the following classes:
        // Double
        // Long
        // String

        // Typecasting
        // You can also cast Java objects, there's a lot of details and deals
        // with some more intermediate concepts. Feel free to check it out here:
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Classes And Functions
        ///////////////////////////////////////

        System.out.println("\n->Classes & Functions");

        // (definition of the Bicycle class follows)

        // Use new to instantiate a class
        Bicycle trek = new Bicycle();

        // Call object methods
        trek.speedUp(3); // You should always use setter and getter methods
        trek.setCadence(100);

        // toString returns this Object's string representation.
        System.out.println("trek info: " + trek.toString());

        // Double Brace Initialization
        // The Java Language has no syntax for how to create static Collections
        // in an easy way. Usually you end up in the following way:

        private static final Set<String> COUNTRIES = new HashSet<String>();
        static {
           validCodes.add("DENMARK");
           validCodes.add("SWEDEN");
           validCodes.add("FINLAND");
        }

        // But there's a nifty way to achieve the same thing in an
        // easier way, by using something that is called Double Brace
        // Initialization.

        private static final Set<String> COUNTRIES = new HashSet<String>() {{
            add("DENMARK");
            add("SWEDEN");
            add("FINLAND");
        }}

        // The first brace is creating a new AnonymousInnerClass and the
        // second one declares an instance initializer block. This block
        // is called when the anonymous inner class is created.
        // This does not only work for Collections, it works for all
        // non-final classes.

    } // End main method
} // End LearnJava class


// You can include other, non-public outer-level classes in a .java file,
// but it is good practice. Instead split classes into separate files.


// Class Declaration Syntax:
// <public/private/protected> class <class name> {
//    // data fields, constructors, functions all inside.
//    // functions are called as methods in Java.
// }

class Bicycle {

    // Bicycle's Fields/Variables
    public int cadence; // Public: Can be accessed from anywhere
    private int speed;  // Private: Only accessible from within the class
    protected int gear; // Protected: Accessible from the class and subclasses
    String name; // default: Only accessible from within this package

    static String className; // Static class variable

    // Static block 
    // Java has no implementation of static constructors, but
    // has a static block that can be used to initialize class variables 
    // (static variables). 
    // This block will be called when the class is loaded.
    static {
        className = "Bicycle";
    }

    // Constructors are a way of creating classes
    // This is a constructor
    public Bicycle() {
        // You can also call another constructor:
        // this(1, 50, 5, "Bontrager");
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }

    // This is a constructor that takes arguments
    public Bicycle(int startCadence, int startSpeed, int startGear,
        String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // Method Syntax:
    // <public/private/protected> <return type> <function name>(<args>)

    // Java classes often implement getters and setters for their fields

    // Method declaration syntax:
    // <access modifier> <return type> <method name>(<args>)
    public int getCadence() {
        return cadence;
    }

    // void methods require no return statement
    public void setCadence(int newValue) {
        cadence = newValue;
    }

    public void setGear(int newValue) {
        gear = newValue;
    }

    public void speedUp(int increment) {
        speed += increment;
    }

    public void slowDown(int decrement) {
        speed -= decrement;
    }

    public void setName(String newName) {
        name = newName;
    }

    public String getName() {
        return name;
    }

    //Method to display the attribute values of this Object.
    @Override // Inherited from the Object class.
    public String toString() {
        return "gear: " + gear + " cadence: " + cadence + " speed: " + speed +
            " name: " + name;
    }
} // end class Bicycle

// PennyFarthing is a subclass of Bicycle
class PennyFarthing extends Bicycle {
    // (Penny Farthings are those bicycles with the big front wheel.
    // They have no gears.)

    public PennyFarthing(int startCadence, int startSpeed){
        // Call the parent constructor with super
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // You should mark a method you're overriding with an @annotation.
    // To learn more about what annotations are and their purpose check this
    // out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
    }
}

// Interfaces
// Interface declaration syntax
// <access-level> interface <interface-name> extends <super-interfaces> {
//     // Constants
//     // Method declarations
// }

// Example - Food:
public interface Edible {
    public void eat(); // Any class that implements this interface, must
                       // implement this method.
}

public interface Digestible {
    public void digest();
}


// We can now create a class that implements both of these interfaces.
public class Fruit implements Edible, Digestible {
  
    @Override
    public void eat() {
        // ...
    }

    @Override
    public void digest() {
        // ...
    }
}

// In Java, you can extend only one class, but you can implement many
// interfaces. For example:
public class ExampleClass extends ExampleClassParent implements InterfaceOne,
    InterfaceTwo {

    @Override
    public void InterfaceOneMethod() {
    }

    @Override
    public void InterfaceTwoMethod() {
    }

}

// Abstract Classes

// Abstract Class declaration syntax
// <access-level> abstract <abstract-class-name> extends <super-abstract-classes> {
//     // Constants and variables
//     // Method declarations
// }

// Marking a class as abstract means that it contains abstract methods that must
// be defined in a child class. Similar to interfaces, abstract classes cannot
// be instantiated, but instead must be extended and the abstract methods
// defined. Different from interfaces, abstract classes can contain a mixture of
// concrete and abstract methods. Methods in an interface cannot have a body,
// unless the method is static, and variables are final by default, unlike an
// abstract class. Also abstract classes CAN have the "main" method.

public abstract class Animal
{
    public abstract void makeSound();

    // Method can have a body
    public void eat()
    {
        System.out.println("I am an animal and I am Eating.");  
        // Note: We can access private variable here.
        age = 30;
    }

    // No need to initialize, however in an interface
    // a variable is implicitly final and hence has
    // to be initialized.
    protected int age;

    public void printAge()
    {
        System.out.println(age);  
    }

    // Abstract classes can have main function.
    public static void main(String[] args)
    {
        System.out.println("I am abstract");
    }
}

class Dog extends Animal
{
    // Note still have to override the abstract methods in the
    // abstract class.
    @Override
    public void makeSound()
    {
        System.out.println("Bark");
        // age = 30;	==> ERROR!	age is private to Animal
    }

    // NOTE: You will get an error if you used the
    // @Override annotation here, since java doesn't allow
    // overriding of static methods.
    // What is happening here is called METHOD HIDING.
    // Check out this awesome SO post: http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Dog pluto = new Dog();
        pluto.makeSound();
        pluto.eat();
        pluto.printAge();
    }
}

// Final Classes

// Final Class declaration syntax
// <access-level> final <final-class-name> {
//     // Constants and variables
//     // Method declarations
// }

// Final classes are classes that cannot be inherited from and are therefore a
// final child. In a way, final classes are the opposite of abstract classes
// because abstract classes must be extended, but final classes cannot be
// extended.
public final class SaberToothedCat extends Animal
{
    // Note still have to override the abstract methods in the
    // abstract class.
    @Override
    public void makeSound()
    {
        System.out.println("Roar");
    }
}

// Final Methods
public abstract class Mammal()
{
    // Final Method Syntax:
    // <access modifier> final <return type> <function name>(<args>)

    // Final methods, like, final classes cannot be overridden by a child class,
    // and are therefore the final implementation of the method.
    public final boolean isWarmBlooded()
    {
        return true;
    }
}


// Enum Type
//
// An enum type is a special data type that enables for a variable to be a set
// of predefined constants. The variable must be equal to one of the values that
// have been predefined for it. Because they are constants, the names of an enum
// type's fields are in uppercase letters. In the Java programming language, you
// define an enum type by using the enum keyword. For example, you would specify
// a days-of-the-week enum type as:

public enum Day {
    SUNDAY, MONDAY, TUESDAY, WEDNESDAY,
    THURSDAY, FRIDAY, SATURDAY 
}

// We can use our enum Day like that:

public class EnumTest {
    
    // Variable Enum
    Day day;
    
    public EnumTest(Day day) {
        this.day = day;
    }
    
    public void tellItLikeItIs() {
        switch (day) {
            case MONDAY:
                System.out.println("Mondays are bad.");
                break;
                    
            case FRIDAY:
                System.out.println("Fridays are better.");
                break;
                         
            case SATURDAY: 
            case SUNDAY:
                System.out.println("Weekends are best.");
                break;
                        
            default:
                System.out.println("Midweek days are so-so.");
                break;
        }
    }
    
    public static void main(String[] args) {
        EnumTest firstDay = new EnumTest(Day.MONDAY);
        firstDay.tellItLikeItIs(); // => Mondays are bad.
        EnumTest thirdDay = new EnumTest(Day.WEDNESDAY);
        thirdDay.tellItLikeItIs(); // => Midweek days are so-so.
    }
}

// Enum types are much more powerful than we show above. 
// The enum body can include methods and other fields.
// You can se more at https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

```

## Further Reading

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.

**Official Oracle Guides**:

* [Java Tutorial Trail from Sun / Oracle](http://docs.oracle.com/javase/tutorial/index.html)

* [Java Access level modifiers](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Object-Oriented Programming Concepts](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](http://www.oracle.com/technetwork/java/codeconvtoc-136057.html)

**Online Practice and Tutorials**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Books**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](http://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](http://www.amazon.com/gp/product/0071606300)
