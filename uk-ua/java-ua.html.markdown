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
    - ["Oleksandr Tatarchuk", "https://github.com/tatarchuk"]
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
        // Управляючі конструкції
        ///////////////////////////////////////
        System.out.println("\n->Control Structures");

        // Оператор if використовується так само, як у мові С
        int j = 10;
        if (j == 10) {
            System.out.println("Це надрукується");
        } else if (j > 10) {
            System.out.println("А це - ні");
        } else {
            System.out.println("Це - також ні");
        }

        // Цикл з передумовою While
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Інкремент лічильника
            // Виконається 100 разів, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("fooWhile Value: " + fooWhile);

        // Цикл з післяумовою Do While
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Інкремент лічильника
            // Виконається 99 разів, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("fooDoWhile Value: " + fooDoWhile);

        // Цикл з параметром For
        // структура циклу => for(<початковий стан>; <умова завершення>; <крок>)
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // Виконається 10 разів, fooFor 0->9
        }
        System.out.println("fooFor Value: " + fooFor);
        
        // Вихід з вкладеного циклу через Label
        outer:
        for (int i = 0; i < 10; i++) {
          for (int j = 0; j < 10; j++) {
            if (i == 5 && j ==5) {
              break outer;
              // вихід із зовнішнього циклу, а не лише внутрішнього
            }
          }
        }
        
        // Цикл For Each
        // Призначений для перебору масивів та колекцій       
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};       

        for (int bar : fooList) {
            System.out.println(bar);
            //Iterates 9 times and prints 1-9 on new lines
        }

        // Оператор вибору Switch Case
        // Оператор вибору працює з типами даних byte, short, char, int.
        // Також працює з переліками Enum, 
        // класом String та класами-обгортками примітивних типів:
        // Character, Byte, Short та Integer.
        int month = 3;
        String monthString;
        switch (month) {
            case 1: monthString = "Січень";
                    break;
            case 2: monthString = "Лютий";
                    break;
            case 3: monthString = "Березень";
                    break;
            default: monthString = "Інший місяць";
                     break;
        }
        System.out.println("Switch Case результат: " + monthString);
        
        // Починаючи з Java 7 і далі, вибір рядкових змінних здійснюється так:
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

        // Тернарний оператор вибору
        // Можна використовувати '?' оператор для визначення умови.
        // Читається так "Якщо (умова) вірна, то <перше значення>, інакше
        // <друге значення"
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Надрукується А, бо умова вірна


        ////////////////////////////////////////
        // Перетворення типів
        ////////////////////////////////////////

        // Перетворення String на Integer
        Integer.parseInt("123");//поверне числову версію рядка "123"

        // Перетворення Integer на String
        Integer.toString(123);//повертає рядкову версію 123

        // Для інших перетворень є наступні класи:
        // Double
        // Long
        // String

        // Приведення типів
        // Тут можна прочитати про приведення об'єктів:
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Класи та функції
        ///////////////////////////////////////

        System.out.println("\n->Classes & Functions");

        // (Клас Bicycle наведений нижче)

        // Новий об'єкт класу
        Bicycle trek = new Bicycle();

        // Виклик методу об'єкта
        trek.speedUp(3); // Постійно використовуються методи з назвами set і get
        trek.setCadence(100);

        // toString повертає рядкове представленя об'єкту.
        System.out.println("trek info: " + trek.toString());
        
        // У Java немає синтаксису для явного створення статичних колекцій.
        // Це можна зробити так:

        private static final Set<String> COUNTRIES = new HashSet<String>();
        static {
           validCodes.add("DENMARK");
           validCodes.add("SWEDEN");
           validCodes.add("FINLAND");
        }

        // Але є інший спосіб - Double Brace Initialization.

        private static final Set<String> COUNTRIES = new HashSet<String>() {{
            add("DENMARK");
            add("SWEDEN");
            add("FINLAND");
        }}

        // Використовується анонімний внутрішній клас

    } // Кінець методу main
} // Кінець класу LearnJava


// Можна додавати інші, не public класи зовнішнього рівня у .java файл,
// але це не є хорошою практикою. Розміщуйте класи в окремих файлах.


// Синтаксис оголошення класу:
// <public/private/protected> class <class name> {
//    // поля, конструктори, функції та ін.
//    // у Java функції називаються методами.
// }

class Bicycle {

    // Поля (змінні) класу Bicycle
    public int cadence; // Public: доступно звідусіль
    private int speed;  // Private: доступно лише у межах класу
    protected int gear; // Protected: доступно лише класу та нащадкам
    String name; // default: доступно у даному пакеті

    static String className; // статична змінна класу

    // статичний блок
    // Java не має статичних конструкторів, але
    // має статичний блок ініціалізації змінних класу    
    // Цей блок виконується при завантаженні класу.
    static {
        className = "Bicycle";
    }

    // Конструктори є способом створення класу
    // Це - конструктор
    public Bicycle() {
        // Можна викликати інший конструктор:
        // this(1, 50, 5, "Bontrager");
        gear = 1;
        cadence = 50;
        speed = 5;
        name = "Bontrager";
    }

    // Цей конструктор приймає аргументи
    public Bicycle(int startCadence, int startSpeed, int startGear,
        String name) {
        this.gear = startGear;
        this.cadence = startCadence;
        this.speed = startSpeed;
        this.name = name;
    }

    // Синтаксис методу:
    // <public/private/protected> <тип повернутого значення> <ім'я методу>(<аргументи>)

    // Java класи часто мають методи для отримання та встановлення змінних

    // Синтаксис оголошення методу:
    // <модифікатор доступу> <тип повернутого значення> <ім'я методу>(<аргументи>)
    public int getCadence() {
        return cadence;
    }

    // void методи не повертають значень
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

    //Метод показує значення змінних об'єкту.
    @Override // Inherited from the Object class.
    public String toString() {
        return "gear: " + gear + " cadence: " + cadence + " speed: " + speed +
            " name: " + name;
    }
} // кінець класу Bicycle

// PennyFarthing є розширенням (нащадком) класу Bicycle
class PennyFarthing extends Bicycle {
    // (Penny Farthings мають велике переднє колесо.
    // Вони не мають передач.)

    public PennyFarthing(int startCadence, int startSpeed){
        // Виклик батьківського конструктора через super
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // Перевизначений метод має відти відмічений аннотацією @annotation.
    // Для ознайомлення з аннотаціями перейдіть за посиланням
    // out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setGear(int gear) {
        gear = 0;
    }
}

// Інтерфейси
// Синтаксис оголошення інтерфейсів
// <рівень доступу> interface <ім'я інтерфейсу> extends <супер-інтерфейс> {
//     // Констатнти
//     // Оголошення методів
// }

//Приклад - Food:
public interface Edible {
    public void eat(); // Будь-які класи, що реалізують цей інтерфейс
                       // повинні реалізувати цей метод.
}

public interface Digestible {
    public void digest();
}


// Можна створити клас, що реалізує обидва інтерфейси.
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

// В Java можна успадковувати лише один клас, але реалізовувати багато
// інтерфейсів. Наприклад:
public class ExampleClass extends ExampleClassParent implements InterfaceOne,
    InterfaceTwo {

    @Override
    public void InterfaceOneMethod() {
    }

    @Override
    public void InterfaceTwoMethod() {
    }

}

// Абстрактні класи

// Синтаксис оголошення абстрактних класів:
// <ріаень доступу> abstract <ім1я класу> extends <супер-абстрактний клас> {
//     // Константи і змінні
//     // Оголошення методів
// }

// Позначення класу як абстрактного означає, що оголошені у ньому методи мають
// бути реалізовані у дочірніх класах. Подібно до інтерфейсів, не можна створити екземпляри
// абстракних класів, але їх можна успадковувати. Нащадок зобов'язаний реалізувати всі абстрактні
// методи. на відміну від інтерфейсів, абстрактні класи можуть мати як визначені,
// так і абстрактні методи. Методи в інтерфейсах не мають тіла,
// за вийнятком статичних методів, а змінні неявно мають модифікатор final, на відміну від
// абстрактного класу. Абстрактні класи МОЖУТЬ мати метод "main".

public abstract class Animal
{
    public abstract void makeSound();

    // Метод може мати тіло
    public void eat()
    {
        System.out.println("I am an animal and I am Eating.");  
        // Зауваження: є доступ до privat змінних.
        age = 30;
    }

    // Ініціалізація не потрібна
    protected int age;

    public void printAge()
    {
        System.out.println(age);  
    }

    // Абстрактні класи МОЖУТЬ мати метод "main".
    public static void main(String[] args)
    {
        System.out.println("I am abstract");
    }
}

class Dog extends Animal
{
    // Слід помічати перевизначення абстрактних методів
    @Override
    public void makeSound()
    {
        System.out.println("Bark");
        // age = 30;	==> ПОМИЛКА! age є private для Animal
    }

    // NOTE: Буде помилка, якщо використати аннотацію
    // @Override тут, так як у java не можна
    // перевизначати статичні методи.
    // Те, що тут відбувається, називається METHOD HIDING.
    // Більш детально: http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Dog pluto = new Dog();
        pluto.makeSound();
        pluto.eat();
        pluto.printAge();
    }
}

// Final класи

// Синтаксис оголошення Final класів
// <рівень доступу> final <ім'я класу> {
//     // Константи і змінні
//     // Оголошення методів
// }

// Final не можуть мати нащадків, також самі вони є останніми нащадками.
// Final класи є протилежністю абстрактних у цьому плані.

public final class SaberToothedCat extends Animal
{
    // Перевизначення методу
    @Override
    public void makeSound()
    {
        System.out.println("Roar");
    }
}

// Final методи
public abstract class Mammal()
{
    // Синтаксис Final методів:
    // <модифікаор доступу> final <тип повернутого значення> <ім'я функції>(<аргументи>)

    // Final методи не можуть бути перевизначені класом-нащадком,
    // вони є остаточною реалізацією методу.
    public final boolean isWarmBlooded()
    {
        return true;
    }
}


// Тип Enum (перелік)
//
// Enum є спеціальним типом даних, який дозволяє змінним бути певною множиною
// визначених констант. Змінна має відповідати одному зі значень, що
// заздалегідь визначені для неї. Так як це константи, імена типів полів у enum
// задаються у верхньому регістрі. У Java задається тип переліку за допомогою
// ключового слова. Наприклад, перелік днів тижня можна задати так:

public enum Day {
    SUNDAY, MONDAY, TUESDAY, WEDNESDAY,
    THURSDAY, FRIDAY, SATURDAY 
}

// Можна використовувати перелік Day так:

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

// Переліки набагато потужніші, ніж тут показано. 
// Тіло переліків може містити методи та інші змінні.
// Дивіться більше тут: https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

```

## Додатково для читання

The links provided here below are just to get an understanding of the topic, feel free to Google and find specific examples.

**Офіційні посібники Oracle**:

* [Java Tutorial Trail from Sun / Oracle](http://docs.oracle.com/javase/tutorial/index.html)

* [Java модифікатори доступу](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [ООП концепції](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Виключення](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Інтерфейси](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [параметризація](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](http://www.oracle.com/technetwork/java/codeconvtoc-136057.html)

**Online практика та посібники**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Книжки**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](http://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](http://www.amazon.com/gp/product/0071606300)
