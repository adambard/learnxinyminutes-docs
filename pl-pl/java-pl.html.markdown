---
language: java
filename: LearnJavaPl.java
contributors:
    - ["Jake Prather", "https://github.com/JakeHP"]
    - ["Jakukyo Friel", "https://weakish.github.io"]
    - ["Madison Dickson", "https://github.com/mix3d"]
    - ["Simon Morgan", "https://sjm.io/"]
    - ["Zachary Ferguson", "https://github.com/zfergus2"]
    - ["Cameron Schermerhorn", "https://github.com/cschermerhorn"]
    - ["Rachel Stiyer", "https://github.com/rstiyer"]
    - ["Michael Dähnert", "https://github.com/JaXt0r"]
    - ["Rob Rose", "https://github.com/RobRoseKnows"]
    - ["Sean Nam", "https://github.com/seannam"]
    - ["Shawn M. Hanes", "https://github.com/smhanes15"]
filename: LearnJava.java
translators:
    - ["Jacek Wachowiak", "https://github.com/jacekwachowiak"]
lang: pl-pl
---

Java jest współbieżnym, opartym na klasach, obiektowym językiem programowania
ogólnego zastosowania.
[Tu znajdziesz więcej informacji po angielsku.]
(https://docs.oracle.com/javase/tutorial/java/)

```java
// Pojedyncze komentarze oznaczamy //

/*
Komentarze wieloliniowe wyglądają tak
*/

/**
 * Komentarze JavaDoc wygladają w ten sposób. Używane są do opisu klas lub
 * różnych właściwości klas.
 * Główne właściwości:
 *
 * @author      Imię i nazwisko (i kontakt np. email) autora.
 * @version     Aktualna wersja programu.
 * @since       Kiedy ta część programu została dodana.
 * @param       Służy do opisu parametrów metody.
 * @return      Służy do opisu zwracanej wartości.
 * @deprecated  Służy do oznaczenia nieaktualnego lub niezalecanego kodu.
 * @see         Linki do innej cześci dokumentacji.
*/

// Import klasy ArrayList z paczki java.util
import java.util.ArrayList;
// Import wszystkich klas z paczki java.security
import java.security.*;

public class LearnJava {

    // Aby móc uruchomić program w języku java musi on mieć główną metodę jako
    // punkt wejścia.
    public static void main(String[] args) {

    ///////////////////////////////////////
    // Operacje wejścia/wyjścia (input/output)
    ///////////////////////////////////////

        /*
        * Wyjście
        */

        // System.out.println() służy do wyświetlania linii tekstu.
        System.out.println("Hello World!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // Aby wyświetlić  bez nowej linii użyj System.out.print().
        System.out.print("Hello ");
        System.out.print("World");

        // System.out.printf() służy do łatwego formatowania wyświetlanego elementu.
        System.out.printf("pi = %.5f", Math.PI); // => pi = 3.14159

        /*
         * Wejście
         */

        // Scanner służy do wczytywania danych
        // niezbędny jest import java.util.Scanner;
        Scanner scanner = new Scanner(System.in);

        // zczytaj string (tekst)
        String name = scanner.next();

        // zczytaj zmienną typu bajt
        byte numByte = scanner.nextByte();

        // zczytaj zmienną typu integer - liczba całkowita
        int numInt = scanner.nextInt();

        // zczytaj zmienną typu float - liczba zmiennoprzecinkowa
        float numFloat = scanner.nextFloat();

        // zczytaj zmienna typu double -liczba zmiennoprzecinkowa
        double numDouble = scanner.nextDouble();

        // zczytaj zmienną typu boolowskiego -
        boolean bool = scanner.nextBoolean();

        ///////////////////////////////////////
        // Zmienne
        ///////////////////////////////////////

        /*
        *  Deklaracja zmiennych
        */
        // Zmienną deklaruje się poprzez <rodzaj> <nazwa>
        int fooInt;
        // Dozwolona jest deklaracja wielu zmiennych tego samego typu na raz
        // rodzaj <nazwa1>, <nazwa2>, <nazwa3>
        int fooInt1, fooInt2, fooInt3;

        /*
        *  Inicjalizacja zmiennych
        */

        // Zmienną inicjalizuje się poprzez <rodzaj> <nazwa> = <wartość>
        int barInt = 1;
        // Możliwe jest zainicjalizowanie wielu zmiennych tego samego typu tą samą wartością
        // rodzaj <nazwa1>, <nazwa2>, <nazwa3>
        // <nazwa1> = <nazwa2> = <nazwa3> = <wartość>
        int barInt1, barInt2, barInt3;
        barInt1 = barInt2 = barInt3 = 1;

        /*
        *  Rodzaje zmiennych
        */
        // Bajt - 8-bitowa, zawierająca ujemne wartości zmienna w dwójkowym
        // systemie pozycyjnym
        // (-128 <= byte <= 127)
        byte fooByte = 100;

        // Jeśli chcemy zinterpretować bajt jako zmienną typu unsigned integer
        // - liczbę całkowitą z wartościami ujemnymi ta operacja może pomóc:
        int unsignedIntLessThan256 = 0xff & fooByte;
        // jako kontrast operacja zmiany typu która może zwrócić wartość ujemną.
        int signedInt = (int) fooByte;

        // Short - 16-bitowa, zawierająca ujemne wartości zmienna w dwójkowym
        // systemie pozycyjnym (-32,768 <= short <= 32,767)
        short fooShort = 10000;

        // Integer - 32-bitowa, zawierająca ujemne wartości zmienna w dwójkowym systemie pozycyjnym
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int bazInt = 1;

        // Long - 64-bitowa, zawierająca ujemne wartości zmienna w dwójkowym
        // systemie pozycyjnym
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long fooLong = 100000L;
        // L jest używane do zaznaczenia, że wartość zmiennej jest typu Long;
        // bez L wszystko inne będzie traktowane z założenia jako integer.

        // Uwaga: byte, short, int and long zawierają ujemne wartości.
        // Nie istnieją odpowiedniki z jedynie pozytywnymi wartościami.
        // Jedynie char jest 16-bitowym typem zmiennej, który akceptuje tylko
        // wartości nieujemne.

        // Float - 32-bitowy typ zmiennoprzecinkowy zgodnie z IEEE 754
        // Floating Point 2^-149 <= float <= (2-2^-23) * 2^127
        float fooFloat = 234.5f;
        // f or F jest używane aby zaznaczyć, że dana zmienna jest typu float;
        // w przeciwnym razie będzie ona traktowana jako double.

        // Double -  64-bitowy typ zmiennoprzecinkowy zgodnie z IEEE 754
        // Floating Point 2^-1074 <= x <= (2-2^-52) * 2^1023
        double fooDouble = 123.4;

        // Typ boolowski - true/prawda & false/fałsz
        boolean fooBoolean = true;
        boolean barBoolean = false;

        // Char - pojedynczy 16-bitowy symbol Unicode
        char fooChar = 'A';

        // zmienne zadeklarowane z użyciem final nie mogą być edytowane,
        final int HOURS_I_WORK_PER_WEEK = 9001;
        // ale możliwa jest późniejsza inicjalizacja.
        final double E;
        E = 2.71828;

        // BigInteger - Nieedytowalny typ zmiennej o nieograniczonej długości
        // dla liczb całkowitych
        //
        // BigInteger jest typem zmiennej, który pozwala na operacje na liczbach całkowitych dłuższych niż 64 bity.
        // Liczby są przechowywane jako tablica bajtów
        // i modyfikowane za pomocą funkcji wbudowanych w BigInteger
        //
        // BigInteger może być zainicjalizowany za pomocą tablicy bajtów lub jako string.
        BigInteger fooBigInteger = new BigInteger(fooByteArray);

        // BigDecimal - Nieedytowalny typ zmiennej o nieograniczonej długości dla
        // liczb zmiennoprzecinkowych
        //
        // BigDecimal zaiwera 2 części: typ integer o arbitralnej precyzji bez skalowania
        // oraz 32-bitową skalę
        //
        // BigDecimal pozwala programiście na całkowitą kontrolę zaokrąglenia dziesiętnego.
        // Zalecane jest używanie BigDecimal z wartościami walut.
        // oraz tam, gdzie absolutna dokładność jest niezbędna.
        //
        // BigDecimal można zainicjalizowac używając int, long, double or String
        // a także inicjalizując nieprzeskalowaną wartość (BigInteger) i skalę (int).
        BigDecimal fooBigDecimal = new BigDecimal(fooBigInteger, fooInt);

        // Uwaga na konstruktor, który przyjmuje float lub double jako, że
        // niedokładność float/double będzie przeniesiona do BigDecimal.
        // Zalecane jest uzywanie konstruktora typu String gdy konieczne jest
        // uzyskanie absolutnej precyzji.
        BigDecimal tenCents = new BigDecimal("0.1");

        // String - zmienna tekstowa
        String fooString = "Tutaj jest mój string!";

        // \n jest symbolem karetki, która rozpoczyna nową linę
        String barString = "Wyświetlanie w nowej linii?\nNie ma problemu!";
        // \t jest symbolem tabulatora, który dodaje odstęp.
        String bazString = "Chesz dodać tabulator?\tBez problemu!";
        System.out.println(fooString);
        System.out.println(barString);
        System.out.println(bazString);

        // Budowanie Stringów
        // #1 - za pomocą operatora dodawania
        // To jest podstawowy sposób (zoptymalizowany)
        String plusConcatenated = "Stringi mogą " + "być łączone " + "operatorem +.";
        System.out.println(plusConcatenated);
        // Wyjście: Stringi będą połączone operatorem +.

        // #2 - za pomocą StringBuilder
        // Ten sposób nie tworzy żadnych pośrednich stringów, jedynie zachowuje
        // części i wiąże je po kolei gdy wywołane jest toString().
        // Wskazówka: Ta klasa nie jest bezpieczna z punktu widzenia wątków.
        // Bezpieczną alternatywą jest (wiążąca się ze spadkiem wydajności)
        // StringBuffer.
        StringBuilder builderConcatenated = new StringBuilder();
        builderConcatenated.append("Możesz ");
        builderConcatenated.append("użyć ");
        builderConcatenated.append("klasy StringBuilder.");
        System.out.println(builderConcatenated.toString()); // dopiero tutaj
        //budowany jest string
        // Wyjście: Używany jest StringBuilder.

        // StringBuilder jest wydajny, gdy połączony string nie jest używany aż do końcowego przetworzenia.
        StringBuilder stringBuilder = new StringBuilder();
        String inefficientString = "";
        for (int i = 0 ; i < 10; i++) {
            stringBuilder.append(i).append(" ");
            inefficientString += i + " ";
        }
        System.out.println(inefficientString);
        System.out.println(stringBuilder.toString());
        // inefficientString wymaga dużo więcej pracy przy stworzeniu ponieważ
        // tworzy string przy każdej iteracji.
        // Proste łączenie za pomocą + jest kompilowane do StringBuilder i
        // toString(). Unikaj łączenia stringów w pętlach.

        // #3 - za pomocą String formatter
        // Inna możliwość, szybka i czytelna.
        String.format("%s wolisz %s.", "A może", "String.format()");
        // Wyjście: Być może wolisz String.format().

        // Tablice
        // Rozmiar tablicy musi być określony przy stworzeniu.
        // Podane poniżej sposoby są dozwolone prz deklaracji tablicy
        // <rodzaj>[] <nazwa> = new <rodzaj>[<rozmiar>];
        // <rodzaj> <nazwa>[] = new <rodzaj>[<rozmiar>];
        int[] intArray = new int[10];
        String[] stringArray = new String[1];
        boolean boolArray[] = new boolean[100];

        // Inny sposób deklaracji i inicjalizacji tablicy
        int[] y = {9000, 1000, 1337};
        String names[] = {"Bob", "John", "Fred", "Juan Pedro"};
        boolean bools[] = {true, false, false};

        // Indeksowanie tablicy - dostęp do elementów
        System.out.println("intArray @ 0: " + intArray[0]);

        // Tablice zaczynają się z indeksem 0 i są edytowalne.
        intArray[1] = 1;
        System.out.println("intArray @ 1: " + intArray[1]); // => 1

        // Inny typ zmiennej, z którymi warto się zapoznać
        // ArrayLists - Tablice z większą funkcjonalnością
        //              i zmiennym rozmiarem.
        // LinkedLists - Dwustronnie połączone listy. Wszystkie operacje
        //               na listach zaimpllementowane.
        // Maps - Mapy zawierające klucz i wartość. Mapa jest interfejsem
        //        i nie może zostać zainicjalizowana.
        //        Rodzaj klucza i wartości dla mapy musi zostać określony
        //        przy inicjalizacji implementującej mapę klasy
        //        Każdy klucz przypisany jest do tylko jednej wartości,
        //        każdy klucz może wystąpić tylko raz (brak duplikatów).
        // HashMaps - Używa tablicy hashów do implementacji interfejsu mapy
        //            Pozwala to na użycie podstawowych operacji, jak
        //            get i insert, które pozostają niezmiennie wydajne
        //            czasowo nawet dla dużych zestawów danych
        // TreeMap - Mapa posortowana przez klucze. Każda modyfikacja
        //           utrzymuje sortowanie, zdefiniowane przez komparator
        //           dodany przy inicjalizacji lub porównanie każdego obiektu
        //           jeśli zaimplementowany jest interfejs Comparable.
        //           Niepowodzenie kluczy wimplemntacji Comparable połączone
        //           z niepowodzeniem dostarczenia komparatora spowoduje
        //           ClassCastExceptions.
        //           Dodawanie i usuwanie kosztuje O(log(n)) czasu,
        //           zalecane jest nieużywanie tego typu jeżeli sortowanie
        //           nie jest przydatne.

        ///////////////////////////////////////
        // Operatory
        ///////////////////////////////////////
        System.out.println("\n->Operatory");

        int i1 = 1, i2 = 2; // Skrót dla wielokrotnych deklaracji

        // Arytmetyka jest prosta
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (int/int zwraca int)
        System.out.println("1/2.0 = " + (i1 / (double)i2)); // => 0.5

        // Modulo
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Porównania
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Operacje boolowskie
        System.out.println("3 > 2 && 2 > 3? " + ((3 > 2) && (2 > 3))); // => false
        System.out.println("3 > 2 || 2 > 3? " + ((3 > 2) || (2 > 3))); // => true
        System.out.println("!(3 == 2)? " + (!(3 == 2))); // => true

        // Operacje na bitach!
        /*
        ~      Odwrócenie bitów
        <<     Przesunięcie w lewo
        >>     Przesunięcie w prawo, arytmetyczne/dla wartości ujemnych -signed
        >>>    Przesunięcie w prawo, logiczne/dla wartości dodatnich - unsigned
        &      Bitowe AND
        ^      Bitowe XOR
        |      Bitowe OR
        */

        // Operatory inkrementacji
        int i = 0;
        System.out.println("\n->In/De-krementacja");
        // Operatory ++ i -- zwiększają lub zmniejszają o 1 daną wartość.
        // Jeżeli używane są przed zmienną, wykonywane są przed powrotem zmiennej.
        // Użyte po zmiennej najpierw zwracają zmienną a następnie dokonują
        // zmiany wartości.
        System.out.println(i++); // i = 1, wyświetli 0 (post-increment)
        System.out.println(++i); // i = 2, wyświetli 2 (pre-increment)
        System.out.println(i--); // i = 1, wyświetli 2 (post-decrement)
        System.out.println(--i); // i = 0, wyświetli 0 (pre-decrement)

        ///////////////////////////////////////
        // Przepływ sterowania
        ///////////////////////////////////////
        System.out.println("\n->Przepływ sterowania");

        // Instrukcja if wygląda jak w c
        int j = 10;
        if (j == 10) {
            System.out.println("Wyświetlam się");
        } else if (j > 10) {
            System.out.println("A ja nie");
        } else {
            System.out.println("Ja też nie");
        }

        // Pętla while
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Licznik jest zwiększany
            // Iteruje 100 razy, fooWhile 0,1,2...99
            fooWhile++;
        }
        System.out.println("Wartość fooWhile: " + fooWhile);

        // Pętla do while
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Licznik jest zwiększany
            // Iteruje 99 razy, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("Wartość fooDoWhile: " + fooDoWhile);

        // Pętla for
        // struktura pętli for => for(<początek>; <warunek>; <krok>)
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // Iteruje 10 razy, fooFor 0->9
        }
        System.out.println("Wartość fooFor: " + fooFor);

        // Wyjście z zagnieżdżonej, oznaczonej pętli for
        outer:
        for (int i = 0; i < 10; i++) {
          for (int j = 0; j < 10; j++) {
            if (i == 5 && j ==5) {
              break outer;
              // wychodzi z zewnętrznej pętli zamiast jednynie z aktualnej z
              // powodu oznaczenia
            }
          }
        }

        // Pętla for each
        // Pętla for each może iterować tablice jak i obiekty
        // które implementują interfejs Iterable.
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // Struktura for each => for (<element> : <obiekt iterowany>)
        // należy rozumieć jako: dla każdego elementu w obiekcie iterowanym
        // uwaga: typ zdefiniowango elementu musi się zgadzać z typem w
        //obiekcie iterowanym.
        for (int bar : fooList) {
            System.out.println(bar);
            //Iteruje 9 razy i wyświetla 1-9 w nowych liniach
        }

        // Switch Case
        // Switch (przełącznik) działa z zmiennymi typu byte, short, char, int.
        // Działa również z enumeratorami (zobacz typ Enum),
        // klasą String, i kilkoma specjalnymi klasami które zawierają typy
        // podstawowe: Character, Byte, Short, and Integer.
        // Z wersją Java 7 i wyższymi możliwe jest użycie typu String.
        // Uwagga: Pamiętaj, że nie dodając "break" na końcu danego case
        // spowoduje przejście do następnego (jeżeli spełniony jest warunek).
        int month = 3;
        String monthString;
        switch (month) {
            case 1: monthString = "Styczeń";
                    break;
            case 2: monthString = "Luty";
                    break;
            case 3: monthString = "Marzec";
                    break;
            default: monthString = "Inny miesiąc";
                     break;
        }
        System.out.println("Wynik Switch Case : " + monthString);


        // Try-with-resources (Java 7+)
        // Try-catch-finally działa zgodnie z oczekiwaniami jednakże w Java 7+
        // dodatkowo jest dostępny try-with-resources statement.
        // Try-with-resources upraszcza try-catch-finally automatycznie
        // usuwając zasoby.

        // Aby użyć try-with-resources, użyj instancji klasy
        // w części "try". Klasa musi implementować java.lang.AutoCloseable.
        try (BufferedReader br = new BufferedReader(new FileReader("foo.txt"))) {
            // Tutaj możesz spróbować wywołac wyjątek.
            System.out.println(br.readLine());
            // W Java 7 zasoby będą zawsze usuwane nawet jeśli nastąpi wyjątek.
        } catch (Exception ex) {
            // Zasób będzie usunięty zanim wykona się catch.
            System.out.println("readLine() nie powiódł się.");
        }
        // Nie ma potrzeby używać sekcji "finally", jako że BufferedReader
        // został już zamknięty. Ten sposób może zostać użyty aby uniknąć
        // pewnych wartości brzegowych gdzie "finally" nie zostałoby wywołane
        // Więcej na ten temat po angielsku:
        // https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html


        // Skrócone instrukcje warunkowe
        // Dozwolone jest użycie operatora '?' aby szybko sprawdzić warunek
        // logiczny. Rozumiane jest to jako "Jeśli (warunek) jest spełniony, użyj
        // <pierwszej wartości>, inaczej, użyj <drugiej wartości>"
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println("bar : " + bar); // Wyśwletli "bar : A", poineważ
        // warunke jest spełniony.
        // Lub prościej
        System.out.println("bar : " + (foo < 10 ? "A" : "B"));


        ////////////////////////////////////////
        // Konwersja typów danych
        ////////////////////////////////////////

        // Konwersja danych

        // Konwersja String do Integer
        Integer.parseInt("123");//zwraca zmienna typu Integer o wartości "123"

        // Konwersja Integer do String
        Integer.toString(123);//zwraca zmienną typu String o wartości 123

        // Inne konwersje możesz sprawdzić dla klas:
        // Double
        // Long
        // String

        ///////////////////////////////////////
        // Klasy i funkcje
        ///////////////////////////////////////

        System.out.println("\n->Klasy & Funkcje");

        // (definicja klasy Rower nieco niżej)

        // Użyj new aby zainstancjonować klasę
        Rower trek = new Rower();

        // Wywoływanie metod klasy
        trek.predkoscZwieksz(3); // Zawsze używaj settera i gettera jako metod
        trek.setPedalowanie(100);

        // toString zwraca reprezentację typu String tego obiektu.
        System.out.println("trek info: " + trek.toString());

        // Inicjalizacja za pomocą podwójnego nawiasu
        // Język Java nie zawiera możliwości stworzenia statycznej kolekcji
        // Dlatego zwykle odbywa się to w ten sposób:
        private static final Set<String> KRAJE = new HashSet<String>();
        static {
           KRAJE.add("DANIA");
           KRAJE.add("SZWECJA");
           KRAJE.add("FINLANDIA");
        }

        // Jest jednak sprytny sposób aby łatwiej osiągnąc ten sam efekt
        // używając czegoś nazywanego Double Brace Initialization -
        // inicjalizacja za pomocą podwójnego nawiasu.
        private static final Set<String> KRAJE = new HashSet<String>() {{
            add("DANIA");
            add("SZWECJA");
            add("FINLANDIA");
        }}

        // Pierwszy nawias tworzy nową klasę AnonymousInnerClass,
        // drugi deklaruje instancję bloku inicjalizacji. Blok ten
        // jest wywoływany gdy wewnętrzna, anonimowa klasa jest tworzona.
        // Dany sposób działa nie tylko dla kolekcji, ale również dla
        // wszystkich nie-finalnych klas.

    } // Koniec metody main
} // Koniec klasy LearnJava

// Możesz zawrzeć inne, niepubliczne, zewnętrzne klasy w pliku .java,
// jednak nie jest to zalecane. Zalecane jest dzielenie klas na osobne pliki.

// Składnia deklaracji klasy:
// <public/private/protected> class <nazwa klasy> {
//    // pola danych, konstruktory, funkcje.
//    // w jężyku Java funkcje są wywoływane jako metody.
// }

class Rower {

    // Zmienne klasy
    public int pedalowanie; // Public: Dostępne wszędzie
    private int predkosc;  // Private: Dostępne tylko w klasie
    protected int przerzutka; // Protected: Dostępne w klasie i podklasach
    String nazwa; // domyślnie: Dostępne tlyko w danej paczce
    static String nazwaKlasy; // Zmienna statyczna

    // Blok statyczny
    // Java nie posiada implemntacji konstruktorów staycznych, ale
    // posiada blok stayczny, który może  być użyty aby zainicjalizować
    // statyczne zmienne klasy
    // Ten blok będzie wywołane gdy klasa jest ładowana.
    static {
        nazwaKlasy = "Rower";
    }

    // Konstruktory służą do stworzenia instancji klas
    // Too jest konstruktor
    public Rower() {
        // Możesz wywołać także  inny konstruktor:
        // this(1, 50, 5, "Bontrager");
        przerzutka = 1;
        pedalowanie = 50;
        predkosc = 5;
        nazwa = "Bontrager";
    }
    // To jest konstruktor, który przyjmuje argumenty
        public Rower(int poczatkowePedalowanie, int poczatkowaPredkosc, int początkowaPrzerzutka,
        String nazwa) {
        this.przerzutka = początkowaPrzerzutka;
        this.pedalowanie = poczatkowePedalowanie;
        this.predkosc = poczatkowaPredkosc;
        this.nazwa = nazwa;
    }

    // Składnia metod:
    // <public/private/protected> <zwracany rodzaj> <nazwa funkcji>(<argumenty>)

    // Klasy często implementują metody getter i setter dla danych wewnątrz

    // Składnia deklaracji metody:
    // <dostępność> <zwracany rodzaj> <nawa metody>(<argumenty>)
    public int getPedalowanie() {
        return pedalowanie;
    }

    // metody void nie wymagają słowa kluczowego return, nic nie zwracają
    public void setPedalowanie(int newValue) {
        pedalowanie = newValue;
    }
    public void setPrzerzutka(int newValue) {
        przerzutka = newValue;
    }
    public void predkoscZwieksz(int inkrement) {
        predkosc += inkrement;
    }
    public void predkoscZmniejsz(int dekrement) {
        predkosc -= dekrement;
    }
    public void nadajNazwe(String nowaNazwa) {
        nazwa = nowaNazwa;
    }
    public String zwrocNazwe() {
        return nazwa;
    }

    // Metoda do wyświetlenia wartości atrybutów danego obiektu.
    @Override // Dziedziczy z klasy obiektu.
    public String toString() {
        return "przerzutka: " + przerzutka + " pedalowanie: " + pedalowanie + " predkosc: " + predkosc +
            " nazwa: " + nazwa;
    }
} // koniec klasy Rower

// PennyFarthing jest podklasą klasy Rower
class PennyFarthing extends Rower {
    // (Penny Farthing to rower z wielkim przednim kołem.
    // Nie ma przerzutek.)

    public PennyFarthing(int poczatkowePedalowanie, int poczatkowaPredkosc) {
        // Wywołanie kostruktora klasy z której dziedziczymy za pomocą super
        super(poczatkowePedalowanie, poczatkowaPredkosc, 0, "PennyFarthing");
    }

    // Używamy annotacji @annotation przy przeciążaniu metod.
    // Aby dowiedzieć się więcej o annotacjach przydatne jest przejrzenie
    // (w języku angielskim):
    // http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setPrzerzutka(int przerzutka) {
        this.przerzutka = 0;
    }
}

// Rzutowanie
// Jako, że  klasa PennyFarthing dziedziczy z klasy Rower, możemy uznać, że
// instancja PennyFarthing jest typu Rower i napisać :
// Rower rower = new PennyFarthing();
// Dana operacja jest rzutowaniem obiektu, gdzie jego domyślna klasa jest inna niż docelowa.
// Więcej szczegółów i przykładów oraz ciekawych konceptów (po angielsku):
// https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html

// Interfejsy
// Składnia deklaracji interfejsu
// <dostępność> interface <nazwa interfejsu> extends <super-interfaces> {
//     // Zmienne typu constant
//     // Deklaracje metod
// }

// Przykład - Jedzenie:
public interface Jadalne {
    public void jedz(); // Każda klasa która implemetuje ten interfejs musi
    // implementować tę metodę.
}

public interface Przetrawialne {
    public void przetrawiaj();
    // Wraz z Java 8, interfejsy mogą mieć metodę domyślną.
    public default void defaultMethod() {
        System.out.println("Hej z metody domyślnej ...");
    }
}

// Teraz stworzymy klasę, która zaimplementuje oba interfejsy.
public class Owoc implements Jadalne, Przetrawialne {
    @Override
    public void jedz() {
        // ...
    }

    @Override
    public void przetrawiaj() {
        // ...
    }
}

// W Javie możesz dziedziczyć jedynie z jednej klasy, jednak implementować
// wiele interfejsów. Na przykład:
public class Przyklad extends Przodek implements Interfejs1,
    Interfejs2 {
    @Override
    public void Interfejs1Metoda() {
    }

    @Override
    public void Interfejs2Metoda() {
    }

}

// Klasy abstrakcyjne

// Składnia deklaracji klasy abstrakcyjnej
// <dostępność> abstract class <nawa klasy abstrakcyjnej> extends
// <superklasy, z których dziedziczy> {
//     // Zmienne i stałe
//     // Deklaracje metod
// }

// Klasy abstrakcyjne nie mogą posiadać instancji.
// Klasy abstrakcyjne mogą definiować  metody abstrakcyjne.
// Metody abstrakcyjne nie mają ciała funkcji i są oznaczone jako abstrakcyjne.
// Nieabstrakcyjne klasy-dzieci muszą przeciążać wszystkie abstrakcyjne metody
// superklasy.
// Klasy abstrakcyjne są użyteczne gdy wymagana jest powtarzalna logika działania,
// jednak należy zaauważyć, że jako, że wymagają dziedziczenia, łamią
// zasadę "Composition over inheritance". Rozważ inne podejścia używając
// kompozycji. https://en.wikipedia.org/wiki/Composition_over_inheritance

public abstract class Zwierze
{
    private int wiek;

    public abstract void dajGlos();

    // Metody mogą mieć ciało
    public void jedz()
    {
        System.out.println("Jestem zwierzeciem i jem.");
        // Uwaga: Możliwy jest dostęp do zmiennych prywatnych.
        wiek = 30;
    }

    public void wyswietlWiek()
    {
        System.out.println(wiek);
    }

    // Klasy abstrakcyjne mogą mieć metodę główną.
    public static void main(String[] args)
    {
        System.out.println("Jestem abstrakcyjna");
    }
}

class Pies extends Zwierze
{
    // Musimy przeciążyć wszystkie abstrakcyjne metody z klasy abstrakcyjnej
    @Override
    public void dajGlos()
    {
        System.out.println("Hau");
        // wiek = 30;    ==> BLAD!    wiek jest typu private dla Zwierze
    }

    // NOTE: Wystąpi błąd jeżeli użyto annotacji @Override jako, że Java
    // nie pozwala na przeciążanie metod statycznych.
    // Występuje tutaj METHOD HIDING - ukrywanie metod.
    // Więcej w poście na SO: http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Pies pluto = new Pies();
        pluto.dajGLos();
        pluto.jedz();
        pluto.wyswietlWiek();
    }
}

// Klasy finalne

// Składnia deklaracji klasy finalnej
// <dostępność> final <nazwa klasy finalnej> {
//     // Zmienne i stałe
//     // Deklaracje Metody
// }

// Klasy finalne są klasami, które nie mogą być użyte do dziedziczenia, są więc
// z założenia ostatnim elementem dziedziczenia. W ten sposób są przeciwnością
// klas abstrakcyjnych, które z założenia muszą być dziedziczone.
public final class TygrysSzablozebny extends Zwierze
{
    // Nadal musimy przeciążyć metody abstrakcyjne klasy abstrakcyjnej Zwierze
    @Override
    public void dajGlos()
    {
        System.out.println("Roar");
    }
}

// Metody finalne
public abstract class Ssak
{
    // Składnia metody finalnej:
    // <dostępność> final <zwracany rodzaj> <nazwa funkcji>(<argumenty>)

    // Metody finalne, jak klasy finalne nie mogą być przeciążane
    // i są w ten sposób ostatecznymi implementacjami danej metody.
    public final boolean jestStalocieplny()
    {
        return true;
    }
}

// Enumeratory
//
// Enumerator jest specjalnym tyme danej, która pozwala zmiennej na bycie
// zestawem wcześniej zdefiniowanych stałych. Zmienna musi być równa jednej z
// wartości wcześniej zdefiniowanych. Jako, że są to stałe, nazwy pól typu enum
// są pisane wielkimi literami. W języku Java typ enum definiujemy przez użycie
// słowa enum. Na przykład aby zdefiniować dni tygodnia:
public enum Dzien {
    PONIEDZIALEK, WTOREK, SRODA, CZWARTEK,
    PIATEK, SOBOTA, NIEDZIELA
}

// We can use our enum Day like that:
public class EnumTest {
    // Zmienna typu enum
    Dzien dzien;

    public EnumTest(Dzien dzien) {
        this.dzien = dzien;
    }

    public void opiszDzien() {
        switch (dzien) {
            case PONIEDZIALEK:
                System.out.println("Nie lubię poniedziałku!");
                break;
            case PIATEK:
                System.out.println("Piątki są dużo lepsze.");
                break;
            case SOBOTA:
            case NIEDZIELA:
                System.out.println("Weekendy są najlepsze.");
                break;
            default:
                System.out.println("Środek tygodnia jest taki sobie.");
                break;
        }
    }

    public static void main(String[] args) {
        EnumTest pierwszyDzien = new EnumTest(Dzien.PONIEDZIALEK);
        pierwszyDzien.opiszDzien(); // => Nie lubię poniedziałku!
        EnumTest trzeciDzien = new EnumTest(Dzien.SRODA);
        trzeciDzien.opiszDzien(); // => Środek tygodnia jest taki sobie.
    }
}

// Typ enum jest bardziej wszechstronny niż powyższa demostracja.
// Ciało typu enum może zawierać metody i inne pola.
// Rzuć okiem na (angielski) https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

// Wprowadzenie do wyrażeń lambda
//
// Nowe w Javie 8 są wyrażenia lambda. Lambdy znajdujemy zwykle w funkcyjnych
// językach programowania, co oznacza, że są metodami, które potrafią być
// stowrzone bez klasy i przekazywane jak gdyby były obiektem oraz wykonywane
// gdy zajdzie potrzeba.
//
// Ostatnia uwaga - lambdy muszą implementować funcjonalny interfejs.
// Interfels funkcjonalny to taki, który ma jedynie jedną zadeklarowaną metodę
// abstrakcyjną, ale może mieć dowolną ilość domyślnych metod. Wyrażenia lambda
// mogą być używane jako instancje tego interfejsu. Każdy inteferjs, który
// spełnia wymagania jest traktowany jako funkcjonalny. Więcej o interfejsach
// znajdziesz powyżej, w odpowiedniej sekcji.
//
import java.util.Map;
import java.util.HashMap;
import java.util.function.*;
import java.security.SecureRandom;

public class Lambdas {
    public static void main(String[] args) {
        // Składnia deklaracji lambdy:
	    // <zero lub więcej parametrów> -> <ciało wyrażenia lub blok instrukcji>

        // Poniżej w przykładzie użyjemy tablicy z hashowaniem.
        Map<String, String> planety = new HashMap<>();
            planety.put("Merkury", "87.969");
            planety.put("Wenus", "224.7");
            planety.put("Ziemia", "365.2564");
            planety.put("Mars", "687");
            planety.put("Jowisz", "4,332.59");
            planety.put("Saturn", "10,759");
            planety.put("Uran", "30,688.5");
            planety.put("Neptun", "60,182");

        // Lambda z zerową liczbą parametrów używając funkcjonalnego interfejsu
        // Supplier z java.util.function.Supplier. Faktyczną lambdą jest częśc
        // po numPlanets =.
        Supplier<String> numPlanety = () -> Integer.toString(planety.size());
        System.out.format("Liczba planet: %s\n\n", numPlanety.get());

        // Lambda z jednym parametrem używająca funkcjonalnego interfejsu
        // Consumer z java.util.function.Consumer.planety jest mapą, która
        // wimplementuje Collection jak i Iterable. Użyty forEach pochodzi z
        // Iterable i jest użyty w lambdzie na każdym elemencie kolekcji
        // Domyślna implementacja forEach wygląda tak:
        /*
            for (T t : this)
                action.accept(t);
        */

        // Faktyczna lambda jest parametrem przekazywanym do forEach.
        planety.keySet().forEach((p) -> System.out.format("%s\n", p));

        // Jeżeli przekazujemy tyklo pojedynczy argumentpowyższy zapis możemy
        // przekształcić do (zauważ brak nawiasów dookoła p):
        planety.keySet().forEach(p -> System.out.format("%s\n", p));

        // Śledząc powyższe widzimy, że planety jest typu HashMap, a keySet()
        // zwraca zestaw kluczy, forEach stosuje o każdego elementu lambdę:
        // (parameter p) -> System.out.format("%s\n", p). Za każdym razem
        // element jest uznawany jako  "konsumowany" i wyrażenie (wyrażenia)
        // w lambdzie są wykonywane. Pamiętaj, że ciało lambdy to część po
        // symbolu ->.

        // Powyższy przykład bez użycia lambdy wyglądałby tradycyjnie tak:
        for (String planeta : planety.keySet()) {
            System.out.format("%s\n", planeta);
        }

        // Poniższy przykład różni się od powyższego sposobem implementacji
        // forEach:  forEach użyty w klasie HashMap implementuje intefejs Map.
        // Poniższy forEach przyjmuje BiConsumer, który ogólnie ujmując jest
        // wymyślnym sposobem stwierdzenia, że zajmuje się zestawem par
        // klucz-wartość Key -> Value dla każdego klucza. Ta domyślna
        // implementacja działa jak:
        /*
            for (Map.Entry<K, V> entry : map.entrySet())
                action.accept(entry.getKey(), entry.getValue());
        */

        // Faktyczna lambda jest parametrem przekazywanym do forEach.
        String orbity = "%s okrąża Słońce w %s dni.\n";
        planety.forEach((K, V) -> System.out.format(orbity, K, V));

        // Powyższe bez użycia lambdy wyglądałoby tradycyjnie tak:
        for (String planet : planety.keySet()) {
            System.out.format(orbity, planet, planety.get(planet));
        }

        // Lub jeżeli postępujemy zgodnie ze specyfikacją domyślnej implementacji:
        for (Map.Entry<String, String> planeta : planety.entrySet()) {
            System.out.format(orbity, planeta.getKey(), planeta.getValue());
        }

        // Podane przykłady pokrywają jedynie podstawowe zastosowanie wyrażeń
        // lambda. Być może wydają się one niezbyt przydatne, jednak należy
        // pamiętać, że lambdy można stworzyć jako obiekty, które nastepnie mogą
        // zostać przekazane jako parametry do innych metod.
    }
}
```

## Dalsze materiały

Linki zamieszczone poniżej służą pomocą w zrozumieniu wybranego tematu, w razie braku rozwiązania wyszukanie w Google zwykle służy pomocą

**Oficjalne poradniki Oracle po angielsku**:

* [Tutorial w Javie od Sun / Oracle](https://docs.oracle.com/javase/tutorial/index.html)

* [Modyfikacje poziomu dostępu w Java](https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Koncepty programowania obiektowego](https://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Dziedziczenie](https://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polimorfizm](https://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstrakcja](https://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Wyjątki](https://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfejsy](https://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Uogólnianie](https://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Konwencja kodu Java](https://www.oracle.com/technetwork/java/codeconvtoc-136057.html)

* Nowości z Java 8:
    * [Funkcje Lambda (programowanie funkcyjne)](https://docs.oracle.com/javase/tutorial/java/javaOO/lambdaexpressions.html)
    * [Data y czas API (java.time package)](http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)

**Kursy po polsku**

* [PJWSTK - Podstawy programowania w języku Java](http://edu.pjwstk.edu.pl/wyklady/ppj/scb/)

* [PJWSTK - Programowanie obiektowe w języku Java](http://edu.pjwstk.edu.pl/wyklady/poj/scb/)

**Tutoriale i ćwiczenia online po angielsku**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)

* [Codewars - Java Katas](https://www.codewars.com/?language=java)

**Książki po angielsku**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](https://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](https://www.amazon.com/gp/product/0071606300)
