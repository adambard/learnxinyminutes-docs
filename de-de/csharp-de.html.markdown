---
language: c#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
translators:
    - ["Frederik Ring", "https://github.com/m90"]
filename: LearnCSharp-de.cs
lang: de-de
---
C# ist eine elegante, typsichere und objektorientierte Sprache, mit der Entwickler eine Vielzahl sicherer und robuster Anwendungen erstellen können, die im .NET Framework ausgeführt werden.

[Mehr über C# erfährst du hier.](http://msdn.microsoft.com/de-de/library/vstudio/z1zx9t92.aspx)

```c#
// Einzeilige Kommentare starten mit zwei Schrägstrichen: //
/*
Mehrzeile Kommentare wie in C Schrägstrich / Stern
*/
/// <summary>
/// XML-Kommentare können zur automatisierten Dokumentation verwendet werden
/// </summary>

// Zu Beginn werden die in der Datei verwendeten Namespaces aufgeführt
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Dynamic;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Threading.Tasks;
using System.IO;

// definiert einen Namespace um Code in "packages" zu organisieren
namespace Learning
{
    // Jede .cs-Datei sollte zumindest eine Klasse mit dem Namen der Datei
    // enthalten. Das ist zwar nicht zwingend erforderlich, es anders zu
    // handhaben führt aber unweigerlich ins Chaos (wirklich)!
    public class LearnCSharp
    {
        // Zuerst erklärt dieses Tutorial die Syntax-Grundlagen,
        // wenn du bereits Java oder C++ programmieren kannst:
        // lies bei "Interessante Features" weiter!
        public static void Syntax()
        {
            // Mit Console.WriteLine kannst du einfachen Text ausgeben:
            Console.WriteLine("Hallo Welt");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

            // Console.Write erzeugt keinen Zeilenumbruch
            Console.Write("Hallo ");
            Console.Write("Welt");

            ///////////////////////////////////////////////////
            // Typen & Variablen
            ///////////////////////////////////////////////////

            // Deklariere eine Variable mit <Typ> <Name>

            // Sbyte - Vorzeichenbehaftete 8-Bit Ganzzahl
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - Vorzeichenlose 8-Bit Ganzzahl
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - 16-Bit Ganzzahl
            // Vorzeichenbehaftet - (-32,768 <= short <= 32,767)
            // Vorzeichenlos - (0 <= ushort <= 65,535)
            short fooShort = 10000;
            ushort fooUshort = 10000;

            // Integer - 32-bit Ganzzahl
            int fooInt = 1; // (-2,147,483,648 <= int <= 2,147,483,647)
            uint fooUint = 1; // (0 <= uint <= 4,294,967,295)

            // Long - 64-bit Ganzzahl
            long fooLong = 100000L; // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            ulong fooUlong = 100000L; // (0 <= ulong <= 18,446,744,073,709,551,615)
            // Ganze Zahlen werden standardmäßig - je nach Größe - als int oder
            // uint behandelt. Ein nachgestelltes L markiert den Wert als long
            // oder ulong.

            // Double - Double-precision 64-bit IEEE 754 Fließkommazahl
            double fooDouble = 123.4; // Genauigkeit: 15-16 Stellen

            // Float - Single-precision 32-bit IEEE 754 Fließkommazahl
            float fooFloat = 234.5f; // Genauigkeit: 7 Stellen
            // Das nachgestellte f zeigt an dass es sich um einen Wert vom Typ
            // float handelt

            // Decimal - ein 128-Bit-Datentyp mit größerer Genauigkeit als
            // andere Fließkommatypen, und somit bestens geeignet für
            // die Berechnung von Geld- und Finanzwerten
            decimal fooDecimal = 150.3m;

            // Boolean - true & false
            bool fooBoolean = true; // oder false

            // Char - Ein einzelnes 16-Bit Unicode Zeichen
            char fooChar = 'A';

            // Strings - im Gegensatz zu allen vorhergehenden Basistypen, die
            // alle Werttypen sind, ist String ein Referenztyp. Strings sind
            // somit nullable, Werttypen sind dies nicht.
            string fooString = "\"maskiere\" Anführungszeichen, und füge \n (Umbrüche) und \t (Tabs) hinzu";
            Console.WriteLine(fooString);

            // Jeder Buchstabe eines Strings kann über seinen Index
            // referenziert werden:
            char charFromString = fooString[1]; // => 'e'
            // Strings sind unveränderlich:
            // `fooString[1] = 'X';` funktioniert nicht

            // Ein Vergleich zweier Strings, unter Berücksichtigung der
            // aktuellen, sprachspezifischen Gegebenheiten (also z.B. a,ä,b,c
            // in deutschsprachigen Umgebungen), und ohne Beachtung von
            // Groß- und Kleinschreibung:
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // Formatierung, genau wie "sprintf"
            string fooFs = string.Format("Mikrofon Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // Datumsangaben und Formatierung
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // Durch ein vorangestelltes @ lässt sich ein mehrzeiliger String
            // schreiben. Um " zu maskieren benutzt man ""
            string bazString = @"Hier geht es
zur nächsten Zeile, ""Wahnsinn!"", die Massen waren kaum zu bändigen";

            // Die Keywords const oder readonly kennzeichnen eine
            // unveränderliche Variable/Konstante. Die Werte von Konstanten
            // werden übrigens bereits zur Compile-Zeit berechnet.
            const int HOURS_I_WORK_PER_WEEK = 9001;

            ///////////////////////////////////////////////////
            // Datenstrukturen
            ///////////////////////////////////////////////////

            // Arrays - Index beginnt bei Null
            // Die Größe des Arrays wird bei der Deklaration festgelegt.
            // Die syntaktische Struktur um ein neues Array zu erzeugen sieht
            // folgendermaßen aus:
            // <datatype>[] <varname> = new <datatype>[<array size>];
            int[] intArray = new int[10];

            // Arrays können auch über ein Array-Literal deklariert werden:
            int[] y = { 9000, 1000, 1337 };

            // Indizierung eines Arrays - Zugriff auf ein bestimmtes Element
            Console.WriteLine("intArray @ 0: " + intArray[0]);
            // Arrays sind veränderbar
            intArray[1] = 1;

            // Listen
            // Durch ihre größere Flexibilität kommen Listen in C# weit
            // häufiger zum Einsatz als Arrays. Eine Liste wird so deklariert:
            // List<datatype> <varname> = new List<datatype>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            List<int> z = new List<int> { 9000, 1000, 1337 };
            // Die <> kennzeichnen "Generics", mehr dazu unter "Coole Sachen"

            // Listen haben keinen Default-Wert.
            // Bevor auf einen Index zugegriffen werden kann, muss dieser
            // auch gesetzt worden sein:
            intList.Add(1);
            Console.WriteLine("intList @ 0: " + intList[0]);

            // Andere interessante Datenstrukturen sind:
            // Stack/Queue
            // Dictionary (entspricht einer Hash Map)
            // HashSet
            // Read-only Collections
            // Tuple (.Net 4+)

            ///////////////////////////////////////
            // Operatoren
            ///////////////////////////////////////
            Console.WriteLine("\n->Operatoren");

            // kurze Schreibweise um mehrere Deklarationen zusammenzufassen:
            // (Benutzung vom C# Styleguide aber ausdrücklich abgeraten!)
            int i1 = 1, i2 = 2;

            // Arithmetik funktioniert wie erwartet:
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // Modulo
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // Vergleiche
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // Bitweise Operatoren
            /*
            ~       Unäres bitweises NICHT
            <<      Verschieben nach links
            >>      Verschieben nach rechts
            &       Bitweises UND
            ^       Bitweises exklusives ODER
            |       Bitweises inklusives ODER
            */

            // Inkremente
            int i = 0;
            Console.WriteLine("\n->Inkrement / Dekrement");
            Console.WriteLine(i++); //i = 1. Post-Inkrement
            Console.WriteLine(++i); //i = 2. Pre-Inkrement
            Console.WriteLine(i--); //i = 1. Post-Dekrement
            Console.WriteLine(--i); //i = 0. Pre-Dekrement

            ///////////////////////////////////////
            // Kontrollstrukturen
            ///////////////////////////////////////
            Console.WriteLine("\n->Kontrollstrukturen");

            // If-Statements funktionieren wie in C
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("Ich werde ausgegeben");
            }
            else if (j > 10)
            {
                Console.WriteLine("Ich nicht");
            }
            else
            {
                Console.WriteLine("Ich leider auch nicht");
            }

            // Ternärer Operator
            // Anstatt eines einfachen if/else lässt sich auch folgendes schreiben:
            // <condition> ? <true> : <false>
            int zumVergleich = 17;
            string isTrue = zumVergleich == 17 ? "Ja" : "Nein";

            // while-Schleife
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                // Wird 100mal wiederholt, fooWhile 0->99
                fooWhile++;
            }

            // do-while-Schleife
            int fooDoWhile = 0;
            do
            {
                // Wird 100mal wiederholt, fooDoWhile 0->99
                fooDoWhile++;
            } while (fooDoWhile < 100);

            //for-Schleifen => for(<start_statement>; <conditional>; <step>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                // Wird 10mal wiederholt, fooFor 0->9
            }

            // foreach-Schleife
            // Die normale Syntax für eine foreach-Schleife lautet:
            // foreach(<iteratorType> <iteratorName> in <enumerable>)
            // foreach kann mit jedem Objekt verwendet werden das IEnumerable
            // oder IEnumerable<T> implementiert. Alle Auflistungs-Typen
            // (Array, List, Dictionary...) im .NET Framework implementieren
            // eines dieser beiden Interfaces.

            foreach (char character in "Hallo Welt".ToCharArray())
            {
                // Ein Durchgang für jedes Zeichen im String
            }
            // (ToCharArray() könnte man hier übrigens auch weglassen,
            // da String IEnumerable bereits implementiert)

            // Switch Struktur
            // Ein Switch funktioniert mit byte, short, char und int Datentypen.
            // Auch Aufzählungstypen können verwendet werden, genau wie
            // die Klasse String, und ein paar Sonderklassen, die Wrapper für
            // Primitives sind: Character, Byte, Short und Integer
            int month = 3;
            string monthString;
            switch (month)
            {
                case 1:
                    monthString = "Januar";
                    break;
                case 2:
                    monthString = "Februar";
                    break;
                case 3:
                    monthString = "März";
                    break;
                // Man kann für mehrere Fälle auch das selbe Verhalten
                // definieren. Jeder Block muss aber mit einem break-Statement
                // abgeschlossen werden. Einzelne Fälle können über
                // `goto case x` erreicht werden
                case 6:
                case 7:
                case 8:
                    monthString = "Sommer!!";
                    break;
                default:
                    monthString = "Irgendein anderer Monat";
                    break;
            }

            ///////////////////////////////////////
            // Umwandlung von Datentypen und Typecasting
            ///////////////////////////////////////

            // Umwandlung

            // von String nach Integer
            // bei einem Fehler wirft diese Code eine Exception
            int.Parse("123"); //gibt die Ganzzahl 123 zurück

            // TryParse gibt bei einem Fehler den Default-Wert zurück
            // (im Fall von int: 0)
            int tryInt;
            if (int.TryParse("123", out tryInt)) // gibt true oder false zurück
            {
                Console.WriteLine(tryInt);       // 123
            }

            // von Integer nach String
            // Die Klasse Convert stellt Methoden zur Konvertierung von
            // unterschiedlichsten Daten zur Verfügung:
            Convert.ToString(123); // "123"
            // oder
            tryInt.ToString(); // "123"
        }

        ///////////////////////////////////////
        // Klassen
        ///////////////////////////////////////
        public static void Classes()
        {

            // Benutze das new-Keyword um eine Instanz einer Klasse zu erzeugen
            Bicycle trek = new Bicycle();

            // So werden Methoden der Instanz aufgerufen
            trek.SpeedUp(3); // Es empfiehlt sich immer Getter und Setter zu benutzen
            trek.Cadence = 100;

            // ToString ist eine Konvention über die man üblicherweiser
            // Informationen über eine Instanz erhält
            Console.WriteLine("Infos zu trek: " + trek.ToString());

            // Wir instantiieren ein neues Hochrad
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("Infos zu funbike: " + funbike.ToString());

            Console.Read();
        } // Ende der Methode main

        // Main als Konsolenstartpunkt
        // Eine Konsolenanwendung muss eine Methode Main als Startpunkt besitzen
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        ///////////////////////////////////////
        // Interessante Features
        ///////////////////////////////////////

        // Methodensignaturen

        public // Sichtbarkeit
        static // Erlaubt einen Zugriff auf der Klasse (nicht auf einer Instanz)
        int // Typ des Rückgabewerts,
        MethodSignatures(
            // Erstes Argument, erwartet int
            int maxCount,
            // setzt sich selbst auf 0 wenn kein anderer Wert übergeben wird
            int count = 0,
            int another = 3,
            // enthält alle weiteren der Methode übergebenen Parameter (quasi Splats)
            params string[] otherParams
        )
        {
            return -1;
        }

        // Methoden können überladen werden, solange sie eindeutige
        // Signaturen haben
        public static void MethodSignatures(string maxCount)
        {
        }

        // Generische Typen
        // Die Typen für TKey und TValue werden erst beim Aufruf der Methode
        // festgelegt. Diese Methode emuliert z.B. SetDefault aus Python:
        public static TValue SetDefault<TKey, TValue>(
            IDictionary<TKey, TValue> dictionary,
            TKey key,
            TValue defaultItem)
        {
            TValue result;
            if (!dictionary.TryGetValue(key, out result))
            {
                return dictionary[key] = defaultItem;
            }
            return result;
        }

        // Möglichen Typen lassen sich auch über ihr Interface beschränken:
        public static void IterateAndPrint<T>(T toPrint) where T: IEnumerable<int>
        {
            // Da T ein IEnumerable ist können wir foreach benutzen
            foreach (var item in toPrint)
            {
                // Item ist ein int
                Console.WriteLine(item.ToString());
            }
        }

        public static void OtherInterestingFeatures()
        {
            // Optionale Parameter
            MethodSignatures(3, 1, 3, "Ein paar", "extra", "Strings");
            // setzt explizit einen bestimmten Parameter, andere werden übersprungen
            MethodSignatures(3, another: 3);

            // Erweiterungsmethoden
            int i = 3;
            i.Print(); // Weiter unten definiert

            // Nullables - perfekt für die Interaktion mit
            // Datenbanken / Rückgabewerten
            // Jeder Wert (d.h. keine Klassen) kann durch das Nachstellen eines ?
            // nullable gemacht werden: <type>? <varname> = <value>
            int? nullable = null; // Die explizite Langform wäre Nullable<int>
            Console.WriteLine("Mein Nullable: " + nullable);
            bool hasValue = nullable.HasValue; // true wenn nicht null

            // ?? ist "syntaktischer Zucker" um einen Defaultwert für den Fall
            // dass die Variable null ist festzulegen.
            int notNullable = nullable ?? 0; // 0

            // Implizit typisierte Variablen
            // Man kann auch den Typ einer Variable auch vom Compiler
            // bestimmen lassen:
            var magic = "magic ist zur Compile-Zeit ein String, folglich geht keine Typsicherheit verloren";
            magic = 9; // funktioniert nicht da magic vom Typ String ist

            // Generics
            var phonebook = new Dictionary<string, string>() {
                {"Resi", "08822 / 43 67"} // Fügt einen Eintrag zum Telefonbuch hinzu
            };

            // Hier könnte man auch unser generisches SetDefault von
            // weiter oben benutzen:
            Console.WriteLine(SetDefault<string,string>(phonebook, "Xaver", "kein Telefon")); // kein Telefon
            // TKey und TValue müssen nicht zwingend angegeben werden, da sie
            // auch implizit vom Compiler ermittelt werden können
            Console.WriteLine(SetDefault(phonebook, "Resi", "kein Telefon")); // 08822 / 43 67

            // Lambdas - konzise Syntax für Inline-Funktionen
            Func<int, int> square = (x) => x * x; // Das letzte Element vom Typ T ist der Rückgabewert
            Console.WriteLine(square(3)); // 9

            // Disposables - einfaches Management von nicht verwalteten Ressourcen
            // So gut wie alle Objekte die auf nicht verwaltete Ressourcen
            // (Dateien, Geräte, ...) zugreifen, implementieren das Interface
            // IDisposable. Das using Statement stellt sicher dass die vom
            // IDisposable benutzten Ressourcen nach der Benutzung wieder
            // freigegeben werden:
            using (StreamWriter writer = new StreamWriter("log.txt"))
            {
                writer.WriteLine("Alles bestens!");
                // Am Ende des Codeblocks werden die Ressourcen wieder
                // freigegeben - auch im Falle einer Exception
            }

            // Parallel Klasse
            // http://blogs.msdn.com/b/csharpfaq/archive/2010/06/01/parallel-programming-in-net-framework-4-getting-started.aspx
            var websites = new string[] {
                "http://www.google.com", "http://www.reddit.com",
                "http://www.shaunmccarthy.com"
            };
            var responses = new Dictionary<string, string>();

            // Für jeden Request wird ein neuer Thread erzeugt, der nächste
            // Schritt wird erst nach Beendigung aller Tasks ausgeführt
            Parallel.ForEach(websites,
                // maximal 3 Threads gleichzeitig
                new ParallelOptions() {MaxDegreeOfParallelism = 3},
                website =>
            {
                // Hier folgt eine langwierige, asynchrone Operation
                using (var r = WebRequest.Create(new Uri(website)).GetResponse())
                {
                    responses[website] = r.ContentType;
                }
            });

            // Dieser Code wird erst nach Beendigung aller Requests ausgeführt
            foreach (var key in responses.Keys)
            {
                Console.WriteLine("{0}:{1}", key, responses[key]);
            }

            // Dynamische Objekte (gut um mit anderen Sprachen zu arbeiten)
            dynamic student = new ExpandoObject();
            // hier muss keine Typ angegeben werden
            student.FirstName = "Christian";

            // Einem solchen Objekt kann man sogar Methoden zuordnen.
            // Das Beispiel gibt einen String zurück und erwartet einen String
            student.Introduce = new Func<string, string>(
                (introduceTo) => string.Format("Hallo {0}, das ist {1}", student.FirstName, introduceTo));
            Console.WriteLine(student.Introduce("Bettina"));

            // IQueryable<T> - So gut wie alle Aufzählungstypen implementieren
            // dieses Interface, welches eine Vielzahl von funktionalen Methoden
            // wie Map / Filter / Reduce zur Verfügung stellt:
            var bikes = new List<Bicycle>();
            // sortiert die Liste
            bikes.Sort();
            // sortiert nach Anzahl Räder
            bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels));
            var result = bikes
                // diese Filter können auch aneinandergehängt werden
                .Where(b => b.Wheels > 3) // (gibt ein IQueryable des vorherigen Typs zurück)
                .Where(b => b.IsBroken && b.HasTassles)
                // diese Zuordnung gibt ein IQueryable<String> zurück
                .Select(b => b.ToString());

            // "Reduce" - addiert alle Räder der Aufzählung zu einem Wert
            var sum = bikes.Sum(b => b.Wheels);

            // So erzeugt man ein implizit typisiertes Objekt, basierend auf
            // den Parametern der Elemente:
            var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
            // Auch wenn wir es hier nicht demonstrieren können:
            // In einer IDE wie VisualStudio kriegen wir hier sogar TypeAhead,
            // da der Compiler in der Lage ist, die passenden Typen zu erkennen.
            foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
            {
                Console.WriteLine(bikeSummary.Name);
            }

            // AsParallel-Methode
            // Jetzt kommen die Schmankerl! Die AsParallel-Methode kombiniert
            // LINQ und parallele Operationen:
            var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
            // Diese Berechnung passiert parallel! Benötigte Threads werden
            // automatisch erzeugt, und die Rechenlast unter ihnen aufgeteilt.
            // Ein Traum für die Verarbeitung von großen Datenmengen
            // auf mehreren Cores!

            // LINQ - bildet einen Datenspeicher auf IQueryable<T> Objekte ab
            // LinqToSql beispielsweise speichert und liest aus einer
            // SQL-Datenbank, LinqToXml aus einem XML-Dokument.
            // LINQ-Operationen werden "lazy" ausgeführt.
            var db = new BikeRepository();

            // Die verzögerte Ausführung ist optimal für Datenbankabfragen
            var filter = db.Bikes.Where(b => b.HasTassles); // noch keine Abfrage
            // Es können noch mehr Filter hinzugefügt werden (auch mit
            // Bedingungen) - ideal für z.B. "erweiterte Suchen"
            if (42 > 6)
            {
                filter = filter.Where(b => b.IsBroken); // immer noch keine Abfrage
            }

            var query = filter
                .OrderBy(b => b.Wheels)
                .ThenBy(b => b.Name)
                .Select(b => b.Name); // auch hier: immer noch keine Abfrage

            // Erst hier wird die Datenbankabfrage wirklich ausgeführt,
            // limitiert auf die Elemente die der foreach-Loop verwendet
            foreach (string bike in query)
            {
                Console.WriteLine(result);
            }

        }

    } // Ende der Klasse LearnCSharp

    // Eine .cs-Datei kann auch mehrere Klassen enthalten

    public static class Extensions
    {
        // Erweiterungsmethoden
        public static void Print(this object obj)
        {
            Console.WriteLine(obj.ToString());
        }
    }

    // Syntax zur Deklaration einer Klasse:
    // <public/private/protected/internal> class <class name>{
    //    // Datenfelder, Konstruktoren und Methoden leben alle
    //    // innerhalb dieser Deklaration
    // }

    public class Bicycle
    {
        // Felder/Variablen der Klasse "Bicycle"
        // Das Keyword public macht das Member von überall zugänglich
        public int Cadence
        {
            get // get definiert eine Methode um die Eigenschaft abzurufen
            {
                return _cadence;
            }
            set // set definiert eine Methode um die Eigenschaft zu setzen
            {
                _cadence = value; // value ist der dem Setter übergebene Wert
            }
        }
        private int _cadence;

        // Das Keyword protected macht das Member nur für die Klasse selbst
        // und ihre Subklassen zugänglich
        protected virtual int Gear
        {
            get; // erzeugt eine Eigenschaft für die kein "Zwischenwert" benötigt wird
            set;
        }

        // Das Keyword internal macht das Member innerhalb der Assembly zugänglich
        internal int Wheels
        {
            get;
            private set; // get/set kann auch über Keywords modifiziert werden
        }

        int _speed; // Member ohne vorangestellte Keywords sind standardmäßig
                    // private, sie sind nur innerhalb der Klasse zugänglich.
                    // Man kann aber natürlich auch das Keyword private benutzen.
        private string Name { get; set; }

        // Ein Enum ist ein klar definierter Satz an benannten Konstanten.
        // Eigentlich ordnet es diese Konstanten nur bestimmten Werten zu
        // (einer int-Zahl, solange nicht anders angegeben). Mögliche Typen für
        // die Werte eines Enums sind byte, sbyte, short, ushort, int, uint,
        // long, oder ulong. Alle Werte in einem Enum sind eindeutig.
        public enum BikeBrand
        {
            Colnago,
            EddyMerckx,
            Bianchi = 42, // so kann man den Wert explizit setzen
            Kynast // 43
        }
        // Nachdem dieser Typ in der Klasse "Bicycle" definiert ist,
        // sollte Code ausserhalb der Klasse den Typen als Bicycle.Brand referenzieren

        // Nachdem das Enum deklariert ist, können wir den Typen verwenden:
        public BikeBrand Brand;

        // Als static gekennzeichnete Member gehören dem Typ selbst,
        // nicht seinen Instanzen. Man kann sie also ohne Referenz zu einem
        // Objekt benutzen
        // Console.WriteLine("Schon " + Bicycle.BicyclesCreated + " Fahrräder, nur für dieses Tutorial!");
        static public int BicyclesCreated = 0;

        // readonly-Werte werden zur Laufzeit gesetzt
        // Ihr Wert kann nur bei ihrer Deklaration, oder in einem Konstruktor
        // festgelegt werden
        readonly bool _hasCardsInSpokes = false; // readonly und private

        // Konstruktoren bestimmen was bei einer Instantiierung passiert.
        // Das ist ein Default-Konstruktor:
        public Bicycle()
        {
            // Member der Klasse können über das Keyword this erreicht werden
            this.Gear = 1;
            // oft ist das aber gar nicht nötig
            Cadence = 50;
            _speed = 5;
            Name = "Bonanzarad";
            Brand = BikeBrand.Kynast;
            BicyclesCreated++;
        }

        // Das ist ein spezifischer Konstruktor (d.h. er erwartet Argumente):
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand)
            : base() // ruft zuerst den "base"-Konstruktor auf
        {
            Gear = startGear;
            Cadence = startCadence;
            _speed = startSpeed;
            Name = name;
            _hasCardsInSpokes = hasCardsInSpokes;
            Brand = brand;
        }

        // Konstruktoren können aneinandergehängt werden:
        public Bicycle(int startCadence, int startSpeed, BikeBrand brand) :
            this(startCadence, startSpeed, 0, "richtig große Räder", true, brand)
        {
        }

        // Syntax für Methoden:
        // <public/private/protected> <return type> <function name>(<args>)

        // Klassen können Getter und Setter für Werte definieren,
        // oder diese Werte direkt als Eigenschaft implementieren
        // (in C# der bevorzugte Weg)

        // Parameter von Methoden können Default-Werte haben.
        // "SpeedUp" kann man also auch ohne Parameter aufrufen:
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // Eigenschaften mit get/set
        // wenn es nur um den Zugriff auf Daten geht, ist eine Eigenschaft zu
        // empfehlen. Diese können Getter und Setter haben, oder auch nur
        // einen Getter bzw. einen Setter
        private bool _hasTassles; // private Variable
        public bool HasTassles // öffentliches Interface
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // Das kann man auch kürzer schreiben:
        // Dieser Syntax erzeugt automatisch einen hinterlegten Wert,
        // (entsprechend `private bool _isBroken`) der gesetzt
        // bzw. zurückgegeben wird:
        public bool IsBroken { get; private set; }
        public int FrameSize
        {
            get;
            // für Getter und Setter kann der Zugriff auch einzeln
            // beschränkt werden, FrameSize kann also nur von innerhalb
            // der Klasse "Bicycle" gesetzt werden
            private set;
        }

        // Diese Methode gibt eine Reihe an Informationen über das Objekt aus:
        public virtual string ToString()
        {
            return "Gang: " + Gear +
                    " Kadenz: " + Cadence +
                    " Geschwindigkeit: " + _speed +
                    " Name: " + Name +
                    " Hipster-Karten zwischen den Speichen: " + (_hasCardsInSpokes ? "Na klar!" : "Bloß nicht!") +
                    "\n------------------------------\n"
                    ;
        }

        // Auch Methoden können als static gekennzeichnet werden, nützlich
        // beispielsweise für Helper-Methoden
        public static bool DidWeCreateEnoughBicyclesYet()
        {
            // In einer statischen Methode können wir natürlich auch nur
            // statische Member der Klasse referenzieren
            return BicyclesCreated > 9000;
        }
        // Wenn eine Klasse nur statische Member enthält, kann es eine gute Idee
        // sein die Klasse selbst als static zu kennzeichnen

    } // Ende der Klasse "Bicycle"

    // "PennyFarthing" ist eine Unterklasse von "Bicycle"
    class PennyFarthing : Bicycle
    {
        // (Hochräder - englisch Penny Farthing - sind diese antiken Fahrräder
        // mit riesigem Vorderrad. Sie haben keine Gangschaltung.)

        // hier wird einfach der Elternkonstruktor aufgerufen
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "Hochrad", true, BikeBrand.EddyMerckx)
        {
        }

        protected override int Gear
        {
            get
            {
                return 0;
            }
            set
            {
                throw new ArgumentException("Ein Hochrad hat keine Gangschaltung, doh!");
            }
        }

        public override string ToString()
        {
            string result = "Hochrad ";
            result += base.ToString(); // ruft die "base"-Version der Methode auf
            return result;
        }
    }

    // Interfaces (auch Schnittstellen genant) definieren nur die Signaturen
    // ihrer Member, enthalten aber auf keinen Fall ihre Implementierung:
    interface IJumpable
    {
        // Alle Member eines Interfaces sind implizit public
        void Jump(int meters);
    }

    interface IBreakable
    {
        // Interfaces können Eigenschaften, Methoden und Events definieren
        bool Broken { get; }
    }

    // Eine Klasse kann nur von einer Klasse erben, kann aber eine beliebige
    // Anzahl von Interfaces implementieren
    class MountainBike : Bicycle, IJumpable, IBreakable
    {
        int damage = 0;

        public void Jump(int meters)
        {
            damage += meters;
        }

        public bool Broken
        {
            get
            {
                return damage > 100;
            }
        }
    }

    // Das hier stellt eine Datenbankverbindung für das LinqToSql-Beispiel her.
    // EntityFramework Code First ist großartig
    // (ähnlich zu Ruby's ActiveRecord, aber bidirektional)
    // http://msdn.microsoft.com/de-de/data/jj193542.aspx
    public class BikeRepository : DbSet
    {
        public BikeRepository()
            : base()
        {
        }

        public DbSet<Bicycle> Bikes { get; set; }
    }
} // Ende des Namespaces
```

## In dieser Übersicht nicht enthalten sind die Themen:

 * Flags
 * Attributes
 * Statische Eigenschaften
 * Exceptions, Abstraction
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)

## Zum Weiterlesen gibt es viele gute Anlaufpunkte:

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/en-us/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/overview/exploring-webmatrix)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)

[C# Coding Conventions](http://msdn.microsoft.com/de-de/library/vstudio/ff926074.aspx)
