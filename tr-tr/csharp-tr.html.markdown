---
language: c#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
    - ["Melih Mucuk", "http://melihmucuk.com"]
filename: LearnCSharp.cs
---

C# zarif ve tip güvenli nesne yönelimli bir dil olup geliştiricilerin .NET framework üzerinde çalışan güçlü ve güvenli uygulamalar geliştirmesini sağlar.

[Daha fazlasını okuyun.](http://msdn.microsoft.com/en-us/library/vstudio/z1zx9t92.aspx)

```c#
// Tek satırlık yorumlar // ile başlar 
/*
Birden fazla satırlı yorumlar buna benzer
*/
/// <summary>
/// Bu bir XML dokümantasyon yorumu
/// </summary>

// Uygulamanın kullanacağı ad alanlarını belirtin
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Dynamic;
using System.Linq;
using System.Linq.Expressions;
using System.Net;
using System.Threading.Tasks;
using System.IO;

// Kodu düzenlemek için paketler içinde alan tanımlayın
namespace Learning
{
    // Her .cs dosyası, dosya ile aynı isimde en az bir sınıf içermeli
    // bu kurala uymak zorunda değilsiniz ancak mantıklı olan yol budur.
    public class LearnCSharp
    {
        // TEMEL SÖZ DİZİMİ - daha önce Java ya da C++ kullandıysanız İLGİNÇ ÖZELLİKLER'e geçin
        public static void Syntax() 
        {
            // Satırları yazdırmak için Console.WriteLine kullanın
            Console.WriteLine("Merhaba Dünya");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

            // Yeni satıra geçmeden yazdırmak için Console.Write kullanın
            Console.Write("Merhaba ");
            Console.Write("Dünya");

            ///////////////////////////////////////////////////
            // Tipler & Değişkenler
            //
            // Bir değişken tanımlamak için <tip> <isim> kullanın
            ///////////////////////////////////////////////////

            // Sbyte - Signed 8-bit integer
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - Unsigned 8-bit integer
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - 16-bit integer
            // Signed - (-32,768 <= short <= 32,767)
            // Unsigned - (0 <= ushort <= 65,535)
            short fooShort = 10000;
            ushort fooUshort = 10000;

            // Integer - 32-bit integer
            int fooInt = 1; // (-2,147,483,648 <= int <= 2,147,483,647)
            uint fooUint = 1; // (0 <= uint <= 4,294,967,295)

            // Long - 64-bit integer
            long fooLong = 100000L; // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            ulong fooUlong = 100000L; // (0 <= ulong <= 18,446,744,073,709,551,615)
            // Sayılar boyutlarına göre ön tanımlı olarak int ya da uint olabilir.
            // L, değişken değerinin long ya da ulong tipinde olduğunu belirtmek için kullanılır.

            // Double - Çift hassasiyetli 64-bit IEEE 754 kayan sayı
            double fooDouble = 123.4; // Hassasiyet: 15-16 basamak

            // Float - Tek hassasiyetli 32-bit IEEE 754 kayan sayı
            float fooFloat = 234.5f; // Hassasiyet: 7 basamak
            // f, değişken değerinin float tipinde olduğunu belirtmek için kullanılır.

            // Decimal - 128-bit veri tiğinde ve diğer kayan sayı veri tiplerinden daha hassastır,
            // finansal ve mali hesaplamalar için uygundur.
            decimal fooDecimal = 150.3m;

            // Boolean - true & false
            bool fooBoolean = true; // veya false

            // Char - 16-bitlik tek bir unicode karakter
            char fooChar = 'A';

            // Strings -- Önceki baz tiplerinin hepsi değer tipiyken,
            // string bir referans tipidir. Null değer atayabilirsiniz
            string fooString = "\"escape\" quotes and add \n (new lines) and \t (tabs)";
            Console.WriteLine(fooString);

            // İndeks numarası kullanarak bir string'in bütün karakterlerine erişilebilirsiniz: 
            char charFromString = fooString[1]; // => 'e'
            // String'ler değiştirilemez: fooString[1] = 'X' işlemini yapamazsınız;

            // String'leri geçerli kültür değeri ve büyük küçük harf duyarlılığı olmadan karşılaştırma
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // sprintf baz alınarak formatlama
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // Tarihler & Formatlama
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // Bir string'i iki satıra bölmek için @ sembolü kullanabilirsiniz. " işaretinden kaçmak için "" kullanın
            string bazString = @"Here's some stuff
on a new line! ""Wow!"", the masses cried";

            // Bir değişkeni değiştirilemez yapmak için const ya da read-only kullanın. 
            // const değerleri derleme sırasında hesaplanır
            const int HOURS_I_WORK_PER_WEEK = 9001;

            ///////////////////////////////////////////////////
            // Veri Yapıları
            ///////////////////////////////////////////////////

            // Diziler - Sıfır indeksli
            // Dizi boyutuna tanımlama sırasında karar verilmelidir.
            // Dizi tanımlama formatı şöyledir:
            // <veri tipi>[] <değişken ismi> = new <veri tipi>[<dizi boyutu>];
            int[] intArray = new int[10];

            // Bir diğer dizi tanımlama formatı şöyledir:
            int[] y = { 9000, 1000, 1337 };

            // Bir diziyi indeksleme - Bir elemente erişme
            Console.WriteLine("intArray @ 0: " + intArray[0]);
            // Diziler değiştirilebilir.
            intArray[1] = 1;

            // Listeler
            // Listeler daha esnek oldukları için dizilerden daha sık kullanılırlar.
            // Bir liste tanımlama formatı şöyledir:
            // List<veri tipi> <değişken ismi> = new List<veri tipi>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            List<int> z = new List<int> { 9000, 1000, 1337 }; // tanımlama
            // <> işareti genelleme içindir - Güzel özellikler sekmesini inceleyin

            // Listelerin varsayılan bir değeri yoktur;
            // İndekse erişmeden önce değer eklenmiş olmalıdır
            intList.Add(1);
            Console.WriteLine("intList @ 0: " + intList[0]);

            // Diğer veri yapıları için şunlara bakın:
            // Stack/Queue (Yığın/Kuyruk)
            // Dictionary (hash map'in uygulanması) (Sözlük)
            // HashSet (karma seti)
            // Read-only Collections (Değiştirilemez koleksiyonlar)
            // Tuple (.Net 4+) (tüp)

            ///////////////////////////////////////
            // Operatörler
            ///////////////////////////////////////
            Console.WriteLine("\n->Operators");

            int i1 = 1, i2 = 2; // Birden çok tanımlamanın kısa yolu

            // Aritmetik basittir
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // Mod
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // Karşılaştırma operatörleri
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // Bit düzeyi operatörleri!
            /*
            ~       Tekli bit tamamlayıcısı
            <<      Sola kaydırma Signed left shift
            >>      Sağa kaydırma Signed right shift
            &       Bit düzeyi AND
            ^       Bit düzeyi harici OR
            |       Bit düzeyi kapsayan OR
            */

            // Arttırma
            int i = 0;
            Console.WriteLine("\n->Inc/Dec-rementation");
            Console.WriteLine(i++); //i = 1. Post-Incrementation
            Console.WriteLine(++i); //i = 2. Pre-Incrementation
            Console.WriteLine(i--); //i = 1. Post-Decrementation
            Console.WriteLine(--i); //i = 0. Pre-Decrementation

            ///////////////////////////////////////
            // Kontrol Yapıları
            ///////////////////////////////////////
            Console.WriteLine("\n->Control Structures");

            // If ifadesi c benzeridir
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

            // Üçlü operatörler
            // Basit bir if/else ifadesi şöyle yazılabilir
            // <koşul> ? <true> : <false>
            string isTrue = (true) ? "True" : "False";

            // While döngüsü
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                //100 kere tekrarlanır, fooWhile 0->99
                fooWhile++;
            }

            // Do While Döngüsü
            int fooDoWhile = 0;
            do
            {
                //100 kere tekrarlanır, fooDoWhile 0->99
                fooDoWhile++;
            } while (fooDoWhile < 100);

            //for döngüsü yapısı => for(<başlangıç ifadesi>; <koşul>; <adım>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                //10 kere tekrarlanır, fooFor 0->9
            }

            // For Each Döngüsü
            // foreach döngüsü yapısı => foreach(<yineleyici tipi> <yineleyici ismi> in <enumerable>)
            // foreach döngüsü, IEnumerable ya da IEnumerable<T> e dönüştürülmüş herhangi bir obje üzerinde döngü yapabilir
            // .Net framework üzerindeki bütün koleksiyon tiplerinden (Dizi, Liste, Sözlük...)
            // biri ya da hepsi uygulanarak gerçekleştirilebilir.
            // (ToCharArray() silindi, çünkü string'ler aynı zamanda IEnumerable'dır.)
            foreach (char character in "Hello World".ToCharArray())
            {
                //String içindeki bütün karakterler üzerinde döner
            }

            // Switch Case
            // Bir switch byte, short, char ve int veri tipleri ile çalışır.
            // Aynı zamanda sıralı tipler ilede çalışabilir.(Enum Tipleri bölümünde tartışıldı),
            // the String class, and a few special classes that wrap
            // primitive types: Character, Byte, Short, and Integer.
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
                // You can assign more than one case to an action
                // But you can't add an action without a break before another case
                // (if you want to do this, you would have to explicitly add a goto case x
                case 6:
                case 7:
                case 8:
                    monthString = "Summer time!!";
                    break;
                default:
                    monthString = "Some other month";
                    break;
            }

            ///////////////////////////////////////
            // Converting Data Types And Typecasting
            ///////////////////////////////////////

            // Converting data

            // Convert String To Integer
            // this will throw an Exception on failure
            int.Parse("123");//returns an integer version of "123"

            // try parse will default to type default on failure
            // in this case: 0
            int tryInt;
            if (int.TryParse("123", out tryInt)) // Function is boolean
                Console.WriteLine(tryInt);       // 123

            // Convert Integer To String
            // Convert class has a number of methods to facilitate conversions
            Convert.ToString(123);
            // or
            tryInt.ToString();
        }

        ///////////////////////////////////////
        // CLASSES - see definitions at end of file
        ///////////////////////////////////////
        public static void Classes()
        {
            // See Declaration of objects at end of file

            // Use new to instantiate a class
            Bicycle trek = new Bicycle();

            // Call object methods
            trek.SpeedUp(3); // You should always use setter and getter methods
            trek.Cadence = 100;

            // ToString is a convention to display the value of this Object.
            Console.WriteLine("trek info: " + trek.Info());

            // Instantiate a new Penny Farthing
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.Info());

            Console.Read();
        } // End main method

        // CONSOLE ENTRY A console application must have a main method as an entry point
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        //
        // INTERESTING FEATURES
        //
        
        // DEFAULT METHOD SIGNATURES

        public // Visibility
        static // Allows for direct call on class without object 
        int // Return Type,
        MethodSignatures(
            int maxCount, // First variable, expects an int
            int count = 0, // will default the value to 0 if not passed in
            int another = 3,
            params string[] otherParams // captures all other parameters passed to method
        )
        { 
            return -1;
        }

        // Methods can have the same name, as long as the signature is unique
        public static void MethodSignatures(string maxCount)
        {
        }

        // GENERICS
        // The classes for TKey and TValue is specified by the user calling this function.
        // This method emulates the SetDefault of Python
        public static TValue SetDefault<TKey, TValue>(
            IDictionary<TKey, TValue> dictionary, 
            TKey key, 
            TValue defaultItem)
        {
            TValue result;
            if (!dictionary.TryGetValue(key, out result))
                return dictionary[key] = defaultItem;
            return result;
        }

        // You can narrow down the objects that are passed in 
        public static void IterateAndPrint<T>(T toPrint) where T: IEnumerable<int>
        {
            // We can iterate, since T is a IEnumerable
            foreach (var item in toPrint)
                // Item is an int
                Console.WriteLine(item.ToString());
        }

        public static void OtherInterestingFeatures()
        {
            // OPTIONAL PARAMETERS
            MethodSignatures(3, 1, 3, "Some", "Extra", "Strings");
            MethodSignatures(3, another: 3); // explicity set a parameter, skipping optional ones

            // EXTENSION METHODS
            int i = 3;
            i.Print(); // Defined below

            // NULLABLE TYPES - great for database interaction / return values
            // any value type (i.e. not a class) can be made nullable by suffixing a ?
            // <type>? <var name> = <value>
            int? nullable = null; // short hand for Nullable<int>
            Console.WriteLine("Nullable variable: " + nullable);
            bool hasValue = nullable.HasValue; // true if not null

            // ?? is syntactic sugar for specifying default value (coalesce)
            // in case variable is null
            int notNullable = nullable ?? 0; // 0

            // IMPLICITLY TYPED VARIABLES - you can let the compiler work out what the type is:
            var magic = "magic is a string, at compile time, so you still get type safety";
            // magic = 9; will not work as magic is a string, not an int

            // GENERICS
            //
            var phonebook = new Dictionary<string, string>() { 
                {"Sarah", "212 555 5555"} // Add some entries to the phone book
            };

            // Calling SETDEFAULT defined as a generic above
            Console.WriteLine(SetDefault<string,string>(phonebook, "Shaun", "No Phone")); // No Phone
            // nb, you don't need to specify the TKey and TValue since they can be 
            // derived implicitly
            Console.WriteLine(SetDefault(phonebook, "Sarah", "No Phone")); // 212 555 5555

            // LAMBDA EXPRESSIONS - allow you to write code in line
            Func<int, int> square = (x) => x * x; // Last T item is the return value
            Console.WriteLine(square(3)); // 9

            // DISPOSABLE RESOURCES MANAGEMENT - let you handle unmanaged resources easily.
            // Most of objects that access unmanaged resources (file handle, device contexts, etc.)
            // implement the IDisposable interface. The using statement takes care of 
            // cleaning those IDisposable objects for you.
            using (StreamWriter writer = new StreamWriter("log.txt"))
            {
                writer.WriteLine("Nothing suspicious here");
                // At the end of scope, resources will be released.
                // Even if an exception is thrown.
            } 

            // PARALLEL FRAMEWORK
            // http://blogs.msdn.com/b/csharpfaq/archive/2010/06/01/parallel-programming-in-net-framework-4-getting-started.aspx
            var websites = new string[] { 
                "http://www.google.com", "http://www.reddit.com", 
                "http://www.shaunmccarthy.com"
            };
            var responses = new Dictionary<string, string>();
            
            // Will spin up separate threads for each request, and join on them
            // before going to the next step!
            Parallel.ForEach(websites, 
                new ParallelOptions() {MaxDegreeOfParallelism = 3}, // max of 3 threads
                website =>
            {
                // Do something that takes a long time on the file
                using (var r = WebRequest.Create(new Uri(website)).GetResponse())
                {
                    responses[website] = r.ContentType;
                }
            });

            // This won't happen till after all requests have been completed
            foreach (var key in responses.Keys)
                Console.WriteLine("{0}:{1}", key, responses[key]);

            // DYNAMIC OBJECTS (great for working with other languages)
            dynamic student = new ExpandoObject();
            student.FirstName = "First Name"; // No need to define class first!

            // You can even add methods (returns a string, and takes in a string)
            student.Introduce = new Func<string, string>(
                (introduceTo) => string.Format("Hey {0}, this is {1}", student.FirstName, introduceTo));
            Console.WriteLine(student.Introduce("Beth"));

            // IQUERYABLE<T> - almost all collections implement this, which gives you a lot of 
            // very useful Map / Filter / Reduce style methods
            var bikes = new List<Bicycle>();
            bikes.Sort(); // Sorts the array
            bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels)); // Sorts based on wheels
            var result = bikes
                .Where(b => b.Wheels > 3) // Filters - chainable (returns IQueryable of previous type)
                .Where(b => b.IsBroken && b.HasTassles)
                .Select(b => b.ToString()); // Map - we only this selects, so result is a IQueryable<string>

            var sum = bikes.Sum(b => b.Wheels); // Reduce - sums all the wheels in the collection

            // Create a list of IMPLICIT objects based on some parameters of the bike
            var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
            // Hard to show here, but you get type ahead completion since the compiler can implicitly work
            // out the types above!
            foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
                Console.WriteLine(bikeSummary.Name);

            // ASPARALLEL
            // And this is where things get wicked - combines linq and parallel operations
            var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
            // this will happen in parallel! Threads will automagically be spun up and the 
            // results divvied amongst them! Amazing for large datasets when you have lots of 
            // cores

            // LINQ - maps a store to IQueryable<T> objects, with delayed execution
            // e.g. LinqToSql - maps to a database, LinqToXml maps to an xml document
            var db = new BikeRepository();

            // execution is delayed, which is great when querying a database
            var filter = db.Bikes.Where(b => b.HasTassles); // no query run
            if (42 > 6) // You can keep adding filters, even conditionally - great for "advanced search" functionality
                filter = filter.Where(b => b.IsBroken); // no query run

            var query = filter
                .OrderBy(b => b.Wheels)
                .ThenBy(b => b.Name)
                .Select(b => b.Name); // still no query run

            // Now the query runs, but opens a reader, so only populates are you iterate through
            foreach (string bike in query) 
                Console.WriteLine(result);
            


        }

    } // End LearnCSharp class

    // You can include other classes in a .cs file

    public static class Extensions
    {
        // EXTENSION FUNCTIONS
        public static void Print(this object obj)
        {
            Console.WriteLine(obj.ToString());
        }
    }

    // Class Declaration Syntax:
    // <public/private/protected/internal> class <class name>{
    //    //data fields, constructors, functions all inside.
    //    //functions are called as methods in Java.
    // }

    public class Bicycle
    {
        // Bicycle's Fields/Variables
        public int Cadence // Public: Can be accessed from anywhere
        {
            get // get - define a method to retrieve the property
            {
                return _cadence;
            }
            set // set - define a method to set a proprety
            {
                _cadence = value; // Value is the value passed in to the setter
            }
        }
        private int _cadence;

        protected virtual int Gear // Protected: Accessible from the class and subclasses
        {
            get; // creates an auto property so you don't need a member field
            set;
        }

        internal int Wheels // Internal: Accessible from within the assembly
        {
            get;
            private set; // You can set modifiers on the get/set methods
        }

        int _speed; // Everything is private by default: Only accessible from within this class. 
                    // can also use keyword private
        public string Name { get; set; }

        // Enum is a value type that consists of a set of named constants
        // It is really just mapping a name to a value (an int, unless specified otherwise).
        // The approved types for an enum are byte, sbyte, short, ushort, int, uint, long, or ulong.
        // An enum can't contain the same value twice.
        public enum BikeBrand
        {
            AIST,
            BMC,
            Electra = 42, //you can explicitly set a value to a name
            Gitane // 43
        }
        // We defined this type inside a Bicycle class, so it is a nested type
        // Code outside of this class should reference this type as Bicycle.Brand

        public BikeBrand Brand; // After declaring an enum type, we can declare the field of this type

        // Static members belong to the type itself rather then specific object.
        // You can access them without a reference to any object:
        // Console.WriteLine("Bicycles created: " + Bicycle.bicyclesCreated);
        static public int BicyclesCreated = 0;

        // readonly values are set at run time
        // they can only be assigned upon declaration or in a constructor
        readonly bool _hasCardsInSpokes = false; // read-only private

        // Constructors are a way of creating classes
        // This is a default constructor
        public Bicycle() 
        {
            this.Gear = 1; // you can access members of the object with the keyword this
            Cadence = 50;  // but you don't always need it
            _speed = 5;
            Name = "Bontrager";
            Brand = BikeBrand.AIST;
            BicyclesCreated++;
        }

        // This is a specified constructor (it contains arguments)
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand) 
            : base() // calls base first
        {
            Gear = startGear; 
            Cadence = startCadence;
            _speed = startSpeed;
            Name = name; 
            _hasCardsInSpokes = hasCardsInSpokes;
            Brand = brand;
        }

        // Constructors can be chained
        public Bicycle(int startCadence, int startSpeed, BikeBrand brand) :
            this(startCadence, startSpeed, 0, "big wheels", true, brand)
        {
        }

        // Function Syntax:
        // <public/private/protected> <return type> <function name>(<args>)

        // classes can implement getters and setters for their fields
        // or they can implement properties (this is the preferred way in C#)

        // Method parameters can have default values.
        // In this case, methods can be called with these parameters omitted
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // properties get/set values
        // when only data needs to be accessed, consider using properties.
        // properties may have either get or set, or both
        private bool _hasTassles; // private variable
        public bool HasTassles // public accessor
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // You can also define an automatic property in one line
        // this syntax will create a backing field automatically.
        // You can set an access modifier on either the getter or the setter (or both)
        // to restrict its access:
        public bool IsBroken { get; private set; }

        // Properties can be auto-implemented
        public int FrameSize
        {
            get;
            // you are able to specify access modifiers for either get or set
            // this means only Bicycle class can call set on Framesize
            private set;
        }

        // It's also possible to define custom Indexers on objects.
        // All though this is not entirely useful in this example, you
        // could do bicycle[0] which yields "chris" to get the first passenger or
        // bicycle[1] = "lisa" to set the passenger. (of this apparent quattrocycle)
        private string[] passengers = { "chris", "phil", "darren", "regina" }

        public string this[int i]
        {
            get {
                return passengers[i];
            }

            set {
                return passengers[i] = value;
            }
        }

        //Method to display the attribute values of this Object.
        public virtual string Info()
        {
            return "Gear: " + Gear +
                    " Cadence: " + Cadence +
                    " Speed: " + _speed +
                    " Name: " + Name +
                    " Cards in Spokes: " + (_hasCardsInSpokes ? "yes" : "no") +
                    "\n------------------------------\n"
                    ;
        }

        // Methods can also be static. It can be useful for helper methods
        public static bool DidWeCreateEnoughBycles()
        {
            // Within a static method, we only can reference static class members
            return BicyclesCreated > 9000;
        } // If your class only needs static members, consider marking the class itself as static.


    } // end class Bicycle

    // PennyFarthing is a subclass of Bicycle
    class PennyFarthing : Bicycle
    {
        // (Penny Farthings are those bicycles with the big front wheel.
        // They have no gears.)

        // calling parent constructor
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "PennyFarthing", true, BikeBrand.Electra)
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
                throw new ArgumentException("You can't change gears on a PennyFarthing");
            }
        }

        public override string Info()
        {
            string result = "PennyFarthing bicycle ";
            result += base.ToString(); // Calling the base version of the method
            return result;
        }
    }

    // Interfaces only contain signatures of the members, without the implementation.
    interface IJumpable
    {
        void Jump(int meters); // all interface members are implicitly public
    }

    interface IBreakable
    {
        bool Broken { get; } // interfaces can contain properties as well as methods & events
    }

    // Class can inherit only one other class, but can implement any amount of interfaces
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

    /// <summary>
    /// Used to connect to DB for LinqToSql example. 
    /// EntityFramework Code First is awesome (similar to Ruby's ActiveRecord, but bidirectional)
    /// http://msdn.microsoft.com/en-us/data/jj193542.aspx
    /// </summary>
    public class BikeRepository : DbSet
    {
        public BikeRepository()
            : base()
        {
        }

        public DbSet<Bicycle> Bikes { get; set; }
    }
} // End Namespace
```

## Topics Not Covered

 * Flags
 * Attributes
 * Static properties
 * Exceptions, Abstraction
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)

## Further Reading

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/en-us/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/tutorials)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)



[C# Coding Conventions](http://msdn.microsoft.com/en-us/library/vstudio/ff926074.aspx)
