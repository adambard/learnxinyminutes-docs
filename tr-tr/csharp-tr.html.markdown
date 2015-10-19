---
language: c#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
translators:
    - ["Melih Mucuk", "http://melihmucuk.com"]
lang: tr-tr
filename: LearnCSharp-tr.cs

---

C# zarif ve tip güvenli nesne yönelimli bir dil olup geliştiricilerin .NET framework üzerinde çalışan güçlü ve güvenli uygulamalar geliştirmesini sağlar.

[Yazım yanlışları ve öneriler için bana ulaşabilirsiniz](mailto:melihmucuk@gmail.com)

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
            // <> işareti generic ifadeler içindir - Güzel özellikler sekmesini inceleyin

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
            int toCompare = 17;
            string isTrue = toCompare == 17 ? "True" : "False";

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
            // Aynı zamanda sıralı tipler ile de çalışabilir.(Enum Tipleri bölümünde tartışıldı),
            // String sınıfı, ve bir kaç özel sınıf kaydırılır
            // basit tipler: Character, Byte, Short, and Integer.
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
                // Bir aksiyon için birden fazla durum atayabilirsiniz
                // Ancak, break olmadan yeni bir durum ekleyemezsiniz
                // (Eğer bunu yapmak istiyorsanız, goto komutu eklemek zorundasınız)
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
            // Veri Tipleri Dönüştürme ve Typecasting
            ///////////////////////////////////////

            // Veri Dönüştürme

            // String'i Integer'a Dönüştürme
            // bu başarısız olursa hata fırlatacaktır
            int.Parse("123");// "123" 'in Integer değerini döndürür

            // try parse hata durumunda değişkene varsayılan bir değer atamak için kullanılır
            // bu durumda: 0
            int tryInt;
            if (int.TryParse("123", out tryInt)) // Fonksiyon boolean'dır
                Console.WriteLine(tryInt);       // 123

            // Integer'ı String'e Dönüştürme
            // Convert sınıfı dönüştürme işlemini kolaylaştırmak için bir dizi metoda sahiptir
            Convert.ToString(123);
            // veya
            tryInt.ToString();
        }

        ///////////////////////////////////////
        // SINIFLAR - dosyanın sonunda tanımları görebilirsiniz
        ///////////////////////////////////////
        public static void Classes()
        {
            // Obje tanımlamalarını dosyanın sonunda görebilirsiniz

            // Bir sınıfı türetmek için new kullanın
            Bicycle trek = new Bicycle();

            // Obje metodlarını çağırma
            trek.SpeedUp(3); // Her zaman setter ve getter metodları kullanmalısınız
            trek.Cadence = 100;

            // ToString objenin değerini göstermek için kullanılır.
            Console.WriteLine("trek info: " + trek.Info());

            // Yeni bir Penny Farthing sınıfı türetmek
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.Info());

            Console.Read();
        } // Ana metodun sonu

        // KONSOLE BAŞLANGICI Bir konsol uygulaması başlangıç olarak mutlaka ana metod'a sahip olmalı
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        //
        // İLGİNÇ ÖZELLİKLER
        //
        
        // VARSAYILAN METOD TANIMLAMALARI

        public // Görünebilir
        static // Sınıf üzerinden obje türetmeden çağırılabilir
        int // Dönüş Tipi,
        MethodSignatures(
            int maxCount, // İlk değişken, int değer bekler
            int count = 0, // Eğer değer gönderilmezse varsayılan olarak 0 değerini alır
            int another = 3,
            params string[] otherParams // Metoda gönderilen diğer bütün parametreleri alır
        )
        { 
            return -1;
        }

        // Metodlar tanımlamalar benzersiz ise aynı isimleri alabilirler
        public static void MethodSignatures(string maxCount)
        {
        }

        // GENERIC'LER
        // TKey ve TValue değerleri kullanıcı tarafından bu fonksiyon çağırılırken belirtilir.
        // Bu metod Python'daki SetDefault'a benzer
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

        // Gönderilen objeleri daraltabilirsiniz
        public static void IterateAndPrint<T>(T toPrint) where T: IEnumerable<int>
        {
            // Eğer T IEnumerable ise tekrarlayabiliriz
            foreach (var item in toPrint)
                // Item bir int
                Console.WriteLine(item.ToString());
        }

        public static void OtherInterestingFeatures()
        {
            // İSTEĞE BAĞLI PARAMETRELER
            MethodSignatures(3, 1, 3, "Some", "Extra", "Strings");
            MethodSignatures(3, another: 3); // isteğe bağlı olanlar gönderilmedi

            // UZANTI METODLARI
            int i = 3;
            i.Print(); // Aşağıda tanımlandı

            // NULLABLE TYPES - veri tabanı işlemleri için uygun / return values
            // Herhangi bir değer tipi sonuna ? eklenerek nullable yapılabilir (sınıflar hariç)
            // <tip>? <değiken ismi> = <değer>
            int? nullable = null; // Nullable<int> için kısa yol
            Console.WriteLine("Nullable variable: " + nullable);
            bool hasValue = nullable.HasValue; // eğer null değilse true döner

            // ?? varsayılan değer belirlemek için söz dizimsel güzel bir özellik
            // bu durumda değişken null'dır
            int notNullable = nullable ?? 0; // 0

            // TİPİ BELİRTİLMEMİŞ DEĞİŞKENLER - compiler değişkenin tipini bilmeden çalışabilir:
            var magic = "magic is a string, at compile time, so you still get type safety";
            // magic = 9; string gibi çalışmayacaktır, bu bir int değil

            // GENERIC'LER
            //
            var phonebook = new Dictionary<string, string>() { 
                {"Sarah", "212 555 5555"} // Telefon rehberine bir kaç numara ekleyelim.
            };

            // Yukarıda generic olarak tanımlanan SETDEFAULT'u çağırma
            Console.WriteLine(SetDefault<string,string>(phonebook, "Shaun", "No Phone")); // Telefonu yok
            // TKey ve TValue tipini belirtmek zorunda değilsiniz
            Console.WriteLine(SetDefault(phonebook, "Sarah", "No Phone")); // 212 555 5555

            // LAMBDA IFADELERİ - satır içinde kod yazmanıza olanak sağlar
            Func<int, int> square = (x) => x * x; // Son T nesnesi dönüş değeridir
            Console.WriteLine(square(3)); // 9

            // TEK KULLANIMLIK KAYNAK YÖNETİMİ -  Yönetilemeyen kaynakların üstesinden kolayca gelebilirsiniz.
            // Bir çok obje yönetilemeyen kaynaklara (dosya yakalama, cihaz içeriği, vb.)
            // IDisposable arabirimi ile erişebilir. Using ifadesi sizin için IDisposable objeleri temizler.
            using (StreamWriter writer = new StreamWriter("log.txt"))
            {
                writer.WriteLine("Nothing suspicious here");
                // Bu bölümün sonunda kaynaklar temilenir.
                // Hata fırlatılmış olsa bile.
            } 

            // PARALEL FRAMEWORK
            // http://blogs.msdn.com/b/csharpfaq/archive/2010/06/01/parallel-programming-in-net-framework-4-getting-started.aspx
            var websites = new string[] { 
                "http://www.google.com", "http://www.reddit.com", 
                "http://www.shaunmccarthy.com"
            };
            var responses = new Dictionary<string, string>();
            
            // Her istek farklı bir thread de işlem görecek 
            // bir sonraki işleme geçmeden birleştirilecek.
            Parallel.ForEach(websites, 
                new ParallelOptions() {MaxDegreeOfParallelism = 3}, // en fazla 3 thread kullanmak için
                website =>
            {
                // Uzun sürecek bir işlem yapın
                using (var r = WebRequest.Create(new Uri(website)).GetResponse())
                {
                    responses[website] = r.ContentType;
                }
            });

            // Bütün istekler tamamlanmadan bu döndü çalışmayacaktır.
            foreach (var key in responses.Keys)
                Console.WriteLine("{0}:{1}", key, responses[key]);

            // DİNAMİK OBJELER (diğer dillerle çalışırken kullanmak için uygun)
            dynamic student = new ExpandoObject();
            student.FirstName = "First Name"; // Önce yeni bir sınıf tanımlamanız gerekmez!

            // Hatta metod bile ekleyebilirsiniz (bir string döner, ve bir string alır)
            student.Introduce = new Func<string, string>(
                (introduceTo) => string.Format("Hey {0}, this is {1}", student.FirstName, introduceTo));
            Console.WriteLine(student.Introduce("Beth"));

            // IQUERYABLE<T> - neredeyse bütün koleksiyonlar bundan türer, bu size bir çok
            // kullanışlı Map / Filter / Reduce stili metod sağlar.
            var bikes = new List<Bicycle>();
            bikes.Sort(); // Dizi sıralama
            bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels)); // Wheels baz alınarak sıralama
            var result = bikes
                .Where(b => b.Wheels > 3) // Filters- chainable (bir önceki tipin IQueryable'ını döner)
                .Where(b => b.IsBroken && b.HasTassles)
                .Select(b => b.ToString()); // Map - sadece bunu seçiyoruz, yani sonuç bir IQueryable<string> olacak

            var sum = bikes.Sum(b => b.Wheels); // Reduce - koleksiyonda bulunan bütün wheel değerlerinin toplamı

            // Bike içindeki bazı parametreleri baz alarak bir liste oluşturmak
            var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
            // Burada göstermek zor ama, compiler yukaridaki tipleri çözümleyebilirse derlenmeden önce tipi verebilir.
            foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
                Console.WriteLine(bikeSummary.Name);

            // ASPARALLEL
            // Linq ve paralel işlemlerini birleştirme
            var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
            // bu paralel bir şekilde gerçekleşecek! Threadler otomatik ve sihirli bir şekilde işleri paylaşacak! 
            // Birden fazla çekirdeğiniz varsa büyük veri setleri ile kullanmak için oldukça uygun bir yapı.

            // LINQ - IQueryable<T> objelerini mapler ve saklar, gecikmeli bir işlemdir
            // e.g. LinqToSql - veri tabanını mapler, LinqToXml xml dökümanlarını mapler.
            var db = new BikeRepository();

            // işlem gecikmelidir, bir veri tabanı üzerinde sorgulama yaparken harikadır.
            var filter = db.Bikes.Where(b => b.HasTassles); // sorgu henüz çalışmadı
            if (42 > 6) // Filtreler eklemeye devam edebilirsiniz - ileri düzey arama fonksiyonları için harikadır
                filter = filter.Where(b => b.IsBroken); // sorgu henüz çalışmadı

            var query = filter
                .OrderBy(b => b.Wheels)
                .ThenBy(b => b.Name)
                .Select(b => b.Name); // hala sorgu çalışmadı

            // Şimdi sorgu çalışıyor, reader'ı açar ama sadece sizin sorgunuza uyanlar foreach döngüsüne girer.
            foreach (string bike in query) 
                Console.WriteLine(result);
            


        }

    } // LearnCSharp sınıfının sonu

    // Bir .cs dosyasına diğer sınıflarıda dahil edebilirsiniz

    public static class Extensions
    {
        // UZANTI FONKSİYONLARI
        public static void Print(this object obj)
        {
            Console.WriteLine(obj.ToString());
        }
    }

    // Sınıf Tanımlama Sözdizimi:
    // <public/private/protected/internal> class <sınıf ismi>{
    //    //veri alanları, kurucular , fonksiyonlar hepsi içindedir.
    //    //Fonksiyonlar Java'daki gibi metod olarak çağırılır.
    // }

    public class Bicycle
    {
        // Bicycle'ın Alanları/Değişkenleri
        public int Cadence // Public: herhangi bir yerden erişilebilir
        {
            get // get - değeri almak için tanımlanan metod
            {
                return _cadence;
            }
            set // set - değer atamak için tanımlanan metod
            {
                _cadence = value; // Değer setter'a gönderilen value değeridir
            }
        }
        private int _cadence;

        protected virtual int Gear // Protected: Sınıf ve alt sınıflar tarafından erişilebilir
        {
            get; // bir üye alanına ihtiyacınız yok, bu otomatik olarak bir değer oluşturacaktır
            set;
        }

        internal int Wheels // Internal: Assembly tarafından erişilebilir
        {
            get;
            private set; // Nitelik belirleyicileri get/set metodlarında atayabilirsiniz
        }

        int _speed; // Her şey varsayılan olarak private'dır : Sadece sınıf içinden erişilebilir.
                    // İsterseniz yinede private kelimesini kullanabilirsiniz.
        public string Name { get; set; }

        // Enum sabitler kümesinden oluşan bir değer tipidir.
        // Gerçekten sadece bir isim ile bir değeri tutmak için kullanılır. (aksi belirtilmedikçe bir int'dir).
        // İzin verilen enum tipleri şunlardır byte, sbyte, short, ushort, int, uint, long, veya ulong.
        // Bir enum aynı değeri birden fazla sayıda barındıramaz.
        public enum BikeBrand
        {
            AIST,
            BMC,
            Electra = 42, // bir isme tam bir değer verebilirsiniz
            Gitane // 43
        }
        // Bu tipi Bicycle sınıfı içinde tanımladığımız için bu bir bağımlı tipdir.
        // Bu sınıf dışında kullanmak için tipi Bicycle.Brand olarak kullanmamız gerekir

        public BikeBrand Brand; // Enum tipini tanımladıktan sonra alan tipini tanımlayabiliriz

        // Static üyeler belirli bir obje yerine kendi tipine aittir
        // Onlara bir obje referans göstermeden erişebilirsiniz:
        // Console.WriteLine("Bicycles created: " + Bicycle.bicyclesCreated);
        static public int BicyclesCreated = 0;

        // readonly değerleri çalışma zamanında atanır
        // onlara sadece tanımlama yapılarak ya da kurucular içinden atama yapılabilir
        readonly bool _hasCardsInSpokes = false; // read-only private

        // Kurucular sınıf oluşturmanın bir yoludur
        // Bu bir varsayılan kurucudur.
        public Bicycle() 
        {
            this.Gear = 1; // bu objenin üyelerine this anahtar kelimesi ile ulaşılır
            Cadence = 50;  // ama her zaman buna ihtiyaç duyulmaz
            _speed = 5;
            Name = "Bontrager";
            Brand = BikeBrand.AIST;
            BicyclesCreated++;
        }

        // Bu belirlenmiş bir kurucudur. (argümanlar içerir)
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand) 
            : base() // önce base'i çağırın
        {
            Gear = startGear; 
            Cadence = startCadence;
            _speed = startSpeed;
            Name = name; 
            _hasCardsInSpokes = hasCardsInSpokes;
            Brand = brand;
        }

        // Kurucular zincirleme olabilir
        public Bicycle(int startCadence, int startSpeed, BikeBrand brand) :
            this(startCadence, startSpeed, 0, "big wheels", true, brand)
        {
        }

        // Fonksiyon Sözdizimi:
        // <public/private/protected> <dönüş tipi> <fonksiyon ismi>(<argümanlar>)

        // sınıflar getter ve setter'ları alanları için kendisi uygular
        // veya property'ler eklenebilir (C# da tercih edilen yol budur)

        // Metod parametreleri varsayılan değerlere sahip olabilir.
        // Bu durumda, metodlar bu parametreler olmadan çağırılabilir.
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // property'lerin get/set değerleri
        // sadece veri gerektiği zaman erişilebilir, kullanmak için bunu göz önünde bulundurun.
        // property'ler sadece get ya da set'e sahip olabilir veya ikisine birden
        private bool _hasTassles; // private değişken
        public bool HasTassles // public accessor
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // Ayrıca tek bir satırda otomatik property tanımlayabilirsiniz. 
        // bu söz dizimi otomatik olarak alan oluşturacaktır.
        // Erişimi kısıtlamak için nitelik belirleyiciler getter veya setter'a ya da ikisine birden atanabilir:
        public bool IsBroken { get; private set; }

        // Property'ler otomatik eklenmiş olabilir
        public int FrameSize
        {
            get;
            // nitelik beliryecileri get veya set için tanımlayabilirsiniz
            // bu sadece Bicycle sınıfı Framesize değerine atama yapabilir demektir
            private set;
        }

        // Ayrıca obje üzerinde özel indeksleyici belirlemek mümkündür.
        // Tüm bunlar bu örnek için çok kullanışlı değil,
        // bicycle[0] ile ilk yolcu olan "chris" i almak mümkün veya
        // bicycle[1] = "lisa" ile yolcuyu atayabilirsiniz. (bariz quattrocycle)
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

        //Bu objenin nitelik değerlerini göstermek için bir metod.
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

        // Metodlar static olabilir. Yardımcı metodlar için kullanışlı olabilir.
        public static bool DidWeCreateEnoughBycles()
        {
            // Bir static metod içinde sadece static sınıf üyeleri referans gösterilebilir
            return BicyclesCreated > 9000;
        } // Eğer sınıfınızın sadece static üyelere ihtiyacı varsa, sınıfın kendisini static yapmayı düşünebilirsiniz.


    } // Bicycle sınıfı sonu

    // PennyFarthing , Bicycle sınıfının alt sınıfıdır.
    class PennyFarthing : Bicycle
    {
        // (Penny Farthing'ler ön jantı büyük bisikletlerdir.
        // Vitesleri yoktur.)

        // Ana kurucuyu çağırmak
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
            result += base.ToString(); // Metodun temel versiyonunu çağırmak
            return result;
        }
    }

    // Arabirimler sadece üyelerin izlerini içerir, değerlerini değil.
    interface IJumpable
    {
        void Jump(int meters); // bütün arbirim üyeleri public'tir
    }

    interface IBreakable
    {
        bool Broken { get; } // arabirimler property'leri, metodları ve olayları içerebilir
    }

    // Sınıflar sadece tek bir sınıftan miras alabilir ama sınırsız sayıda arabirime sahip olabilir
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
    /// LinqToSql örneği veri tabanına bağlanmak için kullanılır.
    /// EntityFramework Code First harika! (Ruby'deki ActiveRecord'a benzer, ama iki yönlü)
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
} // namespace sonu
```

## İşlenmeyen Konular

 * Flags
 * Attributes
 * Static properties
 * Exceptions, Abstraction
 * ASP.NET (Web Forms/MVC/WebMatrix)
 * Winforms
 * Windows Presentation Foundation (WPF)

## Daha Fazlasını Okuyun

 * [DotNetPerls](http://www.dotnetperls.com)
 * [C# in Depth](http://manning.com/skeet2)
 * [Programming C#](http://shop.oreilly.com/product/0636920024064.do)
 * [LINQ](http://shop.oreilly.com/product/9780596519254.do)
 * [MSDN Library](http://msdn.microsoft.com/en-us/library/618ayhy6.aspx)
 * [ASP.NET MVC Tutorials](http://www.asp.net/mvc/tutorials)
 * [ASP.NET Web Matrix Tutorials](http://www.asp.net/web-pages/tutorials)
 * [ASP.NET Web Forms Tutorials](http://www.asp.net/web-forms/tutorials)
 * [Windows Forms Programming in C#](http://www.amazon.com/Windows-Forms-Programming-Chris-Sells/dp/0321116208)



[C# Kodlama Adetleri](http://msdn.microsoft.com/en-us/library/vstudio/ff926074.aspx)
