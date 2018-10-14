---
language: c++
lang: tr-tr
filename: learncpp-tr.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Connor Waters", "http://github.com/connorwaters"]
    - ["Ankush Goyal", "http://github.com/ankushg07"]
    - ["Jatin Dhankhar", "https://github.com/jatindhankhar"]
    - ["Adem Budak", "https://github.com/p1v0t"]
---

C++ 
[yaratıcısı Bjarne Stroustrup'a göre](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote),

- "daha iyi bir C" yapmak
- veri soyutlamayı desteklemek 
- nesneye yönelik programlamayı deskteklemek
- tipten bağımsız programlamayı desteklemek 

için tasarlanmış bir sistem programlama dilir.

Sözdizimi daha yeni dillerden daha zor veya karmaşık olsa da işlemcinin doğrudan çalıştırabileceği
native komutlara derlenerek, donanım üzerinde (C gibi) sıkı bir kontrol sağlar, bunu yaparken
tipten bağımsızlık, exception'lar ve sınıflar gibi yüksek-seviyeli özellikleri destekler.
Bu hız ve kullanışlılık C++'ı en çok kullanılan dillerden biri yapar.

```c++
//////////////////////
// C ile karşılaştırma
//////////////////////

// C++ _neredeyse_ C'nin bir üstkümesidir, değişken tanımı, basit tipleri
// ve fonksiyonları için temelde aynı sözdizimini paylaşır.

// Aynı C gibi, programın başlangıç noktası bir integer döndüren 
// main fonksiyonudur.
// Bu değer programın bitiş statüsünü belli eder.
// Daha fazla bilgi için bknz http://en.wikipedia.org/wiki/Exit_status .

int main(int argc, char** argv)
{
    // Komut satırı argümanları C'de olduğu gibi argv ve argc ile geçilir
    // argc, argüman sayısını belli eder,
    // argv, argümanları belli eden, C-stili string'lerin (char*) dizisidir.
    // İlk argüman çağrılan programın adıdır.
    // Eğer argümanları umursamıyorsan, argv ve argc kullanılmayabilir 
    // int main() gibi

    // 0 çıkış durumu başarıyı belirtir.
    return 0;
}

// Bunlara rağmen C++ aşağıdaki noktalarda farklılaşır:

// C++'ta, karakterler char türündendir
sizeof('c') == sizeof(char) == 1

// C'de, karakterler int türündendir
sizeof('c') == sizeof(int)


// C++ katı bir prototip kuralına sahiptir
void func(); // fonksiyon argüman kabul etmez

// C'de
void func(); // fonksiyon herhangi bir sayıda argüman kabul edebilir

// C++'da NULL yerine nullptr kullanılır
int* ip = nullptr;

// C standard başlıkları başına "c" eklenip, sondaki .h
// kullanılmadan C++'ta kullanılabilir
#include <cstdio>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

//////////////////////////////////
// Fonksiyonun fazladan yüklenmesi
//////////////////////////////////

// C++ herbir fonksiyonun farklı parametereler
// aldığı fonksiyon fazladan yüklenmesini desktekler 

void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("My int is %d", myInt);
}

int main()
{
    print("Hello"); // void print(const char*) fonksiyonunu çağırır.
    print(15); // void print(int) fonksiyonunu çağırır.
}

////////////////////////////////
// Default fonksiyon argümanları
////////////////////////////////

// Eğer çağırıcı tarafından fonksiyona argüman sağlanmamışsa,
// fonksiyona default argüman verebilirsin

void doSomethingWithInts(int a = 1, int b = 4)
{
    // Burada int'lerle birşeyler yap
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// Default argümanlar, argüman listesinin sonunda yer almalı.

void invalidDeclaration(int a = 1, int b) // Hata!
{
}


/////////////////////////
// Namespace(İsim uzayı)
/////////////////////////

// Namespace'ler değişken, fonksiyon ve diğer bildirimlerin 
// kapsama alanını ayırır. 
// Namespace'ler içiçe geçebilir.

namespace First {
    namespace Nested {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // Nested namespace'inin sonu
} // First namespace'inin sonu

namespace Second {
    void foo()
    {
        printf("This is Second::foo\n");
    }
}

void foo()
{
    printf("This is global foo\n");
}

int main()
{
    // Second namespace'i içinideki tüm sembolleri mevcut kapsama alanına dahil eder.
    // Dikkat edersen artık yalnızca foo() çağrısı çalışmayacaktır çünkü hangi
    // namespace'ten çağrıldığı açık değildir.
    using namespace Second;

    Second::foo(); // "This is Second::foo" yazdırıır
    First::Nested::foo(); // "This is First::Nested::foo" yazdırır
    ::foo(); // "This is global foo" yazdırır.
}

///////////////
// Input/Output
///////////////

// C++'ta input ve output stream'leri kullanır.
// cin, cout ve cerr,sırasıyla, stdin, stdout, ve stderr'i temsil eder.
// << araya ekleme ve >> aradan çıkarma operatörüdür.

#include <iostream> // I/O stream'lerini dahil etmek için

using namespace std; // Streamler std namespace'i içindedir(standard kütüphane)

int main()
{
   int myInt;

   // stdout (veya terminal/screen)'ta çıktı verir
   cout << "Enter your favorite number:\n";
   // Girdiyi alır 
   cin >> myInt;

   // cout ayrıca formatlanabilir
   cout << "Your favorite number is " << myInt << "\n";
   // prints "Your favorite number is <myInt>"

    cerr << "Used for error messages";
}

//////////////
// String'ler
/////////////

// String'ler C++'ta nesnedir ve pek çok üye fonksiyonu vardır
#include <string>

using namespace std; // String'ler de std namespace'i içindedir. (standard kütüphane)

string myString = "Hello";
string myOtherString = " World";

// + eklemek için kullanıldır 
cout << myString + myOtherString; // "Hello World"

cout << myString + " You"; // "Hello You"

// C++'ta  stringler are mutable'dır (değişebilir).
myString.append(" Dog");
cout << myString; // "Hello Dog"


///////////////////////
// Reference (Referans)
///////////////////////

// C'deki pointer'lara ek olarak 
// C++ _reference_'lara sahiptir.
// Bunlar bir kere atandınğında tekrardan atanamayan pointer'dır
// ve null olamaz.
// Değişkenin kendisiyle aynı sözdizimine sahiptir:
// Değerine ulaşmak için * ihtiyaç yoktur ve
// atama için & (address of) kullanılmaz.

using namespace std;

string foo = "I am foo";
string bar = "I am bar";


string& fooRef = foo; // Bu foo'nun reference'ını oluşturur.
fooRef += ". Hi!"; // foo'yu reference'ı üzerinden değiştirir.
cout << fooRef; // "I am foo. Hi!" yazdırır.

// "fooRef"e yeniden atama yapmaz. Bu "foo = bar" denktir ve bu satırdan sonra
//  foo == "I am bar" olur
cout << &fooRef << endl; // foo'un adresini yazdırır
fooRef = bar;
cout << &fooRef << endl; //Hala foo'nun adresini yazdırır
cout << fooRef;  //"I am bar" yazdırır

// fooRef'in adresi aynı kalır yani hala foo'nun adresidir.

const string& barRef = bar; // bar'a const reference oluşturur
// C'de olduğu gibi, const değerler (pointer'lar ve reference'ler) değiştirilemez.
barRef += ". Hi!"; // Hata, const reference'ler değiştirilemez.

// Kısa bir ekleme: reference'lere devam etmeden önce, geçici nesne konseptinden
// bahsetmeliyiz. Mesela aşadaki gibi bir kod var:
string tempObjectFun() { ... }
string retVal = tempObjectFun();

// Bu iki satırda aslında ne oluyor:
//   - tempObjectFun fonksiyonundan bir string nesnesi dönüyor
//   - dönmüş olan nesneyle yeni bir string oluşturuyor
//   - dönmüş olan nesne yok ediliyor
// İşte bu dönen nesneye geçici nesne denir. Geçici nesneler fonksiyon nesne
// döndürdüğünde oluşturulur ve ifade işini bitirdiğinde yok edilir (Aslında,
// standard'ın söylediği şey bu ama derleyiciler bu davranışı değiştirmemize 
// izin veriyor. Daha fazla detay için "return value optimization" diye
// aratabilirsin. Sonuç olarak aşağıdaki kodda:
foo(bar(tempObjectFun()))

// foo ve bar'ın varolduğunu kabul ediyoruz, tempObjectFun'dan dönen nesne
// bar'a geçti ve foo çağrılmadan önce yokedildir.

// Şimdi reference'lara dönelim. "ifadenin sonunda" kuralının bir istisnası
// eğer geçici nesne const reference'a geçildiyse oratya çıkar, bu durumda
// nesnenin ömrü mevcut kapsama alanına kadar uzar:

void constReferenceTempObjectFun() {
  // constRef geçici nesneyi alır ve bu durum fonksiyonun sonuna kadar geçerlidir.
  const string& constRef = tempObjectFun();
  ...
}

// C++11 ile gelen diğer bir reference geçici nesnelere özeldir. Bu türden birden 
// bir tip tanımlayamazsın ama aşırı yüklenme sırasında bu tipler öncelik alır:
void someFun(string& s) { ... }  // Regular reference
void someFun(string&& s) { ... }  // Geçici nesneye reference 

string foo;
someFun(foo);  // regular reference'ı çağırır
someFun(tempObjectFun());  // geçici reference'ı çağırır

/////////////////////
// Enum
/////////////////////

// Enum'lar sabit değerler yapmak için kullanılır ve çoğunlukla kodun daha okunaklı
// olması için kullanılır

enum ECarTypes
{
  Sedan,
  Hatchback,
  SUV,
  Wagon
};

ECarTypes GetPreferredCarType()
{
	return ECarTypes::Hatchback;
}

// C++11 ile beraber bir tipi enum'a atamanın kolay bir yolu var, bu enum'un istenen
// tipe dönüştürmek için kullanışlı bir yöntem
enum ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

void WriteByteToFile(uint8_t InputValue)
{
	// Serialize the InputValue to a file
}

void WritePreferredCarTypeToFile(ECarTypes InputCarType)
{
	// enum uint8_t tipine dönüştürüldü
	WriteByteToFile(InputCarType);
}

// Diğer yandan enum'ların yanlışlıkla integer tipini veya diğer enumlara dönüşmesini
// istemiyorsan enum class olarak tanımlayabilirsin
enum class ECarTypes : uint8_t
{
  Sedan, // 0
  Hatchback, // 1
  SUV = 254, // 254
  Hybrid // 255
};

void WriteByteToFile(uint8_t InputValue)
{
	// Serialize the InputValue to a file
}

void WritePreferredCarTypeToFile(ECarTypes InputCarType)
{
	// ECarTypes, uint8_t tipinde olmasına rağmen, "enum class" olarak 
	// tanımlandığından derlenmeyecektir!
	WriteByteToFile(InputCarType);
}

///////////////////////////////////////////
// Sınıflar ve nesneye yönelik proglamalama
///////////////////////////////////////////

// Sınıflara(class) ilk örnek
#include <iostream>

// Sınıfı tanımla.
// Sınıflar genelde header (.h veya .hpp) dosyalarında tanımlanır.
class Dog {
    // Üye değişkenler ve fonksiyonlar default olarak private'dir.
    std::string name;
    int weight;

// Aşağıda, "private:" veya "protected:" bulunana kadar
// bütün üyeler public'tir.
public:

    // Default constructor
    Dog();

    // Üye fonksiyon bildirimi (gerçeklenimi aşağıda)
    // Dikkat ederseniz using namespace std; yerine
    // std::string kullandık.
    // Hiçbir zaman header dosyasında "using namespace std;" kullanma.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // Nesnenin durumunu değiştirmeyen fonksiyonlar const ile işaretlenmelidir

    // Türetilen sınıflarda fonksiyonu override edebilmek için başına 
    // _virtual_ eklenmelidir.
    // Fonksiyonlar, performanslar ilgili nedenlerden ötürü default olarak virtual değildir
    virtual void print() const;

    // Fonksiyonlar class içinde de tanımlanabilir.
    // Bu şekille tanımlanan fonksiyonlar otomatik olarak inline olur.
    void bark() const { std::cout << name << " barks!\n"; }

    // C++ constructor'ların yanında destructor'da sağlar.
    // Bunlar nesne silindiğinde veya scope'un dışına çıktığında çağrılır.
    // Bu RAII gibi güçlü paradigmaları etkin kılar.
    // (aşağıda açıklandı)
    // Eğer sınıf kendisinden türetiliyorsa, destructor virtual olmalıdır,
    // eğer virtual değilse, türetilmiş sınıfın destructor'ı nesne, ana sınıf
    // referans'ı veya pointer'ı üzerinden yok edildiğinde, çağrılmayacaktır.
    virtual ~Dog();

}; // class tanımının sonuda noktalı virgül(;) olmalıdır.

// Sınıfın üye fonksiyonları genelde .cpp dosyaları içinde gerçeklenir.
Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// Objects (such as strings) should be passed by reference
// Nesneler (string gibi) reference ile fonksiyonlara geçilmelidir
// Eğer nesneleri değiştirilecekse reference ile fonksiyonlara geçilmelidir,
// değiştirilmeyecekse const reference ile geçilmelidir.
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// Dikkat edersen "virtual" yalnızca bildirimde gerekli, tanımlamada değil.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

Dog::~Dog()
{
    std::cout << "Goodbye " << name << "\n";
}

int main() {
    Dog myDog; // "A dog has been constructed" yazdırır
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.print(); // "Dog is Barkley and weighs 10 kg" yazdırır.
    return 0;
} // "Goodbye Barkley" yazdırır.

// Inheritance(Miras)

// Bu sınıf, Dog sınıfında public ve protected olan herşeyi miras alır, 
// private olanları da miras alır ama, public ve protected sınıflar aracılıyla
// yapılmıyorsa, doğrudan erişemez.
class OwnedDog : public Dog {

public:
    void setOwner(const std::string& dogsOwner);

    // print fonksiyonunun davranışını bütün OwnedDogs sınıfı için override eder
    // (üstünden geçer, kendine uyarlar).
    // bknz http://en.wikipedia.org/wiki/Polymorphism_(computer_science)
    // override anahtar sözcüpü kullanılma da olur ama kullanılması aslında bir temel
    // temel sınıf fonksiyonunun üzerinden geçtiğimizi gösterir.
    void print() const override;

private:
    std::string owner;
};

// Bu arada takip eden .cpp dosyasında:

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // Ana dog sınıfındaki print fonksiyonunu çağırır 
    std::cout << "Dog is owned by " << owner << "\n";
    // 	      "Dog is <name> and weights <weight>"
    //        "Dog is owned by <owner>"
    // 	       yazdırır
}

/////////////////////////////////////////////////////
// ilk değer atama ve Operatörün fazladan yüklenmesi
/////////////////////////////////////////////////////

// C++ dilinde +, -, *, /, gibi operatörlerin davranışını fazladan yükleyebilirsiniz.
// Bu, operator her kullandınıldığında çağrılan bir fonksiyon tanımlamasıyla yapılır.

#include <iostream>
using namespace std;

class Point {
public:
    // Üye değişkenkenlere default değer atanabilir.
    double x = 0;
    double y = 0;
    
    // Default constructor
    Point() { };

    Point (double a, double b) :
        x(a),
        y(b)
    { /* İlk değer atama dışında birşey yapma */ }

    // + operatorünün fazladan yükle.
    Point operator+(const Point& rhs) const;

    // += operatorünü fazladan yükle
    Point& operator+=(const Point& rhs);

    // - ve -= operatorleri fazladan yüklemek de mantıklı olurdu
    // ama kısa tutmak için burda değinmedik.
};

Point Point::operator+(const Point& rhs) const
{
    // yeni bir nokta oluştur ve bunu rhs ile topla
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
    // Bu Point + operatorünü çağırır
    Point result = up + right;
    // "Result is upright (1,1)" yazdırır.
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

////////////////////////
// Şablonlar (Templates)
////////////////////////

// Şablonlar C++ dilinde tipten bağımsız programlama için kullanılır.

// Zaten aşina olduğun tipten bağımsız programlamayla başladık. Bir tip parametresi
// alan fonksiyon veya sınıf tanımlamaık için:
template<class T>
class Box {
public:
    // Bu sınıfta T, herhangi bir tip için kullanılabilir.
    void insert(const T&) { ... }
};

// Derleme esnasında derleyici aslında, parametreleri yerine konmuş şekilde herbir şablonu üretir,
// bu yüzden sınıfın tam tanımı her çağrılma sırasında var olmak zorundadır. Bu nedenle şablon sınıflarını
// tamamen header dosyalarında görürsün.

// Stack'ta şablon sınıfın bir örneğini oluşturmak için:
Box<int> intBox;

// ve, anladığın gibi, kullanabilirsin:
intBox.insert(123);

// Tabi, şablonları içiçe geçirebilirsin:
Box<Box<int> > boxOfBox;
boxOfBox.insert(intBox);

// C++11'den önce iki '>' arasına boşluk koymak zorundaydık yoksa sağa kaydırma 
// operatörü olarak algılanabilirdi.

// Bazen şunu da görebilirsin
//   template<typename T>
// 'class' ve 'typename' anahtar sözcükleri çoğunlukla 
// birbirlerinin yerine kullanılabilir. Tam açıklama için, bknz.
//   http://en.wikipedia.org/wiki/Typename
// (evet, bu anahtar sözcüğün kendi Wikipedia sayfası var).

// Benzer şekilde, bir şablon fonksiyon:
template<class T>
void barkThreeTimes(const T& input)
{
    input.bark();
    input.bark();
    input.bark();
}

// Dikkat edersen tip parametresi hakkında birşey belirtilmedi. Derleyici bunları üretecek
// ve her parametre geçişinde tip-kontrolü yapacaktır, bu nedenle de fonksiyon herhangi bir T
// tipi için çalışacaktır!

Dog fluffy;
fluffy.setName("Fluffy")
barkThreeTimes(fluffy); // Üç kere "Fluffy barks" yazdırır.

// Şablonun parametresi sınıf olmak zorunda değildir:
template<int Y>
void printMessage() {
  cout << "Learn C++ in " << Y << " minutes!" << endl;
}

// Ve template'i daha etkili kod için dışarıdan özelleştirebilirsin. 
// Tabiki gerçek-dünya kullanımlarında özelleştirme bunun kadar kolay değildir.
// Dikkat edersen, bütün parametreleri dıştan özelleştirmiş olsak bile
// hala fonksiyonu (veya sınıfı( template olarak tanımlamamız gerekli.
template<>
void printMessage<10>() {
  cout << "Learn C++ faster in only 10 minutes!" << endl;
}

printMessage<20>();  // "Learn C++ in 20 minutes!" yazdırır
printMessage<10>();  // "Learn C++ faster in only 10 minutes!" yazdırır


///////////////////////////////////////////////
// İstisnai Durum Yönetimi (Exception Handling)
///////////////////////////////////////////////

// Standard kütüphane bazı istisnai tipler sağlar
// (bknz http://en.cppreference.com/w/cpp/error/exception)
// ama herhangi bir tip de istisnai durum fırlatabilir 

#include <exception>
#include <stdexcept>

// _try_ bloğu içinde fırlatılan bütün istisnai durumlar, takip eden, _catch_ ile 
// yakalanabilir.
try {
    // _new_ kullanarak heap'ten istisnai durumlar için yer ayırma
    throw std::runtime_error("A problem occurred");
}

// istisnai durumlar nesne ise  const reference ile yakala
catch (const std::exception& ex)
{
    std::cout << ex.what();
}

// Bir önceki _catch_ bloğundan kaçan istisnai durum burda yakala
catch (...)
{
    std::cout << "Unknown exception caught";
    throw; // Tekrardan istisnai durum fırlatır
}

///////
// RAII
///////

// RAII, "Resource Acquisition Is Initialization" kelimelerinin kısaltmasıdır.
// Bu Türkçe, "Kaynak alımı aynı zamanda ilk değer atamasıdır." olarak çevrilebilir.
// Bunu basitçe constructor ile ayrılan hafızanın destructor ile iade edilmesi olarak 
// düşünebiliriz.

// Bunun ne şekilde kullanışlı olduğunu görmek için
// bir C dosyasının, dosya işleme biçimine bakabiliriz:
void doSomethingWithAFile(const char* filename)
{
    // Başlangıçta herşeyin yolunda gittiğini düşünelim

    FILE* fh = fopen(filename, "r"); // Dosyayı okuma modunda aç

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // Dosyayı kapat
}

// Malesef hatalarla başa çıkmaya çalışırken işler hızlıca karmaşıklaşır.
// Mesela fopen'ın başarısız olduğunu varsayalım, ve doSoomethingWithTheFile ve 
// doSomethingWithIt hata kodları gönderdi.
//  (İstisnai durumlar yonetimi, hata koduna tercih ediler bir yöntemdir, ama bazı
//   programcılar, özellikle C arkaplanı olanlar, aynı fikirde değildir.
// Bu durumda her bir fonksiyon çağrısını kontrol etmeli ve bir problem oluştuysa
// dosyayı kapatmalıyız.

bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");  // Dosyayı okuma modunda aç
    if (fh == nullptr) // Başarısız olma durumunda dönen değer null olur
        return false; // Başarısız olma durumunu çağırıcıya bildir

    // Başarısız olma durumunda her iki fonksiyonun da false döndürdüğünü kabul edelim
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // Dosyayı kapatalım, akıntı olmasın.
        return false; // Hatayı bildir
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // Dosyayı kapatalım, akıntı olmasın.
        return false; // Hatayı bildir
    }

    fclose(fh); // Dosyayı kapat
    return true; // Başarı durumunu ifade eder
}

// C programcıları biraz goto kullanarak bu durumu temizler
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // Dosyayı kapat 
    return true; // Başarı durumunu ifade eder

failure:
    fclose(fh);
    return false; // Hatayı bildir
}

// Eğer fonksiyon istisnai durum yönetimi araçlarını kullanırsa
// işler daha temiz olur ama hala en iyi durumun altında kalır.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); 
    if (fh == nullptr)
        throw std::runtime_error("Could not open the file.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) {
        fclose(fh); // Hata durumunda dosyayı kapattığından emin ol
        throw; // Sonra, tekrardan istisnai durum fırlat
    }

    fclose(fh); // Dosyayı kapat
    // Herşey başarılı
}

// Şimdi aynı şeyi C++'ın dosya stream sınıfıyla (fstream) karşılaştıralım
// fstream, dosyayı kapatmak için kendi destructor'ını kullanır.
// Destructor'ın, nesne scope dışına çıktığında otomatik olarak çağrıldığını 
// hatırlayın.
void doSomethingWithAFile(const std::string& filename)
{
    std::ifstream fh(filename); // Dosyayı aç

    // Dosyayla birşeyler yap
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // Dosya, destructor tarafından otomatik olarak kapatıldı

// Bunun _çok büyük_ avantajları var:
// 1. Ne olursa olursun,
//    kaynak (bu örnekte dosya tutucusu) temizlenecektir.
//    Destructor doğru yazıldığında,
//    Tutucuyu kapatmayı unutma ve kaynak akıntısı _imkansız_dır.
// 2. Kodun çok daha temiz olduğuna dikkat edin.
//    Destructor, dosyayı kapatma işini, endilenmemize gerek kalmadan
//    arka planda halleder.
// 3. Kod, istisnai durumlara karşı korunaklıdır.
//    İstisnai durum fonksiyonun herhangi bir yerinde fırlatılabilir ve
//    temizleme işi gene de yapılır.

// Bütün C++ kodu deyimleri RAII prensibini tüm kaynakları için kullanır.
// Ek örnekler şunlardır:
// - unique_ptr ve shared_ptr ile hafıza kullanımı
// - Tutucular - standard kütüphane linked list,
//   vector (yani kendiliğinden boyu ayarlanan dizi), hash map vs.
//   scope'un dışına çıktığında içerini otomatik olarak yok eden tüm yapılar.
// - lock_guard ve unique_lock kullanan mutex'ler

///////////////////////////////////////
// Lambda İfadeleri (C++11 ve yukarısı)
///////////////////////////////////////

// lambda'lar, tam olarak çağrıldığı yerde bir anonim fonksiyon tanımlamak
// veya fonksiyona argüman geçmek için uygun bir yoldur.

// Mesela, pair'lardan oluşan bir vector'u, pair'ın ikinci değerine 
// göre sıralamak isteyelim

vector<pair<int, int> > tester;
tester.push_back(make_pair(3, 6));
tester.push_back(make_pair(1, 9));
tester.push_back(make_pair(5, 0));

// sort fonksiyonuna üçüncü argüman olarak lambda ifadesini geç
// sort, <algorithm> başlığında tanımlı

sort(tester.begin(), tester.end(), [](const pair<int, int>& lhs, const pair<int, int>& rhs) {
        return lhs.second < rhs.second;
    });

// Lambda ifadesinin söz dizimine dikkat edin, 
// lambda'daki [], değişkenleri "tutmak" için kullanılır
// "Tutma listesi", fonksiyon gövdesinde nelerin, ne şekilde erişilebilir olduğunu tanımlar
// Şunlardan biri olabilir:
// 	1. bir değer : [x]
//	2. bir referans : [&x]
//	3. mevcut scope içindeki herhangi bir değişkene referans ile [&]
//	4. 3 ile aynı, ama değer ile [=]
// Mesela:
vector<int> dog_ids;
// number_of_dogs = 3;
for(int i = 0; i < 3; i++) {
	dog_ids.push_back(i);
}

int weight[3] = {30, 50, 10};

// Mesela dog_ids vector'unu dog'ların ağırlıklarına göre sıralamak isteyelim
// Yani en sonunda şöyle olmalı: [2, 0, 1]

// Burada lambda ifadesi oldukça kullanışlıdır

sort(dog_ids.begin(), dog_ids.end(), [&weight](const int &lhs, const int &rhs) {
        return weight[lhs] < weight[rhs];
    });
// Dikkat edersen "weight" dizisini referans ile aldık.
// C++'da lambdalar hakkında daha fazla bilgi için : http://stackoverflow.com/questions/7627098/what-is-a-lambda-expression-in-c11

//////////////////////////////////
// Akıllı For (C++11 ve yukarısı)
//////////////////////////////////

// Akıllı for döngüsünü bir tutucuyu dolaşmak için kullanabilirsin
int arr[] = {1, 10, 3};

for(int elem: arr){
	cout << elem << endl;
}

// Tutucunun elemanlarının tipi için endişe etmeden "auto" kullanabilirsin
// Mesela:

for(auto elem: arr) {
	// arr dizisinin elemanlarıyla ilgili bir şeyler yap
}

////////////////
// Güzel Şeyler
////////////////

// C++ dilinin bakış açısı yeni başlayanlar için (hatta dili iyi bilenler için bile)
// şaşırtıcı olabilir. 
// Bu bölüm, ne yazık ki, büyük ölçüde tam değil; C++ kendi ayağına ateş edilebilecek kolay
// dillerden biridir.

// private metodları override edebilirsin!
class Foo {
  virtual void bar();
};
class FooSub : public Foo {
  virtual void bar();  // Foo::bar fonksiyonu override edilir!
};


// 0 == false == NULL (çoğu zaman)!
bool* pt = new bool;
*pt = 0; // 'pt'nin gösterdiği değere false atar.
pt = 0;  // 'pt'ye null pointer atar. Her iki satır uyarısız derlenir.

// nullptr'ın bu meselenin bazılarını çözmesi beklenmiştir:
int* pt2 = new int;
*pt2 = nullptr; // Derlenmez.
pt2 = nullptr;  // pt2'ye null atar.

// bool tipleri için bir istisna vardır.
// Bu null pointer'ları if(!ptr) ile test etmek içindir.
// ama sonuç olarak bir bool değerine nullptr atayabilirsin!
*pt = nullptr;  // '*pt' değeri bir boll olmasına rağmen, hala derlenir!


// '=' != '=' != '='!
// Calls Foo::Foo(const Foo&) or some variant (see move semantics) copy
// Foo::Foo(const Foo&) çağrısını veya kopyalama constructor'ının bir çeşidinin çağrısınıyapar(taşıma semantiklerine bknz.)
Foo f2;
Foo f1 = f2;

// Foo::operator=(Foo&) çağrısını yapar.
Foo f1;
f1 = f2;


///////////////////////////////////////
// Tuple (C++11 ve yukarısı)
///////////////////////////////////////

#include<tuple>

// Ana fikir olarak, Tuple, eski veri yapılarına (C'deki struct'lar) benzer ama isimli veri üyeleri yerine 
// elemanlarına tuple içindeki sırasına göre erişilir.

// Tuple'ı inşa ederek başlayalım
// değişkenleri tuple içinde paketliyoruz
auto first = make_tuple(10, 'A');
const int maxN = 1e9;
const int maxL = 15;
auto second = make_tuple(maxN, maxL);

// 'first' tuple'ının değerlerini yazdırma
cout << get<0>(first) << " " << get<1>(first) << "\n"; // 10 A yazdırır

// 'second' tuple'ının değerlerini yazdırma
cout << get<0>(second) << " " << get<1>(second) << "\n"; // 1000000000 15 yazdırır

// Değişkenleri tuple'dan çıkarma

int first_int;
char first_char;
tie(first_int, first_char) = first;
cout << first_int << " " << first_char << "\n";  // 10 A yazdırır

// Ayrıca şu şekide de tuple oluşturabiliriz.

tuple<int, char, double> third(11, 'A', 3.14141);
// tuple_size, tuple'daki eleman sayısını (constexpr olarak) döndürür

cout << tuple_size<decltype(third)>::value << "\n"; // 3 yazdırır

// tuple_cat, tuple'daki tüm elemanları aynı sırada birleştirir.

auto concatenated_tuple = tuple_cat(first, second, third);
// concatenated_tuple = (10, 'A', 1e9, 15, 11, 'A', 3.14141) olur

cout << get<0>(concatenated_tuple) << "\n"; // 10 yazdırır
cout << get<3>(concatenated_tuple) << "\n"; // 15 yazdırır
cout << get<5>(concatenated_tuple) << "\n"; // 'A' yazdırır


/////////////////////
// Tutucular
/////////////////////

// Tutucular veya Standard Şablon Kütüphanesi(STL) önceden tanımlanmış şablonlar sunar.
// Bunlar elemanları için ayrılan hafıza alanını yönetir
// ve onlara erişim ve değiştirmek için üye fonksiyonlar sağlar

// Bazı tutucular şunlardır:

// Vector (Dinamik Dizi)
// koşma anında nesne dizisi veya list oluşturmamızı sağlar
#include <vector>
string val;
vector<string> my_vector; // vector'ü tanımla
cin >> val;
my_vector.push_back(val); // val değerini my_vector vectörüne push edecektir
my_vector.push_back(val); // val değerini yeniden push edecektir (şu an iki elemanı var)

// vector içinde dolaşmak için iki seçenek var:
// ya klasik döngüyle (0. index'ten son index'e kadar iterasyon yaparak)
for (int i = 0; i < my_vector.size(); i++) {
	cout << my_vector[i] << endl; // vector'ün elemanlarına uşamak için [] operatörünü kullanabiliriz
}

// ya da iteratör kulllanarak:
vector<string>::iterator it; // vector için iterator tanımla
for (it = my_vector.begin(); it != my_vector.end(); ++it) {
	cout << *it  << endl;
}

// Set(Küme)
// Set'ler benzersiz(unique) elemanları belirli bir sırada saklayan tutuculardır.
// Set, benzersiz değerleri, herhangi bir fonksiyon veya kod gerektirmeksizin, sıralı olarak

#include<set>
set<int> ST;    // int tipi için set tanımlar
ST.insert(30);  // ST kümesini 30 değerini dahil eder
ST.insert(10);  // ST kümesini 10 değerini dahil eder
ST.insert(20);  // ST kümesini 20 değerini dahil eder
ST.insert(30);  // ST kümesini 30 değerini dahil eder
// Şimdi kümedeki elemanlar aşağıdaki gibidir
//  10 20 30

// Bir eleman silmek için:
ST.erase(20);  // 20 değerine sahip elemanı siler
// Set ST: 10 30
// Iterator kullanarak Set içinde iterasyon yapmak için:
set<int>::iterator it;
for(it=ST.begin();it<ST.end();it++) {
	cout << *it << endl;
}
// Output:
// 10
// 30

// Tutucuyu tamamen silmek için Tutucu_Adi.clear() kullanırız
ST.clear();
cout << ST.size();  // ST kümesinin eleman sayısı(size)nı yazdırır.
// Output: 0

// NOTE: Aynı elemanlari içerebilen kümle için multiset kullanırız

// Map(Harita)
// Map, elemanları anahtar değer, haritalanmış değer şeklinde özel bir sırada saklar.
// anahtar_değer -> haritalanmış_değer

#include<map>
map<char, int> mymap;  // Anahtar char ve değer int olacak şekilde map tanımlar

mymap.insert(pair<char,int>('A',1));
// 1 değeri için A anahtar değerini ekler
mymap.insert(pair<char,int>('Z',26));
// 26 değeri için Z anahtar değerini ekler

// Map'te dolaşma
map<char,int>::iterator it;
for (it=mymap.begin(); it!=mymap.end(); ++it)
    std::cout << it->first << "->" << it->second << '\n';
// Output:
// A->1
// Z->26

// Anahtar'a atanmış değeri bulmak için
it = mymap.find('Z');
cout << it->second;

// Output: 26


/////////////////////////////////////////////
// Mantıksal ve Bit seviyesindeki operatörler
/////////////////////////////////////////////

// Pek çok C++ operatörleri diğer dillerdekiyle aynıdır

// Mantıksal operatörler

// C++, bool ifadelerinde Kısa-devre değerlendirmesini kullanır yani ikinci argüman yalnızca ilk argüman
// ifadenin değerine karar vermek için yeterli değilse çalıştırılır

true && false // **mantıksal ve** işlemi yapılır ve yanlış sonucu üretilir
true || false // **mantıksal veya** işlemi yapılır ve true  sonucu üretilir 
! true        // **mantıksal değil** işlemi yapılır ve yalnış sonucu üretilir

// Sembolleri kullanmak yerine onlara karşılık gelen anahtar kelimeler kullanılabilir
true and false // **mantıksal ve** işlemi yapılır ve yanlış sonucu üretilir
true or false  // **mantıksal veya** işlemi yapılır ve true  sonucu üretilir 
not true       // **mantıksal değil** işlemi yapılır ve yalnış sonucu üretilir

// Bit seviyesindeki operatörler

// **<<** Sola kaydırma operatörü
// << bitleri sola kaydırır
4 << 1 // 4'ün bitlerini 1 sola kaydırır ve 8 sonucunu verir
// x << n, x * 2^n olarak düşünülebilir


// **>>** Sağa kaydırma operatörü
// >> bitleri sağa kaydırır
4 >> 1 // 4'ün bitlerini 1 sağa kaydırır ve 2 sonucunu verir
// x >> n, x / 2^n olarak düşünülebilir

~4    // Bit seviyesinde değil işlemini gerçekleştirir
4 | 3 // Bit seviyesinde veya işlemini gerçekleştirir
4 & 3 // Bit seviyesinde ve işlemini gerçekleştirir
4 ^ 3 // Bit seviyesinde xor işlemini gerçekleştirir

// Eşdeğer anahtar kelimeler
compl 4    // Bit seviyesinde değil işlemini gerçekleştirir
4 bitor 3  // Bit seviyesinde veya işlemini gerçekleştiri
4 bitand 3 // Bit seviyesinde ve işlemini gerçekleştirir
4 xor 3    // Bit seviyesinde xor işlemini gerçekleştirir


```
İleri okuma:

* Güncel bir referans [CPP Reference](http://cppreference.com/w/cpp) adresinde bulunabilir.
* Ek kaynaklar [CPlusPlus](http://cplusplus.com) adresinde bulunabilir.
* Dilin temellerini ve kodlama ortamını belirleyen bir öğretici [TheChernoProject - C ++](https://www.youtube.com/playlist?list=PLlrATfBNZ98dudnM48yfGUldqGD0S4FFb) adresinde bulunabilir.
