---
name: c
category: language
language: c
filename: learnc-tr.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Haydar KULEKCI", "http://scanf.info/"]
lang: tr-tr

---
/*
C halen modern yüksek performans bilgisayarların dili.

C bir çok programcının kullandığı en düşük seviye dillerdendir, ama
salt hız ile daha fazlasını karşılar. C'nin bellek yönetiminden iyi
anlarsanız sizi isteğiniz yere götürecektir.

```c
// Tek satır yorum // karakterleri ile başlar

/*
Çoklu satırlı yorumlar bu şekilde görünür.
*/

// C Standart kütüphanelerini uygulamanıza #include<ornek.h> ile 
// dahil edebilirsiniz.
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// Kendi başlık(header) dosyalarınız dahil etmek için "çift tırnak" 
// kullanmalısınız.
#include "my_header.h"

// Fonksiyonlarınızı bir .h dosyasında ya da c dosyanızın üst tarafta 
// tanımlayın.

void function_1();
void function_2();

// Programınızın giriş noktası main isimli bir fonksiyondur ve 
// integer değer döner
int main() {

    // çıktıları yazdırmak için printf kullanılır, "print formatted"
    // %d bir sayı tipidir, \n yeni satır karakteridir
    printf("%d\n", 0); // => 0 karakteri yazdırılır.
    // Tüm ifadeler noktalı virgül ile bitmelidir.

    ///////////////////////////////////////
    // Tipler
    ///////////////////////////////////////

    // Değişkenleri kullanmadan önce tanımlamalısınız. Bir değişken tanımlarken
    // tipini belirtmelisiniz; bu tip onun byte olarak boyutunu belirler.

    // int değişken tipi 4 byte boyutundadır.
    int x_int = 0;

    // short değişken tipi genellikle 2 byte boyutundadır.
    short x_short = 0;

    // char tipi 1 byte boyutunu garanti eder. 
    char x_char = 0;
    char y_char = 'y'; // Karakterler '' işaretleri arasına yazılır.

    // long tipi 4-8 byte olur; long long tipi en azından 64 bit garantiler.
    long x_long = 0;
    long long x_long_long = 0; 

    // float tipi 32-bit kayan noktalı sayı boyutundadır.
    float x_float = 0.0;

    // double değişken tipi 64-bit kayan noktalı yazı tipindedir. 
    double x_double = 0.0;

    // Integral türleri işaretsiz olabilir. Bunun anlamı, onlar eksi değer 
    // olamaz demektir, ama aynı boyuttaki işaretsiz bir sayının maksimum 
    // değeri işaretli bir sayının maksimum değeriden büyük olur.
    unsigned char ux_char;
    unsigned short ux_short;
    unsigned int ux_int;
    unsigned long long ux_long_long;

    // Diğer taraftan char, ki her zaman bir byte boyutundadır, bu tipler 
    // makinenize göre boyut değiştirir. sizeof(T) size bir değişkenin byte 
    // cinsinden boyutunu verir öyle ki bu tipin boyutunu taşınabilir bir 
    // şekilde ifade edilebilir
    // Örneğin,
    printf("%lu\n", sizeof(int)); // => 4 (bir çok makinede 4-byte words)

    // Eger arguman düzenli ifae olan sizeof operatoru ise degerlendirilmez.
    // VLAs hariç asagiya bakiniz).
    // Bu durumda verimliligin degeri derleme-zamani sabitidir.
    int a = 1;

    // size_t bir objeyi temsil etmek için kullanılan 2 byte uzunluğundaki bir 
    // işaretsiz tam sayı tipidir

    size_t size = sizeof(a++); // a++ is not evaluated
    printf("sizeof(a++) = %zu where a = %d\n", size, a);
    // yazdirilan "sizeof(a++) = 4 where a = 1" (32-bit mimaride)

    // Diziler somut bir boyut ile oluşturulmalıdır.
    char my_char_array[20]; // Bu dizi 1 * 20 = 20 byte alan kaplar
    int my_int_array[20]; // Bu dizi 4 * 20 = 80 byte alan kaplar
                          // (4-byte bir word varsayılır)

    // Şu şekilde bir diziyi 0 ile oluşturabilirsiniz:
    char my_array[20] = {0};

    // Dizinin elemanlarını indexlemek diğer diller gibidir, veya 
    // diğer diller C gibi.
    my_array[0]; // => 0

    // Diziler değişebilirdir (mutable); O sadece memory!
    my_array[1] = 2;
    printf("%d\n", my_array[1]); // => 2

    // C99'da (ve C11 istege bagli bir ozellik olarak), değidken-uzunluklu diziler (VLAs) bildirilebilirler.
    // Böyle bir dizinin boyuunu derlenmesi gerekmez
    // zaman sabiti:
    printf("Enter the array size: "); // dizi boyutu kullaniciya soruluyor
    char buf[0x100];
    fgets(buf, sizeof buf, stdin);

    // strtoul isaretsiz integerlar icin string ayiricisidir.
    size_t size = strtoul(buf, NULL, 10);
    int var_length_array[size]; // declare the VLA
    printf("sizeof array = %zu\n", sizeof var_length_array);

    // Bu programın olası bir sonucu olabilir:
    // > Enter the array size: 10
    // > sizeof array = 40

    // String'ler bir NUL (0x00) byte ile sonlandırılmış karakter dizileridir,
    // bu string içerisinde özel bir karakter olan '\0' ile gösterilir.
    // (Biz Nul byte'i string karakterleri arasında bulundurmamıza gerek 
    // yoktur; derleyici onu bizim için dizinin sonuna ekler.)
    char a_string[20] = "This is a string";
    printf("%s\n", a_string); // %s bir string formatıdır.

    /*
    a_string 16 karakter uzunluğundadır. 
    17. karakter NUL karakteridir.
    18., 19. ve 20. karakterler tanımsızdır.(undefined)
    */

    printf("%d\n", a_string[16]); // => 0
    // i.e., byte #17 is 0 (as are 18, 19, and 20)

    // Tek tirnak arasinda karakterlere sahipsek, bu karakterler degismezdir.
    // Tip `int` ise, `char` *degildir* (tarihsel sebeplerle).
    int cha = 'a'; // fine
    char chb = 'a'; // fine too (implicit conversion from int to char)

    ///////////////////////////////////////
    // Operatörler
    ///////////////////////////////////////

    int i1 = 1, i2 = 2; // Çoklu tanımlama için kısayol.
    float f1 = 1.0, f2 = 2.0;

    // Aritmatik basittir.
    i1 + i2; // => 3
    i2 - i1; // => 1
    i2 * i1; // => 2
    i1 / i2; // => 0 (0.5'dir ama 0 a yuvarlanmıştır.)

    f1 / f2; // => 0.5, artı veya eksi epsilon

    // Modüler aritmetikte vardır.
    11 % 3; // => 2

    // Karşılaştırma operatörleri muhtemelen tanıdıktır, ama
    // C'de boolean tipi yoktur. Bunun yerine sayı(int) kullanırız.
    // 0 false yerine ve diğer herşey true yerine geçmektedir. 
    // (Karşılaştırma operatörleri her zaman 0 veya 1 dönmektedir.)
    3 == 2; // => 0 (false)
    3 != 2; // => 1 (true)
    3 > 2; // => 1
    3 < 2; // => 0
    2 <= 2; // => 1
    2 >= 2; // => 1

    // Sayılar üzerinde mantık işlemleri
    !3; // => 0 (Logical not)
    !0; // => 1
    1 && 1; // => 1 (Logical and)
    0 && 1; // => 0
    0 || 1; // => 1 (Logical or)
    0 || 0; // => 0

    // Bit boyutunda işlem yapmak için operatörler
    ~0x0F; // => 0xF0 (bitwise negation)
    0x0F & 0xF0; // => 0x00 (bitwise AND)
    0x0F | 0xF0; // => 0xFF (bitwise OR)
    0x04 ^ 0x0F; // => 0x0B (bitwise XOR)
    0x01 << 1; // => 0x02 (bitwise left shift (by 1))
    0x02 >> 1; // => 0x01 (bitwise right shift (by 1))

    // Isaretli sayilari kaydirirken dikkatli olun - tanimsizlar sunlardir:
    // - isaretli sayinin isaret bitinde yap?ilan kaydirma (int a = 1 << 32)
    // - negatif sayilarda sol kaydirma (int a = -1 << 2)
    // - LHS tipinde >= ile olan ofset genisletmelerde yapilan kaydirma:
    //   int a = 1 << 32; // UB if int is 32 bits wide

    ///////////////////////////////////////
    // Kontrol Yapıları
    ///////////////////////////////////////

    if (0) {
      printf("I am never run\n");
    } else if (0) {
      printf("I am also never run\n");
    } else {
      printf("I print\n");
    }

    // While Döngüsü
    int ii = 0;
    while (ii < 10) {
        printf("%d, ", ii++); // ii++, ii değişkenini değerini kullandıktan sonra artırır.
    } // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    int kk = 0;
    do {
        printf("%d, ", kk);
    } while (++kk < 10); // ++kk, kk değişkeninin değerini kullanmadan önce artırır.
    // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    // For Döngüsü
    int jj;
    for (jj=0; jj < 10; jj++) {
        printf("%d, ", jj);
    } // => prints "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    ///////////////////////////////////////
    // Tip Dönüşümleri
    ///////////////////////////////////////

    // C'de her değer bir tipe sahiptir, ama siz bir değeri bir başka tipe 
    // dönüştürebilirsiniz.

    int x_hex = 0x01; // Hex literatüründe değer atayabilirsiniz.

    // Türler arasındaki dönüşümde kendi değerini korumak için çalışacaktır.
    printf("%d\n", x_hex); // => Prints 1
    printf("%d\n", (short) x_hex); // => Prints 1
    printf("%d\n", (char) x_hex); // => Prints 1

    // Tip hiçbir hata vermeden taşacaktır(overflow).
    printf("%d\n", (unsigned char) 257); // => 1 (Max char = 255 eğer karakter 8 bit uzunluğunda ise)

    // `char`, `signed char` ve `unsigned char` karakter tiplerinin maksimum uzunluğunu 
    // belirlemek için <limits.h> kütüphanesindeki CHAR_MAX, SCHAR_MAX ve UCHAR_MAX 
    // macrolarını kullanınız.

    // Integral tipi kayan noktalı yazı tipine dönüştürülecektir ve tam tersi. 
    printf("%f\n", (float)100); // %f formats a float
    printf("%lf\n", (double)100); // %lf formats a double
    printf("%d\n", (char)100.0);

    ///////////////////////////////////////
    // İşaretçiler (Pointers)
    ///////////////////////////////////////

    // Bir işaretci bellek adresini barındıran bir değişkendir. Tanımlaması ile işaret 
    // edeceği verinin tipi de belirtilecektir. Değişkenlerininzi bellek adreslerini 
    // getirerek bellek ile ilgili karışıklığı ortadan kaldırabilirsiniz. 

    int x = 0;
    printf("%p\n", &x); // & işareti bir değişkenin bellek adresini getirmek için kullanılır. 
    // (%p işaretçilerin formatıdır)
    // => Bazı bellek adresleri yazdırılacaktır.


    // İşaretçiler tanımlanırken * ile başlar
    int *px, not_a_pointer; // px sayı tipinde bir işaretçidir. 
    px = &x; // X değişkeninin bellek adresi px değişkeninde tutulmaktadır.
    printf("%p\n", px); // => x değişkeninin bellek adresi yazdırılacaktır. 
    printf("%d, %d\n", (int)sizeof(px), (int)sizeof(not_a_pointer));
    // => 64-bit sistemde "8, 4" yazdırılacaktır.

    // İşaretçinin adres olarak gösterdiği yerdeki değeri almak için 
    // değişkenin önüne * işareti ekleyiniz.
    printf("%d\n", *px); // => 0 bastıracaktır, x in değeridir, 
                         // çünkü px değişkeni x in adresini göstermektedir.

    // Ayrıca siz işaretçinin gösterdiği yerin değerini 
    // değiştirebilirsiniz. Burada referansı parantez içerisinde göstereceğiz 
    // çünkü ++ işleminin önceliği * işleminden yüksektir.
    (*px)++; // px'in işaret ettiği değeri 1 artır. 
    printf("%d\n", *px); // => 1 yazdırılır.
    printf("%d\n", x); // => 1 yazdırılır.

    int x_array[20]; // Diziler(arrays) bellekten yan yana bellek bloklarını 
                     // tahsis etmek için iyi bir yöntemdir.
    int xx;
    for (xx=0; xx<20; xx++) {
        x_array[xx] = 20 - xx;
    } // x_array dizisi 20, 19, 18,... 2, 1 değerleri ile oluşturuluyor.

    // Bir sayı tipinde işaretçi tanımlanıyor ve x_array'i işaret ediyor.
    int* x_ptr = x_array;
    // x_ptr artık dizinin ilk elemanını işaret etmektedir (the integer 20).
    // Bu çalışacaktır çünkü diziler(arrays) aslında sadece onların ilk 
    // elemanlarını gösteren birer işaretçidir.
    // For example, when an array is passed to a function or is assigned to a pointer,
    // it decays into (implicitly converted to) a pointer.
    // Exceptions: when the array is the argument of the `&` (address-od) operator:
    int arr[10];
    int (*ptr_to_arr)[10] = &arr; // &arr is NOT of type `int *`!
                                  // It's of type "pointer to array" (of ten `int`s).
    // or when the array is a string literal used for initializing a char array:
    char arr[] = "foobarbazquirk";
    // or when it's the argument of the `sizeof` or `alignof` operator:
    int arr[10];
    int *ptr = arr; // equivalent with int *ptr = &arr[0];
    printf("%zu %zu\n", sizeof arr, sizeof ptr); // probably prints "40, 4" or "40, 8"

    // Diziler ilk elemanlarını gösteren birer işaretçidirler.
    printf("%d\n", *(x_ptr)); // => 20 yazılacaktır.
    printf("%d\n", x_array[0]); // => 20 yazılacaktır.

    // İşaretçiler kendi tiplerinde artırılır ve azaltılır. 
    printf("%d\n", *(x_ptr + 1)); // => 19 yazılacaktır.
    printf("%d\n", x_array[1]); // => 19 yazılacaktır.

    // Ayrıca dinamik olarak bir bellek bloğunu standart kütüphanede bulunan 
    // malloc fonksiyonu ile uygulamanız için ayırabilirsiniz. Bu fonksiyon 
    // byte türünden ayırmak istediğiniz bloğun boyutunu parametre olarak alır. 
    int* my_ptr = (int*) malloc(sizeof(int) * 20);
    for (xx=0; xx<20; xx++) {
        *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx 'de aynı zamanda çalışabilir
    } // Bellekte 20, 19, 18, 17... 2, 1 (as ints) şeklinde oluşturulmuş olacaktır.

    // Eğer ayrımadığınız bir bellek adresini çağırırsanız
    // öngörülmeyen bir değer dönecektir. 
    printf("%d\n", *(my_ptr + 21)); // => kim-bilir-ne-yazacak?

    // Malloc fonksiyonu ile ayrıdığınız bellek kısmı ile işiniz bittiğinde 
    // onu free fonksiyonu ile boşaltmalısınız, aksi durumda uygulamanız 
    // kapatılana kadar belleğin o kısmını kimse kullanamaz. 
    free(my_ptr);

    // Metin Dizileri(String) birer karakter dizisidir(char array), ama
    // genelde karakter işaretçisi olarak kullanılır.
    char* my_str = "This is my very own string";

    printf("%c\n", *my_str); // => 'T'

    function_1();
} // main fonksiyon sonu

///////////////////////////////////////
// Fonksiyonlar
///////////////////////////////////////

// Fonksiyon tanımlama sözdizimi:
// <return type> <fonksiyon adi>(<args>)

int add_two_ints(int x1, int x2){
    return x1 + x2; // bir değer geri döndürmek için return kullanılır.
}

/*
Fonksiyonlar pass-by-value'dür, ama isterseniz işaretçi referanslarını 
kullanarak fonksiyona gönderilen parametrenin değerini değiştirebilirsiniz. 

Example: Bir metni tersine çevirme
*/

// Bir void konksiyonu hiç bir değer dönmez
void str_reverse(char* str_in){
    char tmp;
    int ii=0, len = strlen(str_in); // Strlen C'nin standart kütüphanesinin bir fonksiyonu
    for(ii=0; ii<len/2; ii++){
        tmp = str_in[ii];
        str_in[ii] = str_in[len - ii - 1]; // sondan ii'inci elemanı
        str_in[len - ii - 1] = tmp;
    }
}

/*
char c[] = "This is a test.";
str_reverse(c);
printf("%s\n", c); // => ".tset a si sihT"
*/

///////////////////////////////////////
// Kullanıcı Tanımlı Tipler ve Yapılar
///////////////////////////////////////

// Typedef'ler bir tip takma adı oluşturur.
typedef int my_type;
my_type my_type_var = 0;

// Struct'lar bir veri koleksiyonudur. 
struct rectangle {
    int width;
    int height;
};

// It's not generally true that
// sizeof(struct rectangle) == sizeof(int) + sizeof(int)
// due to potential padding between the structure members (this is for alignment
// reasons). [1]

void function_1(){

    struct rectangle my_rec;

    // "." ile yapı üyelerine ulaşılabilir.
    my_rec.width = 10;
    my_rec.height = 20;

    // Bir yapının adresini işaretçiye atayabilirsiniz.
    struct rectangle* my_rec_ptr = &my_rec;

    // İşaretçi üzerinden bir yapıya ulaşabilirsiniz. 
    (*my_rec_ptr).width = 30;

    // ya da -> işareti ile yapının elemanlarına ulaşabilirsiniz. 
    my_rec_ptr->height = 10; // (*my_rec_ptr).height = 10; ile aynıdır.
}

// Kolaylık sağlamak için bir yapıya typedef tanımlayabilirsiniz. 
typedef struct rectangle rect;

int area(rect r){
    return r.width * r.height;
}

///////////////////////////////////////
// Fonksiyon İşaretçiler
///////////////////////////////////////

/*
Çalışma zamanında, fonksiyonların bilinen bir bellek adresleri vardır. Fonksiyon
işaretçileri fonksiyonları direk olarak çağırmayı sağlayan veya geri bildirim(callback)
için parametre gönderirken kullanılan başka birer işaretçidirler. Ama, syntax tanımı 
başlangıçta biraz karışık gelebilir. 

Örnek: bir işaretçiden str_reverse kullanımı
*/
void str_reverse_through_pointer(char * str_in) {
    // f adında bir fonksiyon işaretçisi tanımlanır.
    void (*f)(char *); // Signature should exactly match the target function.
    f = &str_reverse; // Assign the address for the actual function (determined at runtime)
    (*f)(str_in); // Just calling the function through the pointer
    // f(str_in); // That's an alternative but equally valid syntax for calling it.
}

/*
As long as function signatures match, you can assign any function to the same pointer.
Function pointers are usually typedef'd for simplicity and readability, as follows:
*/

typedef void (*my_fnp_type)(char *);

// Gerçek bir işaretçi tanımlandığı zaman ki kullanımı:
// ...
// my_fnp_type f; 

```

## Daha Fazla Okuma Listesi

[K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language)'in bir kopyasını bulundurmak mükemmel olabilir

Diğer bir iyi kaynak ise [Learn C the hard way](http://c.learncodethehardway.org/book/)

It's very important to use proper spacing, indentation and to be consistent with your coding style in general.
Readable code is better than clever code and fast code. For a good, sane coding style to adopt, see the
[Linux kernel coding stlye](https://www.kernel.org/doc/Documentation/CodingStyle).

Diğer taraftan google sizin için bir arkadaş olabilir.

[1] http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member
