---
language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
filename: LearnObjectiveC-tr.m
translators:
    - ["Haydar KULEKCI", "http://scanf.info/"]
lang: tr-tr
---

Objective-C Apple tarafından, OSX ve iOS işletim sistemleri ve onların
kendi çatıları olan Cocoa ve Cocoa Touch için kullanılan bir programlama dilidir.
Genel açamlı, object-oriented bir yapıya sahip programlama dilidir. C 
programlama diline Smalltalk stilinde mesajlaşma ekler.  

```objective-c
// Tek satır yorum // işaretleri ile başlar

/*
Çoklu satır yorum bu şekilde görünür.
*/

// #import ile Foundation başlıklarını projeye import edebiliriz. 
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Progarmınızı girişi bir main fonksiyonudur ve bir integer değer döner.
int main (int argc, const char * argv[])
{
    // Programdaki bellek kullanımını kontrol etmek için autorelease bir 
    // oluşturuyoruz. Autorelease bellekte kullanılmayan değerlerin kendi 
    // kendini silmesi demektir.
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
 
    // NSLog konsola bir satırlık bilgi yazdırmak için kullanılır.
    NSLog(@"Hello World!"); // "Hello World!" değeri yazdırılır.
 
    ///////////////////////////////////////
    // Tipler & Değişkenler
    ///////////////////////////////////////
    
    // Basit Tanımlamalar
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;
    
    // Nesne Tanımlamaları
    // strongly-typed nesne tanımlaması için karakter değişken isminin önüne 
    // * karakteri konulur.
    MyClass *myObject1 = nil;  // Strong typing
    id       myObject2 = nil;  // Weak typing
    // %@ bir nesnedir.
    // 'description' objelerin değerlerinin gösterilmesi için bir düzendir.
    NSLog(@"%@ and %@", myObject1, [myObject2 description]); 
            // "(null) and (null)" yazdırılacaktır.
    
    // Karakter Dizisi (String)
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // "Hello World!" yazdırılacaktır.
    
    // Karakterler
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue];
    NSLog(@"%c", theLetterZ);

    // Tamsayılar
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue];
    NSLog(@"%i", fortyTwo);
    
    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue];
    NSLog(@"%u", fortyTwoUnsigned);
    
    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue];
    NSLog(@"%hi", fortyTwoShort);
    
    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue];
    NSLog(@"%li", fortyTwoLong);

    // Kayan Noktalı Sayılar (Floats)
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue];
    NSLog(@"%f", piFloat);
    
    NSNumber *piDoubleNumber = @3.1415926535;
    piDouble                 = [piDoubleNumber doubleValue];
    NSLog(@"%f", piDouble);

    // BOOL Değerler
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;

    // Dizi objeleri
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // "Third number = 3" yazdırılır

    // Dictionary objeleri
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // "Object = (null)" yazıdılır

    ///////////////////////////////////////
    // Operatörler
    ///////////////////////////////////////
    
    // Operatörler C dilindeki gibi çalışır.
    // Örneğin:
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (Logical and)
    0 || 1; // => 1 (Logical or)
    ~0x0F; // => 0xF0 (bitwise negation)
    0x0F & 0xF0; // => 0x00 (bitwise AND)
    0x01 << 1; // => 0x02 (bitwise left shift (by 1))

    ///////////////////////////////////////
    // Kontrol Yapıları
    ///////////////////////////////////////

    // If-Else ifadesi
    if (NO)
    {
        NSLog(@"I am never run");
    } else if (0)
    {
        NSLog(@"I am also never run");
    } else
    {
        NSLog(@"I print");
    }

    // Switch ifadesi
    switch (2)
    {
        case 0:
        {
            NSLog(@"I am never run");
        } break;
        case 1:
        {
            NSLog(@"I am also never run");
        } break;
        default:
        {
            NSLog(@"I print");
        } break;
    }
    
    // While döngü ifadesi
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++, ii değişkenini kullanıldıktan 
                             //sonra yerinde artırır.
    } // =>        "0," 
      //           "1,"
      //           "2,"
      //           "3," yazdırılır

    // For döngü ifadesi
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj++);
    } // =>        "0," 
      //           "1,"
      //           "2,"
      //           "3," yazdırılır
     
    // Foreach ifadesi             
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // =>        "0," 
      //           "1,"
      //           "2,"
      //           "3," yazdırılır

    // Try-Catch-Finally ifadesi
    @try
    {
        // İfadelerinizi buraya yazın
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"Sistemde Dosya Bulunamadı" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally");
    } // =>        "Exception: Sistemde Dosya Bulunamadı"
      //           "Finally"
      // yazdırılacaktır
 
    ///////////////////////////////////////
    // Objeler
    ///////////////////////////////////////
    
    // Bellekten bir alan ayırmak ve objeyi burada oluşturmak bir obje örneği 
    // oluşturalım. Bir obje allocate ve init aşamalarını bitirmeden tam olarak
    // işlevsel değildir.
    MyClass *myObject = [[MyClass alloc] init];
    
    // Objective-C nesne yönelimli programlama modelinin temelinde objelere 
    // mesaj gönderme vardır.
    // Objective-C'de bir method çağırılmaz, ona bir mesaj gönderilir.
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Programda kullanılan bellek temizlenir
    [pool drain];
    
    // Program Sonu
    return 0;
}

///////////////////////////////////////
// Sınıflar ve Fonksiyonlar
///////////////////////////////////////

// Sınıfınızı (MyClass.h) header dosyasında tanımlayın:

// Sınıf tanımlama yapısı:
// @interface ClassName : ParentClassName <ImplementedProtocols>
// {
//    Üye değişken (member variable) tanımlaması;
// }
// -/+ (type) Method tanımlaması;
// @end
@interface MyClass : NSObject <MyCustomProtocol>
{
    int count;
    id data;
    NSString *name;
}
// getter ve setter için otomatik oluşturulmuş gösterim.
@property int count;
@property (copy) NSString *name; // Copy the object during assignment.
@property (readonly) id data;    // Declare only a getter method.

// Metodlar
+/- (return type)methodSignature:(Parameter Type *)parameterName;

// "+" class metodları içindir
+ (NSString *)classMethod;

// "-" instance metodu içindir
- (NSString *)instanceMethodWithParmeter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

@end

// Metodların implementasyonlarını (MyClass.m) dosyasında yapıyoruz:

@implementation UserObject

// Obje bellekten silineceği (release) zaman çağırılır
- (void)dealloc
{
}

// Constructor'lar sınıf oluşturmanın bir yoludur
// Bu varsayılan bir constructor'dur ve bir obje oluşturulurken çağrılır.
- (id)init
{
    if ((self = [super init]))
    {
        self.count = 1;
    }
    return self;
}

+ (NSString *)classMethod
{
    return [[self alloc] init];
}

- (NSString *)instanceMethodWithParmeter:(NSString *)string
{
    return @"New string";
}

- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number
{
    return @42;
}

// MyProtocol içerisinde metod tanımlamaları
- (void)myProtocolMethod
{
    // ifadeler
}

@end

/*
 * Bir `protocol` herhangi bir sınıf tarafından implement edilen metodları tanımlar 
 * `Protocol`ler sınıfların kendileri değildir. Onlar basitçe diğer objelerin 
 * implementasyon için sorumlu oldukları bir arayüz (interface) tanımlarlar. 
 */
@protocol MyProtocol
    - (void)myProtocolMethod;
@end



```
## Daha Fazla Okuma

[Vikipedi Objective-C](http://tr.wikipedia.org/wiki/Objective-C)

[Objective-C Öğrenme](http://developer.apple.com/library/ios/referencelibrary/GettingStarted/Learning_Objective-C_A_Primer/)

[Lise Öğrencileri için iOS: Başlangıç](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
