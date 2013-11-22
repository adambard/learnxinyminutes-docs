---
language: Objective-C
filename: LearnObjectiveC.m
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
translators:
    - ["Evlogy Sutormin", "http://evlogii.com"]
lang: ru-ru
---

Objective-C — компилируемый объектно-ориентированный язык программирования, используемый корпорацией Apple,
построенный на основе языка Си и парадигм Smalltalk.
В частности, объектная модель построена в стиле Smalltalk — то есть объектам посылаются сообщения.

```cpp
// Однострочный комментарий

/*
Многострочный
комментарий
*/

// Импорт файлов фреймворка Foundation с помощью #import
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Точка входа в программу это функция main,
// которая возвращает целый тип integer
int main (int argc, const char * argv[])
{
    // Создание autorelease pool для управления памятью
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
 
    // Используйте NSLog для печати в консоль
    NSLog(@"Hello World!"); // Напечатает строку "Hello World!"
 
    ///////////////////////////////////////
    // Типы и переменные
    ///////////////////////////////////////
    
    // Простое объявление
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;
    
    // Помещайте * в начало названия объекта для строго типизированного объявления
    MyClass *myObject1 = nil;  // Строгая типизация	
    id       myObject2 = nil;  // Слабая типизация 

    NSLog(@"%@ and %@", myObject1, [myObject2 description]); // напечатает "(null) and (null)"
    // %@ – это объект
    // 'description' это общий для всех объектов метод вывода данных
    
    // Строка
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // напечатает "Hello World!"
    
    // Символьные литералы
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue];
    NSLog(@"%c", theLetterZ);

    // Целочисленный литералы
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue];
    NSLog(@"%i", fortyTwo);
    
    // Беззнаковый целочисленный литерал
    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue];
    NSLog(@"%u", fortyTwoUnsigned);

    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue];
    NSLog(@"%hi", fortyTwoShort);
    
    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue];
    NSLog(@"%li", fortyTwoLong);

    // Вещественный литерал
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue];
    NSLog(@"%f", piFloat);
    
    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble                 = [piDoubleNumber doubleValue];
    NSLog(@"%f", piDouble);

    // BOOL (булевый) литерал
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;

    // Массив
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // Print "Third number = 3"

    // Словарь
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // Напечатает "Object = (null)"

    ///////////////////////////////////////
    // Операторы
    ///////////////////////////////////////
    
    // Операторы работают также как в Си.
    // Например:
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (НЕТ)
    3 != 2; // => 1 (ДА)
    1 && 1; // => 1 (логическое И)
    0 || 1; // => 1 (логическое ИЛИ)
    ~0x0F; // => 0xF0 (побитовое отрицание)
    0x0F & 0xF0; // => 0x00 (побитовое И)
    0x01 << 1; // => 0x02 (побитовый сдвиг влево (на 1))

    ///////////////////////////////////////
    // Структуры ветвления
    ///////////////////////////////////////

    // Условный оператор
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

    // Ветвление с множественным выбором
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
    
    // Цикл с предусловием
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ инкрементирует ii после передачи значения
    } // => напечатает "0," 
      //               "1,"
      //               "2,"
      //               "3,"

    // Цикл со счётчиком
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // => напечатает "0," 
      //               "1,"
      //               "2,"
      //               "3,"
     
    // // Цикл просмотра           
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // => напечатает "0," 
      //               "1,"
      //               "2,"
      //               "3,"

    // Обработка исключений
    @try
    {
        // Ваше исключение здесь
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"File Not Found on System" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally");
    } // => напечатает "Exception: File Not Found on System"
      //               "Finally"
 
    ///////////////////////////////////////
    // Объекты
    ///////////////////////////////////////
    
    // Создание объектов через выделение памяти и инициализацию.
    // Объект не является полнофункциональным пока обе части не выполнятся.
    MyClass *myObject = [[MyClass alloc] init];
        
    // В Objective-C можель ООП базируется на передаче сообщений.
    // В Objective-C Вы не просто вызваете метод; вы посылаете сообщение.
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Очищайте память, перед завершением работы программы.
    [pool drain];
    
    // Конец программы.
    return 0;
}

///////////////////////////////////////
// Классы и функции
///////////////////////////////////////

// Объявляйте свой класс в файле МойКласс.h
// Синтаксис объявления:
// @interface ИмяКласса : ИмяКлассаРодителя <ИмплементируемыеПротоколы>
// {
//    Объявление переменных;
// }
// -/+ (тип) Объявление метода(ов).
// @end


@interface MyClass : NSObject <MyProtocol>
{
    int count;
    id data;
    NSString *name;
}
// При объявлении свойств сразу генерируются геттер и сеттер
@property int count;
@property (copy) NSString *name; // Скопировать объект в ходе присвоения.
@property (readonly) id data;    // Генерация только геттера

// Методы
+/- (return type)methodSignature:(Parameter Type *)parameterName;

// + для методов класса
+ (NSString *)classMethod;

// - для метода объекта
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

@end

// Имплементируйте методы в файле МойКласс.m:

@implementation MyClass

// Вызывается при высвобождении памяти под объектом
- (void)dealloc
{
}

// Конструкторы – это способ  осздания объектов класса.
// Это обычный конструктор вызываемый при создании объекта клсааа.
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

- (NSString *)instanceMethodWithParameter:(NSString *)string
{
    return @"New string";
}

- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number
{
    return @42;
}

// Методы объявленные в МyProtocol (см. далее)
- (void)myProtocolMethod
{
    // имплементация 
}

@end

/*
 * Протокол объявляет методы которые должны быть имплементированы
 * Протокол не является классом. Он просто определяет интерфейс,
 * который должен быть имплементирован.
 */

@protocol MyProtocol
    - (void)myProtocolMethod;
@end
```
## На почитать

[Wikipedia Objective-C](http://en.wikipedia.org/wiki/Objective-C)

[Learning Objective-C](http://developer.apple.com/library/ios/referencelibrary/GettingStarted/Learning_Objective-C_A_Primer/)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)

[iOS разработчик: Обзор книг для новичка](http://habrahabr.ru/post/166213/)

[Хочешь быть iOS разработчиком? Будь им!](http://www.pvsm.ru/ios/12662/print/)
