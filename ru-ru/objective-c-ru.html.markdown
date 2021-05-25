---
language: Objective-C
filename: LearnObjectiveC-ru.m
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
    - ["Levi Bostian", "https://github.com/levibostian"]
translators:
    - ["Evlogy Sutormin", "http://evlogii.com"]
    - ["Dmitry Bessonov", "https://github.com/TheDmitry"]
lang: ru-ru
---

Objective-C — основной язык программирования, используемый корпорацией Apple
для операционных систем macOS и iOS и их соответствующих фреймворках Cocoa и
Cocoa Touch.
Он является объектно-ориентированным языком программирования общего назначения,
который добавляет обмен сообщениями в Smalltalk-стиле к языку программирования C.

```objective-c
// Однострочные комментарии начинаются с //

/*
Так выглядят многострочные комментарии
*/

// Импорт заголовочных файлов фреймворка Foundation с помощью #import
// Используйте <>, чтобы импортировать глобальные файлы (обычно фреймворки)
// Используйте "", чтобы импортировать локальные файлы (из проекта)
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Если вы включили модули для iOS >= 7.0 или OS X >= 10.9 проектов в
// Xcode 5, вы можете импортировать фреймворки подобным образом:
@import Foundation;

// Точка входа в программу - это функция main,
// которая возвращает целый тип
int main (int argc, const char * argv[])
{
    // Создание autorelease pool для управления памятью в программе
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // В место этого воспользуйтесь @autoreleasepool, если вы используете
    // автоматический подсчет ссылок (ARC)
    @autoreleasepool {
    
    // Используйте NSLog для печати в консоль
    NSLog(@"Привет Мир!"); // Напечатает строку "Привет Мир!"
 
    ///////////////////////////////////////
    // Типы и переменные
    ///////////////////////////////////////
    
    // Объявление простых типов
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;
    
    // Объявление объектов
    // Помещайте * в начало названия объекта для строго типизированного объявления
    MyClass *myObject1 = nil;  // Строгая типизация	
    id       myObject2 = nil;  // Слабая типизация 
    // %@ – это объект
    // 'description' - это общий для всех объектов метод вывода данных
    NSLog(@"%@ and %@", myObject1, [myObject2 description]); // напечатает "(null) and (null)"
    
    // Строка
    NSString *worldString = @"Мир";
    NSLog(@"Привет %@!", worldString); // напечатает "Привет Мир!"
    // NSMutableString - это изменяемая версия NSString-объекта
    NSMutableString *mutableString = [NSMutableString stringWithString:@"Привет"];
    [mutableString appendString:@" Мир!"];
    NSLog(@"%@", mutableString); // напечатает => "Привет Мир!"
    
    // Символьные литералы
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue]; // или 'Z'
    NSLog(@"%c", theLetterZ);

    // Целочисленные литералы
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue]; // или '42'
    NSLog(@"%i", fortyTwo);
    
    // Беззнаковый целочисленный литерал
    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue]; // или 42
    NSLog(@"%u", fortyTwoUnsigned);

    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue]; // или 42
    NSLog(@"%hi", fortyTwoShort);
    
    NSNumber *fortyOneShortNumber   = [NSNumber numberWithShort:41];
    unsigned short fortyOneUnsigned = [fortyOneShortNumber unsignedShortValue]; // или 41
    NSLog(@"%u", fortyOneUnsigned);
    
    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue]; // или 42
    NSLog(@"%li", fortyTwoLong);
    
    NSNumber *fiftyThreeLongNumber   = @53L;
    unsigned long fiftyThreeUnsigned = [fiftyThreeLongNumber unsignedLongValue]; // или 53
    NSLog(@"%lu", fiftyThreeUnsigned);

    // Вещественный литерал
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue]; // или 3.141592654f
    NSLog(@"%f", piFloat); // напечатает 3.141592654
    NSLog(@"%5.2f", piFloat); // напечатает " 3.14"
    
    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble                 = [piDoubleNumber doubleValue]; // или 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // напечатает "3.14"

    // NSDecimalNumber - это класс с фиксированной точкой, который является
    // более точным, чем float или double
    NSDecimalNumber *oneDecNum = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *twoDecNum = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimalNumber не способен использовать стандартные +, -, *, / операторы,
    // поэтому он предоставляет свои собственные:
    [oneDecNum decimalNumberByAdding:twoDecNum]; 
    [oneDecNum decimalNumberBySubtracting:twoDecNum];
    [oneDecNum decimalNumberByMultiplyingBy:twoDecNum];
    [oneDecNum decimalNumberByDividingBy:twoDecNum];
    NSLog(@"%@", oneDecNum); // напечатает "10.99", т.к. NSDecimalNumber - изменяемый
    
    // BOOL (булевый) литерал
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;
    // или
    BOOL yesBool = YES;
    BOOL noBool  = NO;
    NSLog(@"%i", yesBool); // напечатает 1
    
    // Массив
    // Может содержать различные типы данных, но должен быть объектом Objective-C
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Третье число = %@", thirdNumber); // Напечатает "Третье число = 3"
    // NSMutableArray - это изменяемая версия NSArray, допускающая вам изменять
    // элементы в массиве и расширять или сокращать массив.
    // Удобный, но не эффективный как NSArray.
    NSMutableArray *mutableArray = [NSMutableArray arrayWithCapacity:2];
    [mutableArray addObject:@"Привет"];
    [mutableArray addObject:@"Мир"];
    [mutableArray removeObjectAtIndex:0];
    NSLog(@"%@", [mutableArray objectAtIndex:0]); // напечатает "Мир"

    // Словарь
    NSDictionary *aDictionary = @{ @"ключ1" : @"значение1", @"ключ2" : @"значение2" };
    NSObject *valueObject     = aDictionary[@"Ключ"];
    NSLog(@"Объект = %@", valueObject); // Напечатает "Объект = (null)"
    // NSMutableDictionary тоже доступен, как изменяемый словарь
    NSMutableDictionary *mutableDictionary = [NSMutableDictionary dictionaryWithCapacity:2];
    [mutableDictionary setObject:@"значение1" forKey:@"ключ1"];
    [mutableDictionary setObject:@"значение2" forKey:@"ключ2"];
    [mutableDictionary removeObjectForKey:@"ключ1"];

    // Множество
    NSSet *set = [NSSet setWithObjects:@"Привет", @"Привет", @"Мир", nil];
    NSLog(@"%@", set); // напечатает {(Hello, World)} (порядок может отличаться)
    // NSMutableSet тоже доступен, как изменяемое множество
    NSMutableSet *mutableSet = [NSMutableSet setWithCapacity:2];
    [mutableSet addObject:@"Привет"];
    [mutableSet addObject:@"Привет"];
    NSLog(@"%@", mutableSet); // напечатает => {(Привет)}
    
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
        NSLog(@"Я никогда не выполнюсь");
    } else if (0)
    {
        NSLog(@"Я тоже никогда не выполнюсь");
    } else
    {
        NSLog(@"Я напечатаюсь");
    }

    // Ветвление с множественным выбором
    switch (2)
    {
        case 0:
        {
            NSLog(@"Я никогда не выполнюсь");
        } break;
        case 1:
        {
            NSLog(@"Я тоже никогда не выполнюсь");
        } break;
        default:
        {
            NSLog(@"Я напечатаюсь");
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
     
    // Цикл просмотра           
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // => напечатает "0," 
      //               "1,"
      //               "2,"
      //               "3,"

    // Цикл for для объектов. Может использоваться с любым объектом Objective-C
    for (id item in values) { 
        NSLog(@"%@,", item); 
    } // напечатает => "0," 
      //           "1,"
      //           "2,"
      //           "3,"      
      
    // Обработка исключений
    @try
    {
        // Ваше исключение здесь
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"Файл не найден в системе" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Исключение: %@", e);
    } @finally
    {
        NSLog(@"В конце отводится время для очистки.");
    } // => напечатает "Исключение: Файл не найден в системе"
      //               "В конце отводится время для очистки."
 
    // NSError - это полезные объекты для аргументов функции, чтобы заполнить их
    // пользовательскими ошибками.
    NSError *error = [NSError errorWithDomain:@"Неправильный эл. адрес." code:4 userInfo:nil];
 
    ///////////////////////////////////////
    // Объекты
    ///////////////////////////////////////
    
    // Создание объектов через выделение памяти и инициализацию.
    // Объект не является полнофункциональным пока обе части не выполнятся.
    MyClass *myObject = [[MyClass alloc] init];
        
    // В Objective-C модель ООП базируется на передаче сообщений.
    // В Objective-C Вы не просто вызваете метод; вы посылаете сообщение.
    [myObject instanceMethodWithParameter:@"Стив Джобс"];

    // Очищайте память, перед завершением работы программы.
    [pool drain];
    
    // Конец @autoreleasepool
    }
    
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
//    тип имя; <= Объявление переменных;
// }
// @property тип имя; <= объявление свойств
// -/+ (тип) Объявление метода(ов).
// @end
@interface MyClass : NSObject <MyProtocol> // NSObject - это базовый класс в Objective-C.
{
    // Объявления экземпляров переменных (может существовать в файлах интерфейса или реализвации)
    int count; // По умолчанию защищенный доступ.
    @private id data; // Приватный доступ (Намного удобнее объявлять в файле реализации)
    NSString *name;
}
// Удобное обозначение для переменных с открытым (public) доступом
// автоматически генерируется сеттер-метод
// По умолчанию название сеттер-метода начинается с 'set' с последующим именем
// переменной из @property
@property int propInt; // Имя сеттер-метода = 'setPropInt'
@property (copy) id copyId; // (copy) => Скопировать объект в ходе присвоения.
// (readonly) => Не позволяет установить значение вне @interface
@property (readonly) NSString *roString; // Используйте @synthesize 
                               // в @implementation, чтобы создать аксессор
// Вы можете настроить геттер и сеттер имена вместо используемого 'set'-имени по умолчанию:
@property (getter=lengthGet, setter=lengthSet:) int length;

// Методы
+/- (возвращаемый тип)сигнатураМетода:(Параметр типа *)имяПараметра;

// + для методов класса
+ (NSString *)classMethod;
+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight;

// - для методов объекта
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

// Методы-конструктор с аргументом:
- (id)initWithDistance:(int)defaultDistance;
// В Objective-C имена методов очень описательные. Всегда имена методов соответствуют своим аргументам

@end // Устанавливает конец интерфейса (interface)


// Чтобы обратиться к открытым (public) переменным из файла реализации, @property генерирует сеттер-метод
// автоматически. Название метода - это 'set' с последующим именем переменной из @property:
MyClass *myClass = [[MyClass alloc] init]; // создает экземпляр объекта класса MyClass
[myClass setCount:10]; 
NSLog(@"%d", [myClass count]); // напечатает => 10
// Или используйте свой геттер и сеттер методы, которые определены в @interface:
[myClass lengthSet:32];
NSLog(@"%i", [myClass lengthGet]); // напечатает => 32
// Для удобства вы можете использовать точечную нотацию,
// чтобы установить и получить доступ к переменным объекта:
myClass.count = 45;
NSLog(@"%i", myClass.count); // напечатает => 45

// Вызов методов класса:
NSString *classMethodString = [MyClass classMethod];
MyClass *classFromName = [MyClass myClassFromName:@"Привет"];

// Вызов методов экземпляра:
MyClass *myClass = [[MyClass alloc] init]; // Создает экземпляр объекта MyClass
NSString *stringFromInstanceMethod = [myClass instanceMethodWithParameter:@"Привет"];

// Селекторы
// Это способ динамически представить методы. Используйте для вызова методов класса, передайте методы
// через функции, чтобы сказать другим классам, что они должны вызвать их и сохранить методы
// как переменные
// SEL - это тип данных. @selector() вернет селектор из предоставленного имени метода
// methodAParameterAsString:andAParameterAsNumber: - это название метода в MyClass
SEL selectorVar = @selector(methodAParameterAsString:andAParameterAsNumber:); 
if ([myClass respondsToSelector:selectorVar]) { // Проверяет содержит ли класс метод
    // Необходимо установить все аргументы метода в один объект, что отправить его в performSelector-функцию
    NSArray *arguments = [NSArray arrayWithObjects:@"Привет", @4, nil];
    [myClass performSelector:selectorVar withObject:arguments]; // Вызывает метод
} else {
    // NSStringFromSelector() вернет NSString название метода полученного селектором
    NSLog(@"MyClass не содержит метод: %@", NSStringFromSelector(selectedVar));
}

// Имплементируйте методы в файле MyClass.m:
@implementation MyClass {
    long distance; // Переменная экземпляра с закрытым (private) доступом
    NSNumber height;
}

// Для доступа к public переменной, объявленной в интерфейсе, используйте '_' перед названием переменной:
_count = 5; // Ссылается на "int count" из интерфейса MyClass
// Получение доступа к переменной, объявленной в реализации происходит следующим образом:
distance = 18; // Ссылается на "long distance" из реализации MyClass
// Для использования в иплементации переменной, объявленной в интерфейсе с помощью @property,
// следует использовать @synthesize для создания переменной аксессора:
@synthesize roString = _roString; // Теперь _roString доступна в @implementation (реализации интерфейса)

// Вызывается в первую очередь, перед вызовом других медотов класса или инициализации других объектов
+ (void)initialize 
{
    if (self == [MyClass class]) {
        distance = 0;
    }
}

// Вызывается при высвобождении памяти под объектом
- (void)dealloc
{
    [height release]; // Если не используется ARC, убедитесь в освобождении переменных объекта класса
    [super dealloc];  // and call parent class dealloc
}

// Конструкторы – это способ создания объектов класса.
// Это конструктор по умолчанию, который вызывается, когда объект инициализируется.
- (id)init
{
    if ((self = [super init])) // 'super' используется для того, чтобы обратиться к методам родительского класса
    {
        self.count = 1; // 'self' используется для вызова самого себя
    }
    return self;
}
// Можно создать конструкторы, которые содержат аргументы:
- (id)initWithDistance:(int)defaultDistance 
{
    distance = defaultDistance;
    return self;
}

+ (NSString *)classMethod
{
    return [[self alloc] init];
}

+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight 
{
    height = defaultHeight;
    return [[self alloc] init];
}

- (NSString *)instanceMethodWithParameter:(NSString *)string
{
    return @"Новая строка";
}

- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number
{
    return @42;
}

// Objective-C не содержит объявление приватных методов, но вы можете имитировать их.
// Чтобы сымитировать приватный метод, создайте метод в @implementation, но не в @interface.
- (NSNumber *)secretPrivateMethod {
    return @72;
}
[self secretPrivateMethod]; // Вызывает приватный метод

// Методы объявленные в МyProtocol (см. далее)
- (void)myProtocolMethod
{
    // операторы
}

@end // Устанавливает конец реализации (implementation)

///////////////////////////////////////
// Категории
///////////////////////////////////////
// Категория - это группа методов предназначенные для того, чтобы расширить класс. Они позволяют вам добавить новые методы
// к существующему классу для организационных целей. Это не стоит путать с подклассами.
// Подклассы предназначены для ИЗМЕНЕНИЯ функциональности объекта пока как категории ДОБАВЛЯЮТ
// функциональность в объект.
// Категории позволяют вам:
// -- Добавлять методы в существующий класс для организационных целей.
// -- Допускает вам расширять объекты Objective-C классов (напр.: NSString) добавить ваши собственные методы.
// -- Добавляет возможность создать защищенные и закрытые методы классов. 
// ПРИМЕЧАНИЕ: Не переопределяйте методы базового класса в категории даже если у вас есть возможность это сделать
// to. Переопределение методов может привести к ошибкам компиляции позднее между различными категориями и это 
// нарушает цель категорий, чтобы добавлять только функциональность. Вместо этого подклассы переопределяют методы.

// Здесь простой базовый класс Car.
@interface Car : NSObject

@property NSString *make;
@property NSString *color;

- (void)turnOn;
- (void)accelerate;

@end

// И реализация базового класса Car:
#import "Car.h"

@implementation Car

@synthesize make = _make;
@synthesize color = _color;

- (void)turnOn {
    NSLog(@"Машина заведена.");
}
- (void)accelerate {
    NSLog(@"Ускорение.");
}

@end

// Теперь, если мы хотим создать объект Truck - грузовик, мы должны создать подкласс класса Car, что
// изменит функционал Car и позволит вести себя подобно грузовику. Но что, если мы хотим только добавить
// определенный функционал в уже существующий класс Car? Например - чистка автомобиля. Мы просто создадим
// категорию, которая добавит несколько методов для чистки автомобиля в класс Car:
// @interface ИмяФайла: Car+Clean.h (ИмяБазовогоКласса+ИмяКатегории.h)
#import "Car.h" // Убедитесь в том, что базовый класс импортирован для расширения.

@interface Car (Clean) // Имя категории внутри (), следующие после имени базового класса.

- (void)washWindows; // Названия новых методов, которые мы добавляем в наш объект Car.
- (void)wax;

@end

// @implementation имя файла: Car+Clean.m (ИмяБазовогоКласса+ИмяКатегории.m)
#import "Car+Clean.h" // Импортируйте Очистку файл @interface категории.

@implementation Car (Clean)

- (void)washWindows {
    NSLog(@"Окна промыли.");
}
- (void)wax {
    NSLog(@"Воском натерли.");
}

@end 

// Любой экземпляр объекта Car имеет возможность воспользоваться категорией. Все, что нужно сделать, это импортировать ее:
#import "Car+Clean.h" // Импортировать как множество различных категорий, как вы хотите использовать.
#import "Car.h" // Кроме того, необходимо импортировать базовый класс для использования его оригинальные функциональные возможности.

int main (int argc, const char * argv[]) {
    @autoreleasepool {
        Car *mustang = [[Car alloc] init];
        mustang.color = @"Красный";
        mustang.make = @"Форд";

        [mustang turnOn]; // Используйте методы из базового класса Car.
        [mustang washWindows]; // Используйте методы категории Clean из класса Car.
    }
    return 0; 
}

// Objective-C не поддерживает объявление защищенных методов, но вы можете имитировать их.
// Создайте категорию, содержащую все защищенные методы, затем импортируйте ее только в
// @implementation-файле класса, относящегося к классу Car:
@interface Car (Protected) // Наименование категории с помощью 'Protected'
// дает знать, что методы защищенные.

- (void)lockCar; // Здесь перечисляются методы, которые должны быть созданы
// только с помощью объектов класса Car.

@end
// Чтобы воспользоваться защищенными методами, импортируйте категорию, затем реализуйте методы:
#import "Car+Protected.h" // Запомните, делайте импорт только в файле с @implementation.

@implementation Car 

- (void)lockCar {
    NSLog(@"Машина закрыта."); // Экземпляры класса Car не могут использовать
// метод lockCar, потому что он объявлен не в @interface.
}

@end

///////////////////////////////////////
// Расширения
///////////////////////////////////////
// Расширения позволяют вам переопределять атрибуты свойств и методов
// с открытым доступом в @interface.
// @interface имя файла: Shape.h
@interface Shape : NSObject // Расширение базового класса Shape переопределяет
                                        // свои поля ниже.

@property (readonly) NSNumber *numOfSides;

- (int)getNumOfSides;

@end
// Вы можете переопределить numOfSides-переменную или getNumOfSides-метод
// Внесение изменений с помощью расширения делается следующим образом:
// @implementation имя файла: Shape.m
#import "Shape.h"
// Расширения "живут" в том же файле, где и @implementation класса.
@interface Shape () // После имени базового класса скобки () объявляют расширение.

@property (copy) NSNumber *numOfSides; // Делает numOfSides-свойство
             // копирующим (copy) вместо свойства только для чтения (readonly).
-(NSNumber)getNumOfSides; // Изменяет метод getNumOfSides так,
                         // чтобы он возвращал объект NSNumber вместо типа int.
-(void)privateMethod; // Вы также можете создать новый закрытый метод
                              // внутри расширения.

@end
// Главный @implementation:
@implementation Shape

@synthesize numOfSides = _numOfSides;

-(NSNumber)getNumOfSides { // Все операторы внутри расширения
                                         // должны быть в @implementation.
    return _numOfSides;
}
-(void)privateMethod {
    NSLog(@"Закрытый метод созданный с помощью расширения.");
    NSLog(@"Экземпляр Shape не может вызвать этот метод.");
}

@end

///////////////////////////////////////
// Протоколы
///////////////////////////////////////
// Протокол объявляет методы, которые могут быть реализованы с помощью
// любого класса. Протоколы сами по себе не являются классами. Они просто
// определяют интерфейс, который должен быть реализован другими объектами.
// @protocol имя файла: "CarUtilities.h"
@protocol CarUtilities <NSObject> // <NSObject> => Имя другого протокола,
// который включен в этот протокол.
    @property BOOL engineOn; // Адаптирующий класс должен определить
// все @synthesize для @property и
    - (void)turnOnEngine; // определить все методы.
@end
// Ниже пример класса, реализующий протокол.
#import "CarUtilities.h" // Импорт файла с @protocol.

@interface Car : NSObject <CarUtilities> // Внутри <> имя протокола
// Здесь вам не нужно указывать @property или имена методов для CarUtilities.
// Они нужны только для @implementation.
- (void)turnOnEngineWithUtilities:(id <CarUtilities>)car; // Вы также можете
// указать тип протоколов.
@end
// В @implementation нужно реализовать все @property и методы для протокола.
@implementation Car : NSObject <CarUtilities>

@synthesize engineOn = _engineOn; // Создайте @synthesize-оператор
// для "@property engineOn".

- (void)turnOnEngine { // Реализуйте turnOnEngine как вам угодно. Протоколы
// не определят,
    _engineOn = YES; // как вам реализовать метод, он только требует,
// чтобы вы реализовали его.
}
// Вы можете использовать протокол как данные, если вы знаете, что он реализует
// методы и переменные.
- (void)turnOnEngineWithCarUtilities:(id <CarUtilities>)objectOfSomeKind { 
    [objectOfSomeKind engineOn]; // У вас есть доступ к переменным объекта
    [objectOfSomeKind turnOnEngine]; // и методам.
    [objectOfSomeKind engineOn]; // Может или не может быть значение YES. Класс
// реализует как нужно.
}

@end
// Экземпляры класса Car сейчас имеют доступ к протоколу.
Car *carInstance = [[Car alloc] init];
[carInstance setEngineOn:NO];
[carInstance turnOnEngine];
if ([carInstance engineOn]) {
    NSLog(@"Двигатель запущен."); // напечатает => "Двигатель запущен."
}
// Убедитись в том, что объект типа 'id' реализует протокол перед вызовом методов протокола:
if ([myClass conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"Не работает, т.к. класс MyClass не реализует протокол CarUtilities.");
} else if ([carInstance conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"Работает как класс Car, который реализует протокол CarUtilities.");
}
// Категории тоже могут реализовать протоколы:
// @interface Car (CarCategory) <CarUtilities>
// Вы можете реализовать много протоколов:
// @interface Car : NSObject <CarUtilities, CarCleaning>
// ЗАМЕЧАНИЕ: Если два или более протоколов полагаются друг на друга,
// убедитесь, что они ранее объявлены:
#import "Brother.h"

@protocol Brother; // Оператор раннего объявления. Без него компилятор
// выдаст ошибку.

@protocol Sister <NSObject>

- (void)beNiceToBrother:(id <Brother>)brother;

@end

// Рассмотрите проблему, где протокол Sister полагается на протокол Brother,
// а Brother полагается на Sister.
#import "Sister.h"

@protocol Sister; // Эти строки предотвращают рекурсию, решая этим проблему.

@protocol Brother <NSObject>
 
- (void)beNiceToSister:(id <Sister>)sister;

@end


///////////////////////////////////////
// Блоки
///////////////////////////////////////
// Блоки - это операторы кода, наподобие функции, которую возможно использовать
// как данные.
// Ниже простой блок с целочисленным аргументом, и возвращает аргумент плюс 4.
int (^addUp)(int n); // Объявите переменную, чтобы сохранить блок.
void (^noParameterBlockVar)(void); // Пример объявления блока-переменной
// без аргументов.
// Блоки имею доступ к переменным в той же области видимости. Но переменные
// будут только для чтения, и значения переданных в блок станут значением
// переменной, когда блок создастся.
int outsideVar = 17; // Если мы редактируем outsideVar после объявления addUp,
// outsideVar остается равным 17.
__block long mutableVar = 3; // __block делают переменные перезаписываемыми
// в блоках, в отличие от outsideVar.
addUp = ^(int n) { // Удалите (int n) в блоке, чтобы не принимать
// какие-либо параметры.
    NSLog(@"Вы можете иметь столько строк в блоке, сколько вы хотели.");
    NSSet *blockSet; // Также вы можете объявить локальные переменные.
    mutableVar = 32; // Присвоить новое значение к __block-переменной.
    return n + outsideVar; // Необязательный оператор возврата.
}
int addUp = add(10 + 16); // Вызывает блок кода с аргументами.
// Блоки часто используются как аргументы функции, чтобы позже их вызвать, или
// как функции обратного вызова (callbacks).
@implementation BlockExample : NSObject 
 
- (void)runBlock:(void (^)(NSString))block {
    NSLog(@"В аргументе блок ничего не возвращает и принимает NSString-объект.");
    block(@"Аргумент передан блоку на исполнение."); // Вызов блока.
}

@end
 
 
///////////////////////////////////////
// Управление памятью
///////////////////////////////////////
/* 
Для каждого объекта, используемого в приложении, должна быть выделена память
для таких объектов. Когда приложение прекращает использование объекта, память
должна быть освобождена, чтобы гарантировать эффективность приложения.
Objective-C не использует сборщик мусора, а вместо этого применяет подсчет ссылок.
Пока существует по крайней мере одна ссылка на объект (также называется
"владение" объектом), то объект будет доступен к использованию (еще известно
как "право владения").

Когда экземпляр владеет объектом, его ссылка увеличивается на один. Когда
объекта освобождается, счетчик ссылки уменьшается на один. Когда счетчик ссылки
равен нулю, объект удаляется из памяти.

Над всеми объектами взаимодействуют, следуя паттерну:
(1) создание объекта, (2) использование объекта, (3) затем освобождение объекта из памяти.
*/

MyClass *classVar = [MyClass alloc]; // 'alloc' устанавливает счетчик ссылки
// объекта classVar на 1 и возвращает указатель на объект.
[classVar release]; // Уменьшает счетчик ссылки объекта classVar
// 'retain' заявляет право собственности на существующий экземпляр объекта
// и увеличивает счетчик ссылки. Затем вернет указатель на объект.
MyClass *newVar = [classVar retain]; // Если classVar освободится, объект
// останется в памяти, потому что newVar - владелец
[classVar autorelease]; // Удалит право на владение объектом
// в конце @autoreleasepool блока. Вернет указатель на объект.

// @property может использовать 'retain' и 'assign' тоже для маленького
// удобного определения
@property (retain) MyClass *instance; // Освободит старое значение и сохранит
// одно новое (строгая ссылка)
@property (assign) NSSet *set; // Укажет на новое значение
// без сохранения/освобождения старого значения (слабая ссылка)

// Автоматический подсчет ссылок (ARC)
// Управление памятью может быть трудным, поэтому в Xcode 4.2 и iOS 4 введен
// автоматический подсчет ссылок (ARC).
// ARC - это особенность компилятора, который помещает "retain", "release"
// и "autorelease" автоматически за вас тогда, когда используется ARC,
// вам не нужно больше обращаться к "retain", "release" или "autorelease"
MyClass *arcMyClass = [[MyClass alloc] init];
// ... код, использующий объект arcMyClass
// Без ARC, вам нужно было бы вызвать: [arcMyClass release] после того, как вы
// завершите работу с объектом arcMyClass. Но с ARC,
// теперь этого не нужно делать. Он будет помещать release-вызов за вас

// Что касается 'assign' и 'retain' @property атрибутов, в ARC вы должны
// использовать 'weak' и 'strong'
@property (weak) MyClass *weakVar; // 'weak' не принимает право на владение
// объектом. Если исходный счетчик ссылки экземпляра обнуляется,
// weakVar-свойство автоматически примет значение nil,
// во избежание падения приложения
@property (strong) MyClass *strongVar; // 'strong' принимает право на владение
// объектом. Гарантирует, что объект останется в памяти для использования

// Для обычных переменных (не объявленных с помощью @property), используйте
// следующий способ:
__strong NSString *strongString; // По умолчанию. Переменная сохраняется в памяти,
// пока она не покинет область видимости
__weak NSSet *weakSet; // Слабая ссылка на существующий объект. Когда существующий
// объект освобождается, weakSet принимает nil
__unsafe_unretained NSArray *unsafeArray; // Похож на __weak, но unsafeArray
// не принимает nil, когда существующий объект освобождается

```
## На почитать

[Wikipedia Objective-C](http://en.wikipedia.org/wiki/Objective-C)

[Learning Objective-C](http://developer.apple.com/library/ios/referencelibrary/GettingStarted/Learning_Objective-C_A_Primer/)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)

[iOS разработчик: Обзор книг для новичка](http://habrahabr.ru/post/166213/)

[Хочешь быть iOS разработчиком? Будь им!](http://www.pvsm.ru/ios/12662/print/)
