---
language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
    - ["Levi Bostian", "https://github.com/levibostian"]
    - ["Clayton Walker", "https://github.com/cwalk"]
    - ["Fernando Valverde", "http://visualcosita.xyz"]
translators:
    - ["Gaoang Chen", "https://github.com/chengaoang"]
filename: LearnObjectiveC-cn.m
lang: zh-cn
---
Objective-C（Objective-C 2.0）是苹果公司用于 macOS 和 iOS 操作系统以及相应的框架 Cocoa 和 Cocoa Touch 的主要编程语言。
它是一种面向对象的通用编程语言，为 C 语言添加了 Smalltalk 风格的消息传递机制。

```objective-c
// 单行注释以 // 开头

/*
多行注释的形式
*/

// Xcode 支持使用 pragma mark 来提升代码可读性
// 简单来说就是对代码的分组,方便代码查找和导航用的 它们告诉Xcode编译器,要在编辑器窗格顶部的方法和函数弹出菜单中将代码分隔开。
#pragma mark Navigation Functions // 跳转栏上新增标签 'Navigation Functions'
#pragma mark - Navigation Functions // 同样的标签，现在有一个分隔符

// 使用 #import 导入 Foundation 头文件
// 使用 <> 导入全局文件（一般是框架）
// 使用 "" 导入本地文件（来自项目）
#import <Foundation/Foundation.h>
#import "MyClass.h"

// 如果在 Xcode 5 中为 iOS >= 7.0 或者 OS X >= 10.9 的项目启用模块（modules），可以这样导入框架：
@import Foundation;

// 程序的入口是一个名为 main 的函数，其返回类型为整型
int main (int argc, const char * argv[])
{
    // 创建自动释放池以管理程序内存, init 是一个用于初始化对象的方法
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // 如果使用自动引用计数（ARC），则应使用 @autoreleasepool 代替：
    @autoreleasepool {

    // 使用 NSLog 打印输出到控制台
    NSLog(@"Hello World!"); // 打印字符串 "Hello World!"

    ///////////////////////////////////////
    // 类型和变量
    ///////////////////////////////////////

    // 基本类型声明
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;

    // 对象声明
    // 对于强类型对象声明，在变量名前面加上 *
    MyClass *myObject1 = nil;  // 强类型
    id       myObject2 = nil;  // 弱类型
    // %@ 表示对象
    // 'description' 是一种约定，用于显示对象的值
    NSLog(@"%@ and %@", myObject1, [myObject2 description]); // 输出 => "(null) and (null)"

    // 字符串
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // 输出 => "Hello World!"
    // NSMutableString 是 NSString 对象的可变版本
    NSMutableString *mutableString = [NSMutableString stringWithString:@"Hello"];
    [mutableString appendString:@" World!"];
    NSLog(@"%@", mutableString); // 输出 => "Hello World!"

    // 字符字面量
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue]; // 或直接将 theLetterZ 赋值为字符 'Z'，
                                                               // 而不必通过NSNumber对象来进行转换。
    NSLog(@"%c", theLetterZ);

    // 整数字面量
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue]; // 或直接赋值 42
    NSLog(@"%i", fortyTwo);

    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue]; // 或 42
    NSLog(@"%u", fortyTwoUnsigned);

    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue]; // 或 42
    NSLog(@"%hi", fortyTwoShort);

    NSNumber *fortyOneShortNumber   = [NSNumber numberWithShort:41];
    unsigned short fortyOneUnsigned = [fortyOneShortNumber unsignedShortValue]; // 或 41
    NSLog(@"%u", fortyOneUnsigned);

    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue]; // 或 42
    NSLog(@"%li", fortyTwoLong);

    NSNumber *fiftyThreeLongNumber   = @53L;
    unsigned long fiftyThreeUnsigned = [fiftyThreeLongNumber unsignedLongValue]; // 或 53
    NSLog(@"%lu", fiftyThreeUnsigned);

    // 浮点数字面量
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue]; // 或 3.141592654f
    NSLog(@"%f", piFloat); // 输出 => 3.141592654
    NSLog(@"%5.2f", piFloat); // 输出 => " 3.14"

    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble          = [piDoubleNumber doubleValue]; // 或 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // 输出 => "3.14"

    // NSDecimalNumber 是一种比 float 或 double 更精确的固定点类
    NSDecimalNumber *oneDecNum = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *twoDecNum = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimalNumber 不能使用标准的 +、-、*、/ 运算符，而是提供自己的：
    [oneDecNum decimalNumberByAdding:twoDecNum];
    [oneDecNum decimalNumberBySubtracting:twoDecNum];
    [oneDecNum decimalNumberByMultiplyingBy:twoDecNum];
    [oneDecNum decimalNumberByDividingBy:twoDecNum];
    NSLog(@"%@", oneDecNum); // 输出 => 10.99，因为 NSDecimalNumber 是不可变的

    // BOOL 字面量
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;
    // 或
    BOOL yesBool = YES;
    BOOL noBool  = NO;
    NSLog(@"%i", yesBool); // 输出 => 1

    // 数组对象
    // 数组可以包含不同的数据类型，但必须是 Objective-C 对象
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // 输出 => "Third number = 3"
    // 从 Xcode 7 开始，NSArray 对象可以使用泛型
    NSArray<NSString *> *stringArray = @[@"hello", @"world"];
    // NSMutableArray 是 NSArray 的可变版本，允许更改数组中的项，并扩展或缩小数组对象
    // 方便，但不如 NSArray 高效。
    NSMutableArray *mutableArray = [NSMutableArray arrayWithCapacity:2];
    [mutableArray addObject:@"Hello"];
    [mutableArray addObject:@"World"];
    [mutableArray removeObjectAtIndex:0];
    NSLog(@"%@", [mutableArray objectAtIndex:0]); // 输出 => "World"

    // 字典对象
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // 输出 => "Object = (null)"
    // 从 Xcode 7 开始，不同的 NSDictionary 对象可以使用泛型
    NSDictionary<NSString *, NSNumber *> *numberDictionary = @{@"a": @1, @"b": @2};
    // NSMutableDictionary 也可以作为可变字典对象使用
    NSMutableDictionary *mutableDictionary = [NSMutableDictionary dictionaryWithCapacity:2];
    [mutableDictionary setObject:@"value1" forKey:@"key1"];
    [mutableDictionary setObject:@"value2" forKey:@"key2"];
    [mutableDictionary removeObjectForKey:@"key1"];

    // 从可变类型更改为不可变类型
    // 通常 [object mutableCopy] 会使对象变为可变的，而 [object copy] 会使对象变为不可变的
    NSMutableDictionary *aMutableDictionary = [aDictionary mutableCopy];
    NSDictionary *mutableDictionaryChanged = [mutableDictionary copy];


    // 集合对象
    NSSet *set = [NSSet setWithObjects:@"Hello", @"Hello", @"World", nil];
    NSLog(@"%@", set); // 输出 => {(Hello, World)}（排列顺序可能不同）
    // 从 Xcode 7 开始，NSSet 对象可以使用泛型
    NSSet<NSString *> *stringSet = [NSSet setWithObjects:@"hello", @"world", nil];
    // NSMutableSet 也可用作可变集对象
    NSMutableSet *mutableSet = [NSMutableSet setWithCapacity:2];
    [mutableSet addObject:@"Hello"];
    [mutableSet addObject:@"Hello"];
    NSLog(@"%@", mutableSet); // 输出 => {(Hello)}

    ///////////////////////////////////////
    // 运算符
    ///////////////////////////////////////

    // 运算符与使用 C 语言中的相同
    // 例如：
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (逻辑与)
    0 || 1; // => 1 (逻辑或)
    ~0x0F; // => 0xF0 (按位取反)
    0x0F & 0xF0; // => 0x00 (按位与)
    0x01 << 1; // => 0x02 (按位左移（1 位）)

    ///////////////////////////////////////
    // 控制结构
    ///////////////////////////////////////

    // If-Else 语句
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

    // Switch 语句
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

    // While 循环语句
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ 先使用 ii 的值，再将 ii 增加 1
    } // 输出 => "0,"
      //        "1,"
      //        "2,"
      //        "3,"

    // For 循环语句
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // 输出 => "0,"
      //        "1,"
      //        "2,"
      //        "3,"

    // Foreach 循环语句
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // 输出 => "0,"
      //        "1,"
      //        "2,"
      //        "3,"

    // 对象循环语句。可与任何 Objective-C 对象类型一起使用
    for (id item in values) {
        // 在 Objective-C 中，id 是一个关键字，用于声明指向任意类型对象的指针。
        // 它类似于 C 语言中的 void *，但是在 Objective-C 中，id 更加类型安全，因为它仅用于对象类型。
        NSLog(@"%@,", item);
    } // 输出 => "0,"
      //        "1,"
      //        "2,"
      //        "3,"

    // Try-Catch-Finally 语句
    @try
    {
        // 在此处添加代码
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"File Not Found on System" userInfo:nil];
    } @catch (NSException * e) // 使用：@catch (id exceptionName) 以捕获所有对象
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally. Time to clean up.");
    } // 输出 => "Exception: File Not Found on System"
      //        "Finally. Time to clean up."

    // NSError 对象在函数参数中非常有用，用于在用户错误时填充
    NSError *error = [NSError errorWithDomain:@"Invalid email." code:4 userInfo:nil];

    ///////////////////////////////////////
    // 对象
    ///////////////////////////////////////

    // 创建对象实例，将内存分配和初始化的操作组合在一起
    // 在对象完成这两个步骤之前，它是不完全可用的
    MyClass *myObject = [[MyClass alloc] init];

    // Objective-C 面向对象编程模型基于向对象实例发送消息
    // 在 Objective-C 中，不是简单地调用方法；而是发送消息
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // 释放程序使用的内存
    [pool drain];

    // 结束 @autoreleasepool
    }

    // 结束程序
    return 0;
}

///////////////////////////////////////
// 类和函数
///////////////////////////////////////

// 在头文件（MyClass.h）中声明类：
// 类声明语法：
// @interface 类名 : 父类名 <实现的协议>
// {
//    变量类型 变量名; <= 变量声明
// }
// @property 变量类型 变量名; <= 属性声明
// -/+ (返回类型) 方法声明; <= 方法声明
// @end
@interface MyClass : NSObject <MyProtocol> // NSObject 是 Objective-C 的基础对象类。
{
    // 实例变量声明（可以存在于接口或实现文件）
    int count; // 默认为 protected 访问权限。
    @private id data; // 私有访问权限（在实现文件中更方便地声明）
    NSString *name;
}
// 快捷符号用于公共访问变量以自动生成 setter 方法
// 默认情况下，setter 方法名为“set”后跟 @property 变量名
@property int propInt; // 对应的 setter 方法名为 'setPropInt'
@property (copy) id copyId; // (copy) => 赋值时复制对象
// (readonly) => 不能在 @interface 外设置值
@property (readonly) NSString *roString; // 通过在 @implementation 中使用 @synthesize 来创建访问器，
                                         // 此属性为只读的 NSString 类型。
// 可以自定义 getter 和 setter 的名称，而不使用默认的“set”名称：
@property (getter=lengthGet, setter=lengthSet:) int length;

// 方法
+/- (返回类型)方法签名:(参数类型 *)参数名;

// + 用于类方法：
+ (NSString *)classMethod;
+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight;

// - 用于实例方法：
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

// 使用参数创建的构造方法：
- (id)initWithDistance:(int)defaultDistance;
// Objective-C 方法名非常具有描述性。始终根据其参数命名方法

@end // 标记接口结束


// 从实现文件中访问公共变量，使用 @property 生成 setter 方法
// 方法名为 'set' 后跟 @property 变量名：
MyClass *myClass = [[MyClass alloc] init]; // 创建 MyClass 对象实例
[myClass setCount:10];
NSLog(@"%d", [myClass count]); // 输出 => 10
// 或使用在 @interface 中自定义的 getter 和 setter 方法：
[myClass lengthSet:32];
NSLog(@"%i", [myClass lengthGet]); // 输出 => 32
// 为方便使用，可以使用点符号设置和访问对象实例变量：
myClass.count = 45;
NSLog(@"%i", myClass.count); // 输出 => 45

// 调用类方法：
NSString *classMethodString = [MyClass classMethod];
MyClass *classFromName = [MyClass myClassFromName:@"Hello"];

// 调用实例方法：
MyClass *myClass = [[MyClass alloc] init]; // 创建 MyClass 对象实例
NSString *stringFromInstanceMethod = [myClass instanceMethodWithParameter:@"Hello"];

// 选择器
// 用于动态表示方法。用于调用类的方法，通过函数将方法传递给其他类并告诉它们应该调用它，
// 以及将方法保存为变量
// SEL 是数据类型，@selector() 从提供的方法名返回选择器
// methodAParameterAsString:andAParameterAsNumber: 是 MyClass 中的方法名
SEL selectorVar = @selector(methodAParameterAsString:andAParameterAsNumber:);
if ([myClass respondsToSelector:selectorVar]) { // 检查类是否包含该方法
    // 必须将所有方法参数放入一个对象中才能发送给 performSelector 函数
    NSArray *arguments = [NSArray arrayWithObjects:@"Hello", @4, nil];
    [myClass performSelector:selectorVar withObject:arguments]; // 调用方法
} else {
    // NSStringFromSelector() 返回给定选择器的 NSString 方法名
    NSLog(@"MyClass 不包含方法：%@", NSStringFromSelector(selectedVar));
}

// 写入在接口中声明的方法定义：
@implementation MyClass

- (void)methodSignature:(NSString *)parameterName {
    // 代码块
}

+ (NSString *)classMethod {
    return @"一些字符串";
}

+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight {
    height = defaultHeight;
    return [[self alloc] init];
}

- (NSString *)instanceMethodWithParameter:(NSString *)string {
    return @"新字符串";
}

- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number {
    return @42;
}

// 此处没有创建私有方法声明的机制，但可以模拟它们。
// 要模拟私有方法，请在 @implementation 中创建该方法，但不要在 @interface 中创建它。
- (NSNumber *)secretPrivateMethod {
    return @72;
}
[self secretPrivateMethod]; // 调用私有方法

// 在 MyProtocol 中声明的方法
- (void)myProtocolMethod
{
    // 语句
}

@end // 标记实现结束

///////////////////////////////////////
// 类别
///////////////////////////////////////
// 类别是一组用于扩展类的方法。它们允许你为了组织目的，向一个已存在的类中添加新的方法。不要将其与子类混淆。
// 子类是为了更改对象的功能，而类别是为了给对象添加功能。
// 类别允许您：
// -- 添加用于组织目的的方法到现有类中。
// -- 扩展 Objective-C 对象类（例如 NSString）以添加自己的方法。
// -- 添加创建受保护和私有方法的能力到类中。
// 注意：不要重写类别中基类的方法，尽管你可以这样做。重写方法可能导致编译器后来在不同的类别之间引发错误，
// 也破坏了类别的目的，只添加功能。应该使用子类来重写方法。

// 这是一个简单的 Car 基类。
@interface Car : NSObject

@property NSString *make;
@property NSString *color;

- (void)turnOn;
- (void)accelerate;

@end

// 以及 Car 基类的简单实现：
#import "Car.h"

@implementation Car

@synthesize make = _make;
@synthesize color = _color;

- (void)turnOn {
    NSLog(@"Car is on.");
}
- (void)accelerate {
    NSLog(@"Accelerating.");
}

@end

// 现在，如果我们想要创建一个 Truck 对象，我们将创建 Car 的子类，因为它将改变 Car 的功能。
// 但是，假设我们只是想要添加功能到现有的 Car 中。一个很好的例子是给汽车清洗。所以我们将创建一个类别来新增这些清洗方法。
// @interface 文件名：Car+Clean.h（BaseClassName+CategoryName.h）
#import "Car.h" // 确保导入基类以进行扩展。

@interface Car (Clean) // 在圆括号中写上类别名称，紧跟基类的名称。

- (void)washWindows; // 我们添加到 Car 对象中的新方法的名称。
- (void)wax;

@end

// @implementation 文件名：Car+Clean.m（BaseClassName+CategoryName.m）
#import "Car+Clean.h" // 导入 Clean 类别的 @interface 文件。

@implementation Car (Clean)

- (void)washWindows {
    NSLog(@"Windows washed.");
}
- (void)wax {
    NSLog(@"Waxed.");
}

@end

// Car 实例对象都可以使用类别。只需导入即可：
#import "Car+Clean.h" // 导入尽可能多的不同类别。
#import "Car.h" // 也需要导入基类以使用其原始功能。

int main (int argc, const char * argv[]) {
    @autoreleasepool {
        Car *mustang = [[Car alloc] init];
        mustang.color = @"Red";
        mustang.make = @"Ford";

        [mustang turnOn]; // 使用基类 Car 中的方法。
        [mustang washWindows]; // 使用 Car 的 Clean 类别中的方法。
    }
    return 0;
}

// 确保在使用该 Car 类的实体类的 @implementation 文件中检查类别是否存在：
#import "Car+Clean.h" // 还记得，只导入 @interface 文件。

@implementation Car

- (void)lockCar {
    NSLog(@"Car locked."); // Car 的实例不能使用 lockCar 方法，因为它不在 @interface 中。
}

@end

///////////////////////////////////////
// 扩展
///////////////////////////////////////
// 扩展允许您重写 @interface（接口）中的公共访问属性属性和方法。
// @interface 文件名: Shape.h
@interface Shape : NSObject // Shape 基类扩展重写如下。

@property (readonly) NSNumber *numOfSides;

- (int)getNumOfSides;

@end
// 您可以重写 numOfSides 变量或 getNumOfSides 方法来进行编辑。
// @implementation filename: Shape.m (BaseClassName+CategoryName.m)
#import "Shape.h"
// 扩展与类的 @implementation 放在同一个文件中。
@interface Shape () // 在基类名称之后的 () 中声明一个扩展。

@property (copy) NSNumber *numOfSides; // 使 numOfSides 为可复制而不是只读。
-(NSNumber)getNumOfSides; // 使 getNumOfSides 返回一个 NSNumber 而不是一个 int。
-(void)privateMethod; // 也可以在扩展中创建新的私有方法。

@end
// 主要 @implementation：
@implementation Shape

@synthesize numOfSides = _numOfSides;

-(NSNumber)getNumOfSides { // 所有在扩展中的语句必须在 @implementation 中。
    return _numOfSides;
}
-(void)privateMethod {
    NSLog(@"通过扩展创建的私有方法。Shape 实例无法调用我。");
}

@end

// 从 Xcode 7.0 开始，您可以创建泛型类，从而在不编写过多样板代码的情况下提供更高的类型安全性和清晰度。
@interface Result<__covariant A> : NSObject

- (void)handleSuccess:(void(^)(A))success
              failure:(void(^)(NSError *))failure;

@property (nonatomic) A object;

@end

// 现在我们可以声明此类的实例
Result<NSNumber *> *result;
Result<NSArray *> *result;

// 每种情况都相当于重写 Result 的接口
// 并使用合适的类型替换 A
@interface Result : NSObject
- (void)handleSuccess:(void(^)(NSArray *))success
              failure:(void(^)(NSError *))failure;
@property (nonatomic) NSArray * object;
@end

@interface Result : NSObject
- (void)handleSuccess:(void(^)(NSNumber *))success
              failure:(void(^)(NSError *))failure;
@property (nonatomic) NSNumber * object;
@end

// 很明显，一般来说编写一个类解决一个问题总是比编写两个类要好


// 请注意，Clang 不接受 @implementations 中的泛型类型，
// 因此 Result 的 @implemnation 应如下所示：

@implementation Result

- (void)handleSuccess:(void (^)(id))success
              failure:(void (^)(NSError *))failure {
  // 做一些事情
}

@end


///////////////////////////////////////
// 协议
///////////////////////////////////////
// 协议声明可以被任何类实现的方法。
// 协议本身并不是类，而只是定义其他对象需要实现的接口。
// @protocol 文件名: "CarUtilities.h"
@protocol CarUtilities <NSObject> // <NSObject> => 一个协议包含另一个协议的名称。
    @property BOOL engineOn; // 采用协议的类必须 @synthesize 所有已定义的 @property 和
    - (void)turnOnEngine; // 所有已定义的方法
@end
// 下面是实现协议的一个示例类。
#import "CarUtilities.h" // 导入协议文件。

@interface Car : NSObject <CarUtilities> // 协议的名称放在 <> 内
    // 这里不需要添加 @property 或方法名称，CarUtilities 只需要在 @implementation 中添加即可。
- (void)turnOnEngineWithUtilities:(id <CarUtilities>)car; // 也可以将协议作为数据使用。
@end
// 在 @implementation 中，必须实现协议的 @property 和方法。
@implementation Car : NSObject <CarUtilities>

@synthesize engineOn = _engineOn; // 为 engineOn @property 创建一个 @synthesize 语句

- (void)turnOnEngine { // 可以按照需要实现 turnOnEngine，协议不定义如何实现一个方法
    _engineOn = YES; // 返回语句是可选的
}
// 用协议作为数据时，可以访问协议方法和变量。
- (void)turnOnEngineWithCarUtilities:(id <CarUtilities>)objectOfSomeKind {
    [objectOfSomeKind engineOn]; // 这里可以访问对象变量
    [objectOfSomeKind turnOnEngine]; // 以及方法
    [objectOfSomeKind engineOn]; // 可能为 YES、NO。类以自己想要的方式实现它。
}

@end
// Car 的实例对象现在可以访问该协议。
Car *carInstance = [[Car alloc] init];
[carInstance setEngineOn:NO];
[carInstance turnOnEngine];
if ([carInstance engineOn]) {
    NSLog(@"Car engine is on."); // 输出 => "Car engine is on."
}
// 在调用协议方法之前，请确保检查 id 类型的对象是否实现了该协议：
if ([myClass conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"此处不运行，因为 MyClass 类没有实现 CarUtilities 协议。");
} else if ([carInstance conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"此处运行，因为 Car 类实现了 CarUtilities 协议。");
}
// 类别也可以实现协议：@interface Car (CarCategory) <CarUtilities>
// 您可以实现多个协议：@interface Car : NSObject <CarUtilities, CarCleaning>
// 注意：如果两个或多个协议相互依赖，请确保预先声明它们：
#import "Brother.h"

@protocol Brother; // 前向声明语句，没有它，编译器会引发错误。

@protocol Sister <NSObject>

- (void)beNiceToBrother:(id <Brother>)brother;

@end

// 问题在于 Sister 依赖于 Brother，而 Brother 依赖于 Sister。
#import "Sister.h"

@protocol Sister; // 这些行解除递归，解决问题。

@protocol Brother <NSObject>

- (void)beNiceToSister:(id <Sister>)sister;

@end


///////////////////////////////////////
// 块
///////////////////////////////////////
// 块是能够像函数一样使用的代码语句，可以作为数据使用。
// 下面是一个简单的块，它接受一个整数参数并返回参数加4。
^(int n) {
    return n + 4;
}
int (^addUp)(int n); // 声明一个变量来存储块。
void (^noParameterBlockVar)(void); // 块变量的声明示例，没有参数。
// 块可以访问相同作用域中的变量。但是，变量是只读的，并且在创建块时，传递给该块的值是变量的值。
int outsideVar = 17; // 如果我们在声明 addUp 之后编辑 outsideVar，outsideVar 仍然是 17。
__block long mutableVar = 3; // __block 使变量对块可写，与 outsideVar 不同。
addUp = ^(int n) { // 如果要创建不带参数的块，可以删除 (int n)。
    NSLog(@"块可以有多行代码。");
    NSSet *blockSet; // 此外，您还可以声明局部变量。
    mutableVar = 32; // 将新值分配给 __block 变量。
    return n + outsideVar; // 返回语句是可选的。
}
int addUp = addUp(10 + 16); // 用参数调用块代码
// 块经常用作函数的参数以供以后调用或用于回调。
@implementation BlockExample : NSObject

 - (void)runBlock:(void (^)(NSString))block {
    NSLog(@"块参数不返回任何值，且接受一个 NSString 对象。");
    block(@"传递给块的参数来执行。"); // 调用块。
 }

 @end


///////////////////////////////////////
// 内存管理
///////////////////////////////////////
/*
对于应用程序中使用的每个对象，都必须为该对象分配内存。当应用程序使用完该对象后，必须释放内存以确保应用程序的效率。
Objective-C 不使用垃圾收集，而是使用引用计数。只要对象具有至少一个引用（也称为“拥有”对象），则该对象将可用（称为“所有权”）。

当一个实例拥有一个对象时，其引用计数递增一次。当该对象被释放时，引用计数递减一次。当引用计数为零时，对象将从内存中删除。

与对象的所有相互作用一样，请按照：（1）创建对象，（2）使用对象，（3）然后释放对象内存的模式进行交互。
*/

MyClass *classVar = [MyClass alloc]; // 'alloc' 将 classVar 的引用计数设置为一，返回对象指针
[classVar release]; // classVar 的引用计数减一
// 'retain' 声明对现有对象实例的所有权并递增引用计数。返回对象指针
MyClass *newVar = [classVar retain]; // 如果释放 classVar，则该对象仍留在内存中，因为 newVar 是其所有者
[classVar autorelease]; // 在 @autoreleasepool 块结束时取消对对象的所有权释放它。返回对象指针

// @property 可以使用 'retain' 和 'assign' 属性进行方便定义
@property (retain) MyClass *instance; // 释放旧值，并保留新值（强引用）
@property (assign) NSSet *set; // 指针指向新值，而不释放/保留旧值（弱引用）

// 自动引用计数（ARC）
// 因为内存管理可能是一种负担，所以 Xcode 4.2 和 iOS 4 引入了自动引用计数（ARC）。
// ARC 是一个编译器功能，会自动插入 retain、release 和 autorelease，因此在使用 ARC 时，不能使用 retain、release 或 autorelease。
MyClass *arcMyClass = [[MyClass alloc] init];
// ... 使用 arcMyClass 的代码
// 没有 ARC，您需要在使用完 arcMyClass 后调用：[arcMyClass release]。但是在 ARC 中，没有必要。它会自动为您插入这个 release 语句

// 至于 'assign' 和 'retain' @property 特性，使用 'weak' 和 'strong'
@property (weak) MyClass *weakVar; // 'weak' 不拥有对象。如果原始实例的引用计数被设置为零，weakVar 将自动接收值 nil，以避免应用程序崩溃
@property (strong) MyClass *strongVar; // 'strong' 拥有对象。确保对象保留在内存中以供使用

// 对于常规变量（非 @property 声明的变量），使用以下方法：
__strong NSString *strongString; // 默认。变量在离开作用域之前都保留在内存中
__weak NSSet *weakSet; // 对现有对象的弱引用。当现有对象释放时，weakSet 被设置为 nil
__unsafe_unretained NSArray *unsafeArray; // 与 __weak 类似，但释放现有对象时 unsafeArray 不设置为 nil
```

## 进一步阅读


[Wikipedia Objective-C](http://en.wikipedia.org/wiki/Objective-C)

[Programming with Objective-C. Apple PDF book](https://developer.apple.com/library/ios/documentation/cocoa/conceptual/ProgrammingWithObjectiveC/ProgrammingWithObjectiveC.pdf)

[Programming with Objective-C for iOS](https://developer.apple.com/library/ios/documentation/General/Conceptual/DevPedia-CocoaCore/ObjectiveC.html)

[Programming with Objective-C for Mac OSX](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
