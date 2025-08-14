# objective-c.md (번역)

---
name: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
    - ["Levi Bostian", "https://github.com/levibostian"]
    - ["Clayton Walker", "https://github.com/cwalk"]
    - ["Fernando Valverde", "http://visualcosita.xyz"]
filename: LearnObjectiveC.m
---

Objective-C는 macOS 및 iOS 운영 체제와 해당 프레임워크인 Cocoa 및 Cocoa Touch에서 Apple이 사용하는 주요 프로그래밍 언어입니다.
C 프로그래밍 언어에 Smalltalk 스타일 메시징을 추가한 범용 객체 지향 프로그래밍 언어입니다.

```objective-c
// 한 줄 주석은 //로 시작합니다.

/*
여러 줄 주석은
이와 같습니다.
*/

// XCode는 점프 바 가독성을 향상시키는 pragma mark 지시문을 지원합니다.
#pragma mark Navigation Functions // 'Navigation Functions'라는 새 태그가 점프 바에 표시됩니다.
#pragma mark - Navigation Functions // 동일한 태그, 이제 구분 기호가 있습니다.

// #import로 Foundation 헤더를 가져옵니다.
// 전역 파일을 가져오려면 <>를 사용합니다(일반적으로 프레임워크).
// 로컬 파일을 가져오려면 ""를 사용합니다(프로젝트에서).
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Xcode 5에서 iOS >= 7.0 또는 OS X >= 10.9 프로젝트에 대해 모듈을 활성화하면
// 다음과 같이 프레임워크를 가져올 수 있습니다.
@import Foundation;

// 프로그램의 진입점은 정수 반환 타입의
// main이라는 함수입니다.
int main (int argc, const char * argv[])
{
    // 프로그램의 메모리를 관리하기 위해 자동 해제 풀을 생성합니다.
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    // 자동 참조 계산(ARC)을 사용하는 경우 대신 @autoreleasepool을 사용합니다.
    @autoreleasepool {

    // NSLog를 사용하여 콘솔에 줄을 출력합니다.
    NSLog(@"Hello World!"); // "Hello World!" 문자열을 출력합니다.

    ///////////////////////////////////////
    // 타입 및 변수
    ///////////////////////////////////////

    // 기본 타입 선언
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;

    // 객체 선언
    // 강력한 타입의 객체 선언을 위해 변수 이름 앞에 *를 붙입니다.
    MyClass *myObject1 = nil;  // 강력한 타이핑
    id       myObject2 = nil;  // 약한 타이핑
    // %@는 객체입니다.
    // 'description'은 객체의 값을 표시하는 관례입니다.
    NSLog(@"%@ and %@", myObject1, [myObject2 description]); // => "(null) and (null)" 출력

    // 문자열
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // => "Hello World!" 출력
    // NSMutableString은 NSString 객체의 가변 버전입니다.
    NSMutableString *mutableString = [NSMutableString stringWithString:@"Hello"];
    [mutableString appendString:@" World!"];
    NSLog(@"%@", mutableString); // => "Hello World!" 출력

    // 문자 리터럴
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue]; // 또는 'Z'
    NSLog(@"%c", theLetterZ);

    // 정수 리터럴
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue]; // 또는 42
    NSLog(@"%i", fortyTwo);

    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue]; // 또는 42
    NSLog(@"%u", fortyTwoUnsigned);

    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue]; // 또는 42
    NSLog(@"%hi", fortyTwoShort);

    NSNumber *fortyOneShortNumber   = [NSNumber numberWithShort:41];
    unsigned short fortyOneUnsigned = [fortyOneShortNumber unsignedShortValue]; // 또는 41
    NSLog(@"%u", fortyOneUnsigned);

    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue]; // 또는 42
    NSLog(@"%li", fortyTwoLong);

    NSNumber *fiftyThreeLongNumber   = @53L;
    unsigned long fiftyThreeUnsigned = [fiftyThreeLongNumber unsignedLongValue]; // 또는 53
    NSLog(@"%lu", fiftyThreeUnsigned);

    // 부동 소수점 리터럴
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue]; // 또는 3.141592654f
    NSLog(@"%f", piFloat); // => 3.141592654 출력
    NSLog(@"%5.2f", piFloat); // => " 3.14" 출력

    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble          = [piDoubleNumber doubleValue]; // 또는 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // => "3.14" 출력

    // NSDecimalNumber는 float 또는 double보다 더 정밀한 고정 소수점 클래스입니다.
    NSDecimalNumber *oneDecNum = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *twoDecNum = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimalNumber는 표준 +, -, *, / 연산자를 사용할 수 없으므로 자체 연산자를 제공합니다.
    [oneDecNum decimalNumberByAdding:twoDecNum];
    [oneDecNum decimalNumberBySubtracting:twoDecNum];
    [oneDecNum decimalNumberByMultiplyingBy:twoDecNum];
    [oneDecNum decimalNumberByDividingBy:twoDecNum];
    NSLog(@"%@", oneDecNum); // => NSDecimalNumber는 불변이므로 10.99 출력

    // BOOL 리터럴
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;
    // 또는
    BOOL yesBool = YES;
    BOOL noBool  = NO;
    NSLog(@"%i", yesBool); // => 1 출력

    // 배열 객체
    // 다른 데이터 타입을 포함할 수 있지만 Objective-C 객체여야 합니다.
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // => "Third number = 3" 출력
    // Xcode 7부터 NSArray 객체는 타입을 지정할 수 있습니다 (제네릭).
    NSArray<NSString *> *stringArray = @[@"hello", @"world"];
    // NSMutableArray는 NSArray의 가변 버전으로,
    // 배열의 항목을 변경하거나 배열 객체를 확장하거나 축소할 수 있습니다.
    // 편리하지만 NSArray만큼 효율적이지는 않습니다.
    NSMutableArray *mutableArray = [NSMutableArray arrayWithCapacity:2];
    [mutableArray addObject:@"Hello"];
    [mutableArray addObject:@"World"];
    [mutableArray removeObjectAtIndex:0];
    NSLog(@"%@", [mutableArray objectAtIndex:0]); // => "World" 출력

    // 딕셔너리 객체
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // => "Object = (null)" 출력
    // Xcode 7부터 NSDictionary 객체는 타입을 지정할 수 있습니다 (제네릭).
    NSDictionary<NSString *, NSNumber *> *numberDictionary = @{@"a": @1, @"b": @2};
    // NSMutableDictionary도 가변 딕셔너리 객체로 사용할 수 있습니다.
    NSMutableDictionary *mutableDictionary = [NSMutableDictionary dictionaryWithCapacity:2];
    [mutableDictionary setObject:@"value1" forKey:@"key1"];
    [mutableDictionary setObject:@"value2" forKey:@"key2"];
    [mutableDictionary removeObjectForKey:@"key1"];

    // 가변에서 불변으로 타입 변경
    // 일반적으로 [object mutableCopy]는 객체를 가변으로 만들고 [object copy]는 객체를 불변으로 만듭니다.
    NSMutableDictionary *aMutableDictionary = [aDictionary mutableCopy];
    NSDictionary *mutableDictionaryChanged = [mutableDictionary copy];


    // 세트 객체
    NSSet *set = [NSSet setWithObjects:@"Hello", @"Hello", @"World", nil];
    NSLog(@"%@", set); // => {(Hello, World)} 출력 (순서가 다를 수 있음)
    // Xcode 7부터 NSSet 객체는 타입을 지정할 수 있습니다 (제네릭).
    NSSet<NSString *> *stringSet = [NSSet setWithObjects:@"hello", @"world", nil];
    // NSMutableSet도 가변 세트 객체로 사용할 수 있습니다.
    NSMutableSet *mutableSet = [NSMutableSet setWithCapacity:2];
    [mutableSet addObject:@"Hello"];
    [mutableSet addObject:@"Hello"];
    NSLog(@"%@", mutableSet); // => {(Hello)} 출력

    ///////////////////////////////////////
    // 연산자
    ///////////////////////////////////////

    // 연산자는 C 언어처럼 작동합니다.
    // 예:
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (논리 AND)
    0 || 1; // => 1 (논리 OR)
    ~0x0F; // => 0xF0 (비트 부정)
    0x0F & 0xF0; // => 0x00 (비트 AND)
    0x01 << 1; // => 0x02 (비트 왼쪽 시프트 (1만큼))

    ///////////////////////////////////////
    // 제어 구조
    ///////////////////////////////////////

    // If-Else 문
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

    // Switch 문
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

    // While 루프 문
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++는 값을 사용한 후 ii를 제자리에서 증가시킵니다.
    } // => "0,"
      //           "1,"
      //           "2,"
      //           "3," 출력

    // For 루프 문
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // => "0,"
      //           "1,"
      //           "2,"
      //           "3," 출력

    // Foreach 문
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // => "0,"
      //           "1,"
      //           "2,"
      //           "3," 출력

    // 객체 for 루프 문. 모든 Objective-C 객체 타입과 함께 사용할 수 있습니다.
    for (id item in values) {
        NSLog(@"%@,", item);
    } // => "0,"
      //           "1,"
      //           "2,"
      //           "3," 출력

    // Try-Catch-Finally 문
    @try
    {
        // 여기에 문장을 작성합니다.
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"File Not Found on System" userInfo:nil];
    } @catch (NSException * e) // 모든 객체를 잡으려면 @catch (id exceptionName)를 사용합니다.
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally. Time to clean up.");
    } // => "Exception: File Not Found on System"
      //           "Finally. Time to clean up." 출력

    // NSError 객체는 사용자 실수 시 채울 함수 인수에 유용합니다.
    NSError *error = [NSError errorWithDomain:@"Invalid email." code:4 userInfo:nil];

    ///////////////////////////////////////
    // 객체
    ///////////////////////////////////////

    // 메모리를 할당하고 초기화하여 객체 인스턴스를 생성합니다.
    // 객체는 두 단계가 모두 완료될 때까지 완전히 작동하지 않습니다.
    MyClass *myObject = [[MyClass alloc] init];

    // Objective-C의 객체 지향 프로그래밍 모델은 객체 인스턴스에
    // 메시지를 전달하는 것을 기반으로 합니다.
    // Objective-C에서는 단순히 메서드를 호출하는 것이 아니라 메시지를 보냅니다.
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // 프로그램에서 사용한 메모리를 정리합니다.
    [pool drain];

    // @autoreleasepool의 끝
    }

    // 프로그램 종료
    return 0;
}

///////////////////////////////////////
// 클래스 및 함수
///////////////////////////////////////

// 헤더 파일(MyClass.h)에 클래스를 선언합니다.
// 클래스 선언 구문:
// @interface ClassName : ParentClassName <ImplementedProtocols>
// {
//    type name; <= 변수 선언;
// }
// @property type name; <= 프로퍼티 선언
// -/+ (type) 메서드 선언; <= 메서드 선언
// @end
@interface MyClass : NSObject <MyProtocol> // NSObject는 Objective-C의 기본 객체 클래스입니다.
{
    // 인스턴스 변수 선언 (인터페이스 또는 구현 파일에 있을 수 있음)
    int count; // 기본적으로 Protected 액세스.
    @private id data; // Private 액세스 (구현 파일에 선언하는 것이 더 편리함)
    NSString *name;
}
// setter 메서드를 자동으로 생성하기 위한 public 액세스 변수의 편리한 표기법
// 기본적으로 setter 메서드 이름은 'set' 뒤에 @property 변수 이름이 옵니다.
@property int propInt; // Setter 메서드 이름 = 'setPropInt'
@property (copy) id copyId; // (copy) => 할당 중에 객체 복사
// (readonly) => @interface 외부에서 값을 설정할 수 없음
@property (readonly) NSString *roString; // 접근자를 생성하려면 @implementation에서 @synthesize 사용
// 기본 'set' 이름 대신 getter 및 setter 이름을 사용자 정의할 수 있습니다.
@property (getter=lengthGet, setter=lengthSet:) int length;

// 메서드
+/- (return type)methodSignature:(Parameter Type *)parameterName;

// + 클래스 메서드용:
+ (NSString *)classMethod;
+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight;

// - 인스턴스 메서드용:
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

// 인수가 있는 생성자 메서드:
- (id)initWithDistance:(int)defaultDistance;
// Objective-C 메서드 이름은 매우 설명적입니다. 항상 인수에 따라 메서드 이름을 지정하십시오.

@end // 인터페이스의 끝을 나타냅니다.


// 구현 파일에서 public 변수에 액세스하려면 @property가 setter 메서드를
// 자동으로 생성합니다. 메서드 이름은 'set' 뒤에 @property 변수 이름이 옵니다.
MyClass *myClass = [[MyClass alloc] init]; // MyClass 객체 인스턴스 생성
[myClass setCount:10];
NSLog(@"%d", [myClass count]); // => 10 출력
// 또는 @interface에 정의된 사용자 정의 getter 및 setter 메서드 사용:
[myClass lengthSet:32];
NSLog(@"%i", [myClass lengthGet]); // => 32 출력
// 편의를 위해 점 표기법을 사용하여 객체 인스턴스 변수를 설정하고 액세스할 수 있습니다.
myClass.count = 45;
NSLog(@"%i", myClass.count); // => 45 출력

// 클래스 메서드 호출:
NSString *classMethodString = [MyClass classMethod];
MyClass *classFromName = [MyClass myClassFromName:@"Hello"];

// 인스턴스 메서드 호출:
MyClass *myClass = [[MyClass alloc] init]; // MyClass 객체 인스턴스 생성
NSString *stringFromInstanceMethod = [myClass instanceMethodWithParameter:@"Hello"];

// 선택자
// 메서드를 동적으로 표현하는 방법. 클래스의 메서드를 호출하고, 메서드를
// 함수를 통해 전달하여 다른 클래스가 호출해야 함을 알리고, 메서드를
// 변수로 저장하는 데 사용됩니다.
// SEL은 데이터 타입입니다. @selector()는 제공된 메서드 이름에서 선택자를 반환합니다.
// methodAParameterAsString:andAParameterAsNumber:는 MyClass의 메서드 이름입니다.
SEL selectorVar = @selector(methodAParameterAsString:andAParameterAsNumber:);
if ([myClass respondsToSelector:selectorVar]) { // 클래스에 메서드가 포함되어 있는지 확인
    // performSelector 함수에 보내기 위해 모든 메서드 인수를 하나의 객체에 넣어야 합니다.
    NSArray *arguments = [NSArray arrayWithObjects:@"Hello", @4, nil];
    [myClass performSelector:selectorVar withObject:arguments]; // 메서드 호출
} else {
    // NSStringFromSelector()는 주어진 선택자의 메서드 이름을 NSString으로 반환합니다.
    NSLog(@"MyClass does not have method: %@", NSStringFromSelector(selectedVar));
}

// 구현(MyClass.m) 파일에서 메서드를 구현합니다.
@implementation MyClass {
    long distance; // Private 액세스 인스턴스 변수
    NSNumber *height;
}

// 인터페이스 파일에서 public 변수에 액세스하려면 '_' 뒤에 변수 이름을 사용합니다.
_count = 5; // MyClass 인터페이스의 "int count" 참조
// 구현 파일에 정의된 변수에 액세스:
distance = 18; // MyClass 구현의 "long distance" 참조
// 구현에서 @property 변수를 사용하려면 @synthesize를 사용하여 접근자 변수를 만듭니다.
@synthesize roString = _roString; // _roString은 이제 @implementation에서 사용할 수 있습니다.

// 클래스 메서드를 호출하거나 객체를 인스턴스화하기 전에 호출됩니다.
+ (void)initialize
{
    if (self == [MyClass class]) {
        distance = 0;
    }
}

// initialize 메서드의 반대. 객체의 참조 카운트가 0일 때 호출됩니다.
- (void)dealloc
{
    [height release]; // ARC를 사용하지 않는 경우 클래스 변수 객체를 해제해야 합니다.
    [super dealloc];  // 그리고 부모 클래스 dealloc 호출
}

// 생성자는 클래스의 인스턴스를 만드는 방법입니다.
// 이것은 객체가 초기화될 때 호출되는 기본 생성자입니다.
- (id)init
{
    if ((self = [super init])) // 'super'는 부모 클래스에서 메서드를 액세스하는 데 사용됩니다.
    {
        self.count = 1; // 'self'는 객체가 자신을 호출하는 데 사용됩니다.
    }
    return self;
}
// 인수가 포함된 생성자를 만들 수 있습니다.
- (id)initWithDistance:(int)defaultDistance
{
    distance = defaultDistance;
    return self;
}

+ (NSString *)classMethod
{
    return @"Some string";
}

+ (MyClass *)myClassFromHeight:(NSNumber *)defaultHeight
{
    height = defaultHeight;
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

// Objective-C에는 private 메서드 선언이 없지만 시뮬레이션할 수 있습니다.
// private 메서드를 시뮬레이션하려면 @implementation에 메서드를 만들고 @interface에는 만들지 마십시오.
- (NSNumber *)secretPrivateMethod {
    return @72;
}
[self secretPrivateMethod]; // private 메서드 호출

// MyProtocol에 선언된 메서드
- (void)myProtocolMethod
{
    // 문장
}

@end // 구현의 끝을 나타냅니다.

///////////////////////////////////////
// 카테고리
///////////////////////////////////////
// 카테고리는 클래스를 확장하도록 설계된 메서드 그룹입니다. 조직적인 목적으로
// 기존 클래스에 새 메서드를 추가할 수 있습니다. 이것은 하위 클래스와 혼동해서는 안 됩니다.
// 하위 클래스는 객체의 기능을 변경하는 반면 카테고리는 객체에 기능을
// 추가합니다.
// 카테고리를 사용하면 다음을 수행할 수 있습니다.
// -- 조직적인 목적으로 기존 클래스에 메서드를 추가합니다.
// -- Objective-C 객체 클래스(예: NSString)를 확장하여 자신만의 메서드를 추가할 수 있습니다.
// -- 클래스에 보호 및 비공개 메서드를 만드는 기능을 추가합니다.
// 참고: 카테고리에서 기본 클래스의 메서드를 재정의하지 마십시오.
// 재정의하면 나중에 다른 카테고리 간에 컴파일러 오류가 발생할 수 있으며
// 기능을 추가만 하는 카테고리의 목적을 망칩니다. 메서드를 재정의하려면 대신 하위 클래스를 사용하십시오.

// 다음은 간단한 Car 기본 클래스입니다.
@interface Car : NSObject

@property NSString *make;
@property NSString *color;

- (void)turnOn;
- (void)accelerate;

@end

// 그리고 간단한 Car 기본 클래스 구현:
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

// 이제 Truck 객체를 만들고 싶다면 Car의 하위 클래스를 만들어
// Car의 기능을 트럭처럼 동작하도록 변경합니다. 하지만 기존 Car에
// 기능을 추가하고 싶다고 가정해 봅시다. 좋은 예는 차를 청소하는 것입니다. 따라서
// 이러한 청소 메서드를 추가하기 위해 카테고리를 만듭니다.
// @interface filename: Car+Clean.h (BaseClassName+CategoryName.h)
#import "Car.h" // 확장할 기본 클래스를 가져와야 합니다.

@interface Car (Clean) // 카테고리 이름은 기본 클래스 이름 뒤에 () 안에 있습니다.

- (void)washWindows; // Car 객체에 추가하는 새 메서드의 이름.
- (void)wax;

@end

// @implementation filename: Car+Clean.m (BaseClassName+CategoryName.m)
#import "Car+Clean.h" // Clean 카테고리의 @interface 파일을 가져옵니다.

@implementation Car (Clean)

- (void)washWindows {
    NSLog(@"Windows washed.");
}
- (void)wax {
    NSLog(@"Waxed.");
}

@end

// 모든 Car 객체 인스턴스는 카테고리를 사용할 수 있습니다. 가져오기만 하면 됩니다.
#import "Car+Clean.h" // 사용하려는 만큼 다른 카테고리를 가져옵니다.
#import "Car.h" // 원래 기능을 사용하려면 기본 클래스도 가져와야 합니다.

int main (int argc, const char * argv[]) {
    @autoreleasepool {
        Car *mustang = [[Car alloc] init];
        mustang.color = @"Red";
        mustang.make = @"Ford";

        [mustang turnOn]; // 기본 Car 클래스의 메서드 사용.
        [mustang washWindows]; // Car의 Clean 카테고리 메서드 사용.
    }
    return 0;
}

// Objective-C에는 보호된 메서드 선언이 없지만 시뮬레이션할 수 있습니다.
// 보호된 메서드를 시뮬레이션하려면 모든 보호된 메서드를 포함하는 카테고리를 만든 다음
// Car 클래스에 속한 클래스의 @implementation 파일에만 가져옵니다.
@interface Car (Protected) // 메서드가 보호됨을 기억하기 위해 카테고리 이름을 'Protected'로 지정합니다.

- (void)lockCar; // 여기에 나열된 메서드는 Car 객체만 만들 수 있습니다.

@end
// 보호된 메서드를 사용하려면 카테고리를 가져온 다음 메서드를 구현합니다.
#import "Car+Protected.h" // @implementation 파일에만 가져오는 것을 기억하십시오.

@implementation Car

- (void)lockCar {
    NSLog(@"Car locked."); // Car 인스턴스는 @interface에 없기 때문에 lockCar를 사용할 수 없습니다.
}

@end

///////////////////////////////////////
// 확장
///////////////////////////////////////
// 확장을 사용하면 @interface의 public 액세스 프로퍼티 속성 및 메서드를 재정의할 수 있습니다.
// @interface filename: Shape.h
@interface Shape : NSObject // 기본 Shape 클래스 확장은 아래에서 재정의됩니다.

@property (readonly) NSNumber *numOfSides;

- (int)getNumOfSides;

@end
// numOfSides 변수 또는 getNumOfSides 메서드를 재정의하여 확장으로 편집할 수 있습니다.
// @implementation filename: Shape.m
#import "Shape.h"
// 확장은 클래스 @implementation과 동일한 파일에 있습니다.
@interface Shape () // 기본 클래스 이름 뒤의 ()는 확장을 선언합니다.

@property (copy) NSNumber *numOfSides; // numOfSides를 readonly 대신 copy로 만듭니다.
-(NSNumber)getNumOfSides; // getNumOfSides가 int 대신 NSNumber를 반환하도록 합니다.
-(void)privateMethod; // 확장 내에서 새 private 메서드를 만들 수도 있습니다.

@end
// 주 @implementation:
@implementation Shape

@synthesize numOfSides = _numOfSides;

-(NSNumber)getNumOfSides { // 확장 내의 모든 문장은 @implementation에 있어야 합니다.
    return _numOfSides;
}
-(void)privateMethod {
    NSLog(@"Private method created by extension. Shape instances cannot call me.");
}

@end

// Xcode 7.0부터 제네릭 클래스를 만들 수 있어
// 과도한 상용구 작성 없이 더 나은 타입 안전성과 명확성을
// 제공할 수 있습니다.
@interface Result<__covariant A> : NSObject

- (void)handleSuccess:(void(^)(A))success
              failure:(void(^)(NSError *))failure;

@property (nonatomic) A object;

@end

// 이제 이 클래스의 인스턴스를 다음과 같이 선언할 수 있습니다.
Result<NSNumber *> *result;
Result<NSArray *> *result;

// 이러한 각 경우는 Result의 인터페이스를 다시 작성하고
// A에 적절한 타입을 대체하는 것과 동일합니다.
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

// 그러나 문제를 해결하기 위해 하나의 클래스를 작성하는 것이
// 두 개를 작성하는 것보다 항상 선호된다는 것은 명백해야 합니다.

// Clang은 @implementation에서 제네릭 타입을 허용하지 않으므로
// Result의 @implemnation은 다음과 같아야 합니다.

@implementation Result

- (void)handleSuccess:(void (^)(id))success
              failure:(void (^)(NSError *))failure {
  // 무언가 수행
}

@end


///////////////////////////////////////
// 프로토콜
///////////////////////////////////////
// 프로토콜은 모든 클래스에서 구현할 수 있는 메서드를 선언합니다.
// 프로토콜은 클래스 자체가 아닙니다. 다른 객체가 구현할 책임이 있는
// 인터페이스를 정의할 뿐입니다.
// @protocol filename: "CarUtilities.h"
@protocol CarUtilities <NSObject> // <NSObject> => 이 프로토콜이 포함하는 다른 프로토콜의 이름.
    @property BOOL engineOn; // 채택하는 클래스는 모든 정의된 @properties를 @synthesize해야 하며
    - (void)turnOnEngine; // 모든 정의된 메서드를 구현해야 합니다.
@end
// 아래는 프로토콜을 구현하는 예제 클래스입니다.
#import "CarUtilities.h" // @protocol 파일을 가져옵니다.

@interface Car : NSObject <CarUtilities> // 프로토콜의 이름은 <> 안에 들어갑니다.
    // CarUtilities에 대한 @property 또는 메서드 이름은 여기에 필요하지 않습니다. @implementation만 필요합니다.
- (void)turnOnEngineWithUtilities:(id <CarUtilities>)car; // 프로토콜을 데이터로 사용할 수도 있습니다.
@end
// @implementation은 프로토콜에 대한 @properties 및 메서드를 구현해야 합니다.
@implementation Car : NSObject <CarUtilities>

@synthesize engineOn = _engineOn; // engineOn @property에 대한 @synthesize 문을 만듭니다.

- (void)turnOnEngine { // 원하는 대로 turnOnEngine을 구현합니다. 프로토콜은
    _engineOn = YES; // 메서드를 구현하는 방법을 정의하지 않고 구현해야 한다는 것만 요구합니다.
}
// 프로토콜을 데이터로 사용할 수 있습니다. 어떤 메서드와 변수가 구현되었는지 알기 때문입니다.
- (void)turnOnEngineWithCarUtilities:(id <CarUtilities>)objectOfSomeKind {
    [objectOfSomeKind engineOn]; // 객체 변수에 액세스할 수 있습니다.
    [objectOfSomeKind turnOnEngine]; // 그리고 내부의 메서드.
    [objectOfSomeKind engineOn]; // YES일 수도 있고 아닐 수도 있습니다. 클래스는 원하는 대로 구현합니다.
}

@end
// Car 인스턴스는 이제 프로토콜에 액세스할 수 있습니다.
Car *carInstance = [[Car alloc] init];
[carInstance setEngineOn:NO];
[carInstance turnOnEngine];
if ([carInstance engineOn]) {
    NSLog(@"Car engine is on."); // => "Car engine is on." 출력
}
// 'id' 타입의 객체가 프로토콜 메서드를 호출하기 전에 프로토콜을 구현하는지 확인하십시오.
if ([myClass conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"This does not run as the MyClass class does not implement the CarUtilities protocol.");
} else if ([carInstance conformsToProtocol:@protocol(CarUtilities)]) {
    NSLog(@"This does run as the Car class implements the CarUtilities protocol.");
}
// 카테고리도 프로토콜을 구현할 수 있습니다: @interface Car (CarCategory) <CarUtilities>
// 여러 프로토콜을 구현할 수 있습니다: @interface Car : NSObject <CarUtilities, CarCleaning>
// 참고: 둘 이상의 프로토콜이 서로 의존하는 경우 전방 선언해야 합니다.
#import "Brother.h"

@protocol Brother; // 전방 선언 문. 이것이 없으면 컴파일러가 오류를 발생시킵니다.

@protocol Sister <NSObject>

- (void)beNiceToBrother:(id <Brother>)brother;

@end

// 문제는 Sister가 Brother에 의존하고 Brother가 Sister에 의존한다는 것입니다.
#import "Sister.h"

@protocol Sister; // 이 줄은 재귀를 중지하여 문제를 해결합니다.

@protocol Brother <NSObject>

- (void)beNiceToSister:(id <Sister>)sister;

@end


///////////////////////////////////////
// 블록
///////////////////////////////////////
// 블록은 데이터로 사용할 수 있는 함수와 같은 코드 문입니다.
// 아래는 정수 인수를 받아 인수에 4를 더한 값을 반환하는 간단한 블록입니다.
^(int n) {
    return n + 4;
}
int (^addUp)(int n); // 블록을 저장할 변수를 선언합니다.
void (^noParameterBlockVar)(void); // 인수가 없는 블록의 예제 변수 선언.
// 블록은 동일한 범위의 변수에 액세스할 수 있습니다. 그러나 변수는 읽기 전용이며
// 블록에 전달되는 값은 블록이 생성될 때 변수의 값입니다.
int outsideVar = 17; // addUp을 선언한 후 outsideVar를 편집해도 outsideVar는 여전히 17입니다.
__block long mutableVar = 3; // __block은 outsideVar와 달리 변수를 블록에 쓸 수 있도록 합니다.
addUp = ^(int n) { // 매개변수를 받지 않는 블록을 가지려면 (int n)을 제거합니다.
    NSLog(@"You may have as many lines in a block as you would like.");
    NSSet *blockSet; // 또한 지역 변수를 선언할 수 있습니다.
    mutableVar = 32; // __block 변수에 새 값을 할당합니다.
    return n + outsideVar; // 반환 문은 선택 사항입니다.
}
int addUp = addUp(10 + 16); // 인수로 블록 코드를 호출합니다.
// 블록은 나중에 호출되거나 콜백을 위해 함수의 인수로 자주 사용됩니다.
@implementation BlockExample : NSObject

 - (void)runBlock:(void (^)(NSString))block {
    NSLog(@"Block argument returns nothing and takes in a NSString object.");
    block(@"Argument given to block to execute."); // 블록 호출.
 }

 @end


///////////////////////////////////////
// 메모리 관리
///////////////////////////////////////
/*
애플리케이션에서 사용되는 각 객체에 대해 해당 객체에 대한 메모리를 할당해야 합니다. 애플리케이션이
해당 객체 사용을 마치면 애플리케이션 효율성을 보장하기 위해 메모리를 해제해야 합니다.
Objective-C는 가비지 컬렉션을 사용하지 않고 대신 참조 계산을 사용합니다.
객체에 대한 참조가 하나 이상 있는 한(객체를 "소유"한다고도 함), 객체는
사용할 수 있습니다("소유권"이라고 함).

인스턴스가 객체를 소유하면 참조 카운터가 1 증가합니다. 객체가
해제되면 참조 카운터가 1 감소합니다. 참조 카운트가 0이 되면
객체는 메모리에서 제거됩니다.

모든 객체 상호 작용에서 다음 패턴을 따르십시오.
(1) 객체 생성, (2) 객체 사용, (3) 메모리에서 객체 해제.
*/

MyClass *classVar = [MyClass alloc]; // 'alloc'은 classVar의 참조 카운트를 1로 설정합니다. 객체에 대한 포인터를 반환합니다.
[classVar release]; // classVar의 참조 카운트를 감소시킵니다.
// 'retain'은 기존 객체 인스턴스의 소유권을 주장하고 참조 카운트를 증가시킵니다. 객체에 대한 포인터를 반환합니다.
MyClass *newVar = [classVar retain]; // classVar가 해제되어도 newVar가 소유자이므로 객체는 여전히 메모리에 있습니다.
[classVar autorelease]; // @autoreleasepool 블록 끝에서 객체의 소유권을 제거합니다. 객체에 대한 포인터를 반환합니다.

// @property는 작은 편리한 정의를 위해 'retain' 및 'assign'을 사용할 수 있습니다.
@property (retain) MyClass *instance; // 이전 값을 해제하고 새 값을 유지합니다(강한 참조).
@property (assign) NSSet *set; // 이전 값을 유지/해제하지 않고 새 값에 대한 포인터(약한 참조).

// 자동 참조 계산(ARC)
// 메모리 관리가 번거로울 수 있으므로 Xcode 4.2 및 iOS 4에서 자동 참조 계산(ARC)이 도입되었습니다.
// ARC는 retain, release 및 autorelease를 자동으로 삽입하는 컴파일러 기능이므로 ARC를 사용하는 경우
// retain, release 또는 autorelease를 사용해서는 안 됩니다.
MyClass *arcMyClass = [[MyClass alloc] init];
// ... arcMyClass를 사용하는 코드
// ARC가 없으면 arcMyClass 사용을 마친 후 [arcMyClass release]를 호출해야 합니다. 그러나 ARC를 사용하면
// 그럴 필요가 없습니다. 이 해제 문을 자동으로 삽입합니다.

// 'assign' 및 'retain' @property 속성의 경우 ARC를 사용하여 'weak' 및 'strong'을 사용합니다.
@property (weak) MyClass *weakVar; // 'weak'은 객체의 소유권을 갖지 않습니다. 원래 인스턴스의 참조 카운트가
// 0으로 설정되면 weakVar는 애플리케이션 충돌을 피하기 위해 자동으로 nil 값을 받습니다.
@property (strong) MyClass *strongVar; // 'strong'은 객체의 소유권을 갖습니다. 객체가 메모리에 남아 있도록 보장합니다.

// 일반 변수(@property로 선언되지 않은 변수)의 경우 다음을 사용합니다.
__strong NSString *strongString; // 기본값. 변수는 범위를 벗어날 때까지 메모리에 유지됩니다.
__weak NSSet *weakSet; // 기존 객체에 대한 약한 참조. 기존 객체가 해제되면 weakSet은 nil로 설정됩니다.
__unsafe_unretained NSArray *unsafeArray; // __weak와 같지만 기존 객체가 해제될 때 unsafeArray는 nil로 설정되지 않습니다.
```

## 더 읽을거리

[위키백과 Objective-C](http://en.wikipedia.org/wiki/Objective-C)

[Objective-C로 프로그래밍하기. Apple PDF 책](https://developer.apple.com/library/ios/documentation/cocoa/conceptual/ProgrammingWithObjectiveC/ProgrammingWithObjectiveC.pdf)

[iOS용 Objective-C로 프로그래밍하기](https://developer.apple.com/library/ios/documentation/General/Conceptual/DevPedia-CocoaCore/ObjectiveC.html)

[Mac OSX용 Objective-C로 프로그래밍하기](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html)

[고등학생을 위한 iOS: 시작하기](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
