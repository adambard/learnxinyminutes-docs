---
language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
lang: vi-vn
filename: LearnObjectiveC-vi.m
---

Objective-C là ngôn ngữ lập trình chính được sử dụng bởi Apple cho các hệ điều hành macOS, iOS và các framework tương ứng của họ, Cocoa và Cocoa Touch.
Nó là một ngôn ngữ lập trình mục đích tổng quát, hướng đối tượng có bổ sung thêm kiểu truyền thông điệp giống Smalltalk vào ngôn ngữ lập trình C.

```objective-c
// Chú thích dòng đơn bắt đầu với //

/*
Chú thích đa dòng trông như thế này.
*/

// Nhập các headers của framework Foundation với cú pháp #import
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Đầu vào chương trình của bạn là một hàm gọi là
// main với một kiểu trả về kiểu integer.
int main (int argc, const char * argv[])
{
    // Tạo một autorelease pool để quản lý bộ nhớ vào chương trình
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

    // Sử dụng hàm NSLog() để in ra các dòng lệnh vào console
    NSLog(@"Hello World!"); // Print the string "Hello World!"

    ///////////////////////////////////////
    // Kiểu & Biến (Types & Variables)
    ///////////////////////////////////////

    // Khai báo số nguyên
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;

    // Khai báo đối tượng
    // Đặt dấu nháy * vào trước tên biến cho khai báo đối tượng strong
    MyClass *myObject1 = nil;  // Strong
    id       myObject2 = nil;  // Weak
    // %@ là một đối tượng
    // 'miêu tả' ('desciption') là thông lệ để trình bày giá trị của các Đối tượng
    NSLog(@"%@ và %@", myObject1, [myObject2 description]); // In ra "(null) và (null)"

    // Chuỗi
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // In ra "Hello World!"

    // Ký tự literals
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue];
    NSLog(@"%c", theLetterZ);

    // Số nguyên literals
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

    // Dấu phẩy động (floating point) literals
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue];
    NSLog(@"%f", piFloat);

    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble                 = [piDoubleNumber doubleValue];
    NSLog(@"%f", piDouble);

    // BOOL literals
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;

    // Đối tượng Mảng
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // In ra "Third number = 3"

    // Đối tượng Từ điển
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Đối tượng = %@", valueObject); // In ra "Object = (null)"

    ///////////////////////////////////////
    // Toán Tử (Operators)
    ///////////////////////////////////////

    // Các toán tử cũng hoạt động giống như ngôn ngữ C
    // Ví dụ:
    2 + 5; // => 7
    4.2f + 5.1f; // => 9.3f
    3 == 2; // => 0 (NO)
    3 != 2; // => 1 (YES)
    1 && 1; // => 1 (Logical and)
    0 || 1; // => 1 (Logical or)
    ~0x0F; // => 0xF0 (bitwise negation)
    0x0F & 0xF0; // => 0x00 (bitwise AND)
    0x01 << 1; // => 0x02 (bitwise dịch trái (bởi 1))

    /////////////////////////////////////////////
    // Cấu Trúc Điều Khiển (Controls Structures)
    /////////////////////////////////////////////

    // Câu lệnh If-Else
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

    // Câu lệnh Switch
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

    // Câu lệnh vòng lặp While
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ tăng dần, sau khi sử dụng giá trị của nó.
    } // => in ra  "0,"
      //           "1,"
      //           "2,"
      //           "3,"

    // Câu lệnh vòng lặp For
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // => in ra  "0,"
      //           "1,"
      //           "2,"
      //           "3,"

    // Câu lệnh Foreach
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // => in ra  "0,"
      //           "1,"
      //           "2,"
      //           "3,"

    // Câu lệnh Try-Catch-Finally
    @try
    {
        // Your statements here
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"Không Tìm Thấy Tập Tin trên Hệ Thống" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally");
    } // => in ra  "Exception: Không Tìm Thấy Tập Tin trên Hệ Thống"
      //           "Finally"

    ///////////////////////////////////////
    // Đối Tượng (Objects)
    ///////////////////////////////////////

    // Tạo một thực thể đối tượng bằng cách phân vùng nhớ và khởi tạo đối tượng đó.
    // Một đối tượng sẽ không thật sự hoạt động cho đến khi cả 2 bước alloc] init] được hoàn thành
    MyClass *myObject = [[MyClass alloc] init];

    // Mô hình lập trình hướng đối tượng của Objective-C dựa trên việc truyền thông điệp (message)
    // và các thực thể đối tượng với nhau.
    // Trong Objective-C một đối tượng không đơn thuần gọi phương thức; nó truyền thông điệp.
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Dọn dẹp vùng nhớ mà bạn đã dùng ở chương trình
    [pool drain];

    // Kết thúc chương trình
    return 0;
}

///////////////////////////////////////
// Lớp và Hàm (Classes & Functions)
///////////////////////////////////////

// Khai báo lớp của bạn ở một tập tin header (MyClass.h):
// Cú pháp Khai Báo Lớp:
// @interface ClassName : ParentClassName <ImplementedProtocols>
// {
//    Khai báo biến thành viên;
// }
// -/+ (type) Khai báo method;
// @end
@interface MyClass : NSObject <MyProtocol>
{
    int count;
    id data;
    NSString *name;
}
// Ký hiệu (notation) tiện ích để tự động khởi tạo public getter và setter
@property int count;
@property (copy) NSString *name; // Sao chép đối tượng trong quá trình gán.
@property (readonly) id data;    // Chỉ khai báo phương thức getter.

// Phương thức
+/- (return type)methodSignature:(Parameter Type *)parameterName;

// dấu '+' cho phương thức lớp
+ (NSString *)classMethod;

// dấu '-' cho phương thức thực thể
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

@end

// Thực thi các phương thức trong một tập tin thực thi (MyClass.m):

@implementation MyClass

// Gọi khi đối tượng được release
- (void)dealloc
{
}

// Phương thức khởi tạo (Constructors) là một cách để tạo các lớp
// Đây là phương thức khởi tạo mặc định được gọi khi đối tượng được khởi tạo
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

// Các phương thức được khai báo vào MyProtocol
- (void)myProtocolMethod
{
    // câu lệnh
}

@end

/*
 * Một protocol khai báo các phương thức mà có thể thực thi bởi bất kỳ lớp nào.
 * Các protocol chính chúng không phải là các lớp. Chúng chỉ đơn giản là định ra giao diện (interface)
 * mà các đối tượng khác có trách nhiệm sẽ thực thi.
 */
@protocol MyProtocol
    - (void)myProtocolMethod;
@end



```
## Xem Thêm

+ [Wikipedia Objective-C](http://en.wikipedia.org/wiki/Objective-C)

+ Apple Docs':
    + [Learning Objective-C](http://developer.apple.com/library/ios/referencelibrary/GettingStarted/Learning_Objective-C_A_Primer/)

    + [Programming With Objective-C](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/ProgrammingWithObjectiveC/Introduction/Introduction.html)

    + [Object-Oriented Programming with Objective-C](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/OOP_ObjC/Introduction/Introduction.html#//apple_ref/doc/uid/TP40005149)

    + [Coding Guidelines for Cocoa](https://developer.apple.com/library/mac/documentation/Cocoa/Conceptual/CodingGuidelines/CodingGuidelines.html)

+ [iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
