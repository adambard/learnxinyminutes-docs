---

language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
filename: LearnObjectiveC.m

---

Objective-C is the main programming language used by Apple for the OS X and iOS operating systems and their respective frameworks, Cocoa and Cocoa Touch.
It is a general-purpose, object-oriented programming language that adds Smalltalk-style messaging to the C programming language. 

```cpp
// Single-line comments start with //

/*
Multi-line comments look like this.
*/

// Imports the Foundation headers with #import
#import <Foundation/Foundation.h>
#import "MyClass.h"

// Your program's entry point is a function called
// main with an integer return type.
int main (int argc, const char * argv[])
{
    // Create an autorelease pool to manage the memory into the program
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
 
    // Use NSLog to print lines to the console
    NSLog(@"Hello World!"); // Print the string "Hello World!"
 
    ///////////////////////////////////////
    // Types & Variables
    ///////////////////////////////////////
    
    // Primitive declarations
    int myPrimitive1  = 1;
    long myPrimitive2 = 234554664565;
    
    // Object declarations
    // Put the * in front of the variable names for strongly-typed object declarations
    MyClass *myObject1 = nil;  // Strong typing
    id       myObject2 = nil;  // Weak typing
    // %@ is an object
    // 'description' is a convention to display the value of the Objects
    NSLog(@"%@ and %@", myObject1, [myObject2 description]); // Print "(null) and (null)"
    
    // String
    NSString *worldString = @"World";
    NSLog(@"Hello %@!", worldString); // prints => "Hello World!"
    // The mutable version of NSString is NSMutableString that allows to edit 
    // individual characters or append strings together. 
    NSMutableString *mutableString = [NSMutableString stringWithString:@"Hello"];
    [mutableString appendString:@" World!"];
    NSLog(@"%@", mutableString); // prints => "Hello World!"
    
    // Character literals
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue]; // or 'Z'
    NSLog(@"%c", theLetterZ);

    // Integral literals
    NSNumber *fortyTwoNumber = @42;
    int fortyTwo             = [fortyTwoNumber intValue]; // or 42
    NSLog(@"%i", fortyTwo);
    
    NSNumber *fortyTwoUnsignedNumber = @42U;
    unsigned int fortyTwoUnsigned    = [fortyTwoUnsignedNumber unsignedIntValue]; // or 42
    NSLog(@"%u", fortyTwoUnsigned);
    
    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:42];
    short fortyTwoShort           = [fortyTwoShortNumber shortValue]; // or 42
    NSLog(@"%hi", fortyTwoShort);

    NSNumber *fortyTwoShortNumber = [NSNumber numberWithShort:41];
    unsigned short fortyTwoUnsigned = [fortyTwoShortNumber unsignedShortValue]; // or 41
    NSLog(@"%hu", fortyTwoUnsigned);
    
    NSNumber *fortyTwoLongNumber = @42L;
    long fortyTwoLong            = [fortyTwoLongNumber longValue]; // or 42
    NSLog(@"%li", fortyTwoLong);

    NSNumber *fortyTwoLongNumber = @53L;
    unsigned long fiftyThreeUnsigned = [fortyTwoLongNumber unsignedLongValue]; // or 53
    NSLog(@"%lu", fiftyThreeUnsigned);

    // Floating point literals
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue]; // or 3.141592654f
    NSLog(@"%f", piFloat); // prints => 3.141592654
    NSLog(@"%5.2f", piFloat); // prints => " 3.14"
    
    NSNumber *piDoubleNumber = @3.1415926535;
    double piDouble                 = [piDoubleNumber doubleValue]; // or 3.1415926535
    NSLog(@"%f", piDouble);
    NSLog(@"%4.2f", piDouble); // prints => "3.14"

    // NSDecimalNumber is Objective-C's fixed-point class more precise then float or double
    NSDecimalNumber *oneDecNum = [NSDecimalNumber decimalNumberWithString:@"10.99"];
    NSDecimalNumber *twoDecNum = [NSDecimalNumber decimalNumberWithString:@"5.002"];
    // NSDecimal isn't able to use standard +, -, *, / operators. NSDecimalNumber has its own:
    [oneDecNum decimalNumberByAdding:twoDecNum]; // add
    [oneDecNum decimalNumberBySubtracting:twoDecNum];
    [oneDecNum decimalNumberByMultiplyingBy:twoDecNum];
    [oneDecNum decimalNumberByDividingBy:twoDecNum];
    NSLog(@"%@", oneDecNum); // prints => 10.99 as NSDecimalNumber is immutable. 

    // BOOL literals
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;
    // or
    BOOL yesBool = YES;
    BOOL noBool = NO;
    NSLog(@"%i", yesBool); // prints => 1

    // Array object
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdNumber); // Print "Third number = 3"
    // NSMutableArray is mutable version of NSArray allowing to change items in array
    // and extend or shrink array object. Convenient, but not as efficient as NSArray.
    NSMutableArray *mutableArray = [NSMutableArray arrayWithCapacity:2];
    [mutableArray addObject:@"Hello"];
    [mutableArray addObject:@"World"];
    [mutableArray removeObjectAtIndex:0];
    NSLog(@"%@", [mutableArray objectAtIndex:0]); // prints => "World"

    // Dictionary object
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // Print "Object = (null)"

    // Set object
    NSSet *set = [NSSet setWithObjects:@"Hello", @"Hello", @"World", nil];
    NSLog(@"%@", set); // prints => {(Hello, World)}

    ///////////////////////////////////////
    // Operators
    ///////////////////////////////////////
    
    // The operators works like in the C language
    // For example:
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
    // Control Structures
    ///////////////////////////////////////

    // If-Else statement
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

    // Switch statement
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
    
    // While loops statements
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ increments ii in-place, after using its value.
    } // => prints "0," 
      //           "1,"
      //           "2,"
      //           "3,"

    // For loops statements
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", jj);
    } // => prints "0," 
      //           "1,"
      //           "2,"
      //           "3,"
     
    // Foreach statements             
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // => prints "0," 
      //           "1,"
      //           "2,"
      //           "3,"

    // Try-Catch-Finally statements
    @try
    {
        // Your statements here
        @throw [NSException exceptionWithName:@"FileNotFoundException"
                            reason:@"File Not Found on System" userInfo:nil];
    } @catch (NSException * e)
    {
        NSLog(@"Exception: %@", e);
    } @finally
    {
        NSLog(@"Finally");
    } // => prints "Exception: File Not Found on System"
      //           "Finally"
 
    ///////////////////////////////////////
    // Objects
    ///////////////////////////////////////
    
    // Create an object instance by allocating memory and initializing it.
    // An object is not fully functional until both steps have been completed.
    MyClass *myObject = [[MyClass alloc] init];
        
    // The Objective-C model of object-oriented programming is based on message
    // passing to object instances. 
    // In Objective-C one does not simply call a method; one sends a message.
    [myObject instanceMethodWithParameter:@"Steve Jobs"];

    // Clean up the memory you used into your program
    [pool drain];
    
    // End the program
    return 0;
}

///////////////////////////////////////
// Classes And Functions
///////////////////////////////////////

// Declare your class in a header(MyClass.h) file:
// Class Declaration Syntax:
// @interface ClassName : ParentClassName <ImplementedProtocols>
// {
//    Member variable declarations;
// }
// -/+ (type) Method declarations;
// @end
@interface MyClass : NSObject <MyProtocol>
{
    int count;
    id data;
    NSString *name;
}
// Convenience notation to auto generate public getter and setter
@property int count;
@property (copy) NSString *name; // Copy the object during assignment.
@property (readonly) id data;    // Declare only a getter method.
// To access public variable in implementation file, use '_' followed by variable name:
_count = 5;
NSLog(@"%d", _count); // prints => 5
// To access public variable outside implementation file, @property generates setter method
// automatically. Method name is 'set' followed by @property variable name:
[objInitVar setCount:10]; // objInitVar = variable of object instance @property resides in.
NSLog(@"%@", [objInitVar count]); // prints => 10

// Methods
+/- (return type)methodSignature:(Parameter Type *)parameterName;

// + for class method
+ (NSString *)classMethod;

// - for instance method
- (NSString *)instanceMethodWithParameter:(NSString *)string;
- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number;

@end

// Implement the methods in an implementation (MyClass.m) file:

@implementation MyClass

// Call when the object is releasing
- (void)dealloc
{
}

// Constructors are a way of creating classes
// This is a default constructor which is called when the object is creating
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

// Methods declared into MyProtocol
- (void)myProtocolMethod
{
    // statements
}

@end

/*
 * A protocol declares methods that can be implemented by any class.
 * Protocols are not classes themselves. They simply define an interface
 * that other objects are responsible for implementing.
 */
@protocol MyProtocol
    - (void)myProtocolMethod;
@end



```
## Further Reading

[Wikipedia Objective-C](http://en.wikipedia.org/wiki/Objective-C)

[Learning Objective-C](http://developer.apple.com/library/ios/referencelibrary/GettingStarted/Learning_Objective-C_A_Primer/)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
