---

language: Objective-C
contributors:
    - ["Eugene Yagrushkin", "www.about.me/yagrushkin"]
    - ["Yannick Loriot", "https://github.com/YannickL"]
filename: LearnObjectiveC.m

---

Objective-C is the main programming language used by Apple for the OS X and iOS operating systems and their respective frameworks, Cocoa and Cocoa Touch.
It is a general-purpose, object-oriented programming language that adds Smalltalk-style messaging to the C programming language. 

```Objective-C
// Single-line comments start with //

/*
Multi-line comments look like this.
*/

// Imports the Foundation headers with #import
#import <Foundation/Foundation.h>

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
    NSLog(@"Hello %@!", worldString); // Print "Hello World!"
    
    // Character literals
    NSNumber *theLetterZNumber = @'Z';
    char theLetterZ            = [theLetterZNumber charValue];
    NSLog(@"%c", theLetterZ);

    // Integral literals
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

    // Floating point literals
    NSNumber *piFloatNumber = @3.141592654F;
    float piFloat           = [piFloatNumber floatValue];
    NSLog(@"%f", piFloat);
    
    NSNumber *piDoubleNumber = @3.1415926535;
    piDouble                 = [piDoubleNumber doubleValue];
    NSLog(@"%f", piDouble);

    // BOOL literals
    NSNumber *yesNumber = @YES;
    NSNumber *noNumber  = @NO;

    // Array object
    NSArray *anArray      = @[@1, @2, @3, @4];
    NSNumber *thirdNumber = anArray[2];
    NSLog(@"Third number = %@", thirdObject); // Print "Third number = 3"

    // Dictionary object
    NSDictionary *aDictionary = @{ @"key1" : @"value1", @"key2" : @"value2" };
    NSObject *valueObject     = aDictionary[@"A Key"];
    NSLog(@"Object = %@", valueObject); // Print "Object = (null)"

    ///////////////////////////////////////
    // Operators
    ///////////////////////////////////////
    
    // The operators works like in the C language
    // For example:
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
    switch (2) {
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
    
    // While loops exist
    int ii = 0;
    while (ii < 4)
    {
        NSLog(@"%d,", ii++); // ii++ increments ii in-place, after using its value.
    } // => prints "0, 
                    1,
                    2,
                    3,"

    // For loops too
    int jj;
    for (jj=0; jj < 4; jj++)
    {
        NSLog(@"%d,", ii++);
    } // => prints "0, 
                    1,
                    2,
                    3,"
     
    // Foreach               
    NSArray *values = @[@0, @1, @2, @3];
    for (NSNumber *value in values)
    {
        NSLog(@"%@,", value);
    } // => prints "0, 
                    1,
                    2,
                    3,"

    // Clean up the memory you used into your program
    [pool drain];
    
    // End the program
    return 0;
}

///////////////////////////////////////
// Classes And Functions
///////////////////////////////////////

// Declare your class in a header(.h) file:

@interface UserObject : NSObject
{
    // instance variables
}

// Class method
+ (NSString *)classMethod;

// Instance method
- (NSString *)instanceMethodWithParmeter:(NSString *)string;

@end

// Implement the methods in an implementation (.m) file:

@implementation UserObject

+ (NSString *)classMethod
{
    return @"SomeString";
}

- (NSString *)instanceMethodWithParmeter:(NSString *)string
{
    return @"New string";
}

- (NSNumber *)methodAParameterAsString:(NSString*)string andAParameterAsNumber:(NSNumber *)number
{
    return @42;
}

@end

// Create an object instance by allocating memory and initializing it. An object is not fully functional until both steps have been completed.
UserObject *someObject = [[UserObject alloc] init];

##Calling Methods

// The Objective-C model of object-oriented programming is based on message passing to object instances. 
// In Objective-C one does not simply call a method; one sends a message.

[someObject instanceMethodWithParmeter:@"Steve Jobs"];

##Nested Messages
// nested messages look like this:

[someObject instanceMethodWithParmeter:[someObject otherMethodWithString:@"Jony Ive"]];

```
## Further Reading

[Wikipedia Objective-C](http://en.wikipedia.org/wiki/Objective-C)

[Learning Objective-C](http://developer.apple.com/library/ios/referencelibrary/GettingStarted/Learning_Objective-C_A_Primer/)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
