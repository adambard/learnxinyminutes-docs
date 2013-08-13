---
language: Objectiv-C
author: Eugene Yagrushkin
author_url: www.about.me/yagrushkin
filename: learnc.Objectiv-C
---

Objective-C is the main programming language used by Apple for the OS X and iOS operating systems and their respective APIs, Cocoa and Cocoa Touch.
It's is a general-purpose, object-oriented programming language that adds Smalltalk-style messaging to the C programming language. 

```Objective-C
// Single-line comments start with //

/*
Multi-line comments look like this.
*/

##Basic types
// all the primitive variable types are the same as in C
// char, int, long, double, float


// Simple, common classes
// number
NSNumber *firstNumber = @1;
NSNumber *secondNumber = @23.0;
NSNumber *boolNumber = @YES; 

// string
NSString *aString = @"some string";

// array
NSArray *array = @[ @1, @2];

// dictionary
NSDictionay *dictionary = @{ @"aKey" : @"aValue", @"aKey2" : @"aValue2" };

// Import headers with #import
#import <UIKit/UIKit.h>
#import "SomeAppDelegate.h"

##Coding classes

// Declare your class in a header(.h) file:

@interface UserObject : NSObject{
// instance variables
}

// Class method
	+ (NSString*) ClassMethod;

// Instance method
	- (NSString*) instanceMethodWithParmeter:(NSString*)string;

@end

// Add class methods in an implementation (.m) file:

@implementation UserObject

+ (NSString*) ClassMethod{
	return @"SomeString";
}

- (NSString*) instanceMethodWithParmeter:(NSString*)string;
{
	return @"New string";
}

- (NSString*) otherMethodWithString:(NSString*)string;
{
	return [NSString stringWithString:string];
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

[Objectively Speaking: A Crash Course in Objective-C](http://www.raywenderlich.com/12444/objectively-speaking-a-crash-course-in-objective-c)

[iOS For High School Students: Getting Started](http://www.raywenderlich.com/5600/ios-for-high-school-students-getting-started)
