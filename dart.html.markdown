---
language: dart
filename: learndart.dart
contributors:
  - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
  - ["Vince Ramces Oliveros", "https://github.com/ram231"]
---

**Dart** is a single threaded, general purpose programming language.
It borrows a lot from other mainstream languages.
It supports Streams, Futures(known as Promises in JavaScript), Generics, First-class functions(closures) and static type checking.
Dart can run in any platform such as Web, CLI, Desktop, Mobile and IoT devices.

Dart's most controversial feature is its ~~Optional Typing~~ Static Type safety and [Sound Type checks](https://dart.dev/guides/language/sound-dart).

```dart
import "dart:collection";
import "dart:math" as math;

/// Welcome to Learn Dart in 15 minutes. http://dart.dev/
/// This is an executable tutorial. You can run it with Dart or on
/// the Try Dart! site if you copy/paste it there. http://dartpad.dev/
/// You can also run Flutter in DartPad by click the `< > New Pad ` and choose Flutter


/// In Dart, Everything is an Object.
/// Every declaration of an object is an instance of Null and
/// Null is also an object.


/// 3 Types of comments in dart
// Single line comment
/**
* Multi-line comment
* Can comment more than 2 lines
*/
/// Code doc comment
/// It uses markdown syntax to generate code docs when making an API.
/// Code doc comment is the recommended choice when documenting your APIs, classes and methods.

/// 4 types of variable declaration.
/// Constants are variables that are immutable cannot be change or altered.
/// `const` in dart should practice SCREAMING_SNAKE_CASE name declaration.
const CONSTANT_VALUE = "I CANNOT CHANGE";
CONSTANT_VALUE = "DID I?"; //Error
/// Final is another variable declaration that cannot be change once it has been instantiated. Commonly used in classes and functions
/// `final` can be declared in pascalCase.
final finalValue = "value cannot be changed once instantiated";
finalValue = "Seems not"; //Error

/// `var` is another variable declaration that is mutable and can change its value. Dart will infer types and will not change its data type
var mutableValue = "Variable string";
mutableValue = "this is valid";
mutableValue = false; // Error.

/// `dynamic` is another variable declaration in which the type is not evaluated by the dart static type checking.
/// It can change its value and data type.
/// Some dartisans uses dynamic cautiously as it cannot keep track of its data type. so use it at your own risk
dynamic dynamicValue = "I'm a string";
dynamicValue = false; // false


/// Functions can be declared in a global space
/// Function declaration and method declaration look the same. Function
/// declarations can be nested. The declaration takes the form of
/// name() {} or name() => singleLineExpression;
/// The fat arrow function declaration can be an implicit or
/// explicit return for the result of the expression.
/// Dart will execute a function called `main()` anywhere in the dart project.
///
example1() {
  nested1() {
    nested2() => print("Example1 nested 1 nested 2");
    nested2();
  }

  nested1();
}

/// Anonymous functions don't include a name
example2() {
  //// Explicit return type.
  nested1(void Function() fn) {
    fn();
  }
  nested1(() => print("Example2 nested 1"));
}

/// When a function parameter is declared, the declaration can include the
/// number of parameters the function takes by explicitly specifying the names of the
/// parameters it takes.
example3() {
  planA(fn(String informSomething)) {
    fn("Example3 plan A");
  }
  planB(fn) {
    // Or don't declare number of parameters.
    fn("Example3 plan B");
  }

  planA((s) => print(s));
  planB((s) => print(s));
}

/// Functions have closure access to outer variables.
/// Dart will infer types when the variable has a value of something.
/// In this example dart knows that this variable is a String.
var example4Something = "Example4 nested 1";
example4() {
  nested1(fn(informSomething)) {
    fn(example4Something);
  }

  nested1((s) => print(s));
}

/// Class declaration with a sayIt method, which also has closure access
/// to the outer variable as though it were a function as seen before.
var example5method = "Example5 sayIt";

class Example5Class {
  sayIt() {
    print(example5method);
  }
}

example5() {
  /// Create an anonymous instance of the Example5Class and call the sayIt
  /// method on it.
  /// the `new` keyword is optional in Dart.
  new Example5Class().sayIt();
}

/// Class declaration takes the form of class name { [classBody] }.
/// Where classBody can include instance methods and variables, but also
/// class methods and variables.
class Example6Class {
  var instanceVariable = "Example6 instance variable";
  sayIt() {
    print(instanceVariable);
  }
}

example6() {
   Example6Class().sayIt();
}

/// Class methods and variables are declared with "static" terms.
class Example7Class {
  static var classVariable = "Example7 class variable";
  static sayItFromClass() {
    print(classVariable);
  }

  sayItFromInstance() {
    print(classVariable);
  }
}

example7() {
  Example7Class.sayItFromClass();
  new Example7Class().sayItFromInstance();
}

/// Dart supports Generics.
/// Generics refers to the technique of writing the code for a class
/// without specifying the data type(s) that the class works on.
/// Source: https://stackoverflow.com/questions/4560890/what-are-generics-in-c

/// Type `T` refers to any type that has been instantiated
/// you can call whatever you want
/// Programmers uses the convention in the following
/// T - Type(used for class and primitype types)
/// E - Element(used for List, Set, or Iterable)
/// K,V - Key Value(used for Map)
class GenericExample<T>{
  void printType(){
    print("$T")
  }
  // methods can also have generics
  genericMethod<M>(){
    print("class:$T, method: $M");
  }
}


/// List are similar to arrays but list is a child of Iterable<E>
/// Therefore Maps, List, LinkedList are all child of Iterable<E> to be able to loop using the keyword `for`
/// Important things to remember:
/// () - Iterable<E>
/// [] - List<E>
/// {} - Map<K,V>


/// List are great, but there's a restriction for what List can be
/// outside of function/method bodies. List on the outer scope of class
/// or outside of class have to be constant. Strings and numbers are constant
/// by default. But arrays and maps are not. They can be made constant by
/// declaring them "const". Kind of similar to Javascript's Object.freeze()
const example8List = ["Example8 const array"];
const  example8Map = {"someKey": "Example8 const map"};
/// Declare List or Maps as Objects.
 List<String> explicitList = new List<String>();
 Map<String,dynamic> explicitMaps = new Map<String,dynamic>();

 explicitList.add("SomeArray");
example8() {
  print(example8Map["someKey"]);
  print(explicitList[0]);
}

/// Assigning a list from one variable to another will not be the same result.
/// Because dart is pass-reference-by-value.
/// So when you assign an existing list to a new variable.
/// Instead of List, it becomes an Iterable
var iterableExplicitList = explicitList;
print(iterableExplicitList) // ("SomeArray"); "[]" becomes "()"
var newExplicitLists = explicitList.toList() // Converts Iterable<E> to List<E>

/// Loops in Dart take the form of standard for () {} or while () {} loops,
/// slightly more modern for (.. in ..) {}, or functional callbacks with many
/// supported features, starting with forEach,map and where.
var example9Array = const ["a", "b"];
example9() {
  for (int i = 0; i < example9Array.length; i++) {
    print("Example9 for loop '${example9Array[i]}'");
  }
  var i = 0;
  while (i < example9Array.length) {
    print("Example9 while loop '${example9Array[i]}'");
    i++;
  }
  for (final e in example9Array) {
    print("Example9 for-in loop '${e}'");
  }

  example9Array.forEach((e) => print("Example9 forEach loop '${e}'"));

}

/// To loop over the characters of a string or to extract a substring.
var example10String = "ab";
example10() {
  for (var i = 0; i < example10String.length; i++) {
    print("Example10 String character loop '${example10String[i]}'");
  }
  for (var i = 0; i < example10String.length; i++) {
    print("Example10 substring loop '${example10String.substring(i, i + 1)}'");
  }
}

/// `int`, `double`  and `num` are the three supported number formats.
/// `num` can be either `int` or `double`.
/// `int` and `double` are children of type `num`
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  num myNumDouble = 2.2;
  num myNumInt = 2;
  int myInt = 1;
  double myDouble = 0; // Dart will add decimal prefix, becomes 0.0;
  myNumDouble = myInt; // valid
  myNumDouble = myDouble; //valid
  myNumDouble = myNumInt; //valid

  myNumInt = myInt; // valid
  myNumInt = myDouble; // valid
  myNumInt = myNumDouble; // valid

  myInt = myNumDouble; //Error
  myInt = myDouble; //Error
  myInt = myNumInt; //valid

  myDouble = myInt; //error
  myDouble = myNumInt; //valid
  myDouble = myNumDouble; //valid

  print("Example11 int ${i}");
  print("Example11 double ${d}");

}

/// DateTime provides date/time arithmetic.
example12() {
  var now = new DateTime.now();
  print("Example12 now '${now}'");
  now = now.add(new Duration(days: 1));
  print("Example12 tomorrow '${now}'");
}

/// Regular expressions are supported.
example13() {
  var s1 = "some string", s2 = "some", re = new RegExp("^s.+?g\$");
  match(s) {
    if (re.hasMatch(s)) {
      print("Example13 regexp matches '${s}'");
    } else {
      print("Example13 regexp doesn't match '${s}'");
    }
  }

  match(s1);
  match(s2);
}

/// Boolean expressions support implicit conversions and dynamic type
example14() {
  var a = true;
  if (a) {
    print("true, a is $a");
  }
  a = null;
  if (a) {
    print("true, a is $a");
  } else {
    print("false, a is $a"); /// runs here
  }

  /// dynamic typed null can be convert to bool
  var b;/// b is dynamic type
  b = "abc";
  try {
    if (b) {
      print("true, b is $b");
    } else {
      print("false, b is $b");
    }
  } catch (e) {
    print("error, b is $b"); /// this could be run but got error
  }
  b = null;
  if (b) {
    print("true, b is $b");
  } else {
    print("false, b is $b"); /// runs here
  }

  /// statically typed null can not be convert to bool
  var c = "abc";
  c = null;
  /// complie failed
  /// if (c) {
  ///   print("true, c is $c");
  /// } else {
  ///   print("false, c is $c");
  /// }
}

/// try/catch/finally and throw are used for exception handling.
/// throw takes any object as parameter;
example15() {
  try {
    try {
      throw "Some unexpected error.";
    } catch (e) {
      print("Example15 an exception: '${e}'");
      throw e; /// Re-throw
    }
  } catch (e) {
    print("Example15 catch exception being re-thrown: '${e}'");
  } finally {
    print("Example15 Still run finally");
  }
}

/// To be efficient when creating a long string dynamically, use
/// StringBuffer. Or you could join a string array.
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) {
    sb.write(e);
  }
  print("Example16 dynamic string created with "
      "StringBuffer '${sb.toString()}'");
  print("Example16 join string array '${a.join()}'");
}

/// Strings can be concatenated by just having string List next to
/// one another with no further operator needed.

example17() {
  print("Example17 "
      "concatenate "
      "strings "
      "just like that");
}

/// Strings have single-quote or double-quote for delimiters with no
/// actual difference between the two. The given flexibility can be good
/// to avoid the need to escape content that matches the delimiter being
/// used. For example, double-quotes of HTML attributes if the string
/// contains HTML content.
example18() {
  print('Example18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

/// Strings with triple single-quotes or triple double-quotes span
/// multiple lines and include line delimiters.
example19() {
  print('''Example19 <a href="etc">
Example19 Don't can't I'm Etc
Example19 </a>''');
}

/// Strings have the nice interpolation feature with the $ character.
/// With $ { [expression] }, the return of the expression is interpolated.
/// $ followed by a variable name interpolates the content of that variable.
/// $ can be escaped like so \$ to just add it to the string instead.
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 \$ interpolation ${s1} or $s2 works.");
}

/// Optional types allow for the annotation of APIs and come to the aid of
/// IDEs so the IDEs can better refactor, auto-complete and check for
/// errors. So far we haven't declared any types and the programs have
/// worked just fine. In fact, types are disregarded during runtime.
/// Types can even be wrong and the program will still be given the
/// benefit of the doubt and be run as though the types didn't matter.
/// There's a runtime parameter that checks for type errors which is
/// the checked mode, which is said to be useful during development time,
/// but which is also slower because of the extra checking and is thus
/// avoided during deployment runtime.
class Example21 {
  List<String> _names;
  Example21() {
    _names = ["a", "b"];
  }
  List<String> get names => _names;
  set names(List<String> list) {
    _names = list;
  }

  int get length => _names.length;
  void add(String name) {
    _names.add(name);
  }
}

void example21() {
  Example21 o = new Example21();
  o.add("c");
  print("Example21 names '${o.names}' and length '${o.length}'");
  o.names = ["d", "e"];
  print("Example21 names '${o.names}' and length '${o.length}'");
}

/// Class inheritance takes the form of class name extends AnotherClassName {}.
class Example22A {
  var _name = "Some Name!";
  get name => _name;
}

class Example22B extends Example22A {}

example22() {
  var o = new Example22B();
  print("Example22 class inheritance '${o.name}'");
}

/// Class mixin is also available, and takes the form of
/// class name extends SomeClass with AnotherClassName {}.
/// It's necessary to extend some class to be able to mixin another one.
/// The template class of mixin cannot at the moment have a constructor.
/// Mixin is mostly used to share methods with distant classes, so the
/// single inheritance doesn't get in the way of reusable code.
/// Mixins follow the "with" statement during the class declaration.
class Example23A {}

class Example23Utils {
  addTwo(n1, n2) {
    return n1 + n2;
  }
}

class Example23B extends Example23A with Example23Utils {
  addThree(n1, n2, n3) {
    return addTwo(n1, n2) + n3;
  }
}

example23() {
  var o = new Example23B(), r1 = o.addThree(1, 2, 3), r2 = o.addTwo(1, 2);
  print("Example23 addThree(1, 2, 3) results in '${r1}'");
  print("Example23 addTwo(1, 2) results in '${r2}'");
}

/// The Class constructor method uses the same name of the class and
/// takes the form of SomeClass() : super() {}, where the ": super()"
/// part is optional and it's used to delegate constant parameters to the
/// super-parent's constructor.
class Example24A {
  var _value;
  Example24A({value: "someValue"}) {
    _value = value;
  }
  get value => _value;
}

class Example24B extends Example24A {
  Example24B({value: "someOtherValue"}) : super(value: value);
}

example24() {
  var o1 = new Example24B(), o2 = new Example24B(value: "evenMore");
  print("Example24 calling super during constructor '${o1.value}'");
  print("Example24 calling super during constructor '${o2.value}'");
}

/// There's a shortcut to set constructor parameters in case of simpler classes.
/// Just use the this.parameterName prefix and it will set the parameter on
/// an instance variable of same name.
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}

example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("Example25 shortcut for constructor '${o.value}' and "
      "'${o.anotherValue}'");
}

/// Named parameters are available when declared between {}.
/// Parameter order can be optional when declared between {}.
/// Parameters can be made optional when declared between [].
example26() {
  var _name, _surname, _email;
  setConfig1({name, surname}) {
    _name = name;
    _surname = surname;
  }

  setConfig2(name, [surname, email]) {
    _name = name;
    _surname = surname;
    _email = email;
  }

  setConfig1(surname: "Doe", name: "John");
  print("Example26 name '${_name}', surname '${_surname}', "
      "email '${_email}'");
  setConfig2("Mary", "Jane");
  print("Example26 name '${_name}', surname '${_surname}', "
      "email '${_email}'");
}

/// Variables declared with final can only be set once.
/// In case of classes, final instance variables can be set via constant
/// constructor parameter.
class Example27 {
  final color1, color2;
  /// A little flexibility to set final instance variables with syntax
  /// that follows the :
  Example27({this.color1, color2}) : color2 = color2;
}

example27() {
  final color = "orange", o = new Example27(color1: "lilac", color2: "white");
  print("Example27 color is '${color}'");
  print("Example27 color is '${o.color1}' and '${o.color2}'");
}

/// To import a library, use import "libraryPath" or if it's a core library,
/// import "dart:libraryName". There's also the "pub" package management with
/// its own convention of import "package:packageName".
/// See import "dart:collection"; at the top. Imports must come before
/// other code declarations. IterableBase comes from dart:collection.
class Example28 extends IterableBase {
  var names;
  Example28() {
    names = ["a", "b"];
  }
  get iterator => names.iterator;
}

example28() {
  var o = new Example28();
  o.forEach((name) => print("Example28 '${name}'"));
}

/// For control flow we have:
/// * standard switch with must break statements
/// * if-else if-else and ternary ..?..:.. operator
/// * closures and anonymous functions
/// * break, continue and return statements
example29() {
  var v = true ? 30 : 60;
  switch (v) {
    case 30:
      print("Example29 switch statement");
      break;
  }
  if (v < 30) {
  } else if (v > 30) {
  } else {
    print("Example29 if-else statement");
  }
  callItForMe(fn()) {
    return fn();
  }

  rand() {
    v = new DM.Random().nextInt(50);
    return v;
  }

  while (true) {
    print("Example29 callItForMe(rand) '${callItForMe(rand)}'");
    if (v != 30) {
      break;
    } else {
      continue;
    }
    /// Never gets here.
  }
}

/// Parse int, convert double to int, or just keep int when dividing numbers
/// by using the ~/ operation. Let's play a guess game too.
example30() {
  var gn,
      tooHigh = false,
      n,
      n2 = (2.0).toInt(),
      top = int.parse("123") ~/ n2,
      bottom = 0;
  top = top ~/ 6;
  gn = new DM.Random().nextInt(top + 1); /// +1 because nextInt top is exclusive
  print("Example30 Guess a number between 0 and ${top}");
  guessNumber(i) {
    if (n == gn) {
      print("Example30 Guessed right! The number is ${gn}");
    } else {
      tooHigh = n > gn;
      print("Example30 Number ${n} is too "
          "${tooHigh ? 'high' : 'low'}. Try again");
    }
    return n == gn;
  }

  n = (top - bottom) ~/ 2;
  while (!guessNumber(n)) {
    if (tooHigh) {
      top = n - 1;
    } else {
      bottom = n + 1;
    }
    n = bottom + ((top - bottom) ~/ 2);
  }
}

/// Optional Positional Parameter:
/// parameter will be disclosed with square bracket [ ] & square bracketed parameter are optional.
example31() {
    findVolume31(int length, int breath, [int height]) {
      print('length = $length, breath = $breath, height = $height');
    }

    findVolume31(10,20,30); //valid
    findVolume31(10,20); //also valid
}

/// Optional Named Parameter:
/// parameter will be disclosed with curly bracket { }
/// curly bracketed parameter are optional.
/// have to use parameter name to assign a value which separated with colan :
/// in curly bracketed parameter order does not matter
/// these type parameter help us to avoid confusion while passing value for a function which has many parameter.
example32() {
    findVolume32(int length, int breath, {int height}) {
    print('length = $length, breath = $breath, height = $height');
    }

    findVolume32(10,20,height:30);//valid & we can see the parameter name is mentioned here.
    findVolume32(10,20);//also valid
}

/// Optional Default Parameter:
/// same like optional named parameter in addition we can assign default value for this parameter.
/// which means no value is passed this default value will be taken.
example33() {
    findVolume33(int length, int breath, {int height=10}) {
     print('length = $length, breath = $breath, height = $height');
    }

    findVolume33(10,20,height:30);//valid
    findVolume33(10,20);//valid
}

/// Dart has also added feature such as Null aware operators
var isBool = true;
var hasString = isBool ?? "default String";

/// Programs have only one entry point in the main function.
/// Nothing is expected to be executed on the outer scope before a program
/// starts running with what's in its main function.
/// This helps with faster loading and even lazily loading of just what
/// the program needs to startup with.
main() {
  print("Learn Dart in 15 minutes!");
  [
    example1, example2, example3, example4, example5,
    example6, example7, example8, example9, example10,
    example11, example12, example13, example14, example15,
    example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25,
    example26, example27, example28, example29,
    example30 // Adding this comment stops the dart formatter from putting all items on a new line
  ].forEach((ef) => ef());
}
```

## Further Reading

Dart has a comprehensive web-site. It covers API reference, tutorials, articles and more, including a
useful DartPad (a cloud-based Dart coding playground).
[https://dart.dev/](https://dart.dev)
[https://dartpad.dev/](https://dartpad.dev)
