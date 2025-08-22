---
name: Dart
filename: learndart.dart
contributors:
  - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
  - ["Vince Ramces Oliveros", "https://github.com/ram231"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

**Dart**는 단일 스레드, 범용 프로그래밍 언어입니다.
다른 주류 언어에서 많은 것을 차용했습니다.
스트림, 퓨처(JavaScript에서는 Promise로 알려짐), 제네릭, 일급 함수(클로저) 및 정적 타입 검사를 지원합니다.
Dart는 웹, CLI, 데스크톱, 모바일 및 IoT 장치와 같은 모든 플랫폼에서 실행될 수 있습니다.

Dart의 가장 논란이 많은 기능은 ~~선택적 타이핑~~ 정적 타입 안전성 및 [사운드 타입 검사](https://dart.dev/guides/language/sound-dart)입니다.

```dart
import "dart:collection";
import "dart:math" as math;

/// Y분 만에 Dart 배우기에 오신 것을 환영합니다. http://dart.dev/
/// 이것은 실행 가능한 튜토리얼입니다. Dart로 실행하거나
/// DartPad 사이트에 복사/붙여넣기하여 실행할 수 있습니다. http://dartpad.dev/
/// 또한 < > New Pad를 클릭하고 Flutter를 선택하여 DartPad에서 Flutter를 실행할 수 있습니다.


/// Dart에서는 모든 것이 객체입니다.
/// 모든 객체 선언은 Null의 인스턴스이며
/// Null도 객체입니다.


/// Dart의 3가지 주석 유형
// 한 줄 주석
/**
* 여러 줄 주석
* 여러 줄을 주석 처리할 수 있습니다.
*/
/// 코드 문서 주석
/// API를 만들 때 코드 문서를 생성하기 위해 마크다운 구문을 사용합니다.
/// 코드 문서 주석은 API, 클래스 및 메서드를 문서화할 때 권장되는 선택입니다.

/// 4가지 변수 선언 유형.
/// 상수는 변경할 수 없는 변수입니다.
/// Dart에서 `const`는 SCREAMING_SNAKE_CASE 이름 선언을 사용해야 합니다.
const CONSTANT_VALUE = "I CANNOT CHANGE";
CONSTANT_VALUE = "DID I?"; // 오류
/// Final은 한 번 인스턴스화되면 변경할 수 없는 또 다른 변수 선언입니다. 클래스 및 함수에서 일반적으로 사용됩니다.
/// `final`은 pascalCase로 선언할 수 있습니다.
final finalValue = "value cannot be changed once instantiated";
finalValue = "Seems not"; // 오류

/// `var`는 변경 가능하며 값을 변경할 수 있는 또 다른 변수 선언입니다. Dart는 유형을 추론하며 데이터 유형을 변경하지 않습니다.
var mutableValue = "Variable string";
mutableValue = "this is valid";
mutableValue = false; // 오류.

/// `dynamic`은 Dart 정적 타입 검사에서 유형이 평가되지 않는 또 다른 변수 선언입니다.
/// 값과 데이터 유형을 변경할 수 있습니다.
/// 일부 Dart 개발자는 데이터 유형을 추적할 수 없으므로 dynamic을 신중하게 사용합니다. 따라서 자신의 위험을 감수하고 사용하십시오.
dynamic dynamicValue = "I'm a string";
dynamicValue = false; // false


/// 함수는 전역 공간에 선언될 수 있습니다.
/// 함수 선언과 메서드 선언은 동일하게 보입니다. 함수
/// 선언은 name() {} 또는 name() => singleLineExpression; 형식입니다.
/// 뚱뚱한 화살표 함수 선언은 표현식 결과에 대한 암시적 또는
/// 명시적 반환일 수 있습니다.
/// Dart는 Dart 프로젝트의 어디에서든 `main()`이라는 함수를 실행합니다.
///
example1() {
  nested1() {
    nested2() => print("Example1 nested 1 nested 2");
    nested2();
  }

  nested1();
}

/// 익명 함수에는 이름이 없습니다.
example2() {
  //// 명시적 반환 유형.
  nested1(void Function() fn) {
    fn();
  }
  nested1(() => print("Example2 nested 1"));
}

/// 함수 매개변수가 선언될 때, 선언은 매개변수가 취하는 이름을 명시적으로 지정하여
/// 함수가 취하는 매개변수 수를 포함할 수 있습니다.
example3() {
  planA(fn(String informSomething)) {
    fn("Example3 plan A");
  }
  planB(fn) {
    // 또는 매개변수 수를 선언하지 않습니다.
    fn("Example3 plan B");
  }

  planA((s) => print(s));
  planB((s) => print(s));
}

/// 함수는 외부 변수에 대한 클로저 액세스를 가집니다.
/// Dart는 변수에 값이 있는 경우 유형을 추론합니다.
/// 이 예제에서 Dart는 이 변수가 문자열임을 알고 있습니다.
var example4Something = "Example4 nested 1";
example4() {
  nested1(fn(informSomething)) {
    fn(example4Something);
  }

  nested1((s) => print(s));
}

/// sayIt 메서드가 있는 클래스 선언, 이전과 마찬가지로 함수처럼 외부 변수에 대한 클로저 액세스도 가집니다.
var example5method = "Example5 sayIt";

class Example5Class {
  sayIt() {
    print(example5method);
  }
}

example5() {
  /// Example5Class의 익명 인스턴스를 생성하고 sayIt
  /// 메서드를 호출합니다.
  /// Dart에서 `new` 키워드는 선택 사항입니다.
  new Example5Class().sayIt();
}

/// 클래스 선언은 class name { [classBody] } 형식입니다.
/// 여기서 classBody는 인스턴스 메서드 및 변수뿐만 아니라
/// 클래스 메서드 및 변수도 포함할 수 있습니다.
class Example6Class {
  var instanceVariable = "Example6 instance variable";
  sayIt() {
    print(instanceVariable);
  }
}

example6() {
   Example6Class().sayIt();
}

/// 클래스 메서드 및 변수는 "static" 용어로 선언됩니다.
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

/// Dart는 제네릭을 지원합니다.
/// 제네릭은 클래스가 작동하는 데이터 유형을 지정하지 않고
/// 클래스에 대한 코드를 작성하는 기술을 나타냅니다.
/// 출처: https://stackoverflow.com/questions/4560890/what-are-generics-in-c

/// 유형 `T`는 인스턴스화된 모든 유형을 나타냅니다.
/// 원하는 대로 호출할 수 있습니다.
/// 프로그래머는 다음 규칙을 사용합니다.
/// T - 유형(클래스 및 기본 유형에 사용)
/// E - 요소(목록, 집합 또는 반복 가능에 사용)
/// K,V - 키 값(맵에 사용)
class GenericExample<T>{
  void printType(){
    print("$T");
  }
  // 메서드도 제네릭을 가질 수 있습니다.
  genericMethod<M>(){
    print("class:$T, method: $M");
  }
}


/// 목록은 배열과 유사하지만 목록은 Iterable<E>의 자식입니다.
/// 따라서 맵, 목록, 연결 목록은 모두 `for` 키워드를 사용하여 반복할 수 있도록 Iterable<E>의 자식입니다.
/// 기억해야 할 중요한 사항:
/// () - Iterable<E>
/// [] - List<E>
/// {} - Map<K,V>


/// 목록은 훌륭하지만, 함수/메서드 본문 외부에서 목록이 될 수 있는 것에 대한 제한이 있습니다.
/// 클래스 외부 또는 클래스 외부의 목록은 상수여야 합니다. 문자열과 숫자는 기본적으로 상수입니다.
/// 그러나 배열과 맵은 그렇지 않습니다. "const"로 선언하여 상수로 만들 수 있습니다.
/// JavaScript의 Object.freeze()와 다소 유사합니다.
const example8List = ["Example8 const array"];
const  example8Map = {"someKey": "Example8 const map"};
/// 목록 또는 맵을 객체로 선언합니다.
 List<String> explicitList = new List<String>.empty();
 Map<String,dynamic> explicitMaps = new Map<String,dynamic>();

example8() {
  explicitList.add("SomeArray");
  print(example8Map["someKey"]);
  print(explicitList[0]);

  /// 한 변수에서 다른 변수로 목록을 할당하면 동일한 결과가 나오지 않습니다.
  /// Dart는 참조에 의한 값 전달이기 때문입니다.
  /// 따라서 기존 목록을 새 변수에 할당하면
  /// 목록 대신 반복 가능 객체가 됩니다.
  var iterableExplicitList = explicitList;
  print(iterableExplicitList); // ("SomeArray"); "[]"는 "()"가 됩니다.
  var newExplicitLists = explicitList.toList(); // Iterable<E>를 List<E>로 변환합니다.
}

/// Dart의 루프는 표준 for () {} 또는 while () {} 루프, 약간 더 현대적인 for (.. in ..) {} 또는 forEach, map 및 where로 시작하는 많은 지원 기능을 가진 함수형 콜백 형식입니다.
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

/// 문자열의 문자를 반복하거나 부분 문자열을 추출합니다.
var example10String = "ab";
example10() {
  for (var i = 0; i < example10String.length; i++) {
    print("Example10 String character loop '${example10String[i]}'");
  }
  for (var i = 0; i < example10String.length; i++) {
    print("Example10 substring loop '${example10String.substring(i, i + 1)}'");
  }
}

/// `int`, `double` 및 `num`은 지원되는 세 가지 숫자 형식입니다.
/// `num`은 `int` 또는 `double`일 수 있습니다.
/// `int` 및 `double`은 `num` 유형의 자식입니다.
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  final num myFinalNumDouble = 2.2;
  final num myFinalNumInt = 2;
  final int myFinalInt = 1;
  final double myFinalDouble = 0.1;
  num myNumDouble = 2.2;
  num myNumInt = 2;
  int myInt = 1;
  double myDouble = 0; // Dart는 소수점 접두사를 추가하여 0.0이 됩니다.
  myNumDouble = myFinalInt; // 유효
  myNumDouble = myFinalDouble; // 유효
  myNumDouble = myFinalNumInt; // 유효

  myInt = myNumDouble; // 오류
  myInt = myFinalDouble; // 오류
  myInt = myFinalNumInt; // 오류 (Dart 2.9에서 암시적 다운캐스트 제거됨)
  myInt = myFinalNumInt as int; // 유효

  myDouble = myFinalInt; // 오류
  myDouble = myFinalNumInt; // 오류
  myDouble = myFinalNumDouble; // 오류 (Dart 2.9에서 암시적 다운캐스트 제거됨)
  myDouble = myFinalNumDouble as double; // 유효

  print("Example11 int ${i}");
  print("Example11 double ${d}");

}

/// DateTime은 날짜/시간 산술을 제공합니다.
example12() {
  var now = new DateTime.now();
  print("Example12 now '${now}'");
  now = now.add(new Duration(days: 1));
  print("Example12 tomorrow '${now}'");
}

/// 정규 표현식이 지원됩니다.
example13() {
  var s1 = "some string", s2 = "some", re = new RegExp("^s.+?g");
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

/// 부울 표현식은 암시적 변환 및 동적 유형을 지원합니다.
example14() {
  var a = true;
  if (a) {
    print("true, a is $a");
  }
  a = false;
  if (a) {
    print("true, a is $a");
  } else {
    print("false, a is $a"); /// 여기서 실행됩니다.
  }

  /// 동적 유형의 null은 부울로 변환할 수 없습니다.
  var b; /// b는 동적 유형입니다.
  b = "abc";
  try {
    if (b) {
      print("true, b is $b");
    } else {
      print("false, b is $b");
    }
  } catch (e) {
    print("error, b is $b"); /// 이것은 실행될 수 있지만 오류가 발생했습니다.
  }
  b = null;
  if (b) { /// 어설션 실패: 부울 표현식은 null이 아니어야 합니다.
    print("true, b is $b");
  } else {
    print("false, b is $b");
  }

  /// 정적으로 유형이 지정된 null은 부울로 변환할 수 없습니다.
  var c = "abc";
  c = null;
  /// 컴파일 실패
  /// if (c) {
  ///   print("true, c is $c");
  /// } else {
  ///   print("false, c is $c");
  /// }
}

/// try/catch/finally 및 throw는 예외 처리에 사용됩니다.
/// throw는 모든 객체를 매개변수로 받습니다.
example15() {
  try {
    try {
      throw "Some unexpected error.";
    } catch (e) {
      print("Example15 an exception: '${e}'");
      throw e; /// 다시 던지기
    }
  } catch (e) {
    print("Example15 catch exception being re-thrown: '${e}'");
  } finally {
    print("Example15 Still run finally");
  }
}

/// 동적으로 긴 문자열을 만들 때 효율적으로 하려면
/// StringBuffer를 사용하십시오. 또는 문자열 배열을 결합할 수 있습니다.
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) {
    sb.write(e);
  }
  print("Example16 dynamic string created with "
      "StringBuffer '${sb.toString()}'");
  print("Example16 join string array '${a.join()}'");
}

/// 문자열은 추가 연산자 없이 문자열 목록을 나란히 배치하여 연결할 수 있습니다.

example17() {
  print("Example17 "
      "concatenate "
      "strings "
      "just like that");
}

/// 문자열은 단일 따옴표 또는 이중 따옴표를 구분 기호로 사용하며 둘 사이에 실제 차이는 없습니다.
/// 주어진 유연성은 사용되는 구분 기호와 일치하는 콘텐츠를 이스케이프할 필요성을 피하는 데 유용할 수 있습니다.
/// 예를 들어, 문자열에 HTML 콘텐츠가 포함된 경우 HTML 속성의 이중 따옴표입니다.
example18() {
  print('Example18 <a href="etc">
Don\'t can\'t I\'m Etc
</a>');
}

/// 삼중 단일 따옴표 또는 삼중 이중 따옴표가 있는 문자열은
/// 여러 줄에 걸쳐 있으며 줄 구분 기호를 포함합니다.
example19() {
  print('''Example19 <a href="etc">
Example19 Don't can't I'm Etc
Example19 </a>''');
}

/// 문자열에는 $ 문자를 사용한 멋진 보간 기능이 있습니다.
/// $ { [표현식] }을 사용하면 표현식의 반환 값이 보간됩니다.
/// 변수 이름 뒤에 $를 붙이면 해당 변수의 내용이 보간됩니다.
/// $는 문자열에 추가하기 위해 \$와 같이 이스케이프할 수 있습니다.
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 $ interpolation ${s1} or $s2 works.");
}

/// 선택적 유형은 API에 주석을 달 수 있도록 하며
/// IDE가 더 잘 리팩토링하고 자동 완성하며
/// 오류를 확인할 수 있도록 도와줍니다. 지금까지는 유형을 선언하지 않았으며
/// 프로그램은 잘 작동했습니다. 사실, 유형은 런타임에 무시됩니다.
/// 유형이 잘못되어도 프로그램은 유형이 중요하지 않은 것처럼
/// 실행됩니다. 유형 오류를 확인하는 런타임 매개변수가 있습니다.
/// 이는 개발 중에 유용하다고 알려진 검사 모드이지만,
/// 추가 검사로 인해 느리므로 배포 런타임에는 피합니다.
class Example21 {
  List<String> _names = [];
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

/// 클래스 상속은 class name extends AnotherClassName {} 형식입니다.
class Example22A {
  var _name = "Some Name!";
  get name => _name;
}

class Example22B extends Example22A {}

example22() {
  var o = new Example22B();
  print("Example22 class inheritance '${o.name}'");
}

/// 클래스 믹스인은 class name extends SomeClass with AnotherClassName {} 형식으로도 사용할 수 있습니다.
/// 다른 클래스를 믹스인하려면 일부 클래스를 확장해야 합니다.
/// 믹스인의 템플릿 클래스는 현재 생성자를 가질 수 없습니다.
/// 믹스인은 주로 멀리 떨어진 클래스와 메서드를 공유하는 데 사용되므로
/// 단일 상속이 재사용 가능한 코드에 방해가 되지 않습니다.
/// 믹스인은 클래스 선언 중 "with" 문을 따릅니다.
class Example23A {}

/// Dart 3부터는 'mixin' 키워드가 필요합니다.
mixin Example23Utils {
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

/// 클래스 생성자 메서드는 클래스와 동일한 이름을 사용하며
/// SomeClass() : super() {} 형식입니다. 여기서 ": super()" 부분은 선택 사항이며
/// 상수 매개변수를 상위 생성자에 위임하는 데 사용됩니다.
class Example24A {
  var _value;
  Example24A({value = "someValue"}) {
    _value = value;
  }
  get value => _value;
}

class Example24B extends Example24A {
  Example24B({value = "someOtherValue"}) : super(value: value);
}

example24() {
  var o1 = new Example24B(), o2 = new Example24B(value: "evenMore");
  print("Example24 calling super during constructor '${o1.value}'");
  print("Example24 calling super during constructor '${o2.value}'");
}

/// 더 간단한 클래스의 경우 생성자 매개변수를 설정하는 바로 가기가 있습니다.
/// this.parameterName 접두사를 사용하기만 하면 매개변수를
/// 동일한 이름의 인스턴스 변수에 설정합니다.
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}

example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("Example25 shortcut for constructor '${o.value}' and "
      "'${o.anotherValue}'");
}

/// 명명된 매개변수는 {} 사이에 선언될 때 사용할 수 있습니다.
/// 매개변수 순서는 {} 사이에 선언될 때 선택 사항일 수 있습니다.
/// 매개변수는 [] 사이에 선언될 때 선택 사항일 수 있습니다.
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

/// final로 선언된 변수는 한 번만 설정할 수 있습니다.
/// 클래스의 경우 final 인스턴스 변수는 상수
/// 생성자 매개변수를 통해 설정할 수 있습니다.
class Example27 {
  final color1, color2;
  /// final 인스턴스 변수를 설정하기 위한 약간의 유연성
  /// 콜론 뒤에 오는 구문:
  Example27({this.color1, color2}) : color2 = color2;
}

example27() {
  final color = "orange", o = new Example27(color1: "lilac", color2: "white");
  print("Example27 color is '${color}'");
  print("Example27 color is '${o.color1}' and '${o.color2}'");
}

/// 라이브러리를 가져오려면 import "libraryPath"를 사용하거나 핵심 라이브러리인 경우
/// import "dart:libraryName"을 사용하십시오. "pub" 패키지 관리도 있습니다.
/// import "package:packageName" 규칙을 따릅니다.
/// 상단에 import "dart:collection";를 참조하십시오. import는 다른 코드 선언보다 먼저 와야 합니다.
/// IterableBase는 dart:collection에서 가져옵니다.
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

/// 제어 흐름의 경우 다음이 있습니다:
/// * break 문이 있는 표준 switch
/// * if-else if-else 및 삼항 ..?..:.. 연산자
/// * 클로저 및 익명 함수
/// * break, continue 및 return 문
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
    v = new math.Random().nextInt(50);
    return v;
  }

  while (true) {
    print("Example29 callItForMe(rand) '${callItForMe(rand)}'");
    if (v != 30) {
      break;
    } else {
      continue;
    }
    /// 여기에 도달하지 않습니다.
  }
}

/// int를 구문 분석하고, double을 int로 변환하거나, 숫자를 나눌 때 ~/ 연산을 사용하여 int를 유지합니다. 추측 게임도 해 봅시다.
example30() {
  var gn,
      tooHigh = false,
      n,
      n2 = (2.0).toInt(),
      top = int.parse("123") ~/ n2,
      bottom = 0;
  top = top ~/ 6;
  gn = new math.Random().nextInt(top + 1); /// +1은 nextInt top이 배타적이기 때문입니다.
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

/// 선택적 위치 매개변수:
/// 매개변수는 대괄호 [ ]로 표시되며 대괄호로 묶인 매개변수는 선택 사항입니다.
example31() {
    findVolume31(int length, int breath, [int? height]) {
      print('length = $length, breath = $breath, height = $height');
    }

    findVolume31(10,20,30); // 유효
    findVolume31(10,20); // 또한 유효
}

/// 선택적 명명된 매개변수:
/// 매개변수는 중괄호 { }로 표시됩니다.
/// 중괄호로 묶인 매개변수는 선택 사항입니다.
/// 콜론 :으로 구분된 값을 할당하려면 매개변수 이름을 사용해야 합니다.
/// 중괄호로 묶인 매개변수 순서는 중요하지 않습니다.
/// 이러한 유형의 매개변수는 많은 매개변수를 가진 함수에 값을 전달할 때 혼동을 피하는 데 도움이 됩니다.
example32() {
    findVolume32(int length, int breath, {int? height}) {
    print('length = $length, breath = $breath, height = $height');
    }

    findVolume32(10,20,height:30);// 유효 & 매개변수 이름이 여기에 언급되어 있음을 알 수 있습니다.
    findVolume32(10,20);// 또한 유효
}

/// 선택적 기본 매개변수:
/// 선택적 명명된 매개변수와 동일하지만 이 매개변수에 기본값을 할당할 수 있습니다.
/// 즉, 값이 전달되지 않으면 이 기본값이 사용됩니다.
example33() {
    findVolume33(int length, int breath, {int height=10}) {
     print('length = $length, breath = $breath, height = $height');
    }

    findVolume33(10,20,height:30);// 유효
    findVolume33(10,20);// 유효
}

/// Dart는 또한 Null 인식 연산자와 같은 기능을 추가했습니다.
var isBool = true;
var hasString = isBool ?? "default String";

/// 프로그램은 main 함수에 하나의 진입점만 가집니다.
/// 프로그램이 main 함수에서 실행되기 전에 외부 범위에서 아무것도 실행되지 않습니다.
/// 이는 더 빠른 로딩과 프로그램이 시작하는 데 필요한 것만 지연 로딩하는 데 도움이 됩니다.
main() {
  print("Learn Dart in 15 minutes!");
  [
    example1, example2, example3, example4, example5,
    example6, example7, example8, example9, example10,
    example11, example12, example13, example14, example15,
    example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25,
    example26, example27, example28, example29,
    example30 // 이 주석을 추가하면 Dart 포맷터가 모든 항목을 새 줄에 넣는 것을 방지합니다.
  ].forEach((ef) => ef());
}
```

## 더 읽을거리

Dart는 포괄적인 웹사이트를 가지고 있습니다. API 참조, 튜토리얼, 기사 등을 포함하며,
유용한 DartPad(클라우드 기반 Dart 코딩 플레이그라운드)도 있습니다.
[https://dart.dev/](https://dart.dev)
[https://dartpad.dev/](https://dartpad.dev)