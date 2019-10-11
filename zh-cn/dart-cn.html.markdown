---
language: dart
lang: zh-cn
filename: learndart-cn.dart
contributors:
    - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
translators:
    - ["Guokai Han", "https://github.com/hanguokai/"]
---

Dart 是编程语言王国的新人。
它借鉴了许多其他主流语言，并且不会偏离它的兄弟语言 JavaScript 太多。
就像 JavaScript 一样，Dart 的目标是提供良好的浏览器集成。

Dart 最有争议的特性必然是它的可选类型。

```javascript
import "dart:collection";
import "dart:math" as DM;

// 欢迎进入15分钟的 Dart 学习。 http://www.dartlang.org/
// 这是一个可实际执行的向导。你可以用 Dart 运行它
// 或者在线执行! 可以把代码复制/粘贴到这个网站。 http://try.dartlang.org/

// 函数声明和方法声明看起来一样。
// 函数声明可以嵌套。声明使用这种 name() {} 的形式，
// 或者 name() => 单行表达式; 的形式。
// 右箭头的声明形式会隐式地返回表达式的结果。
example1() {
  example1nested1() {
    example1nested2() => print("Example1 nested 1 nested 2");
    example1nested2();
  }
  example1nested1();
}

// 匿名函数没有函数名。
example2() {
  example2nested1(fn) {
    fn();
  }
  example2nested1(() => print("Example2 nested 1"));
}

// 当声明函数类型的参数的时候，声明中可以包含
// 函数参数需要的参数，指定所需的参数名即可。
example3() {
  example3nested1(fn(informSomething)) {
    fn("Example3 nested 1");
  }
  example3planB(fn) { // 或者不声明函数参数的参数
    fn("Example3 plan B");
  }
  example3nested1((s) => print(s));
  example3planB((s) => print(s));
}

// 函数有可以访问到外层变量的闭包。
var example4Something = "Example4 nested 1";
example4() {
  example4nested1(fn(informSomething)) {
    fn(example4Something);
  }
  example4nested1((s) => print(s));
}

// 下面这个包含 sayIt 方法的类声明，同样有一个可以访问外层变量的闭包，
// 就像前面的函数一样。
var example5method = "Example5 sayIt";
class Example5Class {
  sayIt() {
    print(example5method);
  }
}
example5() {
  // 创建一个 Example5Class 类的匿名实例，
  // 并调用它的 sayIt 方法。
  new Example5Class().sayIt();
}

// 类的声明使用这种形式 class name { [classBody] }.
// classBody 中可以包含实例方法和变量，
// 还可以包含类方法和变量。
class Example6Class {
  var example6InstanceVariable = "Example6 instance variable"; 
  sayIt() {
    print(example6InstanceVariable);
  }
}
example6() {
  new Example6Class().sayIt();
}

// 类方法和变量使用 static 关键词声明。
class Example7Class {
  static var example7ClassVariable = "Example7 class variable"; 
  static sayItFromClass() {
    print(example7ClassVariable);
  }
  sayItFromInstance() {
    print(example7ClassVariable);
  }
}
example7() {
  Example7Class.sayItFromClass();
  new Example7Class().sayItFromInstance();
}

// 字面量非常方便，但是对于在函数/方法的外层的字面量有一个限制，
// 类的外层或外面的字面量必需是常量。
// 字符串和数字默认是常量。
// 但是 array 和 map 不是。他们需要用 "const" 声明为常量。
var example8A = const ["Example8 const array"],
  example8M = const {"someKey": "Example8 const map"}; 
example8() {
  print(example8A[0]);
  print(example8M["someKey"]);
}

// Dart 中的循环使用标准的 for () {} 或 while () {} 的形式，
// 以及更加现代的 for (.. in ..) {} 的形式, 或者
// 以 forEach 开头并具有许多特性支持的函数回调的形式。
var example9A = const ["a", "b"];
example9() {
  for (var i = 0; i < example9A.length; i++) {
    print("Example9 for loop '${example9A[i]}'");
  }
  var i = 0;
  while (i < example9A.length) {
    print("Example9 while loop '${example9A[i]}'");
    i++;
  }
  for (var e in example9A) {
    print("Example9 for-in loop '${e}'");
  }
  example9A.forEach((e) => print("Example9 forEach loop '${e}'"));
}

// 遍历字符串中的每个字符或者提取其子串。
var example10S = "ab";
example10() {
  for (var i = 0; i < example10S.length; i++) {
    print("Example10 String character loop '${example10S[i]}'");
  }
  for (var i = 0; i < example10S.length; i++) {
    print("Example10 substring loop '${example10S.substring(i, i + 1)}'");
  }
}

// 支持两种数字格式 int 和 double 。
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  print("Example11 int ${i}");
  print("Example11 double ${d}");
}

// DateTime 提供了日期/时间的算法。
example12() {
  var now = new DateTime.now();
  print("Example12 now '${now}'");
  now = now.add(new Duration(days: 1));
  print("Example12 tomorrow '${now}'");
}

// 支持正则表达式。
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

// 布尔表达式支持隐式转换以及动态类型
example14() {
  var a = true;
  if (a) {
    print("Example14 true, a is $a");
  }
  a = null;
  if (a) {
    print("Example14 true, a is $a");
  } else {
    print("Example14 false, a is $a"); // 执行到这里
  }

  // 动态类型的null可以转换成bool型
  var b;// b是动态类型
  b = "abc";
  try {
    if (b) {
      print("Example14 true, b is $b");
    } else {
      print("Example14 false, b is $b");
    }
  } catch (e) {
    print("Example14 error, b is $b"); // 这段代码可以执行但是会报错
  }
  b = null;
  if (b) {
    print("Example14 true, b is $b");
  } else {
    print("Example14 false, b is $b"); // 这行到这里
  }

  // 静态类型的null不能转换成bool型
  var c = "abc";
  c = null;
  // 编译出错
  // if (c) {
  //   print("Example14 true, c is $c");
  // } else {
  //   print("Example14 false, c is $c");
  // }
}

// try/catch/finally 和 throw 语句用于异常处理。
// throw 语句可以使用任何对象作为参数。
example15() {
  try {
    try {
      throw "Some unexpected error.";
    } catch (e) {
      print("Example15 an exception: '${e}'");
      throw e; // Re-throw
    }
  } catch (e) {
    print("Example15 catch exception being re-thrown: '${e}'");
  } finally {
    print("Example15 Still run finally");
  }
}

// 要想有效地动态创建长字符串，
// 应该使用 StringBuffer。 或者 join 一个字符串的数组。
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) { sb.write(e); }
  print("Example16 dynamic string created with "
    "StringBuffer '${sb.toString()}'");
  print("Example16 join string array '${a.join()}'");
}

// 字符串连接只需让相邻的字符串字面量挨着，
// 不需要额外的操作符。
example17() {
  print("Example17 "
      "concatenate "
      "strings "
      "just like that");
}

// 字符串使用单引号或双引号做分隔符，二者并没有实际的差异。
// 这种灵活性可以很好地避免内容中需要转义分隔符的情况。
// 例如，字符串内容里的 HTML 属性使用了双引号。
example18() {
  print('Example18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

// 用三个单引号或三个双引号表示的字符串
// 可以跨越多行，并且包含行分隔符。
example19() {
  print('''Example19 <a href="etc"> 
Example19 Don't can't I'm Etc
Example19 </a>''');
}

// 字符串可以使用 $ 字符插入内容。
// 使用 $ { [expression] } 的形式，表达式的值会被插入到字符串中。
// $ 跟着一个变量名会插入变量的值。
// 如果要在字符串中插入 $ ，可以使用 \$ 的转义形式代替。
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 \$ interpolation ${s1} or $s2 works.");
}

// 可选类型允许作为 API 的标注，并且可以辅助 IDE，
// 这样 IDE 可以更好地提供重构、自动完成和错误检测功能。
// 目前为止我们还没有声明任何类型，并且程序运行地很好。
// 事实上，类型在运行时会被忽略。
// 类型甚至可以是错的，并且程序依然可以执行，
// 好像和类型完全无关一样。
// 有一个运行时参数可以让程序进入检查模式，它会在运行时检查类型错误。
// 这在开发时很有用，但是由于增加了额外的检查会使程序变慢，
// 因此应该避免在部署时使用。
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

// 类的继承形式是 class name extends AnotherClassName {} 。
class Example22A {
  var _name = "Some Name!";
  get name => _name;
}
class Example22B extends Example22A {}
example22() {
  var o = new Example22B();
  print("Example22 class inheritance '${o.name}'");
}

// 类也可以使用 mixin 的形式 ：
// class name extends SomeClass with AnotherClassName {}.
// 必需继承某个类才能 mixin 另一个类。
// 当前 mixin 的模板类不能有构造函数。
// Mixin 主要是用来和辅助的类共享方法的，
// 这样单一继承就不会影响代码复用。
// Mixin 声明在类定义的 "with" 关键词后面。
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
  var o = new Example23B(), r1 = o.addThree(1, 2, 3),
    r2 = o.addTwo(1, 2);
  print("Example23 addThree(1, 2, 3) results in '${r1}'");
  print("Example23 addTwo(1, 2) results in '${r2}'");
}

// 类的构造函数名和类名相同，形式为
// SomeClass() : super() {},  其中 ": super()" 的部分是可选的，
// 它用来传递参数给父类的构造函数。
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
  var o1 = new Example24B(),
    o2 = new Example24B(value: "evenMore");
  print("Example24 calling super during constructor '${o1.value}'");
  print("Example24 calling super during constructor '${o2.value}'");
}

// 对于简单的类，有一种设置构造函数参数的快捷方式。
// 只需要使用 this.parameterName 的前缀，
// 它就会把参数设置为同名的实例变量。
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}
example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("Example25 shortcut for constructor '${o.value}' and "
    "'${o.anotherValue}'");
}

// 可以在大括号 {} 中声明命名参数。
// 大括号 {} 中声明的参数的顺序是随意的。
// 在中括号 [] 中声明的参数也是可选的。 
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

// 使用 final 声明的变量只能被设置一次。
// 在类里面，final 实例变量可以通过常量的构造函数参数设置。
class Example27 {
  final color1, color2;
  // 更灵活一点的方法是在冒号 : 后面设置 final 实例变量。
  Example27({this.color1, color2}) : color2 = color2;
}
example27() {
  final color = "orange", o = new Example27(color1: "lilac", color2: "white");
  print("Example27 color is '${color}'");
  print("Example27 color is '${o.color1}' and '${o.color2}'");
}

// 要导入一个库，使用 import "libraryPath" 的形式，或者如果要导入的是
// 核心库使用 import "dart:libraryName" 。还有一个称为 "pub" 的包管理工具，
// 它使用 import "package:packageName" 的约定形式。
// 看下这个文件顶部的 import "dart:collection"; 语句。 
// 导入语句必需在其它代码声明之前出现。IterableBase 来自于 dart:collection 。
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

// 对于控制流语句，我们有：
// * 必需带 break 的标准 switch 语句
// * if-else 和三元操作符 ..?..:.. 
// * 闭包和匿名函数
// * break, continue 和 return 语句
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
    // 不会到这里。
  }
}

// 解析 int，把 double 转成 int，或者使用 ~/ 操作符在除法计算时仅保留整数位。
// 让我们也来场猜数游戏吧。
example30() {
  var gn, tooHigh = false,
    n, n2 = (2.0).toInt(), top = int.parse("123") ~/ n2, bottom = 0;
  top = top ~/ 6;
  gn = new DM.Random().nextInt(top + 1); // +1 because nextInt top is exclusive
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

// 程序的唯一入口点是 main 函数。
// 在程序开始执行 main 函数之前，不期望执行任何外层代码。
// 这样可以帮助程序更快地加载，甚至仅惰性加载程序启动时需要的部分。
main() {
  print("Learn Dart in 15 minutes!");
  [example1, example2, example3, example4, example5, example6, example7,
    example8, example9, example10, example11, example12, example13, example14,
    example15, example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25, example26,
    example27, example28, example29, example30
    ].forEach((ef) => ef());
}

```

## 延伸阅读

Dart 有一个综合性网站。它涵盖了 API 参考、入门向导、文章以及更多，
还包括一个有用的在线试用 Dart 页面。
* [https://www.dartlang.org](https://www.dartlang.org)
* [https://try.dartlang.org](https://try.dartlang.org)



