---
language: dart
lang: zh-tw
filename: learndart-tw.dart
contributors:
    - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
translators:
    - ["Bob Lu", "https://github.com/LuPoYi/"]
---

Dart 是程式語言領域的新人。
它借鑒了許多其他主流語言，並且不會偏離它的兄弟語言 JavaScript 太多。
就像 JavaScript 一樣，Dart 的目標是提供良好的瀏覽器整合性。

Dart 最有爭議的特性必然是它的可選類型。

```javascript
import "dart:collection";
import "dart:math" as DM;

// 歡迎進入15分鐘的 Dart 學習。 http://www.dartlang.org/
// 這是一個可實際執行的範例。你可以用 Dart 執行它
// 或者線上執行! 可以把程式碼複製/貼上到這個網站。 http://try.dartlang.org/

// 函數宣告和方法宣告看起來一樣。
// 函數宣告可以是巢狀的。宣告使用這種 name() {} 的形式，
// 或者 name() => 單行表示式; 的形式。
// 右箭頭的宣告形式會直接地返回表示式的結果。
example1() {
  example1nested1() {
    example1nested2() => print("Example1 nested 1 nested 2");
    example1nested2();
  }
  example1nested1();
}

// 匿名函數沒有函數名。
example2() {
  example2nested1(fn) {
    fn();
  }
  example2nested1(() => print("Example2 nested 1"));
}

// 當宣告函數類型的參數的時候，宣告中可以包含
// 函數參數需要的參數，指定所需的參數名即可。
example3() {
  example3nested1(fn(informSomething)) {
    fn("Example3 nested 1");
  }
  example3planB(fn) { // 或者不宣告函數參數的參數
    fn("Example3 plan B");
  }
  example3nested1((s) => print(s));
  example3planB((s) => print(s));
}

// 函數有可以訪問到外層變數的閉包。
var example4Something = "Example4 nested 1";
example4() {
  example4nested1(fn(informSomething)) {
    fn(example4Something);
  }
  example4nested1((s) => print(s));
}

// 下面這個包含 sayIt 方法的類別宣告，同樣有一個可以訪問外層變數的閉包，
// 就像前面的函數一樣。
var example5method = "Example5 sayIt";
class Example5Class {
  sayIt() {
    print(example5method);
  }
}
example5() {
  // 創建一個 Example5Class 類的匿名實例，
  // 並呼叫它的 sayIt 方法。
  new Example5Class().sayIt();
}

// 類別的宣告使用這種形式 class name { [classBody] }.
// classBody 中可以包含實例方法和變數，
// 還可以包含類別方法和變數。
class Example6Class {
  var example6InstanceVariable = "Example6 instance variable";
  sayIt() {
    print(example6InstanceVariable);
  }
}
example6() {
  new Example6Class().sayIt();
}

// 類別方法和變數使用 static 宣告。
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

// 定數非常方便，但是對於在函數/方法的外層的定數有一個限制，
// 類別的外層或外面的定數必須是常數。
// 字串和數字預設是常數。
// 但是 array 和 map 不是。他們需要用 "const" 宣告為常數。
var example8Array = const ["Example8 const array"],
    example8Map = const {"someKey": "Example8 const map"};
example8() {
  print(example8Array[0]);
  print(example8Map["someKey"]);
}

// Dart 中的迴圈使用標準的 for () {} 或 while () {} 的形式，
// 以及更加現代的 for (.. in ..) {} 的形式, 或者
// 以 forEach 開頭並具有許多特性支援函數回呼的形式。
var example9Array = const ["a", "b"];
example9() {
  for (var i = 0; i < example9Array.length; i++) {
    print("Example9 for loop '${example9Array[i]}'");
  }
  var i = 0;
  while (i < example9Array.length) {
    print("Example9 while loop '${example9Array[i]}'");
    i++;
  }
  for (var e in example9Array) {
    print("Example9 for-in loop '${e}'");
  }
  example9Array.forEach((e) => print("Example9 forEach loop '${e}'"));
}

// 透過迴圈遍歷字串中的每個字元或者取出其子字串。
var example10S = "ab";
example10() {
  for (var i = 0; i < example10S.length; i++) {
    print("Example10 String character loop '${example10S[i]}'");
  }
  for (var i = 0; i < example10S.length; i++) {
    print("Example10 substring loop '${example10S.substring(i, i + 1)}'");
  }
}

// 支援兩種數字格式 int 和 double。
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  print("Example11 int ${i}");
  print("Example11 double ${d}");
}

// DateTime 提供了日期/時間的方法。
example12() {
  var now = new DateTime.now();
  print("Example12 now '${now}'");
  now = now.add(new Duration(days: 1));
  print("Example12 tomorrow '${now}'");
}

// 支援正規表達式。
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

// 布林運算式支持隱式轉換以及動態類型
example14() {
  var a = true;
  if (a) {
    print("Example14 true, a is $a");
  }
  a = null;
  if (a) {
    print("Example14 true, a is $a");
  } else {
    print("Example14 false, a is $a"); // 執行到這裡
  }

// 動態類型的null可以轉換成bool型
  var b; // b是動態類型
  b = "abc";
  try {
    if (b) {
      print("Example14 true, b is $b");
    } else {
      print("Example14 false, b is $b");
    }
  } catch (e) {
    print("Example14 error, b is $b"); // 這段程式碼可以執行但是會報錯
  }
  b = null;
  if (b) {
    print("Example14 true, b is $b");
  } else {
    print("Example14 false, b is $b"); // 執行到這裡
  }

  // 靜態類型的null不能轉換成bool型
  var c = "abc";
  c = null;
  // 編譯出錯
  // if (c) {
  //   print("Example14 true, c is $c");
  // } else {
  //   print("Example14 false, c is $c");
  // }
}

// try/catch/finally 和 throw 語句用於例外處理。
// throw 語句可以使用任何物件作為參數。
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

// 想要有效地動態創建長字串，
// 應該使用 StringBuffer。或者 join 一個字串的陣列。
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) { sb.write(e); }
  print("Example16 dynamic string created with "
    "StringBuffer '${sb.toString()}'");
  print("Example16 join string array '${a.join()}'");
}

// 字串連接只需讓相鄰的字串相連，
// 不需要額外的操作運算符號。
example17() {
  print("Example17 "
      "concatenate "
      "strings "
      "just like that");
}

// 字串使用單引號或雙引號做分隔，二者並沒有實際的差異。
// 這種靈活性可以很好地避免內容中需要轉義換行的情況。
// 例如，字串內容裡的 HTML 屬性使用了雙引號。
example18() {
  print('Example18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

// 用三個單引號或三個雙引號表示的字串
// 可以跨越多行，並且包含換行。
example19() {
  print('''Example19 <a href="etc">
Example19 Don't can't I'm Etc
Example19 </a>''');
}

// 字串可以使用 $ 符號插入內容。
// 使用 $ { [expression] } 的形式，表示式的值會被插入到字串中。
// $ 跟著一個變數名會插入變數的值。
// 如果要在字符串中插入 $ ，可以使用 \$ 的轉義形式取代。
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 \$ interpolation ${s1} or $s2 works.");
}

// 可選類型允許作為 API 的標註，並且可以輔助 IDE，
// 這樣 IDE 可以更好地提供重構、自動完成和錯誤檢測功能。
// 目前為止我們還沒有宣告任何類型，並且程式可以執行。
// 事實上，類型在執行時會被忽略。
// 類型甚至可以是錯的，並且程式依然可以執行，
// 好像和類型完全無關一樣。
// 有一個執行時參數可以讓程式進入檢查模式，它會在執行時檢查類型錯誤。
// 這在開發時很有用，但是由於增加了額外的檢查會使程式變慢，
// 因此應該避免在部署時使用。
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

// 類型的繼承形式是 class name extends AnotherClassName {} 。
class Example22A {
  var _name = "Some Name!";
  get name => _name;
}
class Example22B extends Example22A {}
example22() {
  var o = new Example22B();
  print("Example22 class inheritance '${o.name}'");
}

// 類型也可以使用 mixin 的形式 ：
// class name extends SomeClass with AnotherClassName {}.
// 必需繼承某個類型才能 mixin 另一個類型。
// 當前 mixin 的模板類型不能有建構子。
// Mixin 主要是用來和輔助的類型共享方法的，
// 這樣單一繼承就不會影響程式碼重覆使用。
// Mixin 宣告在類型定義的 "with" 後面。
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

// 類型的建構子和類型名相同，形式為
// SomeClass() : super() {}, 其中 ": super()" 的部分是可選的，
// 它用來傳遞參數給父類型的建構子。
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

// 對於簡單的類型，有一種設置構造函數參數的快捷方式。
// 只需要使用 this.parameterName 的前綴，
// 它就會把參數設置為同名的實例變數。
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}
example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("Example25 shortcut for constructor '${o.value}' and "
    "'${o.anotherValue}'");
}

// 可以在大括號 {} 中宣告命名參數。
// 大括號 {} 中宣告的參數的順序是隨意的。
// 在中括號 [] 中宣告的參數也是可選的。
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

// 使用 final 宣告的變數只能被設置一次。
// 在類型裡面，final 實例變數可以通過常數的建構子參數設置。
class Example27 {
  final color1, color2;
  // 更靈活一點的方法是在冒號 : 後面設置 final 實例變數。
  Example27({this.color1, color2}) : color2 = color2;
}
example27() {
  final color = "orange", o = new Example27(color1: "lilac", color2: "white");
  print("Example27 color is '${color}'");
  print("Example27 color is '${o.color1}' and '${o.color2}'");
}

// 要導入一個函式庫，使用 import "libraryPath" 的形式，或者如果要導入的是
// 核心庫使用 import "dart:libraryName" 。還有一個稱為 "pub" 的套件管理工具，
// 它使用 import "package:packageName" 的約定形式。
// 看下這個文件頂部的 import "dart:collection"; 語法。
// 導入語句必需在其它程式碼宣告之前出現。 IterableBase 來自於 dart:collection 。
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

// 對於控制流程，我們有：
// * 必需帶 break 的標準 switch 語法
// * if-else 和三元運算子 ..?..:..
// * 閉包和匿名函數
// * break, continue 和 return 語法
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
    // 不會到這裡。
  }
}

// 解析 int，把 double 轉成 int，或者使用 ~/ 運算符號在除法計算時僅保留整數位。
// 讓我們也來場猜數遊戲吧。
example30() {
  var gn,
      tooHigh = false,
      n,
      n2 = (2.0).toInt(),
      top = int.parse("123") ~/ n2,
      bottom = 0;
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

// 選填位置參數：
// 參數定義使用方括號 [ ]，傳入參數是選填的。
example31() {
    findVolume31(int length, int breath, [int height]) {
      print('length = $length, breath = $breath, height = $height');
    }

    findVolume31(10,20,30); // 可執行
    findVolume31(10,20); // 也可執行
}

// 選填命名參數：
// 參數定義使用大括號 { }, 傳入參數是選填的。
// 必須傳入參數名稱及參數值，並以 : 分隔
// 大括號的順序沒有差別
// 這種類型參數可以幫我們避免多個參數傳入時造成混淆。
example32() {
    findVolume32(int length, int breath, {int height}) {
    print('length = $length, breath = $breath, height = $height');
    }

    findVolume32(10,20,height:30); // 可執行 & 參數名稱在這邊有傳入
    findVolume32(10,20); // 也可執行
}

// 選填預設參數：
// 與選填命名參數相同，此外，我們為此參數定義的預設值
// 如果沒有傳入值，就使用預設值
example33() {
    findVolume33(int length, int breath, {int height=10}) {
     print('length = $length, breath = $breath, height = $height');
    } 

    findVolume33(10,20,height:30); // 可執行
    findVolume33(10,20); // 可執行
}

// 程式的唯一入口點是 main 函式。
// 在程式開始執行 main 函式之前，不會執行任何外部程式碼。
// 這樣有助於更快加載甚至是延遲加載程式啟動時所需要的部分；
main() {
  print("Learn Dart in 15 minutes!");
  [
    example1, example2, example3, example4, example5,
    example6, example7, example8, example9, example10,
    example11, example12, example13, example14, example15,
    example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25,
    example26, example27, example28, example29,
    example30 // 增加此註解可阻止dart formatter把所有項目都換行
  ].forEach((ef) => ef());
}

```

## 延伸閱讀

Dart 有一個綜合性網站。它涵蓋了 API 參考、入門教學、文章以及更多，
還包括一個有用的線上試用 Dart 頁面。
* [https://www.dartlang.org](https://www.dartlang.org)
* [https://try.dartlang.org](https://try.dartlang.org)
