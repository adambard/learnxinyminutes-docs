---
language: dart
filename: learndart-ru.dart
contributors:
  - ["Joao Pedrosa", "https://github.com/jpedrosa/"]
  - ["Vince Ramces Oliveros", "https://github.com/ram231"]
translators:
    - ["nikaose", "https://github.com/nikaose"]
lang: ru-ru
---

**Dart** это однопоточный язык программирования общего назначения.
Он многое заимствует из других основных языков.
Он поддерживает потоки, фьючерсы (известные как промисы в JavaScript), дженерики, первоклассные функции (замыкания) и проверку статического типа.
Dart может работать на любой платформе, включая веб-интерфейс, интерфейс командной строки, десктопные, мобильные устройства и устройства IoT.

Самая спорная функция Dart это ~~Необязательный ввод~~ Статическая безопасность типов и [Проверка звукового типа](https://dart.dev/guides/language/sound-dart).

```dart
import "dart:collection";
import "dart:math" as math;

/// Добро пожаловать в «Изучите Dart за 15 минут». http://dart.dev/
/// Это исполняемый учебник. Вы можете запустить его с помощью Dart или
/// Попробуйте Dart! если вы скопируете/вставите его сюда: http://dartpad.dev/
/// Вы также можете запустить Flutter в DartPad, щелкнув `< > New Pad ` и выбрав Flutter.


/// В Dart все является объектом.
/// Каждое объявление объекта является экземпляром Null и
/// Null также является объектом.


/// 3 типа комментариев в Dart
// Однострочный комментарий
/**
* Многострочный комментарий
* Можно прокомментировать несколько строк
*/
/// Комментарий к документации кода
/// Он использует синтаксис markdown для создания документации кода при создании API.
/// Комментарий к документации кода — рекомендуемый выбор при документировании API, классов и методов.

/// 4 типа объявления переменных.
/// Константы — это переменные, которые являются неизменяемыми и не могут быть изменены.
/// `const` в Dart должен использовать объявление имени SCREAMING_SNAKE_CASE.
const CONSTANT_VALUE = "Я НЕ МОГУ ПОМЕНЯТЬ";
CONSTANT_VALUE = "Я сделал?"; // Ошибка
/// Final — это еще одно объявление переменной, которое нельзя изменить после создания экземпляра. Обычно используется в классах и функциях.
/// `final` может быть объявлен в pascalCase.
final finalValue = "значение не может быть изменено после создания экземпляра";
finalValue = "Кажется, нет"; // Ошибка

/// `var` — это еще одно объявление переменной, которая является изменяемой и может изменять свое значение. Dart выведет типы и не изменит тип данных.
var mutableValue = "Переменная строка";
mutableValue = "Так работает";
mutableValue = false; // Ошибка.

/// `dynamic` — это еще одно объявление переменной, в котором тип не оценивается при проверке статического типа Dart.
/// Он может изменить свое значение и тип данных.
/// Некоторые дартисты используют динамический подход с осторожностью, поскольку он не может отслеживать тип данных. так что используйте его на свой страх и риск
dynamic dynamicValue = "Я строка";
dynamicValue = false; // false


/// Функции могут быть объявлены в глобальном пространстве.
/// Объявление функции и объявление метода выглядят одинаково.
/// Объявления функции могут быть вложенными.
/// Декларация имеет форму name() {} или name() => singleLineExpression;
/// Объявление функции большой стрелкой может быть неявным или явным возвратом результата выражения.
/// Dart выполнит функцию `main()` в любом месте проекта dart.
///
example1() {
  nested1() {
    nested2() => print("Example1 nested 1 nested 2");
    nested2();
  }

  nested1();
}

/// Анонимные функции не включают имя
example2() {
  //// Явный тип возвращаемого значения.
  nested1(void Function() fn) {
    fn();
  }
  nested1(() => print("Example2 nested 1"));
}

/// Когда объявлен параметр функции, объявление может включать количество параметров,
/// которые принимает функция, путем явного указания имен принимаемых ею параметров.
example3() {
  planA(fn(String informSomething)) {
    fn("Example3 plan A");
  }
  planB(fn) {
    // Или не объявляйте количество параметров.
    fn("Example3 plan B");
  }

  planA((s) => print(s));
  planB((s) => print(s));
}

/// Функции имеют замыкающий доступ к внешним переменным.
/// Dart выводит типы, когда переменная имеет какое-либо значение.
/// В этом примере Dart знает, что эта переменная является строкой.
var example4Something = "Example4 nested 1";
example4() {
  nested1(fn(informSomething)) {
    fn(example4Something);
  }

  nested1((s) => print(s));
}

/// Объявление класса с методом sayIt, который также имеет закрывающий доступ
/// к внешней переменной, как если бы это была функция, как было показано ранее.
var example5method = "Example5 sayIt";

class Example5Class {
  sayIt() {
    print(example5method);
  }
}

example5() {
  /// Создайте анонимный экземпляр класса Example5Class и вызовите для него метод sayIt
  /// Ключевое слово `new` в Dart не является обязательным..
  new Example5Class().sayIt();
}

/// Объявление класса принимает форму имени класса. { [classBody] }.
/// Где classBody может включать методы и переменные экземпляра, 
/// а также методы и переменные класса.
class Example6Class {
  var instanceVariable = "Example6 переменная экземпляра";
  sayIt() {
    print(instanceVariable);
  }
}

example6() {
   Example6Class().sayIt();
}

/// Методы и переменные класса объявляются с помощью «статических» терминов.
class Example7Class {
  static var classVariable = "Example7 переменная класса";
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

/// Dart поддерживает дженерики.
/// Дженерики относятся к технике написания кода для класса.
/// без указания типов данных, с которыми работает класс.
/// Источник: https://stackoverflow.com/questions/4560890/what-are-generics-in-c

/// Тип `T` относится к любому типу, экземпляр которого был создан.
/// вы можете вызывать все, что захотите.
/// Программисты используют это соглашение в следующих случаях:
/// T - Тип (используется для типов классов и примитивов)
/// E - Элемент (используется для списка, набора или итерации)
/// K,V - Ключевое значение (используется для Map)
class GenericExample<T>{
  void printType(){
    print("$T")
  }
  // методы также могут иметь дженерики
  genericMethod<M>(){
    print("class:$T, method: $M");
  }
}


/// Список похож на массивы, но список является дочерним элементом Iterable<E>.
/// Поэтому Maps, List, LinkedList являются дочерними элементами Iterable<E>, 
/// чтобы иметь возможность зацикливаться с использованием ключевого слова `for`
/// Важные вещи, которые следует помнить:
/// () - Iterable<E>
/// [] - List<E>
/// {} - Map<K,V>


/// Список — это здорово, но есть ограничение на то, каким может быть список.
/// outside of function/method bodies. List on the outer scope of class
/// вне тела функции/метода. Список во внешней области класса или
/// вне класса должен быть постоянным. Строки и числа используются по умолчанию.
/// А вот массивы и карты — нет. Их можно сделать постоянными,
/// объявив их "const". Что-то похожее на Object.freeze() в Javascript.
const example8List = ["Example8 const array"];
const  example8Map = {"someKey": "Example8 const map"};
/// Объявите List или Map как объекты.
 List<String> explicitList = new List<String>();
 Map<String,dynamic> explicitMaps = new Map<String,dynamic>();

 explicitList.add("НекоторыйМассив");
example8() {
  print(example8Map["какой-тоКлюч"]);
  print(explicitList[0]);
}

/// Присвоение списка одной переменной другой не будет тем же результатом.
/// Потому что dart передает ссылку по значению.
/// Поэтому, когда вы назначаете существующий список новой переменной.
/// Вместо списка он становится итерируемым
var iterableExplicitList = explicitList;
print(iterableExplicitList) // ("НекоторыйМассив"); "[]" становится "()"
var newExplicitLists = explicitList.toList() // Преобразует Iterable<E> в List<E>

/// Циклы в Dart имеют форму стандартных циклов for () {} или while () {},
/// немного более современный for (.. in ..) {} или функциональные обратные вызовы 
/// со многими поддерживаемыми функциями, начиная с forEach,map иwhere.
var example9Array = const ["a", "b"];
example9() {
  for (int i = 0; i < example9Array.length; i++) {
    print("Example9 цикл цикл '${example9Array[i]}'");
  }
  var i = 0;
  while (i < example9Array.length) {
    print("Example9 цикл while '${example9Array[i]}'");
    i++;
  }
  for (final e in example9Array) {
    print("Example9 цикл for-in '${e}'");
  }

  example9Array.forEach((e) => print("Example9 цикл forEach '${e}'"));

}

/// Чтобы перебрать символы строки или извлечь подстроку.
var example10String = "ab";
example10() {
  for (var i = 0; i < example10String.length; i++) {
    print("Example10 цикл строковых символов '${example10String[i]}'");
  }
  for (var i = 0; i < example10String.length; i++) {
    print("Example10 цикл извлечения '${example10String.substring(i, i + 1)}'");
  }
}

/// `int`, `double` и `num` — три поддерживаемых числовых формата.
/// `num` может быть либо `int`, либо `double`.
/// `int` и `double` являются дочерними элементами типа `num`
example11() {
  var i = 1 + 320, d = 3.2 + 0.01;
  final num myFinalNumDouble = 2.2;
  final num myFinalNumInt = 2;
  final int myFinalInt = 1;
  final double myFinalDouble = 0.1;
  num myNumDouble = 2.2;
  num myNumInt = 2;
  int myInt = 1;
  double myDouble = 0; // Dart добавит десятичный префикс и станет 0.0;
  myNumDouble = myFinalInt; // действительный
  myNumDouble = myFinalDouble; // действительный
  myNumDouble = myFinalNumInt; // действительный

  myNumInt = myFinalInt; // действительный
  myNumInt = myFinalDouble; // действительный
  myNumInt = myFinalNumDouble; // действительный

  myInt = myNumDouble; // ошибка
  myInt = myFinalDouble; // ошибка
  myInt = myFinalNumInt; // действительный

  myDouble = myFinalInt; // ошибка
  myDouble = myFinalNumInt; // ошибка
  myDouble = myFinalNumDouble; // действительный

  print("Example11 int ${i}");
  print("Example11 double ${d}");

}

/// DateTime обеспечивает арифметику даты и времени.
example12() {
  var now = new DateTime.now();
  print("Example12 сейчас '${now}'");
  now = now.add(new Duration(days: 1));
  print("Example12 завтра '${now}'");
}

/// Поддерживаются регулярные выражения.
example13() {
  var s1 = "some string", s2 = "some", re = new RegExp("^s.+?g\$");
  match(s) {
    if (re.hasMatch(s)) {
      print("Example13 совпадения с регулярными выражениями '${s}'");
    } else {
      print("Example13 регулярное выражение не соответствует '${s}'");
    }
  }

  match(s1);
  match(s2);
}

/// Логические выражения поддерживают неявные преобразования и динамический тип.
example14() {
  var a = true;
  if (a) {
    print("true, a равно $a");
  }
  a = false;
  if (a) {
    print("true, a равно $a"); 
  } else {
    print("false, a равно $a"); /// сработает здесь
  }

  /// динамический типизированный null не может быть преобразован в bool
  var b; /// b — динамический тип
  b = "abc";
  try {
    if (b) {
      print("true, b равно $b");
    } else {
      print("false, b равно $b");
    }
  } catch (e) {
    print("error, b равно $b"); /// это можно было запустить, но возникает ошибка
  }
  b = null; 
  if (b) { /// Неудачное утверждение: логическое выражение не должно быть нулевым)
    print("true, b равно $b");
  } else {
    print("false, b равно $b"); 
  }

  /// статически типизированный null не может быть преобразован в bool
  var c = "abc";
  c = null;
  /// компиляция не удалась
  /// if (c) {
  ///   print("true, c равно $c");
  /// } else {
  ///   print("false, c равно $c");
  /// }
}

/// try/catch/finally и throw используются для обработки исключений.
/// throw принимает любой объект в качестве параметра;
example15() {
  try {
    try {
      throw "Какая-то неожиданная ошибка.";
    } catch (e) {
      print("Example15 исключение: '${e}'");
      throw e; /// Re-throw
    }
  } catch (e) {
    print("Example15 catch exception being re-thrown: '${e}'");
  } finally {
    print("Example15 Still run finally");
  }
}

/// Чтобы быть эффективным при динамическом создании длинной строки, 
/// используйте StringBuffer. Или вы можете присоединиться к массиву строк.
example16() {
  var sb = new StringBuffer(), a = ["a", "b", "c", "d"], e;
  for (e in a) {
    sb.write(e);
  }
  print("Example16 динамическая строка, созданная с помощью "
      "StringBuffer '${sb.toString()}'");
  print("Example16 объединить массив строк '${a.join()}'");
}

/// Строки можно объединить, просто разместив список строк рядом друг с другом
/// без необходимости использования дополнительных операторов.

example17() {
  print("Example17 "
      "concatenate "
      "strings "
      "just like that");
}

/// Строки имеют одинарные или двойные кавычки для разделителей, 
/// между которыми нет фактической разницы. Данная гибкость может быть полезной,
/// чтобы избежать необходимости экранировать содержимое, соответствующее используемому разделителю.
/// Например, двойные кавычки атрибутов HTML, если строка содержит содержимое HTML.
example18() {
  print('Example18 <a href="etc">'
      "Don't can't I'm Etc"
      '</a>');
}

/// Строки с тройными одинарными или тройными двойными кавычками занимают несколько строк
/// и включают разделители строк.
example19() {
  print('''Example19 <a href="etc">
Example19 Don't can't I'm Etc
Example19 </a>''');
}

/// Строки имеют удобную функцию интерполяции с помощью символа $.
/// При использовании $ {[expression] } возврат выражения интерполируется.
/// $ за которым следует имя переменной, интерполирует содержимое этой переменной.
/// $ можно экранировать следующим образом: \$, чтобы вместо этого просто добавить его в строку.
example20() {
  var s1 = "'\${s}'", s2 = "'\$s'";
  print("Example20 \$ интерполяция ${s1} или $s2 работает.");
}

/// Необязательные типы позволяют аннотировать API и приходят на помощь IDE,
/// чтобы IDE могли лучше выполнять рефакторинг, автозаполнение и проверку на наличие ошибок.
/// До сих пор мы не объявляли никаких типов, и программы работали нормально.
/// Фактически, типы игнорируются во время выполнения. Типы могут даже быть неправильными,
/// но программа все равно будет иметь преимущество сомнения и будет работать так,
/// как будто типы не имеют значения.
/// Существует параметр времени выполнения, который проверяет наличие ошибок типа.
/// Это проверенный режим, который считается полезным во время разработки,
/// но который также медленнее из-за дополнительной проверки и, 
/// следовательно, его избегают во время выполнения развертывания.
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
  print("Example21 имена '${o.names}' и длина '${o.length}'");
  o.names = ["d", "e"];
  print("Example21 имена '${o.names}' и длина '${o.length}'");
}

/// Наследование классов принимает форму имени класса расширяет AnotherClassName {}.
class Example22A {
  var _name = "Some Name!";
  get name => _name;
}

class Example22B extends Example22A {}

example22() {
  var o = new Example22B();
  print("Example22 наследование класса '${o.name}'");
}

/// Миксин классов также доступен и принимает форму имени класса,
/// расширяющего SomeClass с AnotherClassName {}. 
/// Необходимо расширить какой-то класс, чтобы можно было добавить другой.
/// Класс шаблона миксина на данный момент не может иметь конструктор.
/// Миксин в основном используется для совместного использования методов с удаленными классами,
/// поэтому единое наследование не мешает повторному использованию кода.
/// Миксины следуют за оператором «with» во время объявления класса.
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
  print("Example23 addThree(1, 2, 3) приводит к результату '${r1}'");
  print("Example23 addTwo(1, 2) приводит к результату '${r2}'");
}

/// Метод конструктора класса использует то же имя класса и
/// принимает форму SomeClass() : super() {}, где часть ": super()" является необязательной и
/// используется для делегирования константных параметров суперуправлению-родительский конструктор.
class Example24A {
  var _value;
  Example24A({value: "некотороеЗначение"}) {
    _value = value;
  }
  get value => _value;
}

class Example24B extends Example24A {
  Example24B({value: "некотороеДругоеЗначение"}) : super(value: value);
}

example24() {
  var o1 = new Example24B(), o2 = new Example24B(value: "evenMore");
  print("Example24 вызов super во время конструктора '${o1.value}'");
  print("Example24 вызов super во время конструктора '${o2.value}'");
}

/// Существует ярлык для установки параметров конструктора в случае более простых классов.
/// Просто используйте префикс this.parameterName, и он установит параметр в переменную экземпляра с тем же именем.
class Example25 {
  var value, anotherValue;
  Example25({this.value, this.anotherValue});
}

example25() {
  var o = new Example25(value: "a", anotherValue: "b");
  print("Example25 ярлык для конструктора '${o.value}' и "
      "'${o.anotherValue}'");
}

/// Именованные параметры доступны, если они объявлены между {}.
/// Порядок параметров может быть необязательным, если они объявлены между {}.
/// Параметры можно сделать необязательными, если они объявлены между [].
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
  print("Example26 имя '${_name}', фамилия '${_surname}', "
      "email '${_email}'");
  setConfig2("Mary", "Jane");
  print("Example26 имя '${_name}', фамилия '${_surname}', "
      "email '${_email}'");
}

/// Переменные, объявленные с помощью Final, можно установить только один раз.
/// В случае классов конечные переменные экземпляра могут быть установлены через постоянный параметр конструктора.
class Example27 {
  final color1, color2;
  /// Немного гибкости для установки конечных переменных экземпляра с синтаксисом,
  /// следующим за:
  Example27({this.color1, color2}) : color2 = color2;
}

example27() {
  final color = "оранжевый", o = new Example27(color1: "сиреневый", color2: "белый");
  print("Example27 цвет '${color}'");
  print("Example27 цвет '${o.color1}' и '${o.color2}'");
}

/// Чтобы импортировать библиотеку, используйте import "libraryPath" или,
/// если это основная библиотека, импортируйте "dart:libraryName".
/// Существует также управление пакетами «pub» со своим собственным соглашением
/// об импорте «package:packageName». См. импорт «dart:collection»; в начале.
/// Импорт должен предшествовать другим объявлениям кода. IterableBase происходит из dart:collection.
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

/// Для потока управления у нас есть:
/// * стандартный переключатель с операторами must Break
/// * if-else if-else и тернарный оператор ..?..:..
/// * замыкания и анонимные функции
/// * операторы break, continue и return
example29() {
  var v = true ? 30 : 60;
  switch (v) {
    case 30:
      print("Example29 оператор switch");
      break;
  }
  if (v < 30) {
  } else if (v > 30) {
  } else {
    print("Example29 оператор if-else");
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
    /// Никогда сюда не попадет.
  }
}

/// Парсим int, преобразуем double в int или просто сохраняем целое число при
/// делении чисел с помощью операции ~/. Давайте тоже поиграем в угадайку.
example30() {
  var gn,
      tooHigh = false,
      n,
      n2 = (2.0).toInt(),
      top = int.parse("123") ~/ n2,
      bottom = 0;
  top = top ~/ 6;
  gn = new DM.Random().nextInt(top + 1); /// +1, потому что nextInt top является эксклюзивным
  print("Example30 Угадайте число от 0 до ${top}");
  guessNumber(i) {
    if (n == gn) {
      print("Example30 Угадал! Число ${gn}");
    } else {
      tooHigh = n > gn;
      print("Example30 Число ${n} слишком "
          "${tooHigh ? 'большое' : 'маленькое'}. Попробуйте еще раз");
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

/// Необязательный позиционный параметр:
/// параметр будет указан в квадратных скобках [ ] & параметр в квадратных скобках является необязательным.
example31() {
    findVolume31(int length, int breath, [int height]) {
      print('length = $length, breath = $breath, height = $height');
    }

    findVolume31(10,20,30); //действительный
    findVolume31(10,20); //также действителен
}

/// Необязательный именованный параметр: 
/// параметр будет указан в фигурных скобках { } 
/// Параметр в фигурных скобках не является обязательным. 
/// необходимо использовать имя параметра, чтобы присвоить значение, 
/// которое разделено двоеточием: в фигурных скобках порядок параметров не имеет значения,
/// эти параметры типа помогают нам избежать путаницы при передаче значения для функции,
/// которая имеет много параметров.
example32() {
    findVolume32(int length, int breath, {int height}) {
    print('length = $length, breath = $breath, height = $height');
    }

    findVolume32(10,20,height:30);//действительно, и мы видим, что здесь упоминается имя параметра.
    findVolume32(10,20);//также действителен
}

/// Необязательный параметр по умолчанию: 
/// то же, что и необязательный именованный параметр, кроме того,
/// мы можем назначить этому параметру значение по умолчанию. 
/// это означает, что значение не передается, будет принято значение по умолчанию.
example33() {
    findVolume33(int length, int breath, {int height=10}) {
     print('length = $length, breath = $breath, height = $height');
    }

    findVolume33(10,20,height:30);//действительный
    findVolume33(10,20);//действительный
}

/// В Dart также добавлена ​​такая функция, как операторы, поддерживающие Null.
var isBool = true;
var hasString = isBool ?? "default String";

/// Программы имеют только одну точку входа в главную функцию.
/// Ожидается, что во внешней области ничего не будет выполнено до того,
/// как программа начнет работать с тем, что находится в ее основной функции.
/// Это помогает ускорить загрузку и даже ленивую загрузку именно того, с чем программа должна запускаться.
main() {
  print("Изучите Dart за 15 минут!");
  [
    example1, example2, example3, example4, example5,
    example6, example7, example8, example9, example10,
    example11, example12, example13, example14, example15,
    example16, example17, example18, example19, example20,
    example21, example22, example23, example24, example25,
    example26, example27, example28, example29,
    example30 // Добавление этого комментария не позволяет средству форматирования dart помещать все элементы на новую строку.
  ].forEach((ef) => ef());
}
```

## Дальнейшее чтение

У Dart есть обширный веб-сайт. Он охватывает справочник по API, учебные пособия, статьи и многое другое, включая
полезный DartPad (облачная площадка для программирования Dart).
[https://dart.dev/](https://dart.dev)
[https://dartpad.dev/](https://dartpad.dev)
