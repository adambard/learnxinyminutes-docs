---
language: php
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["SlaF", "https://github.com/SlaF"]
lang: ru-ru
filename: learnphp-ru.php
---

Этот документ описывает версию PHP 5 и выше.

```php
<?php // PHP код должен быть заключен в теги <?php

// Если ваш файл содержит только PHP код, то можно
// пропустить закрывающийся ?>

// А так начинаются комментарии

# Это тоже комментарий но // предпочтительнее

/*
	Окруженный /* и */ текст превращается
	в многострочный комментарий
*/

// Используйте "echo" или "print" для вывода.
print('Hello '); // Напечатать "Hello " без перевода строки

// () необязательно применять для print и echo
echo "World\n"; // Печатать "World" и перейти на новую строку.
// (все утверждения должны заканчиваться ;)

// Любые символы за пределами закрывающегося тега выводятся автоматически:
?>
Hello World Again!
<?php


/************************************
 * Типы и Переменные
 */

// Переменные начинаются с символа $.
// Правильное имя переменной начинается с буквы или знака подчеркивания,
// и может содержать любые цифры, буквы, или знаки подчеркивания.
// Не рекомендуется использовать кирилические символы в именах (прим. пер.)

// Логические значения нечувствительны к регистру
$boolean = true;  // или TRUE или True
$boolean = false; // или FALSE или False

// Целые числа
$int1 = 12;   // => 12
$int2 = -12;  // => -12-
$int3 = 012;  // => 10 (ведущий 0 обозначает восьмеричное число)
$int4 = 0x0F; // => 15 (ведущие символы 0x означает шестнадцатеричное число)

// Дробные числа
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Арифметика
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// Арифметические сокращения
$number = 0;
$number += 1;      // Увеличивает $number на 1
echo $number++;    // Печатает 1 (инкрементируется после вывода)
echo ++$number;    // Печатает 3 (инкрементируется до вывода)
$number /= $float; // Делится и результат присваивается $number

// Строки должны быть заключены в одинарные кавычки;
$sgl_quotes = '$String'; // => '$String'

// Избегайте двойных кавычек за исключением случаев интерполирования переменной
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// Специальные (escape) символы работают только в двойных кавычках
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// Заключайте переменные в фигурные скобки если это необходимо
$money = "I have $${number} in the bank.";

// Начиная с PHP 5.3, синтаксис nowdocs может использоваться для
// неинтерполированного многострочного текста
$nowdoc = <<<'END'
Multi line
string
END;

// Heredocs поддерживает интерполяцию переменных
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// Строки соединяются при помощи .
echo 'This string ' . 'is concatenated';


/********************************
 * Константы
 */
 
// Константа определяется при помощи define()
// и никогда не может быть изменена во время выполнения программы!

// Правильное имя константы начинается с буквы или символа подчеркивания,
// и содержит любое колличество букв, цифр и знаков подчеркивания.
define("FOO", "something");

// Доступ к константе возможен через прямое указание её имени
echo 'This outputs '.FOO;

/********************************
 * Массивы
 */

// Все массивы в PHP - это ассоциативные массивы или хеши,

// Ассоциативные массивы, известные в других языках как хеш-карты.

// Работает во всех версиях РHP
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// В PHP 5.4 появился новый синтаксис
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // печатает 1

// Список тоже содержит целочисленные ключи
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"


/********************************
 * Вывод
 */

echo('Hello World!');
// Печатает Hello World! на stdout.
// Stdout это веб-страница запущенная в браузере.

print('Hello World!'); // Аналогично echo

// echo - это конструкция языка, вы можете опустить скобки.
echo 'Hello World!';
print 'Hello World!'; // Выводит Hello World!

$paragraph = 'paragraph';

echo 100;        // Печать скалярной переменной напрямую
echo $paragraph; // или печать переменной

// Если короткие теги разрешены, или ваша версия PHP >= 5.4
// вы можете использовать сокращенный синтаксис echo
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x теперь содержит значение переменной $y
$z = &$y;
// $z содержит ссылку на $y. Изменение значения $z затронет значение $y и наоборот.
// Значение $x остается неизменным. 

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0


/********************************
 * Логические выражения
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// Утверждение (assert) выдает предупреждение если аргумент не true

// Эти сравнения всегда будут истинными, даже если типы будут различаться
assert($a == $b); // "равно"
assert($c != $a); // "не равно"
assert($c <> $a); // другое обозначение "не равно"
assert($a < $c); // меньше
assert($c > $b); // больше
assert($a <= $b); // меньше или равно
assert($c >= $d); // больше или равно

// Следующие утверждения истинны если переменные имеют одинаковый тип.
assert($c === $d);
assert($a !== $d);
assert(1 == '1');
assert(1 !== '1');

// Переменные могут изменять тип, в зависимости от их использования.
$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (строка превращается в число)

// Выводится 0 по той причине, что оператор + не может привести строку 'one' к
// числовому типу
$string = 'one';
echo $string + $string; // => 0

// Приведение типов (type casting) может быть использовано для преобразование
// переменной в другой тип
$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// Также существуют функции выполняющие приведение типов
$integer = 5;
$string = strval($integer);
$float = floatval($integer);

$var = null; // Null

// И похожая по действию функция
$integer = 10;
$boolen = settype($integer, "string") // теперь $integer имеет строковый тип

// settype возвращает true - если преобразование удалось и false в противном случае

/********************************
 * Управляющие структуры
 */

if (true) {
    print 'I get printed';
}

if (false) {
    print 'I don\'t';
} else {
    print 'I get printed';
}

if (false) {
    print 'Does not get printed';
} elseif(true) {
    print 'Does';
}

// Тернарный оператор
print (false ? 'Does not get printed' : 'Does');

$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}

// Альтернативный синтаксис полезный для шаблонов
?>

<?php if ($x): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>

<?php

// Использование switch.
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // You must include a break, or you will fall through
               // to cases 'two' and 'three'
    case 'two':
    case 'three':
        // Do something if $variable is either 'two' or 'three'
        break;
    default:
        // Do something by default
}

// Циклы: while, do...while и for
$i = 0;
while ($i < 5) {
    echo $i++;
}; // Prints "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // Prints "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // Prints "0123456789"

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// Циклы foreach могут обходить массивы
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // Prints "24"

echo "\n";

// Вы можете обходить как ключи, так и их значения
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Exit out of the while loop
    }
    echo $i++;
} // Prints "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Skip this iteration of the loop
    }
    echo $i;
} // Prints "0124"


/********************************
 * Функции
 */

// Определение функции:
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"

// Правильное имя функции начинается с буквы или символа подчеркивания
// и состоит из букв, цифр или знаков подчеркивания.

function add ($x, $y = 1) { // $y по умолчанию равно 1
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result недоступна за пределами функции
// print $result; // Выдает предупреждение

// Начиная с PHP 5.3 вы можете объявлять анонимные функции:
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Функции могут возвращать функции
function bar ($x, $y) {
  // Используйте 'use' для передачи внешних переменных
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // Prints "A - B - C"

// Вы можете вызывать именованные функции используя строки
$function_name = 'add';
echo $function_name(1, 2); // => 3
// Полезно для программного определения запущенной функции.
// Или используйте call_user_func(callable $callback [, $parameter [, ... ]]);


/********************************
 * Includes
 */

<?php
// PHP код внутри включаемого файла должен начинаться с тега PHP.

include 'my-file.php';
// Код в файле my-file.php теперь доступен в текущем в текущем пространстве имен.
// Если файл не удалось включить, будет выдано предупреждение.

include_once 'my-file.php';
// Если код в файле my-file.php уже был включен, он не будет включен повторно.
// Это предотвращает ошибку повторного включения файла.

require 'my-file.php';
require_once 'my-file.php';

// Same as include(), except require() will cause a fatal error if the
// file cannot be included.
// Действует также как и include(), но если файл не удалось подключить,
// функция выдает неисправимую ошибку

// Содержимое файла my-include.php:
<?php

return 'Anything you like.';
// Конец файла

// Эти функции могут также возвращать значения.
$value = include 'my-include.php';

// Имена файлов содержат их путь в файловой системе, или если передано просто
// имя файла, PHP обращается к директиве include_path. Если файл не найден в
// include_path, предпринимается попытка поиска в папке, где выполняется скрипт
// или в текущей рабочей директории. Если не в одном из этих мест файл не
// найден - выдается ошибка
/* */

/********************************
 * Классы
 */

// Классы определяются при помощи ключевого слова "class"

class MyClass
{
    const MY_CONST      = 'value'; // A constant

    static $staticVar   = 'static';

    // Properties must declare their visibility
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // Accessible from the class and subclasses
    private $priv   = 'private';   // Accessible within the class only

    // Create a constructor with __construct
    public function __construct($instanceProp) {
        // Access instance variables with $this
        $this->instanceProp = $instanceProp;
    }

    // Methods are declared as functions inside a class
    public function myMethod()
    {
        print 'MyClass';
    }

    final function youCannotOverrideMe()
    {
    }

    public static function myStaticMethod()
    {
        print 'I am static';
    }
}

echo MyClass::MY_CONST;    // Выведет 'value';
echo MyClass::$staticVar;  // Выведет 'static';
MyClass::myStaticMethod(); // Выведет 'I am static';

// Новый экземпляр класса используя new
$my_class = new MyClass('An instance property');

// Если аргументы отсутствуют, можно не ставить круглые скобки

// Доступ к членам класса используя ->
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"

// Наследование классов используя "extends"
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // Override a method
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => Выведет "protected"
$my_other_class->myMethod();               // Выведет "MyClass > MyOtherClass"

final class YouCannotExtendMe
{
}

// Вы можете использовать "магические методы" для создания геттеров и сеттеров
class MyMapClass
{
    private $property;

    public function __get($key)
    {
        return $this->$key;
    }

    public function __set($key, $value)
    {
        $this->$key = $value;
    }
}

$x = new MyMapClass();
echo $x->property; // Будет использован метод __get()
$x->property = 'Something'; // Будет использован метод __set()

// Классы могут быть абстрактными (используя ключевое слово abstract)
// или реализовывать интерфейсы (используя ключевое слово implements).
// Интерфейсы определяются при помощи ключевого слова interface

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// Интерфейсы могут быть расширены
interface InterfaceThree extends InterfaceTwo
{
    public function doAnotherContract();
}

abstract class MyAbstractClass implements InterfaceOne
{
    public $x = 'doSomething';
}

class MyConcreteClass extends MyAbstractClass implements InterfaceTwo
{
    public function doSomething()
    {
        echo $x;
    }

    public function doSomethingElse()
    {
        echo 'doSomethingElse';
    }
}

// Классы могут реализовывать более одного интерфейса
class SomeOtherClass implements InterfaceOne, InterfaceTwo
{
    public function doSomething()
    {
        echo 'doSomething';
    }

    public function doSomethingElse()
    {
        echo 'doSomethingElse';
    }
}


/********************************
 * Трейты
 */

// Трейты появились в PHP 5.4.0 и объявляются при помощи ключевого слова trait

trait MyTrait
{
    public function myTraitMethod()
    {
        print 'I have MyTrait';
    }
}

class MyTraitfulClass
{
    use MyTrait;
}

$cls = new MyTraitfulClass();
$cls->myTraitMethod(); // Prints "I have MyTrait"


/********************************
 * Пространства имен
 */

// Это секция особая, ведь объявление пространства имен
// должно быть самым первым в файле. Позвольте сделать вид, что это не так

<?php

// По умолчанию, классы существуют в глобальном пространстве имен и могут быть
// вызваны с обратным слешем.

$cls = new \MyClass();

// Задание пространства имен файла
namespace My\Namespace;

class MyClass
{
}

// (из другого файла)
$cls = new My\Namespace\MyClass;

// Или внутри другого пространства имен.
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// Или вы можете создать псевдоним для пространства имен:
namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

*/

```

## Смотрите также:
Посетите страницу [официальной документации PHP](http://www.php.net/manual/) для справки. 
Если вас интересуют полезные приемы использования PHP посетите [PHP The Right Way](http://www.phptherightway.com/).
Если вы раньше пользовались языком с хорошей организацией пакетов, посмотрите [Composer](http://getcomposer.org/).
Для изучения стандартов использования языка посетите PHP Framework Interoperability Group's [PSR standards](https://github.com/php-fig/fig-standards). 
