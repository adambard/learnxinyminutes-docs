---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Chenbo Li", "http://binarythink.net"]
filename: learnphp-zh.php
lang: zh-cn
---

这份教程所使用的版本是 PHP 5+.

```php
<?php // PHP必须被包围于 <?php ? > 之中

// 如果你的文件中只有php代码，那么最好省略结束括号标记

// 这是单行注释的标志

# 井号也可以，但是//更常见

/*
     这是多行注释
*/

// 使用 "echo" 或者 "print" 来输出信息到标准输出
print('Hello '); // 输出 "Hello " 并且没有换行符

// () 对于echo和print是可选的
echo "World\n"; // 输出 "World" 并且换行
// (每个语句必须以分号结尾)

// 在 <?php 标签之外的语句都被自动输出到标准输出
?>Hello World Again!
<?php


/************************************
 * 类型与变量
 */

// 变量以$开始
// 变量可以以字母或者下划线开头，后面可以跟着数字、字母和下划线

// 布尔值是大小写无关的
$boolean = true;  // 或 TRUE 或 True
$boolean = false; // 或 FALSE 或 False

// 整型
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (0开头代表八进制数)
$int4 = 0x0F; // => 15 (0x开头代表十六进制数)

// 浮点型 (即双精度浮点型)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// 算数运算
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// 算数运算的简写
$number = 0;
$number += 1;      // $number 自增1
echo $number++;    // 输出1 (运算后自增)
echo ++$number;    // 输出3 (自增后运算)
$number /= $float; // 先除后赋值给 $number

// 字符串需要被包含在单引号之中
$sgl_quotes = '$String'; // => '$String'

// 如果需要在字符串中引用变量，就需要使用双引号
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// 特殊字符只有在双引号中有用
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// 可以把变量包含在一对大括号中
$money = "I have $${number} in the bank.";

// 自 PHP 5.3 开始, nowdocs 可以被用作多行非计算型字符串
$nowdoc = <<<'END'
Multi line
string
END;

// 而Heredocs则可以用作多行计算型字符串
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// 字符串需要用 . 来连接
echo 'This string ' . 'is concatenated';


/********************************
 * 数组
 */

// PHP 中的数组都是关联型数组，也就是某些语言中的哈希表或字典

// 在所有PHP版本中均适用：
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4 中引入了新的语法
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // 输出 1

// 声明为列表实际上是给每个值都分配了一个整数键（key）
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"


/********************************
 * 输出
 */

echo('Hello World!');
// 输出到标准输出
// 此时标准输出就是浏览器中的网页

print('Hello World!'); // 和echo相同

// echo和print实际上也属于这个语言本身，所以我们省略括号
echo 'Hello World!';
print 'Hello World!'; 

$paragraph = 'paragraph';

echo 100;        // 直接输出标量
echo $paragraph; // 或者输出变量

// 如果你配置了短标签，或者使用5.4.0及以上的版本
// 你就可以使用简写的echo语法
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x 现在和 $y 的值相同
$z = &$y;
// $z 现在持有 $y 的引用. 现在更改 $z 的值也会更改 $y 的值，反之亦然
// 但是改变 $y 的值不会改变 $x 的值

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0


/********************************
 * 逻辑
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// 如果assert的参数为假，就会抛出警告

// 下面的比较都为真，不管它们的类型是否匹配
assert($a == $b); // 相等
assert($c != $a); // 不等
assert($c <> $a); // 另一种不等的表示
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// 下面的比较只有在类型相同、值相同的情况下才为真
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// 变量可以根据其使用来进行类型转换

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (字符串在此时被转化为整数)

$string = 'one';
echo $string + $string; // => 0
// 输出0，因为'one'这个字符串无法被转换为整数

// 类型转换可以将一个类型视作另一种类型

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// 还有一些专用的函数来进行类型转换
$integer = 5;
$string = strval($integer);

$var = null; // 空值


/********************************
 * 控制结构
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

// 三目运算符
print (false ? 'Does not get printed' : 'Does');

$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}



// 下面的语法常用于模板中:
?>

<?php if ($x): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>

<?php

// 用switch来实现相同的逻辑
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // 在case中必须使用一个break语句，
		       // 否则在执行完这个语句后会直接执行后面的语句
    case 'two':
    case 'three':
        // 如果$variable是 'two' 或 'three'，执行这里的语句
        break;
    default:
        // 其他情况
}

// While, do...while 和 for 循环
$i = 0;
while ($i < 5) {
    echo $i++;
}; // 输出 "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // 输出 "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // 输出 "0123456789"

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// Foreach 循环可以遍历数组
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // 输出 "24"

echo "\n";

// 也可以同时遍历键和值
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // 退出循环
    }
    echo $i++;
} // 输出 "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // 跳过此次遍历
    }
    echo $i;
} // 输出 "0124"


/********************************
 * 函数
 */

// 通过"function"定义函数:
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"

// 函数名需要以字母或者下划线开头, 
// 后面可以跟着任意的字母、下划线、数字.

function add ($x, $y = 1) { // $y 是可选参数，默认值为 1
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result 在函数外部不可访问
// print $result; // 抛出警告

// 从 PHP 5.3 起我们可以定义匿名函数
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// 函数也可以返回一个函数
function bar ($x, $y) {
  // 用 'use' 将外部的参数引入到里面
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // 输出 "A - B - C"

// 你也可以通过字符串调用函数
$function_name = 'add';
echo $function_name(1, 2); // => 3
// 在通过程序来决定调用哪个函数时很有用
// 或者，使用 call_user_func(callable $callback [, $parameter [, ... ]]);

/********************************
 * 导入
 */

<?php
// 被导入的php文件也必须以php开标签开始

include 'my-file.php';
// 现在my-file.php就在当前作用域中可见了
// 如果这个文件无法被导入（比如文件不存在），会抛出警告

include_once 'my-file.php';
// my-file.php中的代码在其他地方被导入了，那么就不会被再次导入
// 这会避免类的多重定义错误

require 'my-file.php';
require_once 'my-file.php';
// 和include功能相同，只不过如果不能被导入时，会抛出错误

// my-include.php的内容:
<?php

return 'Anything you like.';
// 文件结束

// Include和Require函数也有返回值
$value = include 'my-include.php';

// 被引入的文件是根据文件路径或者include_path配置来查找到的
// 如果文件最终没有被找到，那么就会查找当前文件夹。之后才会报错
/* */

/********************************
 * 类
 */

// 类是由class关键字定义的

class MyClass
{
    const MY_CONST      = 'value'; // 常量

    static $staticVar   = 'static';

    // 属性必须声明其作用域
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // 当前类和子类可访问
    private $priv   = 'private';   // 仅当前类可访问

    // 通过 __construct 来定义构造函数
    public function __construct($instanceProp) {
        // 通过 $this 访问当前对象
        $this->instanceProp = $instanceProp;
    }

    // 方法就是类中定义的函数
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

echo MyClass::MY_CONST;    // 输出 'value';
echo MyClass::$staticVar;  // 输出 'static';
MyClass::myStaticMethod(); // 输出 'I am static';

// 通过new来新建实例
$my_class = new MyClass('An instance property');
// 如果不传递参数，那么括号可以省略

// 用 -> 来访问成员
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"


// 使用extends来生成子类
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // 方法覆盖
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => 输出 "protected"
$my_other_class->myMethod();               // 输出 "MyClass > MyOtherClass"

final class YouCannotExtendMe
{
}

// 你可以使用“魔法方法”来生成getter和setter方法
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
echo $x->property; // 会使用 __get() 方法
$x->property = 'Something'; // 会使用 __set() 方法

// 类可以是被定义成抽象类 (使用 abstract 关键字) 或者
// 去实现接口 (使用 implements 关键字).
// 接口需要通过interface关键字来定义

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// 接口可以被扩展
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


// 一个类可以实现多个接口
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
 * 特征
 */

// 特征 从 PHP 5.4.0 开始包括，需要用 "trait" 这个关键字声明

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
$cls->myTraitMethod(); // 输出 "I have MyTrait"


/********************************
 * 命名空间
 */

// 这部分是独立于这个文件的
// 因为命名空间必须在一个文件的开始处。

<?php

// 类会被默认的放在全局命名空间中，可以被一个\来显式调用

$cls = new \MyClass();



// 为一个文件设置一个命名空间
namespace My\Namespace;

class MyClass
{
}

// (或者从其他文件中)
$cls = new My\Namespace\MyClass;

//或者从其他命名空间中
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// 你也可以为命名空间起一个别名

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

*/

```

## 更多阅读

访问 [PHP 官方文档](http://www.php.net/manual/) 

如果你对最佳实践感兴趣（实时更新） [PHP The Right Way](http://www.phptherightway.com/).

如果你很熟悉善于包管理的语言 [Composer](http://getcomposer.org/).

如要了解通用标准，请访问PHP Framework Interoperability Group's [PSR standards](https://github.com/php-fig/fig-standards).
