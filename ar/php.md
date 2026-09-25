---
name: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
filename: learnphp.php
translators:
    - ["Learn X in Y Minutes (ar)", "https://github.com/adambard/learnxinyminutes-docs"]
---

<p dir="rtl">يصف هذا المستند PHP إصدار 5 فما فوق.</p>

```php
<?php // يجب أن تُحاط شيفرة PHP بوسوم <?php

// إن كان الملف يحتوي شيفرة PHP فقط، يُستحسن حذف وسم الإغلاق لتفادي إخراج غير مقصود.

// الشرطتان // تبدآن تعليقاً سطراً واحداً.

# الرمز # أيضاً، لكن // أشيع

/*
     النص بين علامتي بدء وإنهاء التعليق المتعدد يُعرَف كتعليق متعدد الأسطر.
*/

// استخدم echo أو print للإخراج
print('Hello '); // يطبع "Hello " بلا سطر جديد

// الأقواس () اختيارية لـ print و echo
echo "World\n"; // يطبع "World" مع سطر جديد
// (كل الجمل تنتهي بفاصلة منقوطة)

// أي نص خارج وسوم <?php يُصدَر تلقائياً
?>
Hello World Again!
<?php
// لأن PHP بدأ تاريخياً كمحرك قوالب


/************************************
 * الأنواع والمتغيرات
 */

// المتغيرات تبدأ بـ $
// اسم صالح يبدأ بحرف أو شرطة سفلية ثم أحرف أو أرقام أو شرطات

// لا تُصرَّح بالمتغيرات (ولا يمكن)
// عند التعيين يُنشأ المتغير بالنوع المناسب

// القيم المنطقية غير حساسة لحالة الأحرف
$boolean = true;  // أو TRUE أو True
$boolean = FALSE; // أو false أو False

// الأعداد الصحيحة
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (0 في البداية = ثماني)
$int4 = 0x0F; // => 15 (0x = سداسي عشري)
// الأعداد الثنائية منذ PHP 5.4.0
$int5 = 0b11111111; // 255 (0b = ثنائي)

// الأعداد العشرية (double)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// حذف متغير
unset($int1);

// الحساب
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// اختصارات حسابية
$number = 0;
$number += 1;      // زيادة $number بمقدار 1
echo $number++;    // يطبع 1 (يزيد بعد التقييم)
echo ++$number;    // يطبع 3 (يزيد قبل التقييم)
$number /= $float; // قسمة وتعيين الناتج لـ $number

// النصوص بين علامتي اقتباس مفردة
$sgl_quotes = '$String'; // => '$String'

// تجنّب الاقتباس المزدوج إلا لدمج متغيرات
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// المحارف الخاصة تُهرب فقط في الاقتباس المزدوج
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// أحيط المتغير بأقواس معقوفة عند الحاجة
$number = 23;
$apples = "I have {$number} apples to eat.";   // => I have 23 apples to eat.
$oranges = "I have ${number} oranges to eat."; // => I have 23 oranges to eat.
$money = "I have $${number} in the bank.";     // => I have $23 in the bank.

// منذ PHP 5.3: nowdoc لنصوص متعددة الأسطر بلا استيفاء
$nowdoc = <<<'END'
Multi line
string
END;

// Heredoc يدعم استيفاء المتغيرات في النص
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// دمج النصوص بالنقطة .
echo 'This string ' . 'is concatenated';  // يعيد 'This string is concatenated'

// يمكن تمرير عدة معاملات لـ echo
echo 'Multiple', 'Parameters', 'Valid';  // يعيد 'MultipleParametersValid'


/********************************
 * الثوابت
 */

// الثابت يُعرَّف بـ define()
// ولا يتغيّر أثناء التشغيل!

// اسم ثابت صالح يبدأ بحرف أو _ ثم أحرف أو أرقام أو _
define("FOO", "something");

// الوصول بدون $
echo FOO; // يعيد 'something'
echo 'This outputs ' . FOO;  // يعيد 'This outputs something'



/********************************
 * المصفوفات
 */

// كل المصفوفات في PHP ترابطية (مثل الخرائط)

// يعمل مع كل إصدارات PHP
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// صياغة جديدة منذ PHP 5.4
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // prints 1

// إضافة عنصر لمصفوفة ترابطية
$associative['Four'] = 4;

// القوائم الحرفية تعطي مفاتيحاً صحيحة تلقائياً
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"

// إضافة في نهاية المصفوفة
$array[] = 'Four';
// أو
array_push($array, 'Five');

// إزالة عنصر
unset($array[3]);

/********************************
 * الإخراج
 */

echo('Hello World!');
// يطبع إلى stdout
// في المتصفح stdout هو الصفحة

print('Hello World!'); // مثل echo

// echo و print بنى لغة؛ الأقواس اختيارية
echo 'Hello World!';
print 'Hello World!';

$paragraph = 'paragraph';

echo 100;        // قيم قياسية مباشرة
echo $paragraph; // أو متغيرات

// مع تفعيل الوسوم القصيرة أو PHP ≥ 5.4 يمكن اختصار الإخراج
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x يساوي قيمة $y الآن
$z = &$y;
// $z مرجع لـ $y؛ تغيير أحدهما يغيّر الآخر
// $x يبقى كما كان عند نسخ قيمة $y الأصلية

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// يطبع النوع والقيمة
var_dump($z); // prints int(0)

// طباعة مقروءة
print_r($array); // prints: Array ( [0] => One [1] => Two [2] => Three )

/********************************
 * المنطق
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert يُصدِر تحذيراً إن لم يكن المعطى صحيحاً

// هذه المقارنات صحيحة حتى مع اختلاف الأنواع
assert($a == $b); // مساواة
assert($c != $a); // عدم مساواة
assert($c <> $a); // صياغة بديلة
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// التالي صحيح فقط إن تطابقت القيم والأنواع
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// عامل المركبة الفضائية <=> منذ PHP 7
// 0 إن تساويا، 1 إن الأيسر أكبر، -1 إن الأيمن أكبر

$a = 100;
$b = 1000;

echo $a <=> $a; // 0 متساويان
echo $a <=> $b; // -1 لأن $a أصغر
echo $b <=> $a; // 1 لأن $b أكبر

// تحويل الأنواع حسب السياق

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (تحويل نصوص لأعداد)

$string = 'one';
echo $string + $string; // => 0
// 0 لأن + لا يحوّل 'one' إلى رقم

// يمكن الإجبار النوعي

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// دوال تحويل لمعظم الأنواع
$integer = 5;
$string = strval($integer);

$var = null; // قيمة فارغة


/********************************
 * هياكل التحكم
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
} elseif (true) {
    print 'Does';
}

// المشغل الثلاثي
print (false ? 'Does not get printed' : 'Does');

// اختصار Elvis منذ PHP 5.3
// يعادل "$x ? $x : 'Does'"
$x = false;
print($x ?: 'Does');

// دمج null ?? منذ PHP 7
$a = null;
$b = 'Does print';
echo $a ?? 'a is not set'; // prints 'a is not set'
echo $b ?? 'b is not set'; // prints 'Does print'


$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif ($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}



// صياغة بديلة مفيدة للقوالب:
?>

<?php if ($x): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>

<?php

// switch يوفّر فروعاً متعددة
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // يجب break وإلا «يسقط» للفروع التالية
    case 'two':
    case 'three':
        // نفّذ شيئاً إن كانت القيمة two أو three
        break;
    default:
        // الحالة الافتراضية
}

// while و do...while و for كالعادة
$i = 0;
while ($i < 5) {
    echo $i++;
} // يطبع "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // يطبع "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // يطبع "0123456789"

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// foreach يمرّ على المصفوفات
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // يطبع "24"

echo "\n";

// المفتاح والقيمة معاً
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // خروج من while
    }
    echo $i++;
} // يطبع "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // تخطّ هذه الدورة
    }
    echo $i;
} // يطبع "0124"


/********************************
 * الدوال
 */

// التعريف بكلمة function
function my_function () {
    return 'Hello';
}

echo my_function(); // => "Hello"

// اسم دالة صالح يبدأ بحرف أو _ ثم أحرف أو أرقام أو _

function add ($x, $y = 1) { // $y اختياري افتراضه 1
    $result = $x + $y;
    return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result غير مرئي خارج الدالة
// print $result; // تحذير

// دوال مجهولة منذ PHP 5.3
$inc = function ($x) {
    return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
    echo "$x - $y - $z";
}

// دالة يمكنها إرجاع دالة
function bar ($x, $y) {
    // use لالتقاط متغيرات خارجية
    return function ($z) use ($x, $y) {
        foo($x, $y, $z);
    };
}

$bar = bar('A', 'B');
$bar('C'); // يطبع "A - B - C"

// استدعاء دالة باسمها كنص
$function_name = 'add';
echo $function_name(1, 2); // => 3
// مفيد لاختيار الدالة برمجياً
// أو call_user_func


// جمع كل المعاملات الممررة
function parameters() {
    $numargs = func_num_args();
    if ($numargs > 0) {
        echo func_get_arg(0) . ' | ';
    }
    $args_array = func_get_args();
    foreach ($args_array as $key => $arg) {
        echo $key . ' - ' . $arg . ' | ';
    }
}

parameters('Hello', 'World'); // Hello | 0 - Hello | 1 - World |

// عدد معاملات متغير منذ PHP 5.6
function variable($word, ...$list) {
	echo $word . " || ";
	foreach ($list as $item) {
		echo $item . ' | ';
	}
}

variable("Separate", "Hello", "World"); // Separate || Hello | World |

/********************************
 * التضمين
 */

<?php
// الملفات المضمّنة تبدأ أيضاً بوسم PHP

include 'my-file.php';
// شيفرة my-file.php متاحة في النطاق الحالي
// إن فشل التضمين يُصدَر تحذير

include_once 'my-file.php';
// لا يُعاد التضمين إن وُجد مسبقاً — يمنع إعادة تعريف الأصناف

require 'my-file.php';
require_once 'my-file.php';
// مثل include لكن require يُسبِب خطأ قاتلاً إن فشل الملف

// محتوى my-include.php:
<?php

return 'Anything you like.';
// نهاية الملف

// التضمين قد يُرجع قيمة
$value = include 'my-include.php';

// البحث عن الملف حسب المسار أو include_path ثم مجلد السكربت
/* */

/********************************
 * الأصناف
 */

// الأصناف بكلمة class

class MyClass
{
    const MY_CONST      = 'value'; // ثابت صنف

    static $staticVar   = 'static';

    // أعضاء static وإظهارهم
    public static $publicStaticVar = 'publicStatic';
    // داخل الصنف فقط
    private static $privateStaticVar = 'privateStatic';
    // الصنف والفروع
    protected static $protectedStaticVar = 'protectedStatic';

    // الخصائص تُصرَّح بمستوى الإظهار
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // الصنف والفروع
    private $priv   = 'private';   // داخل الصنف فقط

    // الباني __construct
    public function __construct($instanceProp)
    {
        // الوصول لخصائص المثيل بـ $this
        $this->instanceProp = $instanceProp;
    }

    // الدوال دوال داخل الصنف
    public function myMethod()
    {
        print 'MyClass';
    }

    // final تمنع إعادة التعريف في الفرع
    final function youCannotOverrideMe()
    {
    }

    // دوال سحرية

    // عند معاملة الكائن كنص
    public function __toString()
    {
        return $property;
    }

    // عكس __construct — عند زوال المرجع
    public function __destruct()
    {
        print "Destroying";
    }

/*
 * static يجعل الخاصية أو الدالة قابلة للوصول بلا إنشاء مثيل.
 * خاصية static لا تُستعمَل عبر كائن، لكن دالة static يمكن.
 */

    public static function myStaticMethod()
    {
        print 'I am static';
    }
}

// ثوابت الصنف تُستدعى بـ ::
echo MyClass::MY_CONST;    // يطبع 'value'

echo MyClass::$staticVar;  // يطبع 'static'
MyClass::myStaticMethod(); // يطبع 'I am static'

// إنشاء بـ new
$my_class = new MyClass('An instance property');
// الأقواس اختيارية بلا معاملات

// الأعضاء بـ ->
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"

// ?-> منذ PHP 8 عند الشك في وجود خاصية/دالة
echo $my_class->invalid_property // يُرمى خطأ
echo $my_class?->invalid_property // => NULL
echo $my_class?->invalid_property ?? "public" // => "public"

// الوراثة بـ extends
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // إعادة تعريف دالة
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => Prints "protected"
$my_other_class->myMethod();               // يطبع "MyClass > MyOtherClass"

final class YouCannotExtendMe
{
}

// دوال سحرية للوصول للخصائص
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
echo $x->property; // يستدعي __get()
$x->property = 'Something'; // يستدعي __set()

// أصناف abstract أو تنفّذ واجهات implements
// الواجهة بكلمة interface

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// الواجهات تورث واجهات
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


// تنفيذ أكثر من واجهة
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
 * السمات (Traits)
 */

// Traits منذ PHP 5.4 بكلمة trait

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
$cls->myTraitMethod(); // يطبع "I have MyTrait"


/********************************
 * المساحات الاسمية
 */

// تعريف namespace يجب أن يكون أول جملة في الملف — هنا نفترض خلاف ذلك

<?php

// افتراضياً الأصناف في المساحة العامة، تُستدعى بـ \

$cls = new \MyClass();



// مساحة اسم للملف
namespace My\Namespace;

class MyClass
{
}

// (من ملف آخر)
$cls = new My\Namespace\MyClass;

// أو من مساحة اسم أخرى
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// أو اسم مستعار للمساحة

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();


/**********************
* الربط الثابت المتأخر (Late Static Binding)
*
*/

class ParentClass
{
    public static function who()
    {
        echo "I'm a " . __CLASS__ . "\n";
    }

    public static function test()
    {
        // self = الصنف حيث عُرِفت الدالة
        self::who();
        // static = الصنف الذي نُودِعَت عليه الدالة
        static::who();
    }
}

ParentClass::test();
/*
I'm a ParentClass
I'm a ParentClass
*/

class ChildClass extends ParentClass
{
    public static function who()
    {
        echo "But I'm " . __CLASS__ . "\n";
    }
}

ChildClass::test();
/*
I'm a ParentClass
But I'm ChildClass
*/

/**********************
*  ثوابت سحرية
*
*/

// اسم الصنف الحالي — داخل تعريف صنف
echo "Current class name is " . __CLASS__;

// المسار الكامل لمجلد الملف
echo "Current directory is " . __DIR__;

    // شائع مع autoload
    require __DIR__ . '/vendor/autoload.php';

// المسار الكامل للملف
echo "Current file path is " . __FILE__;

// اسم الدالة الحالية
echo "Current function name is " . __FUNCTION__;

// رقم السطر الحالي
echo "Current line number is " . __LINE__;

// اسم الدالة الحالية — داخل صنف أو سمة
echo "Current method is " . __METHOD__;

// المساحة الاسمية الحالية
echo "Current namespace is " . __NAMESPACE__;

// اسم السمة الحالية — داخل سمة أو صنف
echo "Current trait is " . __TRAIT__;

/**********************
*  معالجة الأخطاء
*
*/

// try / catch بسيط

try {
    // شيفرة قد تُرمي استثناءاً
} catch (Exception $e) {
    // معالجة الاستثناء
}

// في مساحة اسم: Exception في المساحة العامة — استخدم \Exception

try {
    // شيفرة قد تُرمي استثناءاً
} catch (\Exception $e) {
    // معالجة الاستثناء
}

// استثناءات مخصصة

class MyException extends Exception {}

try {

    $condition = true;

    if ($condition) {
        throw new MyException('Something just happened');
    }

} catch (MyException $e) {
    // معالجة الاستثناء المخصص
}
```

<h2 dir="rtl">مزيد من المعلومات</h2>

<p dir="rtl">راجع <a href="http://www.php.net/manual/">التوثيق الرسمي لـ PHP</a> والمساهمات المجتمعية.</p>

<p dir="rtl">لممارسات حديثة: <a href="http://www.phptherightway.com/">PHP The Right Way</a>.</p>

<p dir="rtl">دورة أساسيات ولغة وبيئة ومشاريع عملية: <a href="https://www.youtube.com/playlist?list=PLfdtiltiRHWHjTPiFDRdTOPtSyYfz3iLW">Codecourse — PHP Basics</a>.</p>

<p dir="rtl">لإدارة الحزم: <a href="http://getcomposer.org/">Composer</a>.</p>

<p dir="rtl">معايير مشتركة: <a href="https://github.com/php-fig/fig-standards">PSR</a> لمجموعة PHP-FIG.</p>
