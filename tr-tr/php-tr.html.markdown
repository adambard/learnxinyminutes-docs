---
language: PHP
filename: learnphp-tr.php
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Haydar KULEKCI", "http://scanf.info/"]
lang: tr-tr
---

PHP 5+ versiyonu için geçerlidir.

```php
<?php // PHP kodları <?php etiketleri içerisinde bulunmalıdır.

// Eğer php dosyanız sadece PHP kodu içeriyorsa, php'nin kapatma 
// etiketini kapatmayabilirsiniz. 

// // işareti ile tek satırlık yorum satırı başlar.

# # işareti de aynı görevi görür ancak // daha genel kullanımdadır.



/*
    Çoklu satır kodu bu şekilde yazıyoruz. slash yıldız ile başlar 
    ve yıldız slash ile biter.
*/

// "echo" ya da "print" metodları çıktı almak için kullanılır.
print('Hello '); // Ekrana Yeni satır karakteri olmadan "Hello " 
                 // yazdırılır.

// () parantezler "echo" ve "print" metodları için isteğe bağlıdır. 
echo "World\n"; // Ekrana yeni satır karakteri olmadan "World"
                // yazdırılır.
// (Bütün ifadeler noktalı virgül ile bitmelidir.)

// <?php tagı dışarısındaki herşey otomatik olarak yazdırılacaktır.
?>
Hello World Again!
<?php


/************************************
*   Tipler ve Değişkenler
*************************************/

// Değişkenler $ sembolü ile başlar.
// Geçerli bir değişken bir harf veya alt çizgi ile başlar,
// devamında da bir sayı, harf veya alt çizgi ile devam eder. 

// Mantıksal değerler 
// Boolean values are harf büyüklüğüne duyarsızdır.
$boolean = true;  // veya TRUE veya True
$boolean = false; // veya FALSE veya False

// Tam Sayılar
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (öneki 0 olan sekizlik(octal) bir sayıyı gösterir)
$int4 = 0x0F; // => 15 (öneki 0x olanlar hex sayıları gösterir.)

// Kayan Noktalı Sayılar
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Değişken Silmek
unset($int1)

// Aritmetik
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// Aritmetik Kısayolları
$number = 0;
$number += 1;      // $number değişkeninin değerini 1 artırır.
echo $number++;    // 1 yazdırılır. (Yazdırıldıktan sonra artırılır.)
echo ++$number;    // 3 yazdırılır. (yazdırılmadan önce artırılır.)
$number /= $float; // Bölünür ve bölüm $number değerine eşitlenir.

// Karakter dizileri(strings) tek tırnak ile kullanılmalıdır.
$sgl_quotes = '$String'; // => '$String'

// Bir değişken içermediği sürece çift tırnak kullanmaktan kaçının
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// Özel karakterler sadece çift tırnak ile kullanılabilir.
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// Gerekirse bir değişkeni küme ayracı içine alın.
$money = "I have $${number} in the bank.";

// Since PHP 5.3, nowdocs can be used for uninterpolated multi-liners
$nowdoc = <<<'END'
Multi line
string
END;

// Heredocs will do string interpolation
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// . işareti ile karakter dizileri birbirine birleştirilebilir.
echo 'This string ' . 'is concatenated';


/********************************
 * Sabitler
 */

// Bir sabit define() metodu kullanılarak tanımlanır.
// ve çalışma zamanından hiçbir zaman değişmez!

// geçerli bir sabit bir harf veya altçizgi ile başlar,
// ve bir sayı, harf ya da altçizgi ile devam eder.
define("FOO",     "something");

// sabite ulaşmak için direk olarak seçilen ismi kullanabilirsiniz. 
echo 'This outputs '.FOO;


/********************************
 * Diziler
 */

// PHP'de bütün diziler ilişikilendirilebilirdir. (hashmaps),
// İlişkilendirilebilir(associative) diziler, hashmap olarak bilinir. 

// PHP'nin bütün versiyonları için çalışır
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4 ile yeni bir söz dizimi kullanılmaya başlandı
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // 1 yazdıracaktır.

// Liste kullanımında index'ler tam sayıdır.
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"

// Dizinin sonuna bir eleman ekleme
$array[] = 'Four';

// Diziden eleman silme
unset($array[3]);

/********************************
 * Çıktı
 */

echo('Hello World!');
// Hello World! çıktısı stdout'a yazdırılır.
// Eğer bir web browser ile çalışıyorsanır Stdout bir web sayfasıdır.

print('Hello World!'); // echo ile aynıdır.

// Aslında echo bir dil sabitidir, parantezleri kullanmayabilirsiniz. 
echo 'Hello World!';
print 'Hello World!'; // Bu yazdırılacaktır. 

$paragraph = 'paragraph';

echo 100;        // Echo ile doğrudan sayısal değer kullanımı
echo $paragraph; // veya değişken

// PHP 5.4.0 veya daha üstü sürümlerde kısa açılış etiketi 
// konfigürasyonları yapıldı ise, kısa açılış etiketini kullanabilirsiniz.
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // Şu anda $x değişkeni $y ile aynı değere sahiptir.
$z = &$y;
// $z, $y'nin referansını içermektedir. 
// $z'nin değerinin değişmesi $y'nin değerinide değiştireceltir veya
// tam tersi. Ancak $x özgün değeri olarak kalacaktır.

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Dump'lar değişkenin tipi ve değerini yazdırır
var_dump($z); // int(0) yazdırılacaktır

// Print'ler ise değişkeni okunabilir bir formatta yazdıracaktır. 
print_r($array); // Çıktı: Array ( [0] => One [1] => Two [2] => Three )


/********************************
 * Mantık
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// Argüman doğru değilse bir hata fırlatılacaktır.

// Bu karşılaştırmalar tipler aynı olmasa bile her zaman true olacaktır.
assert($a == $b); // equality
assert($c != $a); // inequality
assert($c <> $a); // alternative inequality
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// Aşağıdakiler yanlızca değer ve tip aynı olduğunda true olacaktır.
assert($c === $d);
assert($a !== $d);
assert(1 == '1');
assert(1 !== '1');

// Değişkenler kullanıma bağlı olarak farklı tiplere çevrilebilir.

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (Karakter dizisi tam sayıya çevrilmeye zorlanır)

$string = 'one';
echo $string + $string; // => 0
// Çıktı 0 olacaktır, çünkü + operatörü karakter dizisi olan 'one' değerini
// bir sayıya çeviremez.

// Veri tipi çevirileri bir değişkeni farklı bir türde 
// düzenlemek için kullanılabilir.

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// Veri tiplerini çevirmek için bazı fonksiyonlar vardır.
$integer = 5;
$string = strval($integer);

$var = null; // Null değeri.


/********************************
 * Kontrol Yapıları
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

// Üçlü operatör
print (false ? 'Does not get printed' : 'Does');

$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}



// Bu alternatif sözdizimi template'ler için kullanışlıdır.
?>

<?php if ($x): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>

<?php

// Use switch to save some logic.
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // Bir breake yazmalısınız ya da 'two' ve 'three'
               // durumunuda kapsarsınız.
    case 'two':
    case 'three':
        // $variable değişkeni 'two' ya da 'three' ise 
        break;
    default:
        // Varsayılan olarak bir şey yap
}

// While, do...while ve for döngüleri tanıdıktır.
$i = 0;
while ($i < 5) {
    echo $i++;
}; // "01234" yazdırılacaktır

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // "01234" yazdırılacaktır.

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // "0123456789" yazdırılacaktır.

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// Foreach döngüsü diziler üzerinde çalışır
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // "24" yazdırılacaktır.

echo "\n";

// Key-Value değerlere ulaşabilirsiniz.
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // while döngüsünden çıkar
    }
    echo $i++;
} // Prints "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Aktif döngüyü atlar 
    }
    echo $i;
} // "0124" yazdırılacaktır.



/********************************
 * Fonksiyonlar
 */

// Bir fonksiyon tanımlamak için "function" kullanılır:
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"

// Geçerli bir fonksiyon ismi bir harf veya altçizgi ile başlar ve
// bir sayı, harf ya da alt çizgi ile devam eder.

function add ($x, $y = 1) { // $y değeri isteğe bağlıdır ve 
                            // varsayılan değeri 1'dir
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result fonksiyon dışında ulaşılabilir değildir. 
// print $result; // Bir uyarı verecektir.

// PHP 5.3'den beri bir anonymous fonksiyon tanımlayabilirsiniz;
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// Fonksiyonlar bir fonksiyon dönebilir.
function bar ($x, $y) {
  // Fonksiyona dışardan değişken gönderebilmek için 'use' komutunu kullanın.
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // "A - B - C" yazdırılacaktır.

// Fonksiyonun ismini karakter dizinden çağırabilirsiniz. 
$function_name = 'add';
echo $function_name(1, 2); // => 3
// Programatik olarak fonksiyonları çalıştırmak için yararlı olabilir
// veya, call_user_func(callable $callback [, $parameter [, ... ]]); kulanın.


/********************************
 * Includes
 */

<?php
// PHP'de include edilecek dosyalar PHP açma etiketi ile başlamalı. (!)

include 'my-file.php';
// my-file.php dosyasındaki kodlar artık mevcut scope'da kullanılabilir.
// Eğer dosya include edilemezse bir uyarı (örneğin: file not found) 
// fırlatılacaktır.

include_once 'my-file.php';
// Eğer bir dosya include edildi ise tekrar include edilmeyecektir.
// Bu çoklu class tanımlama hatalarını önler.

require 'my-file.php';
require_once 'my-file.php';
// Dosya include edilemediğinde fatal error veren require() bu konu 
// dışında include() ile aynıdır.

// my-include.php dosyasının içeriği:
<?php

return 'Anything you like.';
// Dosya sonu

// Include'lar ver require'lar aynı zamanda bir return dönebilir.
$value = include 'my-include.php';

// Dosyalar verilen dosya yoluna göre include edilir veya, hiçbirşey 
// verilmezse include_path konfigürasyonuna göre include edecektir. 
// Eğer dosya include_path'da da bulunamazsa include hata vermeden 
// önce içinde bulunulan dizini kontrol ecektir.
/* */


/********************************
 * Sınıflar
 */

// Sınıflar class kelimesi ile tanımlanır

class MyClass
{
    const MY_CONST      = 'value'; // Bir sabit

    static $staticVar   = 'static';

    // Static değişkenler ve onların görünürlüğü
    public static $publicStaticVar = 'publicStatic';
    private static $privateStaticVar = 'privateStatic'; 
        // Sadece bu sınıf içerisinde erişilebilir
    protected static $protectedStaticVar = 'protectedStatic'; 
        // Bu sınıf ve alt sınıflarından erişilebilir

    // Özellikler görünürlüğü ile birlikte tanımlanmalıdır.
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // Sınıf ve alt sınıflardan erişilebilir
    private $priv   = 'private';   // Sadece bu sınıftan erişilebilir

    // __construct ile bir constructor oluşturulabilir.
    public function __construct($instanceProp) {
        // $this ile instance değişkenine erişilir.
        $this->instanceProp = $instanceProp;
    }

    // Sınıfın içerisinde metodlar fonksiyonlar gibi tanımlanır.
    public function myMethod()
    {
        print 'MyClass';
    }

    //final anahtar kelimesi bu metodu override edilemez yapacaktır.
    final function youCannotOverrideMe()
    {
    }

/*
Bir sınıfın özelliğini ya da metodunu statik yaptığınız takdirde sınıfın bir 
objesini oluşturmadan bu elemana erişebilirsiniz. Bir özellik statik tanımlanmış 
ise obje üzerinden bu elemana erişilemez. (Statik metodlar öyle değildir.)
*/


    public static function myStaticMethod()
    {
        print 'I am static';
    }
}

echo MyClass::MY_CONST;    // 'value' şeklinde çıktı verir;
echo MyClass::$staticVar;  // 'static' şeklinde çıktı verir;
MyClass::myStaticMethod(); // 'I am static' şeklinde çıktı verir;

// Sınıfların new ile kullanımı
$my_class = new MyClass('An instance property');
// Eğer argüman göndermeyeceksek parantezler isteğe bağlıdır.

// Sınıfın üyelerine erişim ->
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"


// "extends" ile sınıfı extend etmek
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // Bir methodu ezmek
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // "protected" şeklinde çıktı verir.
$my_other_class->myMethod();    // "MyClass > MyOtherClass" şeklinde çıktı verir

final class YouCannotExtendMe
{
}

// getter ve setter'ları oluşturmak için "magic method"ları kullanabilirsiniz.
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
echo $x->property; // __get() metodunu kullanacaktır.
$x->property = 'Something'; // __set() metodunu kullanacaktır. 

// Sınıflar abstract olabilir(abstract kelimesini kullanarak) veya
// interface'ler uygulanabilir (implements kelimesi kullanılarak).
// Bir interface "interface" kelimesi kullanılarak oluşturulur.

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// interfaces can be extended
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


// Sınıflar birden fazla interface kullanabilir.
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
 * Traits
 */
// Trait'ler PHP 5.4.0'dan beri kullanılabilir ve "trait" kullanılarak 
// tanımlanır.

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
$cls->myTraitMethod(); // "I have MyTrait" çıktısını verir.



/********************************
 * İsim Uzayları
 */

// Bu alan ayrılmıştır, çünkü isim uzayı tanımı bir dosyada en başta 
// yapılmalıdır. Bu örnekte böyle olmadığını varsayalım.

<?php

// Varsayılan olarak, sınıflar global isim uzayındadır, ve açıkça bir 
// ters slash ile çağrılabilir.

$cls = new \MyClass();



// Bir dosya için isim uzayı atama
namespace My\Namespace;

class MyClass
{
}

// (diğer bir dosya)
$cls = new My\Namespace\MyClass;

//veya diğer bir isim uzayında.
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// veya isim uzayına bir takma isim koyabilirsiniz.

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

*/

```

## Daha fazla bilgi

Referans ve topluluk yazıları için [official PHP documentation](http://www.php.net/manual/) adresini ziyaret edin.

Güncel en yi örnekler için [PHP Usulüne Uygun](http://kulekci.net/php-the-right-way/) adresini ziyaret edin.

Eğer bir paket yöneticisi olan dil kullandıysanız, [Composer](http://getcomposer.org/)'a bir göz atın.

Genel standartlar için PHP Framework Interoperability Group'unun [PSR standards](https://github.com/php-fig/fig-standards) ziyaret edebilirsiniz. 

