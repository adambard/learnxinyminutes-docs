---
language: PHP
category: language
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
filename: learnphp-kr.php
translators:
    - ["wikibook", "http://wikibook.co.kr"]
lang: ko-kr
---

이 문서에서는 PHP 5+를 설명합니다.


```php
<?php // PHP 코드는 반드시 <?php 태그로 감싸야 합니다.

// php 파일에 PHP 코드만 들어 있다면 닫는 태그를 생략하는 것이 관례입니다.

// 슬래시 두 개는 한 줄 주석을 의미합니다.

# 해시(파운드 기호로도 알려진)도 같은 역할을 하지만 //이 더 일반적으로 쓰입니다.

/*
     텍스트를 슬래시-별표와 별표-슬래시로 감싸면 
     여러 줄 주석이 만들어집니다.
*/

// 출력결과를 표시하려면 "echo"나 "print"를 사용합니다.
print('Hello '); // 줄바꿈 없이 "Hello "를 출력합니다.

// ()는 print와 echo를 사용할 때 선택적으로 사용할 수 있습니다.
echo "World\n"; // "World"를 출력한 후 줄바꿈합니다.
// (모든 구문은 반드시 세미콜론으로 끝나야 합니다.)

// <?php 태그 밖의 내용은 모두 자동으로 출력됩니다.
?>
Hello World Again!
<?php


/************************************
 * 타입과 변수
 */

// 변수명은 $ 기호로 시작합니다.
// 유효한 변수명은 문자나 밑줄(_)로 시작하고,
// 이어서 임의 개수의 숫자나 문자, 밑줄이 옵니다.

// 불린값은 대소문자를 구분합니다.
$boolean = true;  // 또는 TRUE나 True
$boolean = false; // 또는 FALSE나 False

// Integer
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (a leading 0 denotes an octal number)
$int4 = 0x0F; // => 15 (a leading 0x denotes a hex literal)

// Float (doubles로도 알려짐)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// 산술 연산
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// 축약형 산술 연산
$number = 0;
$number += 1;      // $number를 1만큼 증가
echo $number++;    // 1을 출력(평가 후 증가)
echo ++$number;    // 3 (평가 전 증가)
$number /= $float; // 나눗셈 후 몫을 $number에 할당

// 문자열은 작은따옴표로 감싸야 합니다.
$sgl_quotes = '$String'; // => '$String'

// 다른 변수를 포함할 때를 제외하면 큰따옴표 사용을 자제합니다.
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// 특수 문자는 큰따옴표에서만 이스케이프됩니다.
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// 필요할 경우 변수를 중괄호로 감쌉니다.
$money = "I have $${number} in the bank.";

// PHP 5.3부터는 여러 줄 문자열을 생성하는 데 나우닥(nowdoc)을 사용할 수 있습니다.
$nowdoc = <<<'END'
Multi line
string
END;

// 히어닥(heredoc)에서는 문자열 치환을 지원합니다.
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// 문자열을 연결할 때는 .을 이용합니다.
echo 'This string ' . 'is concatenated';


/********************************
 * 상수
 */
 
// 상수는 define()을 이용해 정의되며,
// 런타임 동안 절대 변경될 수 없습니다!

// 유효한 상수명은 문자나 밑줄로 시작하고,
// 이어서 임의 개수의 숫자나 문자, 밑줄이 옵니다.
define("FOO",     "something");

// 상수명을 이용해 직접 상수에 접근할 수 있습니다.
echo 'This outputs '.FOO;


/********************************
 * 배열
 */

// PHP의 모든 배열은 연관 배열(associative array, 해시맵)입니다.

// 일부 언어에서 해시맵으로도 알려진 연관 배열은

// 모든 PHP 버전에서 동작합니다.
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4에서는 새로운 문법이 도입됐습니다.
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // 1을 출력

// 리스트 리터럴은 암시적으로 정수형 키를 할당합니다.
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"


/********************************
 * 출력
 */

echo('Hello World!');
// 표준출력(stdout)에 Hello World!를 출력합니다.
// 브라우저에서 실행할 경우 표준출력은 웹 페이지입니다.

print('Hello World!'); // echo과 동일

// echo는 실제로 언어 구성물에 해당하므로, 괄호를 생략할 수 있습니다.
echo 'Hello World!';
print 'Hello World!'; // 똑같이 출력됩니다.

$paragraph = 'paragraph';

echo 100;        // 스칼라 변수는 곧바로 출력합니다.
echo $paragraph; // 또는 변수의 값을 출력합니다.

// 축약형 여는 태그를 설정하거나 PHP 버전이 5.4.0 이상이면
// 축약된 echo 문법을 사용할 수 있습니다.
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // 이제 $x의 값은 $y의 값과 같습니다.
$z = &$y;
// $z는 이제 $y에 대한 참조를 담고 있습니다. $z의 값을 변경하면
// $y의 값도 함께 변경되며, 그 반대도 마찬가지입니다.
// $x는 $y의 원래 값을 그대로 유지합니다.

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0


/********************************
 * 로직
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assert는 인자가 참이 아닌 경우 경고를 출력합니다.

// 다음과 같은 비교는 항상 참이며, 타입이 같지 않더라도 마찬가지입니다.
assert($a == $b); // 동일성 검사
assert($c != $a); // 불일치성 검사
assert($c <> $a); // 또 다른 불일치성 검사
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// 다음과 같은 코드는 값과 타입이 모두 일치하는 경우에만 참입니다.
assert($c === $d);
assert($a !== $d);
assert(1 == '1');
assert(1 !== '1');

// 변수는 어떻게 사용하느냐 따라 다른 타입으로 변환될 수 있습니다.

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (문자열이 강제로 정수로 변환됩니다)

$string = 'one';
echo $string + $string; // => 0
// + 연산자는 'one'이라는 문자열을 숫자로 형변환할 수 없기 때문에 0이 출력됩니다.

// 한 변수를 다른 타입으로 처리하는 데 형변환을 사용할 수 있습니다.

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// 대다수의 타입을 형변환하는 데 사용하는 전용 함수도 있습니다.
$integer = 5;
$string = strval($integer);

$var = null; // 널 타입


/********************************
 * 제어 구조
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

// 사항 연산자
print (false ? 'Does not get printed' : 'Does');

$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}



// 다음과 같은 문법은 템플릿에 유용합니다.
?>

<?php if ($x): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>

<?php

// 특정 로직을 표현할 때는 switch를 사용합니다.
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // break을 반드시 포함해야 하며, break를 생략하면
               // 'two'와 'three' 케이스로 넘어갑니다.
    case 'two':
    case 'three':
        // 변수가 'two'나 'three'인 경우에 실행될 코드를 작성합니다.
        break;
    default:
        // 기본값으로 실행될 코드를 작성
}

// while과 do...while, for 문이 아마 더 친숙할 것입니다.
$i = 0;
while ($i < 5) {
    echo $i++;
}; // "01234"를 출력

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // "01234"를 출력

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // "0123456789"를 출력

echo "\n";

$wheels = ['bicycle' => 2, 'car' => 4];

// foreach 문은 배영를 순회할 수 있습니다.
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // "24"를 출력

echo "\n";

// 키와 값을 동시에 순회할 수 있습니다.
foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // while 문을 빠져나옴
    }
    echo $i++;
} // "012"를 출력

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // 이번 순회를 생략
    }
    echo $i;
} // "0124"를 출력


/********************************
 * 함수
 */

// "function"으로 함수를 정의합니다.
function my_function () {
  return 'Hello';
}

echo my_function(); // => "Hello"

// 유효한 함수명은 문자나 밑줄로 시작하고, 이어서 
// 임의 개수의 문자나 숫자, 밑줄이 옵니다.

function add ($x, $y = 1) { // $y는 선택사항이고 기본값은 1입니다.
  $result = $x + $y;
  return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// 함수 밖에서는 $result에 접근할 수 없습니다.
// print $result; // 이 코드를 실행하면 경고가 출력됩니다.

// PHP 5.3부터는 익명 함수를 선언할 수 있습니다.
$inc = function ($x) {
  return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

// 함수에서는 함수를 반환할 수 있습니다.
function bar ($x, $y) {
  // 'use'를 이용해 바깥 함수의 변수를 전달합니다.
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C'); // "A - B - C"를 출력

// 문자열을 이용해 이름이 지정된 함수를 호출할 수 있습니다.
$function_name = 'add';
echo $function_name(1, 2); // => 3
// 프로그램 방식으로 어느 함수를 실행할지 결정할 때 유용합니다.
// 아니면 call_user_func(callable $callback [, $parameter [, ... ]]);를 사용해도 됩니다.

/********************************
 * 인클루드
 */

<?php
// 인클루드된 파일 내의 PHP 코드도 반드시 PHP 여는 태그로 시작해야 합니다.

include 'my-file.php';
// my-file.php 안의 코드는 이제 현재 유효범위에서 이용할 수 있습니다.
// 파일을 인클루드할 수 없으면(예: 파일을 찾을 수 없음) 경고가 출력됩니다.

include_once 'my-file.php';
// my-file.php 안의 코드가 다른 곳에 인클루드됐다면 다시 인클루드되지는 않습니다.
// 따라서 클래스 선언이 여러 번 되어 발생하는 문제가 일어나지 않습니다.

require 'my-file.php';
require_once 'my-file.php';
// require()는 include()와 같지만 파일을 인클루드할 수 없을 경우
// 치명적인 오류가 발생한다는 점이 다릅니다.

// my-include.php의 내용
<?php

return 'Anything you like.';
// 파일의 끝

// include와 require는 값을 반환할 수도 있습니다.
$value = include 'my-include.php';

// 파일은 지정된 파일 경로를 토대로 인클루드되거나, 혹은 아무것도 명시하지 않은 경우
// include_path라는 설정 지시지를 따릅니다. include_path에서 파일을 발견할 수 없으면
// include는 마지막으로 실패하기 전에 호출 스크립트 자체의 디렉터리와 현재 작업 디렉터리를 확인합니다.
/* */

/********************************
 * 클래스
 */

// 클래스는 class라는 키워드로 정의합니다.

class MyClass
{
    const MY_CONST      = 'value'; // 상수

    static $staticVar   = 'static';

    // 프로퍼티에는 반드시 가시성을 선언해야 합니다.
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // 이 클래스와 하위 클래스에서 접근할 수 있음
    private $priv   = 'private';   // 이 클래스 내에서만 접근 가능

    // __construct로 생성자를 만듭니다.
    public function __construct($instanceProp) {
        // $this로 인스턴스 변수에 접근합니다.
        $this->instanceProp = $instanceProp;
    }

    // 메서드는 클래스 안의 함수로서 선언됩니다.
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

echo MyClass::MY_CONST;    // 'value' 출력
echo MyClass::$staticVar;  // 'static' 출력
MyClass::myStaticMethod(); // 'I am static' 출력

// new를 사용해 클래스를 인스턴스화합니다.
$my_class = new MyClass('An instance property');
// 인자를 전달하지 않을 경우 괄호를 생략할 수 있습니다.

// ->를 이용해 클래스 멤버에 접근합니다
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"


// "extends"를 이용해 클래스를 확장합니다.
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // 메서드 재정의
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => "protected" 출력
$my_other_class->myMethod();               // "MyClass > MyOtherClass" 출력

final class YouCannotExtendMe
{
}

// "마법 메서드(magic method)"로 설정자 메서드와 접근자 메서드를 만들 수 있습니다.
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
echo $x->property; // __get() 메서드를 사용
$x->property = 'Something'; // __set() 메서드를 사용

// 클래스는 추상화하거나(abstract 키워드를 사용해) 
// 인터페이스를 구현할 수 있습니다(implments 키워드를 사용해).
// 인터페이스는 interface 키워드로 선언합니다.

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// 인터페이스는 확장할 수 있습니다.
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


// 클래스에서는 하나 이상의 인터페이스를 구현할 수 있습니다.
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
 * 특성
 */

// 특성(trait)은 PHP 5.4.0부터 사용 가능하며, "trait"으로 선언합니다.

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
$cls->myTraitMethod(); // "I have MyTrait"을 출력


/********************************
 * 네임스페이스
 */

// 이 부분은 별도의 영역인데, 파일에서 처음으로 나타나는 문장은 
// 네임스페이스 선언이어야 하기 때문입니다. 여기서는 그런 경우가 아니라고 가정합니다.

<?php

// 기본적으로 클래스는 전역 네임스페이스에 존재하며,
// 백슬래시를 이용해 명시적으로 호출할 수 있습니다.

$cls = new \MyClass();



// 파일에 대한 네임스페이스를 설정합니다.
namespace My\Namespace;

class MyClass
{
}

// (다른 파일에 들어 있는 코드)
$cls = new My\Namespace\MyClass;

// 또는 다른 네임스페이스 내에서 접근하는 경우
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// 혹은 네임스페이스에 별칭을 붙일 수도 있습니다.

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

*/

```

## 더 자세한 정보

레퍼런스와 커뮤니티 관련 내용은 [공식 PHP 문서](http://www.php.net/manual/)를 참고하세요.

최신 모범 사례에 관심이 있다면 [PHP The Right Way](http://www.phptherightway.com/)를 참고하세요.

PHP를 익히기 전에 다른 훌륭한 패키지 관리자를 지원하는 언어를 사용해본 적이 있다면 [컴포저(Composer)](http://getcomposer.org/)를 확인해 보세요.

공통 표준이 궁금하다면 PHP 프레임워크 상호운용성 그룹의 [PSR 표준](https://github.com/php-fig/fig-standards)을 참고하세요.
