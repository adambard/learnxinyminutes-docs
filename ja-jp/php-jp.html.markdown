---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
translators:
    - ["Kazushige Tominaga", "https://github.com/kazu9su"]
filename: learnphp-jp.php
lang: ja-jp
---

このドキュメントでは、 PHP 5+ について説明します。

```php
<?php // PHPのコードは、<?php タグで囲む必要があります。

// もしあなたのphpファイルがPHPのみで構成される場合、
// 意図しない出力を防ぐために、phpの閉じタグは省略するのがベストプラクティスです。
// 一行のコメントを書く場合、2つのスラッシュで始めます。
# ハッシュ(ポンド記号として知られる)を使いたいでしょうが、//のほうが一般的です
/*
    スラッシュとアスタリスク、アスタリスクとスラッシュで囲むと、
    複数行のコメントを書けます。
*/

// 出力をプリントしたい場合は、"echo" か "print" を使います。
print('Hello '); // これは "Hello " という改行なしの文字列をプリントします。

// カッコ()はprintとecho関数に置いては省略できます。
echo "World\n"; // これは改行ありの"World"という文字列をプリントします。
// (全ての命令文では、セミコロンを最後に付けることが必須です。)

// <?php タグの外側にあるものは、自動的にプリントされます。
?>
Hello World Again!
<?php


/************************************
 * 型と変数について
 */

// 変数は"$"マークで始まります
// 有効な変数名にするには、文字またはアンダースコア(_)で始めて,
// その後はどんな数字でも、文字でも、アンダースコアで続けても構いません

//ブーリアン値は大文字、小文字問いません
$boolean = true;  // or TRUE or True
$boolean = false; // or FALSE or False

// 数値
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (先頭の0は8進法を示す)
$int4 = 0x0F; // => 15 (先頭の0xは16進法を示す)

// floats(浮動小数) (別名double)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// 変数の削除
unset($int1);

// 計算式
$sum        = 1 + 1; // 2
$difference = 2 - 1; // 1
$product    = 2 * 2; // 4
$quotient   = 2 / 1; // 2

// 式の省略
$number = 0;
$number += 1;      // $numberに1加算Increment $number by 1
echo $number++;    // 1 がプリントされる(式の評価の後に加算される)
echo ++$number;    // 3 がプリントされる(式の評価の前に加算される)
$number /= $float; // 割り算した結果の商を$numberに割り当てる

// 文字列はシングルクォートで囲むのが望ましいです
$sgl_quotes = '$String'; // => '$String'

// 文字列中に、他の変数を埋め込みたい場合以外は、ダブルクォートを使用するのはやめましょう
$dbl_quotes = "This is a $sgl_quotes."; // => 'This is a $String.'

// Special characters are only escaped in double quotes
// 特殊文字はダブルクォートによってのみ、エスケープされます
$escaped   = "This contains a \t tab character.";
$unescaped = 'This just contains a slash and a t: \t';

// 必要があれば、変数を波括弧で囲みます
$money = "I have $${number} in the bank.";

// PHP 5.3から、nowdocs形式が変数の挿入をしない複数行の文字列の定義に使用できます
$nowdoc = <<<'END'
Multi line
string
END;

// ヒアドキュメント形式なら、文字列中に変数の挿入を行えます。
$heredoc = <<<END
Multi line
$sgl_quotes
END;

// 文字列の連結は . で行います
echo 'This string ' . 'is concatenated';

// 別々のパラメータとしてechoに渡すこともできます
echo 'Multiple', 'Parameters', 'Valid';

/********************************
 * 定数
 */

// 定数は define() を使って定義します
// また、実行中は変更することができないので注意が必要です！

// 有効は定数は文字かアンダースコアで始めます
// それ移行のは、どんな数値でも文字列でもアンダースコアでも構いません
define("FOO",     "something");

// 定義した名前をそのまま($はつけずに)使用することで、定数にアクセスできます
// access to a constant is possible by direct using the choosen name
echo 'This outputs '.FOO;


/********************************
 * 配列
 */

// PHPの配列はすべて連想配列です

// 連想配列は、他の言語ではハッシュ(ハッシュマップ)として知られています

// すべてのバージョンのPHPで動作します
$associative = array('One' => 1, 'Two' => 2, 'Three' => 3);

// PHP 5.4 から、新しいシンタックスが導入されました
$associative = ['One' => 1, 'Two' => 2, 'Three' => 3];

echo $associative['One']; // 1とプリントされます

// キーを指定しないシンプルな配列にも、自動的に数値キーが振られます
$array = ['One', 'Two', 'Three'];
echo $array[0]; // => "One"

// 配列の最後に要素を追加する
$array[] = 'Four';
// または、次のようにも書けます
array_push($array, 'Five');

// 配列から要素を削除
unset($array[3]);

/********************************
 * 出力
 */

echo('Hello World!');
// 標準出力にHello World! とプリントします
// 標準出力はブラウザーで実行していればWebページに出力されます
// Stdout is the web page if running in a browser.

print('Hello World!'); // echoの結果と同じです

// echo は言語自体の構成要素であり、括弧なしで呼び出せます
// echo is actually a language construct, so you can drop the parentheses.
echo 'Hello World!';
print 'Hello World!'; // printも同様です

$paragraph = 'paragraph';

echo 100;        // スカラー数値を直接出力します
echo $paragraph; // 変数も使用できます

// PHPタグの短縮型が設定されているか、使用しているPHPのバージョンが
// 5.4.0 以上であれば、短縮echoシンタックスを使用できます
?>
<p><?= $paragraph ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $xに$yの値を代入します
$z = &$y;
// $zは$yへの参照です。
// $zの値を変更すると$yの値も変更されるでしょう。逆も同様です。
// $xは$yの最初の値を変わらず保持しています

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// 変数の型と値を標準出力へダンプします
var_dump($z); // int(0) と出力されます

// 人間が読めるフォーマットで変数を標準出力にプリントします
print_r($array); // prints: Array ( [0] => One [1] => Two [2] => Three )

/********************************
 * ロジック
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// assertは引数がfalseの場合、Exceptionを投げます

//これらの比較は型が違ったとしても、常に真です。
assert($a == $b); // equality
assert($c != $a); // inequality
assert($c <> $a); // alternative inequality
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// 次の比較は値が等しく、かつ同じ型である場合のみ真です
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// spaceship演算子はPHP7から使用可能です
$a = 100;
$b = 1000;

echo $a <=> $a; // 等しいので0になります
echo $a <=> $b; // $a < $b なので -1 です
echo $b <=> $a; // $b > $a なので 1 です

// 変数は使用するコンテキストによって、変換されます

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (文字列は強制的に数値として処理されます)

$string = 'one';
echo $string + $string; // => 0
// '+'演算子は文字列'one'を数値にキャストできないので、0と出力されます

// 型のキャスティングによって、変数を指定したもう一つの型として扱うことができます
// Type casting can be used to treat a variable as another type

$boolean = (boolean) 1; // => true

$zero = 0;
$boolean = (boolean) $zero; // => false

// 型をキャストするため専用の関数も存在します
$integer = 5;
$string = strval($integer);

$var = null; // Null値


/********************************
 * 制御構造
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

// 三項演算子
print (false ? 'Does not get printed' : 'Does');

// PHP 5.3から、三項演算子の短縮形が使用できます
// $x ? $x : 'Does'と同義です
$x = false;
print($x ?: 'Does');

// null合体演算子はPHP 7から使用できます
$a = null;
$b = 'Does print';
echo $a ?? 'a is not set'; // prints 'a is not set'
echo $b ?? 'b is not set'; // prints 'Does print'


$x = 0;
if ($x === '0') {
    print 'Does not print';
} elseif($x == '1') {
    print 'Does not print';
} else {
    print 'Does print';
}



// :を用いる別の構文はテンプレートで有用です
?>

<?php if ($x): ?>
この部分はifが真のとき表示されます
<?php else: ?>
それ以外の場合は、この部分が表示されます
<?php endif; ?>

<?php

// いくつかのロジックを保存するにはswitchを使用します
switch ($x) {
    case '0':
        print 'Switch does type coercion';
        break; // breakを書く必要があります。
               // でなければ、次の'two', 'three'のcase文を続けて実行することになります。
    case 'two':
    case 'three':
        // 変数が'two'または'three'の場合、何かを実行します
        break;
    default:
        //デフォルトで何かを実行します
}

// while, do,  forの構文は、おそらく他の言語とも共通なものです
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

//Foreachループによって、 配列を反復処理できます
foreach ($wheels as $wheel_count) {
    echo $wheel_count;
} // Prints "24"

echo "\n";

// 値と同じ様に、keyも反復処理できます
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
 * 関数
 */

// 関数を"function"で定義します
function my_function () {
    return 'Hello';
}

echo my_function(); // => "Hello"

// 有効な関数名は、文字またはアンダースコアで始めます。それ以降は
// どれだけ長い文字、数値、アンダースコアを続けても構いません

function add ($x, $y = 1) { // $yはオプショナルな値であり、デフォルトで 1 です
    $result = $x + $y;
    return $result;
}

echo add(4); // => 5
echo add(4, 2); // => 6

// $result には、関数の外からアクセス出来ません
// print $result; // エラーになります

// PHP 5.3 から、無名関数が使えます
$inc = function ($x) {
    return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
    echo "$x - $y - $z";
}

// 関数は、関数を返すことができます
function bar ($x, $y) {
    // 関数外の変数を利用したいときは、'use'を使います
    return function ($z) use ($x, $y) {
        foo($x, $y, $z);
    };
}

$bar = bar('A', 'B');
$bar('C'); // Prints "A - B - C"

// 文字列を使って、定義済みの関数を呼び出すことができます
$function_name = 'add';
echo $function_name(1, 2); // => 3

// プログラミング中に、動的に動かす関数を決める場合に便利です。
// もしくは、call_user_func(callable $callback [, $parameter [, ... ]]) を使っても同じことができます


// 特に指定しなくても、渡された引数を受け取ることもできます
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

/********************************
 * ファイルの読み込み
 */

<?php
// 読み込まれたファイル中のPHPは、同じくPHPのオープンタグで始める必要があります

include 'my-file.php';
// my-file.php中のコードは、現在のスコープの中で使用可能です
// もしファイルを読み込めなければ (例:file not found)、警告が発せられます

include_once 'my-file.php';
// my-file.phpのコードが既にどこかで読み込まれていれば、
// ファイルを読み込みません。これは、クラスの多重定義のエラーを防ぎます

require 'my-file.php';
require_once 'my-file.php';
// include()と同じように、require()はもしファイルを読み込むことができなければ、
// 致命的エラーの原因となります

// my-include.phpの内容
<?php

return 'Anything you like.';
// End file

// include()とrequire()は一つの値を返します
$value = include 'my-include.php';

// ファイルは与えられたファイルパスを基に読み込まれます。
// ファイルパスを指定しない場合は、include_path の設定を利用します。
// もしファイルがinclude_path中に見つからない場合は、
// 呼び出し元スクリプトのディレクトリと作業ディレクトリの中も探します。
// それでも見つからない場合、失敗します。
/* */

/********************************
 * クラス
 */

// クラスはclassキーワードで定義します

class MyClass
{
    const MY_CONST      = 'value'; // クラス定数です

    static $staticVar   = 'static';

    // スタティック変数とアクセス制限
    public static $publicStaticVar = 'publicStatic';
    // クラス内でのみアクセス可能
    private static $privateStaticVar = 'privateStatic';
    // そのクラスと子クラスで参照可能
    protected static $protectedStaticVar = 'protectedStatic';

    // プロパティはアクセス制限を宣言する必要があります
    public $property    = 'public';
    public $instanceProp;
    protected $prot = 'protected'; // そのクラスと子クラスで参照可能
    private $priv   = 'private';   // クラス内でのみアクセス可能

    // __constructでコンストラクターを生成します
    public function __construct($instanceProp) {
        // $thisでインスタンス変数にアクセスします
        $this->instanceProp = $instanceProp;
    }

    // メソッドはクラス内で関数として定義されます
    public function myMethod()
    {
        print 'MyClass';
    }

    // finalキーワードは関数の上書きを禁止します
    final function youCannotOverrideMe()
    {
    }

/*
 * クラスプロパティまたはメソッドをstaticとして作成すれば、
 * クラスをインスタンス化(newすること)しなくてもアクセスできます。
 * プロパティをstaticとして定義すると、
 * インスタンス化されたクラスオブジェクトを通してのアクセスはできなくなります。
 */

    public static function myStaticMethod()
    {
        print 'I am static';
    }
}

// クラス定数は、いつでも静的にアクセスできます。
echo MyClass::MY_CONST;    // Outputs 'value';

echo MyClass::$staticVar;  // Outputs 'static';
MyClass::myStaticMethod(); // Outputs 'I am static';

// クラスをインスタンス化するには、newを使います。
$my_class = new MyClass('An instance property');
// 括弧はもし引数を渡す必要がなければ省略可能です。

// ->を使ってクラスのメンバにアクセスします。
echo $my_class->property;     // => "public"
echo $my_class->instanceProp; // => "An instance property"
$my_class->myMethod();        // => "MyClass"


// extendsを使用してクラスを継承します。
class MyOtherClass extends MyClass
{
    function printProtectedProperty()
    {
        echo $this->prot;
    }

    // メソッドを上書きします。
    function myMethod()
    {
        parent::myMethod();
        print ' > MyOtherClass';
    }
}

$my_other_class = new MyOtherClass('Instance prop');
$my_other_class->printProtectedProperty(); // => Prints "protected"
$my_other_class->myMethod();               // Prints "MyClass > MyOtherClass"

final class YouCannotExtendMe
{
}

// 「マジックメソッド」を使ってゲッターとセッターを生成できます。
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
echo $x->property; // __get() メソッドを使用します
$x->property = 'Something'; // __set() メソッドを使用します

// クラスは抽象クラスにもできます(abstractキーワードを使用します)し、
// インターフェースを実装することもできます(implementsキーワードを使用します)。
// インターフェースはinterfaceキーワードで定義します。

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomethingElse();
}

// インターフェースは継承することができます
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


// クラスは1つ以上のインターフェースを実装できます。
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
 * トレイト
 */

// トレイトはPHP 5.4.0 以上で使用可能で、traitキーワードで定義します。

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
 *  名前空間
 */

// このセクションは名前空間の定義はファイルの先頭で宣言される必要があるため、
// 独立しています。
// そのケースには当てはまらないふりをして続けましょう。

<?php

// デフォルトでは、クラスはグローバルな名前空間に存在し、
// バックスラッシュによって明確にコールできます。

$cls = new \MyClass();



// ファイルに名前空間をセットします
namespace My\Namespace;

class MyClass
{
}

// (別のファイルからの呼び出し)
$cls = new My\Namespace\MyClass;

// 異なる名前空間からの呼び出し
namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();

// 名前空間に別名をつけることもできます

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();

/**********************
*  エラーハンドリング
*  
*/

// シンプルなエラーハンドリングは、try catchを使えば行えます

try {
    // 処理を実行します
} catch ( Exception $e) {
    // 例外を処理します
}

// try catchを名前空間を持った環境で使用するときは、次のようにします。

try { 
    // Do something
    // 処理を実行します
} catch (\Exception $e) { 
    // 例外を処理します
}

// 例外のカスタマイズ

class MyException extends Exception {}

try {
    
    $condition = true; 
    
    if ($condition) {
        throw new MyException('Something just happend');
    }
    
} catch (MyException $e) {
    // Handle my exception
}

```

## より詳しい情報

リファレンスを見るため、またコミュニティへの情報提供のために、 [PHP公式ドキュメント](http://www.php.net/manual/) を訪れてみてください。

もし最新のベストプラクティスに興味があるなら、
[PHP The Right Way](http://www.phptherightway.com/)を訪れてみてください。


もしあなたがよいパッケージマネジメント・システムを持つ言語で開発経験があるのなら、
[Composer](http://getcomposer.org/)も確かめてみてください。


共通基準を知るためには、PHP Framework Interoperability Groupの
[PSR standards](https://github.com/php-fig/fig-standards)にも訪れてみてください。
