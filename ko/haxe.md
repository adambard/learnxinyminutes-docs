---
name: Haxe
filename: LearnHaxe3.hx
contributors:
    - ["Justin Donaldson", "https://github.com/jdonaldson/"]
    - ["Dan Korostelev", "https://github.com/nadako/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Haxe](https://haxe.org/)는 C++, C#, Swf/ActionScript, JavaScript, Java, PHP, Python, Lua, HashLink 및 Neko 바이트코드(후자의 두 가지는 Haxe 작성자가 작성함)에 대한 플랫폼 지원을 제공하는 범용 언어입니다. 참고: 이 가이드는 Haxe 버전 3용입니다. 가이드의 일부는 이전 버전에 적용될 수 있지만 다른 참조를 사용하는 것이 좋습니다.

```haxe
/*
   Y분 만에 Haxe 3 배우기에 오신 것을 환영합니다. http://www.haxe.org
   이것은 실행 가능한 튜토리얼입니다. haxe 컴파일러를 사용하여 컴파일하고 실행할 수 있습니다.
   LearnHaxe.hx와 동일한 디렉토리에 있는 동안:

       $ haxe -main LearnHaxe3 --interp

   이 단락을 둘러싼 슬래시-별표 표시를 찾으십시오. 우리는 "여러 줄 주석" 안에 있습니다.
   컴파일러에서 무시될 몇 가지 메모를 여기에 남길 수 있습니다.

   여러 줄 주석은 haxedoc에 대한 javadoc 스타일 문서를 생성하는 데에도 사용됩니다.
   클래스, 클래스 함수 또는 클래스 변수 바로 앞에 오면 haxedoc에 사용됩니다.
 */

// 이와 같은 이중 슬래시는 한 줄 주석을 제공합니다.

/*
   이것은 첫 번째 실제 haxe 코드입니다. 빈 패키지를 선언합니다.
   패키지는 필요하지 않지만 코드에 대한 네임스페이스를 만들고 싶을 때 유용합니다(예: org.yourapp.ClassName).

   패키지 선언을 생략하는 것은 빈 패키지를 선언하는 것과 같습니다.
 */
package; // 빈 패키지, 네임스페이스 없음.

/*
   패키지는 모듈을 포함하는 디렉토리입니다. 각 모듈은 패키지에 정의된 유형을 포함하는 .hx 파일입니다. 패키지 이름(예: org.yourapp)은 소문자여야 하고 모듈 이름은 대문자여야 합니다. 모듈에는 이름이 대문자인 하나 이상의 유형이 포함됩니다.

   예를 들어, 클래스 "org.yourapp.Foo"는 컴파일러의 작업 디렉토리 또는 클래스 경로에서 액세스할 수 있는 org/module/Foo.hx 폴더 구조를 가져야 합니다.

   다른 파일에서 코드를 가져오는 경우 나머지 코드보다 먼저 선언해야 합니다.
   Haxe는 시작하는 데 도움이 되는 많은 일반적인 기본 클래스를 제공합니다:
 */
import haxe.ds.ArraySort;

// "*"를 사용하여 한 번에 많은 클래스/모듈을 가져올 수 있습니다.
import haxe.ds.*;

// 정적 필드를 가져올 수 있습니다.
import Lambda.array;

// "*"를 사용하여 모든 정적 필드를 가져올 수도 있습니다.
import Math.*;

// "mixin"과 같이 다른 클래스의 기능을 확장할 수 있도록 특별한 방법으로 클래스를 가져올 수도 있습니다. 나중에 'using'에 대해 자세히 설명합니다.
using StringTools;

// Typedef는 변수와 같습니다... 유형에 대한 것입니다. 코드보다 먼저 선언해야 합니다. 나중에 이에 대해 자세히 설명합니다.
typedef FooString = String;

// Typedef는 "구조적" 유형을 참조할 수도 있습니다. 나중에 이에 대해 자세히 설명합니다.
typedef FooObject = { foo: String };

// 다음은 클래스 정의입니다. 파일과 동일한 이름(LearnHaxe3)을 가지므로 파일의 기본 클래스입니다.
class LearnHaxe3 {
    /*
       일부 코드를 자동으로 실행하려면 정적 기본 함수에 넣고 컴파일러 인수에서 클래스를 지정해야 합니다.
       이 경우 위 컴파일러 인수에서 "LearnHaxe3" 클래스를 지정했습니다.
     */
    static function main() {
        /*
           Trace는 haxe 표현식을 화면에 인쇄하는 기본 방법입니다.
           다른 대상에는 이를 수행하는 다른 방법이 있습니다. 예를 들어, java, c++, c# 등은 표준 출력으로 인쇄합니다.
           JavaScript는 console.log로 인쇄하고 flash는 포함된 TextField로 인쇄합니다.
           모든 추적에는 기본적으로 줄 바꿈이 있습니다.
           마지막으로, 컴파일러에서 "--no-traces" 인수를 사용하여 추적이 표시되지 않도록 할 수 있습니다.
         */
        trace("Hello World, with trace()!");

        // Trace는 모든 유형의 값이나 개체를 처리할 수 있습니다. 표현식의 표현을 가능한 한 최선으로 인쇄하려고 합니다.
        // "+" 연산자로 문자열을 연결할 수도 있습니다:
        trace("Integer: " + 10 + " Float: " + 3.14 + " Boolean: " + true);

        // Haxe에서는 동일한 블록의 표현식을 세미콜론으로 구분해야 합니다.
        // 하지만 한 줄에 두 개의 표현식을 넣을 수 있습니다:
        trace('two expressions..'); trace('one line');


        //////////////////////////////////////////////////////////////////
        // 유형 및 변수
        //////////////////////////////////////////////////////////////////
        trace("***유형 및 변수***");

        // "var" 키워드를 사용하여 값과 데이터 구조에 대한 참조를 저장할 수 있습니다:
        var an_integer:Int = 1;
        trace(an_integer + " is the value for an_integer");

        /*
           Haxe는 정적으로 유형이 지정되므로 "an_integer"는 "Int" 유형을 갖도록 선언되고 나머지 표현식은 값 "1"을 할당합니다.
           많은 경우 유형을 선언할 필요가 없습니다. 여기서 haxe 컴파일러는 another_integer의 유형이 "Int"여야 한다고 추론합니다.
         */
        var another_integer = 2;
        trace(another_integer + " is the value for another_integer");

        // $type() 메서드는 컴파일러가 할당하는 유형을 인쇄합니다:
        $type(another_integer);

        // 16진수로 정수를 나타낼 수도 있습니다:
        var hex_integer = 0xffffff;

        /*
           Haxe는 Int 및 Float 크기에 플랫폼 정밀도를 사용합니다. 또한 오버플로에 대한 플랫폼 동작을 사용합니다.
           (다른 숫자 유형 및 동작은 특수 라이브러리를 사용하여 가능합니다.)

           정수, 부동 소수점 및 부울과 같은 단순한 값 외에도 Haxe는 문자열, 배열, 목록 및 맵과 같은 일반적인 데이터 구조에 대한 표준 라이브러리 구현을 제공합니다:
         */

        // 문자열에는 큰따옴표나 작은따옴표를 사용할 수 있습니다.
        var a_string = "some" + 'string';
        trace(a_string + " is the value for a_string");

        // 문자열은 변수를 특정 위치에 삽입하여 "보간"할 수 있습니다.
        // 문자열은 작은따옴표여야 하며 변수 앞에는 "$"가 와야 합니다. 표현식은 ${...}로 묶을 수 있습니다.
        var x = 1;
        var an_interpolated_string = 'the value of x is $x';
        var another_interpolated_string = 'the value of x + 1 is ${x + 1}';

        // 문자열은 불변이며, 인스턴스 메서드는 문자열의 일부 또는 전체의 복사본을 반환합니다. (StringBuf 클래스도 참조하십시오).
        var a_sub_string = a_string.substr(0,4);
        trace(a_sub_string + " is the value for a_sub_string");

        // 정규식도 지원되지만, 여기서는 자세히 설명할 공간이 부족합니다.
        var re = ~/foobar/;
        trace(re.match('foo') + " is the value for (~/foobar/.match('foo'))");

        // 배열은 0부터 시작하고, 동적이며, 변경 가능합니다. 누락된 값은 null로 정의됩니다.
        var a = new Array<String>(); // 문자열을 포함하는 배열
        a[0] = 'foo';
        trace(a.length + " is the value for a.length");
        a[9] = 'bar';
        trace(a.length + " is the value for a.length (after modification)");
        trace(a[3] + " is the value for a[3]"); //null

        // 배열은 *제네릭*이므로 유형 매개변수로 포함하는 값을 나타낼 수 있습니다:
        var a2 = new Array<Int>(); // Int 배열
        var a3 = new Array<Array<String>>(); // 문자열 배열의 배열.

        // 맵은 간단한 키/값 데이터 구조입니다. 키와 값은 모든 유형일 수 있습니다.
        // 여기서 키는 문자열이고 값은 Int입니다:
        var m = new Map<String, Int>();
        m.set('foo', 4);
        // 배열 표기법을 사용할 수도 있습니다:
        m['bar'] = 5;
        trace(m.exists('bar') + " is the value for m.exists('bar')");
        trace(m.get('bar') + " is the value for m.get('bar')");
        trace(m['bar'] + " is the value for m['bar']");

        var m2 = ['foo' => 4, 'baz' => 6]; // 대체 맵 구문
        trace(m2 + " is the value for m2");

        // 유형 추론을 사용할 수 있습니다. Haxe 컴파일러는 유형 매개변수를 설정하는 인수를 처음 전달할 때 변수의 유형을 결정합니다.
        var m3 = new Map();
        m3.set(6, 'baz'); // m3는 이제 Map<Int,String>입니다.
        trace(m3 + " is the value for m3");

        // Haxe는 haxe.ds 모듈에 목록, 스택 및 균형 트리와 같은 더 일반적인 데이터 구조를 가지고 있습니다.


        //////////////////////////////////////////////////////////////////
        // 연산자
        //////////////////////////////////////////////////////////////////
        trace("***연산자***");

        // 기본 산술
        trace((4 + 3) + " is the value for (4 + 3)");
        trace((5 - 1) + " is the value for (5 - 1)");
        trace((2 * 4) + " is the value for (2 * 4)");
        // 나눗셈은 항상 부동 소수점을 생성합니다.
        trace((8 / 3) + " is the value for (8 / 3) (a Float)");
        trace((12 % 4) + " is the value for (12 % 4)");

        // 기본 비교
        trace((3 == 2) + " is the value for 3 == 2");
        trace((3 != 2) + " is the value for 3 != 2");
        trace((3 >  2) + " is the value for 3 > 2");
        trace((3 <  2) + " is the value for 3 < 2");
        trace((3 >= 2) + " is the value for 3 >= 2");
        trace((3 <= 2) + " is the value for 3 <= 2");

        // 표준 비트 연산자
        /*
            ~       단항 비트 보수
            <<      부호 있는 왼쪽 시프트
            >>      부호 있는 오른쪽 시프트
            >>>     부호 없는 오른쪽 시프트
            &       비트 AND
            ^       비트 배타적 OR
            |       비트 포함 OR
        */

        var i = 0;
        trace("전/후- 증감 및 감소");
        trace(i++); // i = 1. 후-증가
        trace(++i); // i = 2. 전-증가
        trace(i--); // i = 1. 후-감소
        trace(--i); // i = 0. 전-감소


        //////////////////////////////////////////////////////////////////
        // 제어 구조
        //////////////////////////////////////////////////////////////////
        trace("***제어 구조***");

        // if 문
        var j = 10;
        if (j == 10) {
            trace("this is printed");
        } else if (j > 10) {
            trace("not greater than 10, so not printed");
        } else {
            trace("also not printed.");
        }

        // "삼항" if도 있습니다:
        (j == 10) ? trace("equals 10") : trace("not equals 10");

        // 마지막으로, 컴파일 타임에 작동하는 또 다른 형태의 제어 구조가 있습니다: 조건부 컴파일.
#if neko
        trace('hello from neko');
#elseif js
        trace('hello from js');
#else
        trace('hello from another platform!');
#end

        // 컴파일된 코드는 플랫폼 대상에 따라 변경됩니다.
        // neko(-x 또는 -neko)용으로 컴파일하므로 neko 인사말만 받습니다.


        trace("반복 및 반복");

        // while 루프
        var k = 0;
        while (k < 100) {
            // trace(counter); // 0-99 숫자 출력
            k++;
        }

        // do-while 루프
        var l = 0;
        do {
            trace("do 문은 항상 최소 한 번 실행됩니다.");
        } while (l > 0);

        // for 루프
        // Haxe에는 C 스타일 for 루프가 없습니다. 오류가 발생하기 쉽고 필요하지 않기 때문입니다.
        // 대신 Haxe에는 반복기(나중에 자세히 설명)를 사용하는 훨씬 간단하고 안전한 버전이 있습니다.
        var m = [1, 2, 3];
        for (val in m) {
            trace(val + " is the value for val in the m array");
        }

        // 범위에서 인덱스를 반복할 수도 있습니다.
        // (범위에 대한 자세한 내용도 나중에 설명)
        var n = ['foo', 'bar', 'baz'];
        for (val in 0...n.length) {
            trace(val + " is the value for val (an index for n)");
        }


        trace("배열 이해");

        // 배열 이해는 필터 및 수정을 생성하면서 배열을 반복할 수 있는 기능을 제공합니다.
        var filtered_n = [for (val in n) if (val != "foo") val];
        trace(filtered_n + " is the value for filtered_n");

        var modified_n = [for (val in n) val += '!'];
        trace(modified_n + " is the value for modified_n");

        var filtered_and_modified_n
            = [for (val in n) if (val != "foo") val += "!"];
        trace(filtered_and_modified_n
                + " is the value for filtered_and_modified_n");


        //////////////////////////////////////////////////////////////////
        // 스위치 문 (값 유형)
        //////////////////////////////////////////////////////////////////
        trace("***스위치 문 (값 유형)***");

        /*
           Haxe의 스위치 문은 매우 강력합니다. 문자열 및 int와 같은 기본 값에서 작동하는 것 외에도 열거형의 일반화된 대수 데이터 유형에서도 작동할 수 있습니다(나중에 열거형에 대해 자세히 설명).
           지금은 몇 가지 기본 값 예제가 있습니다:
         */
        var my_dog_name = "fido";
        var favorite_thing  = "";
        switch (my_dog_name) {
            case "fido" : favorite_thing = "bone";
            case "rex"  : favorite_thing = "shoe";
            case "spot" : favorite_thing = "tennis ball";
            default     : favorite_thing = "some unknown treat";
            // default와 동일:
            // case _   : favorite_thing = "some unknown treat";
        }
        // 위의 "_" 케이스는 무엇이든 일치하는 "와일드카드" 값입니다.

        trace("My dog's name is " + my_dog_name
                + ", and his favorite thing is a: "
                + favorite_thing);


        //////////////////////////////////////////////////////////////////
        // 표현식 문
        //////////////////////////////////////////////////////////////////
        trace("***표현식 문***");

        // Haxe 제어 문은 모든 문이 표현식이기도 하기 때문에 매우 강력합니다. 다음을 고려하십시오:

        // if 문
        var k = if (true) 10 else 20;

        trace("k equals ", k); // 10 출력

        var other_favorite_thing = switch (my_dog_name) {
            case "fido" : "teddy";
            case "rex"  : "stick";
            case "spot" : "football";
            default     : "some unknown treat";
        }

        trace("My dog's name is " + my_dog_name
                + ", and his other favorite thing is a: "
                + other_favorite_thing);


        //////////////////////////////////////////////////////////////////
        // 값 유형 변환
        //////////////////////////////////////////////////////////////////
        trace("***값 유형 변환***");

        // 문자열을 int로 쉽게 변환할 수 있습니다.

        // 문자열을 정수로
        Std.parseInt("0");     // 0 반환
        Std.parseFloat("0.4"); // 0.4 반환

        // 정수를 문자열로
        Std.string(0); // "0" 반환
        // 문자열과 연결하면 자동으로 문자열로 변환됩니다.
        0 + "";    // "0" 반환
        true + ""; // "true" 반환
        // Std의 구문 분석에 대한 자세한 내용은 설명서를 참조하십시오.


        //////////////////////////////////////////////////////////////////
        // 유형 처리
        //////////////////////////////////////////////////////////////////

        /*
           앞서 언급했듯이 Haxe는 정적으로 유형이 지정된 언어입니다. 대체로 정적 타이핑은 훌륭한 것입니다. 정확한 자동 완성을 가능하게 하고 프로그램의 정확성을 철저히 확인하는 데 사용할 수 있습니다. 또한 Haxe 컴파일러는 매우 빠릅니다.

           *그러나* 컴파일러가 특정 경우에 유형 오류를 발생시키지 않고 그냥 넘어가기를 바랄 때가 있습니다.

           이를 위해 Haxe에는 두 개의 개별 키워드가 있습니다. 첫 번째는 "Dynamic" 유형입니다:
         */
        var dyn: Dynamic = "any type of variable, such as this string";

        /*
           Dynamic 변수에 대해 확실히 아는 것은 컴파일러가 더 이상 어떤 유형인지 걱정하지 않는다는 것입니다. 와일드카드 변수와 같습니다. 모든 변수 유형 대신 전달할 수 있으며 원하는 모든 변수 유형을 할당할 수 있습니다.

           다른 더 극단적인 옵션은 "untyped" 키워드입니다:
         */
        untyped {
            var x:Int = 'foo'; // 이것은 옳지 않습니다!
            var y:String = 4;  // 광기!
        }

        /*
           untyped 키워드는 전체 코드 *블록*에서 작동하며, 그렇지 않으면 필요할 수 있는 모든 유형 검사를 건너뜁니다. 이 키워드는 유형 검사가 방해가 되는 제한된 조건부 컴파일 상황과 같이 매우 드물게 사용해야 합니다.

           일반적으로 유형 검사를 건너뛰는 것은 *권장되지 않습니다*. 열거형, 상속 또는 구조적 유형 모델을 사용하여 프로그램의 정확성을 보장하는 데 도움이 됩니다. 유형 모델이 작동하지 않는다고 확신하는 경우에만 "Dynamic" 또는 "untyped"를 사용해야 합니다.
         */


        //////////////////////////////////////////////////////////////////
        // 기본 객체 지향 프로그래밍
        //////////////////////////////////////////////////////////////////
        trace("***기본 객체 지향 프로그래밍***");

        // FooClass의 인스턴스를 만듭니다. 이에 대한 클래스는 파일 끝에 있습니다.
        var foo_instance = new FooClass(3);

        // public 변수를 정상적으로 읽습니다.
        trace(foo_instance.public_any
                + " is the value for foo_instance.public_any");

        // 이 변수를 읽을 수 있습니다.
        trace(foo_instance.public_read
                + " is the value for foo_instance.public_read");
        // 하지만 쓸 수는 없습니다. 주석을 해제하면 오류가 발생합니다:
        // foo_instance.public_read = 4;
        // trace(foo_instance.public_write); // 이것도 마찬가지입니다.

        // toString 메서드를 호출합니다:
        trace(foo_instance + " is the value for foo_instance");
        // 동일한 것:
        trace(foo_instance.toString()
                + " is the value for foo_instance.toString()");

        // foo_instance는 "FooClass" 유형을 가지고 있고, acceptBarInstance는 BarClass 유형을 가지고 있습니다. 그러나 FooClass가 BarClass를 확장하므로 허용됩니다.
        BarClass.acceptBarInstance(foo_instance);

        // 아래 클래스에는 더 고급 예제가 있으며, "example()" 메서드는 여기에서 실행합니다.
        SimpleEnumTest.example();
        ComplexEnumTest.example();
        TypedefsAndStructuralTypes.example();
        UsingExample.example();
    }
}

// 이것은 기본 LearnHaxe3 클래스의 "자식 클래스"입니다.
class FooClass extends BarClass implements BarInterface {
    public var public_any:Int; // public 변수는 어디에서나 액세스할 수 있습니다.
    public var public_read (default, null): Int; // public 읽기만 활성화
    public var public_write (null, default): Int; // 또는 public 쓰기만
    // 이 스타일을 사용하여 getter/setter를 활성화합니다:
    public var property (get, set): Int;

    // private 변수는 클래스 외부에서 사용할 수 없습니다.
    // 이에 대한 해결 방법은 @:allow를 참조하십시오.
    var _private:Int; // public으로 표시되지 않은 변수는 private입니다.

    // public 생성자
    public function new(arg:Int) {
        // BarClass를 확장했으므로 부모 개체의 생성자를 호출합니다:
        super();

        this.public_any = 0;
        this._private = arg;
    }

    // _private에 대한 getter
    function get_property() : Int {
        return _private;
    }

    // _private에 대한 setter
    function set_property(val:Int) : Int {
        _private = val;
        return val;
    }

    // 인스턴스가 문자열로 캐스팅될 때마다 호출되는 특수 함수입니다.
    public function toString() {
        return _private + " with toString() method!";
    }

    // 이 클래스는 BarInterface 인터페이스를 구현하므로 이 함수가 정의되어 있어야 합니다.
    public function baseFunction(x: Int) : String {
        // int를 자동으로 문자열로 변환
        return x + " was passed into baseFunction!";
    }
}

// 확장할 간단한 클래스입니다.
class BarClass {
    var base_variable:Int;
    public function new() {
        base_variable = 4;
    }
    public static function acceptBarInstance(b:BarClass) {}
}

// 구현할 간단한 인터페이스입니다.
interface BarInterface {
    public function baseFunction(x:Int):String;
}


//////////////////////////////////////////////////////////////////
// 열거형 및 스위치 문
//////////////////////////////////////////////////////////////////

// Haxe의 열거형은 매우 강력합니다. 가장 간단한 형태에서 열거형은 제한된 수의 상태를 가진 유형입니다:
enum SimpleEnum {
    Foo;
    Bar;
    Baz;
}

//   다음은 이를 사용하는 클래스입니다:
class SimpleEnumTest {
    public static function example() {
        // "전체" 이름을 지정할 수 있습니다.
        var e_explicit:SimpleEnum = SimpleEnum.Foo;
        var e = Foo; // 하지만 추론도 작동합니다.
        switch (e) {
            case Foo: trace("e was Foo");
            case Bar: trace("e was Bar");
            case Baz: trace("e was Baz"); // 이 줄을 주석 처리하면 오류가 발생합니다.
        }

        /*
           이것은 문자열에 대한 간단한 값 스위치와 크게 다르지 않습니다.
           그러나 *모든* 상태를 포함하지 않으면 컴파일러가 불평합니다.
           위 줄을 주석 처리하여 시도해 볼 수 있습니다.

           열거형 스위치에 대한 기본값도 지정할 수 있습니다:
         */
        switch (e) {
            case Foo: trace("e was Foo again");
            default : trace("default works here too");
        }
    }
}

// 열거형은 단순한 상태보다 훨씬 더 나아가 *생성자*를 열거할 수도 있지만, 더 복잡한 열거형 예제가 필요합니다.
enum ComplexEnum {
    IntEnum(i:Int);
    MultiEnum(i:Int, j:String, k:Float);
    SimpleEnumEnum(s:SimpleEnum);
    ComplexEnumEnum(c:ComplexEnum);
}
// 참고: 위의 열거형은 자신을 포함하여 *다른* 열거형도 포함할 수 있습니다!
// 참고: 이것은 다른 언어에서 *대수 데이터 유형*이라고 하는 것입니다.

class ComplexEnumTest {
    public static function example() {
        var e1:ComplexEnum = IntEnum(4); // 열거형 매개변수 지정
        // 이제 열거형을 전환하고 포함할 수 있는 모든 매개변수를 추출할 수 있습니다.
        switch (e1) {
            case IntEnum(x) : trace('$x was the parameter passed to e1');
            default: trace("Shouldn't be printed");
        }

        // 그 자체가 열거형인 또 다른 매개변수... 열거형 열거형?
        var e2 = SimpleEnumEnum(Foo);
        switch (e2){
            case SimpleEnumEnum(s): trace('$s was the parameter passed to e2');
            default: trace("Shouldn't be printed");
        }

        // 열거형 끝까지
        var e3 = ComplexEnumEnum(ComplexEnumEnum(MultiEnum(4, 'hi', 4.3)));
        switch (e3) {
            // 명시적으로 지정하여 특정 중첩 열거형을 찾을 수 있습니다.
            case ComplexEnumEnum(ComplexEnumEnum(MultiEnum(i,j,k))) : {
                trace('$i, $j, and $k were passed into this nested monster');
            }
            default: trace("Shouldn't be printed");
        }
        // "일반화된 대수 데이터 유형"(GADT)에 대한 자세한 내용은 왜 이것이 그렇게 훌륭한지에 대한 자세한 내용을 참조하십시오.
    }
}

class TypedefsAndStructuralTypes {
    public static function example() {
        // 여기서는 기본 유형 대신 typedef 유형을 사용합니다.
        // 상단에서 "FooString" 유형을 "String" 유형을 의미하도록 선언했습니다.
        var t1:FooString = "some string";

        // "구조적 유형"에도 typedef를 사용할 수 있습니다. 이러한 유형은 클래스 상속이 아닌 필드 구조에 의해 정의됩니다.
        // 다음은 "foo"라는 이름의 문자열 필드가 있는 익명 개체입니다:
        var anon_obj = { foo: 'hi' };

        /*
           anon_obj 변수는 유형이 선언되지 않았으며 컴파일러에 따르면 익명 개체입니다.
           그러나 맨 위에서 FooObj typedef를 선언한 것을 기억하십시오. anon_obj가 해당 구조와 일치하므로 "FooObject" 유형이 예상되는 모든 곳에서 사용할 수 있습니다.
         */
        var f = function(fo:FooObject) {
            trace('$fo was passed in to this function');
        }
        f(anon_obj); // anon_obj로 FooObject 서명 함수 호출.

        /*
           typedef에는 "?"로 표시된 선택적 필드도 있을 수 있습니다.

           typedef OptionalFooObj = {
                ?optionalString: String,
                requiredInt: Int
           }

           Typedef는 조건부 컴파일과 잘 작동합니다. 예를 들어, 파일 상단에 다음을 포함할 수 있습니다:

#if( js )
        typedef Surface = js.html.CanvasRenderingContext2D;
#elseif( nme )
        typedef Surface = nme.display.Graphics;
#elseif( !flash9 )
        typedef Surface = flash8.MovieClip;
#elseif( java )
        typedef Surface = java.awt.geom.GeneralPath;
#end

           그러면 모든 플랫폼에서 작업할 수 있는 단일 "Surface" 유형이 제공됩니다.
         */
    }
}

class UsingExample {
    public static function example() {
        /*
           "using" 가져오기 키워드는 클래스의 모든 정적 메서드의 동작을 변경하는 특수 유형의 클래스 가져오기입니다.

           이 파일에서는 문자열 유형을 처리하기 위한 여러 정적 메서드를 포함하는 "StringTools"에 "using"을 적용했습니다.
         */
        trace(StringTools.endsWith("foobar", "bar") + " should be true!");

        /*
           "using" 가져오기를 사용하면 첫 번째 인수 유형이 메서드로 확장됩니다.
           무슨 뜻일까요? "endsWith"의 첫 번째 인수 유형이 "String"이므로 모든 문자열 유형에는 이제 "endsWith" 메서드가 있습니다:
         */
        trace("foobar".endsWith("bar") + " should be true!");

        /*
           이 기술은 특정 유형에 대한 많은 표현을 가능하게 하면서 단일 파일로 수정 범위를 제한합니다.

           참고: 문자열 인스턴스는 런타임에 수정되지 않습니다.
           새로 연결된 메서드는 실제로 연결된 인스턴스의 일부가 아니며 컴파일러는 여전히 정적 메서드와 동일한 코드를 생성합니다.
         */
    }
}
```

여기서는 Haxe가 할 수 있는 것의 표면만 긁었습니다. 모든 Haxe 기능에 대한 공식적인 개요는 [설명서](https://haxe.org/manual) 및 [API 문서](https://api.haxe.org/)를 참조하십시오. 사용 가능한 타사 Haxe 라이브러리의 포괄적인 디렉토리는 [Haxelib](https://lib.haxe.org/)를 참조하십시오.

더 고급 주제에 대해서는 다음을 확인하는 것을 고려하십시오:

* [추상 유형](https://haxe.org/manual/types-abstract.html)
* [매크로](https://haxe.org/manual/macro.html)
* [컴파일러 기능](https://haxe.org/manual/cr-features.html)


마지막으로, [Haxe 포럼](https://community.haxe.org/), IRC [#haxe on freenode](http://webchat.freenode.net/) 또는 [Haxe Gitter 채팅](https://gitter.im/HaxeFoundation/haxe)에 참여하십시오.