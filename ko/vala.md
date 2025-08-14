---
name: Vala
contributors:
    - ["Milo Gilad", "https://github.com/Myl0g"]
filename: LearnVala.vala
---

GNOME의 말에 따르면, "Vala는 C로 작성된 애플리케이션 및 라이브러리와 비교하여 추가 런타임 요구 사항을 부과하지 않고 다른 ABI를 사용하지 않고도 GNOME 개발자에게 최신 프로그래밍 언어 기능을 제공하는 것을 목표로 하는 프로그래밍 언어입니다."

Vala는 Java와 C#의 측면을 가지고 있으므로 둘 중 하나를 아는 사람들에게는 자연스러울 것입니다.

[여기서 더 읽어보세요.](https://wiki.gnome.org/Projects/Vala)

```vala
// 한 줄 주석

/* 여러 줄
주석 */

/**
* 문서 주석
*/

/* 데이터 유형 */

char character = 'a'
unichar unicode_character = 'u' // 32비트 유니코드 문자

int i = 2; // 정수는 보장된 크기를 가질 수도 있습니다(예: int64, uint64).
uint j = -6; // 컴파일되지 않음; 부호 없는 정수는 양수만 가능합니다.

long k;

short l;
ushort m;

string text = "Hello,"; // == 연산자는 문자열 내용을 확인합니다.

string verbatim = """이것은 있는 그대로의(a.k.a. 원시) 문자열입니다. 특수 문자
(예: \n 및 "")는 해석되지 않습니다. 여러 줄일 수도 있습니다.""";

// 문자열 템플릿을 사용하면 쉽게 문자열 서식을 지정할 수 있습니다.
string string_template = @"$text world"; // "$text"는 "Hello,"로 평가됩니다.

int test = 5;
int test2 = 10;
string template2 = @"$(test * test2) is a number."; // 표현식 평가

string template_slice = string_template[7:12]; // => "world"

// 대부분의 데이터 유형에는 구문 분석을 위한 메서드가 있습니다.

bool parse_bool = bool.parse("false"); // => false
int parse_int = int.parse("-52"); // => -52
string parse_string = parse_int.to_string(); // => "-52"

/* 기본 I/O */

stdout.printf(parse_string); // 콘솔에 인쇄
string input = stdin.read_line(); // 콘솔에서 입력 받기

stderr.printf("Error message"); // 오류 인쇄

/* 배열 */

int[] int_array = new int[10]; // 10개의 슬롯이 있는 정수 배열
int better_int_array[10]; // 위의 표현식, 단축
int_array.length; // => 10;

int[] int_array2 = {5, 10, 15, 20}; // 즉시 생성 가능

int[] array_slice = int_array2[1:3]; // 슬라이스(데이터 복사)
unowned int[] array_slice_ref = int_array2[1:3]; // 데이터에 대한 참조

// 다차원 배열(대괄호 안의 쉼표 수로 정의)

int[,] multi_array = new int[6,4]; // 6은 배열 수, 4는 크기
int[,] multi_array2 = {{7, 4, 6, 4},
                       {3, 2, 4, 6},
                       {5, 9, 5, 1}}; // new int[3,4]
multi_array2[2,3] = 12; // 2는 배열, 3은 배열의 인덱스
int first_d = multi_array2.length[0] // => 3
int second_d = multi_array2.length[1] // => 4

// 배열 길이가 다른 스택 배열(예: int[][])은 지원되지 않습니다.

// 다차원 배열은 슬라이스할 수 없으며 1차원으로 변환할 수도 없습니다.

int[] add_to_array = {};
add_to_array += 12; // 배열에 동적으로 추가할 수 있습니다.

add_to_array.resize(20); // 배열에 이제 20개의 슬롯이 있습니다.

uint8[] chars = "test message".data;
chars.move(5, 0, 7);
stdout.printf((string) chars); // 배열을 문자열로 캐스팅하고 인쇄합니다.

/* 제어 흐름 */

int a = 1;
int b = 2;
int[] foreach_demo = {2, 4, 6, 8};

while (b > a) { // While 루프; 실행 전에 표현식이 참인지 확인
  b--;
}

do {
  b--;
}
while (b > a); // Do While 루프; while (b > a) 전에 "do"의 코드를 실행

for (a = 0; a < 10; a++) { stdout.printf("%d\n", a); } // for 루프

foreach (int foreach_demo_var in foreach_demo) {
  stdout.printf("%d\n", foreach_demo_var);
} // foreach는 모든 반복 가능한 컬렉션에서 작동합니다.

if (a == 0) {
  stdout.printf("%d\n", a);
} else if (a > 1) {
  stdout.printf("%d\n", a);
} else {
  stdout.printf("A is less than 0");
} // if-then-else

switch (a) {
  case 1:
    stdout.printf("A is 1\n");
    break;
  case 5:
  case 10:
    stdout.printf("A is 5 or 10\n");
    break;
  default:
    stdout.printf("???\n")
    break;
} // switch 문

/* 유형 캐스팅 및 추론 */

int cast_to_float = 10;
float casted_float = (float) cast_to_float; // 정적 캐스팅; 런타임 검사 없음

// 런타임 검사의 경우 동적 캐스팅을 사용합니다.
// 동적으로 캐스팅된 객체는 다음 중 하나여야 합니다.
// - 객체의 클래스가 원하는 유형과 동일한 클래스입니다.
// - 객체의 클래스가 원하는 유형의 하위 클래스입니다.
// - 원하는 클래스가 객체의 클래스에서 구현된 인터페이스입니다.

float dyna_casted_float = cast_to_float as float // 컴파일되지 않음

var inferred_string = "hello"; // 유형 추론

/* 메서드 (a.k.a. 함수) */

int method_demo(string arg1, Object arg2) { // int를 반환하고 인수를 받습니다.
    return 1;
}

// Vala 메서드는 오버로드할 수 없습니다.

void some_method(string text) { }
void some_method(int number) { }  // 컴파일되지 않음

// 유사한 기능을 달성하려면 기본 인수 값을 사용하십시오.

void some_better_method(string text, int number = 0) { }

some_better_method("text");
some_better_method("text", 12);

// varargs(가변 길이 인수 목록)도 지원됩니다.

void method_with_varargs(int arg1, ...) {
    var varargs_list = va_list(); // varargs 목록을 가져옵니다.

    string arg_string = varargs_list.arg(); // 인수를 차례로 가져옵니다.
    int int_vararg = varargs_list.arg();

    stdout.printf("%s, %d\n", arg_string, int_vararg)
}

string? ok_to_be_null(int? test_int) { } // "?"는 가능한 null 값을 나타냅니다.

// 대리자

delegate void DelegateDemo(char char_a);

void delegate_match(char char_a) { // DelegateDemo의 서명과 일치합니다.
  stdout.printf("%d\n");
}

void call_delegate(DelegateDemo d, char char_b) { // 대리자 인수를 받습니다.
  d(char_b) // 대리자 호출
}

void final_delegate_demo() {
  call_delegate(delegate_match); // 일치하는 메서드를 인수로 전달합니다.
}

// 람다(a.k.a. 익명 메서드)는 "=>"로 정의됩니다.

(a) => { stdout.printf("%d\n", a); } // "a"를 인쇄합니다.

/* 네임스페이스 */

namespace NamespaceDemo {
  // 변수 이름을 구성할 수 있습니다.
  int namespace_int = 12;
}
namespace_int += 5; // 컴파일되지 않음

using NamespaceDemo;
namespace_int += 5; // 유효함

/* 구조체 및 열거형 */

struct Closet {
  public uint shirts; // 기본 액세스 한정자는 private입니다.
  public uint jackets;
}

Closet struct_init_1 = Closet(); // 또는 Closet struct_init_1 = {};
Closet struct_init_2 = {15, 3};
var struct_init_3 = Closet() { // 유형 추론도 작동합니다.
  shirts = 15;
  jackets = 3;
}

enum HouseSize { // 열거형의 예
  SMALL,
  MODERATE,
  BIG
}

/* 클래스 및 객체 지향 프로그래밍 */

class Message : GLib.Object { // 클래스 Message는 GLib의 Object를 확장합니다.
  private string sender; // private 필드
  public string text {get; set;} // public 속성 (나중에 자세히 설명)
  protected bool is_digital = true; // protected (이 클래스 및 하위 클래스)
  internal bool sent = false; // internal (동일한 패키지의 클래스)

  public void send(string sender) { // public 메서드
    this.sender = sender;
    sent = true;
  }

  public Message() { // 생성자
    // ...
  }

}

// 메서드 오버로딩이 불가능하므로 생성자를 오버로드할 수 없습니다.
// 그러나 명명된 생성자를 사용하여 동일한 기능을 달성할 수 있습니다.

public class Calculator : GLib.Object {

    public Calculator() {
    }

    public Calculator.with_name(string name) {
    }

    public Calculator.model(string model_id, string name = "") {
      this.with_name(@"$model_id $name"); // "this"를 사용한 연쇄 생성자
    }
    ~Calculator() { } // 수동 메모리 관리를 사용하는 경우에만 필요합니다.
}

var calc1 = new Calculator.with_name("Temp");
var calc2 = new Calculator.model("TI-84");

// 시그널(a.k.a. 이벤트 또는 이벤트 리스너)은 동일한 서명을 가진 여러
// 메서드를 동시에 실행하는 방법입니다.

public class SignalDemo : GLib.Object {
  public signal void sig_demo(int sig_demo_int); // public이어야 합니다.

  public static int main(string[] args) {
    // main 메서드; 프로그램은 이것 없이는 컴파일되지 않습니다.

    var sig_demo_class = new SignalDemo(); // 클래스의 새 인스턴스

    sig_demo_class.sig_demo.connect((ob, sig_int) => { // 람다를 핸들러로 사용
        stdout.printf("%d\n", sig_int); // "ob"는 방출된 객체입니다.
      });

    sig_demo_class.sig_demo(27); // 시그널이 방출됩니다.

    return 0;
  }
}

// connect() 메서드를 사용하고 원하는 만큼 핸들러를 연결할 수 있습니다.
// 시그널이 방출되면 모두 거의 동시에 실행됩니다.

// 속성 (getter 및 setter)

class Animal : GLib.Object {
  private int _legs; // 이름 충돌을 방지하기 위해 밑줄로 시작

  public int legs {
    get { return _legs; }
    set { _legs = value; }
  }

  public int eyes { get; set; default = 5; } // 더 짧은 방법
  public int kingdom { get; private set; default = "Animalia"} // 읽기 전용

  public static void main(string args[]) {
    rabbit = new Animal();

    // 모든 GLib.Object에는 속성이 변경될 때 방출되는 "notify" 시그널이 있습니다.

    // 특정 속성을 지정하는 경우 GObject 명명 규칙을 따르기 위해
    // 모든 밑줄을 대시로 바꿉니다.

    rabbit.notify["eyes"].connect((s, p) => { // 모든 것에 대해 ["eyes"] 제거
      stdout.printf("Property '%s' has changed!\n", p.name);
    });

    rabbit.legs = 2;
    rabbit.legs += 2;
    rabbit.eyes = 2;

  }
}

// 상속: Vala 클래스는 1개의 클래스를 상속할 수 있습니다. 상속은 암시적이지 않습니다.

class SuperDemo : GLib.Object {
  public int data1;
  protected int data2;
  internal int data3;
  private int data4;

  public static void test_method {  } // 정적은 객체 없이 호출할 수 있습니다.
}
class SubDemo : SuperDemo {
  public static void main(string args[]) {
    stdout.printf((string) data1); // 컴파일됨
    stdout.printf((string) data2); // Protected는 하위 클래스에서 액세스 가능
    stdout.printf((string) data3); // Internal은 패키지에서 액세스 가능
    stdout.printf((string) data4); // 컴파일되지 않음
  }
}

// 추상 클래스 및 메서드

public abstract class OperatingSystem : GLib.Object {
  public void turn_on() {
    stdout.printf("Booted successfully.\n");
  }
  public abstract void use_computer();
}

public class Linux : OperatingSystem {
  public override void use_computer() { // 추상 메서드는 재정의해야 합니다.
    stdout.printf("Beep boop\n");
  }
}

// 추상 메서드에 기본 동작을 추가하려면 "virtual"로 만드십시오.

public abstract class HardDrive : GLib.Object {
  public virtual void die() {
    stdout.printf("CLICK-CLICK-CLICK\n");
  }
}
public class MyHD : HardDrive {
  public override void die() {
    return;
  }
}

// 인터페이스: 클래스는 이들 중 원하는 수만큼 구현할 수 있습니다.

interface Laptop { // 추상 또는 가상만 포함할 수 있습니다.
  public abstract void turn_on();
  public abstract void turn_off();

  public abstract int cores; // 컴파일되지 않음; 필드는 추상일 수 없음
  public abstract int cores {get; set;} // 컴파일됨

  public virtual void keyboard() { // 가상은 허용됨 (Java/C#과 달리)
    stdout.printf("Clickity-clack\n");
  }
}

// Vala에서 가상을 사용할 수 있다는 것은 다중 상속이
// 가능하다는 것을 의미합니다 (다소 제한적이지만).

// 인터페이스는 인터페이스를 구현할 수 없지만 특정
// 인터페이스나 클래스도 구현해야 한다는 것을 지정할 수 있습니다 (사전 요구 사항).

public interface CellPhone : Collection, GLib.Object {}

// 런타임에 동적으로 클래스의 유형 정보를 얻을 수 있습니다.

bool type_info = object is TypeName; // "is"를 사용하여 bool을 얻습니다.

Type type_info2 = object.get_type();
var type_name = type_info2.name();

Type type_info3 = typeof(Linux);
Linux type_demo = (Linux) Object.new(type_info3);

// 제네릭

class Computer<OperatingSystem> : GLib.Object {
  private OperatingSystem os;

  public void install_os(OperatingSystem os) {
    this.os = os;
  }
  public OperatingSystem retrieve_os() {
    return this.os;
  }
}

var new_computer = new Computer<Linux>();

/* 기타 기능 */

// 어설션: 문이 참이 아닌 경우 충돌 (런타임에)

bool is_true = true;
assert(is_true);

// 계약 프로그래밍

int contract_demo(int arg1, int arg2) {
  requires(arg1 > 0 && arg1 < 10) // 세미콜론 없음 참고
  requires(arg2 >= 12)
  ensures(result >= 0)
}

// 오류 처리

void error_demo(int int_ex) throws GError {
  if (int_ex != 1) {
    throw new GError("TEST MESSAGE");
  }
}
void error_demo2() {
  try {
    error_demo(0);
  } catch (GError ge) {
    stdout.printf("%s\n", ge.message);
  }
}

// 메인 루프

void main() {

  var main_loop = new MainLoop();
  var time = new TimeoutSource(2000);

  time.set_callback(() => { // 2000ms 후에 다음 람다를 실행합니다.
      stdout.printf("2000ms have passed\n");
      main_loop.quit();
      return false;
  });

  time.attach(main_loop.get_context());

  loop.run();
}

// 포인터 (수동 메모리 관리)

Object* pointer_obj = new Object(); // Object 인스턴스를 만들고 포인터를 제공합니다.

pointer_obj->some_method(); // some_method 실행
pointer_obj->some_data; // some_data 반환

delete pointer_obj;

int more = 57;
int* more_pointer = &more; // & = 주소-of
int indirection_demo = more_pointer*; // 간접 참조

// 프로필: 어떤 Vala 기능이 사용 가능하고 C 코드가 어떤 라이브러리를
// 사용할지에 영향을 줍니다.
// - gobject (기본값)
// posix
// dova
// 컴파일할 때 "--profile=whatever"를 사용하십시오.
```

* 더 많은 [Vala 문서](https://valadoc.org/).
* GObject와 유사한 [대체 생성 구문](https://wiki.gnome.org/Projects/Vala/Tutorial#GObject-Style_Construction)
* [계약 프로그래밍](http://en.wikipedia.org/wiki/Contract_programming)에 대한 자세한 정보
* [컬렉션 라이브러리](https://wiki.gnome.org/Projects/Vala/Tutorial#Collections)
* [멀티스레딩](https://wiki.gnome.org/Projects/Vala/Tutorial#Multi-Threading)
* [GTK+ 및 Vala로 GUI 빌드](http://archive.is/7C7bw)에 대해 읽어보십시오.
* [D-Bus 통합](https://wiki.gnome.org/Projects/Vala/Tutorial#D-Bus_Integration)
