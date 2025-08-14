---
name: Hack
contributors:
    - ["Andrew DiMola", "https://github.com/AndrewDiMola"]
    - ["Stephen Holdaway", "https://github.com/stecman"]
    - ["David Lima", "https://github.com/davelima"]
filename: learnhack.hh
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[Hack](https://hacklang.org/)은 정적 타입 검사와 같은 안전 기능이 내장되어 있으면서도 코드를 빠르게 작성할 수 있게 해줍니다.

Hack 코드를 실행하려면 오픈 소스 가상 머신인 [HHVM을 설치](https://docs.hhvm.com/hhvm/installation/introduction)하십시오.

```php
/* ==================================
 *           문서 읽기!
 * ==================================
 */

/* Hack 언어에 대한 자세한 내용은 다음을 참조하십시오:
 * - Hack 정보: https://hacklang.org/
 * - 문서: https://docs.hhvm.com/hack/
 */

/* ==================================
 *           PHP에 대한 참고 사항
 * ==================================
 */

// Hack 언어는 PHP의 상위 집합으로 시작했습니다.
// 그 이후로 언어는 (대부분) 분기되었습니다.
// 더 이상 권장되지 않는 .php 확장자를 만날 수 있습니다.

/* ==================================
 *              주석
 * ==================================
 */

// Hack에는 한 줄 주석이 있습니다...

/* 여러 줄 주석...
 * 
 */

/**
 * ... 그리고 문서 주석을 위한 특별한 구문이 있습니다.
 * 
 * 문서 주석을 사용하여 정의, 함수, 클래스 또는 메서드의 목적을 요약하십시오.
 */

/* ==================================
 *             네임스페이스
 * ==================================
 */

// 네임스페이스에는 클래스, 인터페이스, 트레이트, 함수 및 상수의 정의가 포함됩니다.

namespace LearnHackinYMinutes {

  /* ==================================
   *                유형
   * ==================================
   */

  function demo_hack_types(): void {

    // Hack에는 bool, int, float, string 및 null의 다섯 가지 기본 유형이 있습니다.
    $is_helpful = true; // bool
    $int_value = 10; // int
    $precise_value = 2.0; // float
    $hello_world = "Hello World!"; // string
    $null_string = null; // null

    // shape 키워드를 사용하여 필드 이름 및 값 시리즈로 `shape`을 만듭니다.
    $my_point = shape('x' => -3, 'y' => 6, 'visible' => true);

    // tuple 키워드를 사용하여 두 개 이상의 유형을 값으로 하는 시리즈로 `tuple`을 만듭니다.
    $apple_basket = tuple("apples", 25); // 다른 유형도 괜찮습니다.

    // `arraykey`를 사용하여 정수 또는 문자열을 나타냅니다.
    $the_answer = 42;
    $is_answer = process_key($the_answer);

    // 마찬가지로 `num`은 int 또는 float를 나타냅니다.
    $lucky_number = 7;
    $lucky_square = calculate_square($lucky_number);
  }

  function process_key(arraykey $the_answer): bool {
    if ($the_answer is int) {
      return true;
    } else {
      return false;
    } // true
  }

  function calculate_square(num $arg)[]: float {
    return ((float)$arg * $arg);
  }

  // 열거형은 int 또는 string(Arraykey로) 또는 다른 열거형 값으로 제한됩니다.
  enum Permission: string {
    Read = 'R';
    Write = 'W';
    Execute = 'E';
    Delete = 'D';
  }

  // 반면에 열거형 클래스는 모든 값 유형일 수 있습니다!
  enum class Random: mixed {
    int X = 42;
    string S = 'foo';
  }

  /* ==================================
   *            HACK 배열
   * ==================================
   */

  // 다음 줄은 `C\` 네임스페이스의 함수를 사용할 수 있게 합니다.
  use namespace HH\Lib\C; // `C` 라이브러리는 컨테이너에서 작동합니다.

  function demo_hack_arrays(): void {

    // vec: 정렬됨
    $v = vec[1, 2, 3];
    $letters = vec['a', 'b', 'c'];

    $letters[0]; // 'a' 반환
    $letters[] = 'd'; // 'd' 추가

    // `inout`은 참조 전달 동작을 제공합니다.
    C\pop_back(inout $letters); // 'd' 제거
    C\pop_front(inout $letters); // 'a' 제거

    // keyset: 정렬됨, 중복 없음
    $k = keyset[1, 2, 3]; // 값은 int 또는 string이어야 합니다.
    $colors = keyset['red', 'blue', 'green'];

    // keyset 키는 해당 값과 동일합니다.
    $colors['blue']; // 'blue' 반환.

    $colors[] = 'yellow'; // 'yellow' 추가
    unset($colors['red']); // 'red' 제거

    //  dict: 정렬됨, 키-값 기준
    $d = dict['a' => 1, 'b' => 3]; // 키는 int 또는 string이어야 합니다.
    $alphabet = dict['a' => 1, 'b' => 2];

    $alphabet['a']; // 'a'에서 인덱싱하면 `1` 반환
    $alphabet['c'] = 3; // `c => 3`의 새 키-값 쌍 추가

    unset($alphabet['b']); // 'b' 제거
  }

  /* ==================================
   *  HACK 표준 라이브러리 (HSL)
   * ==================================
   */

  // Hack 표준 라이브러리는 Hack 언어를 위한 함수 및 클래스 집합입니다.
  // 네임스페이스 사용 선언은 이상적으로 파일 상단에 있지만 교육 목적으로 여기에 배치됩니다.

  use namespace HH\Lib\Str; // `Str` 라이브러리는 문자열에서 작동합니다.

  function demo_hack_standard_library(): void {

    $letters = vec['a', 'b', 'c'];
    $colors = keyset['red', 'blue', 'green'];
    $alphabet = dict['a' => 1, 'b' => 2];

    C\contains($letters, 'c'); // 값 확인; 'true' 반환
    C\contains($colors, 'purple'); // 값 확인; 'false' 반환
    C\contains_key($alphabet, 'a'); // 키 확인; 'true' 반환
    C\contains($alphabet, 'd'); // 값 확인; 'false' 반환

    Str\length("foo"); // `3` 반환
    Str\join(vec['foo', 'bar', 'baz'], '!'); // `foo!bar!baz` 반환
  }

  /* ==================================
   *           HELLO WORLD!
   * ==================================
   */

  use namespace HH\Lib\IO; // `IO` 라이브러리는 입출력을 위한 표준 API입니다.

  <<__EntryPoint>> // 일반적인 진입/주 함수에 필요한 속성
  async function main(): Awaitable<
    void,
  > { // 'main'으로 명명할 필요 없음 / 비동기 함수임
    await IO\request_output()->writeAllAsync(
      "Hello World!\n",
    ); // 'Hello World' 인쇄!
  }

  /* ==================================
   *             함수
   * ==================================
   */

  // 함수는 전역적으로 정의됩니다.
  // 함수가 클래스에 정의되면 함수를 메서드라고 합니다.

  // 함수에는 반환 유형(여기서는 `int`)이 있으며
  // 해당 유형의 값을 반환하거나 void 반환 유형 주석이 사용된 경우 값을 반환하지 않아야 합니다.

  function add_one(int $x): int {
    return $x + 1;
  }

  // 함수에는 정의된 기본값도 있을 수 있습니다.
  function add_value(int $x, int $y = 1): int {
    return $x + $y;
  }

  // 함수는 가변적일 수 있습니다(인수 길이가 지정되지 않음).
  function sum_ints(int $val, int ...$vals): int {
    $result = $val;

    foreach ($vals as $v) {
      $result += $v;
    }
    return $result;
  }

  // 함수는 익명일 수도 있습니다(`==>` 화살표로 정의됨).
  // $f = (int $x): int ==> $x + 1;

  /* ==================================
   *           파이프 연산자
   * ==================================
   */

  // 파이프 연산자 `|>`는 왼쪽 표현식의 결과를 평가하고
  // 결과를 미리 정의된 파이프 변수인 `$$`에 저장합니다.

  use namespace HH\Lib\Vec;

  function demo_pipe_operator(): void {

    Vec\sort(Vec\map(vec[2, 1, 3], $a ==> $a * $a)); // vec[1,4,9]

    // 동일한 결과이지만 파이프 연산자와 파이프 변수를 사용합니다:
    $x = vec[2, 1, 3]
      |> Vec\map($$, $a ==> $a * $a) // 값이 vec[2,1,3]인 $$
      |> Vec\sort($$); // 값이 vec[4,1,9]인 $$
  }

  /* ==================================
   *             속성
   * ==================================
   */

  // Hack은 런타임 또는 정적 타입 검사 동작을 변경할 수 있는 내장 속성을 제공합니다.
  // 예를 들어, "Hello World!" 예제에서 `__EntryPoint` 속성을 이전에 사용했습니다.

  // 또 다른 예로, `__Memoize`는 함수의 결과를 캐시합니다.
  <<__Memoize>>
  async function do_expensive_task(): Awaitable<string> {
    $site_contents = await \HH\Asio\curl_exec("http://hacklang.org");
    return $site_contents;
  }

  /* ==================================
   *             컨텍스트
   * ==================================
   */

  // Hack 함수는 다른 컨텍스트 및 기능에 연결됩니다.
  // 컨텍스트는 기능 그룹, 즉 권한 그룹입니다.

  // 허용된 컨텍스트(및 기능)를 선언하려면 컨텍스트 목록 `[]`을 사용하십시오.
  // 컨텍스트가 정의되지 않은 경우 함수에는 Hack의 `defaults` 컨텍스트에 정의된 권한이 포함됩니다.

  // 컨텍스트 목록이 정의되지 않았기 때문에 `defaults` 컨텍스트가 암시적으로 선언됩니다.
  async function implicit_defaults_context(): Awaitable<void> {
    await IO\request_output()->writeAllAsync(
      "Hello World!\n",
    ); // 'Hello World' 인쇄!
  }

  // 아래 함수에서 컨텍스트 목록은 `defaults` 컨텍스트를 갖도록 정의됩니다.
  // 함수는 여러 컨텍스트 [context1, context2, ...]를 가질 수 있습니다.
  // `defaults`에는 Hack 언어에서 정의한 대부분의 기능이 포함됩니다.
  async function explicit_defaults_context()[defaults]: Awaitable<void> {
    await IO\request_output()->writeAllAsync("Hello World!\n");
  }

  // 순수 함수(기능 없음)를 만들기 위해 0개의 컨텍스트를 지정할 수도 있습니다.
  async function empty_context()[]: Awaitable<void> {
    // 다음 줄은 함수에 IO 기능이 없으므로 오류입니다.
    // await IO\request_output()->writeAllAsync("Hello World!\n");
  }

  /* ==================================
   *             제네릭
   * ==================================
   */

  // 제네릭을 사용하면 클래스 또는 메서드를 모든 유형 집합으로 매개변수화할 수 있습니다.
  // 정말 멋지네요!

  // Hack은 일반적으로 값으로 전달합니다. 참조로 전달하려면 `inout`을 사용하십시오.
  function swap<T>(inout T $input1, inout T $input2): void {
    $temp = $input1;
    $input1 = $input2;
    $input2 = $temp;
  }

  /* ==================================
   *             클래스
   * ==================================
   */

  // 클래스는 기능과 상태를 함께 그룹화하는 방법을 제공합니다.
  // 클래스를 정의하려면 `class` 키워드를 사용하십시오. 인스턴스화하려면 `new`를 사용하십시오.
  // 다른 언어와 마찬가지로 `$this`를 사용하여 현재 인스턴스를 참조할 수 있습니다.

  class Counter {
    private int $i = 0;

    public function increment(): void {
      $this->i += 1;
    }

    public function get(): int {
      return $this->i;
    }
  }

  // 속성 및 메서드는 정적일 수 있습니다(인스턴스화 필요 없음).
  class Person {
    public static function favoriteProgrammingLanguage(): string {
      return "Hack";
    }
  }

  function demo_hack_classes(): void {
    // `new`를 사용하여 클래스를 인스턴스화합니다.
    $c1 = new Counter();

    // 정적 속성 또는 메서드를 호출하려면 `::`를 사용하십시오.
    $typical_person = tuple("Andrew", Person::favoriteProgrammingLanguage());
  }

  // 추상 클래스는 정의할 수 있지만 직접 인스턴스화할 수는 없습니다.
  abstract class Machine {
    public function openDoors(): void {
      return;
    }
    public function closeDoors(): void {
      return;
    }
  }

  /* ==================================
   *             인터페이스
   * ==================================
   */

  // 클래스는 인터페이스를 통해 요구 사항 집합을 구현할 수 있습니다.
  // 인터페이스는 메서드 선언 및 상수 집합입니다.

  interface Plane {
    // 상수는 명명된 값입니다. 정의되면 값을 변경할 수 없습니다.
    const MAX_SPEED = 300;
    public function fly(): void;
  }

  /* ==================================
   *             트레이트
   * ==================================
   */

  // 트레이트는 속성 및 메서드 선언을 정의합니다.
  // 트레이트는 재사용을 위해 코드를 추상화할 때 권장됩니다.
  // 트레이트는 `use` 키워드를 통해 코드에 포함됩니다.

  trait Airplane {
    // 다음 구문을 사용하여 클래스 또는 인터페이스 요구 사항을 도입합니다:
    require extends Machine; // 추상 클래스
    require implements Plane; // 인터페이스

    public function takeOff(): void {
      $this->openDoors();
      $this->closeDoors();
      $this->fly();
    }
  }

  class Spaceship extends Machine implements Plane {
    use Airplane;

    public function fly(): void {
      // 바람처럼 날아라
    }
  }

  /* ==================================
   *             계속 읽기!
   * ==================================
   */

  /*  이것은 단순화된 가이드입니다!
   *  배울 것이 훨씬 더 많습니다. 다음을 포함합니다:
   * - 비동기 작업: https://docs.hhvm.com/hack/asynchronous-operations/introduction
   * - 구체화된 제네릭: https://docs.hhvm.com/hack/reified-generics/reified-generics
   * - XHP: https://docs.hhvm.com/hack/XHP/setup
   * - ... 그리고 더!
   */
}