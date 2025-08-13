---
name: Ada
filename: learn.ada
contributors:
    - ["Luke A. Guest", "https://github.com/Lucretia"]
    - ["Fernando Oleo Blanco", "https://github.com/Irvise"]
    - ["Fabien Chouteau", "https://github.com/Fabien-Chouteau"]
    - ["Manuel", "https://github.com/mgrojo"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Ada는 파스칼/알골 계열의 강력한 정적 타입 명령형, [객체 지향](https://ada-lang.io/docs/arm/AA-3/AA-3.9), [실시간](https://ada-lang.io/docs/arm/AA-D), [병렬](https://ada-lang.io/docs/arm/AA-9) 및 [분산](https://ada-lang.io/docs/arm/AA-9) 프로그래밍 언어이지만, 오늘날에는 ```begin/end``` 키워드 쌍, ```:=``` 할당 기호, 레코드 및 ```if/case``` 제어문 구조만 남아 파스칼과 약간의 유사성만 가집니다.

Ada는 원래 [객체 기반](https://ada-lang.io/docs/arm/AA-3/AA-3.3) 언어로 설계되었으며 미국 정부에서 사용하는 수백 개의 언어를 대체하기 위한 것이었습니다. 이는 모든 엔티티가 객체 지향적 의미가 아닌 객체라는 것을 의미합니다. 이 언어는 1995년에 [객체 지향](https://ada-lang.io/docs/arm/AA-3/AA-3.9)이 되었고, 2005년에는 Java에서 파생된 [인터페이스](https://ada-lang.io/docs/arm/AA-3/AA-3.9#Subclause_3.9.4)를 추가했습니다. [계약 기반](https://ada-lang.io/docs/arm/AA-13/AA-13.1#Subclause_13.1.1) 프로그래밍은 Ada 2012와 함께 도입되었습니다.

Ada는 프로그래머가 아닌 사람(예: 조직 내 관리자)도 쉽게 읽고 배울 수 있도록 설계되었으므로 이 언어로 작성된 프로그램은 약간 더 장황한 경향이 있습니다.

Ada는 현대적인 프로그래밍 언어이며, 이제 다른 현대 언어와 마찬가지로 Alire라는 패키지 관리자가 있습니다. 아래를 참조하십시오.

```ada
--  주석은 이중 하이픈으로 작성되며 줄 끝까지 존재합니다.

--  진입점을 "Main" 또는 "main"으로 호출할 필요는 없으며, 프로그램이
--  수행하는 작업에 따라 이름을 지정해야 합니다.
procedure Empty is
   --  이것은 선언부입니다.
begin
   --  여기에 문장이 들어갑니다.
   null;  --  여기서는 아무것도 하지 않습니다.
end Empty;

--  Ada 컴파일러는 라이브러리 패키지, 태스크, 하위 프로그램, 제네릭 등이 될 수 있는
--  컴파일 단위를 허용합니다.

--  여기에 "컨텍스트 절"이 들어갑니다. 이것은 pragma 또는 "with" 문이 될 수 있습니다.
--  "with"는 다른 언어의 "include" 또는 "import"와 동일합니다.
with Ada.Text_IO;  --  라이브러리 패키지에 대한 접근 권한을 얻습니다.

procedure Hello is
begin
   Ada.Text_IO.Put_Line ("Hello, world");

   Ada.Text_IO.Put ("Hello again, world");
   Ada.Text_IO.New_Line;
end Hello;


--  Ada에는 실제 모듈 시스템이 있습니다. 모듈은 패키지라고 불리며
--  사양과 본문이라는 두 가지 구성 요소로 나뉩니다.
--  처음부터 패키지를 사용하게 되므로 패키지를 일찍 소개하는 것이 중요합니다.
package Stuff is
   --  이 패키지가 "main" 프로시저가 시작되기 전에 코드를 실행할 필요가 없다고
   --  컴파일러에 알리기 위해 다음 줄을 추가할 수 있습니다.
   --  pragma Preelaborate;

   --  패키지는 동일한 파일 내에서 또는 외부에서 중첩될 수 있습니다.
   --  중첩된 패키지는 점 표기법(예: Stuff.Things.My)을 통해 접근합니다.
   package Things is
      My : constant Integer := 100;
   end Things;

   --  사양 내에 하위 프로그램이 선언된 경우, 하위 프로그램의 본문은
   --  패키지 본문 내에 선언되어야 합니다.
   procedure Do_Something;  --  하위 프로그램이 매개변수를 받지 않으면 다른 언어와 달리
                            --  빈 괄호가 필요하지 않습니다.

   --  제네릭 하위 프로그램을 만들 수도 있습니다.
   generic
      type Element is (<>);  --  "(<>)" 표기법은 이산 유형만
                             --  제네릭에 전달될 수 있음을 지정합니다.
   procedure Swap (Left, Right : in out Element);

   --  때로는 외부 세계에서 유형이 어떻게 정의되었는지 숨기고 싶을 때가 있습니다.
   --  아무도 직접 건드릴 수 없도록 말이죠. 전체 유형은 아래의 private 섹션에
   --  정의되어야 합니다.
   type Blobs is private;

   --  "is" 키워드 뒤에 이 키워드를 넣어 유형을 "제한"할 수도 있습니다.
   --  이는 사용자가 일반적으로 할 수 있는 것처럼 해당 유형의 객체를
   --  복사할 수 없음을 의미합니다.
private
   type Blobs is new Integer range -25 .. 25;
end Stuff;


package body Stuff is
   --  하위 프로그램 본문.
   procedure Do_Something is
      --  하위 프로그램을 중첩할 수도 있습니다.
      --  매개변수는 이동 방향(in, in out, out)으로 정의됩니다.
      --  이동 방향이 지정되지 않으면 기본적으로 in입니다.
      function Times_4 (Value : in Integer) return Integer is
      begin
         return Value * 4;
      end Times_4;

      I : Integer := 4;
   begin
      I := Times_4 (I);
   end Do_Something;


   --  제네릭 프로시저 본문.
   procedure Swap (Left, Right : in out Element) is
      Temp : Element := Left;
   begin
      Left  := Right;
      Right := Temp;
   end Swap;
begin
   --  패키지 내에서 무언가를 초기화해야 하는 경우 여기에서 할 수 있습니다.
   Do_Something;
end Stuff;


with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Stuff;

procedure LearnAdaInY is
   --  들여쓰기는 3칸입니다.

   --  Ada에서 가장 중요한 기능은 유형입니다. 객체에는 유형이 있으며
   --  한 유형의 객체는 다른 유형의 객체에 할당될 수 없습니다.

   --  모델링하는 도메인에 대해 자신만의 유형을 정의할 수 있으며, 그렇게 해야 합니다.
   --  하지만 표준 유형으로 시작한 다음 나중에 자신만의 유형으로 바꿀 수 있습니다.
   --  이것은 점진적 타이핑의 한 형태라고 할 수 있습니다.

   --  표준 유형은 C와 같은 다른 언어에 바인딩하기 위한 좋은 출발점일 뿐입니다.
   --  Ada는 C, Fortran 및 COBOL과 바인딩하는 표준화된 방법을 가진 유일한 언어입니다!
   --  이러한 언어에 대한 바인딩에 대한 자세한 내용은 참고 자료 섹션의 링크를 참조하십시오.

   type Degrees is range 0 .. 360;  --  이것은 유형입니다. 기본 표현은
                                    --  정수입니다.

   type Hues is (Red, Green, Blue, Purple, Yellow);  --  이것도 마찬가지입니다. 여기서는
                                                     --  열거형을 선언하고 있습니다.

   --  이것은 모듈러 유형입니다. 자동으로 랩어라운드되는 정수처럼 동작합니다.
   --  이 특정 경우의 범위는 0 .. 359입니다.
   --  값 359를 포함하는 변수에 1을 더하면 0을 받게 됩니다.
   --  배열에 매우 유용합니다.
   type Degrees_Wrap is mod 360;

   --  하위 유형을 사용하여 유형의 범위를 제한할 수 있습니다. 이렇게 하면 서로 호환됩니다.
   --  즉, 아래에서 볼 수 있듯이 하위 유형을 유형의 객체에 할당할 수 있습니다.
   subtype Primaries is Hues range Red .. Blue;  --  이것은 범위입니다.

   --  다음과 같이 변수나 상수를 정의할 수 있습니다.
   --  Var_Name : Type := Value;

   --  10은 보편적인 정수입니다. 이러한 보편적인 숫자는 기본 유형과 일치하는
   --  모든 유형과 함께 사용할 수 있습니다.
   Angle : Degrees := 10;
   Value : Integer := 20;
   --  New_Angle : Degrees := Value;  --  호환되지 않는 유형은 컴파일되지 않습니다.
   --  New_Value : Integer := Angle;

   Blue_Hue   :          Primaries := Blue;  --  변수.
   Red_Hue    : constant Primaries := Red;   --  상수.
   Yellow_Hue : constant Hues      := Yellow;
   Colour_1   : constant Hues      := Red_Hue;
   --  Colour_2   : constant Primaries := Yellow_Hue;  --  컴파일하려면 주석을 해제하십시오.

   --  변환을 강제할 수 있지만, 그러면 패키지 이름으로 안전하지 않은 작업을
   --  수행하고 있을 수 있다는 경고를 받게 됩니다.
   function Degrees_To_Int is new Ada.Unchecked_Conversion
     (Source => Degrees,   --  줄 연속은 2칸 들여쓰기됩니다.
      Target => Integer);

   New_Value_2 : Integer := Degrees_To_Int (Angle);  --  참고, ( 앞에 공백.

   --  GNAT는 GNU Ada 번역기(컴파일러)입니다.
   --  Ada에는 스타일 가이드가 있으며 GNAT는 이를 준수하도록 경고하고,
   --  모든 Ada 소스가 일관되게 보이도록 스타일을 확인하는 옵션이 있습니다.
   --  그러나 스타일은 사용자 정의할 수 있습니다.

   --  예, 자신만의 부동 소수점 및 고정 소수점 유형을 정의할 수도 있습니다.
   --  이것은 매우 드물고 독특한 능력입니다. "digits"는 유형이 지원해야 하는
   --  최소 자릿수 정밀도를 나타냅니다. "delta"는 고정 소수점 유형에 대한 것이며
   --  유형이 지원할 가장 작은 변경을 나타냅니다.
   type Real_Angles is digits 3 range 0.0 .. 360.0;
   type Fixed_Angles is delta 0.01 digits 5 range 0.0 .. 360.0;

   RA : constant Real_Angles  := 36.45;
   FA : constant Fixed_Angles := 360.0;  --  부동 소수점으로 만들기 위해 ".0".

   --  기본적으로 일반적인 라틴 1 기반 문자열을 가질 수 있습니다.
   Str  : constant String    := "This is a constant string";
   --  문자열 리터럴에서 초기화할 때 컴파일러는 범위를 알고 있으므로
   --  정의할 필요가 없습니다.

   --  문자열은 배열입니다. 괄호를 사용하여 배열의 요소에 접근하는 방법을
   --  보셨습니까? 이것은 수학적 표기법이며 Ada가 만들어질 당시 모든 키보드에서
   --  대괄호를 사용할 수 없었기 때문에 사용되었습니다. 또한 배열은 수학적 관점에서
   --  함수로 볼 수 있으므로 배열과 함수 간의 변환이 더 쉬워졌습니다.
   Char : constant Character := Str (Str'First);  --  "'First"는 유형 속성입니다.

   --  Ada 2022에는 Ada 2012에 추가된 컨테이너를 사용할 때 배열 초기화를 위해
   --  []를 사용하는 것이 포함됩니다.

   --  배열은 일반적으로 항상 유형으로 정의됩니다.
   --  어떤 차원이든 될 수 있습니다.
   type My_Array_1 is array (1 .. 4, 3 .. 7, -20 .. 20) of Integer;

   --  예, 다른 언어와 달리 열거형 및 모듈러 유형 또는 임의의 범위와 같은
   --  다른 이산 유형으로 배열을 인덱싱할 수 있습니다.
   type Axes is (X, Y, Z);

   --  다른 유형의 'Range 속성을 사용하여 배열의 범위를 정의할 수 있습니다.
   type Vector is array (Axes'Range) of Float;

   V1 : constant Vector := (0.0, 0.0, 1.0);

   --  레코드는 C, C++의 구조체와 동일합니다.
   type Entities is record
      Name     : String (1 .. 10);  --  항상 양수 값에서 시작하며,
                                    --  포함 범위입니다.
      Position : Vector;
   end record;

   --  Ada에서 배열 범위는 변경할 수 없습니다. 따라서 모든 문자에 대해
   --  값이 있는 문자열 리터럴을 제공해야 합니다.
   E1 : constant Entities := ("Blob      ", (0.0, 0.0, 0.0));

   --  대안은 배열 집계를 사용하고 이 집계에서 이전에 할당되지 않은
   --  모든 요소에 기본값을 할당하는 것입니다.
   --  "others"는 명시적으로 초기화되지 않은 다른 모든 것을 나타내는 데 사용됩니다.
   E2 : constant Entities := (('B', 'l', 'o', 'b', others => ' '),
                              (0.0, 0.0, 0.0));

   --  표준 라이브러리에는 동적 길이 문자열(참고 자료 섹션 참조)이 있습니다.

   --  상자 표기법 "<>"을 사용하여 객체를 기본값으로 초기화할 수 있습니다.
   --  "others"는 명시적으로 초기화되지 않은 다른 모든 것을 나타내는 데 사용됩니다.
   Null_Entity : constant Entities := (others => <>);

   --  객체 지향은 레코드 구문의 확장인 태그가 지정된 레코드를 통해 수행됩니다.
   --  위의 첫 번째 단락에 있는 링크를 참조하십시오.

   --  가독성을 높이기 위해 객체 이름(별칭)을 바꿀 수 있습니다.
   package IO renames Ada.Text_IO;
begin
   --  열거형을 이름으로 출력할 수 있습니다.
   IO.Put_Line ("Blue_Hue   = " &  --  &는 문자열 연결 연산자입니다.
                Blue'Image);       --  '는 객체의 속성에 접근합니다.
                  --  Image 속성은 값을 문자열로 변환합니다.
                  --  Ada 2022는 Image를 사용자 정의 유형으로 확장했습니다.
                  --  -gnat2022 컴파일러 플래그로 이것에 접근하십시오.
   IO.Put_Line ("Yellow_Hue = " &
                --  유형의 속성을 사용할 수 있습니다.
                Primaries'Image (Yellow_Hue));

   --  선언 블록 내에 지역 변수를 정의할 수 있으며, 레이블을 지정하여
   --  더 읽기 쉽게 만들 수 있습니다.
   Enum_IO : declare
      package Hue_IO is new IO.Enumeration_IO (Hues);

      --  패키지를 사용하면 해당 패키지 내의 모든 것이 이 블록 내에서
      --  보이게 됩니다. 이것을 컨텍스트 절 내의 전체 패키지가 아닌
      --  로컬에서만 수행하는 것이 좋습니다.
      use Hue_IO;
   begin
      --  열거형 값도 출력할 수 있습니다.
      Put (Purple); --  Put 프로시저 앞에 Hue_IO를 붙일 필요가 없습니다.
      IO.New_Line;  --  여기서는 여전히 IO를 접두사로 붙여야 합니다.
      Put (Red_Hue);
      IO.New_Line;
   end Enum_IO;

   --  루프는 일관된 형태를 가집니다. "<form> loop ... end loop".
   --  여기서 "form"은 "while" 또는 "for"이거나 아래와 같이 없을 수 있습니다.
   --  "loop ... end loop;" 구문을 자체 줄에 배치하면
   --  다른 루프 구문을 더 쉽게 주석 처리하거나 실험할 수 있습니다.
   declare
      Counter : Positive := Positive'First;  --  이것은 1입니다.
   begin
      --  필요한 경우 더 쉽게 빠져나올 수 있도록 루프에 레이블을 지정할 수 있습니다.
      Infinite :
      loop
         IO.Put_Line ("[Infinite loop] Counter = " & Counter'Image);

         Counter := Counter + 1;

         --  다음 줄은 repeat ... until 또는 do ... while 루프 구문을 구현합니다.
         --  무한 루프를 위해 주석 처리하십시오.
         exit Infinite when Counter = 5;  --  등호 테스트는 단일 "="를 사용합니다.
      end loop Infinite;  --  상태 머신을 구현할 때 유용합니다.
   end;

   declare  --  레이블이 없어도 됩니다.
      Counter : Positive := Positive'First;  --  이것은 1입니다.
   begin
      while Counter < 10
      loop
         IO.Put_Line ("Counter = " & Counter'Image);

         Counter := Counter + 1;  --  명시적인 증감 연산자는 없습니다.

         --  Ada 2022는 LHS에 @를 도입했으므로 위는 다음과 같이 작성됩니다.
         --  Counter := @ + 1;  --  -gnat2022로 시도해보십시오.
      end loop;
   end;

   declare
      package Hue_IO is new IO.Enumeration_IO (Hues);

      --  한 줄에 여러 패키지를 가질 수 있지만, 가독성을 위해
      --  한 줄에 하나의 패키지를 사용하는 경향이 있습니다.
      use IO, Hue_IO;
   begin
      Put ("Hues : ");  --  참고, 접두사 없음.

      --  'Range 속성을 사용하고 있기 때문에 컴파일러는 이것이 안전하다는 것을
      --  알고 있으며 여기서 런타임 검사를 생략할 수 있습니다.
      for Hue in Hues'Range
      loop
         Put (Hue);

         --  유형과 객체는 자신의 범위, 즉 First .. Last 값을 알고 있습니다.
         --  이것은 범위 유형으로 지정할 수 있습니다.
         if Hue /= Hues'Last then  --  /=는 수학 기호 ≠와 같이 "같지 않음"을 의미합니다.
            Put (", ");
         end if;
      end loop;

      IO.New_Line;
   end;

   --  문자열을 포함한 모든 객체는 자신의 범위를 알고 있습니다.
   declare
      C : Character := Str (50);  --  경고가 발생하고 런타임에 예외가 발생합니다.
                                  --
      --  위에서 발생한 예외는 외부 범위에서만 처리할 수 있습니다.
      --  아래의 위키북 링크를 참조하십시오.
   begin
      null;  --  위의 이유로 이 지점에는 절대 도달하지 못합니다.
   end;
exception
   when Constraint_Error =>
      IO.Put_Line ("Caught the exception");
end LearnAdaInY;
```

이제 Ada에 대한 기본 소개를 위해 많은 정보를 다루었지만, 아직 표면만 훑었을 뿐입니다. 아래 참고 자료 섹션에는 더 많은 내용이 있습니다. 동적 메모리 할당에 대해서는 아직 다루지 않았는데, 여기에는 [풀](https://ada-lang.io/docs/arm/AA-13/AA-13.11)이 포함됩니다. 이는 대부분의 Ada 프로그램이 필요로 하지 않기 때문이며, 그것 없이도 많은 것을 할 수 있습니다.

위에서 언급했듯이, Ada는 파스칼과 거의 닮지 않았으며, 원래의 [Green 사양](https://apps.dtic.mil/sti/trecms/pdf/ADB950587.pdf) (경고: 4575페이지 분량의 거대한 스캔 PDF - 460페이지부터 시작)을 보면 전혀 닮지 않았음을 알 수 있습니다(해당 PDF의 505페이지).

위의 소스 코드는 컴파일되지만, 강력한 정적 타입 시스템의 힘을 보여주는 경고도 발생합니다.

## 이 소스 다운로드

GNAT 툴체인이 이미 설치되어 있다면, 위의 코드를 새 파일(예: ```learn-ada-in-y.ada```)에 복사하여 붙여넣은 다음 다음을 실행할 수 있습니다.

```bash
$ gnatchop learn-ada-in-y.ada # 이것은 프로그램을 사양 ".ads"와 본문 ".adb"로 나눕니다.
$ gnatmake empty.adb # gnatmake는 모든 단위의 컴파일과 링크를 처리합니다.
$ gnatmake hello.adb
$ gnatmake learnadainy.adb
```

또는 [Alire](https://alire.ada.dev)를 다운로드하여 PATH의 어딘가에 복사한 다음 다음을 수행하십시오.

**참고** Alire는 툴체인이 설치되어 있지 않은 경우 자동으로 설치하며 사용할 툴체인을 선택하도록 요청합니다.

```bash
$ alr search learnadainy
$ alr get learnadainy
$ cd learnadainy
$ alr run empty
$ alr run hello
$ alr run learnadainy
```

## 더 읽을거리

* [Ada 프로그래밍 언어](https://ada-lang.io)
* [Ada 2022 참조 매뉴얼](https://ada-lang.io/docs/arm)
* [Ada 스타일 가이드](https://ada-lang.io/docs/style-guide/Ada_Style_Guide)
* [AdaCore 사이트에서 더 많은 Ada/Spark 배우기](https://learn.adacore.com)

## 위 소스의 참고 자료

1. [위키북](https://en.wikibooks.org/wiki/Ada_Programming/Exceptions#Exception_handlers)
2. [C](https://ada-lang.io/docs/arm/AA-B/AA-B.3)
3. [Fortran](https://ada-lang.io/docs/arm/AA-B/AA-B.5/)
4. [COBOL](https://ada-lang.io/docs/arm/AA-B/AA-B.4/)
5. [동적 길이 문자열](https://ada-lang.io/docs/arm/AA-A/AA-A.4#Subclause_A.4.5)

### 여러 줄 주석

여러 줄 주석은 오류가 발생하기 쉬우므로 허용되지 않습니다.

> 이러한 주석은 닫는 주석 구분 기호가 필요하며, 이는 (의도하지 않은) 닫는 구분 기호 생략과 관련된 위험을 다시 제기합니다. 프로그램의 전체 섹션이 프로그래머가 깨닫지 못한 채 컴파일러에 의해 무시될 수 있습니다.
>
> [Ada 83 Rationale](http://archive.adaic.com/standards/83rat/html/ratl-02-01.html#2.1)
