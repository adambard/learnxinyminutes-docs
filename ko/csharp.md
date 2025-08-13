---
name: C#
contributors:
    - ["Irfan Charania", "https://github.com/irfancharania"]
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Melvyn Laïly", "http://x2a.yt"]
    - ["Shaun McCarthy", "http://www.shaunmccarthy.com"]
    - ["Wouter Van Schandevijl", "http://github.com/laoujin"]
    - ["Jo Pearce", "http://github.com/jdpearce"]
    - ["Chris Zimmerman", "https://github.com/chriszimmerman"]
    - ["Shawn McGuire", "https://github.com/bigbash"]
filename: LearnCSharp.cs
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

C#은 개발자가 크로스 플랫폼 .NET 프레임워크에서 실행되는 다양하고 안전하며 견고한 애플리케이션을 구축할 수 있도록 하는 우아하고 타입 안전한 객체 지향 언어입니다.

[여기에서 더 읽어보십시오.](https://docs.microsoft.com/en-us/dotnet/csharp/tour-of-csharp/)

```csharp
// 한 줄 주석은 //로 시작합니다.

/*
여러 줄 주석은 이렇습니다.
*/

/// <summary>
/// 이것은 외부 문서를 생성하거나 IDE 내에서 컨텍스트 도움말을 제공하는 데 사용할 수 있는 XML 문서 주석입니다.
/// </summary>
/// <param name="firstParam">이것은 firstParam에 대한 매개변수 문서입니다.</param>
/// <returns>함수의 반환 값에 대한 정보</returns>
public void MethodOrClassOrOtherWithParsableHelp(string firstParam) { }

// 이 소스 코드가 사용할 네임스페이스를 지정합니다.
// 아래 네임스페이스는 모두 표준 .NET Framework 클래스 라이브러리의 일부입니다.
using System;
using System.Collections.Generic;
using System.Dynamic;
using System.Linq;
using System.Net;
using System.Threading.Tasks;
using System.IO;

// 하지만 이것은 아닙니다:
using System.Data.Entity;
// 사용하려면 dll 참조를 추가해야 합니다.
// NuGet 패키지 관리자를 사용하여 수행할 수 있습니다: `Install-Package EntityFramework`

// 네임스페이스는 코드를 "패키지" 또는 "모듈"로 구성하기 위한 범위를 정의합니다.
// 다른 소스 파일에서 이 코드를 사용하려면: using Learning.CSharp;

// C# 10에서도 이 작업을 수행할 수 있습니다. 파일 범위 네임스페이스라고 합니다.
// namespace Learning.CSharp;

namespace Learning.CSharp
{
    // 각 .cs 파일에는 파일과 동일한 이름을 가진 클래스가 하나 이상 포함되어야 합니다.
    // 그렇지 않아도 되지만, 정신 건강을 위해 그렇게 하지 않는 것이 좋습니다.
    public class LearnCSharp
    {
        // 기본 구문 - 이전에 Java 또는 C++를 사용해 본 적이 있다면 흥미로운 기능으로 건너뛰십시오.
        public static void Syntax()
        {
            // Console.WriteLine을 사용하여 줄을 인쇄합니다.
            Console.WriteLine("Hello World");
            Console.WriteLine(
                "Integer: " + 10 +
                " Double: " + 3.14 +
                " Boolean: " + true);

            // 새 줄 없이 인쇄하려면 Console.Write를 사용하십시오.
            Console.Write("Hello ");
            Console.Write("World");

            ///////////////////////////////////////////////////
            // 유형 및 변수
            //
            // <type> <name>을 사용하여 변수를 선언합니다.
            ///////////////////////////////////////////////////

            // Sbyte - 부호 있는 8비트 정수
            // (-128 <= sbyte <= 127)
            sbyte fooSbyte = 100;

            // Byte - 부호 없는 8비트 정수
            // (0 <= byte <= 255)
            byte fooByte = 100;

            // Short - 16비트 정수
            // 부호 있는 - (-32,768 <= short <= 32,767)
            // 부호 없는 - (0 <= ushort <= 65,535)
            short fooShort = 10000;
            ushort fooUshort = 10000;

            // Integer - 32비트 정수
            int fooInt = 1; // (-2,147,483,648 <= int <= 2,147,483,647)
            uint fooUint = 1; // (0 <= uint <= 4,294,967,295)

            // Long - 64비트 정수
            long fooLong = 100000L; // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
            ulong fooUlong = 100000L; // (0 <= ulong <= 18,446,744,073,709,551,615)
            // 숫자는 크기에 따라 int 또는 uint로 기본 설정됩니다.
            // L은 이 변수 값이 long 또는 ulong 유형임을 나타내는 데 사용됩니다.

            // Double - 배정밀도 64비트 IEEE 754 부동 소수점
            double fooDouble = 123.4; // 정밀도: 15-16자리

            // Float - 단정밀도 32비트 IEEE 754 부동 소수점
            float fooFloat = 234.5f; // 정밀도: 7자리
            // f는 이 변수 값이 float 유형임을 나타내는 데 사용됩니다.

            // Decimal - 128비트 데이터 유형으로, 다른 부동 소수점 유형보다 정밀도가 높으며,
            // 금융 및 통화 계산에 적합합니다.
            decimal fooDecimal = 150.3m;

            // Boolean - true & false
            bool fooBoolean = true; // 또는 false

            // Char - 단일 16비트 유니코드 문자
            char fooChar = 'A';

            // 문자열 -- 이전 기본 유형은 모두 값 유형이지만,
            // 문자열은 참조 유형입니다. 즉, null로 설정할 수 있습니다.
            string fooString = "\"escape\" quotes and add \n (new lines) and \t (tabs)";
            Console.WriteLine(fooString);

            // 인덱서를 사용하여 문자열의 각 문자에 액세스할 수 있습니다:
            char charFromString = fooString[1]; // => 'e'
            // 문자열은 변경할 수 없습니다: fooString[1] = 'X';를 수행할 수 없습니다.

            // 현재 문화권으로 문자열 비교, 대소문자 구분 안 함
            string.Compare(fooString, "x", StringComparison.CurrentCultureIgnoreCase);

            // sprintf를 기반으로 한 형식 지정
            string fooFs = string.Format("Check Check, {0} {1}, {0} {1:0.0}", 1, 2);

            // 날짜 및 형식 지정
            DateTime fooDate = DateTime.Now;
            Console.WriteLine(fooDate.ToString("hh:mm, dd MMM yyyy"));

            // 문자 그대로의 문자열
            // 문자열 리터럴 앞에 @ 기호를 사용하여 문자열의 모든 문자를 이스케이프할 수 있습니다.
            string path = "C:\\Users\\User\\Desktop";
            string verbatimPath = @"C:\Users\User\Desktop";
            Console.WriteLine(path == verbatimPath);  // => true

            // @ 기호를 사용하여 문자열을 두 줄로 나눌 수 있습니다. "를 이스케이프하려면 ""를 사용하십시오.
            string bazString = @"Here's some stuff
on a new line! ""Wow!"", the masses cried";

            // 변수를 변경할 수 없게 만들려면 const 또는 read-only를 사용하십시오.
            // const 값은 컴파일 타임에 계산됩니다.
            const int HoursWorkPerWeek = 9001;

            ///////////////////////////////////////////////////
            // 데이터 구조
            ///////////////////////////////////////////////////

            // 배열 - 0부터 인덱싱
            // 배열 크기는 선언 시 결정되어야 합니다.
            // 배열을 선언하는 형식은 다음과 같습니다.
            // <datatype>[] <var name> = new <datatype>[<array size>];
            int[] intArray = new int[10];

            // 배열을 선언하고 초기화하는 또 다른 방법
            int[] y = { 9000, 1000, 1337 };

            // 배열 인덱싱 - 요소 액세스
            Console.WriteLine("intArray @ 0: " + intArray[0]);
            // 배열은 변경 가능합니다.
            intArray[1] = 1;

            // 목록
            // 목록은 더 유연하므로 배열보다 더 자주 사용됩니다.
            // 목록을 선언하는 형식은 다음과 같습니다.
            // List<datatype> <var name> = new List<datatype>();
            List<int> intList = new List<int>();
            List<string> stringList = new List<string>();
            List<int> z = new List<int> { 9000, 1000, 1337 }; // 초기화
            // <>는 제네릭용입니다 - 멋진 기능 섹션을 확인하십시오.

            // 목록은 기본적으로 값을 가지지 않습니다.
            // 인덱스에 액세스하기 전에 값을 추가해야 합니다.
            intList.Add(1);
            Console.WriteLine("intList at 0: " + intList[0]);

            // 확인할 다른 데이터 구조:
            // 스택/큐
            // 사전(해시 맵 구현)
            // HashSet
            // 읽기 전용 컬렉션
            // 튜플(.NET 4+)

            ///////////////////////////////////////
            // 연산자
            ///////////////////////////////////////
            Console.WriteLine("\n->연산자");

            int i1 = 1, i2 = 2; // 여러 선언에 대한 약어

            // 산술은 간단합니다.
            Console.WriteLine(i1 + i2 - i1 * 3 / 7); // => 3

            // 모듈로
            Console.WriteLine("11%3 = " + (11 % 3)); // => 2

            // 비교 연산자
            Console.WriteLine("3 == 2? " + (3 == 2)); // => false
            Console.WriteLine("3 != 2? " + (3 != 2)); // => true
            Console.WriteLine("3 > 2? " + (3 > 2)); // => true
            Console.WriteLine("3 < 2? " + (3 < 2)); // => false
            Console.WriteLine("2 <= 2? " + (2 <= 2)); // => true
            Console.WriteLine("2 >= 2? " + (2 >= 2)); // => true

            // 비트 연산자!
            /*
            ~\t단항 비트 보수
            <<\t부호 있는 왼쪽 시프트
            >>\t부호 있는 오른쪽 시프트
            &\t비트 AND
            ^\t비트 배타적 OR
            |\t비트 포함 OR
            */

            // 증가
            int i = 0;
            Console.WriteLine("\n->증감");
            Console.WriteLine(i++); // "0" 인쇄, i = 1. 후위 증가
            Console.WriteLine(++i); // "2" 인쇄, i = 2. 전위 증가
            Console.WriteLine(i--); // "2" 인쇄, i = 1. 후위 감소
            Console.WriteLine(--i); // "0" 인쇄, i = 0. 전위 감소

            ///////////////////////////////////////
            // 제어 구조
            ///////////////////////////////////////
            Console.WriteLine("\n->제어 구조");

            // If 문은 C와 유사합니다.
            int j = 10;
            if (j == 10)
            {
                Console.WriteLine("I get printed");
            }
            else if (j > 10)
            {
                Console.WriteLine("I don't");
            }
            else
            {
                Console.WriteLine("I also don't");
            }

            // 삼항 연산자
            // 간단한 if/else는 다음과 같이 작성할 수 있습니다.
            // <condition> ? <true> : <false>
            int toCompare = 17;
            string isTrue = toCompare == 17 ? "True" : "False";

            // While 루프
            int fooWhile = 0;
            while (fooWhile < 100)
            {
                // 100번 반복, fooWhile 0->99
                fooWhile++;
            }

            // Do While 루프
            int fooDoWhile = 0;
            do
            {
                // 100번 반복 시작, fooDoWhile 0->99
                if (false)
                    continue; // 현재 반복 건너뛰기

                fooDoWhile++;

                if (fooDoWhile == 50)
                    break; // 루프에서 완전히 벗어나기

            } while (fooDoWhile < 100);

            // for 루프 구조 => for(<start_statement>; <conditional>; <step>)
            for (int fooFor = 0; fooFor < 10; fooFor++)
            {
                // 10번 반복, fooFor 0->9
            }

            // For Each 루프
            // foreach 루프 구조 => foreach(<iteratorType> <iteratorName> in <enumerable>)
            // foreach 루프는 IEnumerable 또는 IEnumerable<T>를 구현하는 모든 객체를 반복합니다.
            // .NET 프레임워크의 모든 컬렉션 유형(Array, List, Dictionary...)은 이러한 인터페이스 중 하나 또는 둘 다를 구현합니다.
            // (ToCharArray()는 문자열도 IEnumerable을 구현하므로 제거할 수 있습니다.)
            foreach (char character in "Hello World".ToCharArray())
            {
                // 문자열의 모든 문자를 반복합니다.
            }

            // Switch Case
            // Switch는 byte, short, char 및 int 데이터 유형과 함께 작동합니다.
            // 또한 열거형(열거형 유형에서 논의됨), String 클래스 및 기본 유형을 래핑하는 몇 가지 특수 클래스(Character, Byte, Short 및 Integer)와 함께 작동합니다.
            int month = 3;
            string monthString;
            switch (month)
            {
                case 1:
                    monthString = "January";
                    break;
                case 2:
                    monthString = "February";
                    break;
                case 3:
                    monthString = "March";
                    break;
                // 여러 케이스를 하나의 작업에 할당할 수 있습니다.
                // 그러나 다른 케이스 앞에 break 없이 작업을 추가할 수 없습니다.
                // (이렇게 하려면 명시적으로 goto case x를 추가해야 합니다.)
                case 6:
                case 7:
                case 8:
                    monthString = "Summer time!!";
                    break;
                default:
                    monthString = "Some other month";
                    break;
            }

            ///////////////////////////////////////
            // 데이터 유형 변환 및 유형 캐스팅
            ///////////////////////////////////////

            // 데이터 변환

            // 문자열을 정수로 변환
            // 실패 시 FormatException을 throw합니다.
            int.Parse("123"); // "123"의 정수 버전을 반환합니다.

            // TryParse는 실패 시 유형의 기본값으로 기본 설정됩니다.
            // 이 경우 0입니다.
            int tryInt;
            if (int.TryParse("123", out tryInt)) // 함수는 부울입니다.
                Console.WriteLine(tryInt);       // 123

            // 정수를 문자열로 변환
            // Convert 클래스에는 변환을 용이하게 하는 여러 메서드가 있습니다.

            // 문자열을 int로

            // 더 나은 방법
            bool result = int.TryParse(string, out var integer)
            int.Parse(string);

            // 권장하지 않음
            Convert.ToString(123);

            // Int를 문자열로
            tryInt.ToString();

            // 캐스팅
            // 십진수 15를 int로 캐스팅
            // 그런 다음 long으로 암시적으로 캐스팅
            long x = (int) 15M;
        }

        ///////////////////////////////////////
        // 클래스 - 파일 끝의 정의 참조
        ///////////////////////////////////////
        public static void Classes()
        {
            // 파일 끝의 객체 선언 참조

            // new를 사용하여 클래스를 인스턴스화합니다.
            Bicycle trek = new Bicycle();

            // 객체 메서드 호출
            trek.SpeedUp(3); // 항상 setter 및 getter 메서드를 사용해야 합니다.
            trek.Cadence = 100;

            // ToString은 이 객체의 값을 표시하는 규칙입니다.
            Console.WriteLine("trek info: " + trek.Info());

            // 새 Penny Farthing 인스턴스화
            PennyFarthing funbike = new PennyFarthing(1, 10);
            Console.WriteLine("funbike info: " + funbike.Info());

            Console.Read();
        } // 주 메서드 끝

        // C# 9 이상에서 사용 가능하며, 기본적으로 클래스에 대한 구문 설탕입니다. 레코드는 불변입니다*.
        public record ARecord(string Csharp);

        // 콘솔 진입 - 콘솔 애플리케이션은 진입점으로 main 메서드를 가져야 합니다.
        public static void Main(string[] args)
        {
            OtherInterestingFeatures();
        }

        //
        // 흥미로운 기능
        //

        // 기본 메서드 시그니처

        public // 가시성
        static // 객체 없이 클래스에서 직접 호출 가능
        int // 반환 유형,
        MethodSignatures(
            int maxCount, // 첫 번째 변수, int 예상
            int count = 0, // 전달되지 않으면 값을 0으로 기본 설정
            int another = 3,
            params string[] otherParams // 메서드에 전달된 다른 모든 매개변수 캡처
        )
        {
            return -1;
        }

        // 메서드는 시그니처가 고유한 한 동일한 이름을 가질 수 있습니다.
        // 반환 유형만 다른 메서드는 고유하지 않습니다.
        public static void MethodSignatures(
            ref int maxCount, // 참조로 전달
            out int count)
        {
            // 'count'로 전달된 인수는 이 함수 외부에서 15의 값을 가집니다.
            count = 15; // out 매개변수는 제어가 메서드를 떠나기 전에 할당되어야 합니다.
        }

        // 제네릭
        // TKey 및 TValue에 대한 클래스는 이 함수를 호출하는 사용자가 지정합니다.
        // 이 메서드는 Python의 dict.setdefault()를 에뮬레이트합니다.
        public static TValue SetDefault<TKey, TValue>(
            IDictionary<TKey, TValue> dictionary,
            TKey key,
            TValue defaultItem)
        {
            TValue result;
            if (!dictionary.TryGetValue(key, out result))
                return dictionary[key] = defaultItem;
            return result;
        }

        // 위에서 제네릭으로 정의된 SETDEFAULT를 호출합니다.
        // 암시적으로 파생될 수 있으므로 TKey 및 TValue를 지정할 필요가 없습니다.
        Console.WriteLine(SetDefault<string,string>(phonebook, "Shaun", "No Phone")); // 전화 없음
        // 참고, TKey 및 TValue를 지정할 필요가 없습니다. 암시적으로 파생될 수 있기 때문입니다.
        Console.WriteLine(SetDefault(phonebook, "Sarah", "No Phone")); // 212 555 5555

        // 람다 표현식 - 인라인으로 코드를 작성할 수 있습니다.
        Func<int, int> square = (x) => x * x; // 마지막 T 항목은 반환 값입니다.
        Console.WriteLine(square(3)); // 9

        // 오류 처리 - 불확실한 세상에 대처하기
        try
        {
            var funBike = PennyFarthing.CreateWithGears(6);

            // CreateWithGears가 예외를 throw하므로 더 이상 실행되지 않습니다.
            string some = "";
            if (true) some = null;
            some.ToLower(); // NullReferenceException을 throw합니다.
        }
        catch (NotSupportedException)
        {
            Console.WriteLine("Not so much fun now!");
        }
        catch (Exception ex) // 다른 모든 예외를 잡습니다.
        {
            throw new ApplicationException("It hit the fan", ex);
            // throw; // 호출 스택을 보존하는 다시 throw
        }
        // catch { } // 예외를 캡처하지 않는 catch-all
        finally
        {
            // try 또는 catch 후에 실행됩니다.
        }

        // 일회용 리소스 관리 - 관리되지 않는 리소스를 쉽게 처리할 수 있습니다.
        // 관리되지 않는 리소스(파일 핸들, 장치 컨텍스트 등)에 액세스하는 대부분의 객체는 IDisposable 인터페이스를 구현합니다.
        // using 문은 이러한 IDisposable 객체를 정리하는 것을 처리합니다.
        using (StreamWriter writer = new StreamWriter("log.txt"))
        {
            writer.WriteLine("Nothing suspicious here");
            // 범위 끝에서 리소스가 해제됩니다.
            // 예외가 발생하더라도.
        }

        // 병렬 프레임워크
        // https://devblogs.microsoft.com/csharpfaq/parallel-programming-in-net-framework-4-getting-started/

        var words = new List<string> {"dog", "cat", "horse", "pony"};

        Parallel.ForEach(words,
            new ParallelOptions() { MaxDegreeOfParallelism = 4 },
            word =>
            {
                Console.WriteLine(word);
            }
        );

        // 이것을 실행하면 다른 출력이 생성됩니다.
        // 각 스레드가 다른 시간에 완료되기 때문입니다.
        // 몇 가지 예제 출력은 다음과 같습니다:
        // cat dog horse pony
        // dog horse pony cat

        // 동적 객체(다른 언어와 함께 작업하는 데 좋습니다.)
        dynamic student = new ExpandoObject();
        student.FirstName = "First Name"; // 먼저 클래스를 정의할 필요가 없습니다!

        // 메서드를 추가할 수도 있습니다(문자열을 반환하고 문자열을 받습니다).
        student.Introduce = new Func<string, string>(
            (introduceTo) => string.Format("Hey {0}, this is {1}", student.FirstName, introduceTo));
        Console.WriteLine(student.Introduce("Beth"));

        // IQUERYABLE<T> - 거의 모든 컬렉션이 이를 구현하므로 매우 유용한 Map / Filter / Reduce 스타일 메서드를 많이 사용할 수 있습니다.
        var bikes = new List<Bicycle>();
        bikes.Sort(); // 배열 정렬
        bikes.Sort((b1, b2) => b1.Wheels.CompareTo(b2.Wheels)); // 바퀴를 기준으로 정렬
        var result = bikes
            .Where(b => b.Wheels > 3) // 필터 - 연결 가능(이전 유형의 IQueryable 반환)
            .Where(b => b.IsBroken && b.HasTassles)
            .Select(b => b.ToString()); // 맵 - 이것만 선택하므로 결과는 IQueryable<string>입니다.

        var sum = bikes.Sum(b => b.Wheels); // Reduce - 컬렉션의 모든 바퀴를 합산합니다.

        // 자전거의 일부 매개변수를 기반으로 암시적 객체 목록을 만듭니다.
        var bikeSummaries = bikes.Select(b=>new { Name = b.Name, IsAwesome = !b.IsBroken && b.HasTassles });
        // 여기서 보여주기는 어렵지만, 컴파일러가 위에서 유형을 암시적으로 파악할 수 있으므로 유형 자동 완성 기능을 사용할 수 있습니다!
        foreach (var bikeSummary in bikeSummaries.Where(b => b.IsAwesome))
            Console.WriteLine(bikeSummary.Name);

        // ASPARALLEL
        // 그리고 여기서부터 상황이 복잡해집니다 - linq와 병렬 작업을 결합합니다.
        var threeWheelers = bikes.AsParallel().Where(b => b.Wheels == 3).Select(b => b.Name);
        // 이것은 병렬로 발생합니다! 스레드가 자동으로 생성되고 결과가 그들 사이에 분할됩니다! 많은 코어가 있는 대규모 데이터 세트에 놀랍습니다.

        // LINQ - 저장소를 IQueryable<T> 객체에 매핑하고 지연 실행합니다.
        // 예: LinqToSql - 데이터베이스에 매핑, LinqToXml은 xml 문서에 매핑
        var db = new BikeRepository();

        // 실행이 지연되므로 데이터베이스 쿼리에 좋습니다.
        var filter = db.Bikes.Where(b => b.HasTassles); // 쿼리 실행 안 됨
        if (42 > 6) // 필터를 계속 추가할 수 있으며, 조건부로도 가능합니다 - "고급 검색" 기능에 좋습니다.
            filter = filter.Where(b => b.IsBroken); // 쿼리 실행 안 됨

        var query = filter
            .OrderBy(b => b.Wheels)
            .ThenBy(b => b.Name)
            .Select(b => b.Name); // 여전히 쿼리 실행 안 됨

        // 이제 쿼리가 실행되지만 리더를 열므로 반복할 때만 채워집니다.
        foreach (string bike in query)
            Console.WriteLine(result);



        }

    } // LearnCSharp 클래스 끝

    // .cs 파일에 다른 클래스를 포함할 수 있습니다.

    public static class Extensions
    {
        // 확장 메서드
        public static void Print(this object obj)
        {
            Console.WriteLine(obj.ToString());
        }
    }


    // 대리자 및 이벤트
    public class DelegateTest
    {
        public static int count = 0;
        public static int Increment()
        {
            // count를 증가시킨 다음 반환합니다.
            return ++count;
        }

        // 대리자는 메서드에 대한 참조입니다.
        // Increment 메서드를 참조하려면 먼저 동일한 시그니처를 가진 대리자를 선언해야 합니다.
        // 즉, 인수를 받지 않고 int를 반환합니다.
        public delegate int IncrementDelegate();

        // 이벤트도 대리자를 트리거하는 데 사용할 수 있습니다.
        // 대리자 유형으로 이벤트를 만듭니다.
        public static event IncrementDelegate MyEvent;

        static void Main(string[] args)
        {
            // 대리자를 인스턴스화하고 메서드 자체를 인수로 전달하여 Increment 메서드를 참조합니다.
            IncrementDelegate inc = new IncrementDelegate(Increment);
            Console.WriteLine(inc());  // => 1

            // 대리자는 + 연산자로 구성할 수 있습니다.
            IncrementDelegate composedInc = inc;
            composedInc += inc;
            composedInc += inc;

            // composedInc는 Increment를 3번 실행합니다.
            Console.WriteLine(composedInc());  // => 4


            // 대리자로 이벤트 구독
            MyEvent += new IncrementDelegate(Increment);
            MyEvent += new IncrementDelegate(Increment);

            // 이벤트 트리거
            // 즉, 이 이벤트에 구독된 모든 대리자를 실행합니다.
            Console.WriteLine(MyEvent());  // => 6
        }
    }


    // 클래스 선언 구문:
    // <public/private/protected/internal> class <클래스 이름>{
    //    //데이터 필드, 생성자, 함수 모두 내부에 있습니다.
    //    //함수는 Java에서 메서드라고 불립니다.
    // }

    public class Bicycle
    {
        // Bicycle의 필드/변수
        public int Cadence // Public: 어디서든 액세스할 수 있습니다.
        {
            get // get - 속성을 검색하는 메서드 정의
            {
                return _cadence;
            }
            set // set - 속성을 설정하는 메서드 정의
            {
                _cadence = value; // Value는 setter에 전달된 값입니다.
            }
        }
        private int _cadence;

        protected virtual int Gear // Protected: 클래스 및 하위 클래스에서 액세스할 수 있습니다.
        {
            get; // 멤버 필드가 필요 없는 자동 속성을 만듭니다.
            set;
        }

        internal int Wheels // Internal: 어셈블리 내에서 액세스할 수 있습니다.
        {
            get;
            private set; // getter/setter 메서드에 접근 한정자를 설정할 수 있습니다.
        }

        int _speed; // 모든 것은 기본적으로 private입니다: 이 클래스 내에서만 액세스할 수 있습니다.
                    // private 키워드도 사용할 수 있습니다.
        public string Name { get; set; }

        // 속성에는 단순히 표현식의 결과를 반환하는 읽기 전용 속성을 원하는 경우에 대한 특수 구문도 있습니다.
        public string LongName => Name + " " + _speed + " speed";

        // Enum은 명명된 상수 집합으로 구성된 값 유형입니다.
        // 기본적으로 이름을 값(달리 지정되지 않는 한 int)에 매핑하는 것입니다.
        // enum에 대해 승인된 유형은 byte, sbyte, short, ushort, int, uint, long 또는 ulong입니다.
        // enum은 동일한 값을 두 번 포함할 수 없습니다.
        public enum BikeBrand
        {
            AIST,
            BMC,
            Electra = 42, //이름에 명시적으로 값을 설정할 수 있습니다.
            Gitane // 43
        }
        // 이 유형을 Bicycle 클래스 내에 정의했으므로 중첩된 유형입니다.
        // 이 클래스 외부의 코드는 이 유형을 Bicycle.BikeBrand로 참조해야 합니다.

        public BikeBrand Brand; // enum 유형을 선언한 후 이 유형의 필드를 선언할 수 있습니다.

        // FlagsAttribute로 enum을 장식하여 여러 값을 전환할 수 있음을 나타냅니다.
        // Attribute에서 파생된 모든 클래스는 유형, 메서드, 매개변수 등을 장식하는 데 사용할 수 있습니다.
        // 비트 연산자 & 및 |를 사용하여 AND/OR 연산을 수행할 수 있습니다.

        [Flags]
        public enum BikeAccessories
        {
            None = 0,
            Bell = 1,
            MudGuards = 2, // 값을 수동으로 설정해야 합니다!
            Racks = 4,
            Lights = 8,
            FullPackage = Bell | MudGuards | Racks | Lights
        }

        // 사용법: aBike.Accessories.HasFlag(Bicycle.BikeAccessories.Bell)
        // .NET 4 이전: (aBike.Accessories & Bicycle.BikeAccessories.Bell) == Bicycle.BikeAccessories.Bell
        public BikeAccessories Accessories { get; set; }

        // 정적 멤버는 특정 객체가 아닌 유형 자체에 속합니다.
        // 참조 없이 액세스할 수 있습니다:
        // Console.WriteLine("Bicycles created: " + Bicycle.bicyclesCreated);
        public static int BicyclesCreated { get; set; }

        // 읽기 전용 값은 런타임에 설정됩니다.
        // 선언 시 또는 생성자에서만 할당할 수 있습니다.
        readonly bool _hasCardsInSpokes = false; // 읽기 전용 private

        // 생성자는 클래스를 만드는 방법입니다.
        // 이것은 기본 생성자입니다.
        public Bicycle()
        {
            this.Gear = 1; // this 키워드로 객체의 멤버에 액세스할 수 있습니다.
            Cadence = 50;  // 그러나 항상 필요한 것은 아닙니다.
            _speed = 5;
            Name = "Bontrager";
            Brand = BikeBrand.AIST;
            BicyclesCreated++;
        }

        // 이것은 지정된 생성자입니다(인수를 포함합니다).
        public Bicycle(int startCadence, int startSpeed, int startGear,
                       string name, bool hasCardsInSpokes, BikeBrand brand)
            : base() // 먼저 base를 호출합니다.
        {
            Gear = startGear;
            Cadence = startStartCadence;
            _speed = startSpeed;
            Name = name;
            _hasCardsInSpokes = hasCardsInSpokes;
            Brand = brand;
        }

        // 생성자는 연결될 수 있습니다.
        public Bicycle(int startCadence, int startSpeed, BikeBrand brand) :
            this(startCadence, startSpeed, 0, "big wheels", true, brand)
        {
        }

        // 함수 구문:
        // <public/private/protected> <반환 유형> <함수 이름>(<인수>)

        // 클래스는 필드에 대한 getter 및 setter를 구현할 수 있습니다.
        // 또는 속성을 구현할 수 있습니다(이것이 C#에서 선호되는 방법입니다).

        // 메서드 매개변수는 기본값을 가질 수 있습니다.
        // 이 경우 이러한 매개변수를 생략하고 메서드를 호출할 수 있습니다.
        public void SpeedUp(int increment = 1)
        {
            _speed += increment;
        }

        public void SlowDown(int decrement = 1)
        {
            _speed -= decrement;
        }

        // 속성은 값을 가져오거나 설정합니다.
        // 데이터에만 액세스해야 하는 경우 속성 사용을 고려하십시오.
        // 속성은 get 또는 set 또는 둘 다를 가질 수 있습니다.
        private bool _hasTassles; // private 변수
        public bool HasTassles // public 접근자
        {
            get { return _hasTassles; }
            set { _hasTassles = value; }
        }

        // 속성은 자동으로 구현될 수 있습니다.
        // 이 구문은 자동으로 백킹 필드를 생성합니다.
        // getter 또는 setter(또는 둘 다)에 접근 한정자를 설정하여 접근을 제한할 수 있습니다:
        public bool IsBroken { get; private set; }

        // 속성은 자동으로 구현될 수 있습니다.
        public int FrameSize
        {
            get;
            // getter 또는 setter에 대한 접근 한정자를 지정할 수 있습니다.
            // 이는 Bicycle 클래스만 Framesize에 set을 호출할 수 있음을 의미합니다.
            private set;
        }

        // 객체에 사용자 정의 인덱서를 정의하는 것도 가능합니다.
        // 이 예제에서는 완전히 유용하지 않지만,
        // bicycle[0]을 사용하여 첫 번째 승객을 얻거나
        // bicycle[1] = "lisa"를 사용하여 승객을 설정할 수 있습니다(이 명백한 쿼트로사이클의 경우).
        private string[] passengers = { "chris", "phil", "darren", "regina" };

        public string this[int i]
        {
            get {
                return passengers[i];
            }

            set {
                passengers[i] = value;
            }
        }

        // 이 객체의 속성 값을 표시하는 메서드입니다.
        public virtual string Info()
        {
            return "Gear: " + Gear +
                    " Cadence: " + Cadence +
                    " Speed: " + _speed +
                    " Name: " + Name +
                    " Cards in Spokes: " + (_hasCardsInSpokes ? "yes" : "no") +
                    "\n------------------------------\n"
                    ;
        }

        // 메서드도 정적일 수 있습니다. 도우미 메서드에 유용할 수 있습니다.
        public static bool DidWeCreateEnoughBicycles()
        {
            // 정적 메서드 내에서는 정적 클래스 멤버만 참조할 수 있습니다.
            return BicyclesCreated > 9000;
        } // 클래스에 정적 멤버만 필요한 경우 클래스 자체를 정적으로 표시하는 것을 고려하십시오.


    } // Bicycle 클래스 끝

    // PennyFarthing은 Bicycle의 하위 클래스입니다.
    class PennyFarthing : Bicycle
    {
        // (페니 파딩은 앞바퀴가 굉장히 큰 자전거입니다.
        // 기어가 없습니다.)

        // 부모 생성자 호출
        public PennyFarthing(int startCadence, int startSpeed) :
            base(startCadence, startSpeed, 0, "PennyFarthing", true, BikeBrand.Electra)
        {
        }

        protected override int Gear
        {
            get
            {
                return 0;
            }
            set
            {
                throw new InvalidOperationException("You can't change gears on a PennyFarthing");
            }
        }

        public static PennyFarthing CreateWithGears(int gears)
        {
            var penny = new PennyFarthing(1, 1);
            penny.Gear = gears; // 이런, 이렇게 할 수 없습니다!
            return penny;
        }

        public override string Info()
        {
            string result = "PennyFarthing bicycle ";
            result += base.ToString(); // 메서드의 기본 버전 호출
            return result;
        }
    }

    // 인터페이스는 구현 없이 멤버의 시그니처만 포함합니다.
    interface IJumpable
    {
        void Jump(int meters); // 모든 인터페이스 멤버는 암시적으로 public입니다.
    }

    interface IBreakable
    {
        bool Broken { get; } // 인터페이스는 메서드 및 이벤트뿐만 아니라 속성도 포함할 수 있습니다.
    }

    // 클래스는 다른 클래스를 하나만 상속할 수 있지만, 여러 인터페이스를 구현할 수 있습니다.
    // 그러나 기본 클래스 이름은 목록의 첫 번째에 있어야 하며 모든 인터페이스는 그 뒤에 와야 합니다.
    class MountainBike : Bicycle, IJumpable, IBreakable
    {
        int damage = 0;

        public void Jump(int meters)
        {
            damage += meters;
        }

        public bool Broken
        {
            get
            {
                return damage > 100;
            }
        }
    }

    /// <summary>
    /// LinqToSql 예제를 위한 DB 연결에 사용됩니다.
    /// EntityFramework Code First는 훌륭합니다(Ruby의 ActiveRecord와 유사하지만 양방향).
    /// https://docs.microsoft.com/ef/ef6/modeling/code-first/workflows/new-database
    /// </summary>
    public class BikeRepository : DbContext
    {
        public BikeRepository()
            : base()
        {
        }

        public DbSet<Bicycle> Bikes { get; set; }
    }

    // 클래스는 여러 .cs 파일로 분할될 수 있습니다.
    // A1.cs
    public partial class A
    {
        public static void A1()
        {
            Console.WriteLine("Method A1 in class A");
        }
    }

    // A2.cs
    public partial class A
    {
        public static void A2()
        {
            Console.WriteLine("Method A2 in class A");
        }
    }

    // 부분 클래스 "A"를 사용하는 프로그램
    public class Program
    {
        static void Main()
        {
            A.A1();
            A.A2();
        }
    }

    // 문자열 보간은 문자열 앞에 $를 붙이고 보간하려는 표현식을 { 중괄호 }로 묶습니다.
    // 보간된 문자열과 문자 그대로의 문자열을 $@로 결합할 수도 있습니다.
    public class Rectangle
    {
        public int Length { get; set; }
        public int Width { get; set; }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Rectangle rect = new Rectangle { Length = 5, Width = 3 };
            Console.WriteLine($"The length is {rect.Length} and the width is {rect.Width}");

            string username = "User";
            Console.WriteLine($@"C:\\Users\\{username}\\Desktop");
        }
    }

    // 새 C# 6 기능
    class GlassBall : IJumpable, IBreakable
    {
        // 자동 속성 초기화자
        public int Damage { get; private set; } = 0;

        // getter 전용 속성 초기화자
        public string Name { get; } = "Glass ball";

        // 생성자에서 초기화되는 getter 전용 자동 속성
        public string GenieName { get; } = "";

        public GlassBall(string genieName = null)
        {
            GenieName = genieName;
        }

        public void Jump(int meters)
        {
            if (meters < 0)
                // 새로운 nameof() 표현식; 컴파일러는 식별자가 존재하는지 확인합니다.
                // nameof(x) == "x"
                // 예를 들어 매개변수 이름이 변경되지만 오류 메시지에서 업데이트되지 않는 것을 방지합니다.
                throw new ArgumentException("Cannot jump negative amount!", nameof(meters));

            Damage += meters;
        }

        // 표현식 본문 속성 ...
        public bool Broken
            => Damage > 100;

        // ... 및 메서드
        public override string ToString()
            // 보간된 문자열
            => $"{Name}. Damage taken: {Damage}";

        public string SummonGenie()
            // null 조건부 연산자
            // x?.y는 x가 null이면 즉시 null을 반환합니다; y는 평가되지 않습니다.
            => GenieName?.ToUpper();
    }

    static class MagicService
    {
        private static bool LogException(Exception ex)
        {
            // 예외를 어딘가에 기록합니다.
            return false;
        }

        public static bool CastSpell(string spell)
        {
            try
            {
                // 여기에 API를 호출한다고 가정합니다.
                throw new MagicServiceException("Spell failed", 42);

                // 주문 성공
                return true;
            }
            // Code가 42인 경우에만 잡습니다. 즉, 주문이 실패했습니다.
            catch(MagicServiceException ex) when (ex.Code == 42)
            {
                // 주문 실패
                return false;
            }
            // 다른 예외 또는 Code가 42가 아닌 MagicServiceException
            catch(Exception ex) when (LogException(ex))
            {
                // 이 블록은 실행되지 않습니다.
                // 스택은 해제되지 않습니다.
            }
            return false;
            // 참고: MagicServiceException을 잡고 Code가 42 또는 117이 아닌 경우 다시 throw하는 것은 다릅니다. 그러면 최종 catch-all 블록이 다시 throw된 예외를 잡지 않습니다.
        }
    }

    public class MagicServiceException : Exception
    {
        public int Code { get; }

        public MagicServiceException(string message, int code) : base(message)
        {
            Code = code;
        }
    }

    public static class PragmaWarning {
        // Obsolete 속성
        [Obsolete("Use NewMethod instead", false)]
        public static void ObsoleteMethod()
        {
            // 더 이상 사용되지 않는 코드
        }

        public static void NewMethod()
        {
            // 새 코드
        }

        public static void Main()
        {
            ObsoleteMethod(); // CS0618: 'ObsoleteMethod는 더 이상 사용되지 않습니다: NewMethod를 대신 사용하십시오.'
#pragma warning disable CS0618
            ObsoleteMethod(); // 경고 없음
#pragma warning restore CS0618
            ObsoleteMethod(); // CS0618: 'ObsoleteMethod는 더 이상 사용되지 않습니다: NewMethod를 대신 사용하십시오.'
        }
    }
}

using System;
// C# 6, static using
using static System.Math;

namespace Learning.More.CSharp
{
    class StaticUsing
    {
        static void Main()
        {
            // static using 문 없이...
            Console.WriteLine("The square root of 4 is {0}.", Math.Sqrt(4));
            // static using 문 사용
            Console.WriteLine("The square root of 4 is {0}.", Sqrt(4));
        }
    }
}

// 새 C# 7 기능
// Nuget에서 Microsoft.Net.Compilers 최신 버전 설치
// Nuget에서 System.ValueTuple 최신 버전 설치
using System;
namespace Csharp7
{
    // 튜플, 구조 분해 및 무시
    class TuplesTest
    {
        public (string, string) GetName()
        {
            // 튜플의 필드는 기본적으로 Item1, Item2...로 명명됩니다.
            var names1 = ("Peter", "Parker");
            Console.WriteLine(names1.Item2);  // => Parker

            // 필드는 대신 명시적으로 명명될 수 있습니다.
            // 유형 1 선언
            (string FirstName, string LastName) names2 = ("Peter", "Parker");

            // 유형 2 선언
            var names3 = (First:"Peter", Last:"Parker");

            Console.WriteLine(names2.FirstName);  // => Peter
            Console.WriteLine(names3.Last);  // => Parker

            return names3;
        }

        public string GetLastName() {
            var fullName = GetName();

            // 튜플은 구조 분해될 수 있습니다.
            (string firstName, string lastName) = fullName;

            // 구조 분해된 튜플의 필드는 _를 사용하여 무시될 수 있습니다.
            var (_, last) = fullName;
            return last;
        }

        // 모든 유형은 Deconstruct 메서드를 지정하여 동일한 방식으로 구조 분해될 수 있습니다.
        public int randomNumber = 4;
        public int anotherRandomNumber = 10;

        public void Deconstruct(out int randomNumber, out int anotherRandomNumber)
        {
            randomNumber = this.randomNumber;
            anotherRandomNumber = this.anotherRandomNumber;
        }

        static void Main(string[] args)
        {
            var tt = new TuplesTest();
            (int num1, int num2) = tt;
            Console.WriteLine($"num1: {num1}, num2: {num2}");  // => num1: 4, num2: 10

            Console.WriteLine(tt.GetLastName());
        }
    }

    // 패턴 매칭
    class PatternMatchingTest
    {
        public static (string, int)? CreateLogMessage(object data)
        {
            switch(data)
            {
                // when을 사용한 추가 필터링
                case System.Net.Http.HttpRequestException h when h.Message.Contains("404"):
                    return (h.Message, 404);
                case System.Net.Http.HttpRequestException h when h.Message.Contains("400"):
                    return (h.Message, 400);
                case Exception e:
                    return (e.Message, 500);
                case string s:
                    return (s, s.Contains("Error") ? 500 : 200);
                case null:
                    return null;
                default:
                    return (data.ToString(), 500);
            }
        }
    }

    // 참조 로컬
    // 객체에 대한 참조를 반환할 수 있도록 합니다(값만 반환하는 대신).
    class RefLocalsTest
    {
        // 반환에서 참조를 참고하십시오.
        public static ref string FindItem(string[] arr, string el)
        {
            for(int i=0; i<arr.Length; i++)
            {
                if(arr[i] == el) {
                    // 참조 반환
                    return ref arr[i];
                }
            }
            throw new Exception("Item not found");
        }

        public static void SomeMethod()
        {
            string[] arr = {"this", "is", "an", "array"};

            // 모든 곳에서 참조를 참고하십시오.
            ref string item = ref FindItem(arr, "array");
            item = "apple";
            Console.WriteLine(arr[3]);  // => apple
        }
    }

    // 로컬 함수
    class LocalFunctionTest
    {
        private static int _id = 0;
        public int id;
        public LocalFunctionTest()
        {
            id = generateId();

            // 이 로컬 함수는 이 범위에서만 액세스할 수 있습니다.
            int generateId()
            {
                return _id++;
            }
        }

        public static void AnotherMethod()
        {
            var lf1 = new LocalFunctionTest();
            var lf2 = new LocalFunctionTest();
            Console.WriteLine($"{lf1.id}, {lf2.id}");  // => 0, 1

            int id = generateId();
            // 오류 CS0103: 현재 컨텍스트에 'generateId'라는 이름이 없습니다.
        }
    }
}
```