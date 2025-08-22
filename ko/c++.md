---
name: C++
filename: learncpp.cpp
contributors:
    - ["Steven Basart", "https://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
    - ["Geoff Liu", "http://geoffliu.me"]
    - ["Connor Waters", "https://github.com/connorwaters"]
    - ["Ankush Goyal", "https://github.com/ankushg07"]
    - ["Jatin Dhankhar", "https://github.com/jatindhankhar"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

C++는 시스템 프로그래밍 언어로,
[발명가 Bjarne Stroustrup에 따르면](https://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote), 
다음과 같이 설계되었습니다.

- "더 나은 C"가 되기 위해
- 데이터 추상화 지원
- 객체 지향 프로그래밍 지원
- 제네릭 프로그래밍 지원

구문이 최신 언어보다 더 어렵거나 복잡할 수 있지만,
프로세서에서 직접 실행할 수 있는 네이티브 명령으로 컴파일되고
C와 같이 하드웨어를 긴밀하게 제어할 수 있으면서도 제네릭, 예외, 클래스와 같은
고급 기능을 제공하기 때문에 널리 사용됩니다.
이러한 속도와 기능의 조합으로 C++는
가장 널리 사용되는 프로그래밍 언어 중 하나가 되었습니다.

```c++
//////////////////
// C와의 비교
//////////////////

// C++는 거의 C의 상위 집합이며
// 변수 선언, 기본 타입 및 함수에 대한 기본 구문을 공유합니다.

// C에서와 마찬가지로 프로그램의 진입점은
// 정수 반환 타입을 가진 main이라는 함수입니다.
// 이 값은 프로그램의 종료 상태로 사용됩니다.
// 자세한 내용은 https://en.wikipedia.org/wiki/Exit_status를 참조하십시오.
int main(int argc, char** argv)
{
    // 명령줄 인수는 C에서와 동일한 방식으로
    // argc 및 argv로 전달됩니다.
    // argc는 인수 수를 나타내고,
    // argv는 인수를 나타내는 C 스타일 문자열(char*)의 배열입니다.
    // 첫 번째 인수는 프로그램을 호출한 이름입니다.
    // 인수에 관심이 없는 경우 argc 및 argv를 생략할 수 있으며,
    // 함수 시그니처는 int main()이 됩니다.

    // 종료 상태 0은 성공을 나타냅니다.
    return 0;
}

// 그러나 C++는 다음과 같은 몇 가지 면에서 다릅니다:

// C++에서 문자 리터럴은 char이므로 크기는 1입니다.
sizeof('c') == sizeof(char)

// C에서 문자 리터럴은 int이므로 크기는 4입니다.
sizeof('c') == sizeof(int)


// C++는 엄격한 프로토타이핑을 사용합니다.
void func(); // 인수를 받지 않는 함수
void func(void); // 이전과 동일

// C에서
void func(); // 알 수 없는 타입의 인수를 임의 개수 받을 수 있는 함수
void func(void); // 인수를 받지 않는 함수

// C++에서 NULL 대신 nullptr 사용
int* ip = nullptr;

// 대부분의 C 표준 헤더는 C++에서 사용할 수 있습니다.
// C 헤더는 일반적으로 .h로 끝나지만,
// C++ 헤더는 "c" 접두사가 붙고 ".h" 접미사가 없습니다.

// C++ 표준 버전:
#include <cstdio>

// C 표준 버전:
#include <stdio.h>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

///////////////////////
// 함수 오버로딩
///////////////////////

// C++는 함수 오버로딩을 지원합니다.
// 각 함수가 다른 매개변수를 사용하는 경우에 한합니다.

void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("My int is %d\n", myInt);
}

int main()
{
    print("Hello"); // void print(const char*)로 확인됩니다.
    print(15); // void print(int)로 확인됩니다.
}

/////////////////////////////
// 기본 함수 인수
/////////////////////////////

// 호출자가 제공하지 않은 경우 함수에 대한 기본 인수를 제공할 수 있습니다.

void doSomethingWithInts(int a = 1, int b = 4)
{
    // 여기서 int로 무언가를 합니다.
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// 기본 인수는 인수 목록의 끝에 있어야 합니다.

void invalidDeclaration(int a = 1, int b) // 오류!
{
}


/////////////
// 네임스페이스
/////////////

// 네임스페이스는 변수, 함수 및 기타 선언에 대한 별도의 범위를 제공합니다.
// 네임스페이스는 중첩될 수 있습니다.

namespace First {
    namespace Nested {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // 네임스페이스 Nested 끝
} // 네임스페이스 First 끝

namespace Second {
    void foo()
    {
        printf("This is Second::foo\n");
    }
    void bar()
    {
        printf("This is Second::bar\n");
    }
}

void foo()
{
    printf("This is global foo\n");
}

int main()
{
    // 네임스페이스 Second의 모든 기호를 현재 범위에 포함합니다. 참고
    // bar()는 작동하지만 foo()를 사용하는 것은 더 이상 작동하지 않습니다.
    // 이제 네임스페이스 Second의 foo를 호출하는지 최상위 수준의 foo를 호출하는지
    // 모호하기 때문입니다.
    using namespace Second;

    bar(); // "This is Second::bar" 출력
    Second::foo(); // "This is Second::foo" 출력
    First::Nested::foo(); // "This is First::Nested::foo" 출력
    ::foo(); // "This is global foo" 출력
}

///////////////
// 입력/출력
///////////////

// C++ 입력 및 출력은 스트림을 사용합니다.
// cin, cout 및 cerr은 stdin, stdout 및 stderr을 나타냅니다.
// <<는 삽입 연산자이고 >>는 추출 연산자입니다.

#include <iostream> // I/O 스트림 포함

int main()
{
    int myInt;

    // stdout(또는 터미널/화면)에 출력
    // std::cout은 std 네임스페이스에 대한 액세스를 참조합니다.
    std::cout << "Enter your favorite number:\n";
    // 입력 받기
    std::cin >> myInt;

    // cout도 형식을 지정할 수 있습니다.
    std::cout << "Your favorite number is " << myInt << '\n';
    // "Your favorite number is <myInt>" 출력

    std::cerr << "Used for error messages";

    // 새 줄로 문자열 스트림 버퍼 플러시
    std::cout << "I flushed it away" << std::endl;
}

//////////
// 문자열
//////////

// C++의 문자열은 객체이며 많은 멤버 함수를 가집니다.
#include <string>

std::string myString = "Hello";
std::string myOtherString = " World";

// +는 연결에 사용됩니다.
std::cout << myString + myOtherString; // "Hello World"

std::cout << myString + " You"; // "Hello You"

// C++ 문자열 길이는 string::length() 또는 string::size()에서 찾을 수 있습니다.
cout << myString.length() + myOtherString.size(); // 11 (= 5 + 6) 출력.

// C++ 문자열은 변경 가능합니다.
myString.append(" Dog");
std::cout << myString; // "Hello Dog"

// C++는 cstring을 사용하여 관련 함수로 C 스타일 문자열을 처리할 수 있습니다.
#include <cstring>

char myOldString[10] = "Hello CPP";
cout << myOldString;
cout << "Length = " << strlen(myOldString); // Length = 9

/////////////
// 참조
/////////////

// C의 포인터 외에도
// C++에는 _참조_가 있습니다.
// 이들은 한 번 설정되면 재할당할 수 없는 포인터 유형이며
// null일 수 없습니다.
// 또한 변수 자체와 동일한 구문을 가집니다:
// 역참조에는 *가 필요하지 않으며
// 할당에는 & (주소)가 사용되지 않습니다.

std::string foo = "I am foo";
std::string bar = "I am bar";

std::string& fooRef = foo; // foo에 대한 참조를 생성합니다.
fooRef += ". Hi!"; // 참조를 통해 foo를 수정합니다.
std::cout << fooRef; // "I am foo. Hi!" 출력

std::cout << &fooRef << '\n'; // foo의 주소 출력
// "fooRef"를 재할당하지 않습니다. 이것은 "foo = bar"와 동일하며,
//   foo == "I am bar"
// 이 줄 이후입니다.
fooRef = bar;
std::cout << &fooRef << '\n'; // 여전히 foo의 주소 출력
std::cout << fooRef << '\n';  // "I am bar" 출력

// fooRef의 주소는 동일하게 유지됩니다. 즉, 여전히 foo를 참조합니다.


const std::string& barRef = bar; // bar에 대한 const 참조 생성.
// C와 마찬가지로 const 값(및 포인터 및 참조)은 수정할 수 없습니다.
barRef += ". Hi!"; // 오류, const 참조는 수정할 수 없습니다.

// 옆길: 참조에 대해 더 이야기하기 전에 임시 객체라는 개념을 소개해야 합니다.
// 다음과 같은 코드가 있다고 가정해 보겠습니다:
std::string tempObjectFun() { ... }
std::string retVal = tempObjectFun();

// 두 번째 줄에서 실제로 일어나는 일은 다음과 같습니다:
//   - tempObjectFun에서 문자열 객체가 반환됩니다.
//   - 생성자에 대한 인수로 반환된 객체로 새 문자열이 생성됩니다.
//   - 반환된 객체가 소멸됩니다.
// 반환된 객체를 임시 객체라고 합니다. 임시 객체는
// 함수가 객체를 반환할 때마다 생성되며, 둘러싸는 표현식의 평가가 끝날 때
// 소멸됩니다(음, 이것이 표준에서 말하는 것이지만
// 컴파일러는 이 동작을 변경할 수 있습니다. 이러한 종류의 세부 사항에 관심이 있다면
// "반환 값 최적화"를 찾아보십시오). 따라서 이 코드에서:
foo(bar(tempObjectFun()))

// foo와 bar가 존재한다고 가정하면, tempObjectFun에서 반환된 객체는
// bar에 전달되고 foo가 호출되기 전에 소멸됩니다.

// 이제 참조로 돌아갑니다. "둘러싸는 표현식의 끝에서" 규칙의 예외는
// 임시 객체가 const 참조에 바인딩된 경우이며, 이 경우 수명이
// 현재 범위로 연장됩니다:

void constReferenceTempObjectFun() {
    // constRef는 임시 객체를 가져오고 이 함수가 끝날 때까지 유효합니다.
    const std::string& constRef = tempObjectFun();
    ...
}

// C++11에서 도입된 또 다른 종류의 참조는 임시 객체를 위한 것입니다. 해당 유형의 변수를 가질 수는 없지만 오버로드 확인에서 우선 순위를 가집니다:

void someFun(std::string& s) { ... }  // 일반 참조
void someFun(std::string&& s) { ... }  // 임시 객체에 대한 참조

std::string foo;
someFun(foo);  // 일반 참조가 있는 버전을 호출합니다.
someFun(tempObjectFun());  // 임시 참조가 있는 버전을 호출합니다.

// 예를 들어, std::basic_string에 대해 다음 두 가지 버전의 생성자를 볼 수 있습니다:
std::basic_string(const basic_string& other);
std::basic_string(basic_string&& other);

// 아이디어는 임시 객체(어쨌든 곧 소멸될 예정)에서 새 문자열을 생성하는 경우
// 해당 임시 문자열의 일부를 "구조"하는 더 효율적인 생성자를 가질 수 있다는 것입니다. 이 개념을 "이동 의미론"이라고 합니다.

/////////////
// 열거형
/////////////

// 열거형은 코드의 가독성과 시각화를 용이하게 하기 위해 상수에 값을 할당하는 방법입니다.
enum ECarTypes
{
    Sedan,
    Hatchback,
    SUV,
    Wagon
};

ECarTypes GetPreferredCarType()
{
    return ECarTypes::Hatchback;
}

// C++11부터는 열거형에 타입을 할당하는 쉬운 방법이 있어 데이터 직렬화 및
// 원하는 타입과 해당 상수 간의 열거형 변환에 유용할 수 있습니다.
enum ECarTypes : uint8_t
{
    Sedan, // 0
    Hatchback, // 1
    SUV = 254, // 254
    Hybrid // 255
};

void WriteByteToFile(uint8_t InputValue)
{
    // InputValue를 파일에 직렬화합니다.
}

void WritePreferredCarTypeToFile(ECarTypes InputCarType)
{
    // 열거형은 선언된 열거형 타입으로 인해 uint8_t로 암시적으로 변환됩니다.
    WriteByteToFile(InputCarType);
}

// 반면에 열거형이 정수 타입이나 다른 열거형으로 우연히 캐스팅되는 것을 원하지 않을 수 있으므로
// 암시적으로 변환되지 않는 열거형 클래스를 만들 수 있습니다.
enum class ECarTypes : uint8_t
{
    Sedan, // 0
    Hatchback, // 1
    SUV = 254, // 254
    Hybrid // 255
};

void WriteByteToFile(uint8_t InputValue)
{
    // InputValue를 파일에 직렬화합니다.
}

void WritePreferredCarTypeToFile(ECarTypes InputCarType)
{
    // 열거형이 "열거형 클래스"로 선언되었기 때문에 ECarTypes가 uint8_t임에도 불구하고 컴파일되지 않습니다!
    WriteByteToFile(InputCarType);
}

//////////////////////////////////////////
// 클래스 및 객체 지향 프로그래밍
//////////////////////////////////////////

// 클래스의 첫 번째 예
#include <iostream>

// 클래스를 선언합니다.
// 클래스는 일반적으로 헤더(.h 또는 .hpp) 파일에 선언됩니다.
class Dog {
    // 멤버 변수 및 함수는 기본적으로 private입니다.
    std::string name;
    int weight;

// 이 뒤에 오는 모든 멤버는 public입니다.
// "private:" 또는 "protected:"가 나올 때까지.
public:

    // 기본 생성자
    Dog();

    // 멤버 함수 선언(구현은 나중에)
    // 참고로 여기서는 std::string을 사용하고
    // 위에 using namespace std;를 배치하지 않습니다.
    // 헤더에 "using namespace" 문을 절대 넣지 마십시오.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // 객체의 상태를 수정하지 않는 함수는
    // const로 표시해야 합니다.
    // 이렇게 하면 객체에 대한 const 참조가 주어졌을 때 호출할 수 있습니다.
    // 또한 파생 클래스에서 재정의하려면 함수를 명시적으로 _virtual_로
    // 선언해야 합니다.
    // 성능상의 이유로 함수는 기본적으로 가상이 아닙니다.
    virtual void print() const;

    // 함수는 클래스 본문 내에서도 정의할 수 있습니다.
    // 이렇게 정의된 함수는 자동으로 인라인됩니다.
    void bark() const { std::cout << name << " barks!\n"; }

    // 생성자와 함께 C++는 소멸자를 제공합니다.
    // 이들은 객체가 삭제되거나 범위를 벗어날 때 호출됩니다.
    // 이것은 RAII와 같은 강력한 패러다임을 가능하게 합니다.
    // (아래 참조)
    // 클래스에서 파생될 경우 소멸자는 가상이어야 합니다.
    // 가상이 아닌 경우 파생 클래스의 소멸자는
    // 기본 클래스 참조 또는 포인터를 통해 객체가 소멸될 때
    // 호출되지 않습니다.
    virtual ~Dog();

}; // 클래스 정의 뒤에는 세미콜론이 와야 합니다.

// 클래스 멤버 함수는 일반적으로 .cpp 파일에 구현됩니다.
Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// 객체(예: 문자열)는 수정하는 경우 참조로 전달해야 하며,
// 수정하지 않는 경우 const 참조로 전달해야 합니다.
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// "virtual"은 선언에만 필요하고 정의에는 필요하지 않습니다.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

Dog::~Dog()
{
    std::cout << "Goodbye " << name << '\n';
}

int main() {
    Dog myDog; // "A dog has been constructed" 출력
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.print(); // "Dog is Barkley and weighs 10 kg" 출력
    return 0;
} // "Goodbye Barkley" 출력

// 상속:

// 이 클래스는 Dog 클래스의 모든 public 및 protected를 상속하며,
// private도 상속하지만 public 또는 protected 메서드 없이는
// private 멤버/메서드에 직접 액세스할 수 없습니다.
class OwnedDog : public Dog {

public:
    void setOwner(const std::string& dogsOwner);

    // 모든 OwnedDog에 대한 print 함수의 동작을 재정의합니다. 자세한 내용은
    // https://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping을 참조하십시오.
    // 익숙하지 않은 경우 하위 유형 다형성에 대한 일반적인 소개입니다.
    // override 키워드는 선택 사항이지만 기본 클래스의 메서드를 실제로
    // 재정의하는지 확인합니다.
    void print() const override;

private:
    std::string owner;
};

// 한편, 해당 .cpp 파일에서:

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // 기본 Dog 클래스의 print 함수 호출
    std::cout << "Dog is owned by " << owner << '\n';
    // "Dog is <name> and weights <weight>" 출력
    //        "Dog is owned by <owner>"
}

//////////////////////////////////////////
// 초기화 및 연산자 오버로딩
//////////////////////////////////////////

// C++에서는 +, -, *, /, 등과 같은 연산자의 동작을 오버로드할 수 있습니다.
// 이것은 연산자가 사용될 때마다 호출되는 함수를 정의하여 수행됩니다.

#include <iostream>
using namespace std;

class Point {
public:
    // 멤버 변수는 이 방식으로 기본값을 지정할 수 있습니다.
    double x = 0;
    double y = 0;

    // Point를 기본값(0, 0)으로 초기화하는 것 외에는 아무것도 하지 않는
    // 기본 생성자를 정의합니다.
    Point() { };

    // 다음 구문은 초기화 목록으로 알려져 있으며
    // 클래스 멤버 값을 초기화하는 올바른 방법입니다.
    Point (double a, double b) :
        x(a),
        y(b)
    { /* 값 초기화 외에는 아무것도 하지 않습니다. */ }

    // + 연산자를 오버로드합니다.
    Point operator+(const Point& rhs) const;

    // += 연산자를 오버로드합니다.
    Point& operator+=(const Point& rhs);

    // - 및 -= 연산자를 추가하는 것도 의미가 있지만,
    // 간결함을 위해 생략하겠습니다.
};

Point Point::operator+(const Point& rhs) const
{
    // 이것과 rhs의 합인 새 점을 만듭니다.
    return Point(x + rhs.x, y + rhs.y);
}

// 할당의 가장 왼쪽 변수에 대한 참조를 반환하는 것이 좋습니다.
// `(a += b) == c`는 이 방식으로 작동합니다.
Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;

    // `this`는 메서드가 호출되는 객체에 대한 포인터입니다.
    return *this;
}

int main () {
    Point up (0,1);
    Point right (1,0);
    // 이것은 Point + 연산자를 호출합니다.
    // Point up은 right를 매개변수로 사용하여 + (함수)를 호출합니다.
    Point result = up + right;
    // "Result is upright (1,1)" 출력
    std::cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

/////////////
// 템플릿
/////////////

// C++의 템플릿은 주로 제네릭 프로그래밍에 사용되지만, 다른 언어의
// 제네릭 구성보다 훨씬 강력합니다. 또한 명시적 및 부분적 특수화와
// 함수형 스타일 타입 클래스를 지원합니다. 사실, C++에 내장된
// 튜링 완전 함수형 언어입니다!

// 익숙할 수 있는 제네릭 프로그래밍 종류부터 시작하겠습니다. 타입 매개변수를
// 사용하는 클래스 또는 함수를 정의하려면:
template<class T>
class Box {
public:
    // 이 클래스에서 T는 다른 타입처럼 사용할 수 있습니다.
    void insert(const T&) { ... }
};

// 컴파일 중에 컴파일러는 실제로 매개변수가 대체된 각 템플릿의 복사본을
// 생성하므로 클래스의 전체 정의는 각 호출에 있어야 합니다.
// 이것이 헤더 파일에 전체적으로 정의된 템플릿 클래스를 보게 되는 이유입니다.

// 스택에 템플릿 클래스를 인스턴스화하려면:
Box<int> intBox;

// 그리고 예상대로 사용할 수 있습니다:
intBox.insert(123);

// 물론 템플릿을 중첩할 수 있습니다:
Box<Box<int> > boxOfBox;
boxOfBox.insert(intBox);

// C++11까지는 두 '>' 사이에 공백을 두어야 했습니다. 그렇지 않으면 '>>'가
// 오른쪽 시프트 연산자로 구문 분석됩니다.

// 때로는
//   template<typename T>
//를 보게 될 것입니다. 이 경우 'class' 키워드와 'typename' 키워드는 _대부분_
// 서로 바꿔 사용할 수 있습니다. 전체 설명은
//   https://en.wikipedia.org/wiki/Typename을 참조하십시오.
// (예, 해당 키워드에는 자체 Wikipedia 페이지가 있습니다).

// 마찬가지로 템플릿 함수:
template<class T>
void barkThreeTimes(const T& input)
{
    input.bark();
    input.bark();
    input.bark();
}

// 여기서 타입 매개변수에 대해 아무것도 지정되지 않았습니다. 컴파일러는
// 템플릿의 모든 호출을 생성한 다음 타입 검사를 수행하므로
// 위 함수는 const 'bark' 메서드가 있는 모든 타입 'T'에서 작동합니다!

Dog fluffy;
fluffy.setName("Fluffy")
barkThreeTimes(fluffy); // "Fluffy barks"를 세 번 출력합니다.

// 템플릿 매개변수는 클래스일 필요는 없습니다:
template<int Y>
void printMessage() {
    std::cout << "Learn C++ in " << Y << " minutes!\n";
}

// 그리고 더 효율적인 코드를 위해 템플릿을 명시적으로 특수화할 수 있습니다. 물론,
// 특수화의 실제 사용 사례 대부분은 이처럼 사소하지 않습니다.
// 모든 매개변수를 명시적으로 지정하더라도 함수(또는 클래스)를 템플릿으로
// 선언해야 합니다.
template<>
void printMessage<10>() {
    std::cout << "Learn C++ faster in only 10 minutes!\n";
}

printMessage<20>();  // "Learn C++ in 20 minutes!" 출력
printMessage<10>();  // "Learn C++ faster in only 10 minutes!" 출력


/////////////
// 예외 처리
/////////////

// 표준 라이브러리는 몇 가지 예외 유형을 제공합니다.
// (https://en.cppreference.com/w/cpp/error/exception 참조)
// 하지만 모든 유형을 예외로 던질 수 있습니다.
#include <exception>
#include <stdexcept>

// _try_ 블록 내에서 던져진 모든 예외는 후속
// _catch_ 핸들러에 의해 잡힐 수 있습니다.
try {
    // _new_를 사용하여 힙에 예외를 할당하지 마십시오.
    throw std::runtime_error("A problem occurred");
}

// 객체인 경우 const 참조로 예외를 잡습니다.
catch (const std::exception& ex)
{
    std::cout << ex.what();
}

// 이전 _catch_ 블록에서 잡히지 않은 모든 예외를 잡습니다.
catch (...)
{
    std::cout << "Unknown exception caught";
    throw; // 예외를 다시 던집니다.
}

///////
// RAII
///////

// RAII는 "Resource Acquisition Is Initialization"의 약자입니다.
// C++에서 가장 강력한 패러다임으로 간주되며
// 객체의 생성자가 해당 객체의 리소스를 획득하고
// 소멸자가 이를 해제한다는 간단한 개념입니다.

// 이것이 유용한 이유를 이해하려면
// C 파일 핸들을 사용하는 함수를 고려하십시오:
void doSomethingWithAFile(const char* filename)
{
    // 우선, 아무것도 실패할 수 없다고 가정합니다.

    FILE* fh = fopen(filename, "r"); // 파일을 읽기 모드로 엽니다.
    if (fh == NULL) {
        // 가능한 오류 처리
    }

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // 파일 핸들을 닫습니다.
}

// 불행히도 오류 처리로 인해 상황이 빠르게 복잡해집니다.
// fopen이 실패할 수 있고, doSomethingWithTheFile 및
// doSomethingElseWithIt이 실패하면 오류 코드를 반환한다고 가정합니다.
//  (예외는 실패를 처리하는 선호되는 방법이지만,
//   일부 프로그래머, 특히 C 배경을 가진 프로그래머는
//   예외의 유용성에 대해 동의하지 않습니다).
// 이제 각 호출이 실패했는지 확인하고 문제가 발생하면 파일 핸들을
// 닫아야 합니다.
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // 파일을 읽기 모드로 엽니다.
    if (fh == nullptr) // 반환된 포인터는 실패 시 null입니다.
        return false; // 호출자에게 실패를 보고합니다.

    // 각 함수가 실패하면 false를 반환한다고 가정합니다.
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // 누출되지 않도록 파일 핸들을 닫습니다.
        return false; // 오류를 전파합니다.
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // 누출되지 않도록 파일 핸들을 닫습니다.
        return false; // 오류를 전파합니다.
    }

    fclose(fh); // 누출되지 않도록 파일 핸들을 닫습니다.
    return true; // 성공을 나타냅니다.
}

// C 프로그래머는 종종 goto를 사용하여 이것을 약간 정리합니다:
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // 파일 닫기
    return true; // 성공 표시

failure:
    fclose(fh);
    return false; // 오류 전파
}

// 함수가 예외를 사용하여 오류를 나타내는 경우
// 상황이 약간 더 깨끗하지만 여전히 최적이 아닙니다.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // 파일을 shared_ptr 읽기 모드로 엽니다.
    if (fh == nullptr)
        throw std::runtime_error("Could not open the file.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...)
    {
        fclose(fh); // 오류가 발생하면 파일을 닫아야 합니다.
        throw; // 그런 다음 예외를 다시 던집니다.
    }

    fclose(fh); // 파일 닫기
    // 모든 것이 성공했습니다.
}

// 이것을 C++의 파일 스트림 클래스(fstream) 사용과 비교하십시오.
// fstream은 소멸자를 사용하여 파일을 닫습니다.
// 위에서 언급했듯이 소멸자는 객체가 범위를 벗어날 때마다
// 자동으로 호출됩니다.
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream은 입력 파일 스트림의 약자입니다.
    std::ifstream fh(filename); // 파일 열기

    // 파일로 작업 수행
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // 파일은 소멸자에 의해 자동으로 여기서 닫힙니다.

// 이것은 _엄청난_ 이점이 있습니다:
// 1. 무슨 일이 일어나든,
//    리소스(이 경우 파일 핸들)가 정리됩니다.
//    소멸자를 올바르게 작성하면
//    핸들을 닫는 것을 잊고 리소스를 누출하는 것은 _불가능_합니다.
// 2. 코드가 훨씬 깨끗합니다.
//    소멸자는 걱정할 필요 없이 백그라운드에서 파일 닫기를 처리합니다.
// 3. 코드는 예외에 안전합니다.
//    함수 어디에서나 예외가 발생할 수 있으며 정리는
//    여전히 발생합니다.

// 모든 관용적인 C++ 코드는 모든 리소스에 대해 RAII를 광범위하게 사용합니다.
// 추가 예는 다음과 같습니다.
// - unique_ptr 및 shared_ptr을 사용한 메모리
// - 컨테이너 - 표준 라이브러리 연결 리스트,
//   벡터(즉, 자체 크기 조정 배열), 해시 맵 등
//   모두 범위를 벗어날 때 자동으로 내용을 소멸시킵니다.
// - lock_guard 및 unique_lock을 사용한 뮤텍스


/////////////
// 스마트 포인터
/////////////

// 일반적으로 스마트 포인터는 "원시 포인터"(C에서 각각 malloc/calloc을 사용하는 "new" 사용)를 래핑하는 클래스입니다.
// 목표는 객체를 명시적으로 삭제할 필요 없이 가리키는 객체의 수명을 관리할 수 있도록 하는 것입니다.
// 용어 자체는 언급된 추상화가 있는 포인터 집합을 간단히 설명합니다.
// 객체를 삭제하는 것을 잊어버리면 발생하는 위험한 메모리 누수를 방지하기 위해 원시 포인터보다 스마트 포인터를 선호해야 합니다.

// 원시 포인터 사용:
Dog* ptr = new Dog();
ptr->bark();
delete ptr;

// 스마트 포인터를 사용하면 더 이상 객체 삭제에 대해 걱정할 필요가 없습니다.
// 스마트 포인터는 포인터에 대한 참조를 계산하는 정책을 설명합니다.
// 객체에 대한 마지막 참조가 소멸될 때 객체가 소멸됩니다.

// "std::shared_ptr" 사용:
void foo()
{
    // 더 이상 Dog를 삭제할 필요가 없습니다.
    std::shared_ptr<Dog> doggo(new Dog());
    doggo->bark();
}

// 가능한 순환 참조에 주의하십시오!!!
// 항상 참조가 있으므로 절대 소멸되지 않습니다!
std::shared_ptr<Dog> doggo_one(new Dog());
std::shared_ptr<Dog> doggo_two(new Dog());
doggo_one = doggo_two; // p1은 p2를 참조합니다.
doggo_two = doggo_one; // p2는 p1을 참조합니다.

// 여러 종류의 스마트 포인터가 있습니다.
// 사용해야 하는 방식은 항상 동일합니다.
// 이것은 우리를 다음과 같은 질문으로 이끕니다: 언제 각 종류의 스마트 포인터를 사용해야 할까요?
// std::unique_ptr - 객체에 대한 참조를 하나만 유지하고 싶을 때 사용합니다.
// std::shared_ptr - 동일한 객체에 대한 여러 참조를 유지하고 모든 참조가 사라졌을 때 할당 해제되도록 하고 싶을 때 사용합니다.
// std::weak_ptr - 해당 객체가 할당된 상태를 유지하지 않고 std::shared_ptr의 기본 객체에 액세스하고 싶을 때 사용합니다.
// 약한 포인터는 순환 참조를 방지하는 데 사용됩니다.


/////////////
// 컨테이너
/////////////

// 컨테이너 또는 표준 템플릿 라이브러리는 미리 정의된 템플릿입니다.
// 요소에 대한 저장 공간을 관리하고 액세스 및 조작을 위한 멤버 함수를 제공합니다.

// 몇 가지 컨테이너는 다음과 같습니다:

// 벡터(동적 배열)
// 런타임에 객체의 배열 또는 목록을 정의할 수 있습니다.
#include <vector>
std::string val;
std::vector<string> my_vector; // 벡터 초기화
std::cin >> val;

my_vector.push_back(val); // 'val'의 값을 벡터("배열") my_vector에 푸시합니다.
my_vector.push_back(val); // 값을 벡터에 다시 푸시합니다(이제 두 개의 요소가 있음).

// 벡터를 반복하는 두 가지 선택 사항이 있습니다:
// 클래식 루핑(인덱스 0에서 마지막 인덱스까지 벡터를 반복):
for (int i = 0; i < my_vector.size(); i++) {
    std::cout << my_vector[i] << '\n'; // 벡터의 요소에 액세스하려면 연산자 []를 사용할 수 있습니다.
}

// 또는 반복기 사용:
vector<string>::iterator it; // 벡터에 대한 반복기 초기화
for (it = my_vector.begin(); it != my_vector.end(); ++it) {
    std::cout << *it  << '\n';
}

// 세트
// 세트는 특정 순서를 따르는 고유한 요소를 저장하는 컨테이너입니다.
// 세트는 다른 함수나 코드 없이 정렬된 순서로 고유한 값을 저장하는 데 매우 유용한 컨테이너입니다.

#include<set>
std::set<int> ST;    // int 데이터 유형의 세트를 초기화합니다.
ST.insert(30);  // 세트 ST에 값 30을 삽입합니다.
ST.insert(10);  // 세트 ST에 값 10을 삽입합니다.
ST.insert(20);  // 세트 ST에 값 20을 삽입합니다.
ST.insert(30);  // 세트 ST에 값 30을 삽입합니다.
// 이제 세트의 요소는 다음과 같습니다.
//  10 20 30

// 요소를 지우려면
ST.erase(20);  // 값 20을 가진 요소를 지웁니다.
// 세트 ST: 10 30
// 세트를 반복하려면 반복기를 사용합니다.
std::set<int>::iterator it;
for (it = ST.begin(); it != ST.end(); it++) {
    std::cout << *it << '\n';
}
// 출력:
// 10
// 30

// 전체 컨테이너를 지우려면 Container_name.clear()를 사용합니다.
ST.clear();
std::cout << ST.size();  // 세트 ST의 크기를 인쇄합니다.
// 출력: 0

// 참고: 중복 요소의 경우 multiset을 사용할 수 있습니다.
// 참고: 해시 세트의 경우 unordered_set을 사용하십시오. 더 효율적이지만 순서를 유지하지 않습니다.
// unordered_set은 C++11부터 사용할 수 있습니다.

// 맵
// 맵은 키 값과 매핑된 값의 조합으로 형성된 요소를 특정 순서에 따라 저장합니다.

#include<map>
std::map<char, int> mymap;  // 키를 char로, 값을 int로 맵을 초기화합니다.

mymap.insert(pair<char,int>('A',1));
// 키 A에 값 1을 삽입합니다.
mymap.insert(pair<char,int>('Z',26));
// 키 Z에 값 26을 삽입합니다.

// 반복하려면
std::map<char,int>::iterator it;
for (it = mymap.begin(); it != mymap.end(); ++it)
    std::cout << it->first << "->" << it->second << '\n';
// 출력:
// A->1
// Z->26

// 키에 해당하는 값을 찾으려면
it = mymap.find('Z');
std::cout << it->second;

// 출력: 26

// 참고: 해시 맵의 경우 unordered_map을 사용하십시오. 더 효율적이지만 순서를 유지하지 않습니다.
// unordered_map은 C++11부터 사용할 수 있습니다.

// 비원시 값(사용자 정의 클래스)의 객체 키가 있는 컨테이너는 객체 자체 또는 함수 포인터로 비교 함수가 필요합니다. 원시에는 기본 비교기가 있지만 재정의할 수 있습니다.
class Foo {
public:
    int j;
    Foo(int a) : j(a) {}
};
struct compareFunction {
    bool operator()(const Foo& a, const Foo& b) const {
        return a.j < b.j;
    }
};
// 이것은 허용되지 않습니다(컴파일러에 따라 다를 수 있음).
// std::map<Foo, int> fooMap;
std::map<Foo, int, compareFunction> fooMap;
fooMap[Foo(1)]  = 1;
fooMap.find(Foo(1)); //true


///////////////////////////////////////
// 람다 표현식(C++11 이상)
///////////////////////////////////////

// 람다는 호출되거나 함수에 인수로 전달되는 위치에서 바로 익명 함수 객체를 정의하는 편리한 방법입니다.

// 예를 들어, 쌍의 두 번째 값을 사용하여 쌍의 벡터를 정렬하는 것을 고려하십시오.

std::vector<pair<int, int> > tester;
mailer.push_back(make_pair(3, 6));
mailer.push_back(make_pair(1, 9));
mailer.push_back(make_pair(5, 0));

// 정렬 함수에 세 번째 인수로 람다 표현식을 전달합니다.
// 정렬은 <algorithm> 헤더에 있습니다.

std::sort(tester.begin(), tester.end(), [](const pair<int, int>& lhs, const pair<int, int>& rhs) {
        return lhs.second < rhs.second;
    });

// 람다의 구문을 참고하십시오.
// 람다의 []는 변수를 "캡처"하는 데 사용됩니다.
// "캡처 목록"은 람다 외부에서 함수 본문 내에서 사용할 수 있어야 하는 것과 그 방법을 정의합니다.
// 다음 중 하나일 수 있습니다:
//     1. 값 : [x]
//     2. 참조 : [&x]
//     3. 참조로 현재 범위의 모든 변수 [&]
//     4. 3과 동일하지만 값으로 [=]
// 예:

std::vector<int> dog_ids;
// number_of_dogs = 3;
for (int i = 0; i < 3; i++) {
    dog_ids.push_back(i);
}

int weight[3] = {30, 50, 10};

// 개의 무게에 따라 dog_ids를 정렬하고 싶다고 가정해 보겠습니다.
// 따라서 dog_ids는 결국 [2, 0, 1]이 되어야 합니다.

// 여기서 람다 표현식이 유용합니다.

sort(dog_ids.begin(), dog_ids.end(), [&weight](const int &lhs, const int &rhs) {
        return weight[lhs] < weight[rhs];
    });
// 위 예제에서 "weight"를 참조로 캡처했습니다.
// C++의 람다에 대한 자세한 내용: https://stackoverflow.com/questions/7627098/what-is-a-lambda-expression-in-c11

///////////////////////////////
// 범위 For(C++11 이상)
///////////////////////////////

// 범위 for 루프를 사용하여 컨테이너를 반복할 수 있습니다.
int arr[] = {1, 10, 3};

for (int elem: arr) {
    cout << elem << endl;
}

// "auto"를 사용하고 컨테이너 요소의 유형에 대해 걱정하지 않을 수 있습니다.
// 예를 들어:

for (auto elem: arr) {
    // arr의 각 요소로 무언가를 합니다.
}

/////////////
// 재미있는 것들
/////////////

// 신규 사용자(심지어 일부 베테랑)에게 놀라울 수 있는 C++의 측면.
// 이 섹션은 불행히도 매우 불완전합니다. C++는 발등을 찍기 가장 쉬운 언어 중 하나입니다.

// private 메서드를 재정의할 수 있습니다!
class Foo {
    virtual void bar();
};
class FooSub : public Foo {
    virtual void bar();  // Foo::bar를 재정의합니다!
};


// 0 == false == NULL (대부분의 경우)!
bool* pt = new bool;
*pt = 0; // 'pt'가 가리키는 값을 false로 설정합니다.
pt = 0;  // 'pt'를 null 포인터로 설정합니다. 두 줄 모두 경고 없이 컴파일됩니다.

// nullptr은 그 문제의 일부를 해결하기 위한 것입니다:
int* pt2 = new int;
*pt2 = nullptr; // 컴파일되지 않음
pt2 = nullptr;  // pt2를 null로 설정합니다.

// bool에 대한 예외가 있습니다.
// 이것은 if(!ptr)로 null 포인터를 테스트할 수 있도록 하기 위한 것이지만,
// 결과적으로 nullptr을 bool에 직접 할당할 수 있습니다!
*pt = nullptr;  // '*pt'가 bool임에도 불구하고 이것은 여전히 컴파일됩니다!


// '=' != '=' != '='!
// Foo::Foo(const Foo&) 또는 일부 변형(이동 의미론 참조) 복사
// 생성자를 호출합니다.
Foo f2;
Foo f1 = f2;

// Foo::Foo(const Foo&) 또는 변형을 호출하지만 'fooSub'의 'Foo' 부분만 복사합니다.
// 'fooSub'의 추가 멤버는 버려집니다. 이 때로는
// 끔찍한 동작을 "객체 슬라이싱"이라고 합니다.
FooSub fooSub;
Foo f1 = fooSub;

// Foo::operator=(Foo&) 또는 변형을 호출합니다.
Foo f1;
f1 = f2;


///////////////////////////////////////
// 튜플(C++11 이상)
///////////////////////////////////////

#include<tuple>

// 개념적으로 튜플은 오래된 데이터 구조(C와 유사한 구조체)와 유사하지만
// 명명된 데이터 멤버를 갖는 대신
// 해당 요소는 튜플에서의 순서로 액세스됩니다.

// 튜플 생성부터 시작하겠습니다.
// 튜플에 값 패킹
auto first = make_tuple(10, 'A');
const int maxN = 1e9;
const int maxL = 15;
auto second = make_tuple(maxN, maxL);

// 'first' 튜플의 요소 인쇄
std::cout << get<0>(first) << " " << get<1>(first) << '\n'; // 인쇄: 10 A

// 'second' 튜플의 요소 인쇄
std::cout << get<0>(second) << " " << get<1>(second) << '\n'; // 인쇄: 1000000000 15

// 변수로 튜플 풀기

int first_int;
char first_char;
tie(first_int, first_char) = first;
std::cout << first_int << " " << first_char << '\n';  // 인쇄: 10 A

// 이렇게 튜플을 만들 수도 있습니다.

tuple<int, char, double> third(11, 'A', 3.14141);
// tuple_size는 튜플의 요소 수를 반환합니다(constexpr로).

std::cout << tuple_size<decltype(third)>::value << '\n'; // 인쇄: 3

// tuple_cat은 모든 튜플의 요소를 동일한 순서로 연결합니다.

auto concatenated_tuple = tuple_cat(first, second, third);
// concatenated_tuple은 (10, 'A', 1e9, 15, 11, 'A', 3.14141)이 됩니다.

std::cout << get<0>(concatenated_tuple) << '\n'; // 인쇄: 10
std::cout << get<3>(concatenated_tuple) << '\n'; // 인쇄: 15
std::cout << get<5>(concatenated_tuple) << '\n'; // 인쇄: 'A'


///////////////////////////////////
// 논리 및 비트 연산자
//////////////////////////////////

// C++의 대부분의 연산자는 다른 언어와 동일합니다.

// 논리 연산자

// C++는 단락 평가를 사용합니다. 즉, 두 번째 인수는 첫 번째 인수가 표현식의 값을 결정하기에 충분하지 않은 경우에만 실행되거나 평가됩니다.

true && false // **논리곱**을 수행하여 false를 생성합니다.
true || false // **논리합**을 수행하여 true를 생성합니다.
! true        // **논리 부정**을 수행하여 false를 생성합니다.

// 기호 대신 동등한 키워드를 사용할 수 있습니다.
true and false // **논리곱**을 수행하여 false를 생성합니다.
true or false  // **논리합**을 수행하여 true를 생성합니다.
not true       // **논리 부정**을 수행하여 false를 생성합니다.

// 비트 연산자

// **<<** 왼쪽 시프트 연산자
// <<는 비트를 왼쪽으로 시프트합니다.
4 << 1 // 4의 비트를 왼쪽으로 1만큼 시프트하여 8을 생성합니다.
// x << n은 x * 2^n으로 생각할 수 있습니다.


// **>>** 오른쪽 시프트 연산자
// >>는 비트를 오른쪽으로 시프트합니다.
4 >> 1 // 4의 비트를 오른쪽으로 1만큼 시프트하여 2를 생성합니다.
// x >> n은 x / 2^n으로 생각할 수 있습니다.

~4    // 비트 부정을 수행합니다.
4 | 3 // 비트 또는을 수행합니다.
4 & 3 // 비트 및을 수행합니다.
4 ^ 3 // 비트 배타적 또는을 수행합니다.

// 동등한 키워드는 다음과 같습니다.
compl 4    // 비트 부정을 수행합니다.
4 bitor 3  // 비트 또는을 수행합니다.
4 bitand 3 // 비트 및을 수행합니다.
4 xor 3    // 비트 배타적 또는을 수행합니다.

```