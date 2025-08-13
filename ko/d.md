---
name: D
filename: learnd.d
contributors:
    - ["Nick Papanastasiou", "www.nickpapanastasiou.github.io"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```d
// 무슨 일이 일어날지 아실 겁니다...
module hello;

import std.stdio;

// args는 선택 사항입니다.
void main(string[] args) {
    writeln("Hello, World!");
}
```

저처럼 인터넷에서 너무 많은 시간을 보낸다면 [D](http://dlang.org/)에 대해 들어봤을 가능성이 높습니다. D 프로그래밍 언어는 저수준 기능부터 표현력이 풍부한 고수준 추상화까지 모든 것을 지원하는 현대적이고 범용적인 다중 패러다임 언어입니다.

D는 [Walter Bright](https://en.wikipedia.org/wiki/Walter_Bright)와 [Andrei Alexandrescu](https://en.wikipedia.org/wiki/Andrei_Alexandrescu)가 주도하는 대규모의 매우 똑똑한 그룹에 의해 활발하게 개발되고 있습니다.
이제 모든 것을 제쳐두고 몇 가지 예제를 살펴보겠습니다!

```d
import std.stdio;

void main() {

    // 조건문과 루프는 예상대로 작동합니다.
    for(int i = 0; i < 10000; i++) {
        writeln(i);
    }

    // 'auto'는 유형 추론에 사용할 수 있습니다.
    auto n = 1;

    // 숫자 리터럴은 가독성을 위해 '_'를 자릿수 구분 기호로 사용할 수 있습니다.
    while(n < 10_000) {
        n += n;
    }

    do {
        n -= (n / 2);
    } while(n > 0);

    // For와 while은 좋지만, D 언어에서는 'foreach' 루프를 선호합니다.
    // '..'는 첫 번째 값을 포함하고 마지막 값을 제외하는 연속적인 범위를 만듭니다.
    foreach(n; 1..1_000_000) {
        if(n % 2 == 0)
            writeln(n);
    }

    // 역방향으로 반복하고 싶을 때는 'foreach_reverse'도 있습니다.
    foreach_reverse(n; 1..int.max) {
        if(n % 2 == 1) {
            writeln(n);
        } else {
            writeln("No!");
        }
    }
}
```

`struct`, `class`, `union`, `enum`으로 새 유형을 정의할 수 있습니다. 구조체와 공용체는 값으로 함수에 전달되고(즉, 복사됨) 클래스는 참조로 전달됩니다. 또한 템플릿을 사용하여 유형과 값 모두에 대해 이 모든 것을 매개변수화할 수 있습니다!

```d
// 여기서 'T'는 유형 매개변수입니다. C++/C#/Java의 '<T>'를 생각하십시오.
struct LinkedList(T) {
    T data = null;

    // 매개변수화된 유형을 인스턴스화하려면 '!'를 사용하십시오. 다시 말하지만, '<T>'를 생각하십시오.
    LinkedList!(T)* next;
}

class BinTree(T) {
    T data = null;

    // 템플릿 매개변수가 하나만 있는 경우 괄호를 생략할 수 있습니다.
    BinTree!T left;
    BinTree!T right;
}

enum Day {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
}

// 유형에 대한 약어를 만들려면 alias를 사용하십시오.
alias IntList = LinkedList!int;
alias NumTree = BinTree!double;

// 함수 템플릿도 만들 수 있습니다!
T max(T)(T a, T b) {
    if(a < b)
        return b;

    return a;
}

// 참조로 전달을 보장하려면 ref 키워드를 사용하십시오. 즉, 'a'와 'b'가 값 유형이더라도
// 'swap()'에는 항상 참조로 전달됩니다.
void swap(T)(ref T a, ref T b) {
    auto temp = a;

    a = b;
    b = temp;
}

// 템플릿을 사용하면 유형뿐만 아니라 값으로도 매개변수화할 수 있습니다.
class Matrix(uint m, uint n, T = int) {
    T[m] rows;
    T[n] columns;
}

auto mat = new Matrix!(3, 3); // 유형 'T'는 'int'로 기본 설정되었습니다.
```

클래스에 대해 말하자면, 속성에 대해 잠시 이야기해 봅시다. 속성은 lvalue처럼 작동할 수 있는 함수이므로 POD 구조체(`structure.x = 7`)의 구문을 getter 및 setter 메서드(`object.setX(7)`)의 의미와 함께 사용할 수 있습니다!

```d
// 유형 'T' 및 'U'로 매개변수화된 클래스를 고려하십시오.
class MyClass(T, U) {
    T _data;
    U _other;
}

// 그리고 다음과 같은 "getter" 및 "setter" 메서드:
class MyClass(T, U) {
    T _data;
    U _other;

    // 생성자는 항상 'this'로 명명됩니다.
    this(T t, U u) {
        // 아래의 setter 메서드를 호출합니다.
        data = t;
        other = u;
    }

    // getter
    @property T data() {
        return _data;
    }

    @property U other() {
        return _other;
    }

    // setter
    @property void data(T t) {
        _data = t;
    }

    @property void other(U u) {
        _other = u;
    }
}

// 그리고 다음과 같이 사용합니다:
void main() {
    auto mc = new MyClass!(int, string)(7, "seven");

    // 콘솔에 쓰기 위해 표준 라이브러리에서 'stdio' 모듈을 가져옵니다.
    // (가져오기는 범위에 로컬일 수 있습니다).
    import std.stdio;

    // 값을 가져오기 위해 getter를 호출합니다.
    writefln("Earlier: data = %d, str = %s", mc.data, mc.other);

    // 새 값을 할당하기 위해 setter를 호출합니다.
    mc.data = 8;
    mc.other = "eight";

    // 새 값을 가져오기 위해 getter를 다시 호출합니다.
    writefln("Later: data = %d, str = %s", mc.data, mc.other);
}
```

속성을 사용하면 getter 및 setter 메서드에 원하는 만큼의 논리를 추가하고 멤버에 직접 액세스하는 깔끔한 구문을 유지할 수 있습니다!

사용 가능한 다른 객체 지향 기능으로는 인터페이스, 추상 클래스 및 메서드 재정의가 있습니다. D는 Java와 동일하게 상속을 지원합니다: 하나의 클래스를 확장하고 원하는 만큼의 인터페이스를 구현합니다.

D의 OOP 기능을 살펴보았지만, 이제 주제를 바꿔 봅시다. D는 일급 함수, `pure` 함수 및 불변 데이터를 사용하여 함수형 프로그래밍을 제공합니다. 또한 좋아하는 모든 함수형 알고리즘(map, filter, reduce 등)은 훌륭한 `std.algorithm` 모듈에서 찾을 수 있습니다!

```d
import std.algorithm : map, filter, reduce;
import std.range : iota; // 끝을 제외하는 범위 구축
import std.stdio;

void main() {
    // 1부터 100까지의 짝수 정수의 제곱 목록의 합을 인쇄하고 싶습니다. 쉽습니다!

    // 람다 표현식을 템플릿 매개변수로 전달하기만 하면 됩니다!
    // 원하는 함수를 전달할 수 있지만, 여기서는 람다가 편리합니다.
    auto num = iota(1, 101).filter!(x => x % 2 == 0)
                           .map!(y => y ^^ 2)
                           .reduce!((a, b) => a + b);

    writeln(num);
}
```

num을 계산하기 위해 멋진 Haskellian 파이프라인을 구축한 방법을 주목하십시오. 이는 UFCS(Uniform Function Call Syntax)로 알려진 D 혁신 덕분입니다. UFCS를 사용하면 함수 호출을 메서드 또는 자유 함수 호출로 작성할지 선택할 수 있습니다! Walter는 이에 대한 좋은 기사를 [여기](http://www.drdobbs.com/cpp/uniform-function-call-syntax/232700394)에 작성했습니다.
간단히 말해, 첫 번째 매개변수가 특정 유형 A인 함수를 유형 A의 모든 표현식에서 메서드로 호출할 수 있습니다.

병렬 처리를 좋아합니다. 다른 사람들도 병렬 처리를 좋아합니까? 물론이죠. 병렬 처리를 해 봅시다!

```d
// 1부터 (배열 크기까지) 모든 연속 정수의 제곱근으로 큰 배열을 채우고 싶다고 가정해 봅시다. 그리고 사용 가능한 코어 수만큼 동시에 이 작업을 수행하고 싶습니다.

import std.stdio;
import std.parallelism : parallel;
import std.math : sqrt;

void main() {
    // 큰 배열을 만듭니다.
    auto arr = new double[1_000_000];

    // 인덱스를 사용하고, 모든 배열 요소를 참조로 액세스하고(각 요소를 변경할 것이기 때문에) 배열에서 병렬로 호출합니다!
    foreach(i, ref elem; parallel(arr)) {
        elem = sqrt(i + 1.0);
    }
}
```