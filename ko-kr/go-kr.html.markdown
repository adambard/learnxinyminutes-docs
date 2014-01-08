---
name: Go
category: language
language: Go
filename: learngo.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
translators:
    - ["Jongmin Kim", "http://github.com/atomaths"]
lang: ko-kr
---

Go는 일을 잘 끝낼 필요에 의해 만들어졌다. Go가 잘 알려진 최신의
트렌드는 아니지만, 실세계의 문제들을 해결하기 위해서는 가장
새롭고 빠른 방법이다.

Go는 정적 타이핑(static typing)의 명령형 언어들(imperative languages)이
갖고 있는 특징과 유사한 개념들을 가지고 있다. Go는 컴파일과 실행속도가
빠르며, 오늘날의 멀티코어 CPU를 위해 이해하기 쉬운 동시성(concurrency)
기능이 추가되었다. 그리고 큰 스케일의 프로그래밍에도 도움이 되는
기능들을 가지고 있다.

또한 Go에는 훌륭한 표준 라이브러리와 열정적인 커뮤니티가 있다.

```go
// 한 줄 주석
/* 여러 줄
   주석 */

// 모든 Go 소스 파일은 `package`로 시작한다.
// 패키지 이름 중 `main`은 라이브러리가 아닌 실행파일을 선언하는 특별한 이름이다.
package main

// `import`는 이 Go 소스 파일 내에서 참조하는 라이브러리 패키지들을 선언한다.
import (
    "fmt"      // Go 표준 라이브러리에 있는 패키지
    "net/http" // 표준 라이브러리에는 웹 서버 패키지도 있다! (클라이언트도 있음)
    "strconv"  // 문자열 변환 패키지
)

// 함수 선언. `main`은 실행 프로그램에서 시작점이 되는 특별한 함수다.
// 중괄호를 사용한다.
func main() {
    // `Println`은 표준 출력으로 개행을 출력한다.
    // fmt 패키지를 통해 이용할 수 있다.
    fmt.Println("Hello world!")

    // 다른 함수를 호출한다.
    beyondHello()
}

// 함수에 파라미터가 없더라도 빈 괄호는 있어야 한다.
func beyondHello() {
    var x int // 변수 선언. 변수는 사용하기 전에 선언해야 한다.
    x = 3     // 변수에 값 할당.
    // 짧은 선언(short declaration)으로 `:=` 를 사용하는데,
    // 이렇게 값을 할당하면 값의 타입에 따라 변수의 타입이 결정된다.
    y := 4
    sum, prod := learnMultiple(x, y)        // 함수는 두 개 이상의 리턴 값을 줄 수 있다.
    fmt.Println("sum:", sum, "prod:", prod) // 간단한 출력
    learnTypes()                            // 잠시 후에 좀더 자세히!
}

// 함수는 파라미터들을 가질 수 있고, 복수개의 값을 리턴할 수 있다.
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // 두 개의 값을 리턴.
}

// 내장 타입과 리터럴
func learnTypes() {
    // 짧은 선언은 유용하다.
    s := "Learn Go!" // string 타입

    s2 := `역따옴표 안의 string 리터럴은
개행을 포함할 수 있다.` // 같은 string 타입

    // non-ASCII 리터럴. Go 소스는 UTF-8로 작성해야 한다.
    g := 'Σ' // 유니코드 코드 포인트를 담고 있고, uint32 타입의 가칭(alias)인 rune 타입

    f := 3.14195 // float64, an IEEE-754 64-bit 부동소수 타입
    c := 3 + 4i  // complex128, 내부적으로는 두 개의 float64 타입으로 표현됨

    // 초기값과 함께 사용하는 var 키워드.
    var u uint = 7 // unsigned, 하지만 int에 따른 구현의존적인 크기
    var pi float32 = 22. / 7

    // 짧은 선언으로 변환(conversion)하는 문법.
    // Go에서는 type casting 이라고 하지않고 type conversion 이라고 함.
    n := byte('\n') // byte는 uint8의 가칭(alias)

    // 배열은 컴파일 시에 크기가 정해진다.
    var a4 [4]int           // 모두 0으로 초기화되는 int 타입 4개짜리 배열
    a3 := [...]int{3, 1, 5} // 3, 1, 5로 초기화되는 int 타입 3개짜리 배열

    // 슬라이스(slice)라고 하는 타입은 배열에 대한 가변 크기를 가진다.
    // 배열, 슬라이스 각자 장점이 있지만, 슬라이스가 더 많이 사용된다.
    s3 := []int{4, 5, 9}    // 위의 a3와 비교해보면 생략부호(...)가 없다.
    s4 := make([]int, 4)    // 모두 0으로 초기화되는 int 4개에 대한 슬라이스를 할당.
    var d2 [][]float64      // 여기에서는 선언만 있고 할당은 없다.
    bs := []byte("a slice") // string 타입을 byte 슬라이스 타입으로 형변환(type conversion)

    p, q := learnMemory() // int에 대한 포인터 타입인 p와 q를 선언
    fmt.Println(*p, *q)   // C에서처럼 *는 포인터를 따라가 값을 참조한다. 여기서는 두 개의 int를 출력.

    // 맵(map)은 다른 언어의 해시(hash)나 딕셔너리(dictionary)처럼 가변의 연관배열 타입.
    m := map[string]int{"three": 3, "four": 4}
    m["one"] = 1

    // 선언만 하고 사용하지 않는 변수가 있다면 Go에서는 컴파일 시 에러가 난다.
    // 언더바를 이용해서 변수를 사용한 것처럼 하고 그 값은 무시해버릴 수 있다.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // 물론 출력을 하면 변수로 취급한다.
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() // 잠시 후에 다시 나옴
}

// Go는 가비지 컬렉션 기능을 JVM 같은 곳이 아닌 실행파일 런타임에 포함하고 있다.
// 그리고 포인터는 있지만, 포인터 연산(*p++ 같은)은 없다.
// 그래서 `nil` 포인터 접근같은 것 때문에 실수를 할 수는 있지만
// 포인터 연산으로 인한 실수는 없게 된다.
func learnMemory() (p, q *int) {
    // 지명된 리턴 값(named return value)인 p와 q는 int에 대한 포인터 타입이다.
    p = new(int) // 내장함수인 `new`는 메모리를 할당해준다.
    // 메모리 할당된 int는 0으로 초기화 되고, p는 이제 `nil`이 아니다.
    s := make([]int, 20) // 메모리의 단일 블록으로 20개의 int 공간을 할당한다.
    s[3] = 7             // 그중 하나에 값을 준다.
    r := -2              // 또다른 로컬 변수를 선언한다.
    return &s[3], &r     // `&`는 어떤 대상체의 메모리 주소를 가져오게 된다.
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // `if`문에 중괄호는 필요하지만, 조건이 들어갈 곳에 소괄호는 쓰지 않는다.
    if true {
        fmt.Println("told ya")
    }
    // 모든 Go 소스의 코드 포맷팅은 `go fmt` 커맨드라인 명령으로 규격을 맞춘다.
    if false {
        // pout
    } else {
        // gloat
    }
    // if-else 체인 형태보다 `switch` 사용이 권장된다.
    x := 1
    switch x {
    case 0:
    case 1:
        // case 안에서는 `break`가 없어도 자동으로 다음 case로 내려가지 않는다.
        // 자동으로 내려가게 하려면 `fallthrough` 키워드를 사용한다.
    case 2:
        // x는 1이므로 여기는 실행되지 않음.
    }
    // `if`에서처럼 `for`에서도 양쪽에 소괄호를 쓰지 않는다.
    for x := 0; x < 3; x++ { // `++`은 실행을 제어하는 하나의 구문(statement)이다.
        fmt.Println("iteration", x)
    }
    // 여기서 x는 1이다. 위 for에서 x는 for 안의 블록 범위에 있기 때문.

    // For is the only loop statement in Go, but it has alternate forms.
    // `for`는 Go에서 유일한 루프 구문이지만 다양한 형태로 조건을 주거나 while 
    // 처럼 쓸 수도 있다.
    for { // 무한루프
        break    // 여기서 곧바로 break를 한 건 단지
        continue // break, continue를 루프 안에서 쓸 수 있다는 것을 보여주기 위함.
    }
    // `for`에서처럼 `if`에서 `:=`를 사용하는 것은 y에 먼저 값을 대입하고,
    // 그리고 y > x를 검사한다는 의미.
    if y := expensiveComputation(); y > x {
        x = y
    }
    // 함수 리터럴은 클로저다.
    xBig := func() bool {
        return x > 100 // 위 switch 문 바로 위에 있는 x를 참조한다.
    }
    fmt.Println("xBig:", xBig()) // true (x에 1e6를 대입했었다.)
    x /= 1e5                     // x는 10이 된다.
    fmt.Println("xBig:", xBig()) // 이제 xBig()의 결과는 false가 된다.

    // `goto`가 필요하다면, 좋아하게 될지도...
    goto love
love:

    learnInterfaces() // 곧이어서 좋은 기능에 대한 설명이 나올 거다.
}

// String 이라는 메서드 하나를 가진 Stringer 라는 인터페이스 타입을 정의하자.
type Stringer interface {
    String() string
}

// x와 y라는 이름의 int 타입 필드를 가진 pair라는 struct를 정의하자.
type pair struct {
    x, y int
}

// Define a method on type pair.  Pair now implements Stringer.
// pair 타입에 메서드 String을 정의하자.
// 이제 pair는 Stringer 인터페이스를 구현(implement)한 것이 되었다.
func (p pair) String() string { // 여기서 p는 리시버(receiver)라고 부른다.
    // Sprintf는 fmt 패키지 안에 있는 외부로 공개된(exported) 함수다.
    // 점(.)으로 p의 필드들을 참조할 수 있다.
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // 중괄호 문법은 "구조체 리터럴(struct literal)"인데, 초기화된 구조체로
    // 취급하게 해준다. := 문법으로 p를 이 구조체로 선언하고 초기화한다.
    p := pair{3, 4}
    fmt.Println(p.String()) // 타입 pair인 p의 String 메서드를 호출.
    var i Stringer          // Stringer 인터페이스 타입 i를 선언.
    i = p                   // pair는 Stringer를 구현했기 때문에 이 대입은 유효하다.
    // 타입 Stringer인 i의 String 메서드 호출. 결과는 위와 같다.
    fmt.Println(i.String())

    // fmt 패키지의 함수들을 통해 어떤 객체를 출력해보려고 할 때,
    // fmt 패키지 내에서는 그 객체가 가진 String 메서드를 호출하도록 되어 있다.
    fmt.Println(p) // 결과는 위와 같다. Println은 String 메서드를 호출한다.
    fmt.Println(i) // 결과는 위와 같다.

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok" (comma okay)표현은 무언가가 맞는 것인지 아닌지 확인하는데 사용된다.
    m := map[int]string{3: "three", 4: "four"}
    if x, ok := m[1]; !ok { // 이 map 안에 키가 1인 것은 없으므로 ok는 false가 된다.
        fmt.Println("no one there")
    } else {
        fmt.Print(x) // 만일 1이 map에 있었다면 x는 키 1의 값이 들어가게 된다.
    }
    // An error value communicates not just "ok" but more about the problem.

    // Go에서는 함수가 복수 개의 리턴 값을 줄 수 있다는 점을 활용해 함수의 두 번째 리턴
    // 값으로 error를 리턴해주고 그 error가 nil 인지 아닌지 확인하는 관례가 있다.
    // 이때 이 error 값은 단지 위에서처럼 함수의 결과가 성공했는지 실패했는지를 확인하는
    // 것뿐만 아니라 실패 시 어떤 문제가 있었는지 확인할 수 있는 수단도 된다.
    if _, err := strconv.Atoi("non-int"); err != nil { // _ 는 값을 안 쓰고 버린다는 의미.
        // "strconv.ParseInt: parsing "non-int": invalid syntax" 이런 에러가 출력된다.
        fmt.Println(err)
    }
    // 인터페이스에 대해 잠시 후에 다시 잠깐 볼 것이다.
    learnConcurrency()
}

// c는 goroutine 간의 통신을 위한 채널(channel)이다.
func inc(i int, c chan int) {
    c <- i + 1 // 채널이 <- 이 연산자 왼쪽에 온다면 그 채널로 데이터를 보낸다는 의미다.
}

// We'll use inc to increment some numbers concurrently.
func learnConcurrency() {
    // Same make function used earlier to make a slice.  Make allocates and
    // initializes slices, maps, and channels.
    c := make(chan int)
    // Start three concurrent goroutines.  Numbers will be incremented
    // concurrently, perhaps in parallel if the machine is capable and
    // properly configured.  All three send to the same channel.
    go inc(0, c) // go is a statement that starts a new goroutine.
    go inc(10, c)
    go inc(-805, c)
    // Read three results from the channel and print them out.
    // There is no telling in what order the results will arrive!
    fmt.Println(<-c, <-c, <-c) // channel on right, <- is "receive" operator.

    cs := make(chan string)       // another channel, this one handles strings.
    cc := make(chan chan string)  // a channel of string channels.
    go func() { c <- 84 }()       // start a new goroutine just to send a value
    go func() { cs <- "wordy" }() // again, for cs this time
    // Select has syntax like a switch statement but each case involves
    // a channel operation.  It selects a case at random out of the cases
    // that are ready to communicate.
    select {
    case i := <-c: // the value received can be assigned to a variable
        fmt.Printf("it's a %T", i)
    case <-cs: // or the value received can be discarded
        fmt.Println("it's a string")
    case <-cc: // empty channel, not ready for communication.
        fmt.Println("didn't happen.")
    }
    // At this point a value was taken from either c or cs.  One of the two
    // goroutines started above has completed, the other will remain blocked.

    learnWebProgramming() // Go does it.  You want to do it too.
}

// http 패키지의 함수 하나로 웹 서버를 실행시킨다.
func learnWebProgramming() {
    // ListenAndServe의 첫 번째 파라미터는 listen 하기 위한 TCP 주소고,
    // 두 번째 파라미터는 http.Handler 인터페이스다.
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // don't ignore errors
}

// http.Handler의 하나 뿐인 메서드, ServeHTTP를 pair에서 구현한다.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // http.ResponseWriter의 메서드로 클라이언트에게 데이터를 보낸다.
    w.Write([]byte("You learned Go in Y minutes!"))
}
```

## 더 읽어볼 것들

The root of all things Go is the [official Go web site](http://golang.org/).
There you can follow the tutorial, play interactively, and read lots.

The language definition itself is highly recommended.  It's easy to read
and amazingly short (as language definitions go these days.)

On the reading list for students of Go is the [source code to the standard
library](http://golang.org/src/pkg/).  Comprehensively documented, it
demonstrates the best of readable and understandable Go, Go style, and Go
idioms.  Or you can click on a function name in [the
documentation](http://golang.org/pkg/) and the source code comes up!

