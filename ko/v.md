---
name: V
filename: vlang.v
contributors:
    - ["Maou Shimazu", "https://github.com/Maou-Shimazu"]
---

V는 유지보수 가능한 소프트웨어를 구축하기 위해 설계된 정적 타입 컴파일 프로그래밍 언어입니다.

Go와 유사하며 Oberon, Rust, Swift, Kotlin, Python의 영향도 받았습니다.

이 언어는 최소한의 추상화로 간단하고 명확한 코드를 작성하도록 권장합니다.

단순함에도 불구하고 V는 개발자에게 많은 강력한 기능을 제공합니다.
다른 언어에서 할 수 있는 모든 것을 V에서도 할 수 있습니다.

```v
// 한 줄 주석.
/*
    여러 줄 주석
*/

struct User { // main 함수 내부에 정의할 수 없으며, 나중에 설명합니다.
	age  int
	name string
	pos int = -1 // 사용자 정의 기본값
}
// 구조체 메서드
fn (u User) can_register() bool {
	return u.age > 16
}

struct Parser {
	token Token
}

// c와 유사한 열거형
enum Token {
	plus
	minus
	div
	mult
}

// 1. 함수
// 언어는 세미콜론을 사용하지 않습니다
fn add(x int, y int) int {
	return x + y
}
// 여러 값을 반환할 수 있습니다
fn foo() (int, int) {
	return 2, 3
}

// 함수 가시성
pub fn public_function() { // pub는 명명된 모듈에서만 사용할 수 있습니다.
}

fn private_function() {
}



// 메인 함수
fn main() {
	// 익명 함수는 다른 함수 내부에 선언할 수 있습니다:
	double_fn := fn (n int) int {
		return n + n
	}
	// 2. 변수: 기본적으로 불변입니다
	// 암시적 타입
	x := 1
	// x = 2 // 오류
	mut y := 2
	y = 4
	name := "John"
	large_number := i64(9999999999999)
    println("$x, $y, $name, $large_number") // 1, 4, John, 9999999999999

	// 함수에서 값 언패킹.
	a, b := foo()
	println("$a, $b") // 2, 3
	c, _ := foo() // `_`를 사용하여 값 무시
	println("$c") // 2

	// 숫자
	u := u16(12)
	v := 13 + u    // v는 `u16` 타입입니다
	r := f32(45.6)
	q := r + 3.14  // x는 `f32` 타입입니다
	s := 75        // a는 `int` 타입입니다
	l := 14.7      // b는 `f64` 타입입니다
	e := u + s     // c는 `int` 타입입니다
	d := l + r     // d는 `f64` 타입입니다

	// 문자열
	mut bob := 'Bob'
	assert bob[0] == u8(66) // 인덱싱은 바이트를 반환, u8(66) == `B`
	assert bob[1..3] == 'ob'  // 슬라이싱은 문자열 'ob'를 반환
	bobby := bob + 'by' // +는 문자열을 연결하는 데 사용됩니다
	println(bobby) // "Bobby"
	bob += "by2" // +=는 문자열에 추가하는 데 사용됩니다
	println(bob) // "Bobby2"

	//문자열 값은 불변입니다. 요소를 변경할 수 없습니다:
	//mut s := 'hello 🌎'
	//s[0] = `H` // 허용되지 않음

	//원시 문자열의 경우 r을 앞에 붙입니다. 원시 문자열에 대해서는 이스케이프 처리가 수행되지 않습니다:
	rstring := r'hello\nworld' // `\n`은 두 문자로 유지됩니다
	println(rstring) // "hello\nworld"

	// 문자열 보간
	println('Hello, $bob!') // Hello, Bob!
	println('Bob length + 10: ${bob.len + 10}!') // Bob length + 10: 13!

	// 3. 배열
	mut numbers := [1, 2, 3]
	println(numbers) // `[1, 2, 3]`
	numbers << 4 // <<로 요소 추가
	println(numbers[3]) // `4`
	numbers[1] = 5
	println(numbers) // `[1, 5, 3]`
	// numbers << "John" // 오류: `numbers`는 숫자 배열입니다
	numbers = [] // 배열이 이제 비어 있습니다
	arr := []int{len: 5, init: -1}
	// `arr == [-1, -1, -1, -1, -1]`, arr.cap == 5

	number_slices := [0, 10, 20, 30, 40]
	println(number_slices[1..4]) // [10, 20, 30]
	println(number_slices[..4]) // [0, 10, 20, 30]
	println(number_slices[1..]) // [10, 20, 30, 40]

	// 4. 구조체 및 열거형
	// struct User {
	// 	age  int
	// 	name string
	//  pos int = -1 // 사용자 정의 기본값
	// }
	mut users := User{21, 'Bob', 0}
	println(users.age) // 21

	// enum Token {
	// 	plus
	// 	minus
	// 	div
	// 	mult
	// }

	// struct Parser {
	// 	token Token
	// }
	parser := Parser{}
	if parser.token == .plus || parser.token == .minus
	|| parser.token == .div || parser.token == .mult {
		// ...
	}


	// 5. 맵
	number_map := {
		'one': 1
		'two': 2
	}
	println(number_map) // {'one': 1, 'two': 2}
	println(number_map["one"]) // 1
	mut m := map[string]int{} // `string` 키와 `int` 값을 갖는 맵
	m['one'] = 1
	m['two'] = 2
	println(m['one']) // "1"
	println(m['bad_key']) // "0"
	m.delete('two')

	// 6. 조건문
	a_number := 10
	b_number := 20
	if a_number < b {
		println('$a_number < $b_number')
	} else if a_number > b {
		println('$a_number > $b_number')
	} else {
		println('$a_number == $b_number')
	}
	num := 777
	even_odd := if num % 2 == 0 { 'even' } else { 'odd' }
	println(even_odd)

	match even_odd {
		'even' { println('even') }
		'odd' { println('odd') }
		else { println('unknown') }
	}

	// 7. 루프
	loops := [1, 2, 3, 4, 5]
	for lp in loops {
		println(lp)
	}
	loop_names := ['Sam', 'Peter']
	for i, lname in loop_names {
		println('$i) $lname')
		// 출력: 0) Sam
		//         1) Peter
	}
	// break 및 continue 다음에 레이블 이름을 사용하여
	// 외부 for 루프를 참조할 수도 있습니다:
	outer: for i := 4; true; i++ {
		println(i)
		for {
			if i < 7 {
				continue outer
			} else {
				break outer
			}
		}
	}
}
```

## 더 읽을거리

V에는 공식 [V 문서](https://github.com/vlang/v/blob/master/doc/docs.md)에서 배울 수 있는 더 복잡한 개념이 있습니다.

V 언어에 대한 자세한 정보는 [공식 웹사이트](https://vlang.io/)에서 찾거나 [v 플레이그라운드](https://v-wasm.vercel.app/)에서 확인할 수 있습니다.
