---
name: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Kiwimoe", "https://github.com/kiwimoe"]
filename: learntypescript.ts
---

TypeScript는 JavaScript로 작성된 대규모 애플리케이션 개발을 용이하게 하는 것을 목표로 하는 언어입니다. TypeScript는 클래스, 모듈, 인터페이스, 제네릭 및 (선택적) 정적 타이핑과 같은 일반적인 개념을 JavaScript에 추가합니다. JavaScript의 상위 집합입니다. 모든 JavaScript 코드는 유효한 TypeScript 코드이므로 모든 프로젝트에 원활하게 추가할 수 있습니다. TypeScript 컴파일러는 JavaScript를 내보냅니다.

이 기사는 [JavaScript](../javascript/)와 달리 TypeScript 추가 구문에만 초점을 맞춥니다.

TypeScript 컴파일러를 테스트하려면 [Playground](https://www.typescriptlang.org/play)로 이동하여 코드를 입력하고 자동 완성을 사용하며 내보낸 JavaScript를 직접 볼 수 있습니다.

```ts
// TypeScript에는 3가지 기본 유형이 있습니다.
let isDone: boolean = false;
let lines: number = 42;
let name: string = "Anders";

// 그러나 변수가 명시적 리터럴에서 파생된 경우 유형 주석을 생략할 수 있습니다.
let isDone = false;
let lines = 42;
let name = "Anders";

// 알 수 없는 경우 "Any" 유형이 있습니다.
let notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // 좋아요, 확실히 부울입니다.

// 상수에 const 키워드 사용
const numLivesForCat = 9;
numLivesForCat = 1; // 오류

// 컬렉션의 경우 유형이 지정된 배열과 제네릭 배열이 있습니다.
let list: number[] = [1, 2, 3];
// 또는 제네릭 배열 유형 사용
let list: Array<number> = [1, 2, 3];

// 열거형의 경우:
enum Color { Red, Green, Blue };
let c: Color = Color.Green;
console.log(Color[c]); // "Green"

// 마지막으로 "void"는 함수가 아무것도 반환하지 않는 특수한 경우에 사용됩니다.
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// 함수는 일급 시민이며 람다 "뚱뚱한 화살표" 구문을 지원하고
// 유형 추론을 사용합니다.

// 다음은 동일하며 컴파일러에서 동일한 서명이 추론되고
// 동일한 JavaScript가 내보내집니다.
let f1 = function (i: number): number { return i * i; }
// 반환 유형 추론됨
let f2 = function (i: number) { return i * i; }
// "뚱뚱한 화살표" 구문
let f3 = (i: number): number => { return i * i; }
// 반환 유형이 추론된 "뚱뚱한 화살표" 구문
let f4 = (i: number) => { return i * i; }
// 반환 유형이 추론된 "뚱뚱한 화살표" 구문, 중괄호 없음은 반환
// 키워드가 필요 없음을 의미합니다.
let f5 = (i: number) => i * i;

// 함수는 둘 이상의 유형을 허용할 수 있습니다.
function f6(i: string | number): void {
  console.log("The value was " + i);
}

// 인터페이스는 구조적이며 속성이 있는 모든 것은
// 인터페이스와 호환됩니다.
interface Person {
  name: string;
  // 선택적 속성, "?"로 표시
  age?: number;
  // 그리고 물론 함수
  move(): void;
}

// "Person" 인터페이스를 구현하는 객체
// 이름과 이동 속성이 있으므로 Person으로 취급할 수 있습니다.
let p: Person = { name: "Bobby", move: () => { } };
// 선택적 속성이 있는 객체:
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };
// 나이가 숫자가 아니므로 사람이 아닙니다.
let invalidPerson: Person = { name: "Bobby", age: true };

// 인터페이스는 함수 유형을 설명할 수도 있습니다.
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// 매개변수의 유형만 중요하며 이름은 중요하지 않습니다.
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// 클래스 - 멤버는 기본적으로 public입니다.
class Point {
  // 속성
  x: number;

  // 생성자 - 이 컨텍스트의 public/private 키워드는
  // 속성 및 생성자의 초기화에 대한 상용구 코드를
  // 생성합니다.
  // 이 예에서 "y"는 "x"와 동일하게 정의되지만 코드가 더 적습니다.
  // 기본값도 지원됩니다.

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // 함수
  dist(): number { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // 정적 멤버
  static origin = new Point(0, 0);
}

// 클래스는 인터페이스를 구현하는 것으로 명시적으로 표시할 수 있습니다.
// 누락된 속성은 컴파일 타임에 오류를 발생시킵니다.
class PointPerson implements Person {
    name: string
    move() {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); //y는 0이 됩니다.

// 상속
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // 슈퍼 클래스 생성자를 명시적으로 호출해야 합니다.
  }

  // 재정의
  dist(): number {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// 모듈, "."는 하위 모듈의 구분 기호로 사용할 수 있습니다.
module Geometry {
  export class Square {
    constructor(public sideLength: number = 0) {
    }
    area() {
      return Math.pow(this.sideLength, 2);
    }
  }
}

let s1 = new Geometry.Square(5);

// 모듈을 참조하기 위한 로컬 별칭
import G = Geometry;

let s2 = new G.Square(10);

// 제네릭
// 클래스
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// 인터페이스
interface Pair<T> {
  item1: T;
  item2: T;
}

// 및 함수
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// 정의 파일에 대한 참조 포함:
/// <reference path="jquery.d.ts" />

// 템플릿 문자열 (백틱을 사용하는 문자열)
// 템플릿 문자열을 사용한 문자열 보간
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// 템플릿 문자열을 사용한 여러 줄 문자열
let multiline = `This is an example
of a multiline string`;

// READONLY: TypeScript 3.1의 새로운 기능
interface Person {
  readonly name: string;
  readonly age: number;
}

var p1: Person = { name: "Tyrone", age: 42 };
p1.age = 25; // 오류, p1.age는 읽기 전용입니다.

var p2 = { name: "John", age: 60 };
var p3: Person = p2; // 확인, p2에 대한 읽기 전용 별칭
p3.age = 35; // 오류, p3.age는 읽기 전용입니다.
p2.age = 45; // 확인, 하지만 별칭 때문에 p3.age도 변경됩니다.

class Car {
  readonly make: string;
  readonly model: string;
  readonly year = 2018;

  constructor() {
    this.make = "Unknown Make"; // 생성자에서 할당 허용
    this.model = "Unknown Model"; // 생성자에서 할당 허용
  }
}

let numbers: Array<number> = [0, 1, 2, 3, 4];
let moreNumbers: ReadonlyArray<number> = numbers;
moreNumbers[5] = 5; // 오류, 요소는 읽기 전용입니다.
moreNumbers.push(5); // 오류, push 메서드 없음 (배열을 변경하기 때문)
moreNumbers.length = 3; // 오류, 길이는 읽기 전용입니다.
numbers = moreNumbers; // 오류, 변경 메서드가 없습니다.

// 여러 모양 중 하나일 수 있는 상태를 모델링하기 위한 태그된 유니온 타입
type State =
  | { type: "loading" }
  | { type: "success", value: number }
  | { type: "error", message: string };

declare const state: State;
if (state.type === "success") {
  console.log(state.value);
} else if (state.type === "error") {
  console.error(state.message);
}

// 템플릿 리터럴 타입
// 복잡한 문자열 타입을 만드는 데 사용
type OrderSize = "regular" | "large";
type OrderItem = "Espresso" | "Cappuccino";
type Order = `A ${OrderSize} ${OrderItem}`;

let order1: Order = "A regular Cappuccino";
let order2: Order = "A large Espresso";
let order3: Order = "A small Espresso"; // 오류

// 반복자 및 생성기

// for..of 문
// 반복되는 객체의 값 목록을 반복
let arrayOfAnyType = [1, "string", false];
for (const val of arrayOfAnyType) {
    console.log(val); // 1, "string", false
}

let list = [4, 5, 6];
for (const i of list) {
   console.log(i); // 4, 5, 6
}

// for..in 문
// 반복되는 객체의 키 목록을 반복
for (const i in list) {
   console.log(i); // "0", "1", "2"
}

// 타입 단언

let foo = {} // foo를 빈 객체로 생성
foo.bar = 123 // 오류: 속성 'bar'가 `{}`에 존재하지 않음
foo.baz = 'hello world' // 오류: 속성 'baz'가 `{}`에 존재하지 않음

// foo의 추론된 타입이 `{}`(속성 0개인 객체)이므로
// bar와 baz를 추가할 수 없습니다. 그러나 타입 단언을 사용하면
// 다음이 통과합니다.

interface Foo {
  bar: number;
  baz: string;
}

let foo = {} as Foo; // 여기서 타입 단언
foo.bar = 123;
foo.baz = 'hello world'
```

## 더 읽을거리

* [공식 TypeScript 웹사이트](https://www.typescriptlang.org/)
* [GitHub의 소스 코드](https://github.com/microsoft/TypeScript)
* [TypeScript 배우기](https://learntypescript.dev/)
