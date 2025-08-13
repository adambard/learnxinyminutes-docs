---
name: AssemblyScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
    - ["Steve Huguenin-Elie", "https://github.com/StEvUgnIn"]
    - ["Sebastian Speitel", "https://github.com/SebastianSpeitel"]
    - ["Max Graey", "https://github.com/MaxGraey"]
filename: learnassemblyscript.ts
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---
__AssemblyScript__는 __TypeScript__의 변형(기본적으로 타입이 있는 자바스크립트)을 __Binaryen__을 사용하여 __WebAssembly__로 컴파일합니다. `npm install`만으로 간결하고 효율적인 WebAssembly 모듈을 생성합니다.

이 문서는 [TypeScript](../typescript/) 및 [JavaScript](../javascript/)와 대조적으로 AssemblyScript의 추가 구문에만 초점을 맞춥니다.

AssemblyScript의 컴파일러를 테스트하려면
[Playground](https://www.assemblyscript.org/editor.html#IyFydW50aW1lPXN0dWIKLyoqIENhbGN1bGF0ZXMgdGhlIG4tdGggRmlib25hY2NpIG51bWJlci4gKi8KZXhwb3J0IGZ1bmN0aW9uIGZpYihuOiBpMzIpOiBpMzIgewogIHZhciBhID0gMCwgYiA9IDEKICBpZiAobiA+IDApIHsKICAgIHdoaWxlICgtLW4pIHsKICAgICAgbGV0IHQgPSBhICsgYgogICAgICBhID0gYgogICAgICBiID0gdAogICAgfQogICAgcmV0dXJuIGIKICB9CiAgcmV0dXJuIGEKfQoKIyFodG1sCjx0ZXh0YXJlYSBpZD0ib3V0cHV0IiBzdHlsZT0iaGVpZ2h0OiAxMDAlOyB3aWR0aDogMTAwJSIgcmVhZG9ubHk+PC90ZXh0YXJlYT4KPHNjcmlwdD4KbG9hZGVyLmluc3RhbnRpYXRlKG1vZHVsZV93YXNtLCB7IC8qIGltcG9ydHMgKi8gfSkKICAudGhlbigoeyBleHBvcnRzIH0pID0+IHsKICAgIGNvbnN0IG91dHB1dCA9IGRvY3VtZW50LmdldEVsZW1lbnRCeUlkKCdvdXRwdXQnKQogICAgZm9yIChsZXQgaSA9IDA7IGkgPD0gMTA7ICsraSkgewogICAgICBvdXRwdXQudmFsdWUgKz0gYGZpYigke2l9KSA9ICR7ZXhwb3J0cy5maWIoaSl9XG5gCiAgICB9CiAgfSkKPC9zY3JpcHQ+Cg==)로 이동하여 코드를 입력하고 자동 완성을 사용하며 내보낸 WebAssembly를 직접 볼 수 있습니다.

```ts
// AssemblyScript에는 많은 기본 타입이 있습니다.
let isDone: boolean = false;
let name: string = "Anders";

// 하지만 정수 타입은 부호 있는(8비트에서 64비트까지 크기)으로 제공됩니다.
let lines8: i8 = 42;
let lines16: i16 = 42;
let lines32: i32 = 42;
let lines64: i64 = 42;

// 그리고 부호 없는(8비트에서 64비트까지 크기)으로 제공됩니다.
let ulines8: u8 = 42;
let ulines16: u16 = 42;
let ulines32: u32 = 42;
let ulines64: u64 = 42;

// 그리고 float는 두 가지 크기(32/64)가 가능합니다.
let rate32: f32 = 1.0
let rate64: f64 = 1.0

// 하지만 변수가 명시적인 리터럴에서 파생된 경우 타입 주석을 생략할 수 있습니다.
let _isDone = false;
let _lines = 42;
let _name = "Anders";

// 상수에 const 키워드를 사용합니다.
const numLivesForCat = 9;
numLivesForCat = 1; // 오류

// 컬렉션의 경우 타입이 지정된 배열과 제네릭 배열이 있습니다.
let list1: i8[] = [1, 2, 3];
// 또는 제네릭 배열 타입을 사용합니다.
let list2: Array<i8> = [1, 2, 3];

// 열거형의 경우:
enum Color { Red, Green, Blue };
let c: Color = Color.Green;

// 자바스크립트에서 가져온 함수는 외부로 선언해야 합니다.
// @ts-ignore 데코레이터
@external("alert")
declare function alert(message: string): void;

// 그리고 네임스페이스에서 JS 함수를 가져올 수도 있습니다.
declare namespace window {
  // @ts-ignore 데코레이터
  @external("window", "alert")
  function alert(message: string): void;
}

// 마지막으로, "void"는 함수가 아무것도 반환하지 않는 특별한 경우에 사용됩니다.
export function bigHorribleAlert(): void {
  alert("I'm a little annoying box!"); // 여기서 JS 함수 호출
}

// 함수는 일급 시민이며, 람다 "뚱뚱한 화살표" 구문을 지원합니다.

// 다음은 동일하며, 컴파일러는 아직 함수에 대한 타입 추론을 제공하지 않으며
// 동일한 WebAssembly가 내보내집니다.
export function f1 (i: i32): i32 { return i * i; }
// "뚱뚱한 화살표" 구문
let f2 = (i: i32): i32 => { return i * i; }
// "뚱뚱한 화살표" 구문, 중괄호가 없으면 return 키워드가 필요하지 않음을 의미합니다.
let f3 = (i: i32): i32 => i * i;

// 클래스 - 멤버는 기본적으로 public입니다.
export class Point {
  // 속성
  x: f64;

  // 생성자 - 이 컨텍스트의 public/private 키워드는
  // 속성에 대한 상용구 코드와 생성자의 초기화를 생성합니다.
  // 이 예제에서 "y"는 "x"와 같이 정의되지만 코드는 더 적습니다.
  // 기본값도 지원됩니다.

  constructor(x: f64, public y: f64 = 0) {
    this.x = x;
  }

  // 함수
  dist(): f64 { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // 정적 멤버
  static origin: Point = new Point(0, 0);
}

// 클래스는 부모 클래스를 확장한다고 명시적으로 표시할 수 있습니다.
// 누락된 속성은 컴파일 타임에 오류를 발생시킵니다.
export class PointPerson extends Point {
  constructor(x: f64, y: f64, public name: string) {
    super(x, y);
  }
  move(): void {}
}

let p1 = new Point(10, 20);
let p2 = new Point(25); //y는 0이 됩니다.

// 상속
export class Point3D extends Point {
  constructor(x: f64, y: f64, public z: f64 = 0) {
    super(x, y); // 슈퍼 클래스 생성자에 대한 명시적 호출은 필수입니다.
  }

  // 덮어쓰기
  dist(): f64 {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// 네임스페이스, "."는 하위 네임스페이스의 구분 기호로 사용할 수 있습니다.
export namespace Geometry {
  class Square {
    constructor(public sideLength: f64 = 0) {
    }
    area(): f64 {
      return Math.pow(this.sideLength, 2);
    }
  }
}

let s1 = new Geometry.Square(5);

// 제네릭
// AssemblyScript는 제네릭을 고유한 컨텍스트 타입 인수 집합당 하나의 구체적인 메서드 또는 함수로 컴파일합니다.
// 이를 [단형화]라고도 합니다.
// 이는 모듈이 실제로 사용되는 타입 인수 집합에 대한 구체적인 함수만 포함하고 내보내며
// 구체적인 함수는 컴파일 타임에 [정적 타입 검사]로 단축될 수 있음을 의미하며, 이는 매우 유용한 것으로 판명되었습니다.
// 클래스
export class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

export class Pair<T> {
  item1: T;
  item2: T;
}

// 그리고 함수
export function pairToTuple <T>(p: Pair<T>): Tuple<T, T> {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple<string>({ item1: "hello", item2: "world" });

// TypeScript 전용 정의 파일에 대한 참조 포함:
/// <reference path="jquery.d.ts" />

// 템플릿 문자열 (백틱을 사용하는 문자열)
// 템플릿 문자열을 사용한 문자열 보간
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`
// 템플릿 문자열을 사용한 여러 줄 문자열
let multiline = `This is an example
of a multiline string`;

let numbers: Array<i8> = [0, 1, 2, 3, 4];
let moreNumbers: Array<i8> = numbers;
moreNumbers[5] = 5; // 오류, 요소는 읽기 전용입니다.
moreNumbers.push(5); // 오류, push 메서드 없음 (배열을 변경하기 때문)
moreNumbers.length = 3; // 오류, 길이는 읽기 전용입니다.
numbers = moreNumbers; // 오류, 변경 메서드가 없습니다.

// 배열의 타입 추론
let ints = [0, 1, 2, 3, 4]  // Array<i32>로 추론됩니다.
let floats: f32[] = [0, 1, 2, 3, 4]  // Array<f32>로 추론됩니다.
let doubles = [0.0, 1.0, 2, 3, 4]  // Array<f64>로 추론됩니다.
let bytes1 = [0 as u8, 1, 2, 3, 4]  // Array<u8>로 추론됩니다.
let bytes2 = [0, 1, 2, 3, 4]  as u8[] // Array<u8>로 추론됩니다.
let bytes3: u8[] = [0, 1, 2, 3, 4] // Array<u8>로 추론됩니다.
```

## 더 읽을거리

 * [AssemblyScript 공식 웹사이트](https://www.assemblyscript.org/)
 * [AssemblyScript 소스 문서](https://github.com/AssemblyScript/website/tree/main/src)
 * [GitHub의 소스 코드](https://github.com/AssemblyScript/assemblyscript)