---
name: WebAssembly
filename: learn-wasm.wast
contributors:
    - ["Dean Shaff", "http://dean-shaff.github.io"]
---

```wast
;; learn-wasm.wast

(module
  ;; WebAssembly에서는 모든 것이 모듈에 포함됩니다. 또한, 모든 것을
  ;; s-표현식으로 표현할 수 있습니다. 또는 "스택 머신" 구문이 있지만,
  ;; 이는 Binaryen 중간 표현(IR) 구문과 호환되지 않습니다.

  ;; Binaryen IR 형식은 WebAssembly 텍스트 형식과 *대부분* 호환됩니다.
  ;; 몇 가지 작은 차이점이 있습니다:
  ;; local_set -> local.set
  ;; local_get -> local.get

  ;; 코드를 함수 안에 포함해야 합니다.

  ;; 데이터 유형
  (func $data_types
    ;; WebAssembly에는 네 가지 유형만 있습니다:
    ;; i32 - 32비트 정수
    ;; i64 - 64비트 정수 (JavaScript에서는 지원되지 않음)
    ;; f32 - 32비트 부동 소수점
    ;; f64 - 64비트 부동 소수점

    ;; "local" 키워드로 지역 변수를 선언할 수 있습니다.
    ;; 함수 내에서 어떤 작업을 시작하기 전에 모든 변수를 선언해야 합니다.

    (local $int_32 i32)
    (local $int_64 i64)
    (local $float_32 f32)
    (local $float_64 f64)

    ;; 이 값들은 초기화되지 않은 상태로 남아 있습니다.
    ;; 값을 설정하려면 <type>.const를 사용할 수 있습니다:

    (local.set $int_32 (i32.const 16))
    (local.set $int_64 (i64.const 128))
    (local.set $float_32 (f32.const 3.14))
    (local.set $float_64 (f64.const 1.28))
  )

  ;; 기본 연산
  (func $basic_operations

    ;; WebAssembly에서는 수학을 하거나 변수 값을 가져오는 것을
    ;; 포함하여 모든 것이 s-표현식입니다.

    (local $add_result i32)
    (local $mult_result f64)

    (local.set $add_result (i32.add (i32.const 2) (i32.const 4)))
    ;; add_result의 값은 이제 6입니다!

    ;; 각 연산에 올바른 데이터 유형을 사용해야 합니다:
    ;; (local.set $mult_result (f32.mul (f32.const 2.0) (f32.const 4.0))) ;; 틀림! mult_result는 f64입니다!
    (local.set $mult_result (f64.mul (f64.const 2.0) (f64.const 4.0)))

    ;; WebAssembly에는 기본 수학 및 비트 시프트와 같은 일부 내장 연산이 있습니다.
    ;; 특히 삼각 함수는 내장되어 있지 않습니다.
    ;; 이러한 함수에 액세스하려면 다음 중 하나를 수행해야 합니다.
    ;; - 직접 구현 (권장하지 않음)
    ;; - 다른 곳에서 가져오기 (나중에)
  )

  ;; 함수
  ;; `param` 키워드로 인수를 지정하고 `result` 키워드로 반환 값을
  ;; 지정합니다.
  ;; 스택의 현재 값은 함수의 반환 값입니다.

  ;; `call` 키워드로 정의한 다른 함수를 호출할 수 있습니다.

  (func $get_16 (result i32)
    (i32.const 16)
  )

  (func $add (param $param0 i32) (param $param1 i32) (result i32)
    (i32.add
      (local.get $param0)
      (local.get $param1)
    )
  )

  (func $double_16 (result i32)
    (i32.mul
      (i32.const 2)
      (call $get_16))
  )

  ;; 지금까지는 아무것도 인쇄할 수 없었고,
  ;; 상위 수준의 수학 함수(pow, exp 또는 삼각 함수)에도 액세스할 수 없었습니다.
  ;; 또한 JavaScript에서 WASM 함수를 사용할 수 없었습니다!
  ;; WebAssembly에 이러한 함수를 가져오는 방법은
  ;; Node.js 또는 브라우저 환경에 따라 다르게 보입니다.

  ;; Node.js에 있는 경우 두 단계를 수행해야 합니다. 먼저 WASM 텍스트
  ;; 표현을 실제 웹어셈블리로 변환해야 합니다. Binyaren을 사용하는 경우
  ;; 다음과 같은 명령으로 수행할 수 있습니다:

  ;; wasm-as learn-wasm.wast -o learn-wasm.wasm

  ;; 다음과 같은 명령으로 해당 파일에 Binaryen 최적화를 적용할 수 있습니다:

  ;; wasm-opt learn-wasm.wasm -o learn-wasm.opt.wasm -O3 --rse

  ;; 컴파일된 WebAssembly를 사용하여 이제 Node.js에 로드할 수 있습니다:
  ;; const fs = require('fs')
  ;; const instantiate = async function (inFilePath, _importObject) {
  ;;  var importObject = {
  ;;     console: {
  ;;       log: (x) => console.log(x),
  ;;     },
  ;;     math: {
  ;;       cos: (x) => Math.cos(x),
  ;;     }
  ;;   }
  ;;  importObject = Object.assign(importObject, _importObject)
  ;;
  ;;  var buffer = fs.readFileSync(inFilePath)
  ;;  var module = await WebAssembly.compile(buffer)
  ;;  var instance = await WebAssembly.instantiate(module, importObject)
  ;;  return instance.exports
  ;; }
  ;;
  ;; const main = function () {
  ;;   var wasmExports = await instantiate('learn-wasm.wasm')
  ;;   wasmExports.print_args(1, 0)
  ;; }

  ;; 다음 스니펫은 JavaScript instantiate async 함수에서 정의한
  ;; importObject에서 함수를 가져온 다음, Node.js에서 호출할 수 있는
  ;; "print_args" 함수를 내보냅니다.

  (import "console" "log" (func $print_i32 (param i32)))
  (import "math" "cos" (func $cos (param f64) (result f64)))

  (func $print_args (param $arg0 i32) (param $arg1 i32)
    (call $print_i32 (local.get $arg0))
    (call $print_i32 (local.get $arg1))
  )
  (export "print_args" (func $print_args))

  ;; WebAssembly 메모리에서 데이터 로드.
  ;; JavaScript 배열에 코사인 함수를 적용하고 싶다고 가정해 봅시다.
  ;; 할당된 배열에 액세스하고 반복할 수 있어야 합니다.
  ;; 이 예에서는 입력 배열을 제자리에서 수정합니다.
  ;; f64.load 및 f64.store는 메모리에서 숫자의 위치를 *바이트* 단위로 예상합니다.
  ;; 배열의 세 번째 요소에 액세스하려면
  ;; (i32.mul (i32.const 8) (i32.const 2))와 같은 것을 f64.store 함수에
  ;; 전달해야 합니다.

  ;; JavaScript에서는 다음과 같이 `apply_cos64`를 호출합니다.
  ;; (이전의 instantiate 함수 사용):
  ;;
  ;; const main = function () {
  ;;   var wasm = await instantiate('learn-wasm.wasm')
  ;;   var n = 100
  ;;   const memory = new Float64Array(wasm.memory.buffer, 0, n)
  ;;   for (var i=0; i<n; i++) {
  ;;     memory[i] = i;
  ;;   }
  ;;   wasm.apply_cos64(n)
  ;; }
  ;;
  ;; 이 함수는 JavaScript 측에서 Float32Array를 할당하면
  ;; 작동하지 않습니다.

  (memory (export "memory") 100)

  (func $apply_cos64 (param $array_length i32)
    ;; 루프 카운터 선언
    (local $idx i32)
    ;; 메모리에 액세스할 수 있도록 하는 카운터 선언
    (local $idx_bytes i32)
    ;; f64 숫자의 바이트 수를 나타내는 상수.
    (local $bytes_per_double i32)

    ;; 메모리에서 로드된 값을 저장하기 위한 변수 선언
    (local $temp_f64 f64)

    (local.set $idx (i32.const 0))
    (local.set $idx_bytes (i32.const 0)) ;; 완전히 필요하지는 않음
    (local.set $bytes_per_double (i32.const 8))

    (block
      (loop
        ;; 이것은 idx_bytes를 관심 있는 값의 바이트 오프셋으로 설정합니다.
        (local.set $idx_bytes (i32.mul (local.get $idx) (local.get $bytes_per_double)))

        ;; 메모리에서 배열 값을 가져옵니다:
        (local.set $temp_f64 (f64.load (local.get $idx_bytes)))

        ;; 이제 코사인 함수를 적용합니다:
        (local.set $temp_64 (call $cos (local.get $temp_64)))

        ;; 이제 결과를 메모리의 동일한 위치에 저장합니다:
        (f64.store
          (local.get $idx_bytes)
          (local.get $temp_64))

        ;; 대신 한 단계로 모두 수행
        (f64.store
          (local.get $idx_bytes)
          (call $cos
            (f64.load
              (local.get $idx_bytes))))

        ;; 루프 카운터 증가
        (local.set $idx (i32.add (local.get $idx) (i32.const 1)))

        ;; 루프 카운터가 배열 길이와 같으면 루프 중지
        (br_if 1 (i32.eq (local.get $idx) (local.get $array_length)))
        (br 0)
      )
    )
  )
  (export "apply_cos64" (func $apply_cos64))

  ;; Wasm은 스택 기반 언어이지만, int/float보다 복잡한 값을
  ;; 반환하려면 별도의 메모리 스택을 수동으로 관리해야 합니다. 한 가지
  ;; 접근 방식은 변경 가능한 전역 변수를 사용하여 stack_ptr을 저장하는 것입니다.
  ;; 1MiB의 memstack을 제공하고 아래로 확장합니다.
  ;;
  ;; 아래는 이 C 코드를 수동으로 작성하는 방법의 데모입니다.
  ;;
  ;;   typedef struct {
  ;;       int a;
  ;;       int b;
  ;;   } sum_struct_t;
  ;;
  ;;   sum_struct_t sum_struct_create(int a, int b) {
  ;;     return (sum_struct_t){a, b};
  ;;   }
  ;;
  ;;   int sum_local() {
  ;;     sum_struct_t s = sum_struct_create(40, 2);
  ;;     return s.a + s.b;
  ;;   }

  ;; C와 달리 메모리 스택을 직접 관리해야 합니다. 1MiB를 예약합니다.
  (global $memstack_ptr (mut i32) (i32.const 65536))

  ;; 구조체는 참조로만 반환할 수 있습니다.
  (func $sum_struct_create
        (param $sum_struct_ptr i32)
        (param $var$a i32)
        (param $var$b i32)
    ;; c// sum_struct_ptr->a = a;
    (i32.store
      (get_local $sum_struct_ptr)
      (get_local $var$a)
    )

    ;; c// sum_struct_ptr->b = b;
    (i32.store offset=4
      (get_local $sum_struct_ptr)
      (get_local $var$b)
    )
  )

  (func $sum_local (result i32)
    (local $var$sum_struct$a i32)
    (local $var$sum_struct$b i32)
    (local $local_memstack_ptr i32)

    ;; memstack 공간 예약
    (i32.sub
      (get_global $memstack_ptr)
      (i32.const 8)
    )
    tee_local $local_memstack_ptr ;; tee는 주어진 값을 저장하고 반환합니다.
    set_global $memstack_ptr

    ;; 함수를 호출하여 결과를 memstack에 저장합니다.
    (call $sum_struct_create
      ((;$sum_struct_ptr=;) get_local $local_memstack_ptr)
      ((;$var$a=;) i32.const 40)
      ((;$var$b=;) i32.const 2)
    )

    ;; 구조체에서 값 검색
    (set_local $var$sum_struct$a
      (i32.load offset=0 (get_local $local_memstack_ptr))
    )
    (set_local $var$sum_struct$b
      (i32.load offset=4 (get_local $local_memstack_ptr))
    )

    ;; memstack 공간 예약 해제
    (set_global $memstack_ptr
        (i32.add
          (get_local $local_memstack_ptr)
          (i32.const 8)
        )
    )

    (i32.add
      (get_local $var$sum_struct$a)
      (get_local $var$sum_struct$b)
    )
  )
  (export "sum_local" (func $sum_local))
)
```
