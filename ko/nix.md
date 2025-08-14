# nix.md (번역)

---
name: Nix
filename: learn.nix
contributors:
    - ["Chris Martin", "http://chris-martin.org/"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Javier Candeira", "https://candeira.com/"]
---

Nix는 [Nix 패키지 관리자](https://nixos.org/nix/) 및
[NixOS](https://nixos.org/)를 위해 개발된 간단한 함수형 언어입니다.

[nix-instantiate](https://nixos.org/nix/manual/#sec-nix-instantiate)
또는 [`nix repl`](https://nixos.org/nix/manual/#ssec-relnotes-2.0)를 사용하여 Nix 표현식을 평가할 수 있습니다.

```nix
with builtins; [

  #  주석
  #=========================================

  # 인라인 주석은 이렇습니다.

  /* 여러 줄 주석은
     이렇습니다. */


  #  불리언
  #=========================================

  (true && false)               # And
  #=> false

  (true || false)               # Or
  #=> true

  (if 3 < 4 then "a" else "b")  # 조건문
  #=> "a"


  #  정수와 부동 소수점
  #=========================================

  # 두 가지 숫자 타입이 있습니다: 정수와 부동 소수점

  1 0 42 (-3)       # 일부 정수

  123.43 .27e13     # 두 개의 부동 소수점

  # 연산은 숫자 타입을 유지합니다

  (4 + 6 + 12 - 2)  # 덧셈
  #=> 20
  (4 - 2.5)
  #=> 1.5

  (7 / 2)           # 나눗셈
  #=> 3
  (7 / 2.0)
  #=> 3.5


  #  문자열
  #=========================================

  "문자열 리터럴은 큰따옴표 안에 있습니다."

  "
    문자열 리터럴은 여러 줄에 걸쳐
    있을 수 있습니다.
  "

  ''
    이것은 "들여쓰기된 문자열" 리터럴이라고 합니다.
    선행 공백을 지능적으로 제거합니다.
  ''

  ''
    a
      b
  ''
  #=> "a\n  b"

  ("ab" + "cd")   # 문자열 연결
  #=> "abcd"

  # 문자열 보간(이전에는 "antiquotation"으로 알려짐)을 사용하면 문자열에 값을 포함할 수 있습니다.
  ("Your home directory is ${getEnv "HOME"}")
  #=> "Your home directory is /home/alice"


  #  경로
  #=========================================

  # Nix에는 경로에 대한 기본 데이터 타입이 있습니다.
  /tmp/tutorials/learn.nix

  # 상대 경로는 구문 분석 시 절대 경로로 확인되며,
  # 해당 경로가 발생하는 파일을 기준으로 합니다.
  tutorials/learn.nix
  #=> /the-base-path/tutorials/learn.nix

  # 같은 디렉토리의 파일에 대한 상대 경로는
  # ./ 접두사가 필요합니다.
  ./learn.nix
  #=> /the-base-path/learn.nix

  # 나눗셈을 의미하려면 / 연산자를
  # 공백으로 둘러싸야 합니다.

  7/2        # 이것은 경로 리터럴입니다
  (7 / 2)    # 이것은 정수 나눗셈입니다


  #  가져오기
  #=========================================

  # nix 파일에는 자유 변수가 없는 단일 최상위 표현식이 포함됩니다.
  # 가져오기 표현식은 가져오는 파일의 값으로 평가됩니다.
  (import /tmp/foo.nix)

  # 가져오기는 문자열로도 지정할 수 있습니다.
  (import "/tmp/foo.nix")

  # 가져오기 경로는 절대 경로여야 합니다. 경로 리터럴은
  # 자동으로 확인되므로 괜찮습니다.
  (import ./foo.nix)

  # 그러나 이것은 문자열에서는 발생하지 않습니다.
  (import "./foo.nix")
  #=> error: string ‘foo.nix’ doesn't represent an absolute path


  #  Let
  #=========================================

  # `let` 블록을 사용하면 값을 변수에 바인딩할 수 있습니다.
  (let x = "a"; in
    x + x + x)
  #=> "aaa"

  # 바인딩은 서로 참조할 수 있으며 순서는 중요하지 않습니다.
  (let y = x + "b";
       x = "a"; in
    y + "c")
  #=> "abc"

  # 내부 바인딩은 외부 바인딩을 가립니다.
  (let a = 1; in
    let a = 2; in
      a)
  #=> 2


  #  함수
  #=========================================

  (n: n + 1)      # 1을 더하는 함수

  ((n: n + 1) 5)  # 5에 적용된 동일한 함수
  #=> 6

  # 명명된 함수에 대한 구문은 없지만,
  # 다른 값과 마찬가지로 `let` 블록으로 바인딩할 수 있습니다.
  (let succ = (n: n + 1); in succ 5)
  #=> 6

  # 함수에는 정확히 하나의 인수가 있습니다.
  # 커링을 통해 여러 인수를 얻을 수 있습니다.
  ((x: y: x + "-" + y) "a" "b")
  #=> "a-b"

  # 나중에 집합을 소개한 후 명명된 함수 인수를 가질 수도 있습니다.


  #  리스트
  #=========================================

  # 리스트는 대괄호로 표시됩니다.

  (length [1 2 3 "x"])
  #=> 4

  ([1 2 3] ++ [4 5])
  #=> [1 2 3 4 5]

  (concatLists [[1 2] [3 4] [5]])
  #=> [1 2 3 4 5]

  (head [1 2 3])
  #=> 1
  (tail [1 2 3])
  #=> [2 3]

  (elemAt ["a" "b" "c" "d"] 2)
  #=> "c"

  (elem 2 [1 2 3])
  #=> true
  (elem 5 [1 2 3])
  #=> false

  (filter (n: n < 3) [1 2 3 4])
  #=> [ 1 2 ]


  #  집합
  #=========================================

  # "집합"은 문자열 키를 사용하는 정렬되지 않은 매핑입니다.
  { foo = [1 2]; bar = "x"; }

  # . 연산자는 집합에서 값을 꺼냅니다.
  { a = 1; b = 2; }.a
  #=> 1

  # ? 연산자는 집합에 키가 있는지 테스트합니다.
  ({ a = 1; b = 2; } ? a)
  #=> true
  ({ a = 1; b = 2; } ? c)
  #=> false

  # // 연산자는 두 집합을 병합합니다.
  ({ a = 1; } // { b = 2; })
  #=> { a = 1; b = 2; }

  # 오른쪽 값이 왼쪽 값을 재정의합니다.
  ({ a = 1; b = 2; } // { a = 3; c = 4; })
  #=> { a = 3; b = 2; c = 4; }

  # rec 키워드는 "재귀 집합"을 나타내며,
  # 속성이 서로 참조할 수 있습니다.
  (let a = 1; in     { a = 2; b = a; }.b)
  #=> 1
  (let a = 1; in rec { a = 2; b = a; }.b)
  #=> 2

  # 중첩된 집합은 부분적으로 정의할 수 있습니다.
  {
    a.b   = 1;
    a.c.d = 2;
    a.c.e = 3;
  }.a.c
  #=> { d = 2; e = 3; }

  # 집합은 불변이므로 속성을 재정의할 수 없습니다:
  {
    a = { b = 1; };
    a.b = 2;
  }
  #=> attribute 'a.b' at (string):3:5 already defined at (string):2:11

  # 그러나 속성 자체가 직접 할당되었더라도
  # 속성의 집합 멤버는 부분적으로 정의할 수 있습니다.
  {
    a = { b = 1; };
    a.c = 2;
  }
  #=> { a = { b = 1; c = 2; }; }


  #  With
  #=========================================

  # `with` 블록의 본문은 집합의 매핑이
  # 변수에 바인딩된 상태로 평가됩니다.
  (with { a = 1; b = 2; };
    a + b)
  # => 3

  # 내부 바인딩은 외부 바인딩을 가립니다.
  (with { a = 1; b = 2; };
    (with { a = 5; };
      a + b))
  #=> 7

  # 이 튜토리얼의 첫 줄은 "with builtins;"으로 시작합니다.
  # builtins는 모든 내장 함수(length, head, tail, filter 등)를
  # 포함하는 집합이기 때문입니다. 이렇게 하면 예를 들어
  # "builtins.length" 대신 "length"만 작성할 수 있습니다.


  #  집합 패턴
  #=========================================

  # 집합은 여러 값을 함수에 전달해야 할 때 유용합니다.
  (args: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  # 이것은 집합 패턴을 사용하여 더 명확하게 작성할 수 있습니다.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; }
  #=> "a-b"

  # 기본적으로 패턴은 추가 키가 포함된 집합에서 실패합니다.
  ({x, y}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> error: anonymous function called with unexpected argument ‘z’

  # ", ..."를 추가하면 추가 키를 무시할 수 있습니다.
  ({x, y, ...}: x + "-" + y) { x = "a"; y = "b"; z = "c"; }
  #=> "a-b"

  # 전체 집합은 `@`를 사용하여 변수에 바인딩할 수 있습니다.
  (args@{x, y}: args.x + "-" + args.y) { x = "a"; y = "b"; }
  #=> "a-b"

  #  오류
  #=========================================

  # `throw`는 오류 메시지와 함께 평가를 중단시킵니다.
  (2 + (throw "foo"))
  #=> error: foo

  # `tryEval`은 throw된 오류를 잡습니다.
  (tryEval 42)
  #=> { success = true; value = 42; }
  (tryEval (2 + (throw "foo")))
  #=> { success = false; value = false; }

  # `abort`는 throw와 같지만 치명적입니다. 잡을 수 없습니다.
  (tryEval (abort "foo"))
  #=> error: evaluation aborted with the following error message: ‘foo’

  # `assert`는 true이면 주어진 값으로 평가되고,
  # 그렇지 않으면 잡을 수 있는 예외를 throw합니다.
  (assert 1 < 2; 42)
  #=> 42
  (assert 1 > 2; 42)
  #=> error: assertion failed at (string):1:1
  (tryEval (assert 1 > 2; 42))
  #=> { success = false; value = false; }


  #  불순물
  #=========================================

  # 빌드의 반복성은 Nix 패키지 관리자에게 중요하기 때문에,
  # Nix 패키지를 설명하는 데 사용되는 Nix 언어에서는
  # 함수형 순수성이 강조됩니다. 그러나 몇 가지 불순물이 있습니다.

  # 환경 변수를 참조할 수 있습니다.
  (getEnv "HOME")
  #=> "/home/alice"

  # trace 함수는 디버깅에 사용됩니다. 첫 번째 인수를
  # stderr에 출력하고 두 번째 인수로 평가됩니다.
  (trace 1 2)
  #=> trace: 1
  #=> 2

  # Nix 저장소에 파일을 쓸 수 있습니다. 불순하지만,
  # 파일 이름이 내용의 해시에서 파생되므로
  # 상당히 안전합니다. 어디서든 파일을 읽을 수 있습니다. 이 예에서는
  # 저장소에 파일을 쓰고 다시 읽습니다.
  (let filename = toFile "foo.txt" "hello!"; in
    [filename (readFile filename)])
  #=> [ "/nix/store/ayh05aay2anx135prqp0cy34h891247x-foo.txt" "hello!" ]

  # Nix 저장소에 파일을 다운로드할 수도 있습니다.
  (fetchurl "https://example.com/package-1.2.3.tgz")
  #=> "/nix/store/2drvlh8r57f19s9il42zg89rdr33m2rm-package-1.2.3.tgz"

]
```

### 더 읽을거리

* [Nix 매뉴얼 - Nix 표현 언어](https://nixos.org/nix/manual/#ch-expression-language)
* [James Fisher - 예제로 배우는 Nix - 1부: Nix 표현 언어](https://medium.com/@MrJamesFisher/nix-by-example-a0063a1a4c55)
* [Susan Potter - Nix 요리책 - 예제로 배우는 Nix](https://ops.functionalalgebra.com/nix-by-example/)
* [Zero to Nix - Nix 튜토리얼](https://zero-to-nix.com/)
* [Rommel Martinez - Nix 제품군에 대한 부드러운 소개](https://web.archive.org/web/20210121042658/https://ebzzry.io/en/nix/#nix)
