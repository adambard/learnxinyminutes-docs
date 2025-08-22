---
name: CUE
filename: learncue.cue
contributors:
    - ["Daniel Cox", "https://github.com/danielpcox"]
    - ["Coleman McFarland", "https://github.com/dontlaugh"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

CUE는 표현력이 풍부한(하지만 튜링 완전하지는 않은) JSON 상위 집합으로, JSON 또는 YAML로 내보낼 수 있습니다. 선택적 유형과 대규모 구성 집합 작업에 대한 기타 여러 편의 기능을 지원합니다. 통합 엔진은 논리 프로그래밍에 뿌리를 두고 있으며, 따라서 현대 구성 관리 문제에 대한 즉각적인 솔루션을 제공합니다.

CUE가 JSON으로 내보내질 때, 처리된 모든 파일의 값은 하나의 거대한 객체로 통합됩니다. 다음 두 파일을 고려하십시오:

```yaml
//name.cue
name: "Daniel"
```

```yaml
//disposition.cue
disposition: "oblivious"
```

이제 JSON으로 통합하고 내보낼 수 있습니다:

```bash
% cue export name.cue disposition.cue
{
    "name": "Daniel",
    "disposition": "oblivious"
}
```

또는 YAML:

```bash
% cue export --out yaml name.cue disposition.cue
name: Daniel
disposition: oblivious
```

C 스타일 주석은 출력에 포함되지 않습니다. 또한 CUE 구문의 키는 따옴표가 필요하지 않습니다. 일부 특수 문자는 따옴표가 필요합니다:

```yaml
works_fine: true
"needs-quotes": true
```

통합은 파일 간에만 통합되는 것이 아니라 모든 유형과 값의 *전역 병합*입니다. 다음은 *유형*이 다르기 때문에 실패합니다.

```yaml
//string_value.cue
foo: "baz"
```

```yaml
//integer_value.cue
foo: 100
```

```bash
% cue export string_value.cue integer_value.cue
foo: "baz"와 100의 충돌 값(문자열과 int 유형 불일치):
    integer_value.cue:1:6
    string_value.cue:1:6
```

하지만 정수를 인용하더라도 *값*이 충돌하고 모든 것을 최상위 객체로 통합할 방법이 없기 때문에 여전히 실패합니다.

```yaml
//string_value.cue
foo: "baz"
```

```yaml
//integer_value.cue
foo: "100"  // 이제 문자열
```

```bash
% cue export string_value.cue integer_value.cue
foo: "100"과 "baz"의 충돌 값:
    integer_value.cue:1:6
    string_value.cue:1:6
```

CUE의 유형은 값입니다. 통합 엔진이 특정 동작을 가진다고 아는 특수 값입니다. 통합 중에 값은 지정된 유형과 일치해야 하며, 구체적인 값이 필요한 경우 유형만 있으면 오류가 발생합니다. 따라서 다음은 괜찮습니다:

```yaml
street: "1 Infinite Loop"
street: string
```

`cue export`는 YAML 또는 JSON을 생성하는 반면, `cue eval`은 CUE를 생성합니다. 이는 YAML 또는 JSON을 CUE로 변환하거나 CUE 자체에서 통합된 출력을 검사하는 데 유용합니다. CUE에서 구체적인 값이 누락되어도 괜찮습니다(둘 다 사용 가능하고 일치하는 경우 CUE를 내보낼 때 구체적인 값을 선호하지만).

```yaml
//type-only.cue
amount: float
```

```bash
% cue eval type-only.cue
amount: float
```

그러나 내보내려면(또는 `eval`에 `-c`로 요구하도록 지시하려면) 구체적인 값이 *필요합니다*:

```bash
% cue export type-only.cue
amount: 불완전한 값 float
```

유형과 통합되는 값을 제공하면 모든 것이 잘 작동합니다.

```yaml
//concrete-value.cue
amount: 3.14
```

```bash
% cue export type-only.cue concrete-value.cue
{
    "amount": 3.14
}
```

구체적인 값을 유형과 통합하는 방법은 공통 구문을 공유하는 것보다 훨씬 강력하며, 예를 들어 JSON 스키마보다 훨씬 간결합니다. 이런 식으로 스키마, 기본값 및 데이터는 모두 CUE로 표현할 수 있습니다.

기본값은 별표를 사용하여 유형과 함께 제공될 수 있습니다:

```yaml
// default-port.cue
port: int | *8080
```

```bash
% cue eval default-port.cue
port: 8080
```

열거형 스타일 옵션(CUE의 "분리")은 `|` 구분 기호로 지정할 수 있습니다:

```yaml
//severity-enum.cue
severity: "high" | "medium" | "low"
severity: "unknown"
```

```bash
% cue eval severity-enum.cue
severity: 빈 분리에서 3개의 오류:
severity: "high"와 "unknown"의 충돌 값:
    ./severity-enum.cue:1:11
    ./severity-enum.cue:1:48
severity: "low"와 "unknown"의 충돌 값:
    ./severity-enum.cue:1:31
    ./severity-enum.cue:1:48
severity: "medium"과 "unknown"의 충돌 값:
    ./severity-enum.cue:1:20
    ./severity-enum.cue:1:48
```

구조체 분리도 가능합니다(표시되지 않았지만 예상대로 작동합니다).

CUE에는 "정의"가 있으며, 다른 언어의 변수 선언처럼 사용할 수 있습니다. 또한 구조체 유형을 정의하는 데에도 사용됩니다. 정의 유형의 구조체를 `&`를 사용하여 일부 구체적인 값에 적용할 수 있습니다. 또한 `[...#Whatever]`를 사용하여 "#Whatever 유형의 목록"이라고 말할 수 있습니다.

```yaml
// definitions.cue

#DashboardPort: 1337

configs: {
    host: "localhost"
    port: #DashboardPort
}

#Address: {
    street: string
    city: string
    zip?: int  // ?는 zip을 선택 사항으로 만듭니다.
}

some_address: #Address & {
  street: "1 Rocket Rd"
  city: "Hawthorne"
}

more_addresses: [...#Address] & [
  {street: "1600 Amphitheatre Parkway", city: "Mountain View", zip: "94043"},
  {street: "1 Hacker Way", city: "Menlo Park"}
]
```

```bash
% cue export --out yaml definitions.cue
configs:
  host: localhost
  port: 1337
some_address:
  street: 1 Rocket Rd
  city: Hawthorne
more_addresses:
  - street: 1600 Amphitheatre Parkway
    city: Mountain View
    zip: "94043"
  - street: 1 Hacker Way
    city: Menlo Park
```

CUE는 단순한 JSON 위에 제공하는 모든 설탕으로 상당한 시간을 절약할 수 있습니다. 여기서는 세 줄로 중첩된 구조를 정의, "수정" 및 유효성 검사합니다: (엔진에 `string`이 제약 조건임을 알리기 위해 `string` 주위에 사용된 `[]` 구문을 주목하십시오. 이 경우 `string`은 문자열이 아닙니다.)

```yaml
//paths.cue

// 경로-값 쌍
outer: middle1: inner: 3
outer: middle2: inner: 7

// 컬렉션-제약 조건 쌍
outer: [string]: inner: int
```

```bash
% cue export paths.cue
{
    "outer": {
        "middle1": {
            "inner": 3
        },
        "middle2": {
            "inner": 7
        }
    }
}
```

같은 맥락에서 CUE는 단일 인수의 함수와 유사한 "템플릿"을 지원합니다. 여기서 `Name`은 `container` 바로 아래의 각 문자열 키에 바인딩되는 반면, 그 아래의 구조체는 평가됩니다.

```yaml
//templates.cue

container: [Name=_]: {
    name:     Name
    replicas: uint | *1
    command:  string
}

container: sidecar: command: "envoy"

container: service: {
    command:  "fibonacci"
    replicas: 2
}
```

```bash
% cue eval templates.cue
container: {
    sidecar: {
        name:     "sidecar"
        replicas: 1
        command:  "envoy"
    }
    service: {
        name:     "service"
        command:  "fibonacci"
        replicas: 2
    }
}
```

그리고 그러한 참조에 대해 이야기하는 동안 CUE는 범위 지정 참조를 지원합니다.

```yaml
//scopes-and-references.cue
v: "top-level v"
b: v // 참조
a: {
    b: v // 최상위 v와 일치
}

let V = v
a: {
    v: "a's inner v"
    c: v // 내부 v와 일치
    d: V // a.v에 의해 가려진 최상위 v와 일치
}
av: a.v // a의 v와 일치
```

```bash
% cue eval --out yaml scopes-and-references.cue
```

```yaml
v: top-level v
b: top-level v
a:
  b: top-level v
  v: a's inner v
  c: a's inner v
  d: top-level v
av: a's inner v
```

명확성을 위해 출력에서 키 순서를 변경했습니다. 순서는 실제로 중요하지 않으며, 주어진 수준의 중복 키는 *모두* 통합됩니다.

필드를 `_`로 접두사로 붙여 숨길 수 있습니다(내보낸 필드에 `_` 접두사가 필요한 경우 필드를 인용하십시오).

```yaml
//hiddens.cue
"_foo": 2
_foo:   3
foo:    4
_#foo:  5
#foo : 6
```

```bash
% cue eval hiddens.cue
"_foo": 2
foo:    4
#foo:   6

% cue export hiddens.cue
{
    "_foo": 2,
    "foo": 4
}
```

정의와 관련하여 `eval`과 `export`의 차이점을 주목하십시오. CUE에서 정의를 숨기려면 `_`를 접두사로 붙일 수 있습니다.

값 및 필드 보간:

```yaml
//interpolation.cue

#expense: 90
#revenue: 100
message: "Your profit was $\( #revenue - #expense)"

cat: {
    type: "Cuddly"
    "is\(type)":    true
}
```

```bash
% cue export interpolation.cue
{
    "message": "Your profit was $10",
    "cat": {
        "type": "Cuddly",
        "isCuddly": true
    }
}
```

연산자, 목록 이해, 조건문, 가져오기...

```yaml
//getting-out-of-hand-now.cue
import "strings"  // 나중에 다시 다루겠습니다.

// 연산자는 좋습니다.
g: 5 / 3         // CUE는 수학을 할 수 있습니다.
h: 3 * "blah"    // 그리고 파이썬과 같은 문자열 반복
i: 3 * [1, 2, 3] // 목록도 마찬가지입니다.
j: 8 < 10        // 그리고 부울 연산도 지원합니다.

// 조건문도 좋습니다.
price: number
// 가격이 너무 높으면 정당화가 필요합니다.
if price > 100 {
    justification: string
}
price:         200
justification: "impulse buy"

// 목록 이해는 강력하고 간결합니다.
#items: [ 1, 2, 3, 4, 5, 6, 7, 8, 9]
comp: [ for x in #items if x rem 2 == 0 {x*x}]

// 그리고... 음, 이것도 할 수 있습니다.
#a: [ "Apple", "Google", "SpaceX"]
for k, v in #a {
    "\( strings.ToLower(v) )": {
        pos:     k + 1
        name:    v
        nameLen: len(v)
    }
}
```

```bash
% cue export getting-out-of-hand-now.cue
```

```json
{
    "g": 1.66666666666666666666667,
    "h": "blahblahblah",
    "i": [1, 2, 3, 1, 2, 3, 1, 2, 3],
    "j": true,
    "apple": {
        "pos": 1,
        "name": "Apple",
        "nameLen": 5
    },
    "google": {
        "pos": 2,
        "name": "Google",
        "nameLen": 6
    },
    "price": 200,
    "justification": "impulse buy",
    "comp": [
        4,
        16,
        36,
        64
    ],
    "spacex": {
        "pos": 3,
        "name": "SpaceX",
        "nameLen": 6
    }
}
```

이 시점에서 CUE는 튜링 완전하지 않을 수 있지만, 발등을 찍을 만큼 강력하므로 명확하게 유지하도록 노력해야 합니다. 조심하지 않으면 구성 작업을 *더 어렵게* 만들 수 있습니다. 적어도 주석을 활용하거나...

이를 위해 CUE는 패키지와 모듈을 지원합니다. CUE 파일은 기본적으로 독립 실행형이지만, 상단에 패키지 절을 넣으면 해당 파일이 동일한 패키지 내의 다른 파일과 통합될 수 있음을 의미합니다.

```yaml
//a.cue
package config

foo: 100
bar: int
```

```yaml
//b.cue
package config

bar: 200
```

새 디렉토리에 이 두 파일을 만들고 `cue eval`을 실행하면(인수 없음) 예상대로 통합됩니다. 현재 디렉토리에서 .cue 파일을 검색하고, 모두 동일한 패키지를 가지고 있으면 통합됩니다.

패키지는 "모듈"의 맥락에서 더 명확합니다. 모듈은 조직의 *가장 큰* 단위입니다. 기본적으로 여러 파일에 걸쳐 있는 프로젝트가 있을 때마다 모듈을 만들고 URL의 도메인 및 경로와 유사한 이름(예: `example.com/something`)으로 이름을 지정해야 합니다. 이 모듈에서 무엇이든 가져올 때, 모듈 *내부*에서 가져오더라도 이 모듈 이름이 접두사로 붙는 완전한 모듈 경로를 사용해야 합니다.

새 모듈을 다음과 같이 만들 수 있습니다:

```bash
mkdir mymodule && cd mymodule
cue mod init example.com/mymodule
```

이렇게 하면 `mymodule` 디렉토리 내에 `cue.mod/` 하위 디렉토리가 생성되며, `cue.mod/`에는 다음 파일과 하위 디렉토리가 포함됩니다:

- `module.cue`  (이 경우 `module: "example.com/mymodule"`로 모듈 이름을 정의합니다)
- pkg/
- gen/
- usr/

이에 대한 다른 관점과 내용에 대한 자세한 내용은 [cuelang.org/docs/concepts/packages/](https://cuelang.org/docs/concepts/packages/)를 참조하십시오. 여기서는 이 디렉토리의 내용에 대해 *전혀* 생각할 필요가 없다고 말하겠습니다. 단, 모듈 이름은 모듈 내의 모든 가져오기에 대한 접두사가 됩니다.

모듈 파일 계층 구조는 어디로 갈까요? 모듈의 모든 파일과 디렉토리는 `mymodule/`에 루트를 둡니다. 이 디렉토리에는 `cue.mod/`도 포함됩니다. 패키지를 가져오려면 `example.com/mymodule`을 접두사로 붙이고 `mymodule/`에 루트를 둔 상대 경로를 사용해야 합니다.

구체적으로 설명하자면, 다음을 고려하십시오:

```
mymodule
├── config
│   ├── a.cue
│   └── b.cue
├── cue.mod
│   ├── module.cue
│   ├── pkg
│   └── usr
└── main.cue
```

`cue.mod/` 및 그 아래 파일은 `cue mod init example.com/mymodule`에 의해 생성되었습니다. 그런 다음 `a.cue` 및 `b.cue`가 포함된 `config/` 하위 디렉토리를 만들었습니다. 그런 다음 모든 것을 제어하는 최상위 파일 역할을 하는 `main.cue`를 만들었습니다.

`eval`을 실행하면(인수 없음) 현재 디렉토리의 모든 .cue 파일에 패키지가 하나만 있는지 확인하고, 그렇다면 통합하여 결과를 출력합니다. 이 경우 main.cue에만 패키지 `main`이 있으므로(여기서 "main"에 특별한 것은 없지만 적절해 보였습니다) 그것이 하나입니다.

`config/a.cue` 및 `config/b.cue`는 이전 파일이지만, 이제 둘 다 상단에 `package config`가 있습니다:

```yaml
//a.cue
package config

foo: 100
bar: int
```

```yaml
//b.cue
package config

bar: 200
```

따라서 그렇습니다. `config/` 아래의 두 파일이 실제로 통합되는지 확인하려면 `a.cue`에서 `bar: int`를 `bar: string`으로 변경하고 `cue eval`을 다시 실행하여 멋진 유형 오류를 얻을 수 있습니다:

```
cue eval                                                                     2022-01-06 17:51:24
configuredBar: "string"과 200의 충돌 값(문자열과 int 유형 불일치):
    ./config/a.cue:4:6
    ./config/b.cue:3:6
    ./main.cue:5:16
```

지금은 여기까지입니다. 앞으로 더 많은 패키지 관리 기능이 추가될 것이며 `cue.mod`에 대한 설계 결정은 이를 미리 내다보고 있다는 것을 이해합니다.

마지막으로, CUE에는 강력한 기능을 가진 내장 모듈이 있습니다. 이전에 "strings"를 가져와 `strings.ToLower`를 사용했을 때 그 중 하나를 보았습니다. 완전한 모듈 이름이 없는 가져오기는 내장 기능으로 간주됩니다. 전체 목록과 각 기능에 대한 문서는 여기에서 찾을 수 있습니다: [pkg.go.dev/cuelang.org/go/pkg](https://pkg.go.dev/cuelang.org/go/pkg)

이것은 공식 문서 및 튜토리얼을 요약한 것이므로 원본 자료에 관심을 가져주십시오: [cuelang.org/docs/tutorials/](https://cuelang.org/docs/tutorials/)