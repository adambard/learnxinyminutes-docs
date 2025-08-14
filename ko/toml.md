---
name: TOML
filename: learntoml.toml
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

TOML은 Tom's Obvious, Minimal Language의 약자입니다. 명백한 의미 체계로 인해 읽기 쉬운 최소한의 구성 파일 형식을 목표로 설계된 데이터 직렬화 언어입니다.

YAML 및 JSON의 대안입니다. JSON보다 인간 친화적이고 YAML보다 간단한 것을 목표로 합니다. TOML은 해시 테이블에 명확하게 매핑되도록 설계되었습니다. TOML은 다양한 언어의 데이터 구조로 쉽게 구문 분석할 수 있어야 합니다.

이 문서는 [TOML v1.0.0](https://toml.io/en/v1.0.0)을 따릅니다. 향후 [변경 사항](https://github.com/toml-lang/toml/blob/main/CHANGELOG.md)은 사소하고 이전 버전과 호환될 것으로 예상됩니다.

```toml
# TOML의 주석은 이와 같습니다.

################
# 스칼라 유형 #
################

# 루트 객체(문서 전체에 걸쳐 계속됨)는 다른 언어의 사전, 해시 또는 객체와
# 동일한 맵입니다.

# 키, 등호 및 값은 같은 줄에 있어야 합니다.
# (일부 값은 여러 줄로 나눌 수 있음).
key = "value"
string = "hello"
number = 42
float = 3.14
boolean = true
dateTime = 1979-05-27T07:32:00-08:00
scientificNotation = 1e+12
"key can be quoted" = true # "와 ' 모두 괜찮습니다
"unquoted key may contain" = "letters, numbers, underscores, and dashes"
other_kêys = "are permitted by spec but most implementations don't actually permit them"

# 따옴표 없는 키는 비어 있으면 안 되지만, 빈 따옴표 키는 허용됩니다.
"" = "blank"     # 유효하지만 권장하지 않음
'' = 'blank'     # 유효하지만 권장하지 않음

##########
# 문자열 #
##########

# 모든 문자열은 유효한 UTF-8 문자만 포함해야 합니다.
# 문자를 이스케이프할 수 있으며 일부는 간결한 이스케이프 시퀀스를 가집니다.
# 예를 들어, \t는 탭을 추가합니다. 모든 것을 얻으려면 사양을 참조하십시오.
basicString = "are surrounded by quotation marks. \"I'm quotable\". Name\tJos"

multiLineString = """
는 각 측면에 세 개의 따옴표로 둘러싸여 있으며
개행을 허용합니다."""

literalString = '는 작은따옴표로 둘러싸여 있습니다. 이스케이프는 허용되지 않습니다.'

multiLineLiteralString = '''
는 각 측면에 세 개의 작은따옴표로 둘러싸여 있으며
개행을 허용합니다. 여전히 이스케이프는 없습니다.
첫 번째 개행은 원시 문자열에서 잘립니다.
   다른 모든 공백은
   보존됩니다. #!는 보존됩니까?
'''

# 이진 데이터의 경우 Base64, 다른 ASCII 또는 UTF8
# 인코딩을 사용하는 것이 좋습니다. 해당 인코딩의 처리는
# 응용 프로그램에 따라 다릅니다.

###########
# 정수 #
###########

## 정수는 +, - 또는 아무것도 없이 시작할 수 있습니다.
## 선행 0은 허용되지 않습니다.
## 16진수, 8진수 및 2진수 형식이 허용됩니다.
## 일련의 숫자로 표현할 수 없는 값은 허용되지 않습니다.
int1 = +42
int2 = 0
int3 = -21
int4 = 0xdeadbeef
int5 = 0o755
int6 = 0b11011100
integerRange = 64

## 가독성을 높이기 위해 밑줄을 사용할 수 있습니다. 각
## 밑줄은 적어도 하나의 숫자로 둘러싸여 있어야 합니다.
int4 = 5_349_221
int5 = 1_2_3_4_5     # 유효하지만 권장하지 않음

#########
# 부동 소수점 #
#########

# 부동 소수점은 정수 뒤에 소수 및/또는 지수 부분이 옵니다.
flt1 = 3.1415
flt2 = -5e6
flt3 = 6.626E-34

###########
# 부울 #
###########

bool1 = true
bool2 = false
boolMustBeLowercase = true

############
# 날짜/시간 #
############

date1 = 1979-05-27T07:32:00Z # UTC 시간, RFC 3339/ISO 8601 사양 따름
date2 = 1979-05-26T15:32:00+08:00 # RFC 3339/ISO 8601 오프셋 포함
date3 = 1979-05-27T07:32:00 # 오프셋 없음
date4 = 1979-05-27 # 오프셋 또는 시간 없음

####################
# 컬렉션 유형 #
####################

#########
# 배열 #
#########

array1 = [ 1, 2, 3 ]
array2 = [ "쉼표", "는", "구분 기호" ]
array3 = [ "다른", "유형을", "혼합하지 마십시오" ]
array4 = [ [ 1.2, 2.4 ], ["모든", '문자열은', """같은""", '''유형입니다'''] ]
array5 = [
  "공백", "은", "무시됩니다"
]

#########
# 테이블 #
#########

# 테이블(또는 해시 테이블 또는 사전)은 키/값 쌍의 컬렉션입니다.
# 한 줄에 대괄호로 표시됩니다.
# 빈 테이블은 허용되며 키/값 쌍이 없습니다.
[table]

# 그 아래, 그리고 다음 테이블이나 EOF까지는 해당 테이블의 키/값입니다.
# 테이블 내의 키/값 쌍은 특정 순서로 보장되지 않습니다.
[table-1]
key1 = "some string"
key2 = 123

[table-2]
key1 = "another string"
key2 = 456

# 점은 중첩 테이블을 나타내는 데 사용되므로 따옴표 없는 키에는 점이 금지됩니다.
# 각 점으로 구분된 부분의 명명 규칙은 키와 동일합니다.
[dog."tater.man"]
type = "pug"

# JSON 세계에서는 다음과 같은 구조를 제공합니다:
# { "dog": { "tater.man": { "type": "pug" } } }

# 점으로 구분된 부분 주위의 공백은 무시되지만, 모범 사례는
# 불필요한 공백을 사용하지 않는 것입니다.
[a.b.c]            # 이것이 모범 사례입니다
[ d.e.f ]          # [d.e.f]와 동일
[ j . "ʞ" . 'l' ]  # [j."ʞ".'l']와 동일

# 원하지 않으면 모든 상위 테이블을 지정할 필요가 없습니다. TOML은
# 당신을 위해 그것을 하는 방법을 알고 있습니다.
# [x] 당신은
# [x.y] 이것들을
# [x.y.z] 필요하지 않습니다
[x.y.z.w] # 이것이 작동하도록

# 상위 테이블이 직접 정의되지 않았고 특정 키를 정의하지 않은 한,
# 여전히 쓸 수 있습니다.
[a.b]
c = 1

[a]
d = 2

# JSON에서 다음을 생성합니다:
# { "a": {"b": {"c": 1}, "d": 2 } }

# 키나 테이블을 두 번 이상 정의할 수 없습니다. 그렇게 하는 것은 유효하지 않습니다.

# 이것을 하지 마십시오
[a]
b = 1

[a]
c = 2

# 이것도 하지 마십시오
[a]
b = 1

[a.b]
c = 2

# 모든 테이블 이름은 비어 있으면 안 됩니다.
[]     # 유효하지 않음
[a.]   # 유효하지 않음
[a..b] # 유효하지 않음
[.b]   # 유효하지 않음
[.]    # 유효하지 않음

################
# 인라인 테이블 #
################

inlineTables = { areEnclosedWith = "{ and }", a = { b = { c = { d = 1 } } } }
point = { x = 1, y = 2 }
usingMultiple = {
  lines = "discouraged!",
  instead = "use normal TOML tables",
}

###################
# 테이블 배열 #
###################

# 테이블 배열은 이중 대괄호로 테이블 이름을 사용하여 표현할 수 있습니다.
# 동일한 이중 대괄호 이름의 각 테이블은 배열의 항목이 됩니다.
# 테이블은 마주치는 순서대로 삽입됩니다.

[[products]]
name = "array of table"
sku = 738594937
emptyTableAreAllowed = true

[[products]]

[[products]]
name = "Nail"
sku = 284758393
color = "gray"
```

JSON의 동등한 것은 다음과 같습니다:

```json
{
  "products": [
    {
      "name": "array of table",
      "sku": 7385594937,
      "emptyTableAreAllowed": true
    },
    {},
    {
      "name": "Nail",
      "sku": 284758393,
      "color": "gray"
    }
  ]
}
```

```toml
# 중첩된 테이블 배열도 만들 수 있습니다. 각 이중 대괄호로 묶인
# 하위 테이블은 그 위의 가장 가까운 테이블 요소에 속합니다.

[[fruit]]
  name = "apple" # 나는 과일 테이블/맵의 속성입니다

  [fruit.geometry]
    shape = "round"
    note = "나는 기하학 테이블/맵의 속성입니다"

  [[fruit.color]]
    name = "red"
    note = "나는 사과 과일의 테이블/맵에 있는 배열 항목입니다"

  [[fruit.color]]
    name = "green"
    note = "나는 빨간색과 같은 배열에 있습니다"

[[fruit]]
  name = "banana"

  [[fruit.color]]
    name = "yellow"
    note = "나는 바나나 과일의 테이블/맵에 있는 배열 항목입니다"
```

JSON의 동등한 것은 다음과 같습니다:

```
{
  "fruit": [
    {
      "name": "apple",
      "geometry": { "shape": "round", "note": "..."},
      "color": [
        { "name": "red", "note": "..." },
        { "name": "green", "note": "..." }
      ]
    },
    {
      "name": "banana",
      "color": [
        { "name": "yellow", "note": "..." }
      ]
    }
  ]
}
```

### 더 많은 자료

+ [TOML 공식 저장소](https://github.com/toml-lang/toml)
