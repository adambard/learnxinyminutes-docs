---
name: Jsonnet
filename: learnjsonnet.jsonnet
contributors:
  - ["Huan Wang", "https://github.com/fredwangwang"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Jsonnet은 JSON을 위한 강력한 템플릿 언어입니다. 모든 유효한 JSON 문서는 유효한 Jsonnet 개체입니다. 대화형 데모/튜토리얼을 보려면 [여기](https://jsonnet.org/learning/tutorial.html)를 클릭하십시오.

```jsonnet
// 한 줄 주석

/*
    여러 줄 주석
*/

# 파이썬 스타일 주석도 마찬가지입니다.

# 변수를 정의합니다.
# 변수는 사용되지 않으면 생성된 JSON에 영향을 주지 않습니다.
local num1 = 1;
local num2 = 1 + 1;
local num3 = 5 - 2;
local num4 = 9 % 5;
local num5 = 10 / 2.0;
# jsonnet은 지연 언어이므로 변수가 사용되지 않으면 평가되지 않습니다.
local num_runtime_error = 1 / 0;

# 필드는 따옴표 없는 유효한 식별자입니다.
local obj1 = { a: 'letter a', B: 'letter B' };

local arr1 = ['a', 'b', 'c'];

# 문자열 리터럴은 " 또는 '를 사용합니다.
local str1 = 'a' + 'B';
# ||| 사이의 여러 줄 텍스트 리터럴
# 각 줄은 공백으로 시작해야 합니다.
local str_multiline = |||
  this is a
  multiline string
|||;
# %를 통해 Python 호환 문자열 서식 지정을 사용할 수 있습니다.
# |||와 결합하면 텍스트 파일 템플릿에 사용할 수 있습니다.
local str_templating = |||
  %(f1)0.3f
||| % { f1: 1.2345678 };
assert str_templating == '1.235\n';

# if b then e else e. else 분기는 선택 사항이며 기본값은 null입니다.
local var1 = if 3 < 2 then "YES";
assert var1 == null;

local obj2 = {
  # 개체 내부에 정의된 변수는 ','로 끝납니다.
  local var_in_obj = 0,

  local vowels = ['a', 'e', 'i', 'o', 'u'],
  local numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],

  # [num]을 사용하여 배열 요소 조회
  first_vowel: vowels[0],
  # Python과 같이 배열을 슬라이스할 수도 있습니다.
  even_numbers: numbers[1::2],

  # 파이썬 스타일 목록 및 개체 이해도 지원됩니다.
  double_numbers: [x * 2 for x in numbers],
  even_numbers_map: {
      # 필드 이름의 [ ] 구문은 필드 이름을 동적으로 계산하기 위한 것입니다.
      [x + '_is_even']: true for x in numbers if x % 2 == 0
  },

  nested: {
    nested_field1: 'some-value',
    # self는 현재 개체를 참조합니다.
    # ["field-name"] 또는 .field-name을 사용하여 필드를 조회할 수 있습니다.
    nested_field2: self.nested_field1,
    nested_field3: self.nested_field1,
    # $는 가장 바깥쪽 개체를 참조합니다.
    nested_field4: $.first_vowel,

    assert self.nested_field1 == self.nested_field2,
    assert self.nested_field1 == self.nested_field3,
  },

  special_field: 'EVERYTHING FEELS BAD',
};

local obj3 = {
  local var_in_obj = 1.234,
  local var_in_obj2 = { a: { b: 'c' } },

  concat_array: [1, 2, 3] + [4],
  # 문자열은 +로 연결할 수 있으며, 필요한 경우 한 피연산자를 암시적으로 문자열로 변환합니다.
  concat_string: '123' + 4,

  # ==는 깊은 동등성을 테스트합니다.
  equals: { a: { b: 'c', d: {} } } == var_in_obj2,

  special_field: 'this feels good',
};

# 개체는 +로 병합할 수 있으며, 오른쪽이 필드 충돌에서 이깁니다.
local obj4 = obj2 + obj3;
assert obj4.special_field == 'this feels good';

# 함수를 정의합니다.
# 함수에는 위치 매개변수, 명명된 매개변수 및 기본 인수가 있습니다.
local my_function(x, y, z=1) = x + y - z;
local num6 = my_function(7, 8, 9);
local num7 = my_function(8, z=10, y=9);
local num8 = my_function(4, 5);
# 인라인 익명 함수
local num9 = (function(x) x * x)(3);

local obj5 = {
  # 메서드를 정의합니다.
  # ::로 정의된 필드는 숨겨져 생성된 JSON에 나타나지 않습니다.
  # 함수는 직렬화할 수 없으므로 숨겨야 합니다.
  # 개체가 생성된 JSON에서 사용되는 경우.
  is_odd(x):: x % 2 == 1,
};
assert obj5 == {};

# jsonnet 문서는 개체, 목록, 숫자 또는 문자열 리터럴과 같이 무언가로 평가되어야 합니다.
"FIN"
```

## 추가 자료
이 예제에서 다루지 않은 몇 가지 중요하지만 중요한 개념이 있습니다. 다음을 포함합니다:

- 명령줄에서 변수 전달: [전체 구성 매개변수화](https://jsonnet.org/learning/tutorial.html#parameterize-entire-config)
- 다른 jsonnet 라이브러리/파일 가져오기: [가져오기](https://jsonnet.org/learning/tutorial.html#imports)
- Jsonnet의 OOP 측면에 대한 심층 예제: [객체 지향](https://jsonnet.org/learning/tutorial.html#Object-Orientation)
- 유용한 표준 라이브러리: [Stdlib](https://jsonnet.org/ref/stdlib.html)