---
name: HOCON
filename: learnhocon.conf
contributors:
- [TehBrian, 'https://tehbrian.xyz']
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

인간 최적화 구성 개체 표기법(HOCON)은 사람이 쉽게 편집할 수 있도록 설계된 구성 및 데이터 직렬화 형식입니다.

JSON의 상위 집합이므로 모든 유효한 JSON은 유효한 HOCON이지만, 덜 독선적이라는 점에서 다릅니다. 유연하면서도 결정 가능한 구문 덕분에 결과 구성 파일은 다른 형식보다 덜 시끄럽습니다.

또한 주석을 지원하므로 JSON보다 사용자 대면 구성에 더 적합합니다.

```
// // 또는 # 뒤의 모든 것은 주석입니다. 이것은 주석입니다.
# 이것도 주석입니다.

##################
### 기본 사항 ###
##################

# HOCON의 모든 것은 키, 값 또는 구분 기호입니다.
# : 및 =는 구분 기호입니다. 키와 값을 구분합니다.
key: value
another_key = another_value

# 양쪽에 공백이 있거나 없는 구분 기호를 사용할 수 있습니다.
colon1:value
colon2: value
colon3 : value
equals1=value
equals2= value
equals3 = value
# 보시다시피 HOCON은 매우 제한적이지 않은 구문을 가지고 있습니다.

# HOCON은 키 모양에 대해 독선적이지 않습니다.
THIS_IS_A_VALID_KEY: value
this-is-also-a-valid-key: value
keys can have spaces: value
or even numbers like 12345: value
"you can even quote keys if you'd like!": value

# 키는 대소문자를 구분합니다.
unique: value 1
UnIqUe: value 3
UNIQUE: value 2

# 키, 구분 기호, 값이 뒤따르는 것을 필드라고 합니다.
this_entire_line_is: a field

###################
### 값 유형 ###
###################

# 값은 문자열, 숫자, 개체, 배열, 부울, null 유형일 수 있습니다.
# 단순 값은 배열 및 개체를 제외한 모든 유형의 값입니다.

## 단순 값 ##

quoted_string: "I like quoting my strings."
unquoted_string: I don't like quoting my strings.
# 따옴표 없는 문자열에서 사용할 수 없는 특수 문자는 다음과 같습니다:
# $ " { } [ ] : = , + # ` ^ ? ! @ * &
# 따옴표 없는 문자열은 어떤 종류의 이스케이프도 지원하지 않습니다.
# 문자열에서 이러한 특수 문자 중 하나를 사용하려면 따옴표로 묶인 문자열을 사용하십시오.
multiline_string: """This entire thing is a string!
One giant, multiline string.
You can put 'single' and "double" quotes without it being invalid."""

number: 123
negative: -123
fraction: 3.1415926536
scientific_notation: 1.2e6 // 1.2 * 10^6

boolean: true # 또는 false
empty: null

## 배열 ##

# 배열은 값 목록을 보유합니다.

# 배열의 값은 쉼표로 구분할 수 있습니다..
array: [ 1, 2, 3, 4, 5 ]
fibonacci: [1,1,2,3,5,8,13]
multiples_of_5: [5, 10, 15, 20,] # 후행 쉼표를 주목하십시오. 허용됩니다.
# 또는 줄 바꿈..
friends: [
  "Brian"
  "Sophie"
  "Maya"
  "Sabina"
]
# 또는 둘 다!
ingredients: [
  "Egg",
  "Sugar",
  "Oil",
  "Flour", # 후행 쉼표. 여기에서도 허용됩니다!
]
# 다시 말하지만, HOCON은 매우 자유로운 구문을 가지고 있습니다. 선호하는 스타일을 사용하십시오.

no newline before or after bracket: ["This"
  "is"
  "an"
  "array!"]

# 배열은 다른 배열을 보유할 수 있습니다.
array in array: [ [1, 2, 3], ["a", "b", "c"] ]
array in array in array: [ [ [1, 2], [8, 9] ], [ ["a", "b" ], ["y", "z"] ] ]

## 개체 ##

# 개체는 필드를 보유합니다.

# 배열과 마찬가지로 개체의 필드는 쉼표로 구분할 수 있습니다..
object: { key: value, another_key: another_value }
server_connection: {ip: "127.0.0.1", port: 80}
first: {letter: a, number: 1,} # 후행 쉼표.
# 또는 줄 바꿈..
power_grid: {
  max_capacity: 15000
  current_power: 1200
}
# 또는 둘 다!
food_colors: {
  carrot: orange,
  pear: green,
  apple: red,
  plum: purple,
  banana: yellow, # 후행 쉼표. 이 성가신 것들은 어디에나 나타납니다!
}

# 배열은 개체를 보유할 수 있습니다.
coworkers: [
  {
    name: Jeff
    age: 27
  },
  {
    name: Henry
    age: 35
  },
  {
    name: Timmy
    age: 12
  }
]

# 키 뒤에 {가 오는 경우 필드 구분 기호를 생략할 수 있습니다.
no_separator {
  key: value
  speed_of_light: very fast
  ten: 10

  # 개체는 다른 개체를 보유할 수 있습니다.
  another_object {
    twenty: 20
    speed_of_sound: also pretty fast
  }
}

# 사실, 모든 HOCON 문서는 실제로 개체일 뿐입니다.
# 해당 개체를 루트 개체라고 합니다. 다른 개체와의 유일한 차이점은 문서의 맨 위와 맨 아래에 있는 중괄호를 생략할 수 있다는 것입니다.

# 이것은 HOCON 문서를 일반 개체와 동일한 방식으로 서식 지정할 수 있음을 의미하며, 줄 바꿈 대신 쉼표로 필드를 구분하는 것을 포함합니다.

# 또한, HOCON 문서 전체가 개체일 수 있고 일반적으로 개체이지만 배열일 수도 있습니다. 배열인 경우 문서의 맨 위와 맨 아래에 있는 여는 대괄호와 닫는 대괄호를 명시적으로 작성해야 합니다.

######################
### 중복 키 ###
######################

is_happy: false
# 중복 키가 있는 경우 새 값이 이전 값을 재정의합니다.
is_happy: true
online_users: [Jacob, Mike]
# 배열도 마찬가지입니다.
online_users: [Jacob, Mike, Henry]

# 개체의 경우 약간 다릅니다.
my_car: {
  color: blue
  speed: 9001
  passengers: null

  engine: {
    running: true
    temperature: 137
  }
}
# 중복 키가 있고 두 값이 모두 개체인 경우 개체가 병합됩니다.
my_car: {
  // 이러한 필드는 이전 개체에 추가됩니다.
  nickname: "My Favorite Car"
  type: 2-door sedan

  // 이 중복 키의 값이 개체가 아니므로 이전 값을 단순히 재정의합니다.
  speed: 60
  // 배열도 마찬가지입니다. 병합되지 않고 재정의됩니다.
  passengers: ["Nate", "Ty"]

  // 이 개체는 다른 개체와 재귀적으로 병합됩니다.
  engine: {
    // 이 두 필드는 이전 개체에 추가됩니다.
    type: gas
    oil_level: 10
    // 이 필드는 이전 값을 재정의합니다.
    temperature: 179
  }
}

# 개체 병합은 한 번에 두 개씩 수행됩니다. 즉, 처음 두 개체가 하나로 병합된 다음 해당 개체가 다음 개체와 병합되는 식입니다.

# 이 때문에 개체 값이 있는 필드를 개체가 아닌 값으로 설정한 다음 다시 개체 값으로 설정하면 새 개체가 이전 값을 완전히 재정의합니다.

// Null, 개체가 아닌 값은 개체를 재정의합니다.
my_car: null

// 그런 다음 이 개체는 null을 재정의합니다.
my_car: {
  nickname: "My New Car"
  type: 4-door minivan
  color: gray
  speed: 90
  passengers: ["Ayden", "Liz"]
}

###########################
### 값 연결 ###
###########################

## 단순 값 연결 ##

# 공백으로 구분된 단순 값(배열 및 개체를 제외한 모든 값 유형)은 단일 문자열로 연결됩니다. 값 사이의 공백은 유지됩니다.
number_concat: 1 2 3 12.5 -3 2e5 // "1 2 3 12.5 -3 2e5"
boolean_concat: true false true // "true false true"
null_concat: null null null // "null null null"
mixed_concat: 1 true null // "1 true null"

# 문자열 값 연결은 따옴표로 묶인 문자열이 나타날 수 있는 모든 곳에 나타날 수 있습니다.
number_concat_in_array: [1 2, 3 4, 5 6] // ["1 2", "3 4", "5 6"]

# 사실, 따옴표 없는 문자열은 실제로 문자열 값 연결일 뿐입니다.
unquoted_string_concat: his name is jeff // "his name is jeff"

# 더 나아가, 따옴표 없는 문자열인 키조차도 실제로 문자열 값 연결일 뿐입니다.
this is a key: value // 키는: "this is a key"
# 다음 필드는 위 필드와 동일합니다.
"this is a key": value

# 따옴표로 묶인 문자열도 연결할 수 있습니다.
# 이것은 나중에 대체에 대해 다룰 때 유용합니다.
quoted_string_concat: "her"" name" "is ""jenna" // "her name is jenna"
# 값 사이의 공백(또는 공백 없음)이 유지됨을 주목하십시오.

## 배열 연결 ##

# 공백으로 구분된 배열은 단일 배열로 병합됩니다.
array_concat: [1, 2, 3] [4, 5, 6] // [1, 2, 3, 4, 5, 6]

# 배열은 배열이 아닌 값과 연결할 수 없습니다.
//array_concat: true [false] // 오류!
//array_concat: 1 [2] // 오류!

## 개체 연결 ##

# 공백으로 구분된 개체는 단일 개체로 병합됩니다.
# 병합 기능은 중복 키 개체 병합과 동일합니다.
lamp: {on: true} {color: tan} // {on: true, color: tan}

# 배열과 마찬가지로 개체는 개체가 아닌 값과 연결할 수 없습니다.
//object_concat: true {on: false} // 오류!
//object_concat: 1 {number: 2} // 오류!

########################
### 경로 표현식 ###
########################

# 경로 표현식은 개체 그래프를 통해 경로를 작성하는 데 사용됩니다. 특정 필드로 개체를 탐색하는 것으로 생각하십시오.
# 통과할 각 개체를 요소라고 하며 각 요소는 마침표로 구분됩니다.

country: {
  city: {
    neighborhood: {
      house: {
        name: "My House"
        address: 123 Example Dr.
      }
    }
  }
}
# 주소 경로는 다음과 같이 작성할 수 있습니다:
# country.city.neighborhood.house.address
# 국가, 도시, 이웃, 집 및 주소는 모두 요소입니다.

# 경로 표현식은 두 곳에서 사용됩니다: 대체(잠시 후에 다룰 예정) 및 키. 맞습니다: 키는 경로 표현식일 수 있습니다.
foo: {
  bar: {
    baz: {
      number: 12
    }
  }
}
# 각 개체를 지루하게 지정하는 대신 경로 표현식을 사용할 수 있습니다.
# 다음 필드는 동일한 개체를 나타냅니다.
foo.bar.baz.number: 12

# 경로 표현식으로 지정된 필드 및 개체는 일반적으로 개체가 병합되는 것과 동일한 방식으로 병합됩니다.
foo.bar.baz.bool: true
// foo 개체의 값은: foo { bar { baz { number: 12, bool: true } } }

#####################
### 대체 ###
#####################

# 대체는 일부 경로 표현식의 특정 값을 참조합니다.
# 키나 다른 대체에 중첩된 것이 아니라 값에서만 허용됩니다.

me: {
  favorite_animal: parrots
  favorite_food: cookies
}
# 대체에는 두 가지 구문이 있습니다:
# ${path_expression} 및 ${?path_expression}.
# 후자 구문은 잠시 후에 다룰 예정입니다.
my_fav_animal: ${me.favorite_animal}
my_fav_food: ${me.favorite_food}

# 대체는 따옴표로 묶인 문자열 내에서 구문 분석되지 않습니다. 이를 해결하려면 따옴표 없는 문자열 또는 값 연결을 사용하십시오.
animal_announcement: My favorite animal is ${my_fav_animal}
// "My favorite animal is parrots"
food_announcement: "My favorite food is "${my_fav_food}"!"
// "My favorite food is cookies!"

# 대체는 문서에서 마지막으로 구문 분석됩니다. 이 때문에 아직 정의되지 않은 키를 참조할 수 있습니다.
color_announcement: "My favorite color is" ${my_fav_color}"!
// "My favorite color is blue!"
my_fav_color: blue

# 대체가 마지막으로 구문 분석되는 또 다른 효과는 대체가 항상 문서 전체에서 할당된 최신, 즉 마지막 값을 사용한다는 것입니다.
color: green
their_favorite_color: ${color} // orange
color: orange

# 이것은 병합된 개체를 포함합니다.
random_object: {
  number: 12
}
the_number: ${random_object.number} // 15
random_object: {
  number: 15
}

###############################
### 정의되지 않은 대체 ###
###############################

# 정의되지 않은 경로 표현식, 즉 정의된 값을 가리키지 않는 경로 표현식이 있는 ${path_expression} 구문을 사용하는 대체는 유효하지 않으므로 오류가 발생합니다.
//${does.not.exist} // 오류!

# 그러나 ${?path_expression} 구문을 사용하는 정의되지 않은 대체는 값에 따라 다른 동작을 합니다.
request: {
  # 필드의 값인 경우 필드가 생성되지 않습니다.
  response: ${?does.not.exist} // 이 필드는 존재하지 않습니다.
  type: HTTP
}

request: {
  # 또한 이전 값을 재정의했을 경우 이전 값은 변경되지 않습니다.
  type: ${?does.not.exist} // request.type은 여전히 HTTP입니다.
}

# 배열의 값인 경우 단순히 추가되지 않습니다.
values: [ 172, "Brian", ${?does.not.exist}, null, true, ]
// [ 172, "Brian", null, true ]

# 단순 값 연결의 일부인 경우 빈 문자열 역할을 합니다.
final_string: "String One"${?does.not.exist}"String Two"
// "String OneString Two"

# 배열 연결의 일부인 경우 빈 배열 역할을 합니다.
final_array: [ 1, 2, 3 ] ${?does.not.exist} [ 7, 8, 9 ]
// [ 1, 2, 3, 7, 8, 9 ]

# 개체 연결의 일부인 경우 빈 개체 역할을 합니다.
final_object: { a: 1 } ${?does.not.exist} { c: 3 }
// { a: 1, c: 3 }

######################################
### 자기 참조 대체 ###
######################################

# 대체는 일반적으로 "앞을 내다보고" 문서에 정의된 최종 값을 사용합니다. 그러나 이것이 주기를 생성하는 경우 대체는 뒤로만 봅니다.

# 자신을 가리키거나 결국 자신을 다시 가리키는 다른 필드를 가리키는 대체가 포함된 필드를 자기 참조 필드라고 합니다.
letters: "a b c" // "a b c"
letters: ${letters}" d" // "a b c d"
letters: ${letters}" e" // "a b c d e"

PATH: [/bin] // [/bin]
PATH: ${PATH} [/usr/bin] // [/bin, /usr/bin]
PATH: ${PATH} [/usr/local/bin] // [/bin, /usr/bin, /usr/local/bin]

x: "x" // "x"
y: ${x}"y" // "xy"
x: ${y}"z" // "xyz"

##########################
### += 필드 구분 기호 ###
##########################

# : 및 = 외에도 실제로 다른 구분 기호가 있습니다: +=
# +=로 구분된 필드는 자기 참조 배열 연결을 의미합니다.
# 본질적으로 이전에 정의된 배열에 요소를 추가합니다.

a: [1]
b: [1]
# 이 두 필드는 동일합니다.
a += 2 // [1, 2]
b: ${?b} [2] // [1, 2]

USERS: [/usr/luke] // [/usr/luke]
USERS += /usr/devon // [/usr/luke, /usr/devon]
USERS += /usr/michael // [/usr/luke, /usr/devon, /usr/michael]

# +=는 이전에 존재하는 배열에만 요소를 추가하므로 이전 값이 배열이 아니면 오류가 발생합니다.
OTHER_USERS: /usr/luke
//OTHER_USERS += /usr/devon // 오류!

# 사용된 기본 대체 구문은 ${?path}가 아닌 ${path}입니다.
# ${?} 구문을 사용하면 배열 연결에서 정의되지 않은 대체가 빈 배열 역할을 한다는 것을 기억하십시오. 이 때문에 설정되는 필드가 처음에 정의되지 않은 경우 완벽하게 허용됩니다.
//z: [] // 필요 없음
z += 3 // [3]
z += 4 // [3, 4]

NEW_USERS += /usr/sandra // [/usr/sandra]
NEW_USERS += /usr/kennedy // [/usr/sandra, /usr/kennedy]
NEW_USERS += /usr/robin // [/usr/sandra, /usr/kennedy, /usr/robin]

################
### 포함 ###
################

# 포함을 사용하면 한 HOCON 문서를 다른 문서로 "가져올" 수 있습니다.

# 포함 문은 따옴표 없는 문자열 "include"와 공백, 그리고 다음 중 하나인 리소스 이름으로 구성됩니다:
# - URL, 파일 이름 또는 Java 클래스 경로 리소스로 휴리스틱하게 해석되는 단일 따옴표로 묶인 문자열.
# - url(), file() 또는 classpath(), 괄호는 각각 URL, 파일 이름 또는 클래스 경로 리소스인 따옴표로 묶인 문자열을 둘러쌉니다.
# - required(), 괄호는 위 중 하나를 둘러쌉니다.
include "https://example.com/config.conf"
include "/foo/bar/config.conf"
include "config.conf"

include url("https://example.com/config.conf")
include file("/foo/bar/config.conf")
include classpath("config.conf")

# 포함된 파일이 존재하지 않으면 자동으로 무시되고 빈 개체인 것처럼 작동합니다. 그러나 required()로 래핑된 경우 파일을 확인할 수 없으면 구문 분석이 명시적으로 오류를 발생시킵니다.
//include required("doesnt_exist.conf") // 오류!
//include required(url("https://example.com/doesnt_exist.conf")) // 오류!
//include required(file("doesnt_exist.conf")) // 오류!
//include required(classpath("doesnt_exist.conf")) // 오류!

# 포함 문으로 지정된 파일을 포함된 파일이라고 합니다.
# 포함 문이 포함된 파일을 포함하는 파일이라고 합니다.

# 파일을 포함하는 것은 포함된 파일의 루트 개체 내용으로 포함 문을 직접 교체하는 것처럼 작동합니다.

# 포함된 파일은 루트 값으로 개체를 가져야 하며 배열이 아니어야 합니다.
# 포함된 파일에 루트 값으로 배열이 있는 경우 유효하지 않으며 오류가 발생합니다.

# 다음이 user_config.conf라는 파일에 있다고 가정합니다:
username: RandomUser1337
auto_login: true
color_theme: dark
screensaver: {
  image: usr/images/screensaver.jpg
  turn_on_after: 1m
}

# 그런 다음 해당 파일을 포함합니다.
include file("user_config.conf")

# 이제 해당 파일의 값을 참조할 수 있습니다!
path_to_user_screensaver: ${screensaver.image} // "usr/images/screensaver.jpg"
greeting: "Welcome, "${username}"!" // "Welcome, RandomUser1337!"

# 중복 키는 일반적으로 재정의됩니다.
status: "Auto Login: "${auto_login} // "Auto Login: true"
auto_login: false
status: "Auto Login: "${auto_login} // "Auto Login: false"

# 개체 병합은 평소와 같습니다.
screensaver: {
  // 이것은 screensaver 개체에 추가됩니다.
  enable_during_day: false
  // 이것은 이전 값을 재정의합니다.
  turn_on_after: 30s
}

# 포함 문은 필드 대신 나타날 수 있습니다. 필드가 나타날 수 있는 모든 곳에 포함 문이 나타날 수 있습니다.

# 다음이 server_settings.conf라는 파일에 있다고 가정합니다:
max_connections: 10
url: example.com
port: 80
admin_page: {
  username: admin
  password: pass12345
}

# 그런 다음 개체 내에 중첩된 파일을 포함합니다.
websites: {
  my_epic_website: {
    include file("server_settings.conf")
  }
}

# 이제 server_settings.conf의 내용을 개체 my_epic_website에 직접 작성된 것처럼 참조할 수 있습니다.
server_port: ${websites.my_epic_website.port}

the_password: "The password is: "${websites.my_epic_website.admin_page.password}
// "The password is: pass12345"

max_conn: "Max Connections: "${websites.my_epic_website.max_connections}
// "Max Connections: 10"
```

### 추가 자료

+ [공식 HOCON 사양](https://github.com/lightbend/config/blob/master/HOCON.md)
+ [HOCON 놀이터](https://hocon-playground.tehbrian.dev)