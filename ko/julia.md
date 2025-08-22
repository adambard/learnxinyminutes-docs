---
name: Julia
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
    - ["Pranit Bauva", "https://github.com/pranitbauva1997"]
    - ["Daniel YC Lin", "https://github.com/dlintw"]
filename: learnjulia.jl
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Julia는 기술 컴퓨팅에 중점을 둔 새로운 동형 함수형 언어입니다. 동형 매크로, 일급 함수 및 저수준 제어의 모든 기능을 갖추고 있으면서도 Julia는 Python만큼 배우고 사용하기 쉽습니다.

이것은 Julia 버전 1.0.0을 기반으로 합니다.

```julia
# 한 줄 주석은 해시(파운드) 기호로 시작합니다.
#= 여러 줄 주석은
   '#='를 텍스트 앞에, '=#'를 텍스트 뒤에
   붙여 작성할 수 있습니다. 중첩도 가능합니다.
=#

####################################################
## 1. 기본 데이터 유형 및 연산자
####################################################

# Julia의 모든 것은 표현식입니다.

# 몇 가지 기본 숫자 유형이 있습니다.
typeof(3)       # => Int64
typeof(3.2)     # => Float64
typeof(2 + 1im) # => Complex{Int64}
typeof(2 // 3)  # => Rational{Int64}

# 모든 일반적인 중위 연산자를 사용할 수 있습니다.
1 + 1      # => 2
8 - 1      # => 7
10 * 2     # => 20
35 / 5     # => 7.0
10 / 2     # => 5.0  # 정수 나누기는 항상 Float64를 반환합니다.
div(5, 2)  # => 2    # 잘린 결과를 얻으려면 div를 사용하십시오.
5 \ 35     # => 7.0
2^2        # => 4    # 거듭제곱, 비트 XOR 아님
12 % 10    # => 2

# 괄호를 사용하여 우선 순위를 적용합니다.
(1 + 3) * 2  # => 8

# Julia(예를 들어 Python과 달리)는 정수 언더/오버플로가 있습니다.
10^19      # => -8446744073709551616
# 이를 피하려면 bigint 또는 부동 소수점을 사용하십시오.
big(10)^19 # => 10000000000000000000
1e19       # => 1.0e19
10.0^19    # => 1.0e19

# 비트 연산자
~2         # => -3 # 비트 NOT
3 & 5      # => 1  # 비트 AND
2 | 4      # => 6  # 비트 OR
xor(2, 4)  # => 6  # 비트 XOR
2 >>> 1    # => 1  # 논리적 오른쪽 시프트
2 >> 1     # => 1  # 산술적 오른쪽 시프트
2 << 1     # => 4  # 논리적/산술적 왼쪽 시프트

# bitstring 함수를 사용하여 숫자의 이진 표현을 볼 수 있습니다.
bitstring(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bitstring(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# 부울 값은 기본형입니다.
true
false

# 부울 연산자
!true   # => false
!false  # => true
1 == 1  # => true
2 == 1  # => false
1 != 1  # => false
2 != 1  # => true
1 < 10  # => true
1 > 10  # => false
2 <= 2  # => true
2 >= 2  # => true
# 비교는 Python과 같이 연결될 수 있지만 다른 많은 언어와는 다릅니다.
1 < 2 < 3  # => true
2 < 3 < 2  # => false

# 문자열은 "로 생성됩니다.
"This is a string."

# 문자 리터럴은 '로 작성됩니다.
'a'

# 문자열은 UTF8로 인코딩되므로 "π" 또는 "☃"와 같은 문자열은 단일 문자 배열과 직접적으로 동일하지 않습니다.
# ASCII 문자만 포함하는 경우에만 안전하게 인덱싱할 수 있습니다.
ascii("This is a string")[1]    # => 'T'
# => 'T': ASCII/Unicode U+0054 (카테고리 Lu: 대문자)
# 주의, Julia는 대부분의 언어와 달리 1부터 인덱싱합니다(MATLAB과 유사).
# 그렇지 않으면 문자열을 반복하는 것이 좋습니다(map, for 루프 등).

# 문자열은 사전순으로 비교할 수 있습니다:
"good" > "bye"   # => true
"good" == "good" # => true
"1 + 2 = 3" == "1 + 2 = $(1 + 2)" # => true

# $(..)는 문자열 보간에 사용할 수 있습니다:
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# 괄호 안에 모든 Julia 표현식을 넣을 수 있습니다.

# 인쇄는 쉽습니다.
println("I'm Julia. Nice to meet you!")  # => I'm Julia. Nice to meet you!

# 문자열을 서식 지정하는 또 다른 방법은 stdlib Printf의 printf 매크로입니다.
using Printf   # 이것이 모듈을 로드(또는 가져오기)하는 방법입니다.
@printf "%d is less than %f\n" 4.5 5.3   # => 5 is less than 5.300000


####################################################
## 2. 변수 및 컬렉션
####################################################

# 할당하기 전에 변수를 선언하지 않습니다.
someVar = 5  # => 5
someVar      # => 5

# 이전에 할당되지 않은 변수에 액세스하면 오류가 발생합니다.
try
    someOtherVar  # => ERROR: UndefVarError: someOtherVar not defined
catch e
    println(e)
end

# 변수 이름은 문자 또는 밑줄로 시작합니다.
# 그 후에는 문자, 숫자, 밑줄 및 느낌표를 사용할 수 있습니다.
SomeOtherVar123! = 6  # => 6

# 특정 유니코드 문자를 사용할 수도 있습니다.
# 여기서 ☃는 유니코드 '눈사람' 문자이며, 잘못 표시되면 http://emojipedia.org/%E2%98%83%EF%B8%8F를 참조하십시오.
☃ = 8  # => 8
# 이것들은 상수 π와 같은 수학적 표기법에 특히 편리합니다.
2 * π  # => 6.283185307179586

# Julia의 명명 규칙에 대한 참고 사항:
#
# * 단어 구분은 밑줄('_')로 표시할 수 있지만, 이름이 읽기 어려운 경우가 아니면 밑줄 사용을 권장하지 않습니다.
#
# * 유형 이름은 대문자로 시작하고 단어 구분은 밑줄 대신 CamelCase로 표시됩니다.
#
# * 함수 및 매크로 이름은 소문자이며 밑줄이 없습니다.
#
# * 인수를 수정하는 함수 이름은 !로 끝납니다. 이러한 함수를 때때로 변경 함수 또는 제자리 함수라고 합니다.

# 배열은 1부터 n까지의 정수로 인덱싱된 값의 시퀀스를 저장합니다:
a = Int64[] # => 0-element Array{Int64,1}

# 1차원 배열 리터럴은 쉼표로 구분된 값으로 작성할 수 있습니다.
b = [4, 5, 6] # => 3-element Array{Int64,1}: [4, 5, 6]
b = [4; 5; 6] # => 3-element Array{Int64,1}: [4, 5, 6]
b[1]    # => 4
b[end]  # => 6

# 2차원 배열은 공백으로 구분된 값과 세미콜론으로 구분된 행을 사용합니다.
matrix = [1 2; 3 4] # => 2×2 Array{Int64,2}: [1 2; 3 4]

# 특정 유형의 배열
b = Int8[4, 5, 6] # => 3-element Array{Int8,1}: [4, 5, 6]

# push! 및 append!를 사용하여 목록 끝에 항목 추가
# 규칙에 따라 !가 붙은 함수 이름은 인수를 수정합니다.
push!(a, 1)    # => [1]
push!(a, 2)    # => [1,2]
push!(a, 4)    # => [1,2,4]
push!(a, 3)    # => [1,2,4,3]
append!(a, b)  # => [1,2,4,3,4,5,6]

# pop으로 끝에서 제거
pop!(b)  # => 6
b # => [4,5]

# 다시 넣기
push!(b, 6)  # => [4,5,6]
b # => [4,5,6]

a[1]  # => 1  # Julia는 0이 아닌 1부터 인덱싱한다는 것을 기억하십시오!

# end는 마지막 인덱스에 대한 약어입니다. 모든 인덱싱 표현식에서 사용할 수 있습니다.
a[end]  # => 6

# popfirst! 및 pushfirst!도 있습니다.
popfirst!(a)  # => 1
a # => [2,4,3,4,5,6]
pushfirst!(a, 7)  # => [7,2,4,3,4,5,6]
a # => [7,2,4,3,4,5,6]

# !가 붙은 함수 이름은 인수를 수정함을 나타냅니다.
arr = [5,4,6]  # => 3-element Array{Int64,1}: [5,4,6]
sort(arr)      # => [4,5,6]
arr            # => [5,4,6]
sort!(arr)     # => [4,5,6]
arr            # => [4,5,6]

# 범위를 벗어나면 BoundsError가 발생합니다.
try
    a[0]
    # => ERROR: BoundsError: attempt to access 7-element Array{Int64,1} at
    # index [0]
    # => Stacktrace:
    # =>  [1] getindex(::Array{Int64,1}, ::Int64) at .\array.jl:731
    # =>  [2] top-level scope at none:0
    # =>  [3] ...
    # => in expression starting at ...\LearnJulia.jl:180
    a[end + 1]
    # => ERROR: BoundsError: attempt to access 7-element Array{Int64,1} at
    # index [8]
    # => Stacktrace:
    # =>  [1] getindex(::Array{Int64,1}, ::Int64) at .\array.jl:731
    # =>  [2] top-level scope at none:0
    # =>  [3] ...
    # => in expression starting at ...\LearnJulia.jl:188
catch e
    println(e)
end

# 오류 목록은 표준 라이브러리에 있더라도 발생한 줄과 파일을 나열합니다. julia 폴더 내의 share/julia 폴더에서 이러한 파일을 찾을 수 있습니다.

# 범위에서 배열을 초기화할 수 있습니다.
a = [1:5;]  # => 5-element Array{Int64,1}: [1,2,3,4,5]
a2 = [1:5]  # => 1-element Array{UnitRange{Int64},1}: [1:5]

# 슬라이스 구문으로 범위를 볼 수 있습니다.
a[1:3]    # => [1, 2, 3]
a[2:end]  # => [2, 3, 4, 5]

# splice!를 사용하여 인덱스로 배열에서 요소 제거
arr = [3,4,5]
splice!(arr, 2) # => 4
arr # => [3,5]

# append!로 목록 연결
b = [1,2,3]
append!(a, b) # => [1, 2, 3, 4, 5, 1, 2, 3]
a # => [1, 2, 3, 4, 5, 1, 2, 3]

# in으로 목록에 있는지 확인
in(1, a)  # => true

# length로 길이 검사
length(a)  # => 8

# 튜플은 불변입니다.
tup = (1, 2, 3)  # => (1,2,3)
typeof(tup) # => Tuple{Int64,Int64,Int64}
tup[1] # => 1
try
    tup[1] = 3
    # => ERROR: MethodError: no method matching
    # setindex!(::Tuple{Int64,Int64,Int64}, ::Int64, ::Int64)
catch e
    println(e)
end

# 많은 배열 함수는 튜플에서도 작동합니다.
length(tup) # => 3
tup[1:2]    # => (1,2)
in(2, tup)  # => true

# 튜플을 변수로 풀 수 있습니다.
a, b, c = (1, 2, 3)  # => (1,2,3)
a  # => 1
b  # => 2
c  # => 3

# 괄호를 생략해도 튜플이 생성됩니다.
d, e, f = 4, 5, 6  # => (4,5,6)
d  # => 4
e  # => 5
f  # => 6

# 1-요소 튜플은 포함하는 값과 구별됩니다.
(1,) == 1 # => false
(1) == 1  # => true

# 두 값을 바꾸는 것이 얼마나 쉬운지 보십시오.
e, d = d, e  # => (5,4)
d  # => 5
e  # => 4

# 사전은 매핑을 저장합니다.
emptyDict = Dict()  # => Dict{Any,Any} with 0 entries

# 리터럴을 사용하여 사전을 만들 수 있습니다.
filledDict = Dict("one" => 1, "two" => 2, "three" => 3)
# => Dict{String,Int64} with 3 entries:
# =>  "two" => 2, "one" => 1, "three" => 3

# []로 값 조회
filledDict["one"]  # => 1

# 모든 키 가져오기
keys(filledDict)
# => Base.KeySet for a Dict{String,Int64} with 3 entries. Keys:
# =>  "two", "one", "three"
# 참고 - 사전 키는 정렬되거나 삽입한 순서대로가 아닙니다.

# 모든 값 가져오기
values(filledDict)
# => Base.ValueIterator for a Dict{String,Int64} with 3 entries. Values:
# =>  2, 1, 3
# 참고 - 키 순서에 대한 위와 동일합니다.

# in, haskey를 사용하여 사전에 키가 있는지 확인
in(("one" => 1), filledDict)  # => true
in(("two" => 3), filledDict)  # => false
haskey(filledDict, "one")     # => true
haskey(filledDict, 1)         # => false

# 존재하지 않는 키를 조회하려고 하면 오류가 발생합니다.
try
    filledDict["four"]  # => ERROR: KeyError: key "four" not found
catch e
    println(e)
end

# 기본값을 제공하여 해당 오류를 피하려면 get 메서드를 사용하십시오.
# get(dictionary, key, defaultValue)
get(filledDict, "one", 4)   # => 1
get(filledDict, "four", 4)  # => 4

# 집합을 사용하여 정렬되지 않은 고유한 값의 컬렉션을 나타냅니다.
emptySet = Set()  # => Set(Any[])
# 값으로 집합 초기화
filledSet = Set([1, 2, 2, 3, 4])  # => Set([4, 2, 3, 1])

# 집합에 더 많은 값 추가
push!(filledSet, 5)  # => Set([4, 2, 3, 5, 1])

# 값이 집합에 있는지 확인
in(2, filledSet)   # => true
in(10, filledSet)  # => false

# 집합 교집합, 합집합 및 차집합에 대한 함수가 있습니다.
otherSet = Set([3, 4, 5, 6])         # => Set([4, 3, 5, 6])
intersect(filledSet, otherSet)      # => Set([4, 3, 5])
union(filledSet, otherSet)          # => Set([4, 2, 3, 5, 6, 1])
setdiff(Set([1,2,3,4]), Set([2,3,5])) # => Set([4, 1])

# `=`를 사용한 할당은 복사 없이 동일한 값에 새 레이블을 붙입니다.
a = [1, 2, 3]
b = a
# 이제 `b`와 `a`는 동일한 값을 가리키므로 하나를 변경하면 다른 하나에 영향을 미칩니다:
a[3] = 5
b[3]  # => 5

# `copy()` 함수는 배열, 사전 또는 다른 컨테이너의 얕은 복사본을 만들 수 있습니다.
a = [1, 2, 3]
c = copy(a)
a[3] = 5
c[3] # => 3

####################################################
## 3. 제어 흐름
####################################################

# 변수를 만들어 봅시다.
someVar = 5

# 다음은 if 문입니다. Julia에서는 들여쓰기가 의미가 없습니다.
if someVar > 10
    println("someVar is totally bigger than 10.")
elseif someVar < 10    # 이 elseif 절은 선택 사항입니다.
    println("someVar is smaller than 10.")
else                    # else 절도 선택 사항입니다.
    println("someVar is indeed 10.")
end
# => "some var is smaller than 10" 인쇄

# for 루프는 반복 가능한 항목을 반복합니다.
# 반복 가능한 유형에는 범위, 배열, 집합, 사전 및 AbstractString이 포함됩니다.
for animal = ["dog", "cat", "mouse"]
    println("$animal is a mammal")
    # $를 사용하여 변수 또는 표현식을 문자열에 보간할 수 있습니다.
    # 이 특별한 경우 괄호가 필요하지 않습니다: $animal과 $(animal)은 동일합니다.
end
# => dog is a mammal
# => cat is a mammal
# => mouse is a mammal

# "=" 대신 'in'을 사용할 수 있습니다.
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# => dog is a mammal
# => cat is a mammal
# => mouse is a mammal

for pair in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    from, to = pair
    println("$from is a $to")
end
# => mouse is a mammal
# => cat is a mammal
# => dog is a mammal

for (k, v) in Dict("dog" => "mammal", "cat" => "mammal", "mouse" => "mammal")
    println("$k is a $v")
end
# => mouse is a mammal
# => cat is a mammal
# => dog is a mammal

# while 루프는 조건이 참인 동안 반복됩니다.
let x = 0
    while x < 4
        println(x)
        x += 1  # 제자리 증가의 약어: x = x + 1
    end
end
# => 0
# => 1
# => 2
# => 3

# try/catch 블록으로 예외 처리
try
    error("help")
catch e
    println("caught it $e")
end
# => caught it ErrorException("help")

####################################################
## 4. 함수
####################################################

# 키워드 'function'은 새 함수를 만듭니다.
# function name(arglist)
#   body...
# end
function add(x, y)
    println("x is $x and y is $y")

    # 함수는 마지막 문의 값을 반환합니다.
    x + y
end

add(5, 6)
# => x is 5 and y is 6
# => 11

# 함수의 간결한 할당
f_add(x, y) = x + y  # => f_add (1개의 메서드가 있는 제네릭 함수)
f_add(3, 4)  # => 7

# 함수는 튜플로 여러 값을 반환할 수도 있습니다.
fn(x, y) = x + y, x - y # => fn (1개의 메서드가 있는 제네릭 함수)
fn(3, 4)  # => (7, -1)

# 가변 개수의 위치 인수를 사용하는 함수를 정의할 수 있습니다.
function varargs(args...)
    return args
    # return 키워드를 사용하여 함수 어디에서나 반환
end
# => varargs (1개의 메서드가 있는 제네릭 함수)

varargs(1, 2, 3)  # => (1,2,3)

# ...는 스플랫이라고 합니다.
# 함수 정의에서 방금 사용했습니다.
# 함수 호출에서도 사용할 수 있으며, 배열 또는 튜플의 내용을 인수 목록으로 스플랫합니다.
add([5,6]...)  # 이것은 add(5,6)와 동일합니다.

x = (5, 6)  # => (5,6)
add(x...)  # 이것은 add(5,6)와 동일합니다.


# 선택적 위치 인수가 있는 함수를 정의할 수 있습니다.
function defaults(a, b, x=5, y=6)
    return "$a $b and $x $y"
end
# => defaults (3개의 메서드가 있는 제네릭 함수)

defaults('h', 'g')  # => "h g and 5 6"
defaults('h', 'g', 'j')  # => "h g and j 6"
defaults('h', 'g', 'j', 'k')  # => "h g and j k"
try
    defaults('h')  # => ERROR: MethodError: no method matching defaults(::Char)
    defaults()  # => ERROR: MethodError: no method matching defaults()
catch e
    println(e)
end

# 키워드 인수를 사용하는 함수를 정의할 수 있습니다.
function keyword_args(;k1=4, name2="hello")  # ; 참고
    return Dict("k1" => k1, "name2" => name2)
end
# => keyword_args (1개의 메서드가 있는 제네릭 함수)

keyword_args(name2="ness")  # => ["name2"=>"ness", "k1"=>4]
keyword_args(k1="mine")     # => ["name2"=>"hello", "k1"=>"mine"]
keyword_args()              # => ["name2"=>"hello", "k1"=>4]

# 모든 종류의 인수를 동일한 함수에 결합할 수 있습니다.
function all_the_args(normalArg, optionalPositionalArg=2; keywordArg="foo")
    println("normal arg: $normalArg")
    println("optional arg: $optionalPositionalArg")
    println("keyword arg: $keywordArg")
end
# => all_the_args (2개의 메서드가 있는 제네릭 함수)

all_the_args(1, 3, keywordArg=4)
# => normal arg: 1
# => optional arg: 3
# => keyword arg: 4

# Julia에는 일급 함수가 있습니다.
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end
# => create_adder (1개의 메서드가 있는 제네릭 함수)

# 이것은 익명 함수를 만들기 위한 "stabby lambda syntax"입니다.
(x -> x > 2)(3)  # => true

# 이 함수는 위의 create_adder 구현과 동일합니다.
function create_adder(x)
    y -> x + y
end
# => create_adder (1개의 메서드가 있는 제네릭 함수)

# 원하는 경우 내부 함수에 이름을 지정할 수도 있습니다.
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end
# => create_adder (1개의 메서드가 있는 제네릭 함수)

add_10 = create_adder(10) # => (::getfield(Main, Symbol("#adder#11")){Int64})
                          # (1개의 메서드가 있는 제네릭 함수)
add_10(3) # => 13


# 내장 고차 함수가 있습니다.
map(add_10, [1,2,3])  # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7])  # => [6, 7]

# 목록 이해를 사용할 수 있습니다.
[add_10(i) for i = [1, 2, 3]]   # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] # => [6, 7]

####################################################
## 5. 유형
####################################################

# Julia에는 유형 시스템이 있습니다.
# 모든 값에는 유형이 있습니다. 변수 자체에는 유형이 없습니다.
# `typeof` 함수를 사용하여 값의 유형을 얻을 수 있습니다.
typeof(5)  # => Int64

# 유형은 일급 값입니다.
typeof(Int64)     # => DataType
typeof(DataType)  # => DataType
# DataType은 자신을 포함하여 유형을 나타내는 유형입니다.

# 유형은 문서, 최적화 및 디스패치에 사용됩니다.
# 정적으로 확인되지 않습니다.

# 사용자는 유형을 정의할 수 있습니다.
# 다른 언어의 레코드 또는 구조체와 같습니다.
# 새 유형은 `struct` 키워드를 사용하여 정의됩니다.

# struct Name
#   field::OptionalType
#   ...
# end
struct Tiger
    taillength::Float64
    coatcolor  # 유형 주석을 포함하지 않는 것은 `::Any`와 동일합니다.
end

# 기본 생성자의 인수는 유형의 속성이며, 정의에 나열된 순서대로입니다.
tigger = Tiger(3.5, "orange")  # => Tiger(3.5,"orange")

# 유형은 해당 유형의 값에 대한 생성자 함수 역할을 합니다.
sherekhan = typeof(tigger)(5.6, "fire")  # => Tiger(5.6,"fire")

# 이러한 구조체 스타일 유형을 구체적인 유형이라고 합니다.
# 인스턴스화할 수 있지만 하위 유형을 가질 수 없습니다.
# 다른 종류의 유형은 추상 유형입니다.

# abstract Name
abstract type Cat end  # 이름과 유형 계층의 지점일 뿐입니다.

# 추상 유형은 인스턴스화할 수 없지만 하위 유형을 가질 수 있습니다.
# 예를 들어, Number는 추상 유형입니다.
subtypes(Number)  # => 2-element Array{Any,1}:
                  # =>  Complex
                  # =>  Real
subtypes(Cat)  # => 0-element Array{Any,1}

# AbstractString은 이름에서 알 수 있듯이 추상 유형이기도 합니다.
subtypes(AbstractString)  # => 4-element Array{Any,1}:
                          # =>  String
                          # =>  SubString
                          # =>  SubstitutionString
                          # =>  Test.GenericString

# 모든 유형에는 상위 유형이 있습니다. `supertype` 함수를 사용하여 가져옵니다.
typeof(5) # => Int64
supertype(Int64)    # => Signed
supertype(Signed)   # => Integer
supertype(Integer)  # => Real
supertype(Real)     # => Number
supertype(Number)   # => Any
supertype(supertype(Signed))  # => Real
supertype(Any)      # => Any
# Int64를 제외한 이러한 유형은 모두 추상적입니다.
typeof("fire")      # => String
supertype(String)   # => AbstractString
# 마찬가지로 String도 마찬가지입니다.
supertype(SubString)  # => AbstractString

# <:는 하위 유형 연산자입니다.
struct Lion <: Cat  # Lion은 Cat의 하위 유형입니다.
    maneColor
    roar::AbstractString
end

# 유형에 대한 더 많은 생성자를 정의할 수 있습니다.
# 유형과 동일한 이름의 함수를 정의하고 기존 생성자를 호출하여 올바른 유형의 값을 얻습니다.
Lion(roar::AbstractString) = Lion("green", roar)
# 이것은 유형 정의 외부에 있으므로 외부 생성자입니다.

struct Panther <: Cat  # Panther도 Cat의 하위 유형입니다.
    eyeColor
    Panther() = new("green")
    # Panther는 이 생성자만 가지며 기본 생성자는 없습니다.
end
# Panther와 같이 내부 생성자를 사용하면 유형의 값을 만드는 방법을 제어할 수 있습니다.
# 가능하면 내부 생성자 대신 외부 생성자를 사용해야 합니다.

####################################################
## 6. 다중 디스패치
####################################################

# Julia에서 모든 명명된 함수는 제네릭 함수입니다.
# 이것은 많은 작은 메서드로 구성됨을 의미합니다.
# Lion의 각 생성자는 제네릭 함수 Lion의 메서드입니다.

# 비생성자 예제로 meow 함수를 만들어 보겠습니다:

# Lion, Panther, Tiger에 대한 정의
function meow(animal::Lion)
    animal.roar  # 점 표기법을 사용하여 유형 속성에 액세스
end

function meow(animal::Panther)
    "grrr"
end

function meow(animal::Tiger)
    "rawwwr"
end

# meow 함수 테스트
meow(tigger)  # => "rawwwr"
meow(Lion("brown", "ROAAR"))  # => "ROAAR"
meow(Panther()) # => "grrr"

# 로컬 유형 계층 검토
Tiger   <: Cat  # => false
Lion    <: Cat  # => true
Panther <: Cat  # => true

# Cat을 사용하는 함수 정의
function pet_cat(cat::Cat)
    println("The cat says $(meow(cat))")
end
# => pet_cat (1개의 메서드가 있는 제네릭 함수)

pet_cat(Lion("42")) # => The cat says 42
try
    pet_cat(tigger) # => ERROR: MethodError: no method matching pet_cat(::Tiger)
catch e
    println(e)
end

# OO 언어에서는 단일 디스패치가 일반적입니다.
# 이것은 첫 번째 인수의 유형에 따라 메서드가 선택됨을 의미합니다.
# Julia에서는 모든 인수 유형이 최상의 메서드를 선택하는 데 기여합니다.

# 차이점을 보기 위해 더 많은 인수가 있는 함수를 정의해 보겠습니다.
function fight(t::Tiger, c::Cat)
    println("The $(t.coatcolor) tiger wins!")
end
# => fight (1개의 메서드가 있는 제네릭 함수)

fight(tigger, Panther())  # => The orange tiger wins!
fight(tigger, Lion("ROAR")) # => The orange tiger wins!

# Cat이 구체적으로 Lion일 때 동작 변경
fight(t::Tiger, l::Lion) = println("The $(l.maneColor)-maned lion wins!")
# => fight (2개의 메서드가 있는 제네릭 함수)

fight(tigger, Panther())  # => The orange tiger wins!
fight(tigger, Lion("ROAR")) # => The green-maned lion wins!

# 싸우기 위해 Tiger가 필요하지 않습니다.
fight(l::Lion, c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (3개의 메서드가 있는 제네릭 함수)

fight(Lion("balooga!"), Panther())  # => The victorious cat says grrr
try
    fight(Panther(), Lion("RAWR"))
    # => ERROR: MethodError: no method matching fight(::Panther, ::Lion)
    # => Closest candidates are:
    # =>   fight(::Tiger, ::Lion) at ...
    # =>   fight(::Tiger, ::Cat) at ...
    # =>   fight(::Lion, ::Cat) at ...
    # => ...
catch e
    println(e)
end

# 고양이가 먼저 가도록 하십시오.
fight(c::Cat, l::Lion) = println("The cat beats the Lion")
# => fight (4개의 메서드가 있는 제네릭 함수)

# 이 경고는 다음에서 어떤 싸움이 호출될지 불분명하기 때문입니다:
try
    fight(Lion("RAR"), Lion("brown", "rarrr"))
    # => ERROR: MethodError: fight(::Lion, ::Lion) is ambiguous. Candidates:
    # =>   fight(c::Cat, l::Lion) in Main at ...
    # =>   fight(l::Lion, c::Cat) in Main at ...
    # => Possible fix, define
    # =>   fight(::Lion, ::Lion)
    # => ...
catch e
    println(e)
end
# 결과는 다른 버전의 Julia에서 다를 수 있습니다.

fight(l::Lion, l2::Lion) = println("The lions come to a tie")
# => fight (5개의 메서드가 있는 제네릭 함수)
fight(Lion("RAR"), Lion("brown", "rarrr"))  # => The lions come to a tie


# 내부
# 생성된 llvm 및 어셈블리 코드를 볼 수 있습니다.

square_area(l) = l * l  # square_area (1개의 메서드가 있는 제네릭 함수)

square_area(5)  # => 25

# square_area에 정수를 공급하면 어떻게 됩니까?
code_native(square_area, (Int32,), syntax = :intel)
	#         .text
	# ; Function square_area {
	# ; Location: REPL[116]:1       # Prologue
	#         push    rbp
	#         mov     rbp, rsp
	# ; Function *; {
	# ; Location: int.jl:54
	#         imul    ecx, ecx      # l을 제곱하고 결과를 ECX에 저장
	# ;}
	#         mov     eax, ecx
	#         pop     rbp           # 이전 기본 포인터 복원
	#         ret                   # 결과는 여전히 EAX에 있습니다.
	#         nop     dword ptr [rax + rax]
	# ;}

code_native(square_area, (Float32,), syntax = :intel)
    #         .text
	# ; Function square_area {
	# ; Location: REPL[116]:1
	#         push    rbp
	#         mov     rbp, rsp
	# ; Function *; {
	# ; Location: float.jl:398
	#         vmulss  xmm0, xmm0, xmm0  # 스칼라 단정밀도 곱셈 (AVX)
	# ;}
	#         pop     rbp
	#         ret
	#         nop     word ptr [rax + rax]
	# ;}

code_native(square_area, (Float64,), syntax = :intel)
    #         .text
	# ; Function square_area {
	# ; Location: REPL[116]:1
	#         push    rbp
	#         mov     rbp, rsp
	# ; Function *; {
	# ; Location: float.jl:399
	#         vmulsd  xmm0, xmm0, xmm0  # 스칼라 배정밀도 곱셈 (AVX)
	# ;}
	#         pop     rbp
	#         ret
	#         nop     word ptr [rax + rax]
	# ;}

# julia는 인수 중 하나가 부동 소수점인 경우 부동 소수점 명령을 사용합니다.
# 원의 면적을 계산해 보겠습니다.
circle_area(r) = pi * r * r     # circle_area (1개의 메서드가 있는 제네릭 함수)
circle_area(5)  # 78.53981633974483

code_native(circle_area, (Int32,), syntax = :intel)
    #         .text
	# ; Function circle_area {
	# ; Location: REPL[121]:1
	#         push    rbp
	#         mov     rbp, rsp
	# ; Function *; {
	# ; Location: operators.jl:502
	# ; Function *; {
	# ; Location: promotion.jl:314
	# ; Function promote; {
	# ; Location: promotion.jl:284
	# ; Function _promote; {
	# ; Location: promotion.jl:261
	# ; Function convert; {
	# ; Location: number.jl:7
	# ; Function Type; {
	# ; Location: float.jl:60
	#         vcvtsi2sd       xmm0, xmm0, ecx     # 메모리에서 정수(r) 로드
	#         movabs  rax, 497710928              # pi 로드
	# ;}}}}}
	# ; Function *; {
	# ; Location: float.jl:399
	#         vmulsd  xmm1, xmm0, qword ptr [rax] # pi * r
	#         vmulsd  xmm0, xmm1, xmm0            # (pi * r) * r
	# ;}}
	#         pop     rbp
	#         ret
	#         nop     dword ptr [rax]
	# ;}

code_native(circle_area, (Float64,), syntax = :intel)
    #         .text
	# ; Function circle_area {
	# ; Location: REPL[121]:1
	#         push    rbp
	#         mov     rbp, rsp
	#         movabs  rax, 497711048
	# ; Function *; {
	# ; Location: operators.jl:502
	# ; Function *; {
	# ; Location: promotion.jl:314
	# ; Function *; {
	# ; Location: float.jl:399
	#         vmulsd  xmm1, xmm0, qword ptr [rax]
	# ;}}}
	# ; Function *; {
	# ; Location: float.jl:399
	#         vmulsd  xmm0, xmm1, xmm0
	# ;}
	#         pop     rbp
	#         ret
	#         nop     word ptr [rax + rax]
	# ;}
```

## 추가 자료

[Julia 문서](https://docs.julialang.org/)에서 더 자세한 내용을 얻을 수 있습니다.

Julia에 대한 도움을 얻을 수 있는 가장 좋은 곳은 (매우 친절한) [Discourse 포럼](https://discourse.julialang.org/)입니다.