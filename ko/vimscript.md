# vimscript.md (번역)

---
name: Vimscript
filename: learnvimscript.vim
contributors:
    - ["HiPhish", "http://hiphish.github.io/"]
---

```vim
" ##############
"  소개
" ##############
"
" Vim 스크립트(VimL이라고도 함)는 값, 변수, 함수 또는 루프와 같이 스크립팅 언어에서 기대할 수 있는 여러 기능을 제공하는 Vim의 ex-명령어 하위 집합입니다.
" Vim 스크립트 파일은 단지 ex-명령어의 시퀀스라는 것을 항상 염두에 두십시오. 스크립트가 프로그래밍 언어 기능과 원시 ex-명령어를 혼합하는 것은 매우 일반적입니다.
"
" 명령줄 모드에서 명령을 직접 입력하여 Vim 스크립트를 실행하거나(명령줄 모드로 들어가려면 `:` 누름) 파일에 작성(선행 `:` 없이)하고 실행 중인 Vim 인스턴스에서 소싱할 수 있습니다(`:source path/to/file`).
" 일부 파일은 구성의 일부로 자동으로 소싱됩니다(|startup| 참조). 이 가이드는 ex-명령어에 익숙하고 스크립팅만 다룰 것이라고 가정합니다. 관련 매뉴얼 섹션에 대한 도움말 항목이 포함되어 있습니다.
"
" Vim 스크립트에 대한 공식 소개는 |usr_41.txt|를 참조하십시오. 주석은 일치하지 않는 `"` 다음에 오는 모든 것이 줄 끝까지이며, `|`는 지침을 구분합니다(대부분의 다른 언어에서 `;`가 하는 역할). |help.txt|와 같이 `|`로 둘러싸인 매뉴얼에 대한 참조.

" 이것은 주석입니다

" 세로줄 '|' (파이프)는 명령을 구분합니다
echo 'Hello' | echo 'world!'

" 명령어 뒤에 주석을 다는 것은 보통 작동합니다
pwd                   " 현재 작업 디렉토리를 표시합니다

" 일부 명령어를 제외하고는 그렇지 않습니다. 주석 앞에 명령어 구분 기호를 사용하십시오(echo는 따옴표가 문자열을 시작한다고 가정합니다)
echo 'Hello world!'  | " 메시지를 표시합니다

" 줄 바꿈은 다음 줄의 첫 번째 비공백 문자로 백슬래시를 배치하여 이스케이프할 수 있습니다. 스크립트 파일에서만 작동하며 명령줄에서는 작동하지 않습니다
echo " Hello
    \ world "

echo [1,
    \ 2]

echo {
    \ 'a': 1,
    \ 'b': 2
\}


" #######
"  타입
" #######
"
" 타입에 대한 개요는 |E712|를 참조하십시오. 연산자에 대한 개요는 |expression-syntax|를 참조하십시오.

" 숫자 (|expr-number|)
" #######

echo  123         | " 십진수
echo  0b1111011   | " 이진수
echo  0173        | " 8진수
echo  0x7B        | " 16진수
echo  123.0       | " 부동 소수점
echo  1.23e2      | " 부동 소수점 (과학적 표기법)

" 선행 `0`이 있는 *정수*는 8진수 표기법입니다.
" 일반적인 산술 연산이 지원됩니다.

echo  1 + 2       | " 덧셈
echo  1 - 2       | " 뺄셈
echo  - 1         | " 부정 (단항 빼기)
echo  + 1         | " 단항 더하기 (실제로는 아무것도 하지 않지만 여전히 합법적)
echo  1 * 2       | " 곱셈
echo  1 / 2       | " 나눗셈
echo  1 % 2       | " 나머지 (나머지)

" 불리언 (|Boolean|)
" ########
"
" 숫자 0은 거짓이고 다른 모든 숫자는 참입니다. 문자열은 암시적으로 숫자로 변환됩니다(아래 참조).
" 두 개의 미리 정의된 의미 상수.

echo  v:true      | " 1 또는 문자열 'v:true'로 평가됩니다
echo  v:false     | " 0 또는 문자열 'v:false'로 평가됩니다

" 불리언 값은 두 객체의 비교 결과일 수 있습니다.

echo  x == y             | " 값에 의한 동등성
echo  x != y             | " 불일치
echo  x >  y             | " 보다 큼
echo  x >= y             | " 크거나 같음
echo  x <  y             | " 보다 작음
echo  x <= y             | " 작거나 같음
echo  x is y             | " 인스턴스 ID (리스트 및 딕셔너리)
echo  x isnot y          | " 인스턴스 비 ID (리스트 및 딕셔너리)

" 문자열은 영숫자 순서에 따라 비교됩니다
" echo 'a' < 'b'. 대소문자 구분은 'ignorecase' 설정에 따라 다릅니다.
"
" 명시적인 대소문자 구분은 연산자에 '#' (대소문자 일치) 또는 '?' (대소문자 무시)를 추가하여 지정됩니다.
" 이식 가능한 스크립트를 작성할 때 명시적으로 대소문자 구분을 선호하십시오.

echo  'a' <  'B'         | " 'ignorecase'에 따라 참 또는 거짓
echo  'a' <? 'B'         | " 참
echo  'a' <# 'B'         | " 거짓

" 정규식 일치
echo  "hi" =~  "hello"    | " 정규식 일치, 'ignorecase' 사용
echo  "hi" =~# "hello"    | " 정규식 일치, 대소문자 구분
echo  "hi" =~? "hello"    | " 정규식 일치, 대소문자 무시
echo  "hi" !~  "hello"    | " 정규식 불일치, 'ignorecase' 사용
echo  "hi" !~# "hello"    | " 정규식 불일치, 대소문자 구분
echo  "hi" !~? "hello"    | " 정규식 불일치, 대소문자 무시

" 불리언 연산이 가능합니다.

echo  v:true && v:false       | " 논리 AND
echo  v:true || v:false       | " 논리 OR
echo  ! v:true                | " 논리 NOT
echo  v:true ? 'yes' : 'no'   | " 삼항 연산자


" 문자열 (|String|)
" #######
"
" 정렬된 0-인덱스 바이트 시퀀스입니다. 텍스트를 바이트로 인코딩하는 것은 |'encoding'| 옵션에 따라 다릅니다.

" 리터럴 생성자
echo  "Hello world\n"   | " 마지막 두 문자는 줄 바꿈을 나타냅니다
echo  'Hello world\n'   | " 마지막 두 문자는 리터럴입니다
echo  'Let''s go!'      | " 두 개의 작은따옴표는 하나의 따옴표 문자가 됩니다

" 작은따옴표 문자열은 모든 문자를 리터럴로 사용하지만, 두 개의 작은따옴표는 문자열 자체에서 하나의 작은따옴표로 간주됩니다.
" 가능한 모든 이스케이프 시퀀스는 |expr-quote|를 참조하십시오.

" 문자열 연결
" .. 연산자가 선호되지만 Vim 8.1.1114 이후에만 지원됩니다
echo  'Hello ' .  'world'  | " 문자열 연결
echo  'Hello ' .. 'world'  | " 문자열 연결 (새 변형)

" 문자열 인덱싱
echo  'Hello'[0]           | " 첫 번째 바이트
echo  'Hello'[1]           | " 두 번째 바이트
echo  'Hellö'[4]           | " 문자 'ö'가 아닌 바이트를 반환합니다

" 부분 문자열 (두 번째 인덱스는 포함)
echo  'Hello'[:]           | " 전체 문자열 복사
echo  'Hello'[1:3]         | " 부분 문자열, 두 번째에서 네 번째 바이트
echo  'Hello'[1:-2]        | " 부분 문자열, 끝에서 두 번째 바이트까지
echo  'Hello'[1:]          | " 시작 인덱스가 있는 부분 문자열
echo  'Hello'[:2]          | " 끝 인덱스가 있는 부분 문자열
echo  'Hello'[-2:]         | " 문자열 끝을 기준으로 한 부분 문자열

" 음수 인덱스는 문자열 끝을 기준으로 합니다.
" 모든 문자열 관련 함수는 |string-functions|를 참조하십시오.

" 리스트 (|List|)
" #####
"
" 임의의 Vim 스크립트 객체의 정렬된 0-인덱스 이기종 시퀀스입니다.

" 리터럴 생성자
echo  []                   | " 빈 리스트
echo  [1, 2, 'Hello']      | " 요소가 있는 리스트
echo  [1, 2, 'Hello', ]    | " 후행 쉼표 허용
echo  [[1, 2], 'Hello']    | " 리스트는 임의로 중첩될 수 있습니다

" 리스트 연결
echo  [1, 2] + [3, 4]      | " 새 리스트 생성

" 리스트 인덱싱, 음수는 리스트 끝을 기준으로 합니다 (|list-index|)
echo  [1, 2, 3, 4][2]      | " 세 번째 요소
echo  [1, 2, 3, 4][-1]     | " 마지막 요소

" 리스트 슬라이싱 (|sublist|)
echo  [1, 2, 3, 4][:]      | " 전체 리스트의 얕은 복사
echo  [1, 2, 3, 4][:2]     | " 세 번째 항목까지의 하위 리스트 (포함)
echo  [1, 2, 3, 4][2:]     | " 세 번째 항목부터의 하위 리스트 (포함)
echo  [1, 2, 3, 4][:-2]    | " 끝에서 두 번째 항목까지의 하위 리스트 (포함)

" 모든 슬라이싱 작업은 새 리스트를 생성합니다. 리스트를 제자리에서 수정하려면 리스트 함수(|list-functions|)를 사용하거나 항목에 직접 할당하십시오(아래 변수 참조).


" 딕셔너리 (|Dictionary|)
" ############
"
" 키-값 쌍의 정렬되지 않은 시퀀스이며, 키는 항상 문자열입니다(숫자는 암시적으로 문자열로 변환됨).

" 딕셔너리 리터럴
echo  {}                       | " 빈 딕셔너리
echo  {'a': 1, 'b': 2}         | " 딕셔너리 리터럴
echo  {'a': 1, 'b': 2, }       | " 후행 쉼표 허용
echo  {'x': {'a': 1, 'b': 2}}  | " 중첩된 딕셔너리

" 딕셔너리 인덱싱
echo  {'a': 1, 'b': 2}['a']    | " 리터럴 인덱스
echo  {'a': 1, 'b': 2}.a       | " 간단한 키에 대한 구문 설탕

" 딕셔너리 조작 함수는 |dict-functions|를 참조하십시오.


" 함수 참조 (|Funcref|)
" #######
"
" 함수에 대한 참조이며, 생성에 함수 이름을 문자열로 사용합니다.
" 변수에 저장될 때 변수 이름은 함수 이름과 동일한 제한을 가집니다(아래 참조).

echo  function('type')                   | " 함수 type()에 대한 참조
" `funcref('type')`는 인수가 사용자 정의 함수여야 하므로 오류를 발생시킵니다.
" 사용자 정의 함수 정의에 대해서는 아래를 참조하십시오.
echo  funcref('type')                    | " 이름이 아닌 ID별 참조
" 람다(|lambda|)는 익명 함수입니다. 본문에 하나의 표현식만 포함할 수 있으며, 이것이 암시적인 반환값이기도 합니다.
echo  {x -> x * x}                       | " 익명 함수
echo  function('substitute', ['hello'])  | " 부분 함수


" 정규식 (|regular-expression|)
" ##################
"
" 정규식 패턴은 일반적으로 문자열이지만, 경우에 따라 구분 기호 쌍(보통 `/`이지만 원하는 것을 선택할 수 있음) 사이에 정규식을 사용할 수도 있습니다.

" 'hello'를 'Hello'로 대체
substitute/hello/Hello/


" ###########################
"  암시적 타입 변환
" ###########################
"
" 문자열은 숫자로, 숫자는 필요할 때 문자열로 변환됩니다.
" 숫자는 10진수 표기법으로 문자열이 됩니다. 문자열은 숫자로 구문 분석할 수 있으면 숫자 값이 되고, 그렇지 않으면 0이 됩니다.

echo  "1" + 1         | " 숫자
echo  "1" .. 1        | " 문자열
echo  "0xA" + 1       | " 숫자

" 문자열은 불리언으로 사용될 때 숫자처럼 취급됩니다
echo "true" ? 1 : 0   | " 이 문자열은 0으로 구문 분석되어 거짓입니다

" ###########
"  변수
" ###########
"
" 변수는 범위 내에 바인딩됩니다. 범위가 제공되지 않으면 Vim에서 기본값을 선택합니다.
" 값을 바인딩하려면 `:let` 및 `:const`를 사용하고, 바인딩을 해제하려면 `:unlet`을 사용하십시오.

let b:my_var = 1        | " 현재 버퍼에 로컬
let w:my_var = 1        | " 현재 창에 로컬
let t:my_var = 1        | " 현재 탭 페이지에 로컬
let g:my_var = 1        | " 전역 변수
let l:my_var = 1        | " 현재 함수에 로컬 (아래 함수 참조)
let s:my_var = 1        | " 현재 스크립트 파일에 로컬
let a:my_arg = 1        | " 함수 인수 (아래 함수 참조)

" Vim 범위는 읽기 전용입니다
echo  v:true            | " 특수 내장 Vim 변수 (|v:var|)

" 변수와 같은 특수 Vim 메모리 액세스
let @a = 'Hello'        | " 레지스터
let $PATH=''            | " 환경 변수
let &textwidth = 79     | " 옵션
let &l:textwidth = 79   | " 로컬 옵션
let &g:textwidth = 79   | " 전역 옵션

" 딕셔너리로 범위 액세스 (모든 딕셔너리처럼 수정 가능)
" 액세스 및 조작에 대해서는 |dict-functions|, 특히 |get()|을 참조하십시오.
echo  b:                | " 모든 버퍼 변수
echo  w:                | " 모든 창 변수
echo  t:                | " 모든 탭 페이지 변수
echo  g:                | " 모든 전역 변수
echo  l:                | " 모든 로컬 변수
echo  s:                | " 모든 스크립트 변수
echo  a:                | " 모든 함수 인수
echo  v:                | " 모든 Vim 변수

" 상수 변수
const x = 10            | " |:const|, |:lockvar| 참조

" 함수 참조 변수는 함수 이름과 동일한 제한을 가집니다
let IsString = {x -> type(x) == type('')}    | " 전역: 대문자
let s:isNumber = {x -> type(x) == type(0)}   | " 로컬: 모든 이름 허용

" 생략하면 `g:` 범위가 암시되지만, 함수에서는 `l:`이 암시됩니다.


" 다중 값 바인딩 (리스트 언패킹)
" #######################################
"
" 리스트 값을 여러 변수에 할당 (항목 수가 일치해야 함)
let [x, y] = [1, 2]

" 나머지를 나머지 변수에 할당 (세미콜론 참고)
let [mother, father; children] = ['Alice', 'Bob', 'Carol', 'Dennis', 'Emily']


" ##############
"  제어 흐름
" ##############

" 조건문 (|:if|, |:elseif|, |:else|, |:endif|)
" ###########
"
" 조건은 `if`와 `endif` 사이에 설정됩니다. 중첩될 수 있습니다.

let condition = v:true

if condition
    echo 'First condition'
elseif another_condition
    echo 'Second condition'
else
    echo 'Fail'
endif

" 루프 (|:for|, |:endfor|, |:while|, |:endwhile|, |:break|, |:continue|)
" #####
"
" 두 가지 유형의 루프: `:for` 및 `:while`. 다음 반복으로 건너뛰려면 `:continue`를 사용하고, 루프를 빠져나가려면 `:break`를 사용하십시오.

" For-루프 (|:for|, |:endfor|)
" ========
"
" For-루프는 리스트만 반복합니다. 다른 시퀀스를 반복하려면 리스트를 생성하는 함수를 사용해야 합니다.

" 리스트 반복
for person in ['Alice', 'Bob', 'Carol', 'Dennis', 'Emily']
    echo 'Hello ' .. person
endfor

" 중첩된 리스트를 언패킹하여 반복
for [x, y] in [[1, 0], [0, 1], [-1, 0], [0, -1]]
    echo 'Position: x ='  .. x .. ', y = ' .. y
endfor

" 숫자 범위 반복
for i in range(10, 0, -1)  " 10부터 카운트다운
    echo 'T minus'  .. i
endfor

" 딕셔너리 키 반복
for symbol in keys({'π': 3.14, 'e': 2.71})
    echo 'The constant ' .. symbol .. ' is a transcendent number'
endfor

" 딕셔너리 값 반복
for value in values({'π': 3.14, 'e': 2.71})
    echo 'The value ' .. value .. ' approximates a transcendent number'
endfor

" 딕셔너리 키와 값 반복
for [symbol, value] in items({'π': 3.14, 'e': 2.71})
    echo 'The number ' .. symbol .. ' is approximately ' .. value
endfor

" While-루프 (|:while|, |:endwhile|)

let there_yet = v:true
while !there_yet
    echo 'Are we there yet?'
endwhile


" 예외 처리 (|exception-handling|)
" ##################
"
" 새 예외를 문자열로 throw하고, 문자열에 대한 정규식 패턴 일치로 catch합니다.

" 새 예외 throw
throw "Wrong arguments"

" 예외로부터 보호 (두 번째 catch는 모든 예외와 일치)
try
    source path/to/file
catch /Cannot open/
    echo 'Looks like that file does not exist'
catch /.*/
    echo 'Something went wrong, but I do not know what'
finally
    echo 'I am done trying'
endtry


" ##########
"  함수
" ##########

" 함수 정의 (|:function|, |:endfunction|)
" ##################

" 범위가 없는 함수 이름은 대문자로 시작해야 합니다
function! AddNumbersLoudly(x, y)
    " a: 범위를 사용하여 인수에 액세스
    echo 'Adding'  .. a:x ..  'and'  .. a:y   | " 부작용
    return a:x + a:y                          | " 반환 값
endfunction

" 범위가 있는 함수 이름은 소문자로 시작할 수 있습니다
function! s:addNumbersLoudly(x, y)
    echo 'Adding'  .. a:x ..  'and'  .. a:y
    return a:x + a:y
endfunction

" 느낌표가 없으면 함수를 다시 정의하는 것이 오류가 되지만, 느낌표가 있으면 새 정의가 이전 정의를 대체할 수 있습니다.
" Vim 스크립트 파일은 세션 동안 여러 번 다시 로드될 수 있으므로, 무엇을 하는지 정말로 알지 못하는 한 느낌표를 사용하는 것이 가장 좋습니다.

" 함수 정의에는 인수 목록 뒤에 특수 한정자가 있을 수 있습니다.

" 범위 함수는 두 개의 암시적 인수를 정의하며, ex-명령어의 범위로 설정됩니다
function! FirstAndLastLine() range
    echo [a:firstline, a:lastline]
endfunction

" 패턴과 일치하는 첫 번째 및 마지막 줄을 인쇄합니다 (|cmdline-ranges|)
/^#!/,/!#$/call FirstAndLastLine()

" 함수 중단, 오류 발생 시 중단 (|:func-abort|)
function! SourceMyFile() abort
    source my-file.vim        | " 존재하지 않는 파일 소싱 시도
    echo 'This will never be printed'
endfunction

" 클로저, 외부 범위에서 값을 전달하는 함수 (|:func-closure|)
function! MakeAdder(x)
    function! Adder(n) closure
        return a:n + a:x
    endfunction
    return funcref('Adder')
endfunction
let AddFive = MakeAdder(5)
echo AddFive(3)               | " 8 인쇄

" 딕셔너리 함수, 가난한 사람의 OOP 메서드 (|Dictionary-function|)
function! Mylen() dict
    return len(self.data)     | " 암시적 변수 self
endfunction
let mydict = {'data': [0, 1, 2, 3], 'len': function("Mylen")}
echo mydict.len()

" 또는 더 간결하게
let mydict = {'data': [0, 1, 2, 3]}
function! mydict.len()
    return len(self.data)
endfunction

" 함수 호출 (|:call|)
" #################

" 반환 값 및 부작용을 위해 함수 호출
let animals = keys({'cow': 'moo', 'dog': 'woof', 'cat': 'meow'})

" 부작용만을 위해 함수 호출, 잠재적인 반환 값 무시
call sign_undefine()

" call() 함수는 함수 참조를 호출하고 매개변수를 리스트로 전달하며, 함수의 결과를 반환합니다.
echo  call(function('get'), [{'a': 1, 'b': 2}, 'c', 3])   | " 3 인쇄

" Vim 스크립트는 ex-명령어 내에 포함되어 있으므로 함수를 직접 호출할 수 없고 `:call` ex-명령어를 사용해야 합니다.

" 함수 네임스페이스 (|write-library-script|, |autoload|)
" ###################

" autoload/foo/bar.vim에 정의해야 합니다
" 네임스페이스 함수 이름은 대문자로 시작할 필요가 없습니다
function! foo#bar#log(value)
    echomsg value
endfunction

call foo#bar#log('Hello')


" #############################
"  자주 사용되는 ex-명령어
" #############################


" 런타임 파일 소싱 (|'runtimepath'|)
" ######################

" 런타임 경로 중에서 첫 번째 일치 항목 소싱
runtime plugin/my-plugin.vim


" 새 ex-명령어 정의 (|40.2|, |:command|)
" ########################

" 첫 번째 인수는 명령어 이름, 나머지는 명령어 본문
command! SwapAdjacentLines normal! ddp

" 느낌표는 `:function`과 동일하게 작동합니다. 사용자 정의 명령어는 대문자로 시작해야 합니다.
" `:command` 명령어는 `-nargs`와 같이 여러 속성을 가질 수 있으며, 모두 대시로 시작하여 명령어 이름과 구분합니다.

command! -nargs=1 Error echoerr <args>


" 자동 명령어 정의 (|40.3|, |autocmd|, |autocommand-events|)
" ######################

" 인수는 "이벤트", "패턴", 나머지는 "명령어"입니다
autocmd BufWritePost $MYVIMRC source $MYVIMRC

" 이벤트와 패턴은 공백 없는 쉼표로 구분됩니다.
" 표준 이벤트는 |autocmd-events|를 참조하고, 사용자 정의 이벤트는 |User|를 참조하십시오.
" 나머지는 실행될 ex-명령어입니다.

" 자동 그룹
" ===========
"
" 파일이 여러 번 소싱될 때 자동 명령어가 새로 정의되어 이전 명령어를 삭제하지 않고 시간이 지남에 따라 자동 명령어가 쌓이게 됩니다.
" 이를 방지하기 위해 자동 그룹과 다음 의식을 사용하십시오.

augroup auto-source   | " 그룹 이름은 임의적입니다
    autocmd!          | " 현재 그룹의 모든 자동 명령어 삭제
    autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END           | " 기본 자동 그룹으로 다시 전환

" 그룹을 직접 할당할 수도 있습니다.
" 그룹 정의가 한 스크립트에 있고 자동 명령어 정의가 다른 스크립트에 있는 경우 유용합니다.

" 한 파일에서
augroup auto-source
    autocmd!
augroup END

" 다른 파일에서
autocmd auto-source BufWritePost $MYVIMRC source $MYVIMRC

" 실행 (일종의 런타임 매크로)
" ####################################

" 때로는 명령어의 일부가 런타임까지 알려지지 않은 ex-명령어를 구성해야 합니다.

let line = 3                | " 런타임에 결정되는 줄 번호
execute line .. 'delete'    | " 한 줄 삭제

" 일반 모드 명령어 실행
" ##############################
"
" `:normal`을 사용하여 명령줄에서 일반 모드 명령어 시퀀스를 재생합니다.
" 사용자 매핑을 무시하려면 느낌표를 추가하십시오.

normal! ggddGp             | " 첫 번째 줄을 버퍼 끝으로 이식

" 창 명령어는 :normal과 함께 사용하거나, :normal이 작동하지 않을 경우 :wincmd와 함께 사용할 수 있습니다
wincmd L                   | " 현재 창을 맨 오른쪽으로 이동


" ###########################
"  자주 사용되는 함수
" ###########################

" 기능 확인
echo  has('nvim')                  | " Neovim 실행 중
echo  has('python3')               | " Python 3 플러그인 지원
echo  has('unix')                  | " 유닉스 시스템에서 실행 중
echo  has('win32')                 | " Windows 시스템에서 실행 중


" 무언가 존재하는지 테스트
echo  exists('&mouse')             | " 옵션 (존재만 함)
echo  exists('+mouse')             | " 옵션 (존재하고 작동함)
echo  exists('$HOSTNAME')          | " 환경 변수
echo  exists('*strftime')          | " 내장 함수
echo  exists('**s:MyFunc')         | " 사용자 정의 함수
echo  exists('bufcount')           | " 변수 (범위는 선택 사항)
echo  exists('my_dict["foo"]')     | " 변수 (딕셔셔너리 항목)
echo  exists('my_dict["foo"]')     | " 변수 (딕셔셔너리 항목)
echo  exists(':Make')              | " 명령어
echo  exists("#CursorHold")        | " 이벤트에 대해 정의된 자동 명령어
echo  exists("#BufReadPre#*.gz")   | " 이벤트 및 패턴
echo  exists("#filetypeindent")    | " 자동 명령어 그룹
echo  exists("##ColorScheme")      | " 이벤트에 대해 지원되는 자동 명령어

" 다양한 동적 값 (|expand()| 참조)
echo  expand('%')                  | " 현재 파일 이름
echo  expand('<cword>')            | " 커서 아래 현재 단어
echo  expand('%:p')                | " 수정자 가능

" 타입 테스트
" 다음 타입에 대해 고유한 상수가 정의되어 있습니다. 이전 버전의 Vim에는 타입 변수가 없으므로 해결 방법은 참조 문서를 참조하십시오.
echo  type(my_var) == v:t_number      | " 숫자
echo  type(my_var) == v:t_string      | " 문자열
echo  type(my_var) == v:t_func        | " 함수 참조
echo  type(my_var) == v:t_list        | " 리스트
echo  type(my_var) == v:t_dict        | " 딕셔너리
echo  type(my_var) == v:t_float       | " 부동 소수점
echo  type(my_var) == v:t_bool        | " 명시적 불리언
" null 객체의 경우 자체와 비교해야 합니다
echo  my_var is v:null

" 문자열 서식 지정
echo  printf('%d in hexadecimal is %X', 123, 123)


" #####################
"  업계의 비결
" #####################

" 소스 가드
" ############

" 파일이 여러 번 소싱되는 것을 방지합니다. 사용자는 구성에서 변수를 설정하여 플러그인이 전혀 로드되지 않도록 할 수 있습니다.
if exists('g:loaded_my_plugin')
    finish
endif
let g:loaded_my_plugin = v:true

" 기본값
" ##############

" 기본값을 가져옵니다. 사용자가 변수를 정의하면 해당 변수를 사용하고, 그렇지 않으면 하드코딩된 기본값을 사용합니다. 범위도 딕셔너리라는 사실을 이용합니다.
let s:greeting = get(g:, 'my_plugin_greeting', 'Hello')
```
