---
name: Easylang
contributors:
    - ["chkas", "https://github.com/chkas"]
filename: easylang.el
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

**Easylang**은 내장 그래픽 기능과 사용하기 쉽고 오프라인에서 사용할 수 있는 브라우저 IDE를 갖춘 간단한 프로그래밍 언어입니다. 간단한 구문과 의미 체계 덕분에 교육 및 학습 프로그래밍 언어로 적합합니다. 웹 페이지에 포함할 수 있는 그래픽 애플리케이션을 작성하는 데에도 사용할 수 있습니다.

*Easylang*은 정적으로 유형이 지정되며 데이터 유형으로 문자열과 숫자(부동 소수점), 크기 조정 가능한 문자열 및 숫자 배열, 배열의 배열만 있습니다.

[브라우저 IDE](https://easylang.online/ide/)에는 초보자를 위한 튜토리얼을 포함한 다양한 튜토리얼이 포함되어 있습니다.

```
print "Hello world"
#
# 숫자 변수 (64비트 부동 소수점)
#
h = 3.14
print h
#
# 문자열 변수
#
str$ = "monkey"
# 문자열은 커질 수 있습니다.
str$ &= " circus"
print str$
#
# 블록은 'end' 또는 점으로 끝나며, 줄 바꿈은
# 공백 외에 다른 의미가 없습니다.
#
for i = 1 to 5
  sum += i * i
.
print sum
#
# 함수에는 값 및 참조
# 매개변수가 있으며 반환 값은 없습니다.
#
func gcd a b . res .
  # a와 b는 값 매개변수입니다.
  # res는 참조 매개변수입니다.
  while b <> 0
    # h는 함수에서 처음 사용되므로
    # 지역 변수입니다.
    h = b
    b = a mod b
    a = h
  .
  res = a
.
call gcd 120 35 r
print r
#
# 문자열을 연결할 수 있으며 숫자는
# 자동으로 문자열로 변환됩니다.
#
print "1 + 2 = " & 1 + 2
#
# 숫자 배열
#
a[] = [ 2.1 3.14 3 ]
#
# 배열은 커질 수 있습니다.
a[] &= 4
print a[]
#
# 배열, 문자열 및 숫자는 값으로 복사됩니다.
#
b[] = a[]
a[] &= 4
print a[] ; print b[]
#
# 배열 교환은 빠릅니다.
#
swap a[] b[]
print a[] ; print b[]
#
# 문자열 배열
#
fruits$[] = [ "apple" "banana" "orange" ]
#
# for-in은 배열의 요소를 반복합니다.
#
for fruit$ in fruits$[]
  print fruit$
.
#
# 문자열은 단일 문자에도 사용됩니다.
#
letters$[] = str_chars "ping"
print letters$[]
letters$[1] = "o"
print str_join letters$[]
#
# 2차원 배열은 배열의 배열입니다.
# 이것은 길이 4의 3개 배열을 정의합니다.
#
len a[][] 3
for i range len a[][]
  len a[i][] 4
.
a[1][2] = 99
print a[][]
#
# 내장 함수
if sin 90 = 1
  print "angles are in degree"
.
print pow 2 8
# 1970년 이후 초
print floor sys_time
# 난수
print randomf
print random 6 + 1
#
# 시와 분
print substr time_str sys_time 11 5
#
print str_ord "A"
print str_chr 65
#
# 숫자 형식 설정
numfmt 0 4
print sqrt 2
print pi
print logn 10
#
a$[] = str_split "10,15,22" ","
print a$[]
print 2 * number a$[0]
print len a$[]
print len "Hello"
#
# 'break n'을 사용하면 중첩된 루프와 함수를 나갈 수 있습니다.
#
names$[] = [ ]
func name2id name$ . id .
  for id range len names$[]
    if names$[id] = name$
      # 루프와 함수를 나갑니다.
      break 2
    .
  .
  names$[] &= name$
.
call name2id "alice" id ; print id
call name2id "bob" id ; print id
call name2id "alice" id ; print i
#
# 'repeat'를 사용하면 루프를 만들 수 있으며, 'until'을 사용하여
# 루프 본문에서 나갈 수 있습니다.
#
sum = 0
repeat
  s$ = input
  until s$ = ""
  sum += number s$
.
print "sum: " & sum
#
# "input"은 "input_data" 섹션에서 문자열을 읽습니다.
# 있는 경우, 그렇지 않으면 프롬프트를 통해 읽습니다.
#
input_data
10
-2
6
```

내장 그래픽 기본 요소 및 이벤트 기반 프로그래밍

```
# 마우스를 사용한 간단한 그리기
#
set_linewidth 4
set_color 900
# 색상은 0에서 999까지 코딩되며,
# 왼쪽 숫자는 빨간색 구성 요소를 지정하고,
# 가운데 숫자는 녹색 구성 요소를 지정하고,
# 오른쪽 숫자는 파란색 구성 요소를 지정합니다.
#
on mouse_down
  down = 1
  move_pen mouse_x mouse_y
  # 그리기 펜을 실제 마우스 위치로 이동합니다.
  draw_circle 2
.
on mouse_up
  down = 0
.
on mouse_move
  if down = 1
    draw_line mouse_x mouse_y
  .
.
```

```
# 애니메이션 진자
#
on animate
  # 애니메이션 이벤트는 각 화면 새로 고침 후에 발생합니다.
  #
  clear_screen
  move_pen 50 50
  draw_circle 1
  x = 50 + 40 * sin ang
  y = 50 - 40 * cos ang
  draw_line x y
  draw_circle 5
  vel += sin ang / 5
  ang += vel
.
ang = 10
```

* [Easylang에 대한 추가 정보](https://easylang.online/)

* [소스 코드](https://github.com/chkas/easylang)