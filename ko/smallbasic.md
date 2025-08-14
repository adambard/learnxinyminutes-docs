---
name: SmallBASIC
filename: learnsmallbasic.bas
contributors:
    - ["Chris Warren-Smith", "http://smallbasic.sourceforge.net"]
---

## 정보

SmallBASIC은 일상적인 계산, 스크립트 및 프로토타입에 이상적인 빠르고 배우기 쉬운 BASIC 언어 인터프리터입니다. SmallBASIC에는 삼각 함수, 행렬 및 대수 함수, 내장 IDE, 강력한 문자열 라이브러리, 시스템, 사운드 및 그래픽 명령과 구조화된 프로그래밍 구문이 포함되어 있습니다.

## 개발

SmallBASIC은 1999년 말 니콜라스 크리스토풀로스가 팜 파일럿용으로 처음 개발했습니다. 프로젝트 개발은 2005년경부터 크리스 워렌-스미스가 계속해 왔습니다.

SmallBASIC 버전은 프랭클린 e북맨 및 노키아 770을 포함한 여러 초기 휴대용 장치용으로 만들어졌습니다. 또한 다양한 GUI 도구 키트를 기반으로 한 다양한 데스크톱 버전이 출시되었으며, 그중 일부는 더 이상 사용되지 않습니다. 현재 지원되는 플랫폼은 SDL2 기반의 Linux 및 Windows와 NDK 기반의 Android입니다. 데스크톱 명령줄 버전도 사용할 수 있지만 일반적으로 바이너리 형식으로 릴리스되지는 않습니다.

2008년경에 한 대기업이 비슷한 이름의 BASIC과 유사한 프로그래밍 환경을 출시했습니다. SmallBASIC은 이 다른 프로젝트와 관련이 없습니다.

```
REM 이것은 주석입니다
' 그리고 이것도 주석입니다

REM 텍스트 인쇄
print "hello"
? "?는 PRINT의 약자입니다"

REM 제어 구조
FOR index = 0 TO 10 STEP 2
  ? "이것은 줄 번호 "; index
NEXT
J=0
REPEAT
 J++
UNTIL J=10
WHILE J>0
 J--
WEND

REM Select case 문
Select Case "Cool"
 Case "null", 1,2,3,4,5,6,7,8,"Cool","blah"
 Case "Not cool"
   PRINT "Epic fail"
 Case Else
   PRINT "Fail"
End Select

REM TRY/CATCH로 오류 잡기
Try
  fn = Freefile
  Open filename For Input As #fn
Catch err
  Print "failed to open"
End Try

REM 사용자 정의 서브 및 함수
func add2(x,y)
  ' 변수는 SUB 또는 FUNC 범위 내에서 로컬로 선언될 수 있습니다.
  local K
  k = "이 FUNC가 반환되면 k는 더 이상 존재하지 않습니다"
  add2=x+y
end
Print add2(5,5)
sub print_it(it)
  print it
end
print_it "IT...."

REM 선과 픽셀 표시
At 0,ymax/2+txth("Q")
Color 1: ? "sin(x)":
Color 8: ? "cos(x)":
Color 12: ? "tan(x)"
Line 0,ymax/2,xmax,ymax/2
For i=0 to xmax
  Pset i,ymax/2-sin(i*2*pi/ymax)*ymax/4 color 1
  Pset i,ymax/2-cos(i*2*pi/ymax)*ymax/4 color 8
  Pset i,ymax/2-tan(i*2*pi/ymax)*ymax/4 color 12
Next
showpage

REM SmallBASIC은 프랙탈 및 기타 흥미로운 효과를 실험하는 데 좋습니다.
Delay 3000
Randomize
ff = 440.03
For j = 0 to 20
  r = rnd * 1000 % 255
  b = rnd * 1000 % 255
  g = rnd * 1000 % 255
  c = rgb(r,b,g)
  ff += 9.444
  for i=0 to 25000
    f += ff
    x = min(xmax, -x + cos(f*i))
    y = min(ymax, -y + sin(f*i))
    pset x, y color c
    if (i%1000==0) then
      showpage
    fi
  next
Next j

REM 컴퓨터 역사가들을 위해 SmallBASIC은
REM 초기 컴퓨터 서적 및 잡지에서 찾은 프로그램을 실행할 수 있습니다. 예를 들어:
10 LET A=9
20 LET B=7
30 PRINT A*B
40 PRINT A/B

REM SmallBASIC은 JSON과 같은 몇 가지 최신 개념도 지원합니다.
aa = array("{\"cat\":{\"name\":\"harry\"},\"pet\":\"true\"}")
If (ismap(aa) == false) Then
  throw "not an map"
End If
Print aa

PAUSE
```

## 기사

* [시작하기](http://smallbasic.sourceforge.net/?q=node/1573)
* [SmallBASIC에 오신 것을 환영합니다](http://smallbasic.sourceforge.net/?q=node/838)

## GitHub

* [소스 코드](https://github.com/smallbasic/SmallBASIC)
* [참조 스냅샷](http://smallbasic.github.io/)
