---
name: Wolfram
contributors:
    - ["hyphz", "http://github.com/hyphz/"]
filename: learnwolfram.nb
---

Wolfram 언어는 원래 Mathematica에서 사용되던 기본 언어이지만, 지금은 여러 컨텍스트에서 사용할 수 있습니다.

Wolfram 언어에는 여러 인터페이스가 있습니다:

* 라즈베리 파이의 명령줄 커널 인터페이스(_The Wolfram Language_라고도 함)는 대화형으로 실행되며 그래픽 입력을 생성할 수 없습니다.
* _Mathematica_는 대화형 Wolfram이 내장된 풍부한 텍스트/수학 편집기입니다: "코드 셀"에서 <kbd>shift</kbd> + <kbd>Return</kbd>을 누르면 결과가 포함된 출력 셀이 생성되며, 이는 동적이 아닙니다.
* _Wolfram Workbench_는 Wolfram 언어 백엔드에 연결된 Eclipse입니다.

이 예제의 코드는 모든 인터페이스에 입력하고 Wolfram Workbench로 편집할 수 있습니다. 파일에 셀 서식 정보가 포함되어 있지 않아(텍스트로 읽기에 매우 지저분하게 만듦) Mathematica로 직접 로드하는 것이 어색할 수 있습니다. 보거나 편집할 수는 있지만 일부 설정이 필요할 수 있습니다.

```mathematica
(* 이것은 주석입니다 *)

(* Mathematica에서는 이러한 주석 대신 텍스트 셀을 만들어
   코드를 멋지게 조판된 텍스트와 이미지로 주석 처리할 수 있습니다 *)

(* 표현식을 입력하면 결과가 반환됩니다 *)
2*2              (* 4 *)
5+8              (* 13 *)

(* 함수 호출 *)
(* 참고, 함수 이름(및 다른 모든 것)은 대소문자를 구분합니다 *)
Sin[Pi/2]        (* 1 *)

(* 매개변수 하나를 사용하는 함수 호출의 대체 구문 *)
Sin@(Pi/2)       (* 1 *)
(Pi/2) // Sin    (* 1 *)

(* WL의 모든 구문은 함수 호출과 동등한 형태를 가집니다 *)
Times[2, 2]      (* 4 *)
Plus[5, 8]       (* 13 *)

(* 변수를 처음 사용하면 정의되고 전역 변수가 됩니다 *)
x = 5            (* 5 *)
x == 5           (* True, C 스타일 할당 및 동등성 테스트 *)
x                (* 5 *)
x = x + 5        (* 10 *)
x                (* 10 *)
Set[x, 20]       (* 모든 것이 함수와 동등하다고 말했을 때 농담이 아니었습니다 *)
x                (* 20 *)

(* WL은 컴퓨터 대수 시스템을 기반으로 하므로, *)
(* 정의되지 않은 변수를 사용해도 괜찮습니다. 평가를 방해할 뿐입니다. *)
cow + 5          (* 5 + cow, cow는 정의되지 않았으므로 더 이상 평가할 수 없습니다 *)
cow + 5 + 10     (* 15 + cow, 가능한 것은 평가합니다 *)
%                (* 15 + cow, %는 마지막 반환값을 가져옵니다 *)
% - cow          (* 15, 정의되지 않은 변수 cow가 상쇄됨 *)
moo = cow + 5    (* 주의, moo는 이제 숫자가 아닌 표현식을 담고 있습니다! *)

(* 함수 정의 *)
Double[x_] := x * 2    (* 참고: RHS의 즉시 평가를 막기 위한 :=
                          그리고 x 뒤의 _는 패턴 매칭 제약 조건이 없음을 나타냅니다 *)
Double[10]             (* 20 *)
Double[Sin[Pi/2]]      (* 2 *)
Double @ Sin @ (Pi/2)  (* 2, @-구문은 닫는 괄호의 연속을 피합니다 *)
(Pi/2) // Sin // Double(* 2, //-구문은 함수를 실행 순서대로 나열합니다 *)

(* 명령형 스타일 프로그래밍을 위해 ;를 사용하여 문장을 구분합니다 *)
(* LHS의 모든 출력을 버리고 RHS를 실행합니다 *)
MyFirst[] := (Print@"Hello"; Print@"World")  (* 참고: 바깥쪽 괄호가 중요합니다
                                                ;의 우선순위는 :=보다 낮습니다 *)
MyFirst[]                                    (* Hello World *)

(* C-스타일 For 루프 *)
PrintTo[x_] := For[y=0, y<x, y++, (Print[y])]  (* 시작, 테스트, 증가, 본문 *)
PrintTo[5]                                     (* 0 1 2 3 4 *)

(* While 루프 *)
x = 0; While[x < 2, (Print@x; x++)]     (* 테스트와 본문이 있는 While 루프 *)

(* If와 조건문 *)
x = 8; If[x==8, Print@"Yes", Print@"No"]   (* 조건, 참인 경우, 거짓인 경우 *)
Switch[x, 2, Print@"Two", 8, Print@"Yes"]  (* 값 일치 스타일 스위치 *)
Which[x==2, Print@"No", x==8, Print@"Yes"] (* Elif 스타일 스위치 *)

(* 매개변수 이외의 변수는 함수 내부에서도 기본적으로 전역입니다 *)
y = 10             (* 10, 전역 변수 y *)
PrintTo[5]         (* 0 1 2 3 4 *)
y                  (* 5, PrintTo 내부의 루프 카운터에 의해 전역 y가 덮어쓰여짐 *)
x = 20             (* 20, 전역 변수 x *)
PrintTo[5]         (* 0 1 2 3 4 *)
x                  (* 20, PrintTo의 x는 매개변수이며 자동으로 지역 변수입니다 *)

(* 지역 변수는 Module 메타함수를 사용하여 선언됩니다 *)
(* 지역 변수 버전 *)
BetterPrintTo[x_] := Module[{y}, (For[y=0, y<x, y++, (Print@y)])]
y = 20             (* 전역 변수 y *)
BetterPrintTo[5]   (* 0 1 2 3 4 *)
y                  (* 20, 이제 괜찮습니다 *)

(* Module은 실제로 원하는 모든 범위를 선언할 수 있습니다 *)
Module[{count}, count=0;        (* 이 변수 count의 범위 선언 *)
  (IncCount[] := ++count);      (* 이 함수들은 해당 범위 내에 있습니다 *)
  (DecCount[] := --count)]
count              (* count - 전역 변수 count는 정의되지 않았습니다 *)
IncCount[]         (* 1, 범위 내의 count 변수 사용 *)
IncCount[]         (* 2, incCount가 업데이트함 *)
DecCount[]         (* 1, decCount도 마찬가지 *)
count              (* count - 여전히 해당 이름의 전역 변수 없음 *)

(* 리스트 *)
myList = {1, 2, 3, 4}     (* {1, 2, 3, 4} *)
myList[[1]]               (* 1 - 참고: 리스트 인덱스는 0이 아닌 1부터 시작합니다 *)
Map[Double, myList]       (* {2, 4, 6, 8} - 함수형 스타일 리스트 맵 함수 *)
Double /@ myList          (* {2, 4, 6, 8} - 위 구문의 축약형 *)
Scan[Print, myList]       (* 1 2 3 4 - 명령형 스타일 리스트 순회 *)
Fold[Plus, 0, myList]     (* 10 (0+1+2+3+4) *)
FoldList[Plus, 0, myList] (* {0, 1, 3, 6, 10} - 중간 결과를 저장하는 fold *)
Append[myList, 5]         (* {1, 2, 3, 4, 5} - 참고: myList는 업데이트되지 않습니다 *)
Prepend[myList, 5]        (* {5, 1, 2, 3, 4} - 업데이트하려면 "myList = "를 추가하십시오 *)
Join[myList, {3, 4}]      (* {1, 2, 3, 4, 3, 4} *)
myList[[2]] = 5          (* {1, 5, 3, 4} - 이것은 myList를 업데이트합니다 *)

(* 연관 배열, 즉 딕셔너리/해시 *)
myHash = <|"Green" -> 2, "Red" -> 1|>   (* 연관 배열 생성 *)
myHash[["Green"]]                       (* 2, 사용 *)
myHash[["Green"]] := 5                  (* 5, 업데이트 *)
myHash[["Puce"]] := 3.5                 (* 3.5, 확장 *)
KeyDropFrom[myHash, "Green"]            (* Green 키 제거 *)
Keys[myHash]                            (* {Red, Puce} *)
Values[myHash]                          (* {1, 3.5} *)

(* 그리고 Wolfram 데모에서 이것을 보여주지 않을 수 없습니다 *)
Manipulate[y^2, {y, 0, 20}] (* y^2를 표시하고 슬라이더로 y를 0-20 사이에서
                               조정할 수 있는 반응형 사용자 인터페이스를 반환합니다.
                               그래픽 프론트엔드에서만 작동합니다 *)
```

## 더 읽을거리

* [Wolfram 언어 문서 센터](http://reference.wolfram.com/language/)
