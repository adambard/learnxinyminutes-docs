---
name: EDN
filename: learnedn.edn
contributors:
  - ["Jason Yeo", "https://github.com/jsyeo"]
  - ["Jonathan D Johnston", "https://github.com/jdjohnston"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

확장 가능한 데이터 표기법(EDN)은 데이터를 직렬화하기 위한 형식입니다.

EDN은 Clojure에서 사용되는 구문의 하위 집합입니다. EDN으로 정의된 데이터를 읽는 것은 특히 신뢰할 수 없는 소스에서 전체 Clojure 구문으로 정의된 데이터를 읽는 것보다 안전합니다. EDN은 데이터로 제한되며 코드는 없습니다. JSON과 의도가 비슷합니다. Clojure에서 더 일반적으로 사용되지만 다른 많은 언어에 대한 EDN 구현이 있습니다.

JSON 및 YAML에 대한 EDN의 주요 이점은 확장 가능하다는 것입니다. 나중에 확장되는 방법을 살펴보겠습니다.

```clojure
; 주석은 세미콜론으로 시작합니다.
; 세미콜론 뒤의 모든 것은 무시됩니다.

;;;;;;;;;;;;;;;;;;;
;;; 기본 유형 ;;;
;;;;;;;;;;;;;;;;;;;

il         ; 다른 언어에서는 null로도 알려져 있습니다.

; 부울
true
false

; 문자열은 큰따옴표로 묶습니다.
"hungarian breakfast"
"farmer's cheesy omelette"

; 문자는 백슬래시가 앞에 옵니다.
\g \r \a \c \e

; 키워드는 콜론으로 시작합니다. 열거형처럼 작동합니다. Ruby의 심볼과 비슷합니다.
:eggs
:cheese
:olives

; 심볼은 식별자를 나타내는 데 사용됩니다.
; /를 사용하여 심볼에 네임스페이스를 지정할 수 있습니다. / 앞의 모든 것은 심볼의 네임스페이스입니다.
spoon
kitchen/spoon ; spoon과 다릅니다.
kitchen/fork
github/fork   ; 이것으로 먹을 수 없습니다.

; 정수 및 부동 소수점
42
3.14159

; 목록은 값의 시퀀스입니다.
(:bun :beef-patty 9 "yum!")

; 벡터는 임의 접근을 허용합니다.
[:gelato 1 2 -2]

; 맵은 키를 값과 연결하는 연관 데이터 구조입니다.
{:eggs        2
 :lemon-juice 3.5
 :butter      1}

; 키로 키워드를 사용하는 데 제한이 없습니다.
{[1 2 3 4] "tell the people what she wore",
 [5 6 7 8] "the more you see the more you hate"}

; 가독성을 위해 쉼표를 사용할 수 있습니다. 공백으로 처리됩니다.

; 집합은 고유한 요소를 포함하는 컬렉션입니다.
#{:a :b 88 "huat"}

;;;;;;;;;;;;;;;;;;;;;;;
;;; 태그가 지정된 요소 ;;;
;;;;;;;;;;;;;;;;;;;;;;;

; EDN은 # 기호로 요소를 태그 지정하여 확장할 수 있습니다.

#MyYelpClone/MenuItem {:name "eggs-benedict" :rating 10}

; Clojure 예제로 설명하겠습니다. 해당 EDN 조각을 MenuItem 레코드로 변환하고 싶다고 가정합니다.

(defrecord MenuItem [name rating])

; defrecord는 무엇보다도 map->MenuItem을 정의했습니다. 이 함수는 필드 이름(키워드)에서 값으로의 맵을 가져와 user.MenuItem 레코드를 생성합니다.

; EDN을 Clojure 값으로 변환하려면 내장 EDN 리더인 clojure.edn/read-string을 사용해야 합니다.

(clojure.edn/read-string "{:eggs 2 :butter 1 :flour 5}")
; -> {:eggs 2 :butter 1 :flour 5}

; 태그가 지정된 요소를 변환하려면 clojure.edn/read-string에 태그 심볼을 데이터 리더 함수에 매핑하는 :readers 맵이 있는 옵션 맵을 전달하십시오.

(clojure.edn/read-string
    {:readers {'MyYelpClone/MenuItem map->MenuItem}}
    "#MyYelpClone/MenuItem {:name \"eggs-benedict\" :rating 10}")
; -> #user.MenuItem{:name "eggs-benedict", :rating 10}
```

# 참조

- [EDN 사양](https://github.com/edn-format/edn)
- [구현](https://github.com/edn-format/edn/wiki/Implementations)
- [태그가 지정된 요소](http://www.compoundtheory.com/clojure-edn-walkthrough/)

```