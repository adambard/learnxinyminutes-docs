---
name: Tcl
contributors:
    - ["Poor Yorick", "https://pooryorick.com/"]
filename: learntcl.tcl
---

Tcl은 [John Ousterhout](https://wiki.tcl-lang.org/page/John+Ousterhout)이 저술한 회로 설계 도구를 위한 재사용 가능한 스크립팅 언어로 만들어졌습니다. 1997년에 그는 Tcl로 [ACM 소프트웨어 시스템 상](https://en.wikipedia.org/wiki/ACM_Software_System_Award)을 수상했습니다. Tcl은 내장형 스크립팅 언어와 일반 프로그래밍 언어로 모두 사용할 수 있습니다. 동적 문자열, 목록 및 해시 테이블과 같은 데이터 구조를 제공하므로 스크립팅 기능이 필요하지 않은 경우에도 이식 가능한 C 라이브러리로 사용할 수 있습니다. C 라이브러리는 또한 동적 라이브러리 로드, 문자열 서식 지정 및 코드 변환, 파일 시스템 작업, 네트워크 작업 등을 위한 이식 가능한 기능을 제공합니다. Tcl의 다양한 기능은 다음과 같습니다.

* 편리한 크로스 플랫폼 네트워킹 API

* 완전 가상화된 파일 시스템

* 스택 가능한 I/O 채널

* 핵심적으로 비동기

* 전체 코루틴

* 견고하고 사용하기 쉬운 것으로 알려진 스레딩 모델


Tcl은 Lisp와 많은 공통점을 가지고 있지만, 목록 대신 Tcl은 문자열을 언어의 통화로 사용합니다. 모든 값은 문자열입니다. 목록은 정의된 형식을 가진 문자열이며, 프로시저(스크립트)의 본문도 블록이 아닌 문자열입니다. 성능을 달성하기 위해 Tcl은 내부적으로 이러한 값의 구조화된 표현을 캐시합니다. 예를 들어, 목록 루틴은 내부 캐시된 표현에서 작동하며, Tcl은 스크립트에서 실제로 필요한 경우 문자열 표현을 업데이트합니다. Tcl의 쓰기 시 복사 디자인을 통해 스크립트 작성자는 실제로 추가 메모리 오버헤드 없이 큰 데이터 값을 전달할 수 있습니다. 프로시저는 "uplevel", "upvar" 및 "trace"와 같은 더 동적인 루틴을 사용하지 않는 한 자동으로 바이트 컴파일됩니다.

Tcl은 프로그래밍하기에 즐겁습니다. Lisp, Forth 또는 Smalltalk가 흥미롭다고 생각하는 해커 유형뿐만 아니라 자신의 의지에 따라 구부러지는 도구로 사업을 시작하려는 엔지니어와 과학자에게도 어필할 것입니다. 루핑 및 수학 연산과 같이 일반적으로 다른 언어의 구문에 내장된 것을 포함하여 모든 프로그래밍 기능을 루틴으로 노출하는 원칙은 프로젝트에 필요한 모든 도메인별 기능의 배경으로 사라지게 합니다. Lisp보다 훨씬 가벼운 구문은 그냥 방해가 되지 않습니다.



```tcl
#! /bin/env tclsh

###############################################################################
## 1. 지침
###############################################################################

# Tcl은 Sh나 C가 아닙니다! 표준 셸 인용 습관이 Tcl에서 거의 작동하고 사람들이 Tcl을 익히고 다른 언어에서 아는 구문으로 버티려고 하기 때문에 이 말을 해야 합니다. 처음에는 작동하지만 스크립트가 더 복잡해지면 곧 좌절로 이어집니다.

# 중괄호는 인용 메커니즘이며 코드 블록이나 목록을 구성하는 구문이 아닙니다. Tcl에는 둘 다 없습니다. 중괄호는 특수 문자를 이스케이프하는 데 사용되므로 프로시저 본문과 목록으로 해석되어야 하는 문자열을 인용하는 데 적합합니다.


###############################################################################
## 2. 구문
###############################################################################

# 스크립트는 개행 문자나 세미콜론으로 구분된 명령으로 구성됩니다. 각 명령은 루틴에 대한 호출입니다. 첫 번째 단어는 호출할 루틴의 이름이고, 후속 단어는 루틴에 대한 인수입니다. 단어는 공백으로 구분됩니다. 각 인수는 명령의 단어이므로 이미 문자열이며 인용되지 않을 수 있습니다.
set part1 Sal
set part2 ut; set part3 ations


# 달러 기호는 변수 대입을 도입합니다.
set greeting $part1$part2$part3


# "set"에 변수 이름만 주어지면 해당 변수의 값을 반환합니다.
set part3 ;# 변수의 값을 반환합니다.


# 왼쪽 및 오른쪽 대괄호는 단어에 대입할 결과를 위해 평가할 스크립트를 포함합니다.
set greeting $part1$part2[set part3]


# 포함된 스크립트는 여러 명령으로 구성될 수 있으며, 마지막 명령은 대입 결과를 제공합니다.
set greeting $greeting[
    incr i
    incr i
    incr i
]
puts $greeting ;# 출력은 "Salutations3"입니다.

# 명령의 모든 단어는 루틴 이름을 포함하여 문자열이므로 대입을 사용할 수 있습니다. 이 변수 할당을 감안할 때,
set action pu

# 다음 세 가지 명령은 동일합니다.
puts $greeting
${action}ts $greeting
[set action]ts $greeting


# 백슬래시는 문자의 특수 의미를 억제합니다.
set amount \$16.42


# 백슬래시는 특정 문자에 특수 의미를 추가합니다.
puts lots\nof\n\n\n\n\n\nnewlines


# 중괄호로 묶인 단어는 닫는 중괄호를 찾을 때 중괄호 앞의 백슬래시가 계산되지 않는다는 점을 제외하고는 특별한 해석이나 대입이 적용되지 않습니다.
set somevar {
    This is a literal $ sign, and this \} escaped
    brace remains uninterpreted
}


# 큰따옴표로 묶인 단어에서 공백 문자는 특수 의미를 잃습니다.
set name Neo
set greeting "Hello, $name"


# 변수 이름은 모든 문자열이 될 수 있습니다.
set {first name} New


# 중괄호 형식의 변수 대입은 더 복잡한 변수 이름을 처리합니다.
set greeting "Hello, ${first name}"


# "set"은 항상 변수 대입 대신 사용할 수 있으며 모든 변수 이름을 처리할 수 있습니다.
set greeting "Hello, [set {first name}]"


# 목록을 명령으로 풀려면 확장 연산자 "{*}"를 사용하십시오. 이 두 명령은 동일합니다.
set name Neo
set {*}{name Neo}


# 배열은 다른 변수의 컨테이너인 특수 변수입니다.
set person(name) Neo
set person(destiny) {The One}
set greeting "Hello, $person(name)"


# "variable"은 변수를 선언하거나 설정하는 데 사용할 수 있습니다. 변수 이름을 확인하기 위해 전역 네임스페이스와 현재 네임스페이스를 모두 사용하는 "set"과 달리 "variable"은 현재 네임스페이스에서만 작동합니다.
variable name New


# "namespace eval"은 존재하지 않는 경우 새 네임스페이스를 만듭니다. 네임스페이스에는 루틴과 변수가 모두 포함될 수 있습니다.
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}


# 변수 이름의 네임스페이스 구성 요소를 구분하려면 두 개 이상의 콜론을 사용하십시오.
namespace eval people {
    set greeting "Hello $person1::name"
}

# 루틴 이름의 네임스페이스 구성 요소를 구분하려면 두 개 이상의 콜론을 사용하십시오.
proc people::person1::speak {} {
    puts {I am The One.}
}

# 정규화된 이름은 두 개의 콜론으로 시작합니다.
set greeting "Hello $::people::person1::name"



###############################################################################
## 3. 더 이상 구문 없음
###############################################################################

# 다른 모든 기능은 루틴을 통해 구현됩니다. 이 시점부터 새로운 구문은 없습니다. Tcl에 대해 배울 다른 모든 것은 개별 루틴의 동작과 해당 루틴이 인수에 할당하는 의미에 관한 것입니다.



###############################################################################
## 4. 변수 및 네임스페이스
###############################################################################

# 각 변수와 루틴은 일부 네임스페이스와 연결됩니다.

# 아무것도 할 수 없는 인터프리터를 만들려면 전역 네임스페이스를 삭제하십시오. 그런 일을 하는 것은 별로 유용하지 않지만 Tcl의 본질을 보여줍니다. 전역 네임스페이스의 이름은 실제로 빈 문자열이지만 이를 나타내는 유일한 방법은 정규화된 이름으로 사용하는 것입니다. 시도해 보려면 이 루틴을 호출하십시오.
proc delete_global_namespace {} {
    namespace delete ::
}

# "set"은 항상 전역 네임스페이스와 현재 네임스페이스를 모두 주시하므로 변수를 선언하거나 변수에 값을 할당하려면 "variable"을 사용하는 것이 더 안전합니다. "name"이라는 변수가 전역 네임스페이스에 이미 있는 경우 여기에서 "set"을 사용하면 현재 네임스페이스의 변수 대신 전역 변수에 값을 할당하는 반면 "variable"은 현재 네임스페이스에서만 작동합니다.
namespace eval people {
    namespace eval person1 {
        variable name Neo
    }
}

# 네임스페이스에 변수가 선언되면 [set]은 전역 네임스페이스에서 동일한 이름의 변수를 보는 대신 해당 변수를 봅니다.
namespace eval people {
    namespace eval person1 {
        variable name
        set name Neo
    }
}

# 그러나 "set"이 새 변수를 만들어야 하는 경우 항상 현재 네임스페이스를 기준으로 수행합니다.
unset name
namespace eval people {
    namespace eval person1 {
        set name neo
    }

}
set people::person1::name


# 절대 이름은 항상 전역 네임스페이스의 이름(빈 문자열)으로 시작하고 그 뒤에 두 개의 콜론이 옵니다.
set ::people::person1::name Neo


# 프로시저 내에서 "variable"은 현재 네임스페이스의 변수를 로컬 범위에 연결합니다.
namespace eval people::person1 {
    proc fly {} {
        variable name
        puts "$name is flying!"
    }
}




###############################################################################
## 5. 내장 루틴
###############################################################################

# 수학은 "expr"로 수행할 수 있습니다.
set a 3
set b 4
set c [expr {$a + $b}]

# "expr"은 자체적으로 변수 대입을 수행하므로 Tcl이 먼저 변수 대입을 수행하지 않도록 표현식을 중괄호로 묶습니다. 자세한 내용은 "https://wiki.tcl-lang.org/page/Brace+your+expr-essions"를 참조하십시오.


# "expr"은 변수 및 스크립트 대입을 이해합니다.
set c [expr {$a + [set b]}]


# "expr"은 수학 함수 집합을 제공합니다.
set c [expr {pow($a,$b)}]


# 수학 연산자는 ::tcl::mathop 네임스페이스에서 루틴으로 사용할 수 있습니다.
::tcl::mathop::+ 5 3

# 다른 네임스페이스에서 루틴을 가져올 수 있습니다.
namespace import ::tcl::mathop::+
set result [+ 5 3]


# 숫자가 아닌 값은 인용해야 하며 "eq"와 같은 연산자를 사용하여 연산을 문자열 비교로 제한할 수 있습니다.
set name Neo
expr {{Bob} eq $name}

# 일반 연산자는 숫자 연산이 불가능한 경우 문자열 비교로 대체됩니다.
expr {{Bob} == $name}


# "proc"은 새 루틴을 만듭니다.
proc greet name {
    return "Hello, $name!"
}

# 여러 매개변수를 지정할 수 있습니다.
proc greet {greeting name} {
    return "$greeting, $name!"
}


# 앞에서 언급했듯이 중괄호는 코드 블록을 구성하지 않습니다. "proc"에 대한 세 번째 인수를 포함하여 모든 값은 문자열입니다. 이전 명령은 중괄호 없이 다시 작성할 수 있습니다.
proc greet greeting\ name return\ \"\$greeting,\ \$name!\"
# "



# 마지막 매개변수가 리터럴 값 "args"이면 루틴에 전달된 모든 추가 인수가 목록으로 수집되어 "args"에 할당됩니다.
proc fold {cmd first args} {
    foreach arg $args {
        set first [$cmd $first $arg]
    }
    return $first
}
fold ::tcl::mathop::* 5 3 3 ;# ->  45


# 조건부 실행은 루틴으로 구현됩니다.
if {3 > 4} {
    puts {This will never happen}
} elseif {4 > 4} {
    puts {This will also never happen}
} else {
    puts {This will always happen}
}


# 루프는 루틴으로 구현됩니다. "for"에 대한 첫 번째 및 세 번째 인수는 스크립트로 처리되는 반면 두 번째 인수는 표현식으로 처리됩니다.
set res 0
for {set i 0} {$i < 10} {incr i} {
    set res [expr {$res + $i}]
}
unset res


# "while"에 대한 첫 번째 인수도 표현식으로 처리됩니다.
set i 0
while {$i < 10} {
    incr i 2
}


# 목록은 문자열이며 목록의 항목은 공백으로 구분됩니다.
set amounts 10\ 33\ 18
set amount [lindex $amounts 1]

# 목록 항목의 공백은 인용해야 합니다.
set inventory {"item 1" item\ 2 {item 3}}


# 목록을 수정할 때 목록 루틴을 사용하는 것이 일반적으로 더 좋습니다.
lappend inventory {item 1} {item 2} {item 3}


# 중괄호와 백슬래시를 사용하여 목록에서 더 복잡한 값을 서식 지정할 수 있습니다. 목록은 개행 문자와 세미콜론 문자가 특수 의미를 잃고 스크립트나 변수 대입이 없다는 점을 제외하고는 스크립트와 똑같이 보입니다. 다음 목록에는 세 개의 항목이 있습니다.
set values {

    one\ two

    {three four}

    five\{six

}


# 모든 값과 마찬가지로 목록은 문자열이므로 목록의 서식을 손상시킬 위험이 있으므로 문자열 연산을 수행할 수 있습니다.
set values {one two three four}
set values [string map {two \{} $values] ;# $values는 더 이상 제대로 서식이 지정된 목록이 아닙니다.


# 제대로 서식이 지정된 목록을 얻는 확실한 방법은 "list" 루틴을 사용하는 것입니다.
set values [list one \{ three four]
lappend values { } ;# 목록에 단일 공백을 항목으로 추가


# "eval"을 사용하여 값을 스크립트로 평가합니다.
eval {
    set name Neo
    set greeting "Hello, $name"
}


# 목록은 항상 단일 명령으로 구성된 스크립트로 "eval"에 전달될 수 있습니다.
eval {set name Neo}
eval [list set greeting "Hello, $name"]


# 따라서 "eval"을 사용할 때 "list"를 사용하여 원하는 명령을 구성하십시오.
set command {set name}
lappend command {Archibald Sorbisol}
eval $command


# 일반적인 실수는 명령을 구성할 때 목록 함수를 사용하지 않는 것입니다.
set command {set name}
append command { Archibald Sorbisol}
try {
    eval $command ;# 여기 오류는 {set name Archibald Sorbisol}에서 "set"에 대한 인수가 너무 많다는 것입니다.
} on error {result eoptions} {
    puts [list {received an error} $result]
}

# 이 실수는 "subst"에서도 쉽게 발생할 수 있습니다.

set replacement {Archibald Sorbisol}
set command {set name $replacement}
set command [subst $command]
try {
    eval $command ;# 이전과 동일한 오류: {set name Archibald Sorbisol}에서 "set"에 대한 인수가 너무 많습니다.
} trap {TCL WRONGARGS} {result options} {
    puts [list {received another error} $result]
}


# "list"는 대입을 위해 값을 올바르게 서식 지정합니다.
set replacement [list {Archibald Sorbisol}]
set command {set name $replacement}
set command [subst $command]
eval $command


# "list"는 스크립트로 대입하기 위해 값을 서식 지정하는 데 일반적으로 사용됩니다. 아래에 이에 대한 몇 가지 예가 있습니다.


# "apply"는 두 항목 목록을 루틴으로 평가합니다.
set cmd {{greeting name} {
    return "$greeting, $name!"
}}
apply $cmd Whaddup Neo

# 세 번째 항목을 사용하여 루틴을 적용할 네임스페이스를 지정할 수 있습니다.
set cmd [list {greeting name} {
    return "$greeting, $name!"
} [namespace current]]
apply $cmd Whaddup Neo


# "uplevel"은 호출 스택의 상위 수준에서 스크립트를 평가합니다.
proc greet {} {
    uplevel {puts "$greeting, $name"}
}

proc set_double {varname value} {
    if {[string is double $value]} {
        uplevel [list variable $varname $value]
    } else {
        error [list {not a double} $value]
    }
}


# "upvar"는 호출 스택의 현재 수준에 있는 변수를 상위 수준의 변수에 연결합니다.
proc set_double {varname value} {
    if {[string is double $value]} {
        upvar 1 $varname var
        set var $value
    } else {
        error [list {not a double} $value]
    }
}


# 내장 "while" 루틴을 제거하고 "proc"을 사용하여 새 루틴을 정의합니다.
rename ::while {}
# 처리는 연습 문제로 남겨둡니다.
proc while {condition script} {
    if {[uplevel 1 [list expr $condition]]} {
        uplevel 1 $script
        tailcall [namespace which while] $condition $script
    }
}


# "coroutine"은 새 호출 스택, 해당 호출 스택에 들어갈 새 루틴을 만들고 해당 루틴을 호출합니다. "yield"는 해당 스택에서 평가를 일시 중단하고 호출 스택으로 제어를 반환합니다.
proc countdown count {
    # 코루틴 생성자에게 무언가를 다시 보내고, 당분간 이 호출 스택을 일시 중지합니다.
    yield [info coroutine]

    while {$count > 1} {
        yield [incr count -1]
    }
    return 0
}
coroutine countdown1 countdown 3
coroutine countdown2 countdown 5
puts [countdown1] ;# -> 2
puts [countdown2] ;# -> 4
puts [countdown1] ;# -> 1
puts [countdown1] ;# -> 0
catch {
    puts [countdown1] ;# -> invalid command name "countdown1"
} cres copts
puts $cres
puts [countdown2] ;# -> 3


# 코루틴 스택은 서로 제어를 양보할 수 있습니다.

proc pass {whom args} {
    return [yieldto $whom {*}$args]
}

coroutine a apply {{} {
        yield
        set result [pass b {please pass the salt}]
        puts [list got the $result]
        set result [pass b {please pass the pepper}]
        puts [list got the $result]
}}

coroutine b apply {{} {
    set request [yield]
    while 1 {
        set response [pass c $request]
        puts [list [info coroutine] is now yielding]
        set request [pass a $response]
    }
}}

coroutine c apply {{} {
    set request [yield]
    while 1 {
        if {[string match *salt* $request]} {
            set request [pass b salt]
        } else {
            set request [pass b huh?]
        }
    }
}}

# 움직이기
a
```

## 참고 자료

[공식 Tcl 문서](https://www.tcl-lang.org)

[Tcl 위키](https://wiki.tcl-lang.org)

[Tcl 서브레딧](http://www.reddit.com/r/Tcl)
