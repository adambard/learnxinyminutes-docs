---
category: tool
name: Make
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Stephan Fuhrmann", "https://github.com/sfuhrm"]
filename: Makefile
translators:
    - ["Wooseok Yang","https://github.com/Huansock"]
---

Makefile은 타겟을 생성하기 위한 규칙 그래프를 정의합니다. 그 목적은 최소한의 노력으로 타겟을 최신 버전으로 갱신하는 것입니다. 1976년 스튜어트 펠드먼(Stuart Feldman)이 주말에 이 프로그램을 작성한 것으로 유명하며, 많은 경쟁자와 비판에도 불구하고 특히 유닉스와 리눅스에서 여전히 널리 사용되고 있습니다.

많은 종류의 make 프로그램이 존재하지만 이 문서에서는 리눅스의 표준인 GNU make를 사용합니다.

```make
# 주석은 이렇게 작성될 수 있습니다.

# 파일 이름은 항상 Makefile이어야 합니다. 그리고 그 파일은 'make <target>'처럼 실행될 수 있습니다.
# 아니면 `make -f "filename" <target>` 명령어를 사용할 수도 있습니다.

# 주의 - Makefile에서 여백을 줄 때 들여쓰기(TAB)를 사용하세요! 공백 문자(space)를 사용하면 안 됩니다.

#-----------------------------------------------------------------------
# 기본
#-----------------------------------------------------------------------


# 규칙은 다음과 같은 형식을 띱니다.
# 타겟: <의존성 목록>
# 여기서 전제 조건은 선택 사항입니다.

# 규칙 - 이 규칙은 file0.txt 파일이 존재하지 않을 때만 실행됩니다.
file0.txt:
	echo "foo" > file0.txt
	# 이러한 '레시피' 섹션의 주석도 쉘로 전달됩니다.
	# 'make file0.txt' 또는 단순히 'make' 명령어를 터미널에서 시도하십시오. 첫 번째 규칙이 명령어가 실행하는 기본값입니다.

# 이 규칙은 file0.txt가 file1.txt보다 최신 버전일 때만 실행됩니다.
file1.txt: file0.txt
	cat file0.txt > file1.txt
	# 쉘에서와 동일한 인용 규칙을 사용합니다.
	@cat file0.txt >> file1.txt	
	# @는 명령이 stdout에 에코되는 것을 중지합니다.
	-@echo 'hello'
	# - 오류가 발생한 경우 make가 계속 실행될 것임을 의미합니다.

# 하나의 규칙은 여러 개의 타겟과 여러 개의 의존성 목록을 가질 수 있습니다.
file2.txt file3.txt: file0.txt file1.txt
	touch file2.txt
	touch file3.txt


# Make 프로그램은 동일한 규칙에 여러 개의 레시피가 있으면 불평합니다. 비어 있는
# 레시피는 예외입니다. 그리고 비어 있는 레시피는 새 종속성을 추가하는 데 사용될 수 있습니다.

#-----------------------------------------------------------------------
# 가짜(Phony) 타겟
#-----------------------------------------------------------------------

# 파일이 아닌 타겟은 가짜 타겟입니다. 
# 가짜 타겟은 최신 상태일 수가 없기 때문에 make 명령어는 항상 이를 실행합니다.
all: maker process

# 규칙 순서가 일반적인 함수 선언 순서에 맞지 않아도 우리는 파일 속 어디에나 규칙을 선언할 수 있습니다.
maker:
	touch ex0.txt ex1.txt

# 가짜 타겟의 이름이 실제 파일 이름일 때 실행되지 않는 오류를 피하기 위해
.PHONY: all maker process
# 이와 같은 특별한 타겟을 사용합니다. 특별한 타겟은 이뿐만이 아니라 여러 개 더 있습니다.

# 가짜 타겟을 종속성으로 가지는 타겟은 항상 실행됩니다.
ex0.txt ex1.txt: maker

# 일반적으로 쓰이는 가짜 타겟은 다음과 같습니다: all, make, clean, install 등

#-----------------------------------------------------------------------
# 자동 변수 & 와일드카드
#-----------------------------------------------------------------------

process: file*.txt	# file로 시작하는 파일 이름과 일치하도록 와일드카드를 사용합니다.
	@echo $^	# '$^'는 '현재 타겟의 모든 의존성 목록'입니다.
	@echo $@	# '$@'는 '현재 타겟 이름'입니다.
	#(만약 여러 개의 타겟이 한 규칙에 묶여 있으면, 실제 실행되는 그 타겟 이름)
	@echo $<	# '$<'는 '가장 왼쪽에 있는 첫번째 의존성'입니다.
	@echo $?	# '$?'는 '오래된 의존성 목록'입니다.
	@echo $+	# '$+'는 '중복을 포함한 모든 의존성 목록'입니다.
	#@echo $|	# '$|'는 '순서 전용 의존성 목록'입니다.

# 한 규칙의 의존성을 여러 군데 나누어 작성해도 $^는 모든 의존성들을 찾습니다.
process: ex1.txt file0.txt
# ex1.txt will be found but file0.txt will be deduplicated.
# ex1.txt는 찾아집니다. file0.txt도 찾아지지만 중복되기 때문에 한 번만 출력됩니다.

#-----------------------------------------------------------------------
# 패턴
#-----------------------------------------------------------------------

# 우리는 make 프로그램이 어떤 파일을 어떻게 변환할지 가르칠 수 있습니다.

%.png: %.svg
	inkscape --export-png $^

# 패턴은 항상 실행되지는 않고 make 프로그램이 해당 타겟을 만들 때만 실행될 것입니다.

# 폴더 경로는 일반적으로 패턴 규칙과 일치할 때 무시됩니다. 하지만
# make는 사용 가능한 가장 적절한 규칙을 사용하려고 시도합니다.
small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

# make는 찾은 패턴 규칙에 대해 마지막 규칙을 사용합니다.
%.png: %.svg
	@echo this rule is chosen

# 그러나 make는 타겟을 만들 수 있는 첫 번째 패턴 규칙을 사용합니다.
%.png: %.ps
	@echo this rule is not chosen if *.svg and *.ps are both present

# make에는 이미 몇 가지 패턴 규칙이 내장되어 있습니다. 예를 들어
# *.c 파일을 *.o 파일로 변환하는 방법이 있습니다.

# 오래된 메이크파일은 패턴 규칙 대신 접미사 규칙을 사용할 수 있습니다.
.png.ps:
	@echo this rule is similar to a pattern rule.

# Tell make about the suffix rule
# make에게 접미사 규칙을 알려줄 수 있습니다.
.SUFFIXES: .png

#-----------------------------------------------------------------------
# 변수
#-----------------------------------------------------------------------
# 일명 매크로

# 명령어는 기본적으로 모두 문자열 타입입니다.

name = Ted
name2="Sarah"

echo:
	@echo $(name)
	@echo ${name2}
	@echo $name    # 이건 작동하지 않습니다. $(n)ame처럼 처리됩니다.
	@echo $(name3) # 알 수 없는 변수는 빈 문자열로 처리됩니다.

# 변수를 설정하는 곳은 4가지입니다.
# 우선순위 (높은 것부터 낮은 것까지):
# 1: 명령줄 인수
# 2: Makefile
# 3: 쉘 환경 변수. make 명령어는 이를 자동으로 가져옵니다.
# 4: make에는 미리 정의 된 변수가 있습니다.

name4 ?= Jean
# 환경 변수가 아직 정의되지 않은 경우에만 변수를 설정합니다.

override name5 = David
# 명령줄 인수가 이 변수를 변경하지 못하도록 합니다.

name4 +=grey
# 변수에 값을 추가합니다(공백 포함).

# 패턴 전용 변수 값(GNU 확장).
echo: name2 = Sara # 이 변수 값은 해당 타겟을 빌드할 때 
	# 실행되는 레시피 안에서만 쓰이는 게 아니라,
	# 그 타겟이 다시 빌드를 유도하는 하위 규칙(sub-rules, prerequisites)에도 전달됩니다.


# 일부 변수는 make의 의해 자동으로 정의됩니다.
echo_inbuilt:
	echo $(CC)
	echo ${CXX}
	echo $(FC)
	echo ${CFLAGS}
	echo $(CPPFLAGS)
	echo ${CXXFLAGS}
	echo $(LDFLAGS)
	echo ${LDLIBS}

#-----------------------------------------------------------------------
# 변수 2
#-----------------------------------------------------------------------

# The first type of variables are evaluated each time they are used.
# This can be expensive, so a second type of variable exists which is
# only evaluated once. (This is a GNU make extension)

# 첫 번째 유형의 변수는 각 변수가 사용될 때 값이 결정됩니다.
# 이는 많은 자원을 소모할 수 있습니다. 그래서 두 번째 유형의 변수는 한 번만 값이 결정됩니다.
# (이는 GNU make 확장입니다.)

var := hello
var2 ::= $(var) hello
#:=과 ::=는 똑같습니다.

# 이러한 변수들은 절차적으로 (나타나는 순서대로) 평가됩니다. 
# 이는 make 언어의 나머지 부분(target 정의 등)과는 다릅니다!

# 이건 작동하지 않습니다.
var3 ::= $(var4) and good luck
var4 ::= good night

#-----------------------------------------------------------------------
# 함수
#-----------------------------------------------------------------------

# make에서 많은 함수를 사용할 수 있습니다.

sourcefiles = $(wildcard *.c */*.c)
objectfiles = $(patsubst %.c,%.o,$(sourcefiles))

# 형식은 $(func arg0,arg1,arg2...) 입니다.

# 몇 가지 예시
ls:	* src/*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# 지시문
#-----------------------------------------------------------------------

# 다른 Makefile을 포함할 수 있습니다. 플랫폼별 코드를 다룰 때 유용합니다.
include foo.mk

sport = tennis
# 조건부 컴파일
report:
ifeq ($(sport),tennis)
	@echo 'game, set, match'
else
	@echo "They think it's all over; it is now"
endif

# ifneq, ifdef, ifndef도 존재합니다.

foo = true

ifdef $(foo)
bar = 'hello'
endif
```

### 추가 자료

- [GNU Make 공식 문서](https://www.gnu.org/software/make/manual/make.html)
- [Software Carpentry 튜토리얼](https://swcarpentry.github.io/make-novice/)
- [예제별 Makefile 튜토리얼](https://makefiletutorial.com/#makefile-cookbook)
