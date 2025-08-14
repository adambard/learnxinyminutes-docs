# make.md (번역)

---
category: tool
name: Make
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Stephan Fuhrmann", "https://github.com/sfuhrm"]
filename: Makefile
---

Makefile은 대상(또는 대상들)을 생성하기 위한 규칙 그래프를 정의합니다.
그 목적은 대상을 소스의 최신 버전으로 업데이트하는 데 필요한 최소한의 작업을
수행하는 것입니다. 1976년 Stuart Feldman이 주말 동안 작성한 것으로 유명하며,
많은 경쟁자와 비판에도 불구하고 (특히 Unix 및 Linux에서) 여전히 널리 사용됩니다.

존재하는 make에는 여러 종류가 있지만, 이 글에서는 Linux의 표준인
GNU make를 사용한다고 가정합니다.

```make
# 주석은 이렇게 작성할 수 있습니다.

# 파일 이름은 Makefile이어야 하며, `make <target>`으로 실행할 수 있습니다.
# 그렇지 않으면 `make -f "filename" <target>`을 사용합니다.

# 경고 - Makefile에서는 들여쓰기에 탭만 사용하고 공백은 절대 사용하지 마십시오!

#-----------------------------------------------------------------------
# 기본
#-----------------------------------------------------------------------

# 규칙은 다음과 같은 형식입니다.
# target: <prerequisite>
# 여기서 prerequisite는 선택 사항입니다.

# 규칙 - 이 규칙은 file0.txt가 존재하지 않을 경우에만 실행됩니다.
file0.txt:
	echo "foo" > file0.txt
	# 이 '레시피' 섹션의 주석도 셸로 전달됩니다.
	# `make file0.txt` 또는 단순히 `make`를 시도해보십시오 - 첫 번째 규칙이 기본값입니다.

# 이 규칙은 file0.txt가 file1.txt보다 최신일 경우에만 실행됩니다.
file1.txt: file0.txt
	cat file0.txt > file1.txt
	# 셸에서와 동일한 인용 규칙을 사용합니다.
	@cat file0.txt >> file1.txt
	# @는 명령이 stdout에 에코되는 것을 방지합니다.
	-@echo 'hello'
	# -는 오류가 발생하더라도 make가 계속 진행됨을 의미합니다.
	# 명령줄에서 `make file1.txt`를 시도해보십시오.

# 규칙은 여러 대상과 여러 전제 조건을 가질 수 있습니다.
file2.txt file3.txt: file0.txt file1.txt
	touch file2.txt
	touch file3.txt

# make는 동일한 규칙에 대한 여러 레시피에 대해 불평합니다.
# 하지만 빈 레시피는 계산되지 않으며 새 종속성을 추가하는 데 사용할 수 있습니다.

#-----------------------------------------------------------------------
# 포니 타겟
#-----------------------------------------------------------------------

# 포니 타겟. 파일이 아닌 모든 타겟.
# 절대 최신 상태가 아니므로 make는 항상 실행하려고 시도합니다.
all: maker process

# 순서에 상관없이 선언할 수 있습니다.
maker:
	touch ex0.txt ex1.txt

# 실제 파일과 이름이 같을 때 포니 규칙이 깨지는 것을 방지할 수 있습니다.
.PHONY: all maker process
# 이것은 특별한 타겟입니다. 다른 여러 가지가 있습니다.

# 포니 타겟에 대한 종속성이 있는 규칙은 항상 실행됩니다.
ex0.txt ex1.txt: maker

# 일반적인 포니 타겟은 all make clean install ... 입니다.

#-----------------------------------------------------------------------
# 자동 변수 및 와일드카드
#-----------------------------------------------------------------------

process: file*.txt	#파일 이름과 일치하도록 와일드카드 사용
	@echo $^	# $^는 전제 조건 목록을 포함하는 변수입니다.
	@echo $@	# 타겟 이름을 출력합니다.
	#(여러 타겟 규칙의 경우, $@는 규칙을 실행하게 한 것입니다.)
	@echo $<	# 나열된 첫 번째 전제 조건
	@echo $?	# 최신이 아닌 종속성만
	@echo $+	# 중복을 포함한 모든 종속성 (일반과 다름)
	#@echo $|	# 모든 '순서 전용' 전제 조건

# 규칙 종속성 정의를 분리하더라도 $^는 그것들을 찾습니다.
process: ex1.txt file0.txt
# ex1.txt는 발견되지만 file0.txt는 중복 제거됩니다.

#-----------------------------------------------------------------------
# 패턴
#-----------------------------------------------------------------------

# 특정 파일을 다른 파일로 변환하는 방법을 make에 가르칠 수 있습니다.

%.png: %.svg
	inkscape --export-png $^

# 패턴 규칙은 make가 타겟을 생성하기로 결정한 경우에만 작동합니다.

# 디렉토리 경로는 일반적으로 패턴 규칙을 일치시킬 때 무시됩니다. 하지만
# make는 사용 가능한 가장 적절한 규칙을 사용하려고 시도합니다.
small/%.png: %.svg
	inkscape --export-png --export-dpi 30 $^

# make는 찾은 패턴 규칙의 마지막 버전을 사용합니다.
%.png: %.svg
	@echo this rule is chosen

# 그러나 make는 타겟을 만들 수 있는 첫 번째 패턴 규칙을 사용합니다.
%.png: %.ps
	@echo this rule is not chosen if *.svg and *.ps are both present

# make에는 이미 일부 패턴 규칙이 내장되어 있습니다. 예를 들어,
# *.c 파일을 *.o 파일로 변환하는 방법을 알고 있습니다.

# 이전 Makefile은 패턴 규칙 대신 접미사 규칙을 사용할 수 있습니다.
.png.ps:
	@echo this rule is similar to a pattern rule.

# make에 접미사 규칙에 대해 알립니다.
.SUFFIXES: .png

#-----------------------------------------------------------------------
# 변수
#-----------------------------------------------------------------------
# aka. 매크로

# 변수는 기본적으로 모두 문자열 타입입니다.

name = Ted
name2="Sarah"

echo:
	@echo $(name)
	@echo ${name2}
	@echo $name    # 이것은 작동하지 않으며, $(n)ame으로 처리됩니다.
	@echo $(name3) # 알 수 없는 변수는 빈 문자열로 처리됩니다.

# 변수를 설정하는 4가지 방법이 있습니다.
# 우선 순위가 높은 순서대로:
# 1: 명령줄 인수
# 2: Makefile
# 3: 셸 환경 변수 - make는 이것들을 자동으로 가져옵니다.
# 4: make에는 일부 미리 정의된 변수가 있습니다.

name4 ?= Jean
# 환경 변수가 아직 정의되지 않은 경우에만 변수를 설정합니다.

override name5 = David
# 명령줄 인수가 이 변수를 변경하는 것을 중지합니다.

name4 +=grey
# 변수에 값 추가 (공백 포함).

# 패턴별 변수 값 (GNU 확장).
echo: name2 = Sara # 일치하는 규칙 내에서 참
	# 그리고 재귀적으로 재구성된 종속성 내에서도
	# (그래프가 너무 복잡해지면 깨질 수 있음!)

# make에 의해 자동으로 정의된 일부 변수.
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

# 첫 번째 유형의 변수는 사용될 때마다 평가됩니다.
# 이것은 비용이 많이 들 수 있으므로 한 번만 평가되는 두 번째 유형의
# 변수가 존재합니다. (이것은 GNU make 확장입니다)

var := hello
var2 ::= $(var) hello
#:=와 ::=는 동일합니다.

# 이러한 변수는 절차적으로 평가되므로 (나타나는 순서대로),
# 언어의 나머지 부분과 충돌합니다!

# 이것은 작동하지 않습니다.
var3 ::= $(var4) and good luck
var4 ::= good night

#-----------------------------------------------------------------------
# 함수
#-----------------------------------------------------------------------

# make에는 많은 함수가 있습니다.

sourcefiles = $(wildcard *.c */*.c)
objectfiles = $(patsubst %.c,%.o,$(sourcefiles))

# 형식은 $(func arg0,arg1,arg2...)입니다.

# 일부 예제
ls:	* src/*
	@echo $(filter %.txt, $^)
	@echo $(notdir $^)
	@echo $(join $(dir $^),$(notdir $^))

#-----------------------------------------------------------------------
# 지시문
#-----------------------------------------------------------------------

# 다른 makefile 포함, 플랫폼별 코드에 유용합니다.
include foo.mk

sport = tennis
# 조건부 컴파일
report:
ifeq ($(sport),tennis)
	@echo 'game, set, match'
else
	@echo "They think it's all over; it is now"
endif

# ifneq, ifdef, ifndef도 있습니다.

foo = true

ifdef $(foo)
bar = 'hello'
endif
```

### 더 많은 자료

- [GNU Make 문서](https://www.gnu.org/software/make/manual/make.html)
- [Software Carpentry 튜토리얼](https://swcarpentry.github.io/make-novice/)
- [예제로 배우는 Makefile 튜토리얼](https://makefiletutorial.com/#makefile-cookbook)
