---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
    - ["Jonathan Wang", "https://github.com/Jonathansw"]   
    - ["Leo Rudberg", "https://github.com/LOZORD"]
    - ["Betsy Lorton", "https://github.com/schbetsy"]
    - ["John Detter", "https://github.com/jdetter"]
translators:
    - ["Wooseop Kim", "https://github.com/linterpreteur"]
filename: LearnBash-kr.sh
lang: ko-kr
---

Bash는 유닉스 셸의 이름이며, 리눅스와 맥 OS X의 기본 셸로 그리고 GNU 운영체제를 위한 셸로서 배포되었습니다.
이하의 거의 모든 예시들은 셸 스크립트의 일부이거나 셸에서 바로 실행할 수 있습니다.

[(영어) 이곳에서 더 알아보세요.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# 스크립트의 첫 줄은 시스템에게 스크립트의 실행법을 알려주는 '셔뱅'입니다.
# https://ko.wikipedia.org/wiki/%EC%85%94%EB%B1%85
# 이미 보았듯이 주석은 #으로 시작합니다. 셔뱅 또한 주석입니다.

# 간단한 헬로 월드
echo 헬로 월드!

# 각각의 명령어는 개행 혹은 세미콜론 이후에 시작됩니다.
echo '첫번째 줄'; echo '두번째 줄'

# 변수 선언은 다음과 같습니다.
Variable="어떤 문자열"

# 하지만 다음은 틀린 형태입니다.
Variable = "어떤 문자열"
# Bash는 Variable이 실행해야 하는 명령어라고 판단할 것이고, 해당 명령어를 찾을
# 수 없기 때문에 에러를 발생시킬 것입니다.

# 다음도 같습니다.
Variable= '어떤 문자열'
# Bash는 '어떤 문자열'이 실행해야 하는 명령어라고 판단하여 에러를 발생시킬 것입니다.
# (이 경우에 'Variable=' 부분은 '어떤 문자열' 명령어의 스코프에서만 유효한
# 변수 할당으로 해석됩니다.)

# 변수 사용은 다음과 같습니다.
echo $Variable
echo "$Variable"
echo '$Variable'
# 할당, 내보내기 등 변수 자체를 사용할 때에는 $ 없이 이름을 적습니다.
# 변수의 값을 사용할 때에는 $를 사용해야 합니다.
# 작은 따옴표는 변수를 확장시키지 않는다는 사실에 주의하세요.
# (역자 주: '$Variable'은 변수 Variable의 값이 아닌 문자열 "$Variable"입니다.)

# 인수 확장은 ${ }입니다.
echo ${Variable}
# 이는 인수 확장의 간단한 예시입니다.
# 인수 확장은 변수로부터 값을 받아 그 값을 "확장"하거나 출력합니다.
# 확장을 통해 인수나 그 값이 변경될 수 있습니다.
# 이하는 확장에 대한 다른 예시들입니다.

# 변수에서의 문자열 치환
echo ${Variable/Some/A}
# 처음으로 나타나는 "Some"를 "A"로 치환합니다.

# 변수의 부분열
Length=7
echo ${Variable:0:Length}
# 변수 값에서 처음 7개 문자만을 반환합니다.

# 변수의 기본값
echo ${Foo:-"Foo가_없거나_비어_있을_때의_기본값"}
# null(Foo=) 값이나 빈 문자열(Foo="")일 경우에만 작동합니다. 0은 (Foo=0)은 0입니다.
# 기본값을 반환할 뿐 변수 값을 변경하지는 않는다는 사실에 주목하세요.

# 중괄호 확장 { }
# 임의의 문자열을 생성합니다.
echo {1..10}
echo {a..z}
# 시작 값으로부터 끝 값까지의 범위를 출력합니다.

# 내장 변수
# 유용한 내장 변수들이 있습니다.
echo "마지막 프로그램의 반환값: $?"
echo "스크립트의 PID: $$"
echo "스크립트에 넘겨진 인자의 개수: $#"
echo "스크립트에 넘겨진 모든 인자: $@"
echo "각각 변수로 쪼개진 스크립트 인자: $1 $2..."

# echo와 변수의 사용법을 알게 되었으니,
# bash의 기초를 조금 더 배워봅시다!

# 현재 디렉토리는 `pwd` 명령어로 알 수 있습니다.
# `pwd`는 "print working directory(작업 디렉토리 출력)"의 약자입니다.
# 내장 변수`$PWD`를 사용할 수도 있습니다.
# 이하는 모두 동일합니다.
echo "I'm in $(pwd)" # `pwd`를 실행하여 문자열에 보간
echo "I'm in $PWD" # 변수를 보간

# 터미널이나 결과의 출력물이 너무 많다면
# 명령어 `clear`를 이용해 화면을 지울 수 있습니다.
clear
# 컨트롤+L 또한 화면을 지울 수 있습니다.

# 입력 값 읽기
echo "이름이 뭐에요?"
read Name # 변수 선언이 필요 없다는 데 주목하세요.
echo $Name님, 안녕하세요!

# 평범한 if 구조도 있습니다.
# 'man test'로 조건문에 대해 더 알아보세요.
if [ $Name != $USER ]
then
    echo "사용자가 아닙니다."
else
    echo "사용자입니다."
fi

# $Name이 비어 있다면, bash는 위의 조건을 다음과 같이 인식합니다.
if [ != $USER ]
# 이는 문법적으로 유효하지 않습니다.
# 따라서 bash에서 비어 있을 수 있는 변수를 "안전하게" 사용하는 법은 다음과 같습니다.
if [ "$Name" != $USER ] ...
# $Name이 비어 있다면 bash는
if [ "" != $USER ] ...
# 와 같이 인식하여 예상한 대로 동작합니다.

# 조건부 실행도 있습니다.
echo "항상 실행" || echo "첫 명령어가 실패해야 실행"
echo "항상 실행" && echo "첫 명령어가 실패하지 않아야 실행"

# if문과 함께 &&와 ||을 사용하려면, 대괄호가 여러 쌍 필요합니다.
if [ "$Name" == "철수" ] && [ "$Age" -eq 15 ]
then
    echo "$Name이 철수이고 $Age가 15일 때 실행"
fi

if [ "$Name" == "민희" ] || [ "$Name" == "상민" ]
then
    echo "$Name이 민희이거나 상민일 때 실행"
fi

# 표현식은 다음 형식으로 표기됩니다.
echo $(( 10 + 5 ))

# 다른 프로그래밍 언어와는 달리, bash는 셸이기 때문에 현재 디렉토리의 컨텍스트에서
# 실행됩니다. 현재 디렉토리의 파일과 디렉토리를 ls 명령어로 나열할 수 있습니다.
ls

# 다음은 실행을 제어하는 옵션의 예시입니다.
ls -l # 모든 파일과 디렉토리를 분리된 줄에 나열
ls -t # 디렉토리 내용을 마지막으로 수정된 날짜(내림차순)에 따라 정렬
ls -R # 이 디렉토리와 그 안의 모든 디렉토리에 대해 재귀적으로 `ls` 실행

# 이전 명령어의 결과는 다음 명령어에 입력될 수 있습니다.
# grep 명령어는 입력을 주어진 패턴에 따라 필터링합니다. 다음은 현재 디렉토리의
# .txt 파일을 나열하는 방법입니다.
ls -l | grep "\.txt"

# `cat`을 이용해 stdout으로 파일을 출력합니다.
cat file.txt

# `cat`으로 파일을 읽을 수도 있습니다.
Contents=$(cat file.txt)
echo "파일 시작\n$Contents\n파일 끝"

# `cp`를 이용해 파일이나 디렉토리를 다른 곳으로 복사할 수 있습니다.
# `cp`는 원본의 새로운 버전을 생성하므로 사본을 편집하는 것은
# 원본에 영향을 주지 않으며 그 반대도 마찬가지입니다.
# 목표 위치에 이미 파일이 있다면 덮어쓰게 됩니다.
cp srcFile.txt clone.txt
cp -r srcDirectory/ dst/ # 재귀적으로 복사

# 컴퓨터 간에 파일을 공유하려고 한다면 `scp` 혹은 `sftp`를 사용합니다.
# `scp`는 `cp`와 매우 유사하게 동작하며
# `sftp`는 더 상호작용적입니다.

# `mv`로 파일 혹은 디렉토리를 다른 곳으로 이동합니다.
# `mv`는 `cp`와 유사하지만 원본을 삭제합니다.
# 또한 `mv`로 파일의 이름을 바꿀 수도 있습니다.
mv s0urc3.txt dst.txt # sorry, l33t hackers...

# bash는 현재 디렉토리의 컨텍스트에서 실행되기 때문에, 다른 디렉토리에서 명령어를
# 실행하고 싶으실 수 있습니다. cd를 이용해 위치를 변경합니다.
cd ~    # 홈 디렉토리로 변경
cd ..   # 한 디렉토리 위로 이동
        # (즉 /home/username/Downloads에서 /home/username로)
cd /home/username/Documents   # 특정 디렉토리로 이동
cd ~/Documents/..    # 아직도 홈 디렉토리... 아닌가??

# 서브셸로 디렉토리를 넘어서 작업할 수도 있습니다.
(echo "처음엔 여기 $PWD") && (cd 어딘가; echo "이제는 여기 $PWD")
pwd # 아직도 첫 디렉토리에 있음

# `mkdir`로 새 디렉토리를 만듭니다.
mkdir myNewDir
# `-p` 플래그는 필요하다면 해당 디렉토리의 경로 중간에 있는 디렉토리를 생성합니다.
mkdir -p myNewDir/with/intermediate/directories

# (stdin, stdout, stderr로) 명령어의 입출력을 리디렉션할 수 있습니다.
# stdin의 내용을 ^EOF$까지 읽고 hello.py에 그 내용을 덮어씁니다.
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# stdin, stdoutk, stderr을 다양한 방법으로 리디렉션하여 hello.py를 실행합니다.
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# 출력 오류는 이미 파일이 있을 경우 덮어쓰지만,
# 덮어쓰는 대신에 내용에 추가하고 싶다면 ">>"를 사용합니다.
python hello.py >> "output.out" 2>> "error.err"

# output.out에 덮어쓰고, error.err에 추가하고, 줄을 세기
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# 명령어를 실행하고 그 파일 디스크립터를 출력 (예: /dev/fd/123)
# man fd 참고
echo <(echo "#helloworld")

# output.out을 "#helloworld"으로 덮어쓰기
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# 임시 파일을 지울 수 있습니다. ('-i'로 대화식 실행)
# 경고: `rm` 명령어는 되돌릴 수 없습니다.
rm -v output.out error.err output-and-error.log
rm -r tempDir/ # 재귀적으로 삭제

# 다른 명령어에서 $()을 이용해 명령어를 치환할 수도 있습니다.
# 다음 명령어는 현재 디렉토리의 파일 및 디렉토리의 수를 표시합니다.
echo "$(ls | wc -l)개 항목이 있습니다."

# 백틱(``)을 이용할 수도 있지만 이 방식을 이용하면 중첩할 수 없기 때문에
# $()을 사용하는 것이 더 좋습니다.
echo "`ls | wc -l`개 항목이 있습니다."

# 자바나 C++의 switch와 비슷하게 동작하는 case 문을 사용할 수 있습니다.
case "$Variable" in
    # 충족시킬 조건을 나열
    0) echo "0입니다.";;
    1) echo "1입니다.";;
    *) echo "널이 아닌 값입니다.";;
esac

# for 반복문은 주어진 인자만큼 반복합니다.
# 다음은 $Variable을 세 번 출력합니다.
for Variable in {1..3}
do
    echo "$Variable"
done

# 혹은 "전통적인 for 반복문" 방식을 쓸 수도 있습니다.
for ((a=1; a <= 3; a++))
do
    echo $a
done

# 파일에도 적용될 수 있습니다.
# 다음은 file1과 file2에 'cat' 명령어를 실행합니다.
for Variable in file1 file2
do
    cat "$Variable"
done

# 혹은 명령어의 결과에도 이용할 수 있습니다.
# 다음은 ls의 결과를 cat합니다.
for Output in $(ls)
do
    cat "$Output"
done

# while 반복문
while [ true ]
do
    echo "반복문 몸체"
    break
done

# 함수를 정의할 수도 있습니다.
# 정의:
function foo ()
{
    echo "인자는 함수 인자처럼 작동합니다. $@"
    echo "그리고 $1 $2..."
    echo "함수입니다."
    return 0
}

# 혹은 단순하게
bar ()
{
    echo "함수를 선언하는 다른 방법"
    return 0
}

# 함수 호출
foo "My name is" $Name

# 몇 가지 유용한 명령어를 알아두면 좋습니다.
# file.txt의 마지막 10줄 출력
tail -n 10 file.txt
# file.txt의 첫 10줄 출력
head -n 10 file.txt
# file.txt 줄 별로 정렬
sort file.txt
# 중복되는 줄을 생략하거나 -d를 이용하여 보고
uniq -d file.txt
# ',' 문자 이전의 첫 열만 출력
cut -d ',' -f 1 file.txt
# file.txt에서 'okay'를 모두 'great'로 교체 (정규식 호환)
sed -i 's/okay/great/g' file.txt
# file.txt에서 정규식에 맞는 모든 줄을 stdin에 출력
# 다음 예시는 "foo"로 시작해 "bar"로 끝나는 줄 출력
grep "^foo.*bar$" file.txt
# "-c" 옵션을 넘겨 줄 번호를 대신 출력
grep -c "^foo.*bar$" file.txt
# 다른 유용한 옵션
grep -r "^foo.*bar$" someDir/ # 재귀적으로 `grep`
grep -n "^foo.*bar$" file.txt # 줄 번호 매기기
grep -rI "^foo.*bar$" someDir/ # 재귀적으로 `grep`하되 바이너리 파일은 무시
# 같은 검색으로 시작하여 "baz"를 포함하는 줄만 필터
grep "^foo.*bar$" file.txt | grep -v "baz"

# 정규식이 아니라 문자열로 검색하고 싶다면
# fgrep 혹은 grep -F
fgrep "foobar" file.txt

# trap 명령어로 스크립트에서 신호를 받을 때 명령어를 실행할 수 있습니다.
# 다음 명령어는 셋 중 한 가지 신호를 받으면 rm 명령어를 실행합니다.
trap "rm $TEMP_FILE; exit" SIGHUP SIGINT SIGTERM

# `sudo`를 통해 슈퍼이용자로 명령어를 실행합니다.
NAME1=$(whoami)
NAME2=$(sudo whoami)
echo "$NAME1였다가 더 강한 $NAME2가 되었다"

# 'help' 명령어로 내장 문서를 읽을 수 있습니다.
help
help help
help for
help return
help source
help .

# man으로 매뉴얼을 읽을 수도 있습니다.
apropos bash
man 1 bash
man bash

# info 명령어로 문서를 읽습니다. (?로 도움말)
apropos info | grep '^info.*('
man info
info info
info 5 info

# bash의 info 문서를 읽어 보세요.
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
