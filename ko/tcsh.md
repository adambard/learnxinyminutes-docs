# tcsh.md (번역)

---
name: tcsh
filename: LearnTCSH.csh
contributors:
       - ["Nicholas Christopoulos", "https://github.com/nereusx"]
---

tcsh("tee-see-shell")는 C 셸(csh)을 기반으로 하고 호환되는 유닉스 셸입니다.
본질적으로 프로그래밍 가능한 명령줄 완성, 명령줄 편집 및 몇 가지 다른 기능이 있는 C 셸입니다.
FreeBSD와 같은 BSD 기반 시스템의 기본 루트 셸입니다.

오늘날 거의 모든 Linux 배포판과 BSD는 원래 csh 대신 tcsh를 사용합니다. 대부분의 경우 csh는 tcsh를 가리키는 심볼릭 링크입니다.
이는 tcsh가 csh와 하위 호환되고 후자는 더 이상 유지 관리되지 않기 때문입니다.

- [TCSH 홈페이지](http://www.tcsh.org/)
- [TCSH 위키백과](https://en.wikipedia.org/wiki/Tcsh)
- [TCSH 매뉴얼 페이지](http://www.tcsh.org/tcsh.html/top.html)
- ["C 셸 소개", William Joy](https://docs.freebsd.org/44doc/usd/04.csh/paper.html)
- [TCSH 버그 리포트 및/또는 기능 요청](https://bugs.gw.com/)

추가 파일:
[tcsh 도움말 명령어 (132x35 터미널 크기용)](https://github.com/nereusx/dotfiles/blob/master/csh-help),
[나의 ~/.tcshrc](https://github.com/nereusx/dotfiles/blob/master/.tcshrc)

```tcsh
#!/bin/tcsh
# 스크립트의 첫 번째 줄은 시스템에 스크립트 실행 방법을 알려주는 shebang입니다.
# http://en.wikipedia.org/wiki/Shebang_(Unix)
# TCSH는 shebang을 이해하지 못하는 시스템에서 이를 에뮬레이트합니다.

# 대부분의 경우 `#!/bin/tcsh -f`를 사용합니다. `-f` 옵션은
# 리소스나 시작 파일을 로드하지 않고, 명령어 해싱을 수행하지 않으므로
# 더 빨리 시작됩니다.

# --- echo 명령어 --------------------------------------------------------
# `echo`는 각 단어를 셸의 표준 출력에 공백으로 구분하고
# 줄 바꿈으로 종료하여 씁니다. echo_style 셸 변수는
# 플래그와 이스케이프 시퀀스를 에뮬레이트(또는 에뮬레이트하지 않도록)하도록 설정할 수 있습니다.

# echo_style 값 표시
echo $echo_style

# `echo`가 백슬래시 문자와 `-n` 옵션(줄 바꿈 없음)을 지원하도록 활성화
# 이것은 tcsh의 기본값이지만, 배포판에서 변경할 수 있습니다. Slackware는
# 그렇게 했습니다.
set echo_style = both

# "Hello world" 출력
echo Hello world
echo "Hello world"
echo 'Hello world'
echo `echo Hello world`

# "twonlines"를 한 줄에 출력
echo two\nlines

# 두 줄 출력
echo "two\nlines"
echo 'two\nlines'

# --- 기본 구문 ------------------------------------------------------------

# 특수 문자(공백이나 탭 포함)는 앞에 백슬래시 `\`를 붙여
# 특수 의미를 갖지 않도록 할 수 있습니다.
# 마지막 히스토리 명령어를 표시합니다
echo !!
# 이것은 그렇지 않습니다
echo \!\!

# 작은따옴표도 특수 문자 확장을 방지하지만, `!`나 백슬래시 같은
# 일부 문자는 우선순위가 더 높습니다
# `$` (변수 값)는 확장되지 않습니다
echo '$1 tip'
# `!` (히스토리)는 확장됩니다
echo '!!'

# 백쿼트로 묶인 문자열은 실행되고 그 결과로 대체됩니다.
echo `ls`

# 세미콜론은 명령어를 구분합니다
echo 'first line'; echo 'second line'

# 조건부 실행도 있습니다
echo "Always executed" || echo "Only executed if the first command fails"
echo "Always executed" && echo "Only executed if the first command does NOT fail"

# 괄호로 묶인 명령어는 항상 서브셸에서 실행됩니다,

# 예: 프로젝트를 생성하고 설치하는 동안 완료되었음을 알립니다.
make && ( espeak "BOSS, compilation finished"; make install )

# 홈 디렉토리를 출력하지만 원래 있던 위치에 그대로 둡니다
(cd; pwd); pwd

# tcsh 맨페이지 문서 읽기
man tcsh

# --- 변수 ---------------------------------------------------------------
# 셸은 변수 목록을 유지하며, 각 변수는 0개 이상의 단어로 구성된 목록을 값으로 가집니다.
# 셸 변수의 값은 `set` 및 `unset` 명령어로 표시하고 변경할 수 있습니다.
# 시스템은 자체 "환경" 변수 목록을 유지합니다.
# 이는 `printenv`, `setenv`, `unsetenv`로 표시하고 변경할 수 있습니다.
# `setenv`의 구문은 POSIX sh와 유사합니다.

# 값을 할당하거나 아무것도 할당하지 않으면 변수가 생성됩니다
# 아무것도 할당하지 않음
set var
# 숫자 값 할당
# '@'는 표현식이 산술임을 나타냅니다. 'set'과 유사하게 작동하지만
# 오른쪽 값은 숫자 표현식이 될 수 있습니다.
@ var = 1 + 2
# 문자열 값 할당
set var = "Hello, I am the contents of 'var' variable"
# 프로그램 출력 할당
set var = `ls`

# 변수 제거
unset var
# 변수 `var`가 존재하면 1(참)을 출력하고, 그렇지 않으면 0(거짓)을 출력합니다
echo $?var
# 모든 변수와 그 값을 출력합니다
set

# 'var'의 내용을 출력합니다
echo $var;
echo "$var";
# 문자열 `$var`를 출력합니다
echo \$var
echo '$var'
# 필요할 때 변수를 나머지 부분과 분리하기 위해 중괄호를 사용할 수 있습니다
set num = 12; echo "There ${num}th element"

# 값의 문자 수를 출력합니다: 6
set var = '123456'; echo $%var

### 리스트
# 값 목록 할당
set var = ( one two three four five )
# 모든 요소 출력: one two three four five
echo $var
echo $var[*]
# 요소 개수 출력: 5
echo $#var
# 인덱싱된 요소 출력; 두 번째 요소 출력: two
echo $var[2]
# 요소 범위 출력; 2번째부터 3번째까지 출력: two, three
echo $var[2-3]
# 3번째부터 모든 요소 출력: three four five
echo $var[3-]
# 3번째 요소까지 모두 출력: one two three
echo $var[-3]

### 특수 변수
# $argv         명령줄 인수 목록
# $argv[0]      이 파일 이름 (스크립트 파일의 파일)
# $# $0, $n, $*는 $#argv, $argv[0], $argv[n], $argv[*]와 동일합니다
# $status, $?   마지막으로 실행된 명령어의 종료 코드
# $_            이전 명령줄
# $!            이 셸에서 시작된 마지막 백그라운드 프로세스의 PID
# $$            스크립트의 PID

# $path, $PATH  실행 파일을 찾을 디렉토리 목록
# $home, $HOME  사용자의 홈 디렉토리, `~`를 대신 사용할 수도 있습니다
# $uid          사용자 로그인 ID
# $user         사용자 로그인 이름
# $gid          사용자 그룹 ID
# $group        사용자 그룹 이름
# $cwd, $PWD    현재/작업 디렉토리 출력
# $owd          이전 작업 디렉토리
# $tcsh         tcsh 버전
# $tty          현재 tty; Linux 콘솔의 경우 ttyN, X의 터미널 에뮬레이터의 경우 pts/N
# $term         터미널 유형
# $verbose      설정되면 각 명령어의 단어를 출력합니다.
#               `-v` 명령줄 옵션으로도 설정할 수 있습니다.
# $loginsh      설정되면 로그인 셸입니다

# 팁: $?0은 대화형 셸에서 항상 거짓입니다
# 팁: $?prompt는 비대화형 셸에서 항상 거짓입니다
# 팁: `$?tcsh`가 설정되지 않았다면, 원래 `csh`나 다른 것을 실행하고 있는 것입니다;
#      `echo $shell`을 시도해보세요
# 팁: `$verbose`는 스크립트 디버깅에 유용합니다
# 참고: `$PWD`와 `$PATH`는 `$cwd`와 `$pwd`와 자동으로 동기화됩니다.

# --- 변수 수정자 ------------------------------------------------------
# 구문: ${var}:m[:mN]
# 여기서 <m>은 다음과 같습니다:
# h : 디렉토리  t : 파일 이름  r : 확장자 제거   e : 확장자
# u : 첫 소문자를 대문자로
# l : 첫 대문자를 소문자로
# p : 출력하지만 실행하지는 않음 (hist)
# q : 대체된 단어를 인용하여 추가 대체를 방지
# x : q와 같지만 공백에서 단어로 나눔
# g : 다음 수정자를 각 단어에 한 번 적용
# a  : 다음 수정자를 단일 단어에 가능한 한 많이 적용
# s/l/r/ : `l`을 찾아 `r`로 바꿈, 정규식 아님; `r`의 `&`는 `l`로 대체됨
# & : 이전 대체 반복

# 이 파일로 시작
set f = ~/Documents/Alpha/beta.txt
# ~/Documents/Alpha/beta 출력
echo $f:r
# ~/Documents/Alpha 출력
echo $f:h
# beta.txt 출력
echo $f:t
# txt 출력
echo $f:e
# beta 출력
echo $f:t:r
# Beta 출력
echo $f:t:r:u
# Biota 출력
echo $f:t:r:u:s/eta/iota/

# --- 리다이렉션 -------------------------------------------------------------

# file.txt를 만들고 표준 출력을 씁니다
echo 'this string' > file.txt
# file.txt를 만들고 표준 출력과 표준 에러를 씁니다
echo 'this string' >& file.txt
# 표준 출력을 file.txt에 추가합니다
echo 'this string' >> file.txt
# 표준 출력과 표준 에러를 file.txt에 추가합니다
echo 'this string' >>& file.txt
# file.txt에서 표준 입력을 리다이렉션합니다
cat < file.txt
# 키보드 입력; 입력 줄을 변수 `x`에 저장합니다
set x = $<
# 여기에 문서;
cat << LABEL
...text here...
LABEL

# 팁: 표준 에러를 분리하는 방법은 다음과 같습니다:
(grep 'AGP' /usr/src/linux/Documentation/* > output-file.txt) >& error-file.txt

# 예: 표준 입력에서 이름을 읽고 인사 메시지를 표시합니다
echo -n "Enter your name: "
set name = $<
echo "Greetings $name"

# --- 표현식 ------------------------------------------------------------

# 연산자:
# ==  같음         !=  같지 않음    !  부정
#  >  보다 큼   <  보다 작음   >=  크거나 같음  <= 작거나 같음
# &&  논리 AND   ||  논리 OR

if ( $name != $user ) then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
endif

# 한 줄 형식
if ( $name != $user ) echo "Your name isn't your username"

# 참고: $name이 비어 있으면 tcsh는 위 조건을 다음과 같이 봅니다:
# if ( != $user ) ...
# 이것은 잘못된 구문입니다
# tcsh에서 잠재적으로 비어 있는 변수를 사용하는 "안전한" 방법은 다음과 같습니다:
# if ( "$name" != $user ) ...
# $name이 비어 있을 때 tcsh는 이것을 다음과 같이 봅니다:
# if ( "" != $user ) ...
# 이것은 예상대로 작동합니다

# 조건부 실행도 있습니다
echo "Always executed" || echo "Only executed if the first command fails"
echo "Always executed" && echo "Only executed if the first command does NOT fail"

# if 문에서 &&와 ||를 사용하려면 여러 쌍의
# 대괄호가 필요하지 않습니다:
if ( "$name" == "Steve" && "$age" == 15 ) then
    echo "This will run if $name is Steve AND $age is 15."
endif

if ( "$name" == "Daniya" || "$name" == "Zach" ) then
    echo "This will run if $name is Daniya OR Zach."
endif

# 문자열 일치 연산자 ( `=~` 및 `!~` )
# '==' '!=' '=~' 및 '!~' 연산자는 인수를 문자열로 비교합니다;
# 다른 모든 연산자는 숫자에 대해 작동합니다. '=~' 및 '!~' 연산자는 '!='
# 및 '=='와 같지만 오른쪽이 왼쪽 피연산자와 일치하는
# glob-패턴이라는 점이 다릅니다.

if ( $user =~ ni[ck]* ) echo "Greetings Mr. Nicholas."
if ( $user !~ ni[ck]* ) echo "Hey, get out of Nicholas' PC."

# 산술 표현식은 다음 형식으로 표시됩니다:
@ result = 10 + 5
echo $result

# 산술 연산자
# +, -, *, /, %
#
# 괄호로 묶어야 하는 산술 연산자
# !, ~, |, &, ^, ~, <<, >>,
# 비교 및 논리 연산자
#
# 모든 연산자는 C와 동일합니다.

# 숫자 표현식에 공백이 필요하다는 것은 잘 문서화되어 있지 않습니다;
# 또한, `@`에는 자체 파서가 있으며, 표현식이 괄호로 묶여 있을 때 잘 작동하는 것 같습니다.
# 그렇지 않으면 기본 파서가 활성 상태인 것 같습니다.
# 괄호는 주위에 공백이 필요하며, 이것은 문서화되어 있습니다.

# 잘못됨
@ x = $y+1
@ x = 0644 & 022;      echo $x
@ x = (0644 & 022) +1; echo $x
@ x = (0644 & 022)+ 1; echo $x
@ x = ( ~077 );        echo $x

# 올바름
@ x = $y + 1
@ x = ( 0644 & 022 ) + 1; echo $x
@ x = ( ~ 077 );          echo $x
@ x = ( ~ 077 | 022 );    echo $x
@ x = ( ! 0 );            echo $x

# C의 연산자 ++ 및 --는 할당이 없는 경우 지원됩니다
@ result ++

# 수학을 하기 위해 만들어진 셸은 없습니다;
# 기본 연산을 제외하고 백슬래시와 함께 외부 명령어를 사용하십시오.
#
# 저는 calc를 최상의 옵션으로 제안합니다.
# (http://www.isthe.com/chongo/tech/comp/calc/)
#
# 두 번째 옵션으로 표준 Unix의 bc
# (https://www.gnu.org/software/bc/manual/html_mono/bc.html)
#
# 세 번째 옵션으로 표준 Unix의 AWK
# (https://www.gnu.org/software/gawk/manual/gawk.html)

# `Perl`, `PHP`, `python` 또는 여러 BASIC을 사용할 수도 있지만,
# 더 빠른 로드 및 실행 결과를 위해 위 유틸리티를 선호하십시오.

# 실제 예: (StackExchange에서 답변한 내용)
# 요구사항: x := 1001b OR 0110b

# `tcsh` 표현식에서 (8진수 사용)
@ x = ( 011 | 06 ); echo $x

# `calc`를 사용하여 동일하게 (원래 요구사항대로 이진수 사용)
set x = `calc '0b1001 | 0b110'`; echo $x

# --- 파일 조회 연산자 --------------------------------------------------
# 참고: 내장 `filetest` 명령어는 동일한 작업을 수행합니다.

#### 불리언 연산자
# -r  읽기 접근    -w  쓰기 접근    -x  실행 접근    -e  존재
# -f  일반 파일     -d  디렉토리       -l  심볼릭 링크     -p  명명된 파이프
# -S  소켓 파일
# -o  소유권      -z  크기 0       -s  크기 0 아님
# -u  SUID 설정됨    -g  SGID 설정됨     -k  스티키 비트 설정됨
# -b  블록 장치   -c  문자 장치
# -t  파일(숫자)이 터미널 장치에 대한 열린 파일 디스크립터임

# `README` 파일이 존재하면 메시지를 표시합니다
if ( -e README ) echo "I have already README file"

# `less` 프로그램이 설치되어 있으면 `more` 대신 사용합니다
if ( -e `where less` ) then
    alias more 'less'
endif

#### 비-불리언 연산자
# -Z  파일 크기를 바이트 단위로 반환
# -M  수정 시간(mtime) 반환    -M: mtime 문자열 반환
# -A  마지막 접근 시간(atime) 반환     -A: atime 문자열 반환
# -U  소유자 사용자 ID 반환              -U: 소유자 사용자 이름 반환
# -G  소유자 그룹 ID 반환             -G: 소유자 그룹 이름 반환
# -P  권한을 8진수로 반환  -Pmode는 권한과 모드의 AND 결과 반환

# 이것은 날짜를 유닉스 시간 정수로 표시합니다: 1498511486
filetest -M README.md

# 이것은 "Tue Jun 27 00:11:26 2017"을 표시합니다
filetest -M: README.md

# --- 기본 명령어 ----------------------------------------------------------

# `chdir` (cd)로 파일 시스템 탐색
cd path # 작업 디렉토리 변경
cd      # 홈 디렉토리로 변경
cd -    # 이전 디렉토리로 변경
cd ..   # 한 디렉토리 위로 이동

# 예:
cd ~/Downloads # 나의 `Downloads` 디렉토리로 이동

# `mkdir`를 사용하여 새 디렉토리 생성.
mkdir newdir
# `-p` 플래그는 필요에 따라 새 중간 디렉토리를 생성합니다.
mkdir -p ~/.backup/saves

# which & where
# csh가 tcsh를 가리키는지 찾기
ls -lha `which csh`
# csh가 둘 이상의 디렉토리에 설치되어 있는지 찾기
where csh

# --- 파이프라인 --------------------------------------------------------------
# 파이프라인은 각 프로세스의 출력(stdout)이 다음 프로세스의 입력(stdin)으로
# 직접 공급되도록 표준 스트림으로 함께 연결된 프로세스 시퀀스입니다.
# 이러한 `파이프`는 `|` 특수 문자로 생성되며 유닉스의 가장 강력한
# 특징 중 하나입니다.

# 예:
ls -l | grep key | less
# "ls -l"은 프로세스를 생성하고, 그 출력(stdout)은 "grep key" 프로세스의
# 입력(stdin)으로 파이프됩니다. "less" 프로세스도 마찬가지입니다.

# `ls`, `grep`, `less`는 유닉스 프로그램이며 자체 맨페이지가 있습니다.
# `파이프` 메커니즘은 커널의 일부이지만 구문과 제어는
# 셸의 역할이며, 이 경우 tcsh입니다.

# 참고: Windows에도 `파이프` 메커니즘이 있지만 버그가 있으며, 제가 마지막으로
# 작업했던 Windows XP SP3 API32까지 모든 버전에 대해 서명했습니다.
# Microsoft는 이를 부인했지만, 프로세스 간 통신에 일반적인 방법이기 때문에
# 잘 알려진 버그입니다. 작은 I/O의 경우 잘 작동합니다.
# tcsh는 grep, GCC, Perl과 함께 DOS(EMX DOS 확장기 사용) 및
# 나중에 Windows(1998)로 포팅된 최초의 유닉스 프로그램 중 하나입니다.

# 예: 이것은 tcsh를 PostScript로 변환하고 Okular로 보여줍니다
zcat /usr/man/man1/tcsh.1.gz | groff -Tps -man | okular -

# 더 나은 버전
zcat `locate -b -n 1 '\tcsh.1.gz'` | groff -Tps -man | okular -

# 훨씬 더 나은 버전
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz");
 zcat `eval $loc` | groff -Tps -man | okular -

# 동일, 맨페이지 pdf를 생성하도록 수정
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz");
 zcat `eval $loc` | groff -Tps -man | ps2pdf - ${page}.pdf

# 동일, 이제 ${page}.pdf도 표시
set page = tcsh; set loc = (locate -b -n 1 "\\\\"${page}".1.gz");
 zcat `eval $loc` | groff -Tps -man | ps2pdf - ${page}.pdf && okular tcsh.pdf

# 참고: `okular`는 KDE 환경의 기본 응용 프로그램이며 포스트스크립트 및
# pdf 파일을 표시합니다. 좋아하는 PDF 뷰어로 교체할 수 있습니다.
# `zcat`, `locate`, `groff`는 모든 유닉스에서 일반적인 프로그램입니다. `ps2pdf`
# 프로그램은 널리 사용되는 `ghostscript` 패키지의 일부입니다.

# --- 제어 흐름 ------------------------------------------------------------

#### IF-THEN-ELSE-ENDIF
# 구문:
# if ( expr ) then
#    ...
# [else if ( expr2 ) then
#    ...]
# [else
#    ...]
# endif
#
# 지정된 `expr`이 참이면 첫 번째 else까지의 명령어가 실행됩니다;
# 그렇지 않고 `expr2`가 참이면 두 번째 else까지의 명령어가 실행되는 식입니다.
# else-if 쌍은 얼마든지 가능하며, endif는 하나만 필요합니다.
#
# 한 줄 형식:
#
# if ( expr ) command
#
# `expr`이 참으로 평가되면 명령어가 실행됩니다.
# `command`는 단순 명령어여야 하며, 별칭, 파이프라인, 명령어 목록
#, 또는 괄호로 묶인 명령어 목록이 아니어야 합니다. 간단히 말해 사용을 피하십시오.
#
# 버그: expr이 거짓이고 명령어가 실행되지 않더라도 입/출력 리다이렉션이 발생합니다.
#

# 비대화형 셸인지 확인하고 참이면 종료
if ( $?USER == 0 || $?prompt == 0 ) exit

# 로그인 셸인지 확인
if ( $?loginsh ) then
    # 리눅스 콘솔에 있는지 확인 (X의 터미널 아님)
    if ( $tty =~ tty* ) then
        # 키패드 응용 프로그램 키 활성화 (man console_codes)
        echo '\033='
    endif
endif

#### SWITCH-ENDSW
# 구문:
# switch ( expr )
# case pattern:
#     ...
#     [breaksw]
# [default:
#     ...]
# endsw
#
# tcsh는 C의 switch와 유사하게 작동하는 case 문을 사용합니다.
# 각 case 레이블은 먼저 명령어와 파일 이름이 확장된 지정된 문자열과
# 순차적으로 일치합니다. 파일 메타문자 `*`, `?`, `[...]`를 case 레이블에
# 사용할 수 있습니다. 레이블이 일치하지 않으면 정의된 경우 기본 레이블
# 다음에 실행이 시작됩니다.
# `breaksw` 명령어는 endsw 다음에 실행을 계속하도록 합니다. 그렇지 않으면
# C에서처럼 제어가 case 레이블과 기본 레이블을 통과할 수 있습니다.

switch ( $var )
case *.[1-9]:
case *.[1-9].gz:
    echo "$var is a man-page."
    breaksw
case *gz:
    echo "$var is gzipped"
    breaksw
default:
    file $var
endsw

#### FOREACH-END
# 구문:
# foreach name ( wordlist )
#    ...
#   [break | continue]
# end
#
# 변수 `name`을 `wordlist`의 각 멤버로 순차적으로 설정하고
# 이 명령어와 일치하는 `end` 키워드 사이의 명령어 시퀀스를 실행합니다.
# `continue` 키워드는 맨 위로 돌아가 다음 요소로 점프하고, `break` 키워드는
# 루프를 종료합니다.
#
# 버그: `foreach`는 끝을 찾을 때 here document를 무시하지 않습니다.

# 예: 1에서 10까지 세기
foreach i ( `seq 1 10` )
    echo $i
end

# 예: 목록의 모든 파일 유형
foreach f ( a.txt b.txt c.txt )
    cat $f
end

# 예: wma를 ogg로 변환
foreach f ( *.wma )
    ffmpeg -i "$f" "$f:r".ogg
end

#### WHILE-END
# while ( expr )
#     ...
#     [break | continue]
# end
#
# `expr`이 0이 아닌 값으로 평가되는 동안 `while`과 일치하는 `end` 사이의
# 명령어를 실행합니다. `break`와 `continue`를 사용하여 루프를 조기에
# 종료하거나 계속할 수 있습니다.

# 1에서 10까지 세기
set num = 1
while ( $num <= 10 )
    echo $num
    @ num ++
end

# CWD의 모든 디렉토리 출력
set lst = ( * )
while ( $#lst )
    if ( -d $lst[1] ) echo $lst[1] is directory
    shift lst
end

# 명령줄 인수를 옵션 또는 매개변수로 분리
set options
set params
set lst = ( $* )
while ( $#lst )
    if ( "$lst[1]" =~ '-*' ) then
        set options = ( $options $lst[1] )
    else
        set params = ( $params $lst[1] )
    endif
    shift lst
end
echo 'options =' $options
echo 'parameters =' $params

#### REPEAT
# 구문: repeat count command
#
# 위 한 줄 `if` 문의 명령어와 동일한 제한을 받는 지정된 명령어가
# count 횟수만큼 실행됩니다.
# `count`가 0이더라도 I/O 리다이렉션은 정확히 한 번 발생합니다.
#
# 팁: 대부분의 경우 `while`을 선호하십시오

repeat 3 echo "ding dong"

# --- 함수 ---------------------------------------------------------------
# tcsh에는 함수가 없지만 표현식 구문이 `alias`를 함수로 사용할 만큼
# 충분히 발전했습니다. 다른 방법은 재귀입니다

# 별칭 인수 선택자; 별칭에 제공된 인수를 가져와 참조하는 명령어에
# 적용하는 기능을 정의하는 기능입니다.
# Tcsh는 이 기능을 제공하는 유일한 셸입니다.
#
# \!#   별칭/명령어 자체를 포함한 모든 인수에 대한 인수 선택자;
#       인수를 제공할 필요는 없습니다.
# \!*   별칭/명령어를 제외한 모든 인수에 대한 인수 선택자;
#       인수를 제공할 필요는 없습니다.
# \!$   마지막 인수에 대한 인수 선택자; 인수를 제공할 필요는 없지만,
#       아무것도 제공되지 않으면 별칭 이름이 마지막 인수로 간주됩니다.
# \!^   첫 번째 인수에 대한 인수 선택자; 인수를 반드시 제공해야 합니다.
# \!:n  n번째 인수에 대한 인수 선택자; 인수를 반드시 제공해야 합니다;
#       n=0은 별칭/명령어 이름을 나타냅니다.
# \!:m-n   m번째부터 n번째까지의 인수에 대한 인수 선택자;
#       인수를 반드시 제공해야 합니다.
# \!:n-$   n번째부터 마지막까지의 인수에 대한 인수 선택자;
#       적어도 n번째 인수는 반드시 제공해야 합니다.

# 디렉토리를 변경할 때 내용이 즉시 표시되도록 cd 명령어에 별칭을 지정합니다.
alias cd 'cd \!* && ls'

# --- 재귀 방식 --- 시작 ---
#!/bin/tcsh -f
set todo = option1
if ( $#argv > 0 ) then
    set todo = $argv[1]
endif

switch ( $todo )
case option1:
#    ...
    $0 results
    breaksw
case option2:
#    ...
    $0 results
    breaksw
case results:
    echo "print the results here"
#    ...
    breaksw
default:
    echo "Unknown option: $todo"
#    exit 0
endsw
# --- 재귀 방식 --- 끝 ---

# --- 예제 ----------------------------------------------------------------

# 이 스크립트는 인수가 설정되지 않은 경우 사용 가능한 전원 상태를 출력합니다;
# 그렇지 않으면 $argv[1]의 상태를 설정합니다
# --- 전원 상태 스크립트 --- 시작 --------------------------------------------
#!/bin/tcsh -f
# 매개변수 가져오기 (없으면 "help")
set todo = help
if ( $#argv > 0 ) then
    set todo = $argv[1]
endif
# 사용 가능한 옵션
set opts = `cat /sys/power/state`
# 알려져 있습니까?
foreach o ( $opts )
    if ( $todo == $o ) then
        # 찾음; 실행
        echo -n $todo > /sys/power/state
        break
    endif
end
# 도움말 출력 및 종료
echo "usage: $0 [option]"
echo "available options on kernel: $opts"
# --- 전원 상태 스크립트 --- 끝 ----------------------------------------------

# 비밀 숫자 맞추기 게임
# --- secretnum.csh --- 시작 -------------------------------------------------
#!/bin/tcsh -f
set secret=`shuf -i1-100 -n1`
echo "I have a secret number from 1 up to 100"
while ( 1 )
    echo -n "Guess: "
    set guess = $<
    if ( $secret == $guess ) then
        echo "You found it"
        exit 1
    else
        if ( $secret > $guess ) then
            echo "its greater"
        else if ( $secret < $guess ) then
                echo "its lesser"
            endif
        endif
    endif
end
# --- secretnum.csh --- 끝 ---------------------------------------------------

# -----------------------------------------------------------------------------
# 부록

#### [T]CSH에 대하여:
# * CSH는 버그로 악명이 높습니다;
# * 또한 고급 대화형 모드로도 유명합니다.
# * TCSH는 가장 진보된 완성 하위 시스템을 갖춘 것으로 유명합니다.
# * TCSH는 가장 진보된 별칭 하위 시스템을 갖춘 것으로 유명합니다; 별칭은
#   매개변수를 사용할 수 있으며 종종 함수로 사용될 수 있습니다!
# * TCSH는 더 나은 구문 때문에 사람들에게 잘 알려져 있고 선호됩니다 (저도 마찬가지).
#   모든 셸은 [t]csh, fish, plan9의 셸(rc, ex)을 제외하고 Thomson의 구문을 사용합니다.
# * bash, zsh, 심지어 mksh보다 작고 메모리를 훨씬 적게 소비합니다!
#   (memusage 보고서)
# * TCSH에는 여전히 버그가 있습니다. 더 적지만 있습니다. 읽기 쉬운 깨끗한 코드를 작성하면
#   아무것도 찾을 수 없을 것입니다. 글쎄, 거의 없을 것입니다... 이것은 csh의 구현과
#   관련이 있습니다. 다른 셸의 구현이 좋다는 의미는 아닙니다.
# * 잘 알려진 셸 중 정규 프로그래밍이 가능한 셸은 없습니다. 스크립트가
#   커지면 Python, PHP 또는 Perl과 같은 프로그래밍 언어를 사용하십시오 (좋은
#   스크립팅 언어).
#
# 조언:
# 1. 한 줄 IF에서 리다이렉션을 사용하지 마십시오 (잘 알려진 버그입니다)
#    대부분의 경우 한 줄 IF 사용을 피하십시오.
# 2. 다른 셸의 코드를 망치지 마십시오. c-shell은 다른 셸과 호환되지 않으며
#    다른 기능과 우선순위를 가지고 있습니다.
# 3. 어떤 언어로든 읽기 쉬운 코드를 작성하는 것처럼 공백을 사용하십시오.
#    csh의 버그는 `set x=1`과 `set x = 1`은 작동했지만 `set x =1`은 작동하지 않았습니다!
# 4. 숫자 표현식 사이에 공백이 필요하다는 것은 잘 문서화되어 있습니다;
#    또한 모든 비트 및 단항 연산자를 괄호로 묶으십시오.
# 5. 여러 따옴표, 백슬래시 등으로 거대하고 이상한 표현식을 작성하지 마십시오.
#    일반 프로그래밍에 좋지 않은 습관이며 어떤 셸에서든 위험합니다.
# 6. tcsh를 도와주세요, 여기에 버그를 보고하세요 <https://bugs.gw.com/>
# 7. 맨페이지를 읽으십시오. `tcsh`에는 엄청난 수의 옵션과 변수가 있습니다.
#
#    기본적으로 다음 옵션을 활성화하는 것이 좋습니다
#    --------------------------------------------------
# 비대화형 셸에서도
#    set echo_style=both
#    set backslash_quote
#    set parseoctal
#    unset noclobber
#
# 무엇이든...
#    set inputmode=insert
#    set autolist
#    set listjobs
#    set padhour
#    set color
#    set colorcat
#    set nobeep
#    set cdtohome
#
#    set histdup
#    set histlit
#    set nohistclop
#
#    unset compat_expr
#    unset noglob
#    unset autologout
#    unset time
#    unset tperiod
#
# 참고: `backslash_quote`가 설정되면, 그것 없이 작성된 다른 tcsh 스크립트와
# 호환성 문제가 발생할 수 있습니다.
#
# 참고: `parseoctal`도 마찬가지이지만, 문제가 있는 스크립트를 수정하는 것이
# 더 좋습니다.
#
# 참고: **초보자 전용**
# 필요한 경우 `path` 디렉토리를 자동으로 다시 스캔할 수 있습니다. (bash처럼)
#    set autorehash

#### 일반적인 별칭
#    alias hist  'history 20'
#    alias ll    'ls --color -lha'
#    alias today "date '+%d%h%y'
#    alias ff    'find . -name '

#### 멋진 프롬프트
#    set prompt = "%B%{\033[35m%}%t %{\033[32m%}%n@%m%b %C4 %# "
```
