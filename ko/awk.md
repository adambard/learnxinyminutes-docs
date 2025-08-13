---
category: tool
name: AWK
filename: learnawk.awk
contributors:
     - ["Marshall Mason", "http://github.com/marshallmason"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

AWK는 모든 POSIX 호환 UNIX 시스템의 표준 도구입니다. 명령줄에서 사용하는 flex/lex와 같으며, 텍스트 처리 작업 및 기타 스크립팅 요구에 적합합니다. C와 유사한 구문을 가지고 있지만, 필수 세미콜론(단, 한 줄짜리 스크립트를 작성할 때는 필요하며, AWK는 이 점에서 뛰어납니다), 수동 메모리 관리 또는 정적 타이핑이 없습니다. 텍스트 처리에 탁월합니다. 셸 스크립트에서 호출하거나 독립 실행형 스크립팅 언어로 사용할 수 있습니다.

Perl 대신 AWK를 사용하는 이유는 무엇일까요? 가독성입니다. AWK는 Perl보다 읽기 쉽습니다. 간단한 텍스트 처리 스크립트, 특히 파일을 한 줄씩 읽고 구분 기호로 분할하는 스크립트의 경우 AWK가 적합한 도구일 것입니다.

```awk
#!/usr/bin/awk -f

# 주석은 이렇습니다


# AWK 프로그램은 패턴과 액션의 모음으로 구성됩니다.
pattern1 { action; } # lex와 같습니다
pattern2 { action; } #

# 암시적인 루프가 있으며 AWK는 제공된 각 파일의 각 레코드를 자동으로 읽고 구문 분석합니다. 각 레코드는 기본적으로 공백(여러 공백, 탭은 하나로 계산됨)인 FS 구분 기호로 분할됩니다.
# 명령줄(-F C) 또는 BEGIN 패턴에서 FS를 할당할 수 있습니다.

# 특수 패턴 중 하나는 BEGIN입니다. BEGIN 패턴은 파일을 읽기 전에 참입니다. END 패턴은 마지막 파일(또는 파일이 지정되지 않은 경우 표준 입력)의 파일 끝 이후에 참입니다.
# 또한 할당할 수 있는 출력 필드 구분 기호(OFS)가 있으며, 기본값은 단일 공백입니다.

BEGIN {

    # BEGIN은 프로그램 시작 시 실행됩니다. 텍스트 파일을 처리하기 전에 모든 예비 설정 코드를 넣는 곳입니다. 텍스트 파일이 없는 경우 BEGIN을 기본 진입점으로 생각하십시오.

    # 변수는 전역입니다. 선언할 필요 없이 설정하거나 사용하십시오.
    count = 0;

    # C 및 친구들과 같은 연산자
    a = count + 1;
    b = count - 1;
    c = count * 1;
    d = count / 1; # 정수 나눗셈
    e = count % 1; # 나머지
    f = count ^ 1; # 거듭제곱

    a += 1;
    b -= 1;
    c *= 1;
    d /= 1;
    e %= 1;
    f ^= 1;

    # 1씩 증가 및 감소
    a++;
    b--;

    # 접두사 연산자로 사용하면 증가된 값을 반환합니다.
    ++a;
    --b;

    # 또한 문장을 끝내는 세미콜론과 같은 구두점이 없습니다.

    # 제어문
    if (count == 0)
        print "Starting with count of 0";
    else
        print "Huh?";

    # 또는 삼항 연산자를 사용할 수 있습니다.
    print (count == 0) ? "Starting with count of 0" : "Huh?";

    # 여러 줄로 구성된 블록은 중괄호를 사용합니다.
    while (a < 10) {
        print "String concatenation is done" " with a series" " of"
            " space-separated strings";
        print a;

        a++;
    }

    for (i = 0; i < 10; i++)
        print "Good ol' for loop";

    # 비교에 관해서는 표준입니다:
    # a < b   # 미만
    # a <= b  # 이하
    # a != b  # 같지 않음
    # a == b  # 같음
    # a > b   # 초과
    # a >= b  # 이상

    # 논리 연산자도 있습니다.
    # a && b  # AND
    # a || b  # OR

    # 또한 매우 유용한 정규식 일치가 있습니다.
    if ("foo" ~ "^fo+$")
        print "Fooey!";
    if ("boo" !~ "^fo+$")
        print "Boo!";

    # 배열
    arr[0] = "foo";
    arr[1] = "bar";

    # 내장 함수 split()으로 배열을 초기화할 수도 있습니다.

    n = split("foo:bar:baz", arr, ":");

    # 연관 배열도 있습니다(실제로 모두 연관 배열입니다).
    assoc["foo"] = "bar";
    assoc["bar"] = "baz";

    # 그리고 다차원 배열도 있습니다. 여기서는 언급하지 않을 몇 가지 제한 사항이 있습니다.
    multidim[0,0] = "foo";
    multidim[0,1] = "bar";
    multidim[1,0] = "baz";
    multidim[1,1] = "boo";

    # 배열 멤버십을 테스트할 수 있습니다.
    if ("foo" in assoc)
        print "Fooey!";

    # 'in' 연산자를 사용하여 배열의 키를 순회할 수도 있습니다.
    for (key in assoc)
        print assoc[key];

    # 명령줄은 ARGV라는 특수 배열에 있습니다.
    for (argnum in ARGV)
        print ARGV[argnum];

    # 배열의 요소를 제거할 수 있습니다.
    # 이것은 AWK가 인수를 처리할 파일로 가정하는 것을 방지하는 데 특히 유용합니다.
    delete ARGV[1];

    # 명령줄 인수 수는 ARGC라는 변수에 있습니다.
    print ARGC;

    # AWK에는 여러 내장 함수가 있습니다. 세 가지 범주로 나뉩니다. 나중에 정의된 자체 함수에서 각각을 시연하겠습니다.

    return_value = arithmetic_functions(a, b, c);
    string_functions();
    io_functions();
}

# 함수를 정의하는 방법은 다음과 같습니다.
function arithmetic_functions(a, b, c,     d) {

    # 아마도 AWK에서 가장 성가신 부분은 지역 변수가 없다는 것입니다.
    # 모든 것이 전역입니다. 짧은 스크립트의 경우 이것은 괜찮고 유용하기까지 하지만, 긴 스크립트의 경우 문제가 될 수 있습니다.

    # 해결 방법(해킹)이 있습니다. 함수 인수는 함수에 로컬이며, AWK는 필요한 것보다 더 많은 함수 인수를 정의할 수 있습니다. 따라서 위에서 했던 것처럼 함수 선언에 지역 변수를 넣으십시오. 관례적으로 실제 함수 매개변수와 지역 변수를 구별하기 위해 약간의 추가 공백을 넣으십시오. 이 예에서 a, b, c는 실제 매개변수이고 d는 단지 지역 변수입니다.

    # 이제 산술 함수를 시연하겠습니다.

    # 대부분의 AWK 구현에는 몇 가지 표준 삼각 함수가 있습니다.
    d = sin(a);
    d = cos(a);
    d = atan2(b, a); # b / a의 아크탄젠트

    # 그리고 로그 관련 항목
    d = exp(a);
    d = log(a);

    # 제곱근
    d = sqrt(a);

    # 부동 소수점을 정수로 자르기
    d = int(5.34); # d => 5

    # 난수
    srand(); # 인수로 시드를 제공합니다. 기본적으로 현재 시간을 사용합니다.
    d = rand(); # 0과 1 사이의 난수.

    # 값을 반환하는 방법은 다음과 같습니다.
    return d;
}

function string_functions(    localvar, arr) {

    # AWK는 문자열 처리 언어이므로 여러 문자열 관련 함수가 있으며, 그 중 다수는 정규식에 크게 의존합니다.

    # 검색 및 바꾸기, 첫 번째 인스턴스(sub) 또는 모든 인스턴스(gsub)
    # 둘 다 바뀐 일치 항목 수를 반환합니다.
    localvar = "fooooobar";
    sub("fo+", "Meet me at the ", localvar); # localvar => "Meet me at the bar"
    gsub("e", ".", localvar); # localvar => "M..t m. at th. bar"

    # 정규식과 일치하는 문자열 검색
    # index()는 동일한 작업을 수행하지만 정규식을 허용하지 않습니다.
    match(localvar, "t"); # => 4, 't'가 네 번째 문자이므로

    # 구분 기호로 분할
    n = split("foo-bar-baz", arr, "-");
    # 결과: a[1] = "foo"; a[2] = "bar"; a[3] = "baz"; n = 3

    # 기타 유용한 항목
    sprintf("%s %d %d %d", "Testing", 1, 2, 3); # => "Testing 1 2 3"
    substr("foobar", 2, 3); # => "oob"
    substr("foobar", 4); # => "bar"
    length("foo"); # => 3
    tolower("FOO"); # => "foo"
    toupper("foo"); # => "FOO"
}

function io_functions(    localvar) {

    # 이미 print를 보셨습니다.
    print "Hello world";

    # printf도 있습니다.
    printf("%s %d %d %d\n", "Testing", 1, 2, 3);

    # AWK에는 파일 핸들이 없습니다. 파일 핸들이 필요한 것을 사용할 때 자동으로 파일 핸들을 엽니다. 이를 위해 사용한 문자열은 I/O 목적으로 파일 핸들로 처리될 수 있습니다. 이것은 셸 스크립팅과 비슷하게 느껴지지만, 동일한 출력을 얻으려면 문자열이 정확히 일치해야 하므로 변수를 사용하십시오:

    outfile = "/tmp/foobar.txt";

    print "foobar" > outfile;

    # 이제 문자열 outfile은 파일 핸들입니다. 닫을 수 있습니다:
    close(outfile);

    # 셸에서 무언가를 실행하는 방법은 다음과 같습니다.
    system("echo foobar"); # => foobar 인쇄

    # 표준 입력에서 한 줄을 읽고 localvar에 저장합니다.
    getline localvar;

    # 파이프에서 한 줄을 읽습니다(다시 말하지만, 제대로 닫으려면 문자열을 사용하십시오).
    cmd = "echo foobar";
    cmd | getline localvar; # localvar => "foobar"
    close(cmd);

    # 파일에서 한 줄을 읽고 localvar에 저장합니다.
    infile = "/tmp/foobar.txt";
    getline localvar < infile;
    close(infile);
}

# 처음에 말했듯이 AWK 프로그램은 패턴과 액션의 모음으로 구성됩니다. 이미 BEGIN 패턴을 보셨습니다. 다른 패턴은 파일이나 표준 입력에서 줄을 처리하는 경우에만 사용됩니다.
#
# AWK에 인수를 전달하면 처리할 파일 이름으로 처리됩니다.
# 순서대로 모두 처리합니다. 암시적인 for 루프처럼 생각하십시오.
# 이러한 파일의 줄을 반복합니다. 이러한 패턴과 액션은 루프 내부의 스위치 문과 같습니다.

/^fo+bar$/ {

    # 이 액션은 정규식 /^fo+bar$/와 일치하는 모든 줄에 대해 실행되고, 일치하지 않는 줄은 건너뜁니다. 줄을 인쇄해 보겠습니다:

    print;

    # 인수가 없습니다! print에는 기본 인수인 $0이 있기 때문입니다.
    # $0은 현재 처리 중인 줄의 이름입니다. 자동으로 생성됩니다.

    # 다른 $ 변수가 있다고 추측할 수 있습니다. 모든 줄은 모든 액션이 호출되기 전에 암시적으로 분할됩니다. 셸과 마찬가지로 말이죠. 그리고 셸과 마찬가지로 각 필드는 달러 기호로 접근할 수 있습니다.

    # 이것은 줄의 두 번째와 네 번째 필드를 인쇄합니다.
    print $2, $4;

    # AWK는 각 줄을 검사하고 처리하는 데 도움이 되는 많은 다른 변수를 자동으로 정의합니다. 가장 중요한 것은 NF입니다.

    # 이 줄의 필드 수를 인쇄합니다.
    print NF;

    # 이 줄의 마지막 필드를 인쇄합니다.
    print $NF;
}

# 모든 패턴은 실제로 참/거짓 테스트입니다. 마지막 패턴의 정규식도 참/거짓 테스트이지만, 일부는 숨겨져 있습니다. 테스트할 문자열을 제공하지 않으면 현재 처리 중인 줄인 $0을 가정합니다. 따라서 완전한 버전은 다음과 같습니다:

$0 ~ /^fo+bar$/ {
    print "Equivalent to the last pattern";
}

a > 0 {
    # 이것은 a가 양수인 한 각 줄에 대해 한 번 실행됩니다.
}

# 이제 이해하셨을 겁니다. 텍스트 파일을 처리하고, 한 번에 한 줄씩 읽고, 그것으로 무언가를 하는 것, 특히 구분 기호로 분할하는 것은 UNIX에서 매우 일반적이므로 AWK는 묻지 않고도 모든 것을 수행하는 스크립팅 언어입니다. 입력에 대해 예상하는 것과 그것으로 무엇을 하고 싶은지에 따라 패턴과 액션을 작성하기만 하면 됩니다.

# 다음은 간단한 스크립트의 빠른 예입니다. AWK가 완벽하게 적합한 종류의 것입니다. 표준 입력에서 이름을 읽은 다음 해당 이름의 모든 사람의 평균 연령을 인쇄합니다. 이 데이터 파일의 이름을 인수로 제공한다고 가정해 보겠습니다:
#
# Bob Jones 32
# Jane Doe 22
# Steve Stevens 83
# Bob Smith 29
# Bob Barker 72
#
# 스크립트는 다음과 같습니다:

BEGIN {

    # 먼저 사용자에게 이름을 묻습니다.
    print "What name would you like the average age for?";

    # 명령줄의 파일이 아닌 표준 입력에서 한 줄을 가져옵니다.
    getline name < "/dev/stdin";
}

# 이제 첫 번째 필드가 주어진 이름인 모든 줄을 일치시킵니다.
$1 == name {

    # 여기서는 이미 미리 로드된 여러 유용한 변수에 접근할 수 있습니다:
    # $0은 전체 줄입니다.
    # $3은 세 번째 필드, 즉 우리가 관심 있는 나이입니다.
    # NF는 필드 수이며, 3이어야 합니다.
    # NR은 지금까지 본 레코드(줄) 수입니다.
    # FILENAME은 처리 중인 파일의 이름입니다.
    # FS는 사용 중인 필드 구분 기호이며, 여기서는 " "입니다.
    # ...등등. 맨 페이지에 문서화된 더 많은 것이 있습니다.

    # 실행 중인 총계와 일치하는 줄 수를 추적합니다.
    sum += $3;
    nlines++;
}

# 또 다른 특수 패턴은 END라고 합니다. 모든 텍스트 파일을 처리한 후에 실행됩니다. BEGIN과 달리 입력을 처리하도록 지정한 경우에만 실행됩니다. 모든 파일이 제공된 규칙 및 액션에 따라 읽고 처리된 후에 실행됩니다. 그 목적은 일반적으로 일종의 최종 보고서를 출력하거나 스크립트 과정에서 축적한 데이터의 집계로 무언가를 하는 것입니다.

END {
    if (nlines)
        print "The average age for " name " is " sum / nlines;
}
```

더 읽을거리:

* [Awk 튜토리얼](http://www.grymoire.com/Unix/Awk.html)
* [Awk 맨 페이지](https://linux.die.net/man/1/awk)
* [GNU Awk 사용자 가이드](https://www.gnu.org/software/gawk/manual/gawk.html)
  GNU Awk는 대부분의 Linux 시스템에서 찾을 수 있습니다.
* [AWK 한 줄 모음](http://tuxgraphics.org/~guido/scripts/awk-one-liner.html)
* [Awk alpinelinux 위키](https://wiki.alpinelinux.org/wiki/Awk) 기술적
  요약 및 "함정" 목록(다른 구현이
  다르거나 예기치 않은 방식으로 동작할 수 있는 곳).
* [awk를 위한 기본 라이브러리](https://github.com/dubiousjim/awkenough)