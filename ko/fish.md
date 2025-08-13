---
name: fish
contributors:
    - ["MySurmise", "https://github.com/MySurmise"]
    - ["Geo Maciolek", "https://github.com/GeoffMaciolek"]
filename: learn.fish
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Fish (**f**riendly **i**nteractive **sh**ell)는 이국적인 셸의 이름입니다. 이는 Bourne-Shell이나 C-Shell에서 파생되지 않은 구문을 가진 셸입니다.

fish의 장점은 최신 셸에서 원하는 많은 기능이 기본적으로 제공되므로 zsh 및 oh-my-zsh와 같은 추가 소프트웨어를 설치할 필요가 없다는 것입니다.

이러한 기능의 예로는 자동 제안, 24비트 색상, Man 페이지 완성(즉, fish가 man 페이지를 자동으로 구문 분석하고 명령에 대한 추가 옵션을 제안함) 또는 웹 페이지를 통해 옵션을 만드는 기능(GUI가 설치된 경우)이 있습니다.

2005년 2월에 출시되었습니다.

- [더 읽기](https://fishshell.com/docs/current/language.html)
- [설치 가이드](https://github.com/fish-shell/fish-shell#getting-fish)


## 가이드

최신 fish 셸이 있는지 확인하십시오. 이것은 버전 3.3.0으로 만들어졌습니다. 테스트하려면 다음을 입력하십시오:

```
> fish -v
```

fish 셸을 시작하려면 다음을 입력하십시오:

```
> fish
```

종료하려면 다음을 입력하십시오:

```
> exit
```

또는 <kbd>Ctrl + D</kbd>를 누르십시오.

이제 바로 fish에서 한 가지 성가신 점이 있습니다. 환영 메시지입니다. 누가 필요하겠습니까, 그렇죠? 셸이 시작되면 다음을 입력하십시오:

```
> set -U fish_greeting ""
```

bash로 작성된 단일 명령을 해당 셸로 전환하지 않고 실행하려면 다음을 입력할 수 있습니다:

```
> bash -c 'echo "fish is better than bash"'
```

fish에서는 작은따옴표나 큰따옴표를 사용할 수 있습니다.
이스케이프 문자는 `\`입니다.

구성 파일을 편집하여 fish 구성을 변경할 수 있습니다.

```
> vim ~/.config/fish/config.fish
```

또는 앞서 언급한 웹 설정을 엽니다:

```
> fish_config
```

fish PATH 변수에 무언가를 추가하는 것은 쉽습니다:

```
> fish_add_path ~/cowsay
```

bash로 그렇게 할 수 있습니까? 아니요, 항상 찾아봐야 합니다... 그냥 그렇게 쉽습니다!

하지만 더 있습니다. 대부분의 fish 관련 명령은 예상대로 'fish'로 시작합니다. `fish`를 입력하고 <kbd>TAB</kbd>를 누르십시오. 그러면 fish의 멋진 기능 중 하나인 **그냥 작동하는** 자동 완성이 있습니다.
이제 <kbd>TAB</kbd>, <kbd>Shift + TAB</kbd> 및 화살표 키 <kbd>←</kbd><kbd>↑</kbd><kbd>→</kbd><kbd>↓</kbd>로 탐색할 수 있습니다.

도움이 필요하면 지역 정신과 의사에게 연락하거나 `man`을 입력하십시오. 그러면 해당 명령에 대한 설명서가 표시됩니다. 예를 들어:

```
> man set
```

마침내 fish를 시도했다면, fish에서 정말 멋진 다른 것을 볼 수 있습니다. 모든 것이 멋진 색상을 가지고 있고, 잘못 입력하면 실행하지 않아도 빨간색으로 표시되고, 따옴표 안에 무언가를 넣으면 어디서 끝나는지, 왜 그 따옴표가 작동하지 않는지 볼 수 있습니다. 왜냐하면 따옴표 안에 26번째 위치에 다른 따옴표가 있기 때문입니다.

fish에는 와일드카드와 같은 더 멋진 것들이 있습니다.
예를 들어, 다음을 입력하십시오.

```
> ls *.fish
```

그러면 현재 디렉토리의 모든 fish 파일이 나열됩니다.

명령당 여러 와일드카드를 가질 수 있으며, 재귀 와일드카드인 `**`도 있습니다. 이는 기본적으로 적합한 파일과 디렉토리를 포함한다는 의미입니다.
예를 들어 다음 명령은 (귀하의 경우) 다음을 반환합니다:

```
> ls ~/images/**.jpg

~/images/nudes/pewdiepie.jpg
~/images/nudes/peppa.jpg
~/images/screenshots/2020-42-69.jpg
~/images/omegalul.jpg
```

물론, 명령의 출력을 다른 명령으로 파이프할 수도 있습니다.

```
>echo sick egg, nadia. no u do really goofy shit.   | grep [udense]
```

파일에 쓰기:

```
>echo This\ is\ text > file.txt
```

(이스케이프 문자를 눈치채셨습니까?)
파일에 추가:

```
>echo This\ is\ a\ line >> file.txt
>echo This\ is\ a\ second\ line >> file.txt
```

자동 완성을 위해 항상 <kbd>TAB</kbd>를 누르십시오. fish가 얼마나 많은 것을 알고 있는지 놀랄 것입니다.

변수를 사용하려면 bash와 같이 `$VAR`를 입력하십시오.

```
> echo "My home is $HOME"
My home is /home/myuser
```

여기서 작은따옴표와 큰따옴표의 차이점이 있습니다. 작은따옴표 안에 변수를 사용하면 대체되지 않습니다.

```
> echo 'My home is $HOME'
My home is $HOME
```

변수에 대한 자세한 내용은 나중에 설명합니다.

두 명령을 실행하려면 `;`로 구분하십시오.

```
> echo Lol; echo this is fun
```

마지막 명령의 상태 코드는 `$status`에 저장됩니다.

서로 의존하는 두 명령에 대해 &&를 사용할 수 있습니다.

```
> set var lol && echo $var
```

이전 명령이 성공한 경우 실행되는 `and`, 이전 명령이 성공하지 않은 경우 실행되는 `or`, 명령의 종료 상태를 반전시키는 `not`을 사용할 수도 있습니다.

예를 들어:

```
> if not echo It's very late I should not waste my time with this
      echo Nobody heard you
  end
```

(물론 셸에서 이 모든 것을 할 수 있습니다.)

---
이제 fish의 스크립팅 부분을 시작하겠습니다.

모든 셸과 마찬가지로 셸에서 명령을 실행할 수 있을 뿐만 아니라 `.fish` 파일로 저장된 파일로도 실행할 수 있습니다.
(fish 구문으로 `.sh` 파일을 실행할 수도 있지만, bash 스크립트 파일과 구별하기 위해 항상 fish 구문 스크립트에 `.fish`를 사용합니다.)

```fish
# 이것은 fish의 주석입니다.
#
# 인터프리터를 지정하지 않고 파일을 실행하는 경우,
# 즉, 스크립트를 실행하는 소프트웨어를 지정하지 않는 경우, 셸에
# 해당 인터프리터가 어디에 있는지 알려야 합니다.
# fish의 경우 스크립트의 첫 번째 줄에 다음 주석을 추가하기만 하면 됩니다:

#!/bin/fish

# 예를 들어 fish /path/to/script.fish를 통해 실행하는 경우
# fish를 인터프리터로 지정했으므로 필요하지 않습니다.

# 변수부터 시작하겠습니다.
# 프로그램 내에서 사용하려면 다음 구문을 사용할 수 있습니다.
set name 'My Variable'

# 사용...
set -x name value
# 내보내기(eXport) 또는
set -e name
# 지우기(Erase)

# 공백으로 설정된 변수는 예상대로 두 개의 인수가 아닌 하나의 인수로 전송됩니다.
set turtlefolder 'Turtle Folder'
mkdir $turtlefolder

# 이것은 예상대로 하나의 폴더를 생성하며, bash처럼 두 개가 아닙니다...
# 누가 그런 것을 원하겠습니까? 이것은 기능이지 버그가 아닙니다...

# 목록을 변수로 가질 수도 있습니다. 이것은 실제로 의미가 있습니다. 왜냐하면 두 개의 폴더를 생성하는 변수를 원한다면 mkdir에 폴더 이름 목록을 주기만 하면 되기 때문입니다.

# 그런 다음 다음을 사용하여 해당 목록의 항목 수를 계산할 수 있습니다:
count $PATH

# 모든 것이 멋질 뿐만 아니라 fish에서는 모든 것이 목록입니다.
# 따라서 예를 들어 $PWD는 길이가 1인 목록입니다.
# 목록을 만들려면 set 명령에 여러 인수를 주기만 하면 됩니다:
set list entry1 entry2 entry3

# 그렇게 하면 기존 변수에 무언가를 추가할 수도 있습니다:
set PATH $PATH ~/cowsay/

# 하지만 앞서 언급했듯이 fish에서는 특히 더 간단한 방법이 있습니다.
# 모든 배열/목록과 마찬가지로 다음을 사용하여 액세스할 수 있습니다.
$listvar[2]

# 다음을 사용하여 범위도 있습니다.
$listvar[1..5]

# 그리고 다음과 같이 음수를 사용할 수 있습니다.
$listvar[-1]
# 예를 들어 마지막 요소에 액세스합니다.

# 두 목록 변수를 결합할 때 멋진 데카르트 곱을 할 수도 있습니다:
set a 1 2 3
set 1 a b c
echo $a$1
# 출력: 1a 2a 3a 1b 2b 3b 1c 2c 3c

# 물론, 분리하면 두 개의 개별 인수로 간주하고 차례로 에코합니다. 이것이 예상되는 동작입니다 @bash.

# 다른 유용한 것들도 있습니다. 예를 들어 명령 대체가 있습니다. 예를 들어, 한 줄에 두 명령의 반환을 출력하고 싶을 때입니다. bash에서는 다음과 같이 합니다.
echo "`ls` is in $PWD"
# 또는
echo "$(ls) is in $PWD"

# 제 생각에는 불필요합니다. 저는 항상 잘못된 아포스트로피를 입력합니다. fish처럼 두 개의 괄호를 사용하지 않는 이유는 무엇입니까?
echo (ls) is in $PWD

# 네, 그렇게 쉽습니다. 그리고 fish의 강조 표시 덕분에 올바르게 입력했는지 즉시 확인할 수 있습니다.

# 그리고 예상대로, 제 생각에는 명령이 따옴표 안에서 작동하지 않습니다. 제 말은 왜 bash입니까? 좋습니다, 이제 그만하겠습니다. 하지만 fish에서는 다음과 같이 하십시오:
echo (ls)" is in $PWD"
# 또는
set myvar "The file"(ls -a)" is in the directory $PWD"
# 문자열과 모든 파일이 있는 목록을 만듭니다. 시도해 보십시오. 멋지지 않습니까?

# 그리고 이러한 변수를 별도의 인수로 분리하려면 그 사이에 공백을 두기만 하면 됩니다:

set myvar "The files" (ls -a) " are in the directory $PWD"

# if, else if, else가 있습니다.
if grep fish /etc/shells
    echo Found fish
else if grep bash /etc/shells
    echo Found bash
else
    echo Got nothing
end

# 한 가지 이상한 점은 물론 변수를 설정할 필요가 없기 때문에 한 개의 = 기호로 물건을 비교한다는 것입니다. 하지만 여전히... 그리고 키워드 "test":
if test $var = "test"
    echo yes
else
    echo no
end

# 물론, 다음과 같은 switch case도 있습니다.
switch $OS
case Linux
    echo "you're good"
case Windows
    echo "install Gentoo"
case Arch
    echo "I use arch btw"
case '*'
    echo "what OS is $OS, please?"
end


# fish의 함수는 $argv 변수를 통해 인수를 받습니다. 구문은 다음과 같습니다:

function print
    echo $argv
end

# "fish_exit"-이벤트와 같은 이벤트도 있습니다(이것이 무엇일까요, 흠?).

# 함수 정의에 추가하여 사용할 수 있습니다:

function on_exit --on-event fish_exit
    echo fish is now exiting
end

# 다음 명령으로 이벤트를 찾습니다.
functions --handlers


# functions 명령을 사용하여 함수에 대해 자세히 알아볼 수 있습니다.
# 예를 들어, 모든 함수의 소스 코드를 인쇄할 수 있습니다:
functions cd
functions print
# 또는 모든 함수의 이름을 가져옵니다:
functions

# 물론 while 루프가 있습니다.
while test $var = lol
    echo lol
end

# for 루프 (와일드카드를 사용하면 훨씬 더 멋집니다):
for image in *.jpg
    echo $image
end

# Python의 range(0, 5)와 동등한 것이 있으므로 숫자로 표준 for 루프를 수행할 수도 있습니다:

set files (ls)
for number in (seq 10)
    echo "$files[$number] is file number $number"
end

# 멋지다!

# bashrc에 해당하는 것은 fishrc가 아니라 앞서 언급한 ~/.config/fish/의 config.fish 파일입니다.
# fish에 함수를 추가하려면 해당 디렉토리에 간단한 .fish 파일을 만들어야 합니다. 해당 함수를 config.fish에 붙여넣지 마십시오. 보기 흉합니다.
# 더 있으면 추가하십시오. 하지만 이것들이 가장 중요한 기본 사항입니다.