---
category: tool
name: Git
filename: LearnGit.txt
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
    - ["Andrew Taylor", "http://github.com/andrewjt71"]
    - ["Jason Stathopulos", "http://github.com/SpiritBreaker226"]
    - ["Milo Gilad", "http://github.com/Myl0g"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Git은 분산 버전 제어 및 소스 코드 관리 시스템입니다.

프로젝트의 일련의 스냅샷을 통해 이를 수행하며, 해당 스냅샷과 함께 작동하여 소스 코드의 버전을 관리하고 관리하는 기능을 제공합니다.

## 버전 관리 개념

### 버전 관리란 무엇입니까?

버전 관리는 시간이 지남에 따라 파일의 변경 사항을 기록하는 시스템입니다.

### 중앙 집중식 버전 관리 대 분산 버전 관리

* 중앙 집중식 버전 관리는 파일 동기화, 추적 및 백업에 중점을 둡니다.
* 분산 버전 관리는 변경 사항 공유에 중점을 둡니다. 모든 변경 사항에는 고유한 ID가 있습니다.
* 분산 시스템에는 정의된 구조가 없습니다. git을 사용하여 SVN 스타일의 중앙 집중식 시스템을 쉽게 가질 수 있습니다.

[추가 정보](https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control)

### Git을 사용하는 이유

* 오프라인으로 작업할 수 있습니다.
* 다른 사람과 협업하기 쉽습니다!
* 브랜칭이 쉽습니다!
* 브랜칭이 빠릅니다!
* 병합이 쉽습니다!
* Git은 빠릅니다.
* Git은 유연합니다.

## Git 아키텍처

### 리포지토리

파일, 디렉토리, 기록 기록, 커밋 및 헤드의 집합입니다. 소스 코드 데이터 구조로 상상해보십시오. 각 소스 코드 "요소"는 다른 것들 중에서 수정 기록에 대한 액세스를 제공하는 속성을 가집니다.

Git 리포지토리는 .git 디렉토리 및 작업 트리로 구성됩니다.

### .git 디렉토리 (리포지토리 구성 요소)

.git 디렉토리에는 모든 구성, 로그, 브랜치, HEAD 등이 포함됩니다.
[자세한 목록.](https://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### 작업 트리 (리포지토리 구성 요소)

이것은 기본적으로 리포지토리의 디렉토리 및 파일입니다. 종종 작업 디렉토리라고 합니다.

### 인덱스 (.git 디렉토리 구성 요소)

인덱스는 git의 스테이징 영역입니다. 기본적으로 작업 트리를 Git 리포지토리와 분리하는 계층입니다. 이를 통해 개발자는 Git 리포지토리로 전송되는 내용에 대해 더 많은 권한을 가질 수 있습니다.

### 커밋

Git 커밋은 작업 트리에 대한 일련의 변경 또는 조작의 스냅샷입니다. 예를 들어, 5개의 파일을 추가하고 2개의 다른 파일을 제거한 경우 이러한 변경 사항은 커밋(또는 스냅샷)에 포함됩니다. 그런 다음 이 커밋을 다른 리포지토리로 푸시하거나 푸시하지 않을 수 있습니다!

### 브랜치

브랜치는 본질적으로 마지막으로 만든 커밋에 대한 포인터입니다. 커밋을 계속하면 이 포인터는 자동으로 최신 커밋을 가리키도록 업데이트됩니다.

### 태그

태그는 기록의 특정 지점에 대한 표시입니다. 일반적으로 사람들은 이 기능을 사용하여 릴리스 지점(v1.0 등)을 표시합니다.

### HEAD 및 head (.git 디렉토리 구성 요소)

HEAD는 현재 브랜치를 가리키는 포인터입니다. 리포지토리에는 *활성* HEAD가 하나만 있습니다.
head는 모든 커밋을 가리키는 포인터입니다. 리포지토리에는 여러 개의 head가 있을 수 있습니다.

### Git의 단계
* 수정됨 - 파일이 변경되었지만 파일이 아직 Git 데이터베이스에 커밋되지 않았습니다.
* 스테이징됨 - 수정된 파일을 다음 커밋 스냅샷에 포함하도록 표시합니다.
* 커밋됨 - 파일이 Git 데이터베이스에 커밋되었습니다.

## 명령어

### init

빈 Git 리포지토리를 만듭니다. Git 리포지토리의 설정, 저장된 정보 등은 ".git"이라는 디렉토리(폴더)에 저장됩니다.

```bash
$ git init
```

### config

설정을 구성합니다. 리포지토리, 시스템 자체 또는 전역 구성(전역 구성 파일은 `~/.gitconfig`)에 대한 설정입니다.

```bash
# 일부 기본 구성 변수 설정 및 인쇄 (전역)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"

$ git config --global user.email
$ git config --global user.name
```

[git config에 대해 자세히 알아보기.](https://git-scm.com/docs/git-config)

### help

각 명령에 대한 매우 자세한 가이드에 빠르게 액세스하거나 일부 의미 체계를 빠르게 상기시켜줍니다.

```bash
# 사용 가능한 명령을 빠르게 확인
$ git help

# 사용 가능한 모든 명령 확인
$ git help -a

# 명령별 도움말 - 사용자 설명서
# git help <command_here>
$ git help add
$ git help commit
$ git help init
# 또는 git <command_here> --help
$ git add --help
$ git commit --help
$ git init --help
```

### ignore files

git에서 의도적으로 파일 및 폴더를 추적하지 않도록 합니다. 일반적으로 리포지토리에서 공유될 개인 및 임시 파일을 위한 것입니다.

```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```

### status

인덱스 파일(기본적으로 작업 복사본/리포지토리)과 현재 HEAD 커밋 간의 차이점을 표시합니다.

```bash
# 브랜치, 추적되지 않은 파일, 변경 사항 및 기타 차이점을 표시합니다.
$ git status

# git status에 대한 다른 "팁"을 알아보려면
$ git help status
```

### add

스테이징 영역/인덱스에 파일을 추가합니다. 새 파일을 스테이징 영역/인덱스에 `git add`하지 않으면 커밋에 포함되지 않습니다!

```bash
# 현재 작업 디렉토리에 파일 추가
$ git add HelloWorld.java

# 중첩된 디렉토리에 파일 추가
$ git add /path/to/file/HelloWorld.c

# 정규 표현식 지원!
$ git add ./*.java

# 작업 디렉토리의 모든 것을 스테이징 영역에 추가할 수도 있습니다.
$ git add -A
```

이것은 파일을 스테이징 영역/인덱스에만 추가하며, 작업 디렉토리/리포지토리에 커밋하지는 않습니다.

### branch

브랜치를 관리합니다. 이 명령을 사용하여 브랜치를 보고, 편집하고, 만들고, 삭제할 수 있습니다.

```bash
# 기존 브랜치 및 원격 나열
$ git branch -a

# 새 브랜치 만들기
$ git branch myNewBranch

# 브랜치 삭제
$ git branch -d myBranch

# 브랜치 이름 바꾸기
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# 브랜치 설명 편집
$ git branch myBranchName --edit-description
```

### tag

태그를 관리합니다.

```bash
# 태그 나열
$ git tag

# 주석이 달린 태그 만들기
# -m은 태그와 함께 저장되는 태그 지정 메시지를 지정합니다.
# 주석이 달린 태그에 대한 메시지를 지정하지 않으면 Git은 편집기를 실행하여 입력할 수 있도록 합니다.
$ git tag -a v2.0 -m 'my version 2.0'

# 태그 정보 표시
# 태그 지정자 정보, 커밋이 태그 지정된 날짜 및 커밋 정보를 표시하기 전에 주석 메시지를 표시합니다.
$ git show v2.0

# 원격에 단일 태그 푸시
$ git push origin v2.0

# 원격에 많은 태그 푸시
$ git push origin --tags
```

### checkout

작업 트리의 모든 파일을 인덱스 또는 지정된 트리의 버전과 일치하도록 업데이트합니다.

```bash
# 리포지토리 체크아웃 - 기본적으로 마스터 브랜치
$ git checkout

# 지정된 브랜치 체크아웃
$ git checkout branchName

# 새 브랜치를 만들고 전환
# "git branch <name>; git checkout <name>"과 동일

$ git checkout -b newBranch
```

### clone

기존 리포지토리를 새 디렉토리에 복제하거나 복사합니다. 또한 복제된 리포지토리의 각 브랜치에 대한 원격 추적 브랜치를 추가하여 원격 브랜치에 푸시할 수 있습니다.

```bash
# learnxinyminutes-docs 복제
$ git clone https://github.com/adambard/learnxinyminutes-docs.git

# 얕은 복제 - 최신 스냅샷만 가져오는 더 빠른 복제
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git

# 특정 브랜치만 복제
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

인덱스의 현재 내용을 새 "커밋"에 저장합니다. 이 커밋에는 사용자가 만든 변경 사항과 메시지가 포함됩니다.

```bash
# 메시지와 함께 커밋
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"

# 메시지와 함께 서명된 커밋 (user.signingkey는 GPG 키로 설정되어 있어야 함, 예: git config --global user.signingkey 5173AAD5)
$ git commit -S -m "signed commit message"

# 새 파일을 제외하고 수정되거나 삭제된 파일을 자동으로 스테이징한 다음 커밋
$ git commit -a -m "Modified foo.php and removed bar.php"

# 마지막 커밋 변경 (이전 커밋을 새 커밋으로 삭제)
$ git commit --amend -m "Correct message"
```

### diff

작업 디렉토리, 인덱스 및 커밋의 파일 간의 차이점을 표시합니다.

```bash
# 작업 디렉토리와 인덱스 간의 차이점 표시
$ git diff

# 인덱스와 가장 최근 커밋 간의 차이점 표시.
$ git diff --cached

# 작업 디렉토리와 가장 최근 커밋 간의 차이점 표시
$ git diff HEAD
```

### grep

리포지토리를 빠르게 검색할 수 있습니다.

선택적 구성:

```bash
# Travis Jeffery에게 감사드립니다.
# grep 검색 결과에 줄 번호 표시 설정
$ git config --global grep.lineNumber true

# 그룹화를 포함하여 검색 결과를 더 읽기 쉽게 만듭니다.
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# 모든 java 파일에서 "variableName" 검색
$ git grep 'variableName' -- '*.java'

# "arrayListName"을 포함하고 "add" 또는 "remove"를 포함하는 줄 검색
$ git grep -e 'arrayListName' --and \( -e add -e remove \
```

Google은 당신의 친구입니다. 더 많은 예제를 보려면
[Git Grep Ninja](https://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

리포지토리에 대한 커밋을 표시합니다.

```bash
# 모든 커밋 표시
$ git log

# 커밋 메시지 및 참조만 표시
$ git log --oneline

# 병합 커밋만 표시
$ git log --merges

# ASCII 그래프로 표시된 모든 커밋 표시
$ git log --graph
```

### merge

외부 커밋의 변경 사항을 현재 브랜치로 "병합"합니다.

```bash
# 지정된 브랜치를 현재 브랜치로 병합합니다.
$ git merge branchName

# 병합 시 항상 병합 커밋 생성
$ git merge --no-ff branchName
```

### mv

파일 이름 바꾸기 또는 이동

```bash
# 파일 이름 바꾸기
$ git mv HelloWorld.c HelloNewWorld.c

# 파일 이동
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# 강제 이름 바꾸기 또는 이동
# "existingFile"이 디렉토리에 이미 존재하며 덮어쓰게 됩니다.
$ git mv -f myFile existingFile
```

### pull

리포지토리에서 가져와 다른 브랜치와 병합합니다.

```bash
# 원격 "origin" 및 "master" 브랜치에서 새 변경 사항을 병합하여 로컬 리포지토리를 업데이트합니다.
# git pull <remote> <branch>
$ git pull origin master

# 기본적으로 git pull은 원격 추적 브랜치에서 새 변경 사항을 병합하여 현재 브랜치를 업데이트합니다.
$ git pull

# 원격 브랜치에서 변경 사항을 병합하고 로컬 리포지토리에 브랜치 커밋을 리베이스합니다. 예: "git fetch <remote> <branch>, git rebase <remote>/<branch>"
$ git pull origin master --rebase
```

### push

브랜치의 변경 사항을 원격 및 브랜치로 푸시하고 병합합니다.

```bash
# 로컬 리포지토리의 변경 사항을 "origin" 및 "master" 브랜치라는 원격으로 푸시하고 병합합니다.
# git push <remote> <branch>
$ git push origin master

# 기본적으로 git push는 현재 브랜치의 변경 사항을 원격 추적 브랜치로 푸시하고 병합합니다.
$ git push

# 현재 로컬 브랜치를 원격 브랜치와 연결하려면 -u 플래그를 추가하십시오:
$ git push -u origin master
# 이제 동일한 로컬 브랜치에서 푸시하고 싶을 때마다 바로 가기를 사용하십시오:
$ git push
```

### stash

Stashing은 작업 디렉토리의 더러운 상태를 가져와 언제든지 다시 적용할 수 있는 미완성 변경 사항 스택에 저장합니다.

git 리포지토리에서 일부 작업을 수행했지만 원격에서 가져오고 싶다고 가정해 보겠습니다. 일부 파일에 더러운(커밋되지 않은) 변경 사항이 있으므로 `git pull`을 실행할 수 없습니다. 대신 `git stash`를 실행하여 변경 사항을 스택에 저장할 수 있습니다!

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

이제 가져올 수 있습니다!

```bash
git pull
```

`...변경 사항 적용...`

이제 모든 것이 정상인지 확인하십시오.

```bash
$ git status
# On branch master
nothing to commit, working directory clean
```

`git stash list`를 사용하여 지금까지 숨겨진 "덩어리"를 볼 수 있습니다. 
"덩어리"는 후입선출 스택에 저장되므로 가장 최근 변경 사항이 맨 위에 있습니다.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

이제 스택에서 팝하여 더러운 변경 사항을 다시 적용해 보겠습니다.

```bash
$ git stash pop
# On branch master
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#
#      modified:   index.html
#      modified:   lib/simplegit.rb
#
```

`git stash apply`도 동일한 작업을 수행합니다.

이제 다시 작업할 준비가 되었습니다!

[추가 자료.](https://git-scm.com/book/en/v2/Git-Tools-Stashing-and-Cleaning)

### rebase (주의)

한 브랜치에 커밋된 모든 변경 사항을 가져와 다른 브랜치에 다시 적용합니다.
*공개 리포지토리에 푸시한 커밋은 리베이스하지 마십시오*.

```bash
# experimentBranch를 마스터에 리베이스
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[추가 자료.](https://git-scm.com/book/en/v2/Git-Branching-Rebasing)

### reset (주의)

현재 HEAD를 지정된 상태로 재설정합니다. 이를 통해 병합, 가져오기, 커밋, 추가 등을 취소할 수 있습니다. 훌륭한 명령이지만 무엇을 하는지 모르면 위험하기도 합니다.

```bash
# 스테이징 영역을 재설정하여 최신 커밋과 일치하도록 합니다(디렉토리는 변경되지 않음).
$ git reset

# 스테이징 영역을 재설정하여 최신 커밋과 일치하도록 하고 작업 디렉토리를 덮어씁니다.
$ git reset --hard

# 현재 브랜치 팁을 지정된 커밋으로 이동합니다(디렉토리는 변경되지 않음).
# 모든 변경 사항은 여전히 디렉토리에 존재합니다.
$ git reset 31f2bb1

# 현재 브랜치 팁을 지정된 커밋으로 뒤로 이동하고
# 작업 디렉토리를 일치시킵니다(커밋되지 않은 변경 사항 및 지정된 커밋 이후의 모든 커밋 삭제).
$ git reset --hard 31f2bb1
```

### reflog (주의)

Reflog는 지정된 기간(기본값 90일) 동안 수행한 대부분의 git 명령을 나열합니다.

이를 통해 잘못된 git 명령을 되돌릴 수 있습니다(예: 리베이스로 인해 애플리케이션이 손상된 경우).

이렇게 할 수 있습니다:

1. `git reflog`를 사용하여 리베이스에 대한 모든 git 명령을 나열합니다.

```
38b323f HEAD@{0}: rebase -i (finish): returning to refs/heads/feature/add_git_reflog
38b323f HEAD@{1}: rebase -i (pick): Clarify inc/dec operators
4fff859 HEAD@{2}: rebase -i (pick): Update java.html.markdown
34ed963 HEAD@{3}: rebase -i (pick): [yaml/en] Add more resources (#1666)
ed8ddf2 HEAD@{4}: rebase -i (pick): pythonstatcomp spanish translation (#1748)
2e6c386 HEAD@{5}: rebase -i (start): checkout 02fb96d
```

2. 재설정할 위치를 선택합니다. 이 경우 `2e6c386` 또는 `HEAD@{5}`입니다.
3. 'git reset --hard HEAD@{5}'는 리포지토리를 해당 헤드로 재설정합니다.
4. 리베이스를 다시 시작하거나 그대로 둘 수 있습니다.

[추가 자료.](https://git-scm.com/docs/git-reflog)

### revert

Revert는 커밋을 취소하는 데 사용할 수 있습니다. 프로젝트의 상태를 이전 지점으로 복원하는 재설정과 혼동해서는 안 됩니다. Revert는 지정된 커밋의 역인 새 커밋을 추가하여 되돌립니다.

```bash
# 지정된 커밋 되돌리기
$ git revert <commit>
```

### rm

git add의 반대인 git rm은 현재 작업 트리에서 파일을 제거합니다.

```bash
# HelloWorld.c 제거
$ git rm HelloWorld.c

# 중첩된 디렉토리에서 파일 제거
$ git rm /pather/to/the/file/HelloWorld.c
```

### blame

코드의 특정 부분을 검사하고 해당 줄을 마지막으로 수정한 작성자를 찾습니다.

```bash
# 최신 수정된 줄의 작성자 찾기
$ git blame google_python_style.vim
b88c6a1b (Google Python team  2019-12-30 13:45:23 -0800 12) " See the License for the specific language governing permissions and
b88c6a1b (Google Python team  2019-12-30 13:45:23 -0800 13) " limitations under the License.
b88c6a1b (Google Python team  2019-12-30 13:45:23 -0800 14)
222e6da8 (mshields@google.com 2010-11-29 20:32:06 +0000 15) " Indent Python in the Google way.
222e6da8 (mshields@google.com 2010-11-29 20:32:06 +0000 16)
222e6da8 (mshields@google.com 2010-11-29 20:32:06 +0000 17) setlocal indentexpr=GetGooglePythonIndent(v:lnum)
```

## 추가 정보

* [Git 브랜칭 배우기 - 웹에서 Git을 배우는 가장 시각적이고 대화형인 방법](https://learngitbranching.js.org/)

* [Udemy Git 튜토리얼: 종합 가이드](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [Git 몰입 - git의 기본 사항을 안내하는 가이드 투어](https://gitimmersion.com/)

* [git-scm - 비디오 튜토리얼](https://git-scm.com/videos)

* [git-scm - 문서](https://git-scm.com/docs)

* [Atlassian Git - 튜토리얼 및 워크플로](https://www.atlassian.com/git/)

* [SalesForce 치트 시트](https://res.cloudinary.com/hy4kyit2a/image/upload/SF_git_cheatsheet.pdf)

* [git - 간단한 가이드](https://rogerdudler.github.io/git-guide/index.html)

* [Pro Git](https://git-scm.com/book/en/v2)

* [초보자를 위한 Git 및 GitHub 소개 (튜토리얼)](https://product.hubspot.com/blog/git-and-github-tutorial-for-beginners)

* [기본 명령 및 워크플로를 다루는 Git에 대한 새로운 보스턴 튜토리얼](https://www.youtube.com/playlist?list=PL6gx4Cwl9DGAKWClAD_iKpNC0bGHxGhcx)

* [컴퓨터 과학자를 위한 Git](https://eagain.net/articles/git-for-computer-scientists/)