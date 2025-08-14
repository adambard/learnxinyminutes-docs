---
category: tool
name: Mercurial
contributors:
  - ["Will L. Fife", "http://github.com/sarlalian"]
filename: LearnMercurial.txt
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Mercurial은 무료 분산 소스 제어 관리 도구입니다.
직관적인 인터페이스를 사용하면서 모든 크기의 프로젝트를 효율적으로 처리할 수 있는 강력한 기능을 제공합니다. 사용하기 쉽고 깨지기 어려워 버전 관리된 파일로 작업하는 모든 사람에게 이상적입니다.

## 버전 관리 개념

### 버전 관리란 무엇인가?

버전 관리는 시간 경과에 따른 파일 및/또는 디렉토리 집합의 변경 사항을 추적하는 시스템입니다.

### 왜 Mercurial을 사용하는가?

* 분산 아키텍처 - 전통적으로 CVS 및 Subversion과 같은 버전 제어 시스템은 프로젝트의 개정 기록을 저장하는 중앙 서버가 있는 클라이언트 서버 아키텍처입니다. 그러나 Mercurial은 진정한 분산 아키텍처로, 각 개발자에게 전체 개발 기록의 전체 로컬 복사본을 제공합니다. 중앙 서버와 독립적으로 작동합니다.
* 빠름 - 전통적으로 CVS 및 Subversion과 같은 버전 제어 시스템은 프로젝트의 개정 기록을 저장하는 중앙 서버가 있는 클라이언트 서버 아키텍처입니다. 그러나 Mercurial은 진정한 분산 아키텍처로, 각 개발자에게 전체 개발 기록의 전체 로컬 복사본을 제공합니다. 중앙 서버와 독립적으로 작동합니다.
* 플랫폼 독립적 - Mercurial은 플랫폼 독립적으로 작성되었습니다. Mercurial의 대부분은 Python으로 작성되었으며, 작은 성능이 중요한 부분은 이식 가능한 C로 작성되었습니다. 바이너리 릴리스는 모든 주요 플랫폼에서 사용할 수 있습니다.
* 확장 가능 - Mercurial의 기능은 Mercurial과 함께 제공되는 공식 확장을 활성화하거나 [위키에서 다운로드](https://www.mercurial-scm.org/wiki/UsingExtensions)하거나 [직접 작성](https://www.mercurial-scm.org/wiki/WritingExtensions)하여 확장할 수 있습니다. 확장은 Python으로 작성되었으며 기본 명령의 작동 방식을 변경하고, 새 명령을 추가하고, Mercurial의 모든 핵심 기능에 액세스할 수 있습니다.
* 사용하기 쉬움 - Mercurial 명령 세트는 Subversion 사용자가 기대하는 것과 일치하므로 집처럼 편안하게 느낄 것입니다. 대부분의 위험한 작업은 사용하려면 활성화해야 하는 확장의 일부입니다.
* 오픈 소스 - Mercurial은 [GNU 일반 공중 사용 허가서 버전 2](http://www.gnu.org/licenses/gpl-2.0.txt) 또는 이후 버전의 조건에 따라 라이선스가 부여된 무료 소프트웨어입니다.

## 용어

| 용어 | 정의 |
| ------------- | ---------------------------------- |
| 저장소 | 저장소는 개정판 모음입니다. |
| hgrc | 저장소의 기본값을 저장하는 구성 파일입니다. |
| 개정판 | 커밋된 변경 집합: REV 번호가 있습니다. |
| 변경 집합 | 차이점으로 저장된 변경 사항 집합 |
| 차이점 | 파일 간의 변경 사항 |
| 태그 | 명명된 개정판 |
| 부모 | 개정판의 직계 조상 |
| 브랜치 | 개정판의 자식 |
| 헤드 | 헤드는 자식 변경 집합이 없는 변경 집합입니다. |
| 병합 | 두 개의 HEAD를 병합하는 프로세스 |
| 팁 | 모든 브랜치의 최신 개정판 |
| 패치 | 두 개정판 간의 모든 차이점 |
| 번들 | 권한 및 이름 바꾸기 지원이 포함된 패치 |

## 명령어

### init

지정된 디렉토리에 새 저장소를 생성하며, 설정 및 저장된 정보는 `.hg`라는 디렉토리에 있습니다.

```bash
$ hg init
```

### help

각 명령어에 대한 매우 자세한 설명에 액세스할 수 있습니다.

```bash
# 사용 가능한 명령어를 빠르게 확인
$ hg help

# 특정 명령어에 대한 도움말 얻기
# hg help <command>
$ hg help add
$ hg help commit
$ hg help init
```

### status

디스크에 있는 것과 현재 브랜치 또는 태그에 커밋된 것 사이의 차이점을 표시합니다.

```bash
# 파일 상태 표시
$ hg status

# 상태 하위 명령어에 대한 도움말 얻기
$ hg help status
```

### add

다음 커밋 시 지정된 파일을 저장소에 추가합니다.

```bash
# 현재 디렉토리에 파일 추가
$ hg add filename.rb

# 하위 디렉토리에 파일 추가
$ hg add foo/bar/filename.rb

# 패턴별로 파일 추가
$ hg add *.rb
```

### branch

현재 브랜치 이름을 설정하거나 표시합니다.

*브랜치 이름은 영구적이고 전역적입니다. 대신 가벼운 북마크를 만들려면 'hg bookmark'를 사용하십시오. 명명된 브랜치와 북마크에 대한 자세한 내용은 'hg help glossary'를 참조하십시오.*

```bash
# 인수가 없으면 현재 브랜치 이름을 표시합니다.
$ hg branch

# 이름 인수가 있으면 현재 브랜치를 변경합니다.
$ hg branch new_branch
작업 디렉토리를 브랜치 new_branch로 표시했습니다.
(브랜치는 영구적이고 전역적입니다. 북마크를 원하셨습니까?)
```

### tag

현재 또는 지정된 개정판에 하나 이상의 태그를 추가합니다.

태그는 저장소의 특정 개정판에 이름을 지정하는 데 사용되며, 다른 개정판을 비교하거나, 중요한 이전 버전으로 돌아가거나, 브랜치 지점을 릴리스로 표시하는 데 매우 유용합니다. 기존 태그를 변경하는 것은 일반적으로 허용되지 않습니다. 재정의하려면 -f/--force를 사용하십시오.

```bash
# 태그 목록
$ hg tags
tip                                2:efc8222cd1fb
v1.0                               0:37e9b57123b3

# 현재 개정판에 새 태그 생성
$ hg tag v1.1

# 특정 개정판에 태그 생성
$ hg tag -r efc8222cd1fb v1.1.1
```

### clone

기존 저장소의 복사본을 새 디렉토리에 생성합니다.

대상 디렉토리 이름이 지정되지 않은 경우 소스의 기본 이름으로 기본 설정됩니다.

```bash
# 원격 저장소를 로컬 디렉토리로 복제
$ hg clone https://some-mercurial-server.example.com/reponame

# 로컬 저장소를 원격 서버로 복제
$ hg clone . ssh://username@some-mercurial-server.example.com/newrepo

# 로컬 저장소를 로컬 저장소로 복제
$ hg clone . /tmp/some_backup_dir
```

### commit / ci

지정된 파일의 변경 사항을 저장소에 커밋합니다.

```bash
# 메시지와 함께 커밋
$ hg commit -m 'This is a commit message'

# 현재 트리의 모든 추가/제거된 파일 커밋
$ hg commit -A 'Adding and removing all existing files in the tree'

# 'hg status'가 현재 보고하는 변경 사항 외에 부모의 변경 사항을
# 포함하는 새 커밋으로 작업 디렉토리의 부모를 수정합니다.
$ hg commit --amend -m "Correct message"
```

### diff

통합 diff 형식을 사용하여 지정된 파일에 대한 개정판 간의 차이점을 표시합니다.

```bash
# 현재 디렉토리와 이전 개정판 간의 차이점 표시
$ hg diff -r 10

# 두 이전 개정판 간의 차이점 표시
$ hg diff -r 30 -r 20
```

### grep

지정된 파일에서 패턴에 대한 개정 기록을 검색합니다.

```bash
# 특정 구문에 대한 파일 검색
$ hg grep "TODO:"
```

### log / history

전체 저장소 또는 파일의 개정 기록을 표시합니다. 개정 범위가 지정되지 않은 경우 --follow가 설정되지 않은 한 기본값은 "tip:0"이며, 이 경우 작업 디렉토리 부모가 시작 개정판으로 사용됩니다.

```bash
# 전체 저장소의 기록 표시
$ hg log

# 단일 파일의 기록 표시
$ hg log myfile.rb

# 가장 최근 변경 집합이 맨 위에 있는 ASCII 아트 DAG로
# 개정 변경 사항을 표시합니다.
$ hg log -G
```

### merge

다른 개정판을 작업 디렉토리로 병합합니다.

```bash
# 로컬 저장소에 변경 집합 병합
$ hg merge

# 명명된 브랜치 또는 개정판에서 현재 로컬 브랜치로 병합
$ hg merge branchname_or_revision

# 성공적인 병합 후 변경 사항 커밋
hg commit
```

### move / mv / rename

파일 이름 바꾸기; 복사 + 제거와 동일합니다. 대상을 소스의 복사본으로 표시하고, 소스를 삭제하도록 표시합니다. 대상이 디렉토리인 경우 복사본이 해당 디렉토리에 배치됩니다. 대상이 파일인 경우 소스는 하나만 있을 수 있습니다.

```bash
# 단일 파일 이름 바꾸기
$ hg mv foo.txt bar.txt

# 디렉토리 이름 바꾸기
$ hg mv some_directory new_directory
```

### pull

원격 저장소에서 로컬 저장소로 변경 사항을 가져옵니다.

```bash
# 원격 경로 목록
$ hg paths
remote1 = http://path/to/remote1
remote2 = http://path/to/remote2

# 원격 1에서 가져오기
$ hg pull remote1

# 원격 2에서 가져오기
$ hg pull remote2
```

### push

로컬 저장소의 변경 집합을 지정된 대상으로 푸시합니다.

```bash
# 원격 경로 목록
$ hg paths
remote1 = http://path/to/remote1
remote2 = http://path/to/remote2

# 원격 1에서 푸시
$ hg push remote1

# 원격 2에서 푸시
$ hg push remote2
```

### rebase

변경 집합(및 하위 항목)을 다른 브랜치로 이동합니다.

리베이스는 반복적인 병합을 사용하여 기록의 한 부분(소스)에서 다른 부분(대상)으로 변경 집합을 이식합니다. 이는 마스터 개발 트리에 대한 *로컬* 변경 사항을 선형화하는 데 유용할 수 있습니다.

* 커밋을 소스 개정판으로 다시 초안으로 작성합니다.
* -s는 소스, 즉 리베이스하는 대상입니다.
* -d는 대상, 즉 보내는 곳입니다.

```bash
# 커밋을 초안 상태로 만듭니다.
# 이것은 관련 브랜치의 모든 후속 커밋을 초안으로 만듭니다.
$ hg phase --draft --force -r 1206

# 개정 102에서 개정 208로 리베이스
$ hg rebase -s 102 -d 208
```

### revert

파일을 체크아웃 상태로 복원합니다. 개정판이 지정되지 않은 경우 지정된 파일 또는 디렉토리를 작업 디렉토리의 부모에 있던 내용으로 되돌립니다. 이렇게 하면 파일의 내용을 수정되지 않은 상태로 복원하고 추가, 제거, 복사 및 이름 바꾸기를 예약 취소합니다. 작업 디렉토리에 두 개의 부모가 있는 경우 개정판을 명시적으로 지정해야 합니다.

```bash
# 특정 파일을 체크아웃 상태로 재설정
$ hg revert oops_i_did_it_again.txt

# .orig 파일을 남기지 않고 특정 파일을 체크아웃 상태로 되돌리기
$ hg revert -C oops_i_did_it_again.txt

# 모든 변경 사항 되돌리기
$ hg revert -a
```

### rm / remove

다음 커밋 시 지정된 파일을 제거합니다.

```bash
# 특정 파일 제거
$ hg remove go_away.txt

# 패턴별로 파일 그룹 제거
$ hg remove *.txt
```

## 추가 정보

* [워크플로에서 Mercurial 배우기](https://www.mercurial-scm.org/guide)
* [Mercurial 빠른 시작](https://www.mercurial-scm.org/wiki/QuickStart)
* [Mercurial: Bryan O'Sullivan의 결정판 가이드](http://hgbook.red-bean.com/)
