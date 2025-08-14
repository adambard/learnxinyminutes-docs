# tmux.md (번역)

---
category: tool
name: tmux
contributors:
    - ["mdln", "https://github.com/mdln"]
filename: LearnTmux.txt
---


[tmux](http://tmux.github.io)는 터미널 멀티플렉서입니다: 여러 터미널을
하나의 화면에서 생성, 액세스 및 제어할 수 있습니다. tmux는
화면에서 분리되어 백그라운드에서 계속 실행될 수 있으며,
나중에 다시 연결할 수 있습니다.


```
  tmux [command]     # 명령어 실행
                     # 명령어가 없는 'tmux'는 새 세션을 생성합니다.

    new              # 새 세션 생성
     -s "Session"    # 명명된 세션 생성
     -n "Window"     # 명명된 창 생성
     -c "/dir"       # 대상 디렉토리에서 시작

    attach           # 마지막/사용 가능한 세션에 연결
     -t "#"          # 대상 세션에 연결
     -d              # 다른 인스턴스에서 세션 분리

    ls               # 열린 세션 목록
     -a              # 모든 열린 세션 목록

    lsw              # 창 목록
     -a              # 모든 창 목록
     -s              # 세션의 모든 창 목록

    lsp              # 창 목록
     -a              # 모든 창 목록
     -s              # 세션의 모든 창 목록
     -t              # 대상의 모든 창 목록

    kill-window      # 현재 창 종료
     -t "#"          # 대상 창 종료
     -a              # 모든 창 종료
     -a -t "#"       # 대상을 제외한 모든 창 종료

    kill-session     # 현재 세션 종료
     -t "#"          # 대상 세션 종료
     -a              # 모든 세션 종료
     -a -t "#"       # 대상을 제외한 모든 세션 종료
```


### 키 바인딩

연결된 tmux 세션을 제어하는 방법은 '접두사' 키라고 하는
키 조합을 통하는 것입니다.

```
----------------------------------------------------------------------
  (C-b) = Ctrl + b    # 키 바인딩을 사용하려면 '접두사' 조합이 필요합니다.

  (M-1) = Meta + 1 -또는- Alt + 1
----------------------------------------------------------------------

  ?                  # 모든 키 바인딩 목록
  :                  # tmux 명령어 프롬프트 입력
  r                  # 연결된 클라이언트 강제 다시 그리기
  c                  # 새 창 생성

  !                  # 현재 창을 창에서 분리합니다.
  %                  # 현재 창을 좌우로 분할합니다.
  "                  # 현재 창을 위아래로 분할합니다.

  n                  # 다음 창으로 변경
  p                  # 이전 창으로 변경
  {                  # 현재 창을 이전 창과 교체
  }                  # 현재 창을 다음 창과 교체
  [                  # 텍스트 복사 또는 기록 보기를 위해 복사 모드로 들어갑니다.

  s                  # 연결된 클라이언트에 대한 새 세션을 대화식으로
                     # 선택합니다.
  w                  # 현재 창을 대화식으로 선택
  0 to 9             # 창 0에서 9까지 선택

  d                  # 현재 클라이언트 분리
  D                  # 분리할 클라이언트 선택

  &                  # 현재 창 종료
  x                  # 현재 창 종료

  Up, Down           # 위, 아래, 왼쪽 또는 오른쪽 창으로 변경
  Left, Right

  M-1 to M-5         # 창 정렬:
                       # 1) 수평으로 균등하게
                       # 2) 수직으로 균등하게
                       # 3) 주 수평
                       # 4) 주 수직
                       # 5) 타일형

  C-Up, C-Down       # 현재 창 크기를 한 셀 단위로 조정
  C-Left, C-Right

  M-Up, M-Down       # 현재 창 크기를 다섯 셀 단위로 조정
  M-Left, M-Right
```


### ~/.tmux.conf 구성

tmux.conf는 .vimrc나 init.el이 사용되는 것처럼 시작 시
옵션을 자동으로 설정하는 데 사용할 수 있습니다.

```
# 예제 tmux.conf
# 2015.12


### 일반
###########################################################################

# 스크롤백/기록 제한
set -g history-limit 2048

# 인덱스 시작
set -g base-index 1

# 마우스
set-option -g -q mouse on

# 구성 파일 강제 다시 로드
unbind r
bind r source-file ~/.tmux.conf


### 키 바인딩
###########################################################################

# 기본 접두사로 C-b 바인딩 해제
unbind C-b

# 새 기본 접두사 설정
set-option -g prefix `

# 접두사를 두 번 누를 때 이전 창으로 돌아가기
bind C-a last-window
bind ` last-window

# F11/F12를 사용하여 C-a와 ` 교체 허용
bind F11 set-option -g prefix C-a
bind F12 set-option -g prefix `

# 키 바인딩 기본 설정
setw -g mode-keys vi
set-option -g status-keys vi

# vim 이동 키로 창 간 이동
bind h select-pane -L
bind j select-pane -D
bind k select-pane -U
bind l select-pane -R

# 창 순환/교체
bind e previous-window
bind f next-window
bind E swap-window -t -1
bind F swap-window -t +1

# 쉬운 창 분할 명령어
bind = split-window -h
bind - split-window -v
unbind '"'
unbind %

# 명령을 보내기 위해 가장 안쪽 세션 활성화 (tmux 중첩 시)
bind a send-prefix


### 테마
###########################################################################

# 상태 표시줄 색상 팔레트
set-option -g status-justify left
set-option -g status-bg black
set-option -g status-fg white
set-option -g status-left-length 40
set-option -g status-right-length 80

# 창 테두리 색상 팔레트
set-option -g pane-active-border-fg green
set-option -g pane-active-border-bg black
set-option -g pane-border-fg white
set-option -g pane-border-bg black

# 메시지 색상 팔레트
set-option -g message-fg black
set-option -g message-bg green

# 창 상태 색상 팔레트
setw -g window-status-bg black
setw -g window-status-current-fg green
setw -g window-status-bell-attr default
setw -g window-status-bell-fg red
setw -g window-status-activity-attr default
setw -g window-status-activity-fg yellow


### UI
###########################################################################

# 알림
setw -g monitor-activity on
set -g visual-activity on
set-option -g bell-action any
set-option -g visual-bell off

# 창 제목 자동 설정
set-option -g set-titles on
set-option -g set-titles-string '#H:#S.#I.#P #W #T' # 창 번호, 프로그램 이름, 활성(또는 비활성)

# 상태 표시줄 조정
set -g status-left "#[fg=red] #H#[fg=green]:#[fg=white]#S#[fg=green] |#[default]"

# 상태 표시줄에 성능 카운터 표시
# https://github.com/thewtex/tmux-mem-cpu-load/ 필요
set -g status-interval 4
set -g status-right "#[fg=green] | #[fg=white]#(tmux-mem-cpu-load)#[fg=green] | #[fg=cyan]%H:%M #[default]"
```


### 참고 자료

[Tmux | 홈페이지](http://tmux.github.io)

[Tmux 매뉴얼 페이지](http://www.openbsd.org/cgi-bin/man.cgi/OpenBSD-current/man1/tmux.1?query=tmux)

[Gentoo 위키](http://wiki.gentoo.org/wiki/Tmux)

[Archlinux 위키](https://wiki.archlinux.org/index.php/Tmux)

[상태 표시줄에 CPU/MEM % 표시](https://stackoverflow.com/questions/11558907/is-there-a-better-way-to-display-cpu-usage-in-tmux)

[tmuxinator - 복잡한 tmux 세션 관리](https://github.com/tmuxinator/tmuxinator)
