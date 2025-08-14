---
name: Emacs Lisp
contributors:
    - ["Bastien Guerry", "https://bzg.fr"]
    - ["Saurabh Sandav", "http://github.com/SaurabhSandav"]
    - ["rilysh", "https://github.com/rilysh"]
filename: learn-emacs-lisp.el
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```elisp
;; 이 문서는 15분 만에 Emacs Lisp에 대한 소개를 제공합니다 (v0.2d)
;;
;; 먼저 Peter Norvig의 이 글을 읽으십시오:
;; http://norvig.com/21-days.html
;;
;; 그런 다음 최신 버전의 GNU Emacs를 설치하십시오:
;;
;; Debian: apt-get install emacs (또는 배포판 지침 참조)
;; OSX: https://emacsformacosx.com/
;; Windows: https://ftp.gnu.org/gnu/emacs/windows/
;;
;; 더 일반적인 정보는 다음에서 찾을 수 있습니다:
;; http://www.gnu.org/software/emacs/#Obtaining

;; 중요한 경고:
;;
;; 이 튜토리얼을 진행해도 컴퓨터가 손상되지 않습니다.
;; 화가 나서 바닥에 던지지 않는 한 말이죠. 그 경우,
;; 저는 어떠한 책임도 지지 않습니다. 즐거운 시간 보내세요!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs를 실행하십시오.
;;
;; 환영 메시지를 닫으려면 `q` 키를 누르십시오.
;;
;; 이제 창 하단의 회색 줄을 보십시오:
;;
;; "*scratch*"는 현재 있는 편집 공간의 이름입니다.
;; 이 편집 공간을 "버퍼"라고 합니다.
;;
;; 스크래치 버퍼는 Emacs를 열 때 기본 버퍼입니다.
;; 파일을 편집하는 것이 아니라, 파일에 저장할 수 있는 버퍼를 편집하는 것입니다.
;;
;; "Lisp interaction"은 여기에서 사용할 수 있는 명령 집합을 나타냅니다.
;;
;; Emacs에는 모든 버퍼에서 사용할 수 있는 내장 명령 집합이 있으며,
;; 특정 모드를 활성화할 때 사용할 수 있는 여러 하위 집합의 명령이 있습니다.
;; 여기서는 Elisp 코드를 평가하고 탐색하는 명령이 포함된 `lisp-interaction-mode`를 사용합니다.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 세미콜론은 줄의 어느 곳에서나 주석을 시작합니다.
;;
;; Elisp 프로그램은 기호 표현식("sexps")으로 구성됩니다:
(+ 2 2)

;; 이 기호 표현식은 "2에 2를 더하라"고 읽습니다.

;; Sexps는 괄호로 묶여 있으며, 중첩될 수 있습니다:
(+ 2 (+ 1 1))

;; 기호 표현식에는 원자 또는 다른 기호 표현식이 포함됩니다.
;; 위 예제에서 1과 2는 원자이고,
;; (+ 2 (+ 1 1))과 (+ 1 1)은 기호 표현식입니다.

;; `lisp-interaction-mode`에서 sexps를 평가할 수 있습니다.
;; 닫는 괄호 바로 뒤에 커서를 놓고
;; 컨트롤을 누른 상태에서 j 키를 누르십시오(줄여서 "C-j").

(+ 3 (+ 1 2))
;;           ^ 커서 여기
;; `C-j` => 6

;; `C-j`는 평가 결과를 버퍼에 삽입합니다.

;; `C-xC-e`는 Emacs 하단 줄에 동일한 결과를 표시합니다.
;; 이를 "에코 영역"이라고 합니다. 우리는 일반적으로 `C-xC-e`를 사용할 것입니다.
;; 버퍼를 불필요한 텍스트로 어지럽히고 싶지 않기 때문입니다.

;; `setq`는 변수에 값을 저장합니다:
(setq my-name "Bastien")
;; `C-xC-e` => "Bastien" (에코 영역에 표시됨)

;; `insert`는 커서가 있는 곳에 "Hello!"를 삽입합니다:
(insert "Hello!")
;; `C-xC-e` => "Hello!"

;; 우리는 "Hello!"라는 단일 인수로 `insert`를 사용했지만,
;; 더 많은 인수를 전달할 수 있습니다 -- 여기서는 두 개를 사용합니다:

(insert "Hello" " world!")
;; `C-xC-e` => "Hello world!"

;; 문자열 대신 변수를 사용할 수 있습니다:
(insert "Hello, I am " my-name)
;; `C-xC-e` => "Hello, I am Bastien"

;; sexps를 함수로 결합할 수 있습니다:
(defun hello () (insert "Hello, I am " my-name))
;; `C-xC-e` => hello

;; 함수를 평가할 수 있습니다:
(hello)
;; `C-xC-e` => Hello, I am Bastien

;; 함수의 정의에 있는 빈 괄호는
;; 인수를 받지 않음을 의미합니다. 하지만 항상 `my-name`을 사용하는 것은
;; 지루하므로, 함수가 하나의 인수를 받도록 알려줍시다(여기서
;; 인수는 "name"이라고 합니다):

(defun hello (name) (insert "Hello " name))
;; `C-xC-e` => hello

;; 이제 고유한 인수에 대한 값으로 문자열 "you"를 사용하여 함수를 호출해 보겠습니다:
(hello "you")
;; `C-xC-e` => "Hello you"

;; 네!

;; 숨을 고르십시오.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 이제 다른 창에서 "*test*"라는 새 버퍼로 전환하십시오:

(switch-to-buffer-other-window "*test*")
;; `C-xC-e`
;; => [화면에 두 개의 창이 있고 커서는 *test* 버퍼에 있습니다]

;; 마우스를 위쪽 창 위로 가져가 왼쪽 클릭하여 돌아갑니다. 또는 `C-xo`를 사용할 수 있습니다.
;; (즉, 컨트롤-x를 누른 상태에서 o를 누름)를 사용하여 대화식으로 다른 창으로 이동합니다.

;; `progn`을 사용하여 여러 sexps를 결합할 수 있습니다:
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))
;; `C-xC-e`
;; => [화면에 두 개의 창이 있고 커서는 *test* 버퍼에 있습니다]

;; 이제 괜찮으시다면, `C-xC-e`를 누르라고 요청하는 것을 멈추겠습니다: 다음 모든 sexp에 대해 그렇게 하십시오.

;; 항상 마우스 또는 `C-xo`를 사용하여 *scratch* 버퍼로 돌아갑니다.

;; 버퍼를 지우는 것이 종종 유용합니다:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))

;; 또는 다른 창으로 돌아가기:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))

;; `let`을 사용하여 지역 변수에 값을 바인딩할 수 있습니다:
(let ((local-name "you"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; 이 경우 `progn`을 사용할 필요가 없습니다. `let`도 여러 sexps를 결합하기 때문입니다.

;; 문자열 서식 지정:
(format "Hello %s!\n" "visitor")

;; %s는 "visitor"로 대체되는 문자열의 자리 표시자입니다.
;; \n은 줄 바꿈 문자입니다.

;; format을 사용하여 함수를 구체화해 보겠습니다:
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; `let`을 사용하는 다른 함수를 만들어 보겠습니다:
(defun greeting (name)
  (let ((your-name "Bastien"))
    (insert (format "Hello %s!\n\nI am %s."
                    name       ; 함수의 인수
                    your-name  ; let으로 바인딩된 변수 "Bastien"
                    ))))

;; 그리고 평가합니다:
(greeting "you")

;; 일부 함수는 대화식입니다:
(read-from-minibuffer "Enter your name: ")

;; 이 함수를 평가하면 프롬프트에 입력한 내용이 반환됩니다.

;; `greeting` 함수가 이름을 묻도록 만들어 보겠습니다:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (insert (format "Hello!\n\nI am %s and you are %s."
                    from-name ; 함수의 인수
                    your-name ; let으로 바인딩된 변수, 프롬프트에서 입력됨
                    ))))

(greeting "Bastien")

;; 다른 창에 결과를 표시하여 완성해 보겠습니다:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello %s!\n\nI am %s." your-name from-name))
    (other-window 1)))

;; 이제 테스트합니다:
(greeting "Bastien")

;; 숨을 고르십시오.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 이름 목록 저장:
;; 데이터의 리터럴 목록을 만들려면 '를 사용하여 평가되지 않도록 하십시오 - 말 그대로 데이터를 "인용"하십시오.
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; `car`를 사용하여 이 목록의 첫 번째 요소를 가져옵니다:
(car list-of-names)

;; `cdr`을 사용하여 첫 번째 요소를 제외한 모든 요소의 목록을 가져옵니다:
(cdr list-of-names)

;; `push`를 사용하여 목록의 시작 부분에 요소를 추가합니다:
(push "Stephanie" list-of-names)

;; 참고: `car`와 `cdr`은 목록을 수정하지 않지만 `push`는 수정합니다.
;; 이것은 중요한 차이점입니다: 일부 함수는 부작용이 없지만(`car`와 같이) 다른 함수는 있습니다(`push`와 같이).

;; `list-of-names`의 각 요소에 대해 `hello`를 호출해 보겠습니다:
(mapcar 'hello list-of-names)

;; `list-of-names`의 모든 사람에게 인사하도록 `greeting`을 구체화합니다:
(defun greeting ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (mapcar 'hello list-of-names)
    (other-window 1))

(greeting)

;; 위에서 정의한 `hello` 함수를 기억하십니까? 이름이라는 하나의 인수를 받습니다.
;; `mapcar`는 `hello`를 호출하고, `list-of-names`의 각 요소를 `hello`의 인수로 순차적으로 사용합니다.

;; 이제 표시된 버퍼에 있는 것을 약간 정리해 보겠습니다:

(defun replace-hello-by-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (search-forward "Hello")
      (replace-match "Bonjour"))
    (other-window 1))

;; (goto-char (point-min))은 버퍼의 시작으로 이동합니다.
;; (search-forward "Hello")는 문자열 "Hello"를 검색합니다.
;; (while x y)는 x가 무언가를 반환하는 동안 y sexp를 평가합니다.
;; x가 `nil`(아무것도 없음)을 반환하면 while 루프를 종료합니다.

(replace-hello-by-bonjour)

;; *test* 버퍼에서 "Hello"의 모든 발생이
;; "Bonjour"로 대체된 것을 볼 수 있습니다.

;; 또한 "Search failed: Hello"라는 오류가 발생해야 합니다.
;; 
;; 이 오류를 피하려면 `search-forward`에 버퍼의 특정 지점에서 검색을 중지해야 하는지 여부와
;; 아무것도 찾지 못했을 때 자동으로 실패해야 하는지 여부를 알려야 합니다:

;; (search-forward "Hello" nil t)가 그 역할을 합니다:

;; `nil` 인수는 검색이 위치에 바인딩되지 않음을 의미합니다.
;; `'t'` 인수는 아무것도 찾지 못했을 때 자동으로 실패함을 의미합니다.

;; 오류를 발생시키지 않는 아래 함수에서 이 sexp를 사용합니다:

(defun hello-to-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    ;; `list-of-names`의 이름에 인사
    (mapcar 'hello list-of-names)
    (goto-char (point-min))
    ;; "Hello"를 "Bonjour"로 바꾸기
    (while (search-forward "Hello" nil t)
      (replace-match "Bonjour"))
    (other-window 1))

(hello-to-bonjour)

;; 이름을 굵게 만들어 보겠습니다:

(defun boldify-names ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (re-search-forward "Bonjour \(.+\)!")
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold)))
    (other-window 1))

;; 이 함수는 `re-search-forward`를 도입합니다: 문자열 "Bonjour"를 검색하는 대신
;; "정규 표현식"(접두사 "re-"로 축약됨)을 사용하여 패턴을 검색합니다.

;; 정규 표현식은 "Bonjour \(.+\)!")이며 다음과 같이 읽습니다:
;; 문자열 "Bonjour "와
;; 그룹 | 이것은 \( ... \) 구문입니다.
;;   모든 문자       | 이것은 .입니다.
;;   반복될 수 있음   | 이것은 +입니다.
;; 그리고 "!" 문자열.

;; 준비되셨습니까? 테스트하십시오!

(boldify-names)

;; `add-text-properties`는 얼굴과 같은 텍스트 속성을 추가합니다.

;; 좋습니다, 끝났습니다. 즐거운 해킹 되세요!

;; 변수나 함수에 대해 더 알고 싶다면:
;; 
;; C-h v a-variable RET
;; C-h f a-function RET
;; 
;; Emacs로 Emacs Lisp 설명서를 읽으려면:
;; 
;; C-h i m elisp RET
;; 
;; Emacs Lisp에 대한 온라인 소개를 읽으려면:
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html
```

### 추가 자료
- [GNU Elisp 설명서](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)
- [Emacs 위키](https://www.emacswiki.org/emacs/LearningEmacs)
- [Emacs 문서](https://emacsdocs.org/docs/elisp/Emacs-Lisp)
- [Mpre Elisp 문서](https://www.math.utah.edu/docs/info/elisp_22.html)