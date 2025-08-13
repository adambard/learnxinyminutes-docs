---
name: LaTeX
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "https://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
    - ["Ramanan Balakrishnan", "https://github.com/ramananbalakrishnan"]
    - ["Svetlana Golubeva", "https://attillax.github.io/"]
    - ["Oliver Kopp", "http://orcid.org/0000-0001-6962-4290"]
filename: learn-latex.tex
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

```tex
% 모든 주석 줄은 %로 시작합니다.
% 여러 줄 주석은 없습니다.

% LaTeX는 MS Word나 OpenOffice Writer와 같은 "보이는 대로 얻는" 워드 프로세싱 소프트웨어가 아닙니다.

% 모든 LaTeX 명령은 백슬래시(\)로 시작합니다.

% LaTeX 문서는 컴파일할 문서 유형을 정의하는 것으로 시작합니다.
% 다른 문서 유형에는 책, 보고서, 프레젠테이션 등이 포함됩니다.
% 문서에 대한 옵션은 [] 대괄호 안에 나타납니다. 이 경우 12pt 글꼴을 사용하도록 지정합니다.
\documentclass[12pt]{article}

% 다음으로 문서가 사용하는 패키지를 정의합니다.
% 다른 언어 파일의 그래픽, 색상 텍스트 또는 소스 코드를 문서에 포함하려면 LaTeX의 기능을 향상시켜야 합니다. 이것은 패키지를 추가하여 수행됩니다.
% 그림을 위해 float 및 caption 패키지를 포함하고 하이퍼링크를 위해 hyperref 패키지를 포함합니다.
\usepackage{caption}
\usepackage{float}
\usepackage{hyperref}

% 다른 문서 속성도 정의할 수 있습니다!
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu & \\
Svetlana Golubeva}
\date{\today}
\title{Y분 만에 \LaTeX{} 배우기!}

% 이제 문서를 시작할 준비가 되었습니다.
% 이 줄 이전의 모든 것을 "서문"이라고 합니다.
\begin{document}
% 저자, 날짜, 제목 필드를 설정하면 LaTeX가 제목 페이지를 만들어 줄 수 있습니다.
\maketitle

% 섹션이 있는 경우 목차를 만들 수 있습니다. 올바른 순서로 표시되도록 문서를 두 번 컴파일해야 합니다.
% 문서 본문에서 목차를 분리하는 것이 좋습니다. 이를 위해 \newpage 명령을 사용합니다.
\newpage
\tableofcontents

\newpage

% 대부분의 연구 논문에는 초록이 있으며, 이를 위해 미리 정의된 명령을 사용할 수 있습니다.
% 이것은 논리적 순서대로 나타나야 하므로, 상단 자료 뒤, 본문의 주요 섹션 앞에 나타나야 합니다.
% 이 명령은 문서 클래스 article 및 report에서 사용할 수 있습니다.
\begin{abstract}
 \LaTeX{} 문서가 \LaTeX{}로 작성되었습니다! 얼마나 참신하고 완전히 제 아이디어가 아닌지!
\end{abstract}

% 섹션 명령은 직관적입니다.
% 모든 섹션 제목은 목차에 자동으로 추가됩니다.
\section{소개}
안녕하세요, 제 이름은 Colton이고 함께 \LaTeX{}를 탐색할 것입니다!

\section{다른 섹션}
이것은 다른 섹션의 텍스트입니다. 하위 섹션이 필요하다고 생각합니다.

\subsection{이것은 하위 섹션입니다} % 하위 섹션도 직관적입니다.
또 다른 것이 필요하다고 생각합니다.

\subsubsection{피타고라스}
이제 훨씬 낫습니다.
\label{subsec:pythagoras}

% 별표를 사용하면 LaTeX의 내장 번호 매기기를 억제할 수 있습니다.
% 이것은 다른 LaTeX 명령에서도 작동합니다.
\section*{이것은 번호가 매겨지지 않은 섹션입니다}
그러나 모든 섹션에 번호가 매겨져야 하는 것은 아닙니다!

\section{일부 텍스트 참고 사항}
%\section{간격} % 공간 간격에 대한 더 많은 정보를 추가해야 합니다.
\LaTeX{}는 일반적으로 텍스트를 있어야 할 곳에 배치하는 데 매우 능숙합니다.
줄이 \\ 끊어져야 하는 경우 \\ 소스 코드에 추가합니다.

빈 줄로 단락을 구분합니다.

약어 뒤에 쉼표가 없는 경우 비분리 공백을 위해 약어 뒤에 물결표를 추가해야 합니다. 그렇지 않으면 점 뒤의 간격이 너무 큽니다:
예:, 즉, 등~은 그러한 약어입니다.

\section{목록}
목록은 \LaTeX{}에서 가장 쉽게 만들 수 있는 것 중 하나입니다! 내일 쇼핑을 가야 하므로 식료품 목록을 만들어 보겠습니다.
\begin{enumerate} % 이것은 "enumerate" 환경을 만듭니다.
  % \item은 enumerate에 증가하도록 지시합니다.
  \item 샐러드.
  \item 수박 27개.
  \item 잭래빗 한 마리.
  % []를 사용하여 항목 번호를 재정의할 수도 있습니다.
  \item[몇 개?] 중간 크기 물총.

  목록 항목은 아니지만 여전히 enumerate의 일부입니다.

\end{enumerate} % 모든 환경에는 끝이 있어야 합니다.

\section{수학}

\LaTeX{}의 주요 용도 중 하나는 학술 논문이나 기술 문서를 작성하는 것입니다. 일반적으로 수학 및 과학 분야입니다. 따라서 논문에 특수 기호를 추가할 수 있어야 합니다!

수학에는 키보드에서 찾을 수 있는 것보다 훨씬 많은 기호가 있습니다.
집합 및 관계 기호, 화살표, 연산자 및 그리스 문자가 그 예입니다.

집합과 관계는 많은 수학 연구 논문에서 중요한 역할을 합니다. X에 속하는 모든 x를 다음과 같이 나타냅니다: $\forall x \in X$.
% 기호 앞뒤에 $ 기호를 추가해야 했습니다. 이것은 작성할 때 텍스트 모드에 있기 때문입니다.
% 그러나 수학 기호는 수학 모드에만 존재합니다.
% $ 기호로 텍스트 모드에서 수학 모드로 들어갈 수 있습니다.
% 반대도 마찬가지입니다. 변수도 수학 모드에서 렌더링할 수 있습니다.
% \[\ \]로 수학 모드로 들어갈 수도 있습니다.

\[a^2 + b^2 = c^2 \]

제가 가장 좋아하는 그리스 문자는 $\xi$입니다. $\beta$, $\gamma$ 및 $\sigma$도 좋아합니다.
아직 \LaTeX{}가 모르는 그리스 문자를 찾지 못했습니다!

연산자는 수학 문서의 필수적인 부분입니다: 삼각 함수($\sin$, $\cos$, $\tan$), 로그 및 지수($\log$, $\exp$), 극한($\lim$) 등에는 미리 정의된 LaTeX 명령이 있습니다. 어떻게 하는지 보기 위해 방정식을 작성해 보겠습니다:
$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

분수(분자-분모)는 다음과 같은 형식으로 작성할 수 있습니다:

% 10 / 7
$$ ^{10}/_{7} $$

% 비교적 복잡한 분수는 다음과 같이 작성할 수 있습니다.
% \frac{분자}{분모}
$$ \frac{n!}{k!(n - k)!} $$

방정식을 "방정식 환경"에 삽입할 수도 있습니다.

% 방정식 '환경'으로 수학 표시
\begin{equation} % 수학 모드로 들어감
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % 참조용
\end{equation} % 모든 \begin 문에는 end 문이 있어야 합니다.

그런 다음 새 방정식을 참조할 수 있습니다!
Eqn.\ref{eq:pythagoras}는 피타고라스 정리라고도 하며, Sec.\ref{subsec:pythagoras}의 주제이기도 합니다. 그림, 방정식, 섹션 등 많은 것을 레이블로 지정할 수 있습니다.

합계 및 적분은 sum 및 int 명령으로 작성됩니다:

% 일부 LaTeX 컴파일러는 빈 줄이 있으면 불합니다.
% 방정식 환경에서.
\begin{equation}
  \sum_{i=0}^{5} f_{i}
\end{equation}
\begin{equation}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation}

\section{그림}

그림을 삽입해 보겠습니다. 그림 배치는 약간 까다로울 수 있습니다. 기본 옵션은 위쪽 [t], 아래쪽 [b], 여기 [h](대략)입니다.
매번 배치 옵션을 찾아봐야 합니다.
% 자세한 내용은 https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions를 참조하십시오.

\begin{figure}[H] % H는 여기서 배치 옵션을 나타냅니다.
    \centering % 페이지에서 그림을 가운데에 맞춥니다.
    % 페이지 너비의 0.8로 크기가 조정된 그림을 삽입합니다.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % 컴파일 목적으로 주석 처리되었습니다. 상상력을 발휘하십시오.
    \caption{변 $a$, $b$, $c$가 있는 직각 삼각형}
    \label{fig:right-triangle}
\end{figure}

\subsection{표}
그림과 동일한 방식으로 표를 삽입할 수도 있습니다.

\begin{table}[H]
  \caption{표에 대한 캡션입니다.}
  % 아래의 {} 인수는 테이블의 각 행이 어떻게 그려지는지 설명합니다.
  % 기본은 간단합니다: 각 열에 대해 하나의 문자로 정렬을 제어합니다:
  % 기본 옵션은 c, l, r 및 p이며, 각각 가운데, 왼쪽, 오른쪽 및 단락입니다.
  % 선택적으로 수직선을 위해 |를 추가할 수 있습니다.
  % 자세한 내용은 https://en.wikibooks.org/wiki/LaTeX/Tables를 참조하십시오.
  \begin{tabular}{c|cc}  % 여기서 "가운데 | 수직선, 가운데 가운데"를 의미합니다.
    Number &  First Name & Last Name \ % 열 행은 &로 구분됩니다.
    \hline % 수평선
    1 & Biggus & Dickus \ 
    2 & Monty & Python
  \end{tabular}
  % 대략 다음과 같이 표시됩니다.
  % Number | First Name     Last Name
  % -------|---------------------------  % \hline 때문에
  %   1    |   Biggus        Dickus
  %   2    |   Monty         Python
\end{table}

\section{\LaTeX{}가 무언가를 컴파일하지 않도록 하기 (즉, 소스 코드)}
\LaTeX{} 문서에 일부 코드를 포함하고 싶다고 가정해 보겠습니다. 그러면 \LaTeX{}가 해당 텍스트를 해석하려고 시도하지 않고 대신 문서에 인쇄하도록 해야 합니다. 이것은 verbatim 환경으로 수행합니다.

% 다른 패키지가 존재하지만(즉, minty, lstlisting 등) verbatim은 기본입니다.
\begin{verbatim}
  print("Hello World!")
  a%b; % 보세요! verbatim에서 % 기호를 사용할 수 있습니다.
  random = 4; #공정한 임의 주사위 굴림으로 결정됨, https://www.xkcd.com/221/
  https://www.explainxkcd.com/wiki/index.php/221:_Random_Number 참조
\end{verbatim}

\section{컴파일}

이제 이 멋진 문서를 컴파일하고 \LaTeX{} pdf의 영광스러운 영광을 보는 방법을 궁금해하실 것입니다.
(예, 이 문서는 실제로 컴파일됩니다).

\LaTeX{}를 사용하여 최종 문서를 얻는 것은 다음 단계로 구성됩니다:
  \begin{enumerate}
    \item 일반 텍스트로 문서를 작성합니다("소스 코드").
    \item 소스 코드를 컴파일하여 pdf를 생성합니다.
     컴파일 단계는 다음과 같습니다(Linux에서): \\
     \begin{verbatim}
        > pdflatex learn-latex.tex
     \end{verbatim}
  \end{enumerate}

많은 \LaTeX{} 편집기는 1단계와 2단계를 동일한 소프트웨어에 결합합니다. 따라서 1단계를 볼 수 있지만 2단계는 완전히 볼 수 없습니다.
2단계는 여전히 백그라운드에서 발생합니다\footnote{참조(예: Eqn.\ref{eq:pythagoras})를 사용하는 경우 중간 *.aux 파일을 생성하기 위해 2단계를 여러 번 실행해야 할 수 있습니다.}.
% 또한 이것이 문서에 각주를 추가하는 방법입니다!
% 간단한 \footnote{...} 명령으로. 기본적으로 ¹, ², ...로 번호가 매겨집니다.

1단계에서 모든 서식 정보를 일반 텍스트로 작성합니다.
2단계의 컴파일 부분은 1단계에서 정의한 형식으로 문서를 생성하는 것을 처리합니다.

\section{하이퍼링크}
문서에 하이퍼링크를 삽입할 수도 있습니다. 이를 위해 서문에 hyperref 패키지를 포함해야 합니다:
\begin{verbatim}
    \usepackage{hyperref}
\end{verbatim}

두 가지 주요 유형의 링크가 있습니다: 보이는 URL \\
\url{https://learnxinyminutes.com/latex/}, 또는
\href{https://learnxinyminutes.com/latex/}{텍스트로 가려짐}
% 컴파일 중에 실수를 유발하므로 그림자 텍스트에 추가 공백이나 특수 기호를 추가할 수 없습니다.

이 패키지는 또한 출력 PDF 문서에 썸네일 목록과 목차에 활성 링크를 생성합니다.

\section{ASCII 또는 다른 인코딩으로 작성}

기본적으로 역사적으로 LaTeX는 순수 ASCII(128)인 입력을 허용하지만, 악센트(à, è 등) 및 비라틴 기호가 없는 확장 ASCII는 허용하지 않습니다.

악센트 및 기본 라틴 기호를 삽입하는 것은 백슬래시 바로 가기를 사용하여 쉽습니다.
\,c, \'e, \`A, \ae 및 \oe 등과 같습니다.  % ç, é, À 등의 경우
% 자세한 내용은 https://en.wikibooks.org/wiki/LaTeX/Special_Characters#Escaped_codes를 참조하십시오.

UTF-8로 직접 작성하려면 pdflatex로 컴파일할 때 다음을 사용하십시오.
\begin{verbatim}
    \usepackage[utf8]{inputenc}
\end{verbatim}
선택한 글꼴은 문서에 사용된 글리프를 지원해야 하며, 다음을 추가해야 합니다.
\begin{verbatim}
    \usepackage[T1]{fontenc}
\end{verbatim}

LuaTeX 및 XeLaTeX는 UTF-8을 내장 지원하도록 설계되었으므로 비라틴 알파벳으로 작성하는 것이 더 쉽습니다.

\section{끝}

지금은 여기까지입니다!

% 대부분의 경우 문서에 참조 섹션이 필요합니다.
% 이를 설정하는 가장 쉬운 방법은 참고 문헌 섹션을 사용하는 것입니다.
\begin{thebibliography}{1}
  % 다른 목록과 마찬가지로 \bibitem 명령을 사용하여 항목을 나열할 수 있습니다.
  % 각 항목은 텍스트 본문에서 직접 인용할 수 있습니다.
  \bibitem{latexwiki} 놀라운 \LaTeX{} 위키북: \emph{https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} 실제 튜토리얼: \emph{http://www.latex-tutorial.com}
\end{thebibliography}

% 문서 끝
\end{document}

## LaTeX에 대한 추가 정보

* 놀라운 LaTeX 위키북: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* 실제 튜토리얼: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
* LaTeX를 배우기 위한 빠른 가이드: [30분 만에 LaTeX 배우기](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes)
* LaTeX를 배우기 위한 대화형 플랫폼(설치 불필요) [learnlatex.org/](https://www.learnlatex.org/)
* TeX, LaTeX, ConTeXt 등에 대한 Stack Exchange의 질문 및 답변 사이트 [tex.stackexchange.com](https://tex.stackexchange.com/)