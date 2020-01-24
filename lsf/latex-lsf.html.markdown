---
language: latex
lang: lsf
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translators:
    - ["Victore Leve", "https://github.com/AcProIL"]
filename: learn-latex-lsf.tex
---

```tex
% Solo existe commentario monolinea, illo incipe cum charactere %

% LaTeX non es sicut MS Word aut OpenOffice: que scribe non es que obtine.
% Primo, scribe imperio (que semper incipe cum \) et secundo programma crea
% lima.

% Nos defini typo de document (id es articulo aut libro aut libello etc.).
% Optione muta quomodo programma age, per exemplo altore de littera.
\documentclass[12pt]{article}

% Deinde nos lista paccettos que nos vol ute. Es classe de imperio que alio
% utatore e scribe. Pote muta funda, geometria de pagina, etc. vel adde
% functionnalitate.
\usepackage{euler}
\usepackage{graphicx}

% Ultimo statione ante scribe documento es metadata id es titulo, auctore et
% tempore. Charactere ~ es spatio que non pote es secato.
\title{Disce LaTeX in~Y Minutos!}
\author{Chaitanya Krishna Ande, Colton Kohnke \& Sricharan Chiruvolu}
\date{\today}

% Principio de documento
\begin{document}
    \maketitle % Nos vol adfige metadata.

    % Saepe nos adde breviario us describe texto.
    \begin{abstract}
        Hic es exmplo de documento sibre cum lingua de LaTeX.
    \end{abstract}

    % \section crea sectione cum titulo dato sicut sperato
    \section{Introductione}

    Traductione de hic cursu es importante.

    \subsection{Methodo}
    Iste parte non es utile.

    \subsubsection{Methodo peculiare}
    % \label da nomine ad parte ut post ute imperio de referentia \ref.
    \label{subsec:metpec}

    % Cum asteritco nos indice que nos non vol numero ante titulo de sectione.
    \section*{Me non aestima numero…}

    …sed de Peano aut de Church.

    \section{Listas}
   
    Que me debe scribe:
    
    \begin{enumerate} % `enumerate` designa lista cum numeros contra `itemize`.
        \item articulo,
        \item libro,
        \item cursu.
    \end{enumerate}

    \section{Mathematica}

    Methematicas ute multo programma LaTeX ut communica suo decooperito.
    Illo necessita symbolo multo instar de logica vel sagitta vel littera cum
    accento.

    % Fornula es in linea si nos scribe inter \( et \) (aut duo $) sed magno si
    % nos ute \[ et \].
    \(\forall n\in N_0\) % pro omne n in classe N₀
    \[^{3}/_{4} = \frac{3}{4} < 1\] % ¾ < 1
    
    Alphabeta graeco contine littera $\alpha$.

    % Ut scribe equatione cum numero et nomine, existe circumiecto `equation`.
    \begin{equation}
        c^2 = a^2 + b^2
        \label{eq:pythagoras}
    \end{equation}

    \begin{equation}
        % Summa ab 1 ad n de numeros dimidio de n(n+1)
        \sum_{i=1}^n i = \frac{n(n+1)}{2}
    \end{equation}

    \section{Figura}

    % Nos adde imagine `right-triangle.png` cum latitudo de quinque centimetro,
    % horizontaliter in centro et cum capite «Triangulo recto».
    \begin{figure}
        \centering
        \includegraphics[width=5cm]{right-triangle.png}
        \caption{Triangulo recto}
        \label{fig:right-triangle}
    \end{figure}

    \subsection{Tabula}

    \begin{table}
    \caption{Título para la tabla.}
        % Argumento de `tabular` es lineamente de columna.
        % c: centro, l: sinistra, r: destra, | linea verticale
        \begin{tabular}{c|cc}
            Numero & B & C \\
            \hline % linea horizontale
            1 & et & aut \\
            2 & atque & vel
        \end{tabular}
    \end{table}

    \section{Stylo}

    Texto pote es \textbf{crasso} et \textit{italico}!

    \section{Texto puro}

    % Circumiecto `verbatim` ignora imperio, nos saepe ute id pro monstra
    % programma.
    \begin{verbatim}
from math import tau, e
print(e ** tau)
    \end{verbatim}

    \section{Et plus!}
    LaTeX habe facultate crea bibliographia, paritura, scaccarip… cum paccetto
    dedicato.
\end{document}
```

Imperio ut conge documento es `pdflatex documento` in terminale.

## Ut progrede

### In lingua anglo

* [LaTeX tutorial](http://www.latex-tutorial.com/) per Claudio Vellage
