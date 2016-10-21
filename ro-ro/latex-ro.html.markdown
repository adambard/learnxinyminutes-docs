---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
    - ["Ramanan Balakrishnan", "https://github.com/ramananbalakrishnan"]
translators:
    - ["Petru Dimitriu", "http://petru-dimitriu.github.io"]
filename: learn-latex-ro.tex
lang: ro-ro
---

```tex
% Toate comentariile încep cu %
% Nu există comentarii multi-linie

% LaTeX NU este un program software de procesare text de tipul
% "What You See Is What You Get"
% precum MS Word, sau OpenOffice Writer

% Toate comenzile LaTeX încep cu backslash. (\)

% Documentele LaTeX încep cu o linie care definește tipul documentului
% care urmează a fi compilat. Alte tipuri de documente sunt book (carte),
% presentation (prezentare), etc. Opțiunile pentru document apar
% între paranteze drepte. În acest caz, specificăm că vrem să folosim
% un corp de text (font) de 12 puncte.
\documentclass[12pt]{article}

% Mai apoi definim pachetele pe care documentul le folosește.
% Dacă vreți să includeți grafice, text colorat sau
% cod sursă din alt fișier în documentul dumneavoastră,
% trebuie să îmbogățiți capabilitățile LaTeX. Aceasta se realizează
% adăugând pachete. Voi include pachetele float și caption pentru
% imagini.
\usepackage{caption}
\usepackage{float}
% această comandă este necesară atunci când vreți să scrieți codul
% sursă folosind diacrtice! (cum e cazul aici, unde translatorul
% a vrut să scrie neapărat folosind diacriticele românești)
\usepackage[utf8]{inputenc}

% De asemenea, putem defini și alte proprietăți pentru documente.
\author{Chaitanya Krishna Ande, Colton Kohnke \& Sricharan Chiruvolu \\ Traducere de Petru Dimitriu}
\date{\today}
\title{Învățați LaTeX în Y minute!}

% Suntem gata să începem documentul.
% Tot ce se află înaintea acestei linii se numește "Preambul"
\begin{document}
% dacă am setat autorul, data și titlul, putem cere LaTeX să
% creeze o pagină de titlu
\maketitle

% Cele mai multe documente științifice au un rezumat; puteți folosi comenzile
% predefinite pentru acesta. Acesta ar trebui să apară, așa cum ar fi logic,
% după titlu, dar înainte de secțiunile principale ale corpului.
% Această comandă este disponibilă în clasele de document article și report.
\begin{abstract}
 Documentațue LaTeX scrisă în LaTeX. O idee nicidecum nouă și nicidecum a mea!
\end{abstract}

% Comenzile pentru secțiuni sunt intuitive.
% Toate titlurile secțiunilor sunt adăugate automat la tabla de materii (cuprins).
\section{Introducere}
Salut, mă numesc Petru. Astăzi vom învăța împreună LaTeX!

\section{Altă secțiune}
Acesta este textul pentru altă secțiune. Vom face o subsecțiune.

\subsection{Aceasta este o subsecțiune}
Și încă una.

\subsubsection{Pitagora}
Mult mai bine.
\label{subsec:pitagora}

% Folosind asteriscul putem suprima numărătoarea automată a LaTeX.
% Aceasta funcționează și pentru alte comenzi LaTeX.
\section*{Secțiune fără numerotare}
Totuși nu toate secțiunile trebuie să fie nenumerotate!

\section{Note despre text}
În general LaTeX se pricepe să pună textul unde trebuie. Dacă o linie are \\
nevoie \\ să \\ fie \\ întreruptă, puteți adăuga două caractere backslash
la codul sursă.

\section{Liste}
Listele sunt printre cel mai simplu de făcut lucruri în LaTeX! Mâine merg la
cumpărături așa că fac o listă:
\begin{enumerate} % Aceasta creează un mediu "enumerate"
  % \item spune mediului "enumerate" să incrementeze
  \item salată
  \item 27 pepeni
  \item un singur iepuroi
  % putem suprascrie numărul elementului folosind []
  \item[câte?] conserve de ton

  Nu este un element din listă, dar încă face parte din "enumerate".

\end{enumerate} % Toate mediile trebuie să aibă o instrucțiune de încheiere.

\section{Matematică}

Una dintre principalele întrebuințări ale LaTeX este realizarea
articolelor academice sau a fișelor tehnice, de obicei aflate în
universul matematicii și științelor exacte. Astfel, trebuie să putem
adăuga simboluri speciale în documentul nostru! \\

Matematica are multe simboluri, mult mai multe decât se găsesc
pe o tastatură - printre ele, simboluri pentru mulțimi și relații,
săgeți, operatori și litere grecești.\\

Mulțimile și relațiile sunt esențiale în lucrările științifce matematice.
Iată cum se scrie: toți y aparținând lui X.\\
$\forall$ x $\in$ X. \\

% Observați cum am avut nevoie să pun semnul $ înainte și după simboluri.
% Aceasta pentru că atunci când scriem, suntem în modul text (text-mode).
% Totuși simbolurile matematice există numai în modul matematic (math-mode).
% Când ne aflăm în text-mode, putem scrie texte în math-mode punând $ înainte
% și după simboluri. La fel și viceversa. Și variabilele pot fi redate
% în math-mode. Putem intra în math-mode și scriind \[\].

\[a^2 + b^2 = c^2 \]

Îmi place litera $\xi$. De asemenea îmi plac $\beta$, $\gamma$ și
$\sigma$. Nu există nicio literă grecească necunoscută pentru LaTeX!

Operatorii sunt esențiali într-un document matematic!
funcțiile trigonometrice ($\sin$, $\cos$, $\tan$),
logaritmii și exponențialele ($\log$, $\exp$),
limitele ($\lim$), etc.
au comenzi definite în LaTeX pentru fiecare.
Să vedem cum scriem o ecuație: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Fracțiile (numărător - numitor) pot fi scrise astfel:
% 10 / 7
$^{10}/_{7}$ \\

% Fracții relativ complexe pot fi scrie ca
% \frac{numărător}{numitor}
$\frac{n!}{k!(n - k)!}$ \\

Putem insera ecuații și într-un "mediu pentru ecuații".

% Afișează text matematic într-un mediu pentru ecuații.
\begin{equation} % intră în math-mode
    c^2 = a^2 + b^2.
    \label{eq:pitagora} % pentru referențiere
\end{equation}
% toate instrucțiunile cu \begin trebuie să fie cuplate cu o instrucțiune cu \end

Putem referenția noua noastră ecuație!
~\ref{eq:pitagora} este cunoscută și ca Teorema lui Pitagora, despre care vorbim și la Sec.~\ref{subsec:pitagora}. Multe lucruri prot fi etichetate:
figuri, ecuații, secțiuni, etc.

Sumele discrete și integralele se scriu cu comenzile sum și int.

% Unele compilatoare LaTeX nu acceptă să existe linii goale
% într-un mediu pentru ecuații.
\begin{equation}
  \sum_{i=0}^{5} f_{i}
\end{equation}
\begin{equation}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation}

\section{Figuri}

Să inserăm o figură. Așezarea figurilor poate fi ușor dificilă.
Eu trebuie să mă uit peste opțiunile de așezare de fiecare dată.

\begin{figure}[H] % H denumește opțiunle de așezare
    \centering % centrează figura pe pagină
    % Inserează o figură scalată la 0.8 din lățimea paginii.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % Comentat pentru a nu împiedica fișierul să compileze.
    \caption{Triunghi dreptunghic cu laturile $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabel}
Putem insera tabele la fel cum inserăm figuri.

\begin{table}[H]
  \caption{Descriere pentru tabel}
  % argumentele {} controlează cum vor fi afișate coloanele
  \begin{tabular}{c|cc}
    Număr &  Nume & Prenume \\ % Numele coloanelor sunt separate prin $
    \hline % a linie orizonală
    1 & Popescu & Ion \\
    2 & Sima & Felix
  \end{tabular}
\end{table}

% \section{Hyperlinkuri} % În curând

\section{Cum facem ca LaTeX să nu compileze ceva (de exemplu cod sursă)}
Să zicem că vrem să includem niște cod în documentul nostru LaTeX.
Vom avea nevoie ca LaTeX să nu încerce să interpreteze acel cod,
ci doar să îl redea în document. Vom face asta cu un mediu verbatim.

% Există și alte pachete (i.e. minty, lstlisting, etc.)
% dar verbatim este pachetul cel mai simplu.
\begin{verbatim}
  print("Salut lume!")
  a%b; % hei! putem folosi % în verbatim
  random = 4;
\end{verbatim}

\section{Compilarea}
Acum vă întrebați cum se compilează acest document minunat și să vă
minunați de rezultat, un PDF LaTeX. (da, documentul acesta chiar
compilează). \\
Realizarea documentului cu LaTeX va parcurge următorii pași:
  \begin{enumerate}
    \item Se scrie documentul în text simplu. (codul sursă)
    \item Se compilează documentul pentru a produce un PDF.
     Compilarea arată cam așa în Linux:\\
     \begin{verbatim}
        $pdflatex learn-latex.tex learn-latex.pdf
     \end{verbatim}
  \end{enumerate}

Anumite editoare pentru LaTeX combină pașii 1 și 2 în același produs software.
Așadar, dacă vreți să vedeți realizați pasul 1 dar nu și pasul 2, el se poate
realiza "în spate".

Scrieți toate informațiile de formatare în pasul 1. Compilarea din pasul 2
se ocupă de producerea documentului în formatul definit în pasul 1.

\section{Final}

Asta e tot pentru moment!

% De multe ori veți vrea să aveți o secțiune cu bibliografie în document.
% Cea mai ușoară modalitate este folosind mediul thebibliography.
\begin{thebibliography}{1}
  % Similar celorlalte liste, comanda \bibitem e folosită pentru a înșirui
  % elemente; fiecare element poate fi citat în interiorul textului
  \bibitem{latexwiki} Uimitoarea carte wiki LaTeX: {\em https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} Un tutorial propriu-zis: {\em http://www.latex-tutorial.com}
\end{thebibliography}

% încheie documentul
\end{document}
```

## Mai multe despre LaTeX

* Uimitoarea carte wiki LaTeX: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* Un tutorial propriu-zis: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
