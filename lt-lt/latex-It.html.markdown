---
language: LaTeX
lang: it-it
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
    - ["Ramanan Balakrishnan", "https://github.com/ramananbalakrishnan"]
    - ["Svetlana Golubeva", "https://attillax.github.io/"]
    - ["Oliver Kopp", "http://orcid.org/0000-0001-6962-4290"]
translators:
    - ["Giovanni Cinieri", "https://github.com/Gio0203c"]
filename: learn-latex-It.tex
---

```tex
% Tutte le righe di commento iniziano con %
% Non esistono commenti su più righe

% LaTeX NON è un software di elaborazione testi "What You See Is What You Get" come
% MS Word o OpenOffice Writer

% Ogni comando LaTeX inizia con una barra rovesciata (\)

% I documenti LaTeX iniziano con una definizione del tipo di documento che si sta compilando
% Altri tipi di documenti includono book, report, presentazioni, ecc.
% Le opzioni per il documento appaiono tra le parentesi quadre  []. In questo caso
% specifica che vogliamo usare il font 12pt.
\documentclass[12pt]{article}

% Successivamente definiamo i pacchetti utilizzati dal documento.
% Se si desidera includere grafica, testo colorato o
% codice sorgente da un altro file di linguaggio nel documento,
% è necessario migliorare le capacità di LaTeX. Ciò viene fatto aggiungendo pacchetti.
% Sto per includere i pacchetti float e caption per le figure
% e il pacchetto hyperref per gli hyperlink
\usepackage{caption}
\usepackage{float}
\usepackage{hyperref}

% Possiamo definire anche altre proprietà del documento!
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu \& \\
Svetlana Golubeva}
\date{\today}
\title{Impara \LaTeX{} in Y minuti!}

% Ora siamo pronti per iniziare il documento
% Tutto ciò che precede questa riga è chiamato "Prefazione"
\begin{document}
% se impostiamo i campi autore, data, titolo, possiamo far creare a LaTeX
% una pagina del titolo per noi.
\maketitle

% Se abbiamo delle sezioni, possiamo creare un indice. Dobbiamo compilare il nostro
% documento due volte per farlo apparire nell'ordine corretto.
% È una buona pratica separare l'indice dal corpo del
% documento. Per farlo usiamo il comando \newpage
\newpage
\tableofcontents

\newpage

% La maggior parte dei lavori di ricerca ha un abstract, puoi usare i comandi predefiniti per questo.
% Questo dovrebbe apparire nel suo ordine logico, quindi, dopo le informazioni preliminari,
% ma prima delle sezioni principali del corpo.
% Questo comando è disponibile nelle classi di documenti article e report.
\begin{abstract}
  Documentazione \LaTeX{} scritta in \LaTeX! Quanto è innovativo e totalmente non una mia idea!
\end{abstract}

% I comandi delle sezioni sono intuitivi.
% Tutti i titoli delle sezioni vengono aggiunti automaticamente all'indice.
\section{Introduzione}
Ciao, mi chiamo Colton e insieme esploreremo \LaTeX!

\section{Un'altra sezione}
Questo è il testo di un'altra sezione. Penso che necessiti di una sottosezione.

\subsection{Questa è una sottosezione} % Le sottosezioni sono anche intuitive.
Penso che ne abbiamo bisogno di un'altra.

\subsubsection{Pitagora}
Molto meglio adesso.
\label{subsec:pitagora}

% Usando l'asterisco possiamo sopprimere la numerazione integrata di LaTeX.
% Questo funziona anche per altri comandi LaTeX.
\section*{Questa è una sezione non numerata}
Tuttavia non tutte le sezioni devono essere numerate!

\section{Alcune note sul testo}
%\section{Spaziatura} % Bisogna aggiungere maggiori informazioni sugli intervalli di spazio
\LaTeX{} è generalmente piuttosto bravo a posizionare il testo dove dovrebbe
andare. Se
una riga \\ deve \\ andare \\ a capo \\ aggiungi \textbackslash\textbackslash{}
al codice sorgente.

Separa i paragrafi con linee vuote.

È necessario aggiungere una tilde dopo le abbreviazioni (se non seguite da una virgola) per uno 
spazio non divisibile, perché altrimenti la spaziatura dopo il punto è troppo grande:
E.g., i.e., etc.~sono tali abbreviazioni.

\section{Liste}
Le liste sono una delle cose più facili da creare in \LaTeX! Devo andare a fare compere
domani, quindi creiamo una lista della spesa.
\begin{enumerate} % Questo crea un ambiente "enumerate".
  % \item dice all'enumerate di incrementare
  \item Insalata.
  \item 27 angurie.
  \item Un singolo jackrabbit.
  % possiamo anche sovrascrivere il numero dell'elemento usando []
  \item[quanti?] Pistole ad acqua di medie dimensioni.

  Non un elemento della lista, ma comunque parte dell'ambiente enumerate.

\end{enumerate} % Tutti gli ambienti devono avere una fine.

\section{Matematica}

Uno degli usi principali di \LaTeX{} è quello di produrre articoli accademici
o testi tecnici. Di solito nel campo della matematica e delle scienze. Pertanto,
dobbiamo essere in grado di aggiungere simboli speciali al nostro documento!

La matematica ha molti simboli, ben oltre quelli che puoi trovare su una tastiera;
Insiemi e simboli di relazione, frecce, operatori e lettere greche per citarne alcuni.

Insiemi e relazioni svolgono un ruolo vitale in molti articoli di ricerca matematica.
Ecco come indicare tutte le x che appartengono a X, $\forall x \in X$.
% Nota come ho dovuto aggiungere segni $ prima e dopo i simboli. Questo è
% perché quando scriviamo, siamo in modalità testo.
% Tuttavia, i simboli matematici esistono solo in modalità matematica.
% Possiamo entrare in modalità matematica dalla modalità testo con i segni $.
% Il contrario vale anche. Le variabili possono anche essere rese in modalità matematica.
% Possiamo anche entrare in modalità matematica con \[\]

\[a^2 + b^2 = c^2 \]

La mia lettera greca preferita è $\xi$. Mi piacciono anche $\beta$, $\gamma$ e $\sigma$.
Non ho ancora trovato una lettera greca che \LaTeX{} non conosca!

Gli operatori sono parti essenziali di un documento matematico:
funzioni trigonometriche ($\sin$, $\cos$, $\tan$),
logaritmi ed esponenziali ($\log$, $\exp$),
limiti ($\lim$), etc.~hanno comandi LaTeX predefiniti.
Scriviamo un'equazione per vedere come si fa:
$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Frazioni (Numeratore-denominatori) possono essere scritte in queste forme:

% 10 / 7
$$ ^{10}/_{7} $$

% Frazioni relativamente complesse possono essere scritte come
% \frac{numerator}{denominator}
$$ \frac{n!}{k!(n - k)!} $$

Possiamo anche inserire equazioni in un ``ambiente di equazione''.

% Visualizza la matematica con l'ambiente 'equation'
\begin{equation} % entra in modalità matematica
    c^2 = a^2 + b^2.
    \label{eq:pitagora} % per il riferimento
\end{equation} % tutte le dichiarazioni \ devono avere una dichiarazione finale

Possiamo quindi fare riferimento alla nostra nuova equazione!
Eqn.~\ref{eq:pitagora} è anche nota come il Teorema di Pitagora che è anche
l'oggetto della Sec.~\ref{subsec:pitagora}. Molte cose possono essere etichettate:
figure, equazioni, sezioni, etc.

Sommatorie e Integrali sono scritti con i comandi sum e int:

\begin{equation}
  \sum_{i=0}^{5} f_{i}
\end{equation}
\begin{equation}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation}

\section{Figure}

Inseriamo una figura. Il posizionamento delle figure può essere un po' complicato.
Le opzioni di base sono [t] per l'alto, [b] per il basso, [h] per qui (approssimativamente).
Devo assolutamente cercare le opzioni di posizionamento ogni volta.
% Vedi https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions per maggiori dettagli

\begin{figure}[H] % H qui indica l'opzione di posizionamento.
    \centering % centra la figura sulla pagina
    % Inserisce una figura ridimensionata allo 0,8 della larghezza della pagina.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % Commentato per scopi di compilazione. Per favore usa la tua immaginazione.
    \caption{Triangolo rettangolo con lati $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabella}
Possiamo anche inserire tabelle nello stesso modo in cui inseriamo le figure.

\begin{table}[H]
  \caption{Titolo della tabella.}
  % Le parentesi grafe {} in basso descrivono come viene disegnata ogni riga della tabella.
  % Le basi sono semplici: una lettera per ogni colonna, per controllare l'allineamento:
  % Le opzioni di base sono: c, l, r e p per centrato, sinistra, destra e paragrafo
  % Opzionalmente, puoi aggiungere un | per una linea verticale
  % Vedi https://en.wikibooks.org/wiki/LaTeX/Tables per maggiori dettagli
  \begin{tabular}{c|cc}  % qui significa "centrato | linea verticale, centrato centrato"
    Numero & Nome & Cognome \\ % Le righe delle colonne sono separate da &
    \hline % una linea orizzontale
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
  % Verrà visualizzato approssimativamente come questo
  % Numero | Nome        Cognome
  % -------|---------------------------  % a causa di \hline
  %   1    |   Biggus        Dickus
  %   2    |   Monty         Python
\end{table}

\section{Impedire a LaTeX di compilare qualcosa (Codice sorgente, per esempio)}

Supponiamo di voler includere del codice nel nostro documento \LaTeX{},
allora avremmo bisogno che \LaTeX{} non provi a interpretare quel testo e
invece lo stampi semplicemente nel documento. Facciamo questo con un ambiente 
verbatim.
% Esistono altri pacchetti (come minty, lstlisting, ecc.)
% ma verbatim è quello base più semplice.

\begin{verbatim}
  print("Hello World!")
  a%b; % guarda! Possiamo usare i segni % in verbatim.
  random = 4; # deciso da un tiro di dado casuale equo, https://www.xkcd.com/221/
  Vedi https://www.explainxkcd.com/wiki/index.php/221:_Random_Number
\end{verbatim}

\section{Compilazione}

A questo punto ti starai probabilmente chiedendo come compilare questo favoloso documento
e guardare la gloriosa gloria che è un PDF di \LaTeX{}.
(Sì, questo documento si compila davvero).

Arrivare al documento finale usando \LaTeX{} consiste nei seguenti passaggi:
  \begin{enumerate}
    \item Scrivi il documento in testo normale (il ``codice sorgente'').
    \item Compila il codice sorgente per produrre un pdf.
     Il passaggio di compilazione si presenta così (in Linux): \\
     \begin{verbatim}
        > pdflatex learn-latex.tex
     \end{verbatim}
  \end{enumerate}

Un certo numero di editor di LaTeX combinano sia il Passo 1 che il Passo 2
nello stesso software. Quindi, puoi vedere il Passo 1, ma non completamente il Passo 2.
Il Passo 2 sta ancora accadendo in background\footnote{In caso, dove si utilizzano 
riferimenti (come Eqn.~\ref{eq:pythagoras}), potrebbe essere necessario eseguire il Passo 2 
più volte, per generare un file *.aux intermedio.}.
% Anche così è come si aggiungono le note a piè di pagina al documento!
% con un semplice comando \footnote{...}. Sono numerate ¹, ²,... di default.

Scrivi tutte le tue informazioni di formattazione in testo normale nel Passo 1.
La parte di compilazione nel Passo 2 si occupa di produrre il documento nel
formato che hai definito nel Passo 1.

\section{Hyperlink}

Possiamo anche inserire dei collegamenti ipertestuali nel nostro documento. Per fare ciò dobbiamo includere il 
pacchetto hyperref nel preambolo con il comando:

\begin{verbatim}
    \usepackage{hyperref}
\end{verbatim}

Esistono due tipi principali di collegamenti: URL visibili \\
\url{https://learnxinyminutes.com/docs/latex/}, o 
\href{https://learnxinyminutes.com/docs/latex/}{con del testo}
% Non puoi aggiungere spazi extra o simboli speciali nel testo ombreggiato poiché ciò 
% causerà errori durante la compilazione.

Questo pacchetto genera anche un elenco di miniature nel documento PDF di output e 
collegamenti attivi nel sommario.

\section{Scrittura in ASCII o altre codifiche}

Per impostazione predefinita, storicamente LaTeX accetta input che sono ASCII puro (128),
ma non ASCII esteso, ovvero senza accenti (à, è ecc.) e simboli non latini.

È facile inserire accenti e simboli latini di base, con scorciatoie con barra rovesciata
Come \,c, \'e, \`A, \ae e \oe ecc.  % per ç, é, À, etc
% Vedi https://en.wikibooks.org/wiki/LaTeX/Special_Characters#Escaped_codes per maggiori dettagli

Per scrivere direttamente in UTF-8, durante la compilazione con pdflatex, usa:
\begin{verbatim}
    \usepackage[utf8]{inputenc}\end{verbatim}
Il font selezionato deve supportare i glifi utilizzati per il tuo documento, devi aggiungere:
\begin{verbatim}
    \usepackage[T1]{fontenc}\end{verbatim}

Poiché LuaTeX e XeLaTeX sono stati progettati con supporto integrato per UTF-8, 
facilitando la scrittura in alfabeti non latini.

\section{Conclusione}

Questo è tutto per ora! 
% Spesso vorresti avere una sezione riferimenti nel tuo documento.
% Il modo più semplice per impostarla è utilizzare la sezione bibliografia:

\begin{thebibliography}{1}
  % simile ad altre liste, il comando \bibitem può essere usato per elencare gli elementi
  % ogni voce può poi essere citata direttamente nel corpo del testo
  \bibitem{latexwiki} The amazing \LaTeX{} wikibook: 
  \emph{https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} An actual tutorial: \emph{http://www.latex-tutorial.com}
\end{thebibliography}

% termina il documento
\end{document}
```

## Altro su LaTeX
* The amazing LaTeX Wikibook (la pagina italiana non è ancora disponibile): [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* Una guida veloce per imparare il LaTeX (in inglese): [Learn LaTeX in 30 minutes](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes)
* Una piattaforma interattiva dove imparare LaTex (senza necessità di installazione e in italiano)[https://www.learnlatex.org/it/](https://www.learnlatex.org/it/)
* Domande, risposte e discussioni riguardo TeX, LaTeX, ConTeX, etc. su Stack Exchange (per lo più in inglese)[tex.stackexchange.com](https://tex.stackexchange.com/)
