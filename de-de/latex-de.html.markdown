---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translators:
  - ["Moritz Kammerer", "https://github.com/phxql"]
  - ["Jerome Meinke", "https://github.com/jmeinke"]
lang: de-de
filename: latex-de.tex
---
```
% Alle Kommentare starten mit einem Prozentzeichen %

% LaTeX ist keine "What You See Is What You Get" Textverarbeitungssoftware wie z.B.
% MS Word oder OpenOffice Writer

% Jedes LaTeX-Kommando startet mit einem Backslash (\)

% LaTeX-Dokumente starten immer mit der Definition des Dokuments, die sie darstellen
% Weitere Dokumententypen sind z.B. book, report, presentations, etc.
% Optionen des Dokuments stehen zwischen den eckigen Klammern []. In diesem Fall
% wollen wir einen 12 Punkte-Font verwenden.
\documentclass[12pt]{article}

% Als nächstes definieren wir die Pakete, die wir verwenden wollen.
% Wenn du z.B. Grafiken, farbigen Text oder Quelltext in dein Dokument einbetten möchtest,
% musst du die Fähigkeiten von LaTeX durch Hinzufügen von Paketen erweitern.
% Wir verwenden die Pakete float und caption für Bilder.
\usepackage{caption}
\usepackage{float}

% Mit diesem Paket können leichter Umlaute getippt werden
\usepackage[utf8]{inputenc}

% Es gibt eigentlich keine Kommentare über mehrere Zeilen, solche kann man
% aber selbst durch die Angabe eigener Kommandos definieren.
% Dieses Kommando kann man später benutzen.
\newcommand{\comment}[1]{}

% Es können durchaus noch weitere Optionen für das Dokument gesetzt werden!
\author{Chaitanya Krishna Ande, Colton Kohnke \& Sricharan Chiruvolu}
\date{\today}
\title{Learn \LaTeX\ in Y Minutes!}

% Nun kann's losgehen mit unserem Dokument.
% Alles vor dieser Zeile wird die Präambel genannt.
\begin{document}

\comment{
  Dies ist unser selbst-definierter Befehl
  für mehrzeilige Kommentare.
}

% Wenn wir den Autor, das Datum und den Titel gesetzt haben, kann
% LaTeX für uns eine Titelseite generieren
\maketitle

% Die meisten Paper haben ein Abstract. LaTeX bietet dafür einen vorgefertigen Befehl an.
% Das Abstract sollte in der logischen Reihenfolge, also nach dem Titel, aber vor dem
% Inhalt erscheinen.
% Dieser Befehl ist in den Dokumentenklassen article und report verfügbar.
\begin{abstract}
 \LaTeX -Dokumentation geschrieben in \LaTeX ! Wie ungewöhnlich und garantiert nicht meine Idee!
\end{abstract}

% Section Befehle sind intuitiv.
% Alle Titel der sections werden automatisch in das Inhaltsverzeichnis übernommen.
\section{Einleitung}
Hi, mein Name ist Moritz und zusammen werden wir \LaTeX\ erforschen!

\section{Noch eine section}
Das hier ist der Text für noch eine section. Ich glaube, wir brauchen eine subsection.

\subsection{Das ist eine subsection} % Subsections sind auch ziemlich intuitiv.
Ich glaube, wir brauchen noch eine.

\subsubsection{Pythagoras}
So ist's schon viel besser.
\label{subsec:pythagoras}

% Wenn wir den Stern nach section schreiben, dann unterdrückt LateX die Nummerierung.
% Das funktioniert auch bei anderen Befehlen.
\section*{Das ist eine unnummerierte section}
Es müssen nicht alle Sections nummeriert sein!

\section{Ein paar Notizen}
\LaTeX\ ist ziemlich gut darin, Text so zu platzieren, dass es gut aussieht.
Falls eine Zeile \\ mal \\ woanders \\ umgebrochen \\ werden \\ soll, füge
\textbackslash\textbackslash in den Code ein.\\

\section{Listen}
Listen sind eine der einfachsten Dinge in \LaTeX. Ich muss morgen einkaufen gehen,
also lass uns eine Einkaufsliste schreiben:
\begin{enumerate} % Dieser Befehl erstellt eine "enumerate" Umgebung.
  % \item bringt enumerate dazu, eins weiterzuzählen.
  \item Salat.
  \item 27 Wassermelonen.
  \item einen Hasen.
  % Wir können die Nummer des Eintrags durch [] überschreiben
  \item[Wie viele?] Mittelgroße Wasserpistolen.

  Kein Listeneintrag, aber immer noch Teil von enumerate.

\end{enumerate} % Alle Umgebungen müssen ein end haben.

\section{Mathe}

Einer der Haupteinsatzzwecke von \LaTeX\ ist das Schreiben von akademischen
Artikeln oder Papern. Meistens stammen diese aus dem Bereich der Mathe oder
anderen Wissenschaften. Und deswegen müssen wir in der Lage sein, spezielle
Symbole zu unserem Paper hinzuzufügen! \\

Mathe kennt sehr viele Symbole, viel mehr als auf einer Tastatur zu finden sind;
Symbole für Mengen und Relationen, Pfeile, Operatoren und Griechische Buchstaben,
um nur ein paar zu nennen.\\

Mengen und Relationen spielen eine sehr wichtige Rolle in vielen mathematischen
Papern. So schreibt man in \LaTeX, dass alle y zu X gehören: $\forall$ y $\in$ X. \\

% Achte auf die $ Zeichen vor und nach den Symbolen. Wenn wir in LaTeX schreiben,
% geschieht dies standardmäßig im Textmodus. Die Mathe-Symbole existieren allerdings
% nur im Mathe-Modus. Wir können den Mathe-Modus durch das $ Zeichen aktivieren und
% ihn mit $ wieder verlassen. Variablen können auch im Mathe-Modus angezeigt werden.

Mein Lieblingsbuchstabe im Griechischen ist $\xi$. Ich mag auch $\beta$, $\gamma$ und $\sigma$.
Bis jetzt habe ich noch keinen griechischen Buchstaben gefunden, den \LaTeX nicht kennt!

Operatoren sind ebenfalls wichtige Bestandteile von mathematischen Dokumenten:
Trigonometrische Funktionen ($\sin$, $\cos$, $\tan$),
Logarithmus und Exponenten ($\log$, $\exp$),
Grenzwerte ($\lim$), etc. haben vordefinierte Befehle.
Lass uns eine Gleichung schreiben: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$\\

Brüche (Zähler / Nenner) können so geschrieben werden:

% 10 / 7
$^{10}/_{7}$

% Komplexere Brüche können so geschrieben werden:
% \frac{Zähler}{Nenner}
$\frac{n!}{k!(n - k)!}$ \\

Wir können Gleichungen auch in einer equation Umgebung verwenden.

% Dies zeigt Mathe in einer equation Umgebung an
\begin{equation} % Aktiviert automatisch den Mathe-Modus.
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % Pythagoras referenzieren
\end{equation} % Alle \begin Befehle müssen einen \end Befehl besitzen

Wir können nun unsere Gleichung referenzieren!
Gleichung ~\ref{eq:pythagoras} ist auch als das Theorem des Pythagoras bekannt. Dieses wird in
Abschnitt ~\ref{subsec:pythagoras} behandelt. Es können sehr viele Sachen mit Labels versehen werden:
Grafiken, Gleichungen, Sections, etc.

Summen und Integrale können mit den sum und int Befehlen dargestellt werden:

% Manche LaTeX-Compiler beschweren sich, wenn Leerzeilen in Gleichungen auftauchen
\begin{equation}
  \sum_{i=0}^{5} f_{i}
\end{equation}
\begin{equation}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation}

\section{Grafiken}

Lass uns eine Grafik einfügen. Das Platzieren von Grafiken kann etwas trickreich sein.
Aber keine Sorge, ich muss auch jedes mal nachschauen, welche Option wie wirkt.

\begin{figure}[H] % H ist die Platzierungsoption
    \centering % Zentriert die Grafik auf der Seite
    % Fügt eine Grafik ein, die auf 80% der Seitenbreite einnimmt.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % Auskommentiert, damit es nicht im Dokument auftaucht.
    \caption{Dreieck mit den Seiten $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabellen}
Wir können Tabellen genauso wie Grafiken einfügen.

\begin{table}[H]
  \caption{Überschrift der Tabelle.}
  % Die {} Argumente geben an, wie eine Zeile der Tabelle dargestellt werden soll.
  % Auch hier muss ich jedes Mal nachschauen. Jedes. einzelne. Mal.  
  \begin{tabular}{c|cc}
    Nummer &  Nachname & Vorname \\ % Spalten werden durch & getrennt
    \hline % Eine horizontale Linie
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

% \section{Links} % Kommen bald!

\section{Verhindern, dass \LaTeX\ etwas kompiliert (z.B. Quelltext)}
Angenommen, wir wollen Quelltext in unserem \LaTeX-Dokument. \LaTeX\ soll
in diesem Fall nicht den Quelltext als \LaTeX-Kommandos interpretieren,
sondern es einfach ins Dokument schreiben. Um das hinzubekommen, verwenden
wir eine verbatim Umgebung.

% Es gibt noch weitere Pakete für Quelltexte (z.B. minty, lstlisting, etc.)
% aber verbatim ist das simpelste.
\begin{verbatim}
  print("Hello World!")
  a%b; % Schau dir das an! Wir können % im verbatim verwenden!
  random = 4; #decided by fair random dice roll
\end{verbatim}

\section{Kompilieren}

Ich vermute, du wunderst dich, wie du dieses tolle Dokument in ein PDF
verwandeln kannst. (Ja, dieses Dokument kompiliert wirklich!) \\

Dafür musst du folgende Schritte durchführen:
  \begin{enumerate}
    \item Schreibe das Dokument. (den \LaTeX -Quelltext).
    \item Kompiliere den Quelltext in ein PDF.
     Das Kompilieren sieht so ähnlich wie das hier aus (Linux): \\
     \begin{verbatim}
        $pdflatex learn-latex.tex learn-latex.pdf
     \end{verbatim}
  \end{enumerate}

Manche \LaTeX-Editoren kombinieren Schritt 1 und 2. Du siehst also nur Schritt 1 und Schritt
2 wird unsichtbar im Hintergrund ausgeführt.

Alle Formatierungsoptionen werden in Schritt 1 in den Quelltext geschrieben. Schritt 2 verwendet
dann diese Informationen und kümmert sich drum, dass das Dokument korrekt erstellt wird.

\section{Ende}

Das war's erst mal!

% Dokument beenden
\end{document}
```
## Mehr Informationen über LateX

* Das tolle LaTeX wikibook: [https://de.wikibooks.org/wiki/LaTeX-Kompendium](https://de.wikibooks.org/wiki/LaTeX-Kompendium)
* Ein Tutorial (englisch): [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
