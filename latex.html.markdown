---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "https://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
    - ["Ramanan Balakrishnan", "https://github.com/ramananbalakrishnan"]
    - ["Svetlana Golubeva", "https://attillax.github.io/"]
    - ["Oliver Kopp", "http://orcid.org/0000-0001-6962-4290"]
filename: learn-latex.tex
---

```tex
% All comment lines start with %
% There are no multi-line comments

% LaTeX is NOT a "What You See Is What You Get" word processing software like
% MS Word, or OpenOffice Writer

% Every LaTeX command starts with a backslash (\)

% LaTeX documents start with a defining the type of document it's compiling
% Other document types include book, report, presentations, etc.
% The options for the document appear in the [] brackets. In this case
% it specifies we want to use 12pt font.
\documentclass[12pt]{article}

% Next we define the packages the document uses.
% If you want to include graphics, colored text, or
% source code from another language file into your document,
% you need to enhance the capabilities of LaTeX. This is done by adding packages.
% I'm going to include the float and caption packages for figures
% and hyperref package for hyperlinks
\usepackage{caption}
\usepackage{float}
\usepackage{hyperref}

% We can define some other document properties too!
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu \& \\
Svetlana Golubeva}
\date{\today}
\title{Learn \LaTeX{} in Y Minutes!}

% Now we're ready to begin the document
% Everything before this line is called "The Preamble"
\begin{document}
% if we set the author, date, title fields, we can have LaTeX
% create a title page for us.
\maketitle

% If we have sections, we can create table of contents. We have to compile our
% document twice to make it appear in right order.
% It is a good practice to separate the table of contents form the body of the
% document. To do so we use \newpage command
\newpage
\tableofcontents

\newpage

% Most research papers have abstract, you can use the predefined commands for this.
% This should appear in its logical order, therefore, after the top matter,
% but before the main sections of the body.
% This command is available in the document classes article and report.
\begin{abstract}
 \LaTeX{} documentation written as \LaTeX! How novel and totally not
 my idea!
\end{abstract}

% Section commands are intuitive.
% All the titles of the sections are added automatically to the table of contents.
\section{Introduction}
Hello, my name is Colton and together we're going to explore \LaTeX!

\section{Another section}
This is the text for another section. I think it needs a subsection.

\subsection{This is a subsection} % Subsections are also intuitive.
I think we need another one.

\subsubsection{Pythagoras}
Much better now.
\label{subsec:pythagoras}

% By using the asterisk we can suppress LaTeX's inbuilt numbering.
% This works for other LaTeX commands as well.
\section*{This is an unnumbered section}
However not all sections have to be numbered!

\section{Some Text notes}
%\section{Spacing} % Need to add more information about space intervals
\LaTeX{} is generally pretty good about placing text where it should
go. If
a line \\ needs \\ to \\ break \\ you add \textbackslash\textbackslash{}
to the source code.

Separate paragraphs by empty lines.

You need to add a tilde after abbreviations (if not followed by a comma) for a
non-breaking space, because otherwise the spacing after the dot is too large:
E.g., i.e., etc.~are are such abbreviations.

\section{Lists}
Lists are one of the easiest things to create in \LaTeX! I need to go shopping
tomorrow, so let's make a grocery list.
\begin{enumerate} % This creates an "enumerate" environment.
  % \item tells the enumerate to increment
  \item Salad.
  \item 27 watermelon.
  \item A single jackrabbit.
  % we can even override the item number by using []
  \item[how many?] Medium sized squirt guns.

  Not a list item, but still part of the enumerate.

\end{enumerate} % All environments must have an end.

\section{Math}

One of the primary uses for \LaTeX{} is to produce academic articles
or technical papers. Usually in the realm of math and science. As such,
we need to be able to add special symbols to our paper!

Math has many symbols, far beyond what you can find on a keyboard;
Set and relation symbols, arrows, operators, and Greek letters to name a few.

Sets and relations play a vital role in many mathematical research papers.
Here's how you state all x that belong to X, $\forall x \in X$.
% Notice how I needed to add $ signs before and after the symbols. This is
% because when writing, we are in text-mode.
% However, the math symbols only exist in math-mode.
% We can enter math-mode from text mode with the $ signs.
% The opposite also holds true. Variable can also be rendered in math-mode.
% We can also enter math mode with \[\]

\[a^2 + b^2 = c^2 \]

My favorite Greek letter is $\xi$. I also like $\beta$, $\gamma$ and $\sigma$.
I haven't found a Greek letter yet that \LaTeX{} doesn't know
about!

Operators are essential parts of a mathematical document:
trigonometric functions ($\sin$, $\cos$, $\tan$),
logarithms and exponentials ($\log$, $\exp$),
limits ($\lim$), etc.~have pre-defined LaTeX commands.
Let's write an equation to see how it's done:
$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Fractions (Numerator-denominators) can be written in these forms:

% 10 / 7
$$ ^{10}/_{7} $$

% Relatively complex fractions can be written as
% \frac{numerator}{denominator}
$$ \frac{n!}{k!(n - k)!} $$

We can also insert equations in an ``equation environment''.

% Display math with the equation 'environment'
\begin{equation} % enters math-mode
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % for referencing
\end{equation} % all \begin statements must have an end statement

We can then reference our new equation!
Eqn.~\ref{eq:pythagoras} is also known as the Pythagoras Theorem which is also
the subject of Sec.~\ref{subsec:pythagoras}. A lot of things can be labeled:
figures, equations, sections, etc.

Summations and Integrals are written with sum and int commands:

% Some LaTeX compilers will complain if there are blank lines
% In an equation environment.
\begin{equation}
  \sum_{i=0}^{5} f_{i}
\end{equation}
\begin{equation}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation}

\section{Figures}

Let's insert a figure. Figure placement can get a little tricky.
Basic options are [t] for top, [b] for bottom, [h] for here (approximately).
I definitely have to lookup the placement options each time.
% See https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions for more details

\begin{figure}[H] % H here denoted the placement option.
    \centering % centers the figure on the page
    % Inserts a figure scaled to 0.8 the width of the page.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % Commented out for compilation purposes. Please use your imagination.
    \caption{Right triangle with sides $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Table}
We can also insert Tables in the same way as figures.

\begin{table}[H]
  \caption{Caption for the Table.}
  % the {} arguments below describe how each row of the table is drawn.
  % The basics are simple: one letter for each column, to control alignment:
  % basic options are: c, l, r and p for centered, left, right and paragraph
  % optionally, you can add a | for a vertical line
  % See https://en.wikibooks.org/wiki/LaTeX/Tables for more details
  \begin{tabular}{c|cc}  % here it means "centered | vertical line, centered centered"
    Number &  First Name & Last Name \\ % Column rows are separated by &
    \hline % a horizontal line
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
  % it will approximately be displayed like this
  % Number | First Name     Last Name
  % -------|---------------------------  % because of \hline
  %   1    |   Biggus        Dickus
  %   2    |   Monty         Python
\end{table}

\section{Getting \LaTeX{} to not compile something (i.e.~Source Code)}
Let's say we want to include some code into our \LaTeX{} document,
we would then need \LaTeX{} to not try and interpret that text and
instead just print it to the document. We do this with a verbatim
environment.

% There are other packages that exist (i.e. minty, lstlisting, etc.)
% but verbatim is the bare-bones basic one.
\begin{verbatim}
  print("Hello World!")
  a%b; % look! We can use % signs in verbatim.
  random = 4; #decided by fair random dice roll, https://www.xkcd.com/221/
  See https://www.explainxkcd.com/wiki/index.php/221:_Random_Number
\end{verbatim}

\section{Compiling}

By now you're probably wondering how to compile this fabulous document
and look at the glorious glory that is a \LaTeX{} pdf.
(Yes, this document actually does compile).

Getting to the final document using \LaTeX{} consists of the following
steps:
  \begin{enumerate}
    \item Write the document in plain text (the ``source code'').
    \item Compile source code to produce a pdf.
     The compilation step looks like this (in Linux): \\
     \begin{verbatim}
        > pdflatex learn-latex.tex
     \end{verbatim}
  \end{enumerate}

A number of \LaTeX{} editors combine both Step 1 and Step 2 in the
same piece of software. So, you get to see Step 1, but not Step 2 completely.
Step 2 is still happening behind the scenes\footnote{In cases, where you use
references (like Eqn.~\ref{eq:pythagoras}), you may need to run Step 2
multiple times, to generate an intermediary *.aux file.}.
% Also, this is how you add footnotes to your document!
% with a simple \footnote{...} command. They are numbered ¹, ², ... by default.

You write all your formatting information in plain text in Step 1.
The compilation part in Step 2 takes care of producing the document in the
format you defined in Step 1.

\section{Hyperlinks}
We can also insert hyperlinks in our document. To do so we need to include the
package hyperref into preamble with the command:
\begin{verbatim}
    \usepackage{hyperref}
\end{verbatim}

There exists two main types of links: visible URL \\
\url{https://learnxinyminutes.com/docs/latex/}, or
\href{https://learnxinyminutes.com/docs/latex/}{shadowed by text}
% You can not add extra-spaces or special symbols into shadowing text since it
% will cause mistakes during the compilation

This package also produces list of thumbnails in the output PDF document and
active links in the table of contents.

\section{Writing in ASCII or other encodings}

By default, historically LaTeX accepts inputs which are pure ASCII (128),
but not extended ASCII, meaning without accents (à, è etc.) and non-Latin symbols.

It is easy to insert accents and basic Latin symbols, with backslash shortcuts
Like \,c, \'e, \`A, \ae and \oe etc.  % for ç, é, À, etc
% See https://en.wikibooks.org/wiki/LaTeX/Special_Characters#Escaped_codes for more

To write directly in UTF-8, when compiling with pdflatex, use
\begin{verbatim}
    \usepackage[utf8]{inputenc}
\end{verbatim}
The selected font has to support the glyphs used for your document, you have to add
\begin{verbatim}
    \usepackage[T1]{fontenc}
\end{verbatim}

Since LuaTeX and XeLaTeX were designed with built-in support for UTF-8, making
life easier for writing in non-Latin alphabets.

\section{End}

That's all for now!

% Most often, you would want to have a references section in your document.
% The easiest way to set this up would be by using the bibliography section
\begin{thebibliography}{1}
  % similar to other lists, the \bibitem command can be used to list items
  % each entry can then be cited directly in the body of the text
  \bibitem{latexwiki} The amazing \LaTeX{} wikibook: \emph{https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} An actual tutorial: \emph{http://www.latex-tutorial.com}
\end{thebibliography}

% end the document
\end{document}
```

## More on LaTeX

* The amazing LaTeX Wikibook: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* An actual tutorial: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
* A quick guide for learning LaTeX: [Learn LaTeX in 30 minutes](https://www.overleaf.com/learn/latex/Learn_LaTeX_in_30_minutes)
* An interactive platform to learn LaTeX (installationfree) [learnlatex.org/](https://www.learnlatex.org/)
* Stack Exchange's question and answer site about TeX, LaTeX, ConTeXt, etc. [tex.stackexchange.com](https://tex.stackexchange.com/)
