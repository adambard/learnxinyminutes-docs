---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
filename: learn-latex.tex
---

LaTeX is known to create aesthetically pleasing documents without you worrying
about the formatting. It is also great if one wants to create documents
containing a lot of mathematics. Getting a good document is very easy, but
getting it to behave exactly the way you want can be a bit hairy.


```latex

% All comment lines start with %
% There are no multi-line comments

% LaTeX is NOT a ``What You See Is What You Get'' word processing software like
% MS Word, or OpenOffice Writer

% LaTeX documents start with a defining the type of document it's compiling
% Other document types include book, report, presentations, etc. 
\documentclass[12pt]{article}

% Next we define the packages the document uses.
% I'm going to include the float and caption packages for figures.
\usepackage{caption}
\usepackage{float}

% We can define some other document properties too!
\author{Chaitanya Krishna Ande \& Colton Kohnke}
\date{\today}
\title{Learn LaTeX in Y Minutes!}

% Now we're ready to begin the document
% Everything before this line is called "The Preamble"
\begin{document} 
% if we set the author, date, title fields, we can have LaTeX 
% create a title page fo us.
\maketitle

\section{Introduction}
Hello, my name is Colton and together we're going to explore LaTeX !

\section{Another section}
This is the text for another section. I think it needs a subsection.

\subsection{This is a subsection}
I think we need another one

\subsubsection{Pythagoras}
Much better now.
\label{subsec:pythagoras}

\section*{This is an unnumbered section}
However not all sections have to be numbered!

\section{Some Text notes}
LaTeX is generally pretty good about placing text where it should go. If 
a line \\ needs \\ to \\ break \\ you add \textbackslash\textbackslash to 
the text. In case you haven't noticed the \textbackslash is the character
the tells the LaTeX compiler it should pay attention to what's next.

\section{Math}

One of the primary uses for LaTeX is to produce academic article or 
technical papers. Usually in the realm of math and science. As such, 
we need to be able to add special symbols to our paper! \\

My favorite Greek letter is $\xi$. I also like $\beta$, $\gamma$ and $\sigma$.
Notice how I needed to add \$ signs before and after the symbols. This is 
because when writing, we are in text-mode. However, the math symbols only exist
in math-mode. We can enter math-mode from text mode with the \$ signs. 
The opposite also holds true. Variable can also be rendered in math-mode. \\

% We can also add references
For a right angled triangle (see Fig.~\ref{fig:right-triangle}) with sides $a$,
 $b$ and $c$, where $c$ is the hypotenuse, the following holds:
% Display math with the equation 'environment'
\begin{equation} % enters math-mode
    c^2 = a^2 + b^2.
    % for cross-reference
    \label{eq:pythagoras}
\end{equation} % all \begin statments must have an end statement

Eqn.~\ref{eq:pythagoras} is also known as the Pythagoras Theorem which is also
the subject of Sec.~\ref{subsec:pythagoras}.


\section{Figures}

Let's insert a Figure. Figure placement can get a little tricky. 
I definately have to lookup the placement options each time.

\begin{figure}[H]
    \centering
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % Commented out for compilation purposes. Use your imagination.
    \caption{Right triangle with sides $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Table}
Let's insert a Table.

\begin{table}[H]
  \caption{Caption for the Table.}
  \begin{tabular}{ccc}
    Number &  Last Name & First Name \\
    \hline
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}


\section{Compiling} 

By now you're probably wondering how to compile this fabulous document 
(yes, it actually compiles). \\
Getting to the final document using LaTeX consists of the following steps:
  \begin{enumerate} % we can also created numbered lists!
    \item Write the document in plain text
    \item Compile plain text document to produce a pdf. 
     The compilation step looks something like this: \\
     % Verbatim tells the compiler to not interpret.
     \begin{verbatim} 
        $pdflatex learn-latex.tex learn-latex.pdf 
     \end{verbatim}
  \end{enumerate}

A number of LaTeX editors combine both Step 1 and Step 2 in the same piece of
software. So, you get to see Step 1, but not Step 2 completely.
Step 2 is still happening behind the scenes.

You write all your formatting information in plain text in Step 1.
The compilation part in Step 2 takes care of producing the document in the
format you defined in Step 1.

\section{End}

That's all for now!

% end the document
\end{document}
```
## More on LaTeX

* The amazing LaTeX wikibook: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* An actual tutorial: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)


