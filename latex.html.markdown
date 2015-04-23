---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
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

% Getting to the final document using LaTeX consists of the following steps:
% 1. Write the document in plain text
% 2. Compile plain text document to produce a pdf
%    The compilation step looks something like this:
%    $ pdflatex your-tex-file.tex your-tex-file.pdf
% A number of LaTeX editors combine both Step 1 and Step 2 in the same piece of
% software. So, you get to see Step 1, but not Step 2 completely.
% Step 2 is still happening behind the scenes.

% You write all your formatting information in plain text in Step 1.
% The compilation part in Step 2 takes care of producing the document in the
% format you defined in Step 1.

% For Step 1, it is best if you get a good text editor
% On Windows, probably Notepad++
% For Step 2, you will need to get a TeX distribution
% Windows: MikTeX
% MacOS: MacTeX
% Linux: Should be available from your package manager

% Let's get to the final pdf document as soon as possible

% Choose the kind of document you want to write.
% You can replace article with book, report, etc.
\documentclass{article}
% begin the document
\begin{document}
% end the document
\end{document}
% Compile to pdf
% Now, you already have a final document which of course it is empty.
% Everything that you write is between the
% \begin{document}
% \end{document}

% Start a new document from here.
% Let's do a decent document
\documentclass{article}
% required for inserting images
\usepackage{graphicx} 
% begin the document
\begin{document}
% set the title (optional)
\title{Title of the document}
% set the author (optional)
\author{Chaitanya Krishna Ande}

% make the title (optional)
\maketitle

% start the first section
\section{Introduction}

% write your text
This is the introduction.

% start another section
\section{Another section}
This is the text for another section.

% another section with subsection
\section{Section with sub-section}
Text for the section.
\subsection{Sub-section}
Let's discuss the Pythagoras theorem.
\subsubsection{Pythagoras Theorm}
% for cross-reference
\label{subsec:pythagoras}

% notice how the sections and sub-sections are automatically numbered

% Some math
% Inline math within $ $
For a right angled triangle (see Fig.~\ref{fig:right-triangle}) with sides $a$, $b$ and $c$, where $c$ is the
hypotenuse, the following holds:
% Display math with the equation 'environment'
\begin{equation}
    c^2 = a^2 + b^2.
    % for cross-reference
    \label{eq:pythagoras}
\end{equation}

% Let's cross-reference the equation
Eqn.~\ref{eq:pythagoras} is also known as the Pythagoras Theorem which is also
the subject of Sec.~\ref{subsec:pythagoras}.

\subsubsection{Figure}
Let's insert a Figure.

\begin{figure}
    \centering
    \includegraphics[width=0.8\linewidth]{right-triangle.png}
    \caption{Right triangle with sides a, b, c}
    \label{fig:right-triangle}
\end{figure}


\subsubsection{Table}
Let's insert a Table.

\begin{table}
\caption{Caption for the Table.}
\begin{tabular}{ccc}
Number &  Last Name & First Name \\
\hline
1 & Biggus & Dickus \\
2 & Monty & Python
\end{tabular}
\end{table}

% end the document
\end{document}
```
