---
language: latex
lang: es-es
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translators:
    - ["Mario Pérez", "https://github.com/MarioPerezEsteso"]
filename: learn-latex-es.tex
---

```tex
% Todas las líneas comentadas comienzan con %
% No existen los comentarios multilínea

% LaTeX NO es un software de procesamiento de texto que cumple con
% "Lo que ves es lo que tienes" como MS Word u OpenOffice

% Todos los comandos de LaTeX comienzan con una contrabarra (\)

% Los documentos LaTeX comienzan definiendo el tipo de documento que se va a
% compilar. Algunos tipos de documentos son libros, informes, presentaciones,
% etc. Las opciones para el documento comienzan en los corchetes []. En este
% caso, se especifica que queremos utilizar una fuente de tamaño 12pt.
\documentclass[12pt]{article}

% A continuación, definimos los paquetes que utilizará el documento.
% Si quieres incluir gráficos, texto coloreado o código fuente de otro lenguaje,
% debes extender las funciones de LaTeX. Esto se consigue añadiendo paquetes.
% A continuación se incluirán los paquetes float y caption para figuras.
\usepackage{caption}
\usepackage{float}

% También podemos definir otras propiedades en el documento
\author{Chaitanya Krishna Ande, Colton Kohnke \& Sricharan Chiruvolu}
\date{\today}
\title{Learn LaTeX in Y Minutes!}

% Ahora estamos preparados para comenzar el documento
% Todo lo que se encuentre antes de esta línea se llama "El Preámbulo"
\begin{document}
% Si especificamos el autor, fecha y título, LaTeX creará una página como título
% por nosotros
\maketitle

% La mayoría de los artículos de investigación tienen un abstract. Es posible
% utilizar comandos predefinidos para ello.
% Esto debería aparecer en su orden lógico. Tras el título pero antes de las
% secciones principales del cuerpo.
% Este comando está disponible en los tipos de documentos article y report.
\begin{abstract}
 Documentación de LaTex escrita en LaTex.
\end{abstract}

% Los comandos de sección son intuitivos.
% Todos los títulos de secciones son añadidos automáticamente a la tabla de contenidos.
\section{Introducción}
Hola, mi nombre es Mario Pérez y estoy traduciendo este documento para aprender LaTex.

\section{Otra sección}
Este es el texto de otra sección. Creo que necesitará una subsección.

\subsection{Esto es una subsección} % Las subsecciones también son fáciles.
Creo que necesitamos otra más.

\subsubsection{Pitágoras}
Mejor ahora.
\label{subsec:pitagoras}

% Utilizando el asterisco podemos decirle a LaTeX que no ponga los números de secciones y subsecciones.
% Esto también funciona con otros comandos de LaTeX.
\section*{Esto es una sección no numerada}
¡No todas las secciones tienen que estar numeradas!

\section{Algunas notas}
LaTeX es generalmente bastante bueno situando el texto donde debe ir. Si una lína \\ necesita \\ ser \\ rota \\ puedes poner \textbackslash\textbackslash en el código fuente. \\

\section{Listas}
Las listas son de las cosas más fáciles de crear en LaTeX. Necesito ir a comprar mañana, así que vamos a crear una lista de la compra.
\begin{enumerate} % Esto crea una lista numerada.
  % \item crea un elemento
  \item Ensalada.
  \item 27 sandías.
  \item Pescado.
  % podemos incluso sobreescribir el número del ítem usando []
  \item[cuántos?] Plátanos.

  No es un ítem de la lista, pero sigue siendo parte de la enumeración.

\end{enumerate} % Todos los contextos deben tener un final.

\section{Matemáticas}

Uno de los usos principales de LaTeX es la producción de artículos académicos o técnicos. Normalmente relacionados con la ciencia y las matemáticas. Debido a esto, necesitamos poder añadir símbolos especiales a nuestro artículo.\\

En matemáticas hay muchos símbolos. Más de los que podemos encontrar en un teclado. Flechas o letras por nombrar un par.\\

Algunos símbolos juegan un papel fundamental en muchos artículos de investigación matemática. Así es como se establece que todo Y pertenece a X:  $\forall$ x $\in$ X. \\
He necesitado añadir el signo $ antes de los símbolos. Esto se debe a que cuando escribimos, estamos en modo texto. Sin embargo, los símbolos solo pueden utilizarse en modo matemático, al cual se entra con el signo $.
% Lo opuesto también se cumple. Una variable también puede ser mostrada en modo matemático, al que también se puede entrar con \[\]

\[a^2 + b^2 = c^2 \]

Mi letra griega favorita es $\xi$. También me gustan $\beta$, $\gamma$ y $\sigma$.
Todavía no he encontrado una letra griega que LaTeX no conozca.

Los operadores son también una parte esencial de un documento matemático: 
funciones trigonométricas ($\sin$, $\cos$, $\tan$), logaritmos y exponenciales ($\log$, $\exp$), límites ($\lim$), etc. tienen comandos predefinidos en LaTeX.

Vamos a escribir una ecuación para ver cómo se hace: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Las fracciones (numeradores-denominadores) pueden escribirse de la siguiente forma:

% 10 / 7
$^{10}/_{7}$

% Las fracciones relativamente complejas pueden escribirse como
% \frac{numerador}{denominador}
$\frac{n!}{k!(n - k)!}$ \\

También podemos insertar ecuaciones en un contexto de ecuación.

% Mostrar matemáticas en el contexto de ecuaciones
\begin{equation} % entra en modo matemático
    c^2 = a^2 + b^2.
    \label{eq:pitagoras} % para referencias
\end{equation} % Todos los contextos deben tener un final.

Podemos referenciar nuestra nueva ecuación.
Ecuación ~\ref{eq:pythagoras} también se conoce como el Teorema de Pitágoras, el cual también se encuentra en la sección ~\ref{subsec:pythagoras}. Muchas cosas pueden ser etiquetadas: figures, equations, sections, etc.

Los sumatorios e integrales son escritor son los comandos sum e int:

% Algunos compiladores de LaTeX se quejarán si hay líneas en blanco
% En un contexto de ecuación.
\begin{equation}
  \sum_{i=0}^{5} f_{i}
\end{equation}
\begin{equation}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation}

\section{Figuras}

Vamos a insertar una figura. Situarla puede ser algo complicado.

\begin{figure}[H] % H aquí establece la situación de la figura.
    \centering % centra la figura en la página
    % Inserta una figura escalada por 0.8 el ancho de la página.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % La línea anterior ha sido comentada para poder compilar este archivo. Por favor, usa tu imaginación.
    \caption{Triángulo con lados $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tablas}
También podemos insertar tablas de la misma manera que las figuras.

\begin{table}[H]
  \caption{Título para la tabla.}
  % los argumentos en {} describen cómo cada fila va a ser representada.
  \begin{tabular}{c|cc}
    Número & Nombre & Apellido \\
    \hline % una línea horizontal
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

% \section{Hyperlinks} % En construcción

\section{Haciendo que LaTeX no compile algo (por ejemplo, código fuente)}
Digamos que queremos incluir código fuente dentro de nuestro documento LaTex. En ese caso, debemos indicarle a LaTeX que no trate de compilarlo y simplemente lo muestre en el documento. Esto lo realizamos en el contexto verbatim.

% Hay otros paquetes para esta misma tarea, pero verbatim es el más básico.
\begin{verbatim}
  print("Hola Mundo!")
  a%b; % Podemos usar los signos % en verbatim.
  aleatorio = 4; # Número aleatorio
\end{verbatim}

\section{Compilación}

Ahora mismo te estarás preguntando cómo compilar este fabuloso documento y obtener un documento PDF.\\
Para obtener el documento final utilizando LaTeX hay que seguir los siguientes pasos:
  \begin{enumerate}
    \item Escribe el documento en texto plano.
    \item Compila el código para producir un PDF.
     Los pasos de compilación serán algo parecido a esto (en Linux): \\
     \begin{verbatim}
        $pdflatex learn-latex.tex learn-latex.pdf
     \end{verbatim}
  \end{enumerate}

Un gran número de editores LaTeX combinan ambos pasos para que sea más sencillo obtener el documento.

Escribe toda la información de formato en el paso 1 y con el paso 2 obtendrás el documento que has definido en el paso anterior.

\section{End}

Esto es todo por ahora.

% fin del documento
\end{document}
```

## Más información sobre LaTeX

* El wikilibro LaTeX: [https://es.wikibooks.org/wiki/Manual_de_LaTeX](https://es.wikibooks.org/wiki/Manual_de_LaTeX)
* Un tutorial real: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
