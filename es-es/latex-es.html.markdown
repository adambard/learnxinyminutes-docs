---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translators:
    - ["David Hsieh", "http://github.com/deivuh"]
lang: es-es
filename: learn-latex-es.tex
---

```tex
% Todas las líneas de comentario empiezaon con %
% No hay comentarios de bloque (multiples líneas)

% Latex NO es un procesador de palabras "What You See Is What You Get"
% (Lo que ves es lo que obtienes) como MS Word u OpenOffice Writer

% Cada comando de LaTeX inicia con una barra invertida (\)

% Los documentos de LaTeX inicia con una definición de tipo del documento 
% que compila. Entre otros tipos de documentos se encuentran libros, reportes,
% presentaciones, etc. 
% Las opciones para el documento aparecen dentro de los corchetes []. En
% este caso esepecifica que quieremos utilizar una fuente de 12pt.
\documentclass[12pt]{article}

% Luego definimos los paquetes que utiliza el documento.
% Si deseas incluir gráficas, texto colorido o archivos de código fuente 
% de otro lenguaje en tu documento, necesitas mejorar las capacidades de LaTeX,
% Esto se hace agregando paquetes. 
% Incluiré los paquetes float y caption para utilizar figuras.
\usepackage{caption}
\usepackage{float}

% También podemos definir otras propiedades del documento
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu \& David Hsieh }
\date{\today}
\title{Aprende LaTeX en Y Minutos!}

% Ahora estamos listos para empezar el documento
% Todo lo anterior a esta línea es llamado "El Preámbulo"
\begin{document} 
% Si definimos el autor, la fecha, los títulos, podemos hacer que LaTeX 
% cree una carátula para nosotros.
\maketitle

% La mayoría de los documentos de investigación cuentan con un abstract, puedes
% utilizar comandos predefinidos para esto. Esto debe de aparecer en su
% orden lógico, por lo tanto, después del título, pero antes de las secciones
% principales del cuerpo.
% Este comando está disponible en las clases de documento article y report.
\begin{abstract}
 Documentación de LaTeX escrita en LaTeX! Que novedoso, y definitivamente 
 no fue mi idea!
\end{abstract}

% Los comandos se sección son intuitivos.
% Todos los títulos de las secciones son agregados automáticamente a la tabla 
% de contenido.
\section{Introducción}
Hola, mi nombre es David y juntos vamos a explorar LaTeX!

\section{Otra sección}
Éste es el texto de otra sección. Creo que necesita una subsección.

\subsection{Subsección} % Las subsecciones también son intuitivas.
Creo que necesitamos otra

\subsubsection{Pythagoras}
Ahora está mejor
\label{subsec:pythagoras}

% Al utilizar asteriscos podemos eliminar la enumeración predefinida. 
% Esto funciona también para otros comandos de LaTeX.
\section*{Ésta es una sección sin numeración} 
Sin embargo no todas las secciones serán enumeradas!

\section{Algunas notas de texto}
LaTeX es generalmente bueno para poner el texto en donde debe de ir. Si
una línea \\ necesita \\ terminar \\ agregas \textbackslash\textbackslash al
código fuente. \\
LaTeX is generally pretty good about placing text where it should go. If 
a line \\ needs \\ to \\ break \\ you add \textbackslash\textbackslash to 
the source code. \\ 

\section{Listas}
Las listas son una de las cosas más fáciles de crear en LaTeX! Necesito ir 
de compras mañana, así que hagamos una lista de compras.
\begin{enumerate} % Esto crea un ambiente de "enumeración"
  % \item incrementa la enumeración
  \item Ensalada
  \item 27 sandillas
  \item Una liebre
  % podemos sobreescribir el número de item utilizando []
  \item[Cuántas?] Pistolas de agua

  No es un elemento de la lista, pero aún así es parte de la enumeración.

\end{enumerate} % Todos los ambientes deben de tener un fin.

\section{Matemáticas}

Uno de los primeros usos para LaTeX es producir artículos académicos o 
documentos técnicos. Usualmente en el reino de las ciencias y matemáticas. 
Por lo que necesitamos poder agregar símbolos especiales a nuestro
documento!\\

Las matemáticas tienen muchos símbolos, mucho más de los que puedes encontrar
en un teclado; Símbolos de conjuntos y relación, flechas, operadores, las
letras del alfabeto griego, entre otros.

Los conjuntos y las relaciones tienen un papel importante en los documentos
de investigación matemáticas. Así es como definimos que todo y que pertenece
a X, $\forall$ x $\in$ X. \\

% Ten en cuenta como necesitaba agregar signos $ antes y después de los símbolos.
% Estos es debido a que cuando estamos escribiendo estamos en modo texto.
% Sin embargo, los símbolos matemáticos solo existen en modo matemático.
% Podemos ingresar al modo matemático desde el modo texto con los signos $.
% Esto también se cumple del otro modo. Las variables también pueden ser
% renderizadas en modo matemático.

My letra griega favorita es $\xi$. También me gusta $\beta$, $\gamma$ y $\sigma$.
No he encontrado ninguna letra griega que LaTeX no conozca!

Los operadores son partes esenciales de un documento matemático:
Funciones trigonométricas ($\sin$, $\cos$, $\tan$), 
logaritmos y exponenciales ($\log$, $\exp$), 
límites ($\lim$), etc. 
tienen comandos de LaTeX predefinidos.
Escribamos una ecuación para ver cómo se hace: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Las fracciones(numerador-denominador) pueden ser escritos de la siguiente manera:

% 10 / 7
$^{10}/_{7}$ 

% Las fracciones relativamentes complejas pueden ser escritas como
% \frac{numerador}{denominador}
$\frac{n!}{k!(n - k)!}$ \\

También podemos insertar ecuaciones en un "ambiente de ecuación".

% Mostrar matemática con el 'ambiente' de ecuación.
\begin{equation} % Ingresa al modo matemático
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % para referencia
\end{equation} % todas las declaraciones \begin deben de tener una declaración
               % de fin

Luego podemos hacerle referencia a nuestra nueva ecuación!
Eqn.~\ref{eq:pythagoras} también es conocido como el Teorema de Pitágoras
que también es el sujeto de Sec.~\ref{subsec:pythagoras}. Muchas cosas
se pueden etiquetar:
figuras, ecuaciones, secciones, etc.

Las sumatorias y las integrales se escriben con los comandos sum e int:

% Algunos compiladores de LaTeX se quejarán si hay líneas en blanco
% en un ambiente de ecuación
\begin{equation} 
  \sum_{i=0}^{5} f_{i}
\end{equation} 
\begin{equation} 
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation} 

\section{Figuras}

Agreguemos una figura. El reemplazo de figuras puede tener truco, 
por lo que cada vez que los uso busco cuáles son las opciones que tiene.

\begin{figure}[H] % Aquí H es denotado con la opción colocación.
    \centering % Centra la figura figura en la página
    % Inserta una figura a una escala de 0.8 con el ancho de la página.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png} 
    % Comentado para propósitos de compilación. Por favor usa tu imaginación.
    \caption{Triángulo rectángulo con lados $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabla}
También podemos agregar tablas de la misma manera que las figuras.

\begin{table}[H]
  \caption{Leyenda para la tabla.}
  % Los argumentos {} a continuación describen como cada fila de la 
  % tabla es graficada
  \begin{tabular}{c|cc} 
    Número &  Apellido & Nombre \\ % Las columnas son separadas por &
    \hline % una línea horizontal
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

\section{Haciendo que latex no compile algo (i.e. Código fuente)}
Digamos que queremos incluir un poco de código a nuestro documento de LaTeX.,
necesitaríamos que LaTeX no intente interpretar ese texto y que en lugar
de eso solo lo imprima en el documento. Podemos hacer esto en un ambiente
verbatim.

% Existen otros paquetes (i.e. minty, lstlisting, etc.) pero 
% verbatim es el más básico.
\begin{verbatim} 
  print("Hola mundo!")
  a%b; % mira! Podemos usarlos signos % en verbatim. 
  aleatorio = 4; #decido al tirar los dados.
\end{verbatim}

\section{Compilando} 

Hasta ahora probablemente te preguntarás cómo compilar el código de 
este documento fabuloso y ver lo glorioso que es un pdf de LaTeX.
(Sí, el documento sí se compila). \\
Llegando al último documento usando LaTeX consiste en los siguientes pasos:
  \begin{enumerate}
    \item Escribir el documento en texto plano (el "código fuente").
    \item Compilar el código fuente para producir un pdf. 
     El paso de compilación se ve algo así (en Linux)): \\
     \begin{verbatim} 
        $pdflatex learn-latex-es.tex learn-latex-es.pdf 
     \end{verbatim}
  \end{enumerate}

Una cantidad de editores LaTeX combina los pasos 1 y 2 en en una misma pieza
de software. Así que, llegas a ver el paso 1, pero no el paso 2 completamente.
El paso 2 todavía ocurre detrás de las escenas.

Escribes toda la información de formato en texto plano en el paso 1.
La compilación de la parte del paso 2 se encarga de producir el documento
 en el formato que definiste en el paso 1.

\section{Fin}

Eso es todo por ahora!

% Termina el documento
\end{document}
```

## Más sobre LaTeX

* The amazing LaTeX wikibook: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* An actual tutorial: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
