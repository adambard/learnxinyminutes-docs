---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translators:
	- ["César Suárez Ortega", "http://github.com/csuarez"]
filename: learn-latex-es.tex
---
% Todas los comentarios empiezan con %
% No hay comentarios multi línea.

% LaTeX NO es un procesador de texto "What You See Is What You Get" ("lo qué
% ves es lo que obtienes", como MS Word o OpenOffice Writer

% Todos los comandos LaTeX empiezan con una barra invertida (\)

% Los documentos LaTeX comienzan definiendo el tipo de documento que están
% compilando. Otros tipos de documentos pueden ser libros, informes,
% presentaciones, etc.
% Las opciones del documento aparecen entre los corchetes ([]). En este caso
% se especifica que queremos usar una fuente de tamaño 12pt.
\documentclass[12pt]{article}

% I'm going to include the float and caption packages for figures.
% A continuación definimos los paquetes que va a usar el documento.
% Si quieres añadir imágenes, textos de colores o código fuente de otros
% lenguajes en tu documento, necesitas aumentar las capacidades de LaTeX.
% Esto se hace añadiendo paquetes.
\usepackage{caption}
\usepackage{float}
\usepackage[utf8]{inputenc}

% ¡También podemos definir otro tipo de propiedades del documento!
\author{Chaitanya Krishna Ande, Colton Kohnke,\\ Sricharan Chiruvolu \& César Suárez}
\date{\today}
\title{¡Aprende LaTeX en Y minutos!}

% Ya estamos listos para comenzar el documento.
% Todo lo anterior a esta línea es el llamado "Preámbulo".
\begin{document} 

% Si hemos definido el autor, la fecha y el título, podemos hacer que LaTeX
% cree la portada por nosotros
\maketitle

% La mayoría de artículos de investigación (papers) tienen un resumen, y puedes
% usar comandos ya predefinidos para esto. Eso sí, debe aparecer en un orden
% lógico: después de las cabeceras, pero antes de las secciones principales del
% texto.
% Este comando está disponible en los tipos de documento 'article' y 'report'.
\begin{abstract}
 ¡Documentación sobre LaTeX escrita en LaTeX! ¡Qué original!
\end{abstract}

% Los comandos para crear secciones son muy intuitivos.
% Todos los títulos de las secciones se añaden automáticamente al índice.
\section{Introducción}
Hola, mi nombre es Colton y juntos vamos a explorar LaTeX.

\section{Otra sección}
Este es el texto de otra sección. Creo que necesita una subsección.

\subsection{Esto es una subsección} % Las subsecciones son también intuitivas.
Creo que necesitamos otra.

\subsubsection{Pitágoras}
Mucho mejor ahora.
\label{subsec:pitagoras}

% Usando un asterisco podemos suprimir la numeración que usa LaTeX por defecto.
% Esto también funciona para otros comandos LaTeX.
\section*{Esto es una sección sin numerar} 
¡No todas las secciones tienen porque estar numeradas!

\section{Some Text notes}
LaTeX generalmente es muy bueno poniendo texto donde debería ir. Si una línea \\ necesita \\ crearse \\ añade \textbackslash\textbackslash  a tu código fuente. \\

\section{Listas}
Lists are one of the easiest things to create in Latex! I need to go shopping tomorrow, so let's make a grocery list.
Las listas son una de las cosas más fáciles de crear con LaTeX. Necesito hacer la compra mañana, así que vamos a hacer la lista:
\begin{enumerate} % Esto crea un entorno "enumerate".
  % \item dice a "enumerate" que ha de incrementarse.
  \item Ensalada.
  \item 27 sandías.
  \item Una liebre.
  % Incluso puedes sobreescribir la numeración usando []
  \item[¿Cuántas?] Pistolas de aguas pequeñas.

  No es un item, pero sigue en la lista.

\end{enumerate} % Hay que cerrar todos los entornos.

\section{Matemáticas}

Uno de los principales usos de LaTeX es crear artículos académicos o papers técnicos, generalmente en el ámbito de las matemáticas y las ciencias. Por tanto, necesitamos la posibilidad de añadir símbolos especiales a nuestros artículos. \\

Las matemáticas tienen muchos símbolos, muchos más de los que hay en un teclado: símbolos para representar conjuntos y relaciones, flechas, operadores, letras griegas, etc.\\

Los conjuntos y las relaciones juegan un papel vital en muchos artículos de investigación. Así es como se indica que y pertece a X, $\forall$ x $\in$ X. \\
% Comprueba como se necesita añadir el símbolo $ antes y después de cada
% símbolo. Esto es porque estamos en el "modo texto". Sin embargo, los
% símbolos matemáticos sólo existen en el "modo matemático". Podemos entrar en 
% este modo desde el modo texto con $. Esto es recíproco, una variable también
% puede ser renderizada en el modo matemático.

Mi letra griega favorita es $\xi$. También me gusta $\beta$, $\gamma$ y $\sigma$. Todavía no he encontrado una letra griega que LaTeX no conozca.

Los operadores son esenciales dentro de un documento sobre matemáticas:
funciones trigonométricas ($\sin$, $\cos$, $\tan$),
logaritmos y funciones exponenciales ($\log$, $\exp$),
límites ($\lim$), etc. 
tienen comandos LaTeX predefinidos.
Vamos a escribir una ecuación y veamos cómo queda: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Las fracciones (numerador-denominador) pueden ser escritas de las siguientes formas:

% 10 / 7
$^{10}/_{7}$ 

% Para fracciones complejas se puede formular así
% \frac{numerator}{denominator}
$\frac{n!}{k!(n - k)!}$ \\

También podemos añadir ecuaciones en un "entorno de ecuaciones". 

% Mostramos expresiones matemáticas con el "entorno de ecuaciones"
\begin{equation} % enters math-mode
    c^2 = a^2 + b^2.
    \label{eq:pitagoras} % Para referenciar
\end{equation} % Cualquier elemento \begin debe tener su correspondiente \end

Podemos referenciar nuestra nueva ecuación.
Eqn.~\ref{eq:pitagoras} es también conocido como el Teorema de Pitágoras que también es el tema de la Sección ~\ref{subsec:pitagoras}. Se pueden etiquetar muchas cosas: imágenes, ecuaciones, secciones, etc.

Los sumadores y las integrales se escriben con los comandos sum e int respectivamente:

% Algunos compiladores LaTeX fallan si hay líneas en blanco en el "entorno de
% ecuaciones".
\begin{equation} 
  \sum_{i=0}^{5} f_{i}
\end{equation} 
\begin{equation} 
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation} 

\section{Imágenes}

Vamos a insertar una imagen. El colocar bien una imagen a veces es complicado.
De hecho yo tengo que mirar siempre las opciones de colocación.

\begin{figure}[H] % H indica la opción de colocación. 
    \centering % centra la imagen en la página
    % Añade una imagen escalada con un factor 0.8 respecto al ancho de la página
    %\includegraphics[width=0.8\linewidth]{right-triangle.png} 
    % Comentado para que no falle la compilación. Usa tu imaginación.
    \caption{Triángulo rectángulo con sus lados $a$, $b$, $c$}
    \label{fig:triangulo-rectangulo}
\end{figure}

\subsection{Tablas}
También puedemos añadir tablas de la misma manera que las imágenes.

\begin{table}[H]
  \caption{Subtítulo para la tabla.}
  % Los argumentos entre {} describen como se dibuja cada fila.
  % De nuevo, tengo que mirar esto. Todas. Y. Cada. Una. De. Las. Veces.
  \begin{tabular}{c|cc} 
    Número & Nombre & Apellido \\ % Las columnas de una fila se separan con $
    \hline % una línea horizontal
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

% \section{Enlaces} % Próximamente

\section{Haciendo que LaTeX no compile algo (p.e. código fuente)}
Let's say we want to include some code into our Latex document,
we would then need Latex to not try and interpret that text and
instead just print it to the document. We do this we a verbatim 
environment. 
Digamos que queremos incluir algo de código fuente en nuestro documento LaTeX, así que necesitamos que LaTeX no intente interpretarlo y que simplemente lo muestre en el documento. Esto lo hacemos con el "modo textual".

% Hay otros paquetes que hacen esto (p.e. minty, lstlisting, etc.) pero
% verbatim es el básico sin nada de paja.
\begin{verbatim} 
  print("¡Hola Mundo!")
  a%b; % ¡Mira! Podemos usar símbolos % con verbatim. 
  random = 4; # Decidido con una limpia tirada de dados\end{verbatim}

\section{Compilando} 

Seguramente ahora mismo te estés preguntando como compilar este maravilloso documento y mirar lo glorioso que resulta un PDF hecho con LaTeX.
(Sí, este documento compila). \\
Conseguir un documento final con LaTeX implica los siguientes casos:
  \begin{enumerate}
    \item Escribe el documento en texto planto (el "código fuente").
    \item Compila el código fuente para obtener un PDF. 
     El paso de compilación es algo como esto (en Linux): \\
     \begin{verbatim} 
        $pdflatex learn-latex-es.tex learn-latex-es.pdf 
     \end{verbatim}
  \end{enumerate}

Hay varios editores de LaTeX que combinan los pasos 1 y 2, obligándote a hacer el paso 1 pero no el 2 completamente, ya que este ocurre de manera transparente. 

Resumiendo, en el paso 1 lo que debes hacer es escribir en texto plano el propio texto y la información de formateo. Luego la compilación del paso 2 se encarga de producir un documento con el formato que definiste en el paso 1.

\section{Final}

¡Esto es todo por ahora!

% end the document
\end{document}
```
## Más sobre LaTeX

* El genial wikilibro de LaTeX: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* Un tutorial de verdad: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
