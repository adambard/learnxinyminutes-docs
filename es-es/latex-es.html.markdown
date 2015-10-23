---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translators:
    - ["Zach Larsen", "http://zachariahlarsen.com"]
lang: es-es
filename: learn-latex-es.tex
---

```tex
% Todo lineas de comentos empiezan con %
% No hay comentos de mas de una linea

% LaTeX NO es "Lo que ve es lo que recibe" software de procesamiento de textos como
% MS Word, o OpenOffice Writer

% Cada LaTeX mando empieza con una barra invertida (\)

% LaTeX documentos empiezan con una definición del tipo 
% de documento de lo que esta compilando
% Otro tipos de documentos incluye libro, articulo, presentaciones, etc.
% los opciones por el documento aparece en los [] soportes. En este caso
% especifica que queremos usar 12pt tipo de letra.
\documentclass[12pt]{article}

% Próximo definamos los paquetes el documento usa.
% Si quiere incluir gráficos, texto de color, o
% código de fuente del otro lenguaje en su documento,
% necesita mejorar los capacidades de LaTeX. Esto se hace con añadiendo paquetes. 
% Voy a incluir el float y caption paquetes por figures.
\usepackage{caption}
\usepackage{float}

% Podems definir otro propiedades del documento también!
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu \& Zach Larsen}
\date{\today}
\title{Aprender LaTeX en Y Minutos!}

% Ahora estamos listo para empezar el documento
% Todo antes de este linea se llama "The Preamble"
\begin{document} 
% Si fijamos el autor, fecha, y titulo espacios LaTeX crea 
% una pagina de titulo por nosotros.
\maketitle

% Casi todo artículos de investigación tienen abstracto,
% puede usar los mandos predefinidos por esto.
% Este debe aparecer en el orden lógico después del parte de arriba,
% pero después los secciones del cuerpo.
% Este mando es disponible en los clases de documento artículos.
\begin{abstract}
 LaTeX documentación escrito en LaTeX! Que novel y totalmente no es mi idea!
\end{abstract}

% Sección mandos son intuitivo. 
% Todo los titulos de los secciones se agregan automáticamente al table de contentos.
\section{Introduction}
Hola, mi nombre es Zach y juntos exploráramos LaTeX!

\section{Otro section}
Aquí es el texto por otro sección. Creo que necesita una subsección.

\subsection{Esta es una subsección } % Subsección son intuitivo también.
Creo que necesitamos otro
\subsubsection{Pythagoras}
Mucho mejor.
\label{subsec:pythagoras}

% Cuando usamos el asterisco podemos suprimir LaTeX's incorporado números.
% Funciona por otro LaTeX mandos también. 
\section*{Esta es una sección sin numero} 
De todos modos no todos secciones necesitan números!

\section{Algunos notas de Texto}
LaTeX generalmente funciona muy bien cuando ponga texto donde debe ir. Si 
una linea \\ necesita \\ romper \\ agrega \textbackslash\textbackslash al
código fuente. \\ 

\section{Listas}
Listas son una de las cosas mas facil para crear en LaTeX! necesito salir a comprar
mañana, entonces creamos una lista de compras,
\begin{enumerate} % Este crea un "enumerate" entorno.
  % \item manda el enumerate a incrementar
  \item Ensalada.
  \item 27 sandias.
  \item una liebre.
  % Aun podemos anular el ítem numero cuando usamos []
  \item[Cuantos?] Mediano pistola de agua.

  No es un ítem de la lista, pero todovia puede enumerar.

\end{enumerate} % Todo entornos necesitan terminar.

\section{Matemáticas}

Uno de los usos primarios de LaTeX es producir artículos académicos o
artículos técnicos . Usualmente en el reino de las ciencias . Entonces, 
necesitamos agregar símbolos especiales a nuestro articulo! \\

Matemáticas tiene mucho símbolos, mucho mas de lo que puede encontrar en su tecla;
Set y relación símbolos, flechas, operadores, y letras Griego para nombre algunos\\

Sets y relaciones son vital en muchos articulos de matemáticas.
Asi es como se dice que todo y pertenece a X, $\forall$ x $\in$ X. \\
% Noticie como necesitamos agregar signos de $ antes de después los símbolos. Este es 
% porque cuando esta escribiendo, estamos en “text-mode” modo de texto. 
% Los símbolos de matemáticas solo existen en “math-mode” modo de matemáticas. 
% Podemos entrar “math-mode” desde “text-mode” con los signos de $.
% El opuesto es verdad también. Variables tambien pueden hacer en “math-mode”.

Mi letra Griega preferida es $\xi$. Tambien a mi me gusta $\beta$, $\gamma$ y $\sigma$.
No he podido encontrar una letra Griega que Latex no conoce!

Operadores son partes esenciales de un documento de matemática: 
funciones de trigonométrico ($\sin$, $\cos$, $\tan$), 
logaritmos y exponenciales ($\log$, $\exp$), 
limites ($\lim$), etc. 
tienen mandos LaTeX predefinidos. 
Escribamos una ecuación para ver como se hace: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Fracciones(Numerador-denominadores) pueden escribir así:

% 10 / 7
$^{10}/_{7}$ 

% Relativamente complejo fracciones pueden escribir así:
% \frac{numerator}{denominator}
$\frac{n!}{k!(n - k)!}$ \\

También podemos agregar ecuaciones en un "equation environment" entorno de ecuación . 

% Muestra matemáticas con la ecuación “equation” entorno
\begin{equation} % entra “math-mode”
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % por referencia
\end{equation} % todo \begin declaraciones necesitan terminar con una “end” declaración

Ahora podemos referirnos a este nuevo ecuación!
Eqn.~\ref{eq:pythagoras} también se conoce como la teorema de Pythagoras que 
también es el sujeto de Sec.~\ref{subsec:pythagoras}. Muchas cosas pueden recibir etiqueta: 
figuras, ecuaciones, secciones, etc.

Sumas y Integrales son escrito con mandos “sum” y “int”:

% Algunos LaTeX compiladores se quejan si hay lineas blanco
% En una ecuación entorno.
\begin{equation} 
  \sum_{i=0}^{5} f_{i}
\end{equation} 
\begin{equation} 
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation} 

\section{Figuras}

Vamos a agregar una Figura. Colocación de una figura puede ser difícil.
Cada vez tengo que buscar los opciones de colocación.

\begin{figure}[H] % H es el opción de colocación.
    \centering % centra la figura en la pagina
    % Agrega una figura escalado a 0.8 de la anchura de la pagina.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png} 
    % Esta comentado para que podamos compilar. Use su imaginación por favor.
    \caption{triángulo derecho con lados $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabla}
Podemos agregar Tablas en la misma manera de figuras.

\begin{table}[H]
  \caption{Subtitulo por la Tabla.}
  % los {} argumentos abajo describen como cada fila de la tabla esta dibujado.
  % Otra vez, Tengo que buscar estés mandos. Cada Vez.
  \begin{tabular}{c|cc} 
    Numero &  Apellido & Nombre de pila \\ % Columna filas están separados por $
    \hline % una linea horizontal
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

% \section{Hiperenlaces} % Viene pronto

\section{Cuando LaTeX no compila algo (i.e. Código de Fuente)}
decimos que queremos incluir un poco de codigo en nuestro documento de LaTeX ,
Necesitamos LaTeX no probar interpretar el texto y
en vez imprimirlo en el documento. Para hacer esto usamos un
“verbatim environment” entorno literal. 

% Hay otro paquetes que existen (i.e. minty, lstlisting, etc.)
% pero “verbatim” es lo mas básico.
\begin{verbatim} 
  print("Hola Mundo!")
  a%b; % Mira! Podemos usar signos de % en “verbatim”. 
  random = 4; #decidir por aleatorio una tirada de dados
\end{verbatim}

\section{Compilando} 

Ya esta pensando como puedo compilar este documento fabuloso
y ver la gloria que es una pdf de LaTeX.
(Si, este documento actualmente compila). \\
Para obtener el ultimo documento usando LaTeX consista de los últimos pasos:
  \begin{enumerate}
    \item Escriba el documento en texto plano (el código del fuente).
    \item Compilar código de fuente para producir una pdf. 
     El paso de compilación parece algo como así (en Linux): \\
     \begin{verbatim} 
        $pdflatex aprender-latex.tex aprender-latex.pdf 
     \end{verbatim}
  \end{enumerate}

Algunos editores de LaTeX combinen Paso 1 y Paso 2 en lo mismo
software. Entonces puede ver Paso 1, pero no puede ver Paso 2 completamente.
Paso 2 todavía pasa atrás de las escenas.

Escriba todo su información de formato en texto plano en Paso 1.
el parte de compilación en Paso 2 produzca el documento en el
formato que ya ha definido en Paso 1.

\section{End}

Eso es todo por ahora

% terminar el documento
\end{document}
```

## Mas en LaTeX

* el maravilloso wikiLibro de LaTeX: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* Un tutoría actual: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)