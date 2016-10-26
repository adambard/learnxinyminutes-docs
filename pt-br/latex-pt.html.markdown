---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
translator:
    - ["Eric Araújo", "http://github.com/LedZeck"]
filename: learn-latex.tex
---
% Todo comentário começa com %
% Não há comentários multilinha

% LaTeX NÃO é um software de processamento de texto do tipo"What You See Is What You Get" como o
% MS Word, ou o OpenOffice Writer

% Todo comando em LaTeX começa com uma contra-barra (\)

% Documentos LaTeX começam definindo o tipo de documento a ser compilado
% Outros tipos de documentos incluem livro, relatório, apresentações, etc.
% As opções para que o documento apareça nos colchetes []. Neste caso
% é especificado que nós queremos usar corpo de fonte 12pt.
\documentclass[12pt]{article}

% Depois nós definimos os pacotes que o documento usará.
% Se você quiser incluir gráficos, texto colorido, ou
% código-fonte de uma outra linguagem dentro do seu documento,
% você precisa otimizar as capacidades do LaTeX. Isso é feito pela adição de pacotes.
% Eu vou incluir os pacotes de float e legenda para figuras.
\usepackage{caption}
\usepackage{float}

% Nós também podemos definir algumas outras propriedades do documento!
\author{Chaitanya Krishna Ande, Colton Kohnke \& Sricharan Chiruvolu}
\date{\today}
\title{Learn LaTeX in Y Minutes!}

% Agora nós estamos prontos para iniciar o documento
% Tudo que vier antes dessa linha é chamado "O Preâmbulo"
\begin{document}
% Se nós definirmos o autor, data, campos de título, nós podemos fazer o LaTeX
% criar uma página de título.
\maketitle

% A maioria dos artigos científicos possuem abstract, você pode usar os comandos predefinidos para isso.
% Isso deve aparecer em sua ordem lógica, portanto, depois do conteúdo do topo,
% e antes das sessões principais do corpo.
% Este comando está disponível nas classes, artigos e relatórios.
\begin{abstract}
 Documentação de LaTeX escrita como LaTeX!
\end{abstract}

% Sessões de comando são intuitivas.
% Todos os títulos das sessões são adicionadas automaticamente à tabela de conteúdos.
\section{Introdução}
Olá, meu nome é João e, juntos, nós vamos explorar o LaTeX

\section{Outra sessão}
Este é o texto para outra sessão. Acho que isso precisa de uma subsessão.

\subsection{Isto é uma subsessão} % Subsessões também são intuitivas.
I think we need another one

\subsubsection{Pitagoras}
Bem melhor agora.
\label{subsec:pitagoras}

% Usando o asterisco, nós podemos suprimir as funções implícitas de numeração do LaTeX.
% Isso também funciona para outros comandos LaTeX.
\section*{Esta é uma sessão não numerada}
Nem todas as sessões precisam ser numeradas!

\section{Algumas notas de texto}
LaTeX geralmente é muito bom em posicionar o texto onde ele deve ir. Se
uma linha \\ precisa \\ ser \\ quebrada \\ você adiciona \textbackslash\textbackslash ao
código-fonte. \\

\section{Lists}
Listas são uma das coisas mais fáceis de criar em LaTeX! Eu preciso fazer compras amanhã, então vamos fazer uma lista de compras.
\begin{enumerate} % Isso cria um ambiente "enumerado".
  % \item diz o item para incrementar
  \item Salada.
  \item 27 Melancia.
  \item Um único Uísque.
  % nós podemos, inclusive sobrepor o número de um item usando []
  \item[quantos?] Pistolas d'água médias.

  Não é um item da lista, mas ainda é parte da numeração.

\end{enumerate} % Todos os ambientes devem ter um fim.

\section{Matemática}

Um dos principais usos para o LaTeX é a produção de artigos acadêmicos ou informes
técnicos. Normalmente na área da matemática e ciência. Para tanto,
nós precisamos estar aptos a adicionar símbolos especiais ao nosso artigo \\

A matemática possui vários símbolos, muito além dos que você pode encontrar no teclado;
Símbolos de conjunto e relação, setas, operadores e letras gregas são apenas alguns exemplos.\\

Conjuntos e relações compõem um papel crucial em muitos artigos científicos.
Aqui está como você estabelece que y pertence a x, $\forall$ x $\in$ X. \\
% Perceba como eu precisei adicionar o caractere $ antes e depois dos símbolos. Isto é
% quando escrevemos, nós estamos no modo texto.
% Entretanto, os símbolos matemáticos existem apenas no modo matemático.
% Nós podemos entrar no modo matemático, a partir do modo texto, com os sinais $.
% O oposto também ocorre. Variáveis também podem ser renderizadas no modo matemático.

Minha letra grega favorita é  $\xi$. Eu também gosto de $\beta$, $\gamma$ e $\sigma$.
Eu ainda não encontrei uma letra grega que o LaTeX não conheça.

Operadores são parte essencial de um documento matemático:
funções trigonométricas ($\sin$, $\cos$, $\tan$),
logaritmos e exponenciais ($\log$, $\exp$),
limites ($\lim$), etc.
Têm comandos LaTeX pre-definidos.
Vamos escrever uma equação para ver como isso é feito: \\

$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$

Frações(Numeradores-denominadores) podem ser escritos dessas formas:

% 10 / 7
$^{10}/_{7}$

% Frações relativamente complexas podem ser escritas assim
% \frac{numerator}{denominator}
$\frac{n!}{k!(n - k)!}$ \\

Nós também podemos inserir equações em um "ambiente de equações".

% Mostrar matemática com o 'ambiente' de equação
\begin{equation} % entra no modo matemático
    c^2 = a^2 + b^2.
    \label{eq:pitagoras} % para referenciar
\end{equation} % todo estado \begin deve ter um estado end

Nós podemos, então, referenciar nossa nova equação
Eqn.~\ref{eq:pitagoras} também é conhecida como o Teorema de Pitágoras que também é
o foco de Sec.~\ref{subsec:pitagoras}. Muitas coisas podem ser marcadas:
figuras, equações, sessões, etc.

Somas e integrais são escritas com os comandos sum e int, respectivamente:

% Alguns compiladores LaTeX irão reclamar se houver linhas em braco
% Em um ambiente de equação.
\begin{equação}
  \sum_{i=0}^{5} f_{i}
\end{equação}
\begin{equação}
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equação}

\section{Figuras}

Vamos inserir uma figura. O posicionamento de figuras pode tornar-se um pouco comfuso.
Eu definitivamente tenho que revisar as opções de posicionamento toda vez.

\begin{figure}[H] % H aqui é o qe define a opção de posição.
    \centering % centraliza a figura na página
    % Inserir uma figura escalada para 0.8 da largura da página.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png}
    % Comentando apenas para fins de compilação. Use sua imaginação aqui.
    \caption{Right triangle with sides $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabela}
Nós também podemos inserir tabelas do mesmo jeito que fazemos com as figuras.

\begin{tabela}[H]
  \caption{Legenda para a tabela.}
  % os {} argumentos abaixo descrevem como cada linha da coluna é desenhada.
  % Novamente, eu tenho que revisar isso. Cada. Vez. Que. Uso.
  \begin{tabular}{c|cc}
    Número & Sobrenome & Nome  \\ % As linhas das colunas são separadas por $
    \hline % uma linha horizontal
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{tabela}

% \section{Hyperlinks} % Em breve

\section{Usando LaTeX para não compilar (i.e. Código-fonte)}
Digamos que nós queremos incluir algum código dentro do nosso arquivo LaTeX,
nós queremos, então, que o LaTeX não interprete aquele texto e,
ao invés disso, apenas o imprima no documento. Nós fazemos isso como um "ambiente textual".

% Existem outros pacotes para isso (i.e. minty, lstlisting, etc.)
% mas verbatim é o principal deles.
\begin{verbatim}
  print("Hello World!")
  a%b; % Perceba! Nós podemos usar o caractere % no verbatim.
  random = 4; #sorteado por um dado não-viciado
\end{verbatim}

\section{Compilando}

Até agora você provavelmente está se perguntando como compilar este documento fabuloso
E olhar para um incrível pdf LaTeX.
(Sim, este documento realmente compila). \\
Chegar ao documento final, utilizando LaTeX consiste das seguintes etapas::
  \begin{enumerate}
    \item Escreva o documento em texto simples (o "código-fonte").
    \item Compilar o código fonte para produzir um pdf.
     A etapa de compilação será algo parecido com isto (no Linux): \\
     \begin{verbatim}
        $pdflatex learn-latex.tex learn-latex.pdf
     \end{verbatim}
  \end{enumerate}

Alguns editores LaTeX combinam os passos 1 e 2 no mesmo espaço de software. Então
você começa a ver o passo 1 mas não vê o passo 2 completamente. Ele continua sendo executado em segundo plano.


Você escreve todas as suas informações de formatação de texto no passo 1.
A parte de compilação no passo 2 é responsável por produzir o documento
no formato definito no passo 1.

\section{End}

Isso é tudo por enquanto

% fim do documento
\end{document}
```
## Mais em LaTeX

* The amazing LaTeX wikibook: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* An actual tutorial: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
