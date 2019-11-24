---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
    - ["Ramanan Balakrishnan", "https://github.com/ramananbalakrishnan"]
    - ["Svetlana Golubeva", "https://attillax.github.io/"]
translators:
    - ["Paulo Henrique Rodrigues Pinheiro", "https://github.com/paulohrpinheiro"]
lang: pt-br
filename: learn-latex-pt.tex
---

```tex
% Todas as linhas de comentários começam com %
% Não existem comentários multilinhas

$ LaTeX não é um programa processador de textos "Visual" como
% MS Word ou OpenOffice Writer

$ Todo comando LaTeX começa com uma barra invertida (\)

% Documentos LaTeX começam com a definição do tipo que será % compilado
% Os tipos de documento podem ser livro, relatório, apresentação, etc.
% As opções para um documento aparecem entre [] chaves. Nesse caso
% está especificado que queremos o tamanho da fonte em 12pt.
\documentclass[12pt]{article}

% Em seguida definimos os pacotes que o documento usa.
% Se você quiser incluir gráficos, texto colorido, ou código fonte de outra
% linguagem em outro arquivo em seu documento, você precisa ampliar as
% capacidades do LaTeX. Isso é feito adicionando-se pacotes.
% Serão incluídos os pacotes float e caption para imagens e hyperref
% para links.
\usepackage{caption}
\usepackage{float}
\usepackage{hyperref}

% Para poder usar caracteres acentuados, use o seguinte pacote:
\usepackage[utf8]{inputenc}

% Podemos definir algumas outras propriedades do documento também!
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu \& \\
Svetlana Golubeva}
\date{\today}
\title{Aprenda \LaTeX \hspace{1pt} em Y Minutos!}

% Agora estamos prontos para começar o documento
% Tudo antes dessa linha é chamado "preâmbulo".
\begin{document}
% Se informarmos os campos author (autores), date (data), "title" (título),
% LaTeX poderá cria uma página inicial para nós.
\maketitle
% Se tivermos seções, poderemos criar uma tabela de conteúdo. Para isso,
% o documento deve ser compilado duas vezes, para que tudo apareça na ordem
% correta.
% É uma boa prática separar a tabela de conteúdo do corpo do documento. Para
% isso usa-se o comando \newpage
\newpage
\tableofcontents

\newpage

% Muitos artigos de pesquisa possuem um resumo, e pode-se usar comandos
% predefinidos para isso.
% Isso deve aparecer em sua ordem lógica, portanto, após o topo,
% mas antes das seções principais do corpo.
% Esse comando está disponível para os documentos do tipo artigo (article)
% e relatório (report).
\begin{abstract}
 Documentação do \LaTeX \hspace{1pt} escrita em \LaTeX! Nada original!
\end{abstract}

% Comandos para seções são intuitivos. 
% Todos os títulos de seção são adicionados automaticamente à tabela de conteúdo.
\section{Introdução}
Olá, meu nome é Colton e juntos estamos explorando o mundo do \LaTeX!

\section{Outra seção}
Esse é o texto para outra seção. Penso que precisamos de uma subseção.

\subsection{Isso é uma subseção} % Subseções também são intuitivas.
Penso que precisamos de mais uma

\subsubsection{Pythagoras}
Muito melhor agora.
\label{subsec:pythagoras}

% Ao usar o asterisco nós impedimos a numeração automática.
% Isso funciona para outros comandos \LaTeX também.
\section*{Essa é uma seção não numerada} 
Afinal nem todas as seções precisam ser numeradas!

\section{Algumas notas sobre texto}
%\section{Espaçamento % É necessário mais informação sobre intervalos de espaço.
\LaTeX \hspace{1pt} geralmente é muito bom sobre colocar texto onde ele deve
ser posto. Se 
uma linha \\ deve \\ ser \\ quebrada \\ adicione \textbackslash\textbackslash
\hspace{1pt} ao código de seu documento. \\ 

\section{Listas}
Listas são uma das coisas mais fáceis de criar no \LaTeX! Preciso fazer compras
amanhã, então façamos uma lista de compras.
\begin{enumerate} % Isso cria o bloco "enumerate".
  % \item faz com que o enumerate incremente
  \item Salada.
  \item 27 melancias.
  \item Uma lebre.
  % pode-se também sobrescrever o número do item usando []
  \item[quantas?] Pistolas de água médias.

  Não é um item da lista, mas faz parte do bloco enumerate.

  \end{enumerate} % Todos os blocos devem ter um final (end{}).

\section{Matemática}

Um dos usos iniciais para \LaTeX \hspace{1pt} foi a produção de artigos
acadêmicos e técnicos. Usualmente nos campos da matemática e ciência. Assim, é
necessários que consigamos incluir alguns símbolos especiais em nosso texto! \\

A matemática tem muitos símbolos, além dos quais se pode encontrar no teclado;
símbolos para relações e conjuntos, setas, operadores, e letras gregas, apenas
para mencionar alguns.\\

Conjuntos e relações são essenciais em muitos textos de pesquisa em matemática.
Aqui está como você pode indicar como todo x que pertence
a X, $\forall$ x $\in$ X. \\
% Perceba que é necessário adicionar os sinais $ antes e depois dos símbolos.
% Isso é porque quando escrevendo, estamos em modo texto.
% Mas os símbolos de matemática só existem no modo matemática.
% Podemos entrar no modo matemática a partir do modo texto com os símbolos $.
% O oposto também pode ocorrer. Variáveis podem ser renderizadas no modo
% matemática.
% Também podemos entrar no modo matemática com \[\]

\[a^2 + b^2 = c^2 \]

Minha letra grega favorita é $\xi$. Eu também gosto da $\beta$, $\gamma$ e $\sigma$.
Eu ainda não encontrei uma letra grega que o \LaTeX \hspace{1pt} não tenha!\\

Operadores são parte essencial de um documento sobre matemática:
funções trigonométricas ($\sin$, $\cos$, $\tan$),
logaritmo e exponencial ($\log$, $\exp$), 
limites ($\lim$), etc. 
possuem comandos pré-definidos em LaTex. 
Vamos escrever uma equação para ver como se faz:
$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$ \\

Frações (numerador/denominador) podem ser escritas dessa forma:

% 10 / 7
$$ ^{10}/_{7} $$

% Frações relativamente complexas podem ser escritas como
% \frac{numerator}{denominator}
$$ \frac{n!}{k!(n - k)!} $$ \\

Também podemos escrever equações em um ``bloco de equação''.

% Apresenta matemática com o 'bloco' equação
\begin{equation} % entra no modo matemática
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % para referência
    \end{equation} % toda declaração \begin precisa de uma declaração end

Podemos então referenciar nossa nova equação!
A equação~\ref{eq:pythagoras} é também conhecida como Teorema de Pitágoras que é
também assunto da Seção~\ref{subsec:pythagoras}. Muitas coisas podem ser
rotuladas: figuras, equações, seções, etc.

Somatórios e Integrais são escritas com os comandos sum e int:

% Alguns compiladores LaTeX irão reclamar se existirem linhas em branco
% em um bloco de equação.
\begin{equation} 
  \sum_{i=0}^{5} f_{i}
\end{equation} 
\begin{equation} 
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation} 

\section{Figuras}

Insiramos uma Figura. O local para colocar a figura pode ser difícil
de determinar. Eu tenho sempre que verificar as opções toda vez.

\begin{figure}[H] % H aqui é uma opção para o local da figura. 
    \centering % centra a figura na página
    % Inclui uma figura com escala de 0.8 do tamanho da página.
    %\includegraphics[width=0.8\linewidth]{right-triangle.png} 
    % Comentado para propósitos de compilação. Por favor, use sua imaginação.
    \caption{Triângulo retângulo com lados $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Tabelas}
Também podemos incluir tabelas da mesma forma que figuras.

\begin{table}[H]
  \caption{Título para a Tabela.}
  % os argumentos {} abaixo descrevem como cada linha da tabela é desenhada.
  % Aqui também, Preciso ver isso. Toda. E. Cada. Vez.
  \begin{tabular}{c|cc} 
    Número &  Sobrenome & Primeiro Nome \\ % Colunas são separadas por &
    \hline % uma linha horizontal
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

\section{Fazendo o \LaTeX \hspace{1pt} não compilar algo (o código fonte)}
Digamos que precisamos incluir algum código dentro do nosso
documento \LaTeX \hspace{1pt}, para isso precisamos com o \LaTeX \hspace{1pt}
não tente interpretar esse texto e que apenas inclua ele no documento. Fazemos
isso com o bloco verbatim.

% Existem outros pacotes (por exemplo, minty, lstlisting, etc.)
% mas verbatim é o básico
\begin{verbatim} 
  print("Hello World!")
  a%b; % olha só! Podemos usar os sinais % no bloco verbatim. 
  random = 4; #decided by fair random dice roll
\end{verbatim}

\section{Compilando} 

Imagino que agora você esteja pensando como compilar esse fantástico documento
e visualizar a gloriosa glória que é um pdf gerado por \LaTeX \hspace{1pt} pdf.
(sim, esse documento é compilável). \\

Finalizando o documento usando \LaTeX \hspace{1pt} consiste nos seguintes passos:
  \begin{enumerate}
    \item Escrever o documento em texto puro (o ``código fonte'').
    \item Compilar o código fonte para gerar um pdf. 
     Os passos para compilar se parecem (em Linux) com: \\
     \begin{verbatim} 
        > pdflatex learn-latex.tex
     \end{verbatim}
  \end{enumerate}

Existem editores de \LaTeX \hspace{1pt} que combinam os passos 1 e 2 no mesmo
sistema de software. Assim, você pode ver o passo 1, mas não o passo 2 por
completo. Passo 2 estará acontecendo escondido\footnote{Por exemplo, quando usar
referências (como Equação~\ref{eq:pythagoras}), pode ser necessário executar o
passo 2 várias vezes, para gerar arquivos *.aux intermediários.}.
% É assim que você adiciona notas de rodapé em seus documentos!

Você escreve toda a informação de formatação em texto puro, no passo 1. O
momento da compilação no passo 2 é responsável por produzir o documento no
formato que você definiu no passo 1.

\section{Links}
Nós podemos inserir links em nosso documento. Para isso nós necessitamos incluir
o pacote hyperref no preâmbulo com o comando:
\begin{verbatim} 
    \usepackage{hyperref}
\end{verbatim}

Existem dois tipos principais de links: URL visíveis \\
\url{https://learnxinyminutes.com/docs/latex/}, ou
\href{https://learnxinyminutes.com/docs/latex/}{um texto alternativo}
% Você não pode adicionar espaços extras ou símbolos especiais no texto
% alternativo, pois isso causará problemas na compilação.

Esse pacote também produz uma lista de thumbnails no documento pdf gerado e
ativa os links na tabela de conteúdo.

\section{End}

Por enquanto é isso!

% Frequentemente você precisa de uma seção de referências em seu documento.
% A forma mais fácil de configurá-la é usando uma seção de bibliografia
\begin{thebibliography}{1}
  % como em outras listas, o comando \bibitem pode ser usado para itens da lista
  % cada entrada pode ser citada diretamente no corpo do texto
  \bibitem{latexwiki} The amazing \LaTeX \hspace{1pt} wikibook: {\em 
https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} An actual tutorial: {\em http://www.latex-tutorial.com}
\end{thebibliography}

% end the document
\end{document}
```

## Mais sobre LaTeX

* The amazing LaTeX wikibook: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* An actual tutorial: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
