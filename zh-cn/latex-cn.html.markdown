---
language: latex
contributors:
    - ["Chaitanya Krishna Ande", "http://icymist.github.io"]
    - ["Colton Kohnke", "http://github.com/voltnor"]
    - ["Sricharan Chiruvolu", "http://sricharan.xyz"]
    - ["Ramanan Balakrishnan", "https://github.com/ramananbalakrishnan"]
    - ["Svetlana Golubeva", "https://attillax.github.io/"]
translators:
    - ["Dp Leo", "https://github.com/minoriwww"]
filename: learn-latex-cn.tex
lang: zh-cn
---

```tex
% 所有的注释行以 % 开头
% 没有多行注释语法

% LaTeX 不是一个“所见即所得” 的文字处理软件
% 这与 MS Word，和 OpenOffice Writer 不同

% 每一个LaTeX命令由反斜线 (\) 开始

% LaTeX 文档以对编译对象文档的定义开始
% 这些文档包括书籍，报告，演示等
% 文档的选项出现在中括号里
% 下例中，我们设定文章字体为12pt
\documentclass[12pt]{article}

% 之后我们定义该文档所用的库
% 如果想要引入图片，彩色字，或是其他语言的源码在您的文档中
% 您需要增强 LaTeX 的功能。这将通过添加库来实现
% 下例中将要为展示数据引入 float 和 caption 库
% 为超链接引入 hyperref 库
\usepackage{caption}
\usepackage{float}
\usepackage{hyperref}

% 我们还可以定义其他文档属性！
\author{Chaitanya Krishna Ande, Colton Kohnke, Sricharan Chiruvolu \& \\
Svetlana Golubeva}
\date{\today}
\title{Learn \LaTeX \hspace{1pt} in Y Minutes!}

% 现在我们开始正文
% 这一行之前都是“序章”
\begin{document} 
% 如果想设定作者，时间，标题字段我们可使用 LaTeX 来建立标题页
\maketitle

% 分章节时，可以建立目录
% 我们需要编译文档两次来保证他们顺序正确
% 使用目录来分开文档是很好的做法
% 这里我们使用 \newpage 操作符
\newpage
\tableofcontents

\newpage

% 许多研究论文有摘要部分。这可以使用预定义的指令来实现
% 它应被放在逻辑上正确的位置，即顶部标题等的下面和文章主体的上面
% 该指令可以再报告和文章中使用
\begin{abstract}
 \LaTeX \hspace{1pt} documentation written as \LaTeX! How novel and totally not
 my idea!
\end{abstract}

% 章节指令非常直观
% 所有章节标题会自动地添加到目录中
\section{Introduction}
Hello, my name is Colton and together we're going to explore \LaTeX!

\section{Another section}
This is the text for another section. I think it needs a subsection.

\subsection{This is a subsection} % 子章节同样非常直观
I think we need another one

\subsubsection{Pythagoras}
Much better now.
\label{subsec:pythagoras}

% 使用型号我们可以借助 LaTeX 内置的编号功能
% 这一技巧也在其他指令中有效
\section*{This is an unnumbered section} 
然而并不是所有章节都要被标序号

\section{Some Text notes}
%\section{Spacing} % 需要增加有关空白间隔的信息
\LaTeX \hspace{1pt} is generally pretty good about placing text where it should
go. If 
a line \\ needs \\ to \\ break \\ you add \textbackslash\textbackslash 
\hspace{1pt} to the source code. \\ 

\section{Lists}
Lists are one of the easiest things to create in \LaTeX! I need to go shopping
tomorrow, so let's make a grocery list.
\begin{enumerate} % 此处创建了一个“枚举”环境
  % \item 使枚举增加一个单位
  \item Salad.
  \item 27 watermelon.
  \item A single jackrabbit.
  % 我们甚至可以通过使用 [] 覆盖美剧的数量
  \item[how many?] Medium sized squirt guns.

  Not a list item, but still part of the enumerate.

\end{enumerate} % 所有环境都有终止符

\section{Math}

使用 \LaTeX \hspace{1pt} 的一个最主要的方面是学术论文和技术文章
通常在数学和科学的领域 
因此我们需要在文章中插入特殊符号！ \\

数学符号极多，远超出你能在键盘上找到的那些；
集合关系符，箭头，操作符，希腊字符等等 \\

集合与关系在数学文章中很重要
如声明所有 x 属于 X $\forall$ x $\in$ X. \\
% 注意我们需要在这些符号之前和之后增加 $ 符号
% 因为在编写时我们处于 text-mode，然而数学符号只在 math-mode 中存在
% text mode 进入 math-mode 使用 $ 操作符
% 反之亦然，变量同时会在 math-mode 中被渲染。
% 我们也可以使用 \[\] 来进入 math mode

\[a^2 + b^2 = c^2 \]

My favorite Greek letter is $\xi$. I also like $\beta$, $\gamma$ and $\sigma$.
I haven't found a Greek letter yet that \LaTeX \hspace{1pt} doesn't know
about! \\

常用函数操作符同样很重要： 
trigonometric functions ($\sin$, $\cos$, $\tan$), 
logarithms 和 exponentials ($\log$, $\exp$), 
limits ($\lim$), etc. 
在 LaTeX 指令中预定义 
让我们写一个等式看看发生了什么：
$\cos(2\theta) = \cos^{2}(\theta) - \sin^{2}(\theta)$ \\

分数可以写成以下形式：

% 10 / 7
$$ ^{10}/_{7} $$

% 相对比较复杂的分数可以写成
% \frac{numerator}{denominator}
$$ \frac{n!}{k!(n - k)!} $$ \\

我们同样可以插入公式（equations）在环境 ``equation environment'' 下。

% 展示数学相关时，使用方程式环境
\begin{equation} % 进入 math-mode
    c^2 = a^2 + b^2.
    \label{eq:pythagoras} % 为了下一步引用
\end{equation} % 所有 \begin 语句必须有end语句对应

引用我们的新等式！
Eqn.~\ref{eq:pythagoras} is also known as the Pythagoras Theorem which is also
the subject of Sec.~\ref{subsec:pythagoras}. A lot of things can be labeled: 
figures, equations, sections, etc.

求和（Summations）与整合（Integrals）写作 sum 和 int ：

% 一些编译器会提醒在等式环境中的空行

\begin{equation} 
  \sum_{i=0}^{5} f_{i}
\end{equation} 
\begin{equation} 
  \int_{0}^{\infty} \mathrm{e}^{-x} \mathrm{d}x
\end{equation} 

\section{Figures}

让我们插入图片。图片的放置非常微妙。
我在每次使用时都会查找可用选项。

\begin{figure}[H] % H 是放置选项的符号
    \centering % 图片在本页居中
    % 宽度放缩为页面的0.8倍
    %\includegraphics[width=0.8\linewidth]{right-triangle.png} 
    % 需要使用想象力决定是否语句超出编译预期
    \caption{Right triangle with sides $a$, $b$, $c$}
    \label{fig:right-triangle}
\end{figure}

\subsection{Table}
插入表格与插入图片方式相同

\begin{table}[H]
  \caption{Caption for the Table.}
  % 下方的 {} 描述了表格中每一行的绘制方式
  % 同样，我在每次使用时都会查找可用选项。
  \begin{tabular}{c|cc} 
    Number &  Last Name & First Name \\ % 每一列被 & 分开
    \hline % 水平线
    1 & Biggus & Dickus \\
    2 & Monty & Python
  \end{tabular}
\end{table}

\section{Getting \LaTeX \hspace{1pt} to not compile something (i.e. Source Code)}
现在增加一些源代码在 \LaTeX \hspace{1pt} 文档中，
我们之后需要 \LaTeX \hspace{1pt} 不翻译这些内容而仅仅是把他们打印出来
这里使用 verbatim environment。 

% 也有其他库存在 (如. minty, lstlisting, 等)
% 但是 verbatim 是最基础和简单的一个
\begin{verbatim} 
  print("Hello World!")
  a%b; % 在这一环境下我们可以使用 %
  random = 4; #decided by fair random dice roll
\end{verbatim}

\section{Compiling} 

现在你大概想了解如何编译这个美妙的文档
然后得到饱受称赞的 \LaTeX \hspace{1pt} pdf文档
(这个文档确实被编译了)。 \\
得到最终文档，使用 \LaTeX \hspace{1pt} 组合步骤：
  \begin{enumerate}
    \item Write the document in plain text (the ``source code'').
    \item Compile source code to produce a pdf. 
     The compilation step looks like this (in Linux): \\
     \begin{verbatim} 
        > pdflatex learn-latex.tex
     \end{verbatim}
  \end{enumerate}

许多 \LaTeX \hspace{1pt}编译器把步骤1和2在同一个软件中进行了整合
所以你可以只看步骤1完全不看步骤2
步骤2同样在以下情境中使用情景 \footnote{以防万一，当你使用引用时
 (如 Eqn.~\ref{eq:pythagoras})，你将需要多次运行步骤2
来生成一个媒介文件 *.aux 。}.
% 同时这也是在文档中增加脚标的方式

在步骤1中，用普通文本写入格式化信息
步骤2的编译阶段则注意在步骤1 中定义的格式信息。

\section{Hyperlinks}
同样可以在文档中加入超链接
使用如下命令在序言中引入库：
\begin{verbatim} 
    \usepackage{hyperref}
\end{verbatim}

有两种主要的超链接方式 \\
\url{https://learnxinyminutes.com/docs/latex/}， 或  
\href{https://learnxinyminutes.com/docs/latex/}{shadowed by text}
% 你不可以增加特殊空格和符号，因为这将会造成编译错误

这个库同样在输出PDF文档时制造略缩的列表，或在目录中激活链接


\section{End}

这就是全部内容了！

% 通常，你会希望文章中有个引用部分
% 最简单的建立方式是使用书目提要章节
\begin{thebibliography}{1}
  % 与其他列表相同， \bibitem 命令被用来列出条目
  % 每个记录可以直接被文章主体引用
  \bibitem{latexwiki} The amazing \LaTeX \hspace{1pt} wikibook: {\em 
https://en.wikibooks.org/wiki/LaTeX}
  \bibitem{latextutorial} An actual tutorial: {\em http://www.latex-tutorial.com}
\end{thebibliography}

% 结束文档
\end{document}
```

## LaTeX 进阶

* The amazing LaTeX wikibook: [https://en.wikibooks.org/wiki/LaTeX](https://en.wikibooks.org/wiki/LaTeX)
* An actual tutorial: [http://www.latex-tutorial.com/](http://www.latex-tutorial.com/)
