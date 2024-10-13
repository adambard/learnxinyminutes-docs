---
category: framework
framework: Statistical Computing with Python
contributors:
    - ["e99n09", "https://github.com/e99n09"]
translators:
    - ["waltercjunior", "https://github.com/waltercjunior"]
filename: pythonstatcomp-pt.py
lang: pt-br
---

Este é um tutorial sobre como fazer algumas tarefas típicas de programação estatística usando Python.
É destinado basicamente à pessoas familizarizadas com Python e experientes com programação estatística em linguagens como R,
Stata, SAS, SPSS ou MATLAB.

```python
# 0. Preparando-se ====

"""  Para começar, instale o seguinte : jupyther, numpy, scipy, pandas,
    matplotlib, seaborn, requests.
    Certifique-se de executar este tutorial utilizando o Jupyther notebook para
    que você utilize os gráficos embarcados e ter uma fácil consulta à
    documentação.
    O comando para abrir é simplesmente '`jupyter notebook`, quando abrir então
    clique em 'New -> Python'.
"""

# 1. Aquisição de dados ====

""" A única razão das pessoas optarem por Python no lugar de R é que pretendem
    interagir com o ambiente web, copiando páginas diretamente ou solicitando
    dados utilizando uma API. Você pode fazer estas coisas em R, mas no
    contexto de um projeto já usando Python, há uma vantagem em se ater uma
    linguágem única.
"""

import requests  # para requisições HTTP (web scraping, APIs)
import os

# web scraping
r = requests.get("https://github.com/adambard/learnxinyminutes-docs")
r.status_code  # se retornou código 200, a requisição foi bem sucedida
r.text  # código fonte bruto da página
print(r.text)  # formatado bonitinho
# salve a o código fonte d apágina em um arquivo:
os.getcwd()  # verifique qual é o diretório de trabalho
with open("learnxinyminutes.html", "wb") as f:
    f.write(r.text.encode("UTF-8"))

# Baixar um arquivo csv
fp = "https://raw.githubusercontent.com/adambard/learnxinyminutes-docs/master/"
fn = "pets.csv"
r = requests.get(fp + fn)
print(r.text)
with open(fn, "wb") as f:
    f.write(r.text.encode("UTF-8"))

""" para mais informações sobre o módulo de solicitações, incluindo API's, veja em
    http://docs.python-requests.org/en/latest/user/quickstart/
"""

# 2. Lendo um arquivo formato CSV ====

""" Um pacote de pandas da Wes McKinney lhe dá um objeto 'DataFrame' em Python.
    Se você já usou R, já deve estar familiarizado com a ideia de "data.frame".
"""

import pandas as pd
import numpy as np
import scipy as sp
pets = pd.read_csv(fn)
pets
#        name  age  weight species
# 0    fluffy    3      14     cat
# 1  vesuvius    6      23    fish
# 2       rex    5      34     dog

""" Usuários R: observe que o Python, como a maioria das linguagens de programação
    influenciada pelo C, a indexação começa de 0. Em R, começa a indexar em 1
    devido à influência do Fortran.
"""

# duas maneiras diferentes de imprimir uma coluna
pets.age
pets["age"]

pets.head(2)  # imprima as 2 primeiras linhas
pets.tail(1)  # imprima a última linha

pets.name[1]  # 'vesuvius'
pets.species[0]  # 'cat'
pets["weight"][2]  # 34

# Em R, você esperaria obter 3 linhas fazendo isso, mas aqui você obtem 2:
pets.age[0:2]
# 0    3
# 1    6

sum(pets.age) * 2  # 28
max(pets.weight) - min(pets.weight)  # 20

""" Se você está fazendo alguma álgebra linear séria e processamento de
    números você pode desejar apenas arrays, não DataFrames. DataFrames são
    ideais para combinar colunas de diferentes tipos de dados.
"""

# 3. Gráficos ====

import matplotlib as mpl
import matplotlib.pyplot as plt
%matplotlib inline

# Para fazer a visualiação de dados em Python, use matplotlib

plt.hist(pets.age);

plt.boxplot(pets.weight);

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# seaborn utiliza a biblioteca do matplotlib e torna os enredos mais bonitos

import seaborn as sns

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# também existem algumas funções de plotagem específicas do seaborn
# observe como o seaborn automaticamenteo o eixto x neste gráfico de barras
sns.barplot(pets["age"])

# Veteranos em R ainda podem usar o ggplot
from ggplot import *
ggplot(aes(x="age",y="weight"), data=pets) + geom_point() + labs(title="pets")
# fonte: https://pypi.python.org/pypi/ggplot

# há até um d3.js veja em: https://github.com/mikedewar/d3py

# 4. Limpeza de dados simples e análise exploratória ====

""" Aqui está um exemplo mais complicado que demonstra dados básicos
    fluxo de trabalho de limpeza levando à criação de algumas parcelas
    e a execução de uma regressão linear.
        O conjunto de dados foi transcrito da Wikipedia à mão. Contém
    todos os sagrados imperadores romanos e os marcos importantes em suas vidas
    (birth, death, coronation, etc.).
        O objetivo da análise será explorar se um relacionamento existe
    entre o ano de nascimento (birth year) e a expectativa de vida (lifespam)
    do imperador.
    Fonte de dados: https://en.wikipedia.org/wiki/Holy_Roman_Emperor
"""

# carregue alguns dados dos sagrados imperadores romanos
url = "https://raw.githubusercontent.com/adambard/learnxinyminutes-docs/master/hre.csv"
r = requests.get(url)
fp = "hre.csv"
with open(fp, "wb") as f:
    f.write(r.text.encode("UTF-8"))

hre = pd.read_csv(fp)

hre.head()
"""
   Ix      Dynasty        Name        Birth             Death
0 NaN  Carolingian   Charles I  2 April 742    28 January 814
1 NaN  Carolingian     Louis I          778       20 June 840
2 NaN  Carolingian   Lothair I          795  29 September 855
3 NaN  Carolingian    Louis II          825     12 August 875
4 NaN  Carolingian  Charles II  13 June 823     6 October 877

       Coronation 1   Coronation 2 Ceased to be Emperor
0   25 December 800            NaN       28 January 814
1  11 September 813  5 October 816          20 June 840
2       5 April 823            NaN     29 September 855
3        Easter 850     18 May 872        12 August 875
4   29 December 875            NaN        6 October 877
"""

# limpar as colunas Birth e Death

import re  # módulo para expressões regulares

rx = re.compile(r'\d+$')  # conincidir com os códigos finais

""" Esta função aplia a expressão reguar a uma coluna de entrada (here Birth,
    Death), nivela a lista resultante, converte-a em uma lista de objetos, e
    finalmente converte o tipo do objeto da lista de String para inteiro. para
    mais informações sobre o que as diferentes partes do código fazer, veja em:
      - https://docs.python.org/2/howto/regex.html
      - http://stackoverflow.com/questions/11860476/how-to-unlist-a-python-list
      - http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.html
"""

from functools import reduce

def extractYear(v):
    return(pd.Series(reduce(lambda x, y: x + y, map(rx.findall, v), [])).astype(int))

hre["BirthY"] = extractYear(hre.Birth)
hre["DeathY"] = extractYear(hre.Death)

# faça uma coluna infomrnado a idade estimada ("EstAge")
hre["EstAge"] = hre.DeathY.astype(int) - hre.BirthY.astype(int)

# gráfico de dispersão simples, sem linha de tendência, cor representa dinastia
sns.lmplot("BirthY", "EstAge", data=hre, hue="Dynasty", fit_reg=False)

# use o scipy para executar uma regrassão linear
from scipy import stats
(slope, intercept, rval, pval, stderr) = stats.linregress(hre.BirthY, hre.EstAge)
# código fonte: http://wiki.scipy.org/Cookbook/LinearRegression

# varifique o declive (slope)
slope  # 0.0057672618839073328

# varifique o valor R^2:
rval**2  # 0.020363950027333586

# varifique o valor p-value
pval  # 0.34971812581498452

# use o seaborn para fazer um gráfico de dispersão e traçar a linha de tendência de regrassão linear
sns.lmplot("BirthY", "EstAge", data=hre)

""" Para mais informações sobre o seaborn, veja
      - http://web.stanford.edu/~mwaskom/software/seaborn/
      - https://github.com/mwaskom/seaborn
    Para mais informações sobre o SciPy, veja
      - http://wiki.scipy.org/SciPy
      - http://wiki.scipy.org/Cookbook/
    Para ver uma versão da análise dos sagrados imperadores romanos usando R, consulte
      - http://github.com/e99n09/R-notes/blob/master/holy_roman_emperors_dates.R
"""
```

Se você quiser saber mais, obtenha o Python para análise de dados de Wes McKinney. É um excelente recurso e usei-o como referência ao escrever este tutorial.

Você também pode encontrar muitos tutoriais interativos de IPython sobre assuntos específicos de seus interesses, como Cam Davidson-Pilon's [Programação Probabilística e Métodos Bayesianos para Hackers](http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/).

Mais alguns módulos para pesquisar:

   - análise de texto e processamento de linguagem natural: [nltk](http://www.nltk.org)
   - análise de rede social: [igraph](http://igraph.org/python/)
