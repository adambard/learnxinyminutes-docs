---
category: tool
tool: Statistical Computing with Python
contributors:
    - ["e99n09", "https://github.com/e99n09"]
translators:
    - ["Rafael Lacerda", "http://www.cs.toronto.edu/~lacerda/"]
lang: pt-br
filename: pythonstatcomp.txt
---

Este tutorial mostra como realizar tarefas simples de programação estatística em Python. O público alvo são pessoas familiares com Python e experientes em programação estatística em linguagens como R, Stata, SAS, SPSS ou MATLAB

```python


# 0. Preparação ====

""" Preparar o ambiente com IPython e usar instalar com pip os módulos: numpy,
    scipy, pandas, matplotlib, seaborn, requests.
    Certifique que este tutorial está rodando num IPython notebook para ter
    gráficos contextuais e fácil acesso a documentação.
"""

# 1. Aquisição de dados ====

""" Um dos motivos pelo qual Python é escolhido ao invés de R é a intenção de
    interagir com a web, seja por scraping de páginas ou aquisição de dados via
    uma API web. Isto também é possível em R, mas no contexto de um projeto que
    já utiliza Python, há um benefício em permanecer em uma única linguagem.
"""

import requests  # for HTTP requests (web scraping, APIs)
import os

# scraping de páginas web
r = requests.get("https://github.com/adambard/learnxinyminutes-docs")
r.status_code  # se 200, a requisição obteve êxito
r.text  # raw page source
print(r.text)  # bem formatado
# save the page source in a file:
os.getcwd()  # verificar o diretório atual
f = open("learnxinyminutes.html", "wb")
f.write(r.text.encode("UTF-8"))
f.close()

# baixar um arquivo csv
fp = "https://raw.githubusercontent.com/adambard/learnxinyminutes-docs/master/"
fn = "pets.csv"
r = requests.get(fp + fn)
print(r.text)
f = open(fn, "wb")
f.write(r.text.encode("UTF-8"))
f.close()

""" para mais informações sobre o módulo requests, incluindo APIs, veja
    http://docs.python-requests.org/en/latest/user/quickstart/
"""

# 2. Lendo um arquivo CSV ====

""" O pacote pandas de Wes McKinney fornece objetos 'DataFrame' em Python. Se 
    você já utilizou R, então já possui familiaridade com a idéa de um "data.frame".
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

""" Usuários de R: Note que em Python, assim como muitas linguages de programação
    inicia índices com 0. A indexação iniciando em 1 do R é atípica.
"""

# duas maneiras diferentes de imprimir uma coluna
pets.age
pets["age"]

pets.head(2)  # imprime as primeiras 2 linhas
pets.tail(1)  # imprime a última linha

pets.name[1]  # 'vesuvius'
pets.species[0]  # 'cat'
pets["weight"][2]  # 34

# Em R, isto geraria 3 linhas com este código, enquanto aqui apenas 2:
pets.age[0:2]
# 0    3
# 1    6

sum(pets.age) * 2  # 28
max(pets.weight) - min(pets.weight)  # 20

""" Se você está usando muita álgebra linear e operações matemáticas, usar
    vetores pode ser mais apropriado do que DataFrames. DataFrames são ideais
    para combinar colunas de diferentes tipos.
"""

# 3. Gráficos ====

import matplotlib as mpl
import matplotlib.pyplot as plt
%matplotlib inline

# Para exibir visualizações em Python, use matplotlib

plt.hist(pets.age);

plt.boxplot(pets.weight);

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# seaborn trabalha em cima do matplotlib e deixa os gráficos mais bonitos

import seaborn as sns

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# há também algumas funções gráficas específicas do seaborn
# note como o seaborn automaticamente marca o título do eixo x neste
# gráfico de barras
sns.barplot(pets["age"])

# Veteranos de R ainda podem usar ggplot
from ggplot import *
ggplot(aes(x="age",y="weight"), data=pets) + geom_point() + labs(title="pets")
# fonte: https://pypi.python.org/pypi/ggplot

# há ainda uma conversão do d3.js: https://github.com/mikedewar/d3py

# 4. Limpeza de dados simples e análise exploratória ====

""" Este é um exemplo mais complicado que demonstra um fluxo de trabalho
    básico de limpeza de dados, levando à criação de alguns dos gráficos
    exploratórios e execução de uma regressão linear.
        O conjunto de dados foi transcrito da Wikipedia manualmente.
    Contém todos os imperadores romanos e seus marcos (nascimento, 
    morte, coroação, etc.).
        O objetivo desta análise é explorar se existe uma relação entre
    o ano de nascimento e o tempo de vida dos imperadores.
    fonte de dados: https://en.wikipedia.org/wiki/Holy_Roman_Emperor
"""

# carregar alguns dados sobre imperadores romanos
url = "https://raw.githubusercontent.com/e99n09/R-notes/master/data/hre.csv"
r = requests.get(url)
fp = "hre.csv"
with open(fp, "wb") as f:
    f.write(r.text.encode("UTF-8"))

hre = pd.read_csv(fp)

hre.head()
"""
   Ix      Dynasty        Name        Birth             Death Election 1
0 NaN  Carolingian   Charles I  2 April 742    28 January 814        NaN
1 NaN  Carolingian     Louis I          778       20 June 840        NaN
2 NaN  Carolingian   Lothair I          795  29 September 855        NaN
3 NaN  Carolingian    Louis II          825     12 August 875        NaN
4 NaN  Carolingian  Charles II  13 June 823     6 October 877        NaN

  Election 2      Coronation 1   Coronation 2 Ceased to be Emperor
0        NaN   25 December 800            NaN       28 January 814
1        NaN  11 September 813  5 October 816          20 June 840
2        NaN       5 April 823            NaN     29 September 855
3        NaN        Easter 850     18 May 872        12 August 875
4        NaN   29 December 875            NaN        6 October 877

  Descent from whom 1 Descent how 1 Descent from whom 2 Descent how 2
0                 NaN           NaN                 NaN           NaN
1           Charles I           son                 NaN           NaN
2             Louis I           son                 NaN           NaN
3           Lothair I           son                 NaN           NaN
4             Louis I           son                 NaN           NaN
"""

# limpar as colunas nascimento (Birth) e morte (Death)

import re  # módulo para expressões regulares

rx = re.compile(r'\d+$')  # correspondência de dígitos no final

""" Esta função aplica a expressão regular em uma coluna de entrada (neste caso,
    nascimento (Birth) e morte (Death), achata a lista resultante e finalmente
    converte o tipo do objeto Series de texto para número inteiro. Para mais 
    informações sobre o que algumas das partes do código fazem, veja:
      - https://docs.python.org/2/howto/regex.html
      - http://stackoverflow.com/questions/11860476/how-to-unlist-a-python-list
      - http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.html
"""

def extractYear(v):
    return(pd.Series(reduce(lambda x, y: x + y, map(rx.findall, v), [])).astype(int))

hre["BirthY"] = extractYear(hre.Birth)
hre["DeathY"] = extractYear(hre.Death)

# criar uma coluna com a idade estimada
hre["EstAge"] = hre.DeathY.astype(int) - hre.BirthY.astype(int)

# gráfico de dispersão simples, sem linha de tendência, cor representa dinastia
sns.lmplot("BirthY", "EstAge", data=hre, hue="Dynasty", fit_reg=False);

# usar scipy para rodar uma regressão linear
from scipy import stats
(slope, intercept, rval, pval, stderr) = stats.linregress(hre.BirthY, hre.EstAge)
# código-fonte: http://wiki.scipy.org/Cookbook/LinearRegression

# checar a inclinação
slope  # 0.0057672618839073328

# checar o R^2:
rval**2  # 0.020363950027333586

# checar o p-valor
pval  # 0.34971812581498452

# usar o seaborn para criar um gráfico de dispersão e plotar a linha de tendência
# da regressão linear
sns.lmplot("BirthY", "EstAge", data=hre);

""" Para mais informações sobre seaborn, veja
      - http://web.stanford.edu/~mwaskom/software/seaborn/
      - https://github.com/mwaskom/seaborn
    Para mais informações sobre SciPy, veja
      - http://wiki.scipy.org/SciPy
      - http://wiki.scipy.org/Cookbook/
    Para ver a análise dos imperadores romanos em R, veja
      - http://github.com/e99n09/R-notes/blob/master/holy_roman_emperors_dates.R
"""

```

Para aprender mais, consulte _Python for Data Analysis_ por Wes McKinney. É uma fonte excelente que utilizei como referência para escrever este tutorial.

Há também uma série de tutoriais interativos em IPython sobre assuntos específicos a interesses, como <a href="http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/" Title="Probabilistic Programming and Bayesian Methods for Hackers">Probabilistic Programming and Bayesian Methods for Hackers</a> por Cam-Davidson Pylon.

Outros módulos para pesquisar:
   - análise textual e processamento de linguagem natural: nltk, http://www.nltk.org
   - análise de redes sociais: igraph, http://igraph.org/python/
