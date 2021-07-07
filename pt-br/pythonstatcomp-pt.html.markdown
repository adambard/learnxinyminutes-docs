---
category: tool
tool: Statistical Computing with Python
contributors:
    - ["e99n09", "https://github.com/e99n09"]
translators:
    - ["Raul Almeida", "https://github.com/almeidaraul"]
lang: pt-br
---

Este é um tutorial de como realizar algumas tarefas usuais em estatística usando Python. Ele é destinado a pessoas familiarizadas com Python básico e experientes em alguma linguagem de programação dedicada à estatística, como R, Stata, SAS, SPSS, ou MATLAB.

```python



# 0. Configurações iniciais ====

""" Para começar, instale os seguintes pacotes usando o pip ('pip install X'):
    jupyter, numpy, scipy, pandas, matplotlib, seaborn, requests.
        Acompanhe este tutorial num caderno do Jupyter pelos resultados
    imediatos e fácil acesso à documentação. Você pode abrir um caderno do
    jupyter pela shell com o comando 'jupyter notebook' (clique em
    New->Python).
"""

# 1. Aquisição de dados ====

""" Um bom motivo para preferir Python em vez de R é a intenção de interagir
    muito com a internet - ou processando páginas diretamente ou recebendo
    dados por uma API. Isso é possível em R, mas se o projeto é maior do que
    somente aquela análise faz mais sentido ficar em uma só linguagem.
"""

import requests  # para requisições HTTP (consumir páginas, API)
import os

# consumo de páginas
r = requests.get("https://github.com/adambard/learnxinyminutes-docs")
r.status_code  # requisição teve sucesso se for 200
r.text  # código-fonte da página
print(r.text)  # código-fonte formatado
# salvando o código-fonte em um arquivo:
os.getcwd()  # descobrindo o diretório atual
with open("learnxinyminutes.html", "wb") as f:
    f.write(r.text.encode("UTF-8"))

# baixando um CSV (valores separados por vírgulas)
fp = "https://raw.githubusercontent.com/adambard/learnxinyminutes-docs/master/"
fn = "pets.csv"
r = requests.get(fp + fn)
print(r.text)
with open(fn, "wb") as f:
    f.write(r.text.encode("UTF-8"))

""" para mais sobre o módulo requests, incluindo APIs, acesse
    http://docs.python-requests.org/en/latest/user/quickstart/
"""

# 2. Lendo um arquivo CSV ====

""" O pacote pandas, de Wes McKinney, oferece a classe 'DataFrame'.
    Se você já usou R, você está familiarizado com a ideia do "data.frame".
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

""" Usuários de R: percebam que Python, assim como a maioira das linguagens
    influenciadas por C, faz indexação iniciada por 0. R faz com 1 por
    influência da Fortran.
"""

# duas maneiras diferentes de imprimir uma coluna
pets.age
pets["age"]

pets.head(2)  # imprime as duas primeiras linhas
pets.tail(1)  # imprime a última linha

pets.name[1]  # 'vesuvius'
pets.species[0]  # 'cat'
pets["weight"][2]  # 34

# a descrição de intervalo não é inclusiva para o segundo termo
# abaixo, só serão incluídos os índices 0 e 1 (não o 2)
pets.age[0:2]
# 0    3
# 1    6

sum(pets.age) * 2  # 28
max(pets.weight) - min(pets.weight)  # 20

""" Se você está trabalhando com álgebra linear ou cálculo numérico pesado,
    você talvez queira arrays e não DataFrames. DataFrames são ideais para
    combinar colunas de tipos diferentes.
"""

# 3. Gráficos ====

import matplotlib as mpl
import matplotlib.pyplot as plt
%matplotlib inline

# Visualização de dados, no Python, é feita com o matplotlib

plt.hist(pets.age);

plt.boxplot(pets.weight);

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# seaborn deixa a visualização mais elegante

import seaborn as sns

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# também existem funções específicas do pacote seaborn para desenhar gráficos
# perceba como o seaborn automaticamente rotula o eixo x neste plot:
sns.barplot(pets["age"])

# Usuários do R ainda podem usar ggplot
from ggplot import *
ggplot(aes(x="age",y="weight"), data=pets) + geom_point() + labs(title="pets")
# fonte: https://pypi.python.org/pypi/ggplot

# também existe uma porta d3.js: https://github.com/mikedewar/d3py

# 4. Limpeza e análise exploratória simples de dados ====

""" Vamos a um exemplo mais complicado que demonstre o processo de limpeza
    básica de dados levando à criação de alguns plots exploratórios e à
    execução de uma regressão linear.
        O conjunto de dados foi transcrito da Wikipedia à mão. Ele contém
    todos os imperadores romanos e alguns registros sobre suas vidas
    (nascimento, morte, coroação, etc).
        O objetivo desta análise será explorar se existe uma relação entre
    a época de nascimento e o tempo de vida de um imperador.
    Fonte: https://en.wikipedia.org/wiki/Holy_Roman_Emperor
"""

# carregando os dados
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

# Limpando as colunas de nascimento e morte

import re  # módulo que lida com expressões regulares

rx = re.compile(r'\d+$')  # encontrando dígitos à direita

""" Esta função aplica a expressão regular a uma coluna de entrada (aqui,
    nascimento e morte) e gera uma lista no tipo apropriado. Para mais
    informações sobre esse processo, acesse:
      - https://docs.python.org/2/howto/regex.html
      - http://stackoverflow.com/questions/11860476/how-to-unlist-a-python-list
      - http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.html
"""

from functools import reduce

def extractYear(v):
    return(pd.Series(reduce(lambda x, y: x + y, map(rx.findall, v), [])).astype(int))

hre["BirthY"] = extractYear(hre.Birth)
hre["DeathY"] = extractYear(hre.Death)

# coluna com idade estimada
hre["EstAge"] = hre.DeathY.astype(int) - hre.BirthY.astype(int)

# gráfico de dispersão sem linha de tendência; cor representa dinastia
sns.lmplot("BirthY", "EstAge", data=hre, hue="Dynasty", fit_reg=False)

# regressão linear com scipy
from scipy import stats
(slope, intercept, rval, pval, stderr) = stats.linregress(hre.BirthY, hre.EstAge)
# fonte do código: http://wiki.scipy.org/Cookbook/LinearRegression

# checando a curva
slope  # 0.0057672618839073328

# checando R²:
rval**2  # 0.020363950027333586

# checando o valor p:
pval  # 0.34971812581498452

# usando seaborn para a mesma coisa
sns.lmplot("BirthY", "EstAge", data=hre)

""" Para mais informação sobre o seaborn, veja
      - http://web.stanford.edu/~mwaskom/software/seaborn/
      - https://github.com/mwaskom/seaborn
    Para mais sobre o SciPy:
      - http://wiki.scipy.org/SciPy
      - http://wiki.scipy.org/Cookbook/
    Uma versão da análise usando R:
      - http://github.com/e99n09/R-notes/blob/master/holy_roman_emperors_dates.R
"""

```

Se você quiser aprender mais, uma ideia é o livro _Python for Data Analysis_ por Wes McKinney. Existe muito conteúdo sobre Python para computação e estatística pela internet, tanto de graça quanto por preços numa faixa muito grande, então procure com atenção e com certeza encontrará os recursos certos para você.

Existem diversos tutorials interativos em IPython sobre conteúdos específicos, como <a href="http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/" Title="Probabilistic Programming and Bayesian Methods for Hackers">Probabilistic Programming and Bayesian Methods for Hackers</a> (por Cam Davidson-Pilon)

Outros módulos que talvez sejam interessantes:
   - análise de texto e processamento natural de linguagem: nltk, http://www.nltk.org
   - análise de rede social: igraph, http://igraph.org/python/
