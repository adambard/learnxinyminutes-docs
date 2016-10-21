---
language: Statistical computing with Python
contributors:
    - ["e99n09", "https://github.com/e99n09"]
filename: pythonstatcomp-es.py
translators:
    - ["Damaso Sanoja", "https://github.com/damasosanoja"]
lang: es-es
---

Este es un tutorial de como realizar tareas típicas de programación estadística usando Python. Está destinado a personas con cierta familiaridad con Python y con experiencia en programación estadística en lenguajes como R, Stata, SAS, SPSS, or MATLAB.

```python

# 0. Cómo configurar ====

""" Configurar con IPython y pip install lo siguiente: numpy, scipy, pandas,
    matplotlib, seaborn, requests.
        Asegúrese de realizar este tutorial con el IPython notebook para tener fácil
    acceso a las ayudas en tiempo real y la documentación respectiva.
"""

# 1. Captura de datos ====

""" Muchos prefieren Python sobre R ya que quieren interactuar mucho
    con la web, bien sea haciendo webscraping o solicitando datos mediante
    un API. Esto se puede hacer en R, pero en el contexto de un proyecto
    que ya usa Python, existen beneficios al mantener un solo lenguaje.
"""

import requests # para llamados HTTP (webscraping, APIs)
import os

# webscraping
r = requests.get("https://github.com/adambard/learnxinyminutes-docs")
r.status_code # si es 200, el llamado ha sido exitoso
r.text # código fuente de la página
print(r.text) # formateado y embellecido
# graba el código fuente en un fichero:
os.getcwd() # verifica cual es el directorio de trabajo
f = open("learnxinyminutes.html","wb")
f.write(r.text.encode("UTF-8"))
f.close()

# descargando un csv
fp = "https://raw.githubusercontent.com/adambard/learnxinyminutes-docs/master/"
fn = "pets.csv"
r = requests.get(fp + fn)
print(r.text)
f = open(fn,"wb")
f.write(r.text.encode("UTF-8"))
f.close()

""" para saber más del módulo de peticiones, incluyendo APIs, ver
    http://docs.python-requests.org/en/latest/user/quickstart/
"""

# 2. Leyendo un fichero CSV ====

""" El paquete pandas de Wes McKinney brinda objetos 'DataFrame' en Python. Si
    has usado R, ya estarás familiarizado con la idea de "data.frame".
"""

import pandas as pd, numpy as np, scipy as sp
pets = pd.read_csv(fn)
pets
#       nombre edad  peso especies
# 0    fluffy     3    14      cat
# 1  vesuvius     6    23     fish
# 2       rex     5    34      dog

""" Usuarios de R: notar que Python, al igual que otros lenguajes de programación
    normales, comienza indexando desde 0. R de forma inusual comienza desde 1.
"""

# dos formas distintas de imprimir una columna
pets.age
pets["age"]

pets.head(2) # imprime las primeras dos filas
pets.tail(1) # imprime la última fila

pets.name[1] # 'vesuvius'
pets.species[0] # 'cat'
pets["weight"][2] # 34

# en R, puedes esperar obtener 3 filas haciendo esto, pero aquí obtienes 2:
pets.age[0:2]
# 0    3
# 1    6

sum(pets.age)*2 # 28
max(pets.weight) - min(pets.weight) # 20

""" Si estás procesando grandes cantidades de cálculos de álgebra lineal, podrías
    querer usar matrices, no DataFrames. Los DataFrames son ideales para combinar
    columnas de diferentes tipos.
"""

# 3. Gráficas ====

import matplotlib as mpl, matplotlib.pyplot as plt
%matplotlib inline

# Para hacer virtualización de datos en Python, usa matplotlib

plt.hist(pets.age);

plt.boxplot(pets.weight);

plt.scatter(pets.age, pets.weight); plt.xlabel("age"); plt.ylabel("weight");

# seaborn está por encima de matplotlib y logra mejores gráficos

import seaborn as sns

plt.scatter(pets.age, pets.weight); plt.xlabel("age"); plt.ylabel("weight");

# también hay algunas funciones gráficas específicas de seaborn
# nota como seaborn etiqueta automáticamente el eje x en este gráfico de barras
sns.barplot(pets["age"])

# los veteranos de R pueden seguir usando ggplot
from ggplot import *
ggplot(aes(x="age",y="weight"), data=pets) + geom_point() + labs(title="pets")
# fuente: https://pypi.python.org/pypi/ggplot

# incluso hay un porteo d3.js: https://github.com/mikedewar/d3py

# 4. Limpieza simple de datos y análisis exploratorio ====

""" Tenemos ahora un ejemplo más complicado que demuestra un flujo básico para
    limpieza de datos que lleva a la creación de algunos gráficos exploratorios
    y la ejecución de una regresión lineal.
        El conjunto de datos fue transcrito de Wikipedia a mano. Contiene
    todos los Emperadores Romanos Sagrados y fechas claves en sus vidas
    (nacimiento, muerte, coronación, etc.).
        El objetivo del análisis es explorar si existe alguna relación
    entre el año de nacimiento del Emperador y su tiempo de vida.
    fuente de datos: https://en.wikipedia.org/wiki/Holy_Roman_Emperor
"""

# cargar algunos datos de los Emperadores Romanos Sagrados
url = "https://raw.githubusercontent.com/e99n09/R-notes/master/data/hre.csv"
r = requests.get(url)
fp = "hre.csv"
f = open(fp,"wb")
f.write(r.text.encode("UTF-8"))
f.close()

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

# limpiar las columnas de Nacimiento y Muerte

import re # módulo para expresiones regulares

rx = re.compile(r'\d+$') # coincidencia de últimos dígitos

""" Esta función aplica una expresión regular a una columna de entrada (Birth,
    Death), nivela la lista resultante, la convierte en un objeto Series, y
    finalmente convierte el tipo del objeto Series de string a entero. Para
    más información sobre que hace cada parte del código, ver:
      - https://docs.python.org/2/howto/regex.html
      - http://stackoverflow.com/questions/11860476/how-to-unlist-a-python-list
      - http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.html
"""
def extractYear(v):
    return(pd.Series(reduce(lambda x,y: x+y,map(rx.findall,v),[])).astype(int))

hre["BirthY"] = extractYear(hre.Birth)
hre["DeathY"] = extractYear(hre.Death)

# hacer una columna decir la edad estimada
hre["EstAge"] = hre.DeathY.astype(int) - hre.BirthY.astype(int)

# gráfica de dispersión simple, sin línea de tendencia, el color representa dinastía
sns.lmplot("BirthY", "EstAge", data=hre, hue="Dynasty", fit_reg=False);

# usa scipy para hacer regresiones lineales
from scipy import stats
(slope,intercept,rval,pval,stderr)=stats.linregress(hre.BirthY,hre.EstAge)
# código fuente: http://wiki.scipy.org/Cookbook/LinearRegression

# verifica la pendiente (slope)
slope # 0.0057672618839073328

# verifica el valor R^2 :
rval**2 # 0.020363950027333586

# verifica el valor p
pval # 0.34971812581498452

# usa seaborn para hacer un gráfico de dispersión y dibujar una regresión lineal
# de la tendencia
sns.lmplot("BirthY", "EstAge", data=hre);

""" Para más información sobre seaborn, ver
      - http://web.stanford.edu/~mwaskom/software/seaborn/
      - https://github.com/mwaskom/seaborn
    Para más información sobre SciPy, ver
      - http://wiki.scipy.org/SciPy
      - http://wiki.scipy.org/Cookbook/
    Para ver una versión del análisis de los Emperadores Romanos usando R, ver
      - http://github.com/e99n09/R-notes/blob/master/holy_roman_emperors_dates.R
"""
```

Si quieres aprender más, obtén _Python for Data Analysis_ por Wes McKinney. Es un extraordinario recurso usado como referencia para escribir este tutorial.

También puedes encontrar gran cantidad de tutoriales interactivos de IPython en temas específicos a tus intereses, como Pilon de Cam Davidson <a href="http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/" Title="Probabilistic Programming and Bayesian Methods for Hackers">Probabilistic Programming and Bayesian Methods for Hackers</a>.

Ver más módulos para investigar:
   - análisis de texto y procesamiento natural del lenguaje: nltk, http://www.nltk.org
   - análisis de redes sociales: igraph, http://igraph.org/python/
