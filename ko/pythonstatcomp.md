---
category: framework
name: Statistical computing with Python
contributors:
    - ["e99n09", "https://github.com/e99n09"]
filename: pythonstatcomp.py
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

이것은 파이썬을 사용하여 몇 가지 일반적인 통계 프로그래밍 작업을 수행하는 방법에 대한 튜토리얼입니다. 기본적으로 파이썬에 익숙하고 R, Stata, SAS, SPSS 또는 MATLAB과 같은 언어로 통계 프로그래밍 경험이 있는 사람들을 대상으로 합니다.

```python
# 0. 설정하기 ====

""" 시작하려면 다음을 pip 설치하십시오: jupyter, numpy, scipy, pandas,
    matplotlib, seaborn, requests.
        인라인 플롯과 쉬운 문서 조회를 위해 이 튜토리얼을
    Jupyter 노트북에서 수행해야 합니다. 셸 명령어는
    `jupyter notebook`이며, New -> Python을 클릭하십시오.
"""

# 1. 데이터 수집 ====

""" 사람들이 R보다 파이썬을 선택하는 한 가지 이유는 웹과
    많이 상호 작용하려는 의도 때문입니다. 직접 페이지를 스크래핑하거나
    API를 통해 데이터를 요청하는 등. R에서도 이러한 작업을 할 수 있지만,
    이미 파이썬을 사용하는 프로젝트의 맥락에서는 한 가지 언어를
    고수하는 것이 이점이 있습니다.
"""

import requests  # HTTP 요청용 (웹 스크래핑, API)
import os

# 웹 스크래핑
r = requests.get("https://github.com/adambard/learnxinyminutes-docs")
r.status_code  # 200이면 요청이 성공한 것입니다.
r.text  # 원시 페이지 소스
print(r.text)  # 예쁘게 서식 지정됨
# 페이지 소스를 파일에 저장:
os.getcwd()  # 작업 디렉토리 확인
with open("learnxinyminutes.html", "wb") as f:
    f.write(r.text.encode("UTF-8"))

# csv 다운로드
fp = "https://raw.githubusercontent.com/adambard/learnxinyminutes-docs/master/"
fn = "pets.csv"
r = requests.get(fp + fn)
print(r.text)
with open(fn, "wb") as f:
    f.write(r.text.encode("UTF-8"))

""" requests 모듈에 대한 자세한 내용, API 포함,
    http://docs.python-requests.org/en/latest/user/quickstart/ 참조
"""

# 2. CSV 파일 읽기 ====

""" Wes McKinney의 pandas 패키지는 파이썬에서 'DataFrame' 객체를 제공합니다.
    R을 사용해 본 적이 있다면 이미 "data.frame"의 개념에 익숙할 것입니다.
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

""" R 사용자는 C의 영향을 받은 대부분의 프로그래밍 언어와 마찬가지로 파이썬은
    0부터 인덱싱을 시작한다는 점에 유의해야 합니다. R은 Fortran의 영향으로 1부터
    인덱싱을 시작합니다.
"""

# 열을 출력하는 두 가지 다른 방법
pets.age
pets["age"]

pets.head(2)  # 처음 2개 행 출력
pets.tail(1)  # 마지막 행 출력

pets.name[1]  # 'vesuvius'
pets.species[0]  # 'cat'
pets["weight"][2]  # 34

# R에서는 이렇게 하면 3개의 행을 얻을 것으로 예상하지만, 여기서는 2개를 얻습니다.
pets.age[0:2]
# 0    3
# 1    6

sum(pets.age) * 2  # 28
max(pets.weight) - min(pets.weight)  # 20

""" 심각한 선형 대수 및 숫자 계산을 수행하는 경우
    DataFrame이 아닌 배열을 원할 수 있습니다. DataFrame은
    다른 유형의 열을 결합하는 데 이상적입니다.
"""

# 3. 차트 ====

import matplotlib as mpl
import matplotlib.pyplot as plt
%matplotlib inline

# 파이썬에서 데이터 시각화를 하려면 matplotlib를 사용하십시오.

plt.hist(pets.age);

plt.boxplot(pets.weight);

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# seaborn은 matplotlib 위에 있으며 플롯을 더 예쁘게 만듭니다.

import seaborn as sns

plt.scatter(pets.age, pets.weight)
plt.xlabel("age")
plt.ylabel("weight");

# seaborn 전용 플로팅 함수도 있습니다.
# 이 막대 그래프에서 seaborn이 x축에 자동으로 레이블을 지정하는 방법을 확인하십시오.
sns.barplot(pets["age"])

# R 베테랑은 여전히 ggplot을 사용할 수 있습니다.
from ggplot import *
ggplot(aes(x="age",y="weight"), data=pets) + geom_point() + labs(title="pets")
# 출처: https://pypi.python.org/pypi/ggplot

# d3.js 포트도 있습니다: https://github.com/mikedewar/d3py

# 4. 간단한 데이터 정리 및 탐색적 분석 ====

""" 다음은 일부 탐색적 플롯 생성 및 선형 회귀 실행으로
    이어지는 기본 데이터 정리 워크플로를 보여주는 더 복잡한 예입니다.
        데이터 세트는 위키백과에서 직접 필사되었습니다. 여기에는
    모든 신성 로마 황제와 그들의 삶의 중요한 이정표(출생, 사망,
    대관식 등)가 포함되어 있습니다.
        분석의 목표는 황제 출생 연도와 황제 수명 사이에
    관계가 있는지 탐색하는 것입니다.
    데이터 출처: https://en.wikipedia.org/wiki/Holy_Roman_Emperor
"""

# 신성 로마 황제에 대한 일부 데이터 로드
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

# 출생 및 사망 열 정리

import re  # 정규식 모듈

rx = re.compile(r'\d+$')  # 후행 숫자 일치

""" 이 함수는 정규식을 입력 열(여기서는 출생, 사망)에 적용하고,
    결과 목록을 평탄화하고, Series 객체로 변환하고,
    마지막으로 Series 객체의 유형을 문자열에서 정수로 변환합니다.
    코드의 다른 부분이 무엇을 하는지에 대한 자세한 내용은 다음을 참조하십시오.
      - https://docs.python.org/2/howto/regex.html
      - http://stackoverflow.com/questions/11860476/how-to-unlist-a-python-list
      - http://pandas.pydata.org/pandas-docs/stable/generated/pandas.Series.html
"""

from functools import reduce

def extractYear(v):
    return(pd.Series(reduce(lambda x, y: x + y, map(rx.findall, v), [])).astype(int))

hre["BirthY"] = extractYear(hre.Birth)
hre["DeathY"] = extractYear(hre.Death)

# 예상 수명을 알려주는 열 만들기
hre["EstAge"] = hre.DeathY.astype(int) - hre.BirthY.astype(int)

# 간단한 산점도, 추세선 없음, 색상은 왕조를 나타냄
sns.lmplot("BirthY", "EstAge", data=hre, hue="Dynasty", fit_reg=False)

# scipy를 사용하여 선형 회귀 실행
from scipy import stats
(slope, intercept, rval, pval, stderr) = stats.linregress(hre.BirthY, hre.EstAge)
# 코드 출처: http://wiki.scipy.org/Cookbook/LinearRegression

# 기울기 확인
slope  # 0.0057672618839073328

# R^2 값 확인:
rval**2  # 0.020363950027333586

# p-값 확인
pval  # 0.34971812581498452

# seaborn을 사용하여 산점도를 만들고 선형 회귀 추세선 플롯
sns.lmplot("BirthY", "EstAge", data=hre)

""" seaborn에 대한 자세한 내용은 다음을 참조하십시오.
      - http://web.stanford.edu/~mwaskom/software/seaborn/
      - https://github.com/mwaskom/seaborn
    SciPy에 대한 자세한 내용은 다음을 참조하십시오.
      - http://wiki.scipy.org/SciPy
      - http://wiki.scipy.org/Cookbook/
    R을 사용한 신성 로마 황제 분석 버전을 보려면 다음을 참조하십시오.
      - http://github.com/e99n09/R-notes/blob/master/holy_roman_emperors_dates.R
"""
```

더 배우고 싶다면 Wes McKinney의 _Python for Data Analysis_를 구하십시오. 이 튜토리얼을 작성할 때 참고 자료로 사용한 훌륭한 자료입니다.

Cam Davidson-Pilon의 [해커를 위한 확률론적 프로그래밍 및 베이지안 방법](http://camdavidsonpilon.github.io/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/)과 같이 관심 분야에 특정한 주제에 대한 대화형 IPython 튜토리얼도 많이 찾을 수 있습니다.

연구할 추가 모듈:

   - 텍스트 분석 및 자연어 처리: [nltk](http://www.nltk.org)
   - 소셜 네트워크 분석: [igraph](http://igraph.org/python/)
