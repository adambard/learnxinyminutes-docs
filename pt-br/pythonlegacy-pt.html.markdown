---
language: Python 2 (legacy)
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["Vilson Vieira", "http://automata.cc"]
lang: pt-br
filename: learnpythonlegacy-pt.py
---

Python foi criado por Guido Van Rossum no começo dos anos 90. Atualmente é uma
das linguagens de programação mais populares. Eu me apaixonei por Python, por
sua clareza de sintaxe. É basicamente pseudocódigo executável.

Comentários serão muito apreciados! Você pode me contactar em
[@louiedinh](http://twitter.com/louiedinh) ou louiedinh [arroba]
[serviço de email do google]

Nota: Este artigo usa Python 2.7 especificamente, mas deveria ser aplicável a
qualquer Python 2.x. Logo haverá uma versão abordando Python 3!

```python
# Comentários de uma linha começam com cerquilha (ou sustenido)
""" Strings de várias linhas podem ser escritas
    usando três ", e são comumente usadas
    como comentários
"""

####################################################
## 1. Tipos de dados primitivos e operadores
####################################################

# Você usa números normalmente
3 #=> 3

# Operadores matemáticos são aqueles que você já está acostumado
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# A divisão é um pouco estranha. A divisão de números inteiros arredonda
# para baixo o resultado, automaticamente
5 / 2 #=> 2

# Para concertar a divisão, precisamos aprender sobre números de ponto
# flutuante (conhecidos como 'float').
2.0     # Isso é um 'float'
11.0 / 4.0 #=> 2.75 ahhh... muito melhor

# Forçamos a precedência de operadores usando parênteses
(1 + 3) * 2 #=> 8

# Valores booleanos (ou 'boolean') são também tipos primitivos
True
False

# Negamos usando 'not'
not True #=> False
not False #=> True

# Testamos igualdade usando '=='
1 == 1 #=> True
2 == 1 #=> False

# E desigualdade com '!='
1 != 1 #=> False
2 != 1 #=> True

# Mais comparações
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# As comparações podem ser encadeadas!
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Strings são criadas com " ou '
"Isso é uma string."
'Isso também é uma string.'

# Strings podem ser somadas (ou melhor, concatenadas)!
"Olá " + "mundo!" #=> "Olá mundo!"

# Uma string pode ser tratada como uma lista de caracteres
"Esta é uma string"[0] #=> 'E'

# O caractere % pode ser usado para formatar strings, desta forma:
"%s podem ser %s" % ("strings", "interpoladas")

# Um jeito novo de formatar strings é usando o método 'format'.
# Esse método é o jeito mais usado
"{0} podem ser {1}".format("strings", "formatadas")
# Você pode usar palavras-chave (ou 'keywords') se você não quiser contar.
"{nome} quer comer {comida}".format(nome="João", comida="lasanha")

# 'None' é um objeto
None #=> None

# Não use o operador de igualdade `==` para comparar objetos com 'None'
# Ao invés disso, use `is`
"etc" is None #=> False
None is None  #=> True

# O operador 'is' teste a identidade de um objeto. Isso não é
# muito útil quando estamos lidando com valores primitivos, mas é
# muito útil quando lidamos com objetos.

# None, 0, e strings/listas vazias são todas interpretadas como 'False'.
# Todos os outros valores são 'True'
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Variáveis e Coleções
####################################################

# Imprimir na tela é muito fácil
print "Eu sou o Python. Prazer em te conhecer!"


# Nós não precisamos declarar variáveis antes de usá-las, basta usar!
alguma_variavel = 5    # A convenção é usar caixa_baixa_com_sobrescritos
alguma_variavel #=> 5

# Acessar uma variável que não teve nenhum valor atribuído anteriormente é
# uma exceção.
# Veja a seção 'Controle' para aprender mais sobre tratamento de exceção.
outra_variavel  # Gera uma exceção de erro de nome

# 'if' pode ser usado como uma expressão
"uepa!" if 3 > 2 else 2 #=> "uepa!"

# Listas armazenam sequências de elementos
lista = []
# Você pode inicializar uma lista com valores
outra_lista = [4, 5, 6]

# Adicione elementos no final da lista usando 'append'
lista.append(1)    # lista é agora [1]
lista.append(2)    # lista é agora [1, 2]
lista.append(4)    # lista é agora [1, 2, 4]
lista.append(3)    # lista é agora [1, 2, 4, 3]
# Remova elementos do fim da lista usando 'pop'
lista.pop()        #=> 3 e lista é agora [1, 2, 4]
# Vamos adicionar o elemento novamente
lista.append(3)    # lista agora é [1, 2, 4, 3] novamente.

# Acesse elementos de uma lista através de seu índices
lista[0] #=> 1
# Acesse o último elemento com índice negativo!
lista[-1] #=> 3

# Tentar acessar um elemento fora dos limites da lista gera uma exceção
# do tipo 'IndexError'
lista[4] # Gera uma exceção 'IndexError'

# Você pode acessar vários elementos ao mesmo tempo usando a sintaxe de
# limites
# (Para quem gosta de matemática, isso é um limite fechado/aberto)
lista[1:3] #=> [2, 4]
# Você pode omitir o fim se quiser os elementos até o final da lista
lista[2:] #=> [4, 3]
# O mesmo para o início
lista[:3] #=> [1, 2, 4]

# Remova um elemento qualquer de uma lista usando 'del'
del lista[2] # lista agora é [1, 2, 3]

# Você pode somar listas (obs: as listas originais não são modificadas)
lista + outra_lista #=> [1, 2, 3, 4, 5, 6]

# Você também pode concatenar usando o método 'extend' (lista será modificada!)
lista.extend(outra_lista) # Agora lista é [1, 2, 3, 4, 5, 6]

# Para checar se um elemento pertence a uma lista, use 'in'
1 in lista #=> True

# Saiba quantos elementos uma lista possui com 'len'
len(lista) #=> 6


# Tuplas são iguais a listas, mas são imutáveis
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # Isso gera uma exceção do tipo TypeError

# Você pode fazer nas tuplas todas aquelas coisas fez com a lista
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Você pode 'desempacotar' tuplas (ou listas) em variáveis, associando cada
# elemento da tupla/lista a uma variável correspondente
a, b, c = (1, 2, 3)     # a agora é 1, b agora é 2, c agora é 3
# Tuplas são criadas por padrão, mesmo se você não usar parênteses
d, e, f = 4, 5, 6
# Sabendo disso, veja só como é fácil trocar os valores de duas variáveis!
e, d = d, e     # d agora é 5, e agora é 4


# Dicionários armazenam 'mapeamentos' (do tipo chave-valor)
dicionario_vazio = {}
# Aqui criamos um dicionário já contendo valores
dicionario = {"um": 1, "dois": 2, "três": 3}

# Acesse valores usando []
dicionario["um"] #=> 1

# Retorna uma lista com todas as chaves do dicionário
dicionario.keys() #=> ["três", "dois", "um"]
# Nota: A ordem das chaves não é garantida.
# O resultado no seu interpretador não necessariamente será igual a esse.

# Retorna uma lista com todos os valores do dicionário
dicionario.values() #=> [3, 2, 1]
# Nota: A mesma nota acima sobre a ordenação é válida aqui.

# Veja se uma chave qualquer está em um dicionário usando 'in'
"um" in dicionario #=> True
1 in dicionario #=> False

# Tentar acessar uma chave que não existe gera uma exceção do tipo 'KeyError'
dicionario["quatro"] # Gera uma exceção KeyError

# Você pode usar o método 'get' para evitar gerar a exceção 'KeyError'.
# Ao invés de gerar essa exceção, irá retornar 'None' se a chave não existir.
dicionario.get("um") #=> 1
dicionario.get("quatro") #=> None
# O método 'get' suporta um argumento que diz qual valor deverá ser
# retornado se a chave não existir (ao invés de 'None').
dicionario.get("um", 4) #=> 1
dicionario.get("quatro", 4) #=> 4

# O método 'setdefault' é um jeito seguro de adicionar um novo par
# chave-valor a um dicionário, associando um valor padrão imutável à uma chave
dicionario.setdefault("cinco", 5) # dicionario["cinco"] é definido como 5
dicionario.setdefault("cinco", 6) # dicionario["cinco"] ainda é igual a 5


# Conjuntos (ou sets) armazenam ... bem, conjuntos
# Nota: lembre-se que conjuntos não admitem elementos repetidos!
conjunto_vazio = set()
# Podemos inicializar um conjunto com valores
conjunto = set([1, 2, 2, 3, 4]) # conjunto é set([1, 2, 3, 4]), sem repetição!

# Desde o Python 2.7, {} pode ser usado para declarar um conjunto
conjunto = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Adicione mais ítens a um conjunto com 'add'
conjunto.add(5) # conjunto agora é {1, 2, 3, 4, 5}

# Calcule a intersecção de dois conjuntos com &
outro_conj = {3, 4, 5, 6}
conjunto & outro_conj #=> {3, 4, 5}

# Calcule a união de dois conjuntos com |
conjunto | outro_conj #=> {1, 2, 3, 4, 5, 6}

# E a diferença entre dois conjuntos com -
{1,2,3,4} - {2,3,5} #=> {1, 4}

# Veja se um elemento existe em um conjunto usando 'in'
2 in conjunto #=> True
10 in conjunto #=> False


####################################################
## 3. Controle
####################################################

# Para começar, vamos apenas criar uma variável
alguma_var = 5

# Aqui está uma expressão 'if'. Veja como a identação é importante em Python!
# Esses comandos irão imprimir "alguma_var é menor que 10"
if alguma_var > 10:
    print "some_var é maior que 10."
elif some_var < 10:    # Esse 'elif' é opcional
    print "some_var é menor que 10."
else:           # Esse 'else' também é opcional
    print "some_var é igual a 10."


"""
Laços (ou loops) 'for' iteram em listas.
Irá imprimir:
    cachorro é um mamífero
    gato é um mamífero
    rato é um mamífero
"""
for animal in ["cachorro", "gato", "rato"]:
    # Você pode usar % para interpolar strings formatadas
    print "%s é um mamífero" % animal
    
"""
A função `range(um número)` retorna uma lista de números
do zero até o número dado.
Irá imprimir:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
Laços 'while' executam enquanto uma condição dada for verdadeira.
Irá imprimir:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Isso é um atalho para a expressão x = x + 1

# Tratamos excessões usando o bloco try/except
# Funciona em Python 2.6 e versões superiores:

try:
    # Use 'raise' para gerar um erro
    raise IndexError("Isso é um erro de índice")
except IndexError as e:
    pass    # Pass é um operador que não faz nada, deixa passar.
            # Usualmente você iria tratar a exceção aqui...


####################################################
## 4. Funções
####################################################

# Use 'def' para definir novas funções
def soma(x, y):
    print "x é %s e y é %s" % (x, y)
    return x + y    # Retorne valores usando 'return'

# Chamando funções com parâmetros
soma(5, 6) #=> imprime "x é 5 e y é 6" e retorna o valor 11

# Um outro jeito de chamar funções é especificando explicitamente os valores
# de cada parâmetro com chaves
soma(y=6, x=5)   # Argumentos com chaves podem vir em qualquer ordem.

# Você pode definir funções que recebem um número qualquer de argumentos
# (respeitando a sua ordem)
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# Você também pode definir funções que recebem um número qualquer de argumentos
# com chaves
def args_com_chaves(**ch_args):
    return ch_args

# Vamos chamar essa função para ver o que acontece
args_com_chaves(pe="grande", lago="Ness") #=> {"pe": "grande", "lago": "Ness"}

# Você pode fazer as duas coisas ao mesmo tempo, se desejar
def todos_args(*args, **ch_wargs):
    print args
    print ch_args
"""
todos_args(1, 2, a=3, b=4) imprime:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Quando você chamar funções, pode fazer o oposto do que fizemos até agora!
# Podemos usar * para expandir tuplas de argumentos e ** para expandir
# dicionários de argumentos com chave.
args = (1, 2, 3, 4)
ch_args = {"a": 3, "b": 4}
todos_args(*args)  # equivalente a todos_args(1, 2, 3, 4)
todos_args(**ch_args) # equivalente a todos_args(a=3, b=4)
todos_args(*args, **ch_args) # equivalente a todos_args(1, 2, 3, 4, a=3, b=4)

# Em Python, funções são elementos de primeira ordem (são como objetos, 
# strings ou números)
def cria_somador(x):
    def somador(y):
        return x + y
    return somador

soma_10 = cria_somador(10)
soma_10(3) #=> 13

# Desta forma, existem também funções anônimas
(lambda x: x > 2)(3) #=> True

# E existem funções de alta ordem por padrão
map(soma_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]
reduce(lambda x, y: x + y, [3, 4, 5, 6, 7]) #=> 25

# Nós podemos usar compreensão de listas para mapear e filtrar também
[soma_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. Classes
####################################################

# Para criar uma nova classe, devemos herdar de 'object'
class Humano(object):

    # Um atributo de classe. Ele é compartilhado por todas as instâncias dessa
    # classe
    especie = "H. sapiens"

    # Definimos um inicializador básico
    def __init__(self, nome):
        # Atribui o valor de argumento dado a um atributo da instância
        self.nome = nome

    # Um método de instância. Todos os métodos levam 'self' como primeiro
    # argumento
    def diga(self, msg):
       return "%s: %s" % (self.nome, msg)

    # Um método de classe é compartilhado por todas as instâncias
    # Eles são chamados passando o nome da classe como primeiro argumento
    @classmethod
    def get_especie(cls):
        return cls.especie

    # Um método estático é chamado sem uma referência a classe ou instância
    @staticmethod
    def ronca():
        return "*arrrrrrr*"


# Instancie uma classe
i = Humano(nome="Ivone")
print i.diga("oi")     # imprime "Ivone: oi"

j = Human("Joel")
print j.say("olá")  #prints out "Joel: olá"

# Chame nosso método de classe
i.get_especie() #=> "H. sapiens"

# Modifique um atributo compartilhado
Humano.especie = "H. neanderthalensis"
i.get_especie() #=> "H. neanderthalensis"
j.get_especie() #=> "H. neanderthalensis"

# Chame o método estático
Humano.ronca() #=> "*arrrrrrr*"


####################################################
## 6. Módulos
####################################################

# Você pode importar módulos
import math
print math.sqrt(16) #=> 4.0

# Você pode importar funções específicas de um módulo
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# Você também pode importar todas as funções de um módulo
# Atenção: isso não é recomendado!
from math import *

# Você pode usar apelidos para os módulos, encurtando seus nomes
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Módulos em Python são apenas arquivos Python. Você
# pode escrever o seu próprio módulo e importá-lo. O nome do
# módulo será o mesmo que o nome do arquivo.

# Você pode descobrir quais funções e atributos
# estão definidos em um módulo qualquer.
import math
dir(math)


```

## Pronto para mais?

### Online e gratuito

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### Livros impressos

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

