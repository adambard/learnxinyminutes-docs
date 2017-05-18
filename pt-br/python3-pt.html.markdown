---
language: python3
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
translators:
    - ["Paulo Henrique Rodrigues Pinheiro", "http://www.sysincloud.it"]
lang: pt-br
filename: learnpython3-pt.py
---

Python foi criado por Guido Van Rossum nos anos 1990. Ele é atualmente uma
das mais populares linguagens em existência. Eu fiquei morrendo de amor
pelo Python por sua clareza sintática. É praticamente pseudocódigo executável.

Suas opiniões são grandemente apreciadas. Você pode encontrar-me em
[@louiedinh](http://twitter.com/louiedinh) ou louiedinh [em]
[serviço de e-mail do google].

Observação: Este artigo trata de Python 3 especificamente. Verifique
[aqui](http://learnxinyminutes.com/docs/pt-br/python-pt/) se você pretende
aprender o velho Python 2.7.

```python

# Comentários em uma única linha começam com uma cerquilha (também conhecido por sustenido).

""" Strings de várias linhas podem ser escritas
    usando três ", e são comumente usadas
    como comentários.
"""

####################################################
## 1. Tipos de dados primitivos e operadores
####################################################

# Você usa números normalmente
3  # => 3

# Matemática é como você espera que seja
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20

# Números inteiros por padrão, exceto na divisão, que retorna número
# de ponto flutuante (float).
35 / 5  # => 7.0

# O resultado da divisão inteira arredonda para baixo tanto para números
# positivos como para negativos.
5 // 3       # => 1
5.0 // 3.0   # => 1.0 # funciona em float também
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# Quando você usa um float, o resultado é float.
3 * 2.0  # => 6.0

# operador módulo
7 % 3  # => 1

# Exponenciação (x**y, x elevado à potência y)
2**4  # => 16

# Determine a precedência usando parêntesis
(1 + 3) * 2  # => 8

# Valores lógicos são primitivos (Atenção à primeira letra maiúscula)
True
False

# negação lógica com not
not True   # => False
not False  # => True

# Operadores lógicos
# Observe que "and" e "or" são sensíveis a maiúsculas e minúsculas
True and False  # => False
False or True   # => True

# Observe a utilização de operadores lógicos com números inteiros
0 and 2     # => 0
-5 or 0     # => -5
0 == False  # => True
2 == True   # => False
1 == True   # => True

# Igualdade é ==
1 == 1  # => True
2 == 1  # => False

# Diferença é !=
1 != 1  # => False
2 != 1  # => True

# Mais comparações
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Comparações podem ser agrupadas
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (operador 'is' e operador '==') is verifica se duas referenciam um
# mesmo objeto, mas == verifica se as variáveis apontam para o
# mesmo valor.
a = [1, 2, 3, 4]  # Referência a uma nova lista, [1, 2, 3, 4]
b = a             # b referencia o que está referenciado por a
b is a            # => True, a e b referenciam o mesmo objeto
b == a            # => True, objetos a e b tem o mesmo conteúdo
b = [1, 2, 3, 4]  # Referência a uma nova lista, [1, 2, 3, 4]
b is a            # => False, a e b não referenciam o mesmo objeto
b == a            # => True, objetos a e b tem o mesmo conteúdo

# Strings são criadas com " ou '
"Isto é uma string."
'Isto também é uma string.'

# Strings também podem ser somadas! Mas tente não fazer isso.
"Olá " + "mundo!"  # => "Olá mundo!"
# Strings podem ser somadas sem usar o '+'
"Olá " "mundo!"    # => "Olá mundo!"

# Uma string pode ser manipulada como se fosse uma lista de caracteres
"Isso é uma string"[0]  # => 'I'

# .format pode ser usado para formatar strings, dessa forma:
"{} podem ser {}".format("Strings", "interpoladas")  # => "Strings podem ser interpoladas"

# Você pode repetir os argumentos para digitar menos.
"Seja ágil {0}, seja rápido {0}, salte sobre o {1} {0}".format("Jack", "castiçal")
# => "Seja ágil Jack, seja rápido Jack, salte sobre o castiçal Jack."

# Você pode usar palavras-chave se quiser contar.
"{nome} quer comer {comida}".format(nome="Beto", comida="lasanha")  # => "Beto quer comer lasanha"

# Se você precisa executar seu código Python3 com um interpretador Python 2.5 ou acima, você pode usar a velha forma para formatação de texto:
"%s podem ser %s da forma %s" % ("Strings", "interpoladas", "antiga")  # => "Strings podem ser interpoladas da forma antiga"


# None é um objeto
None  # => None

# Não use o operador de igualdade "==" para comparar objetos com None
# Use "is" para isso. Ele checará pela identidade dos objetos.
"etc" is None  # => False
None is None   # => True

# None, 0, e strings/listas/dicionários vazios todos retornam False.
# Qualquer outra coisa retorna True
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False


####################################################
## 2. Variáveis e coleções
####################################################

# Python tem uma função print
print("Eu sou o Python. Prazer em conhecer!")  # => Eu sou o Python. Prazer em conhecer!

# Por padrão a função print também imprime o caractere de nova linha ao final.
# Use o argumento opcional end para mudar o caractere final.
print("Olá, Mundo", end="!")  # => Olá, Mundo!

# Forma simples para capturar dados de entrada via console
input_string_var = input("Digite alguma coisa: ") # Retorna o que foi digitado em uma string
# Observação: Em versões antigas do Python, o método input() era chamado raw_input()

# Não é necessário declarar variáveis antes de iniciá-las
# È uma convenção usar letras_minúsculas_com_sublinhados
alguma_variavel = 5
alguma_variavel  # => 5

# Acessar uma variável que não tenha sido inicializada gera uma exceção.
# Veja Controle de Fluxo para aprender mais sobre tratamento de exceções.
alguma_variavel_nao_inicializada  # Gera a exceção NameError

# Listas armazenam sequencias
li = []
# Você pode iniciar com uma lista com alguns valores
outra_li = [4, 5, 6]

# Adicionar conteúdo ao fim da lista com append
li.append(1)    # li agora é [1]
li.append(2)    # li agora é [1, 2]
li.append(4)    # li agora é [1, 2, 4]
li.append(3)    # li agora é [1, 2, 4, 3]
# Remover do final da lista com pop
li.pop()        # => 3 e agora li é [1, 2, 4]
# Vamos colocá-lo lá novamente!
li.append(3)    # li agora é [1, 2, 4, 3] novamente.

# Acessar uma lista da mesma forma que você faz com um array
li[0]   # => 1
# Acessa o último elemento
li[-1]  # => 3

# Acessando além dos limites gera um IndexError
li[4]  # Gera o IndexError

# Você pode acessar vários elementos com a sintaxe de limites
# (É um limite fechado, aberto pra você que gosta de matemática.)
li[1:3]   # => [2, 4]
# Omitindo o final
li[2:]    # => [4, 3]
# Omitindo o início
li[:3]    # => [1, 2, 4]
# Selecione cada segunda entrada
li[::2]   # => [1, 4]
# Tenha uma cópia em ordem invertida da lista
li[::-1]  # => [3, 4, 2, 1]
# Use qualquer combinação dessas para indicar limites complexos
# li[inicio:fim:passo]

# Faça uma cópia profunda de um nível usando limites
li2 = li[:]  # => li2 = [1, 2, 4, 3] mas (li2 is li) resultará em False.

# Apague elementos específicos da lista com "del"
del li[2]  # li agora é [1, 2, 3]

# Você pode somar listas
# Observação: valores em li e other_li não são modificados.
li + other_li  # => [1, 2, 3, 4, 5, 6]

# Concatene listas com "extend()"
li.extend(other_li)  # Agora li é [1, 2, 3, 4, 5, 6]

# Verifique se algo existe na lista com "in"
1 in li  # => True

# Examine  tamanho com "len()"
len(li)  # => 6


# Tuplas são como l istas, mas imutáveis.
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # Gera um TypeError

# Observe que uma tupla de tamanho um precisa ter uma vírgula depois do
# último elemento mas tuplas de outros tamanhos, mesmo vazias, não precisa,.
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# Você pode realizar com tuplas a maior parte das operações que faz com listas
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# Você pode desmembrar tuplas (ou listas) em variáveis.
a, b, c = (1, 2, 3)  # a é 1, b é 2 e c é 3
# Por padrão, tuplas são criadas se você não coloca parêntesis.
d, e, f = 4, 5, 6
# Veja como é fácil permutar dois valores
e, d = d, e  # d é 5, e é 4

# Dicionários armazenam mapeamentos
empty_dict = {}
# Aqui está um dicionário preenchido na definição da referência
filled_dict = {"um": 1, "dois": 2, "três": 3}

# Observe que chaves para dicionários devem ser tipos imutáveis. Isto é para
# assegurar que a chave pode ser convertida para uma valor hash constante para
# buscas rápidas.
# Tipos imutáveis incluem inteiros, flotas, strings e tuplas.
invalid_dict = {[1,2,3]: "123"}  # => Gera um TypeError: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # Já os valores, podem ser de qualquer tipo.

# Acesse valores com []
filled_dict["um"]  # => 1

# Acesse todas as chaves como um iterável com "keys()". É necessário encapsular
# a chamada com um list() para transformá-las em uma lista. Falaremos sobre isso
# mais adiante. Observe que a ordem de uma chave de dicionário não é garantida.
# Por isso, os resultados aqui apresentados podem não ser exatamente como os
# aqui apresentados.
list(filled_dict.keys())  # => ["três", "dois", "um"]


# Acesse todos os valores de um iterável com "values()". Novamente, é
# necessário encapsular ele com list() para não termos um iterável, e sim os
# valores. Observe que, como foi dito acima, a ordem dos elementos não é
# garantida.
list(filled_dict.values())  # => [3, 2, 1]


# Verifique a existência de chaves em um dicionário com "in"
"um" in filled_dict  # => True
1 in filled_dict      # => False

# Acessar uma chave inexistente gera um KeyError
filled_dict["quatro"]  # KeyError

# Use o método "get()" para evitar um KeyError
filled_dict.get("um")      # => 1
filled_dict.get("quatro")     # => None
# O método get permite um parâmetro padrão para quando não existir a chave
filled_dict.get("um", 4)   # => 1
filled_dict.get("quatro", 4)  # => 4

# "setdefault()" insere em dicionário apenas se a dada chave não existir
filled_dict.setdefault("cinco", 5)  # filled_dict["cinco"] tem valor 5
filled_dict.setdefault("cinco", 6)  # filled_dict["cinco"] continua 5

# Inserindo em um dicionário
filled_dict.update({"quatro":4})  # => {"um": 1, "dois": 2, "três": 3, "quatro": 4}
#filled_dict["quatro"] = 4        #outra forma de inserir em um dicionário

# Remova chaves de um dicionário com del
del filled_dict["um"]  # Remove a chave "um" de filled_dict


# Armazenamento em sets... bem, são conjuntos
empty_set = set()
# Inicializa um set com alguns valores. Sim, ele parece um dicionário. Desculpe.
some_set = {1, 1, 2, 2, 3, 4}  # some_set agora é {1, 2, 3, 4}

# Da mesma forma que chaves em um dicionário, elementos de um set devem ser
# imutáveis.
invalid_set = {[1], 1}  # => Gera um TypeError: unhashable type: 'list'
valid_set = {(1,), 1}

# Pode definir novas variáveis para um conjunto
filled_set = some_set

# Inclua mais um item no set
filled_set.add(5)  # filled_set agora é {1, 2, 3, 4, 5}

# Faça interseção de conjuntos com &
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# Faça união de conjuntos com |
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# Faça a diferença entre conjuntos com -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Verifique a existência em um conjunto com in
2 in filled_set   # => True
10 in filled_set  # => False



####################################################
## 3. Controle de fluxo e iteráveis
####################################################

# Iniciemos um variável
some_var = 5

# Aqui está uma expressão if. Indentação é significante em python!
# imprime "somevar é menor que10"
if some_var > 10:
    print("some_var é absolutamente maior que 10.")
elif some_var < 10:    # Esta cláusula elif é opcional.
    print("some_var é menor que 10.")
else:                  # Isto também é opcional.
    print("some_var é, de fato, 10.")


"""
Laços for iteram sobre listas
imprime:
    cachorro é um mamífero
    gato é um mamífero
    rato é um mamífero
"""
for animal in ["cachorro", "gato", "rato"]:
    # Você pode usar format() para interpolar strings formatadas
    print("{} é um mamífero".format(animal))

"""
"range(número)" retorna um iterável de números
de zero até o número escolhido
imprime:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(menor, maior)" gera um iterável de números
começando pelo menor até o maior
imprime:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(menor, maior, passo)" retorna um iterável de números
começando pelo menor número até o maior númeno, pulando de
passo em passo. Se o passo não for indicado, o valor padrão é um.
imprime:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)
"""

Laços while executam até que a condição não seja mais válida.
imprime:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Maneira mais curta para for x = x + 1

# Lide com exceções com um bloco try/except
try:
    # Use "raise" para gerar um erro
    raise IndexError("Isto é um erro de índice")
except IndexError as e:
    pass                 # Pass é um não-operador. Normalmente você usa algum código de recuperação aqui.
except (TypeError, NameError):
    pass                 # Varias exceções podem ser gerenciadas, se necessário.
else:                    # Cláusula opcional para o bloco try/except. Deve estar após todos os blocos de exceção.
    print("Tudo certo!")   # Executa apenas se o código em try não gera exceção
finally:                 #  Sempre é executado
    print("Nós podemos fazer o código de limpeza aqui.")

# Ao invés de try/finally para limpeza você pode usar a cláusula with
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Python provê uma abstração fundamental chamada Iterável.
# Um iterável é um objeto que pode ser tratado como uma sequência.
# O objeto retornou a função range, um iterável.

filled_dict = {"um": 1, "dois": 2, "três": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => range(1,10). Esse é um objeto que implementa nossa interface iterável.

# Nós podemos percorrê-la.
for i in our_iterable:
    print(i)  # Imprime um, dois, três

# Mas não podemos acessar os elementos pelo seu índice.
our_iterable[1]  # Gera um TypeError

# Um iterável é um objeto que sabe como criar um iterador.
our_iterator = iter(our_iterable)

# Nosso iterador é um objeto que pode lembrar o estado enquanto nós o percorremos.
# Nós acessamos o próximo objeto com "next()".
next(our_iterator)  # => "um"

# Ele mantém o estado enquanto nós o percorremos.
next(our_iterator)  # => "dois"
next(our_iterator)  # => "três"

# Após o iterador retornar todos os seus dados, ele gera a exceção StopIterator
next(our_iterator)  # Gera StopIteration

# Você pode capturar todos os elementos de um iterador aplicando list() nele.
list(filled_dict.keys())  # => Retorna ["um", "dois", "três"]


####################################################
## 4. Funções
####################################################

# Use "def" para criar novas funções.
def add(x, y):
    print("x é {} e y é {}".format(x, y))
    return x + y  # Retorne valores com a cláusula return

# Chamando funções com parâmetros
add(5, 6)  # => imprime "x é 5 e y é 6" e retorna 11

# Outro meio de chamar funções é com argumentos nomeados
add(y=6, x=5)  # Argumentos nomeados podem aparecer em qualquer ordem.

# Você pode definir funções que pegam um número variável de argumentos
# posicionais
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# Você pode definir funções que pegam um número variável de argumentos nomeados
# também
def keyword_args(**kwargs):
    return kwargs

# Vamos chamá-lo para ver o que acontece
keyword_args(peh="grande", lago="ness")  # => {"peh": "grande", "lago": "ness"}


# Você pode fazer ambos simultaneamente, se você quiser
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) imprime:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Quando chamar funções, você pode fazer o oposto de args/kwargs!
# Use * para expandir tuplas e use ** para expandir dicionários!
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # equivalente a foo(1, 2, 3, 4)
all_the_args(**kwargs)         # equivalente a foo(a=3, b=4)
all_the_args(*args, **kwargs)  # equivalente a foo(1, 2, 3, 4, a=3, b=4)

# Retornando múltiplos valores (com atribuição de tuplas)
def swap(x, y):
    return y, x  # Retorna múltiplos valores como uma tupla sem os parêntesis.
                 # (Observação: os parêntesis foram excluídos mas podem estar
                 # presentes)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # Novamente, os parêntesis foram excluídos mas podem estar presentes.

# Escopo de função
x = 5

def setX(num):
    # A variável local x não é a mesma variável global x
    x = num    # => 43
    print (x)  # => 43

def setGlobalX(num):
    global x
    print (x)  # => 5
    x = num    # variável global x agora é 6
    print (x)  # => 6

setX(43)
setGlobalX(6)


# Python tem funções de primeira classe
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# Também existem as funções anônimas
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# TODO - Fix for iterables
# Existem funções internas de alta ordem
map(add_10, [1, 2, 3])          # => [11, 12, 13]
map(max, [1, 2, 3], [4, 2, 1])  # => [4, 2, 3]

filter(lambda x: x > 5, [3, 4, 5, 6, 7])  # => [6, 7]

# Nós podemos usar compreensão de lista para interessantes mapas e filtros
# Compreensão de lista armazena a saída como uma lista que pode ser uma lista
# aninhada
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

####################################################
## 5. Classes
####################################################


# Nós usamos o operador "class" para ter uma classe
class Human:

    # Um atributo de classe. Ele é compartilhado por todas as instâncias dessa
    # classe.
    species = "H. sapiens"

    # Construtor básico, é chamado quando esta classe é instanciada.
    # Note que dois sublinhados no início e no final de uma identificados
    # significa objetos ou atributos que são usados pelo python mas vivem em
    # um namespace controlado pelo usuário. Métodos (ou objetos ou atributos)
    # como: __init__, __str__, __repr__, etc. são chamados métodos mágicos (ou
    # algumas vezes chamados métodos dunder - "double underscore")
    # Você não deve usar nomes assim por sua vontade.
    def __init__(self, name):
        @ Atribui o argumento ao atributo da  instância
        self.name = name

    # Um método de instância. Todos os métodos tem "self" como primeiro
    # argumento
    def say(self, msg):
        return "{name}: {message}".format(name=self.name, message=msg)

    # Um método de classe é compartilhado por todas as instâncias
    # Eles são chamados com a classe requisitante como primeiro argumento
    @classmethod
    def get_species(cls):
        return cls.species

    # Um método estático é chamado sem uma referência a classe ou instância
    @staticmethod
    def grunt():
        return "*grunt*"


# Instancie uma classe
i = Human(name="Ian")
print(i.say("oi"))     # imprime "Ian: oi"

j = Human("Joel")
print(j.say("olá"))  # imprime "Joel: olá"

# Chama nosso método de classe
i.get_species()  # => "H. sapiens"

# Altera um atributo compartilhado
Human.species = "H. neanderthalensis"
i.get_species()  # => "H. neanderthalensis"
j.get_species()  # => "H. neanderthalensis"

# Chama o método estático
Human.grunt()    # => "*grunt*"


####################################################
## 6. Módulos
####################################################

# Você pode importar módulos
import math
print(math.sqrt(16))  # => 4

# Você pode importar apenas funções específicas de um módulo
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# Você pode importar todas as funções de um módulo para o namespace atual
# Atenção: isso não é recomendado
from math import *

# Você pode encurtar o nome dos módulos
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Módulos python são apenas arquivos python comuns. Você
# pode escrever os seus, e importá-los. O nome do
# módulo é o mesmo nome do arquivo.

# Você pode procurar que atributos e funções definem um módulo.
import math
dir(math)


####################################################
## 7. Avançado
####################################################

# Geradores podem ajudar você a escrever código "preguiçoso"
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# Um gerador cria valores conforme necessário.
# Ao invés de gerar e retornar todos os valores de uma só vez ele cria um em
# cada interação. Isto significa que valores maiores que 15 não serão
# processados em double_numbers.
# Nós usamos um sublinhado ao final do nome das variáveis quando queremos usar
# um nome que normalmente colide com uma palavra reservada do python.
range_ = range(1, 900000000)
# Multiplica por 2 todos os números até encontrar um resultado >= 30
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break


# Decoradores
# Neste exemplo beg encapsula say
# beg irá chamar say. Se say_please é verdade então ele irá mudar a mensagem
# retornada
from functools import wraps


def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Por favor! Eu sou pobre :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Você me paga uma cerveja?"
    return msg, say_please


print(say())                # Você me paga uma cerveja?
print(say(say_please=True)) # Você me paga uma cerveja? Por favor! Eu sou pobre :(
```

## Pronto para mais?

### Free Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)

### Dead Tree

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
