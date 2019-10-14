---
language: python3
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["Ygor Sad", "https://github.com/ysads"]
translators:
    - ["Paulo Henrique Rodrigues Pinheiro", "http://www.sysincloud.it"]
lang: pt-br
filename: learnpython3-pt.py
---

Python foi criado por Guido Van Rossum nos anos 1990. Atualmente, é uma linguagem
bastante popular, sobretudo no meio científico. Uma de suas vantagens é a sintaxe
de altíssimo nível, que se aproxima de pseudo-código. 

Observação: Este artigo trata de Python 3 especificamente. Se estiver procurando
algo sobre Python 2.7, dê uma olhada aqui: 
[aqui](http://learnxinyminutes.com/docs/pt-br/python-pt/).

```python

# Comentários em uma única linha começam com uma cerquilha (também conhecido com hashtag ou sharp).

""" Strings de várias linhas podem ser escritas
    usando três ", e são comumente usadas
    como comentários.
"""

####################################################
## 1. Tipos de dados primitivos e operadores
####################################################

# Você pode usar números normalmente
3  # => 3

# Matemática básica funciona como o esperado
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20

# Divisão entre números inteiros, por padrão, retorna números decimais
35 / 5  # => 7.0

# No caso da divisão inteira, ela retorna o resultado arredondado para baixo
# e também funciona com floats
5 // 3       # => 1
5.0 // 3.0   # => 1.0
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# Quando você usa um float, o resultado é float
3 * 2.0  # => 6.0

# O operador módulo retorna o resto da divisão inteira
7 % 3  # => 1

# Exponenciação (x**y, x elevado à potência y)
2 ** 4  # => 16

# Para mudar a precedência dos operadores, use parênteses
(1 + 3) * 2  # => 8

# Valores lógicos são primitivos (Atenção à primeira letra maiúscula)
True
False

# Negação lógica usa not
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

# Operador 'is' e Operador '=='
# - is verifica se duas variáveis referenciam um mesmo objeto na memória
# - == verifica se as variáveis possuem o mesmo valor

a = [1, 2, 3, 4]  # Cria uma nova referência à lista [1, 2, 3, 4]
b = a             # b referencia o que está referenciado por a
b is a            # => True, a e b referenciam o mesmo objeto
b == a            # => True, objetos a e b tem o mesmo conteúdo

b = [1, 2, 3, 4]  # b agora possui uma referência a uma nova lista [1, 2, 3, 4]
b is a            # => False, a e b não referenciam o mesmo objeto
b == a            # => True, objetos a e b tem o mesmo conteúdo

# Strings são criadas com " ou '
"Isto é uma string."
'Isto também é uma string.'

# Strings também podem ser somadas! Mas evite fazer isso
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

# Você também pode usar palavras-chave, se preferir algo mais semântico
"{nome} quer comer {comida}".format(nome="Beto", comida="lasanha")  # => "Beto quer comer lasanha"

# Se você precisa executar seu código Python3 com um interpretador Python 2.5 ou acima, você pode usar a antiga forma para formatação de texto:
"%s podem ser %s da forma %s" % ("Strings", "interpoladas", "antiga")  # => "Strings podem ser interpoladas da forma antiga"

# None é um objeto
None  # => None

# Não use o operador de igualdade "==" para comparar objetos com None.
# Use "is" para isso. Ele checará pela identidade dos objetos.
"etc" is None  # => False
None is None   # => True

# None, 0 e strings/listas/dicionários vazios equivalem a False.
# Qualquer outra coisa retorna True.
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

# Para capturar dados de entrada via console
input_string_var = input("Digite alguma coisa: ") # Retorna o que foi digitado em uma string
# Observação: Em versões antigas do Python, o método input() era chamado raw_input()

# Não é necessário declarar variáveis antes de inicializá-las.
# É uma convenção usar snake_case para nomear variáveis.
alguma_variavel = 5
alguma_variavel  # => 5

# Acessar uma variável que não tenha sido inicializada gera uma exceção.
# Veja a seção "Controle de Fluxo" para aprender mais sobre tratamento de exceções.
alguma_variavel_nao_inicializada  # Gera a exceção NameError

# Listas armazenam sequencias
li = []

# Você pode inicializar uma lista com alguns valores
outra_li = [4, 5, 6]

# Para adicionar conteúdo ao fim da lista, use append
li.append(1)    # li agora é [1]
li.append(2)    # li agora é [1, 2]
li.append(4)    # li agora é [1, 2, 4]
li.append(3)    # li agora é [1, 2, 4, 3]

# Se quiser remover do final da lista, pop pode te ajudar
li.pop()        # => 3 e agora li é [1, 2, 4]

# Vamos colocá-lo lá novamente!
li.append(3)    # li agora é [1, 2, 4, 3] novamente.

# Você pode acessar os elementos de uma lista da mesma forma que você faz com um array
li[0]   # => 1

# Acessa o último elemento
li[-1]  # => 3

# Acessando além dos limites gera um IndexError
li[4]   # Gera o IndexError

# Você pode acessar vários elementos com a sintaxe de limites.
# Note que esse é um limite fechado-aberto, pois não inclui
# o elemento na posição passada como limite superior.
li[1:3]   # => [2, 4]

# Omitindo o final
li[2:]    # => [4, 3]

# Omitindo o início
li[:3]    # => [1, 2, 4]

# Selecione cada segunda entrada
li[::2]   # => [1, 4]

# Se quiser inverter a lista
li[::-1]  # => [3, 4, 2, 1]

# Use qualquer combinação dessas para indicar limites complexos
# li[inicio:fim:passo]

# Faça uma cópia profunda de um nível usando limites
li2 = li[:]  # => li2 = [1, 2, 4, 3]

# Porém (li2 is li) resultará em False.
li2 is li  # => False

# Apague elementos específicos da lista com "del"
del li[2]  # li agora é [1, 2, 3]

# Você pode somar listas
# Observação: valores em li e other_li não são modificados.
li + other_li  # => [1, 2, 3, 4, 5, 6]

# Concatene listas com "extend()"
li.extend(other_li)  # Agora li é [1, 2, 3, 4, 5, 6]

# Verifique se algo existe na lista com "in"
1 in li  # => True

# O tamanho da lista pode ser obtido com "len()"
len(li)  # => 6

# Tuplas são como listas, mas são imutáveis.
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # Gera um TypeError

# Observe que uma tupla de tamanho 1 precisa ter uma vírgula depois do
# último elemento mas tuplas de outros tamanhos, mesmo vazias, não precisam.
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
a, b = b, a  # a é 2, b é 1

# Dicionários armazenam mapeamento de chaves a valores
empty_dict = {}

# Este é um dicionário preenchido
filled_dict = {"um": 1, "dois": 2, "três": 3}

# Observe que chaves para dicionários devem ser tipos imutáveis. Isto é para
# assegurar que a chave pode ser convertida para uma hash de valor constante.
# Isso aumenta a performance das buscas.
# Tipos imutáveis incluem inteiros, floats, strings e tuplas.
invalid_dict = {[1,2,3]: "123"}  # => Gera um TypeError: unhashable type: 'list'
valid_dict = {(1,2,3): [1,2,3]}  # Já os valores, podem ser de qualquer tipo.

# Acesse valores com []
filled_dict["um"]  # => 1

# Você pode acessar todas as chaves contidas no dicionário com "keys()".
# É necessário encapsular a chamada com um list() para transformá-las 
# em uma lista. Falaremos sobre isso mais adiante.
# Observe que a ordem de uma chave de dicionário não é garantida. Por isso,
# os resultados que você vai ver quando executar esse código podem não estar
# na mesma ordem que apresentamos aqui.
list(filled_dict.keys())  # => ["três", "dois", "um"]

# Já os valores no dicionário podem ser obtidos por meio de "values()". Novamente,
# é necessário encapsular ele com list() para não termos um iterável, e sim os
# valores. Observe que, como foi dito acima, a ordem dos elementos não é
# garantida.
list(filled_dict.values())  # => [3, 2, 1]

# Verifique a existência de chaves em um dicionário com "in"
"um" in filled_dict  # => True
1 in filled_dict     # => False

# Acessar uma chave inexistente gera um KeyError
filled_dict["quatro"]  # KeyError

# Use o método "get()" para evitar um KeyError
filled_dict.get("um")      # => 1
filled_dict.get("quatro")  # => None

# O método get permite que você defina uma resposta-padrão para quando não existir
# a chave buscada
filled_dict.get("um", 4)      # => 1
filled_dict.get("quatro", 4)  # => 4

# "setdefault()" faz uma inserção no dicionário apenas se a chave passada não existir
filled_dict.setdefault("cinco", 5)  # filled_dict["cinco"] tem valor 5
filled_dict.setdefault("cinco", 6)  # filled_dict["cinco"] continua 5

# Inserindo em um dicionário
filled_dict.update({"quatro":4})  # => {"um": 1, "dois": 2, "três": 3, "quatro": 4}
filled_dict["quatro"] = 4         # outra forma de inserir em um dicionário

# Remova chaves de um dicionário com del
del filled_dict["um"]  # Remove a chave "um" de filled_dict

# Python também apresenta um tipo de dados para conjuntos
empty_set = set()

# Você pode inicializar um set com alguns valores.
# Note como ele é parecido com os dicionários
some_set = {1, 1, 2, 2, 3, 4}  # some_set vale {1, 2, 3, 4}

# Da mesma forma que chaves em um dicionário, elementos de um set devem ser
# imutáveis.
invalid_set = {[1], 1}  # => Gera um TypeError: unhashable type: 'list'
valid_set = {(1,), 1}

# Você pode criar novas referências para um set já existente
filled_set = some_set

# Para adicionar mais um item no set
filled_set.add(5)  # filled_set agora é {1, 2, 3, 4, 5}

# A interseção de conjuntos pode ser feita com &
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# E união de conjuntos com |
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# A diferença entre conjuntos é feita com -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Para saber se um set possui algum elemento, use in
2 in filled_set   # => True
10 in filled_set  # => False


####################################################
## 3. Controle de fluxo e iteráveis
####################################################

# Iniciemos um variável
some_var = 5

# Condicionais são feitas de maneira semelhante a outras linguagens.
# Cuidado: python usa indentação para marcar quais expressões estão em um bloco!
# O if abaixo imprime "some_var é menor que10"
if some_var > 10:
    print("some_var é absolutamente maior que 10.")
elif some_var < 10:    # As cláusulas elif são opcionais.
    print("some_var é menor que 10.")
else:                  # Também não é necessário ter else.
    print("some_var é, de fato, 10.")


"""
Laços for iteram sobre listas
O for abaixo imprime:
    cachorro é um mamífero
    gato é um mamífero
    rato é um mamífero
"""
for animal in ["cachorro", "gato", "rato"]:
    # Você também pode usar format() para interpolar strings
    print("{} é um mamífero".format(animal))

"""
"range(número)" retorna um iterável de números no intervalo
de zero até o número escolhido, mas sem incluí-lo.
imprime:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(menor, maior)" gera um iterável de números começando pelo
menor até o maior, mas sem incluí-lo.
imprime:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(menor, maior, passo)" retorna um iterável de números do menor
até o maior, pulando de passo em passo. Se o passo não for indicado,
o valor padrão é um.
imprime:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)

"""
Laços while executam até que a condição não seja mais verdadeira.
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


# Para tratar eventuais exceções lançadas pelo Python, é
# necessário usar blocos try/except. 
try:
    # "raise" lança uma nova exceção do tipo passado
    raise IndexError("Isto é um erro de índice")
except IndexError as e:         # Dentro de except você pode executar algo para tratar as exceções
    pass                        # "pass" está aqui como placeholder, a fim de exemplificar o uso de try
except (TypeError, NameError):  # Vários tipos de exceções podem ser tratadas, se necessário.
    pass                 
else:                           # Cláusula opcional para o bloco try/except. Deve estar após todos os blocos de exceção.
    print("Tudo certo!")        # Executa apenas se o código em try não gera exceção
finally:                        # Sempre é executado, haja ou não exceção
    print("Aqui nós podemos fazer algum código de limpeza.")

# Em vez de usar try/finally para limpeza, você pode usar a cláusula with.
# Nesse exemplo f é uma variável local contendo o resultado de open("myfile.txt").
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Python provê uma abstração fundamental chamada Iterável.
# Um iterável é simplesmente um objeto que pode ser tratado como
# uma sequência, que é possível ser percorrido com for ou outras
# estruturas de loop.
# O objeto-retorno da função range é um iterável, por exemplo.

filled_dict = {"um": 1, "dois": 2, "três": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['um', 'dois', 'três']). Esse é um objeto que implementa nossa interface iterável.

"""
Nós podemos percorrê-la usando for, por exemplo:
imprime:
    um
    dois
    três
"""
for i in our_iterable:
    print(i)

# Mas não podemos acessar os elementos pelo seu índice.
our_iterable[1]  # Gera TypeError: 'dict_keys' object is not subscriptable

# Um iterável é um objeto que sabe como criar um iterador.
our_iterator = iter(our_iterable)

# Nosso iterador é um objeto mantém seu estado enquanto nós o percorremos.
# Com isso nós conseguimos saber qual é o próximo item usando "next()".
next(our_iterator)  # => "um"

# Podemos usar "next()" para percorrer todos os items no iterador
next(our_iterator)  # => "dois"
next(our_iterator)  # => "três"

# Após o iterador retornar todos os seus dados, ele gera a exceção StopIterator
next(our_iterator)  # Gera StopIteration

# Você pode listar todos os elementos de um iterador aplicando "list()" nele.
list(filled_dict.keys())  # => Retorna ["um", "dois", "três"]


####################################################
## 4. Funções
####################################################

# Use "def" para criar novas funções.
def add(x, y):
    print("x é {} e y é {}".format(x, y))
    return x + y  # Retorne valores com a cláusula return

# Funções são chamadas como parênteses, como na maioria das linguagens.
add(5, 6)  # => imprime "x é 5 e y é 6" e retorna 11

# Outro meio de chamar funções é com argumentos nomeados.
# Note que argumentos nomeados podem aparecer em qualquer ordem.
add(y=6, x=5)  

# Você pode definir funções que aceitam um número variável de argumentos
# posicionais
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# Também é possível definir funções que aceitam um número variável de
# argumentos nomeados
def keyword_args(**kwargs):
    return kwargs

# Vamos chamá-lo para ver o que acontece
keyword_args(peh="grande", lago="ness")  # => {"peh": "grande", "lago": "ness"}

# Você pode fazer ambos simultaneamente, se você quiser
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)

"""
A chamada abaixo imprime:
(1, 2)
{"a": 3, "b": 4}
"""
all_the_args(1, 2, a=3, b=4)

# Ao chamar funções, você também pode fazer o oposto de args/kwargs!
# Usando * você expande tuplas; e ** expande dicionários.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}

all_the_args(*args)            # equivalente a foo(1, 2, 3, 4)
all_the_args(**kwargs)         # equivalente a foo(a=3, b=4)
all_the_args(*args, **kwargs)  # equivalente a foo(1, 2, 3, 4, a=3, b=4)

# É possível retornanr múltiplos valores em Pythob
# Nesse cenário o interpretador cria implicitamente uma tupla com os
# valores retornados.
def swap(x, y):
    return y, x  # Você também pode envolver os valores em parênteses, se quiser

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1

# A expressão acima equivale a esta aqui abaixo. Novamente os parênteses são opcionais.
(x, y) = swap(x,y)  

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

# Em python, funções também são valores de primeira-ordem.
# Isso significa que você pode retornar funções ou atribuí-las
# a variáveis, como se fosse valores quaisquer.
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# Também existem as funções anônimas
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# Python conta com algumas funções internas de primeira-ordem,
# tais como map e filter.

# Aplica a função add_10 a todos os itens da lista [1, 2, 3]
res_1 = map(add_10, [1, 2, 3])  # retorna um iterador que pode ser percorrido normalmente

next(res_1)  # => 11
next(res_1)  # => 12
next(res_1)  # => 13
next(res_1)  # => StopIteration

# Você também pode aplicar funções a vários argumentos
res_2 = map(max, [1, 2, 3], [4, 2, 1])
list(res_2)  # => [4, 2, 3]

# Também é possível passar um lambda, caso você não queira
# definir uma função comum só para isso
res_3 = filter(lambda x: x > 5, [3, 4, 5, 6, 7])
list(res_3)  # => [6, 7]

# Também é possível usar a sintaxe de list comprehensions para
# operar sobre listas.
# Apesar de parecer confusa, essa sintaxe oferece uma maneira
# concisa de armazenar o resultado de um for dentro de uma lista.
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]


####################################################
## 5. Orientação a objetos
####################################################

# Nós usamos o operador "class" para definir uma classe
class Human:

    # Você pode criar um atributo de classe simplesmente atribuindo ele dentro
    # da classe. Note é compartilhado por todas as instâncias dessa classe.
    species = "H. sapiens"

    # Construtor básico. Esse método é chamado sempre que esta classe é
    # instanciada. Observe que os dois sublinhados no início e no final do
    # nome do método. Eles representam objetos ou atributos que são usados
    # pelo Python mas que são definidos em um namespace controlado pelo usuário.
    # Métodos (ou objetos ou atributos) como: __init__, __str__, __repr__, etc
    # são chamados métodos mágicos (ou algumas vezes chamados métodos dunder -
    # "double underscore").
    # Você não deve usar nomes assim por vontade própria em métodos seus.
    def __init__(self, name):
        # @ define um atributo de instância. Note que não é preciso definir
        # esses atributos. Basta atribuir a eles algum valor e Python os criará.
        self.name = name

    # Um método de instância. Todos os métodos tem "self" como primeiro
    # argumento. self aqui representa uma referência a uma instância da classe.
    def say(self, msg):
        return "{name}: {message}".format(name=self.name, message=msg)

    # Um método de classe é compartilhado por todas as instâncias da classe.
    # Python sempre passa uma referência à própria classe como primeiro
    # argumento.
    @classmethod
    def get_species(cls):
        return cls.species

    # Um método estático é chamado sem uma referência a classe ou instância.
    @staticmethod
    def grunt():
        return "*grunt*"


# Instanciando uma classe
i = Human(name="Ian")
print(i.say("oi"))   # imprime "Ian: oi"

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
print(math.sqrt(16))  # => 4.0

# Você pode importar apenas funções específicas de um módulo
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0
print(log(10))     # => lança NameError, pois log não foi importada

from math import log
print(log(10))    # => 2.302585092994046

# Você pode importar todas as funções de um módulo para o namespace atual
# Atenção: essa não é uma boa prática e não deve ser feita.
from math import *

# Você pode criar apelidos para o nome dos módulos.
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Módulos python são apenas arquivos python comuns. Você
# pode escrever os seus e importá-los. O nome do
# módulo é o mesmo nome do arquivo.

# Você pode procurar que atributos e funções definem um módulo.
import math
dir(math)


####################################################
## 7. Avançado
####################################################

# Você pode usar geradores para escrever código de avaliação "preguiçosa".
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# Um gerador cria valores conforme necessário.
# Em vez de gerar e retornar todos os valores de uma só vez ele cria um
# a cada interação. Isto significa que valores maiores que 15 não serão
# processados em double_numbers.
# Nós usamos um sublinhado ao final do nome das variáveis quando queremos usar
# um nome que normalmente colide com uma palavra reservada do python.
range_ = range(1, 900000000)

# Multiplica por 2 todos os números até encontrar um resultado >= 30
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break

# Decoradores estendem ou ampliam o funcionamento de outras estruturas
# já existentes. Neste exemplo beg encapsula say.
# beg irá chamar say. Se say_please for True então ele mudará a mensagem
# retornada.
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

## Quer saber mais?

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
