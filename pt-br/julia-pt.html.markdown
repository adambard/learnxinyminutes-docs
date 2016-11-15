---
language: Julia
filename: learnjulia-pt.jl
contributors:
    - ["Leah Hanson", "http://leahhanson.us"]
translators:
    - ["Davidson Mizael", "https://github.com/davidsonmizael"]
lang: pt-br
---

Julia é uma linguagem homoiconic funcional focada na computação tecnica. Ao mesmo tempo que ela tem todo o poder dos homoiconic macros, funções de primeira classe, e controle de baixo nivel, Julia é tão facil para aprender e usar quanto Python.

Este tutorial é baseado no Julia 0.3.

```ruby
# Linhas únicas de comentários começam com o simbolo hash(jogo da velha).
#= Comentários de multiplas linhas podem ser escritos
   colocando '#=' antes do texto e '=#'
   após o texto. Eles também podem ser agrupados
=#

####################################################
## 1. Tipos primitivos e operadores
####################################################

# Tudo em Julia é uma expressão.

# Há muitos tipos básicos de numeros.
3 # => 3 (Int64)
3.2 # => 3.2 (Float64)
2 + 1im # => 2 + 1im (Complex{Int64})
2//3 # => 2//3 (Rational{Int64})

# Todos os operadores inseguros normais estão disponiveis.
1 + 1 # => 2
8 - 1 # => 7
10 * 2 # => 20
35 / 5 # => 7.0
5 / 2 # => 2.5 # dividir um Int por um Int resulta em um float
div(5, 2) # => 2 # para um restultado truncado, use div
5 \ 35 # => 7.0
2 ^ 2 # => 4 # elevado,não o opeardor binário xor
12 % 10 # => 2

# Impõe a priodidade nos parenteses
(1 + 3) * 2 # => 8

# Operadores binarios
~2 # => -3   # not
3 & 5 # => 1 # and
2 | 4 # => 6 # or
2 $ 4 # => 6 # xor
2 >>> 1 # => 1 # deslocamento lógico de bits a direita
2 >> 1  # => 1 # deslocamento aritmético de bits a direita
2 << 1  # => 4 # deslocamento lógico/aritmético de bits a esquerda

# Você pode usar a função bits para ver a representação binária de um numero.
bits(12345)
# => "0000000000000000000000000000000000000000000000000011000000111001"
bits(12345.0)
# => "0100000011001000000111001000000000000000000000000000000000000000"

# Valores booleanos são primitivos.
true
false

# Operadores booleanos
!true # => false
!false # => true
1 == 1 # => true
2 == 1 # => false
1 != 1 # => false
2 != 1 # => true
1 < 10 # => true
1 > 10 # => false
2 <= 2 # => true
2 >= 2 # => true
# Comparações podem ser encadeadas
1 < 2 < 3 # => true
2 < 3 < 2 # => false

# Strings são criadas com  "
"Isso é uma String."

# Caracteres literais são escritos com '
'a'

# Uma string pode ser indexada como um vetor de caracteres
"Isso é uma string"[1] # => 'I' # Julia começa a indexar a partir do 1
# Porém isso não funcionará direito com strings em UTF8,
# portanto é recomendado usar iterações sobre uma string (map, loops com for, etc). 

# $ pode ser usado para interpolação de string:
"2 + 2 = $(2 + 2)" # => "2 + 2 = 4"
# Você pode usar qualquer expressão Julia dentro dos parenteses.

# Outro jeito de formatar strings é com um macro no printf.
@printf "%d é menor que %f" 4.5 5.3 # 5 é menor que 5.300000

# Escrever na tela é fácil
println("Eu sou Julia. Prazer em conhece-lo!")

####################################################
## 2. Variáveis e coleções
####################################################

#Você não declara variáveis antes de atribui-lás.
some_var = 5 # => 5
some_var # => 5

# Acessando a variável anterior não iniciada é um erro
try
    some_other_var # => ERROR: some_other_var não definida 
catch e
    println(e)
end

# Nomes de variáveis começam com uma letra.
# Depois disso, você pode usar letras, digitos, underscores e pontos de exclamação.
SomeOtherVar123! = 6 # => 6

# Você também pode usar caractéres unicode
☃ = 8 # => 8
# Estes são especialmente reservados para notações matemáticas.
2 * π # => 6.283185307179586

# Uma nota na convenção de nomes em Julia:
#
# * A separação de palavras pode ser feita por underscores ('_'), mas o uso
#   de underscore é desencorajado a menos que o nome da variável seja dificil
#   de ler.
#
# * Os nomes de tipos começam com letra maiúscula e a separação de letras é 
#   feita a partir de CamelCase no lugar de underscores.
#
# * Nomes de funções e macros são em minúsculo, sem underscore.
#
# * Funções que modificam a própria entrada tem nomes que terminam em !. Estas
#   funções são chamadas as vezes de funções de mutação ou função in-place.

# Vetores armazenam uma sequencia de valores indexados por integer de 1 a n:
a = Int64[] # => 0-element Int64 Array

# 1-Vetores dimensionais literais podem ter seus valores separados por virgula.
b = [4, 5, 6] # => 3-element Int64 Array: [4, 5, 6]
b[1] # => 4
b[end] # => 6

# 2-Vetores dimensionais usam espaço para separar valores e ponto e virgula para linhas.
matrix = [1 2; 3 4] # => 2x2 Int64 Array: [1 2; 3 4]

# Adiciona-se coisas ao final de uma lista com push! e append!
push!(a,1)     # => [1]
push!(a,2)     # => [1,2]
push!(a,4)     # => [1,2,4]
push!(a,3)     # => [1,2,4,3]
append!(a,b) # => [1,2,4,3,4,5,6]

# Remove-se do final com pop!
pop!(b)        # => 6 e 'b' agora é [4,5]

# Vamos coloca-lo de novo
push!(b,6)   # 'b' agora é [4,5,6] de novo.

a[1] # => 1 # lembre-se que Julia indexa a partir de 1, não 0.

# end é um atalho para a ultima posição. Pode ser usada em qualquer
# expressão indexada.
a[end] # => 6

# nós também temos shift e unshift
shift!(a) # => 1 e 'a' agora é [2,4,3,4,5,6]
unshift!(a,7) # => [7,2,4,3,4,5,6]

# Funções que terminam com ponto de exclamação indicam que elas modificam
# seus argumentos.
arr = [5,4,6] # => 3-element Int64 Array: [5,4,6]
sort(arr) # => [4,5,6]; 'arr' continua [5,4,6]
sort!(arr) # => [4,5,6]; 'arr' agora é [4,5,6]

# Olhar além dos limites é um BoundsError
try
    a[0] # => ERROR: BoundsError() in getindex at array.jl:270
    a[end+1] # => ERROR: BoundsError() in getindex at array.jl:270
catch e
    println(e)
end

# Erros listam a linha e o nome do arquivo que ele está, mesmo se for uma 
# biblioteca padrão. Se você construiu Julia pelo source, você pode olhar na 
# pasta base dentro da pasta do Julia para encontrar esses arquivos.

# Você pode inicializar vetores com limites
a = [1:5;] # => 5-element Int64 Array: [1,2,3,4,5]

# Você pode ver até um limite com a sintaxe separada
a[1:3] # => [1, 2, 3]
a[2:end] # => [2, 3, 4, 5]

# Remova elementos de um array pelo index com splice!
arr = [3,4,5]
splice!(arr,2) # => 4 ; arr is now [3,5]

# Concatene listas com append!
b = [1,2,3]
append!(a,b) # 'a' agora é [1, 2, 3, 4, 5, 1, 2, 3]

# Cheque se um valor existe me uma lista com in
in(1, a) # => true

# Veja o tamanho com lenght
length(a) # => 8

# Tuples não podem ser mudados.
tup = (1, 2, 3) # => (1,2,3) # um tuple (Int64,Int64,Int64).
tup[1] # => 1
try:
    tup[1] = 3 # => ERROR: não há metodo setindex!((Int64,Int64,Int64),Int64,Int64)
catch e
    println(e)
end

# Muitas litas de funções também trabalham com tuples
length(tup) # => 3
tup[1:2] # => (1,2)
in(2, tup) # => true

#Você pode desempacotar tuples para variáveis.
a, b, c = (1, 2, 3) # => (1,2,3)  # 'a' agora é 1, 'b' agora é 2 e 'c' agora é 3

# Tuplas são criados mesmo se você deixar fora dos parenteses
d, e, f = 4, 5, 6 # => (4,5,6)

# Uma tupla de um elemento é diferente do valor que ele contém
(1,) == 1 # => false
(1) == 1 # => true

# Olhe como é facil pra trocar dois valores
e, d = d, e  # => (5,4) # 'd' agora é 5 e 'e' agora é 4

# Dicionários armazenam mapeamentos
empty_dict = Dict() # => Dict{Any,Any}()

# Você pode criar um dicionário usando um literal
filled_dict = ["one"=> 1, "two"=> 2, "three"=> 3]
# => Dict{ASCIIString,Int64}

# Veja os valores com []
filled_dict["one"] # => 1

# Pegue todas as chaves
keys(filled_dict)
# => KeyIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Nota - as chaves dos dicionários não são ordenadas nem estão na ordem que você as inseriu.

# Pegue todos os valores
values(filled_dict)
# => ValueIterator{Dict{ASCIIString,Int64}}(["three"=>3,"one"=>1,"two"=>2])
# Nota - A mesma coisa que na nota acima sobre a ordenação das chaves.

# Cheque pela existencia de chaves em um dicionário com in e haskey
in(("one", 1), filled_dict) # => true
in(("two", 3), filled_dict) # => false
haskey(filled_dict, "one") # => true
haskey(filled_dict, 1) # => false

# Procurar por uma chave não existente irá gerar um erro
try
    filled_dict["four"] # => ERROR: key not found: four in getindex at dict.jl:489
catch e
    println(e)
end

# Use o método get para escapar desse erro passando um valor padrão
# get(dictionary,key,default_value)
get(filled_dict,"one",4) # => 1
get(filled_dict,"four",4) # => 4

# Use sets para representar coleções de valores unicos e não ordenados
empty_set = Set() # => Set{Any}()
# Inicialize um set com valores
filled_set = Set(1,2,2,3,4) # => Set{Int64}(1,2,3,4)

# Adicione mais valores para um set
push!(filled_set,5) # => Set{Int64}(5,4,2,3,1)

# Cheque se um valor está no set
in(2, filled_set) # => true
in(10, filled_set) # => false

# Não há funções para interseção de set, união e diferença.
other_set = Set(3, 4, 5, 6) # => Set{Int64}(6,4,5,3)
intersect(filled_set, other_set) # => Set{Int64}(3,4,5)
union(filled_set, other_set) # => Set{Int64}(1,2,3,4,5,6)
setdiff(Set(1,2,3,4),Set(2,3,5)) # => Set{Int64}(1,4)

####################################################
## 3. Controle de fluxo
####################################################

# Vamos fazer uma variável
some_var = 5

# Aqui está um if. Identação nao é importante em Julia.
if some_var > 10
    println("some_var é totalmente maior que 10.")
elseif some_var < 10    # Essa clausula elseif é opcional.
    println("some_var é menor que 10.")
else                    # A clausula else é opcional também.
    println("some_var é literalmente 10.")
end
# => exibe "some_var é menor que 10"

# Loops for repetem sobre variaveis iteráveis.
# Tipos iterativos incluem Range, Array, set Dict e String.
for animal=["dog", "cat", "mouse"]
    println("$animal is a mammal")
	# Você pode interpolar variáveis usando $ ou expressões em strings
end
# exibe:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# Você pode usar 'in' no lugar de '='.
for animal in ["dog", "cat", "mouse"]
    println("$animal is a mammal")
end
# exibe:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

for a in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$(a[1]) is a $(a[2])")
end
# exibe:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

for (k,v) in ["dog"=>"mammal","cat"=>"mammal","mouse"=>"mammal"]
    println("$k is a $v")
end
# exibe:
#    dog is a mammal
#    cat is a mammal
#    mouse is a mammal

# Loops while circulam enquanto a condição é true
x = 0
while x < 4
    println(x)
    x += 1  # Abreveação para x = x + 1
end
# exibe:
#   0
#   1
#   2
#   3

# Trate exceções com um bloco try/catch
try
   error("help")
catch e
   println("caught it $e")
end
# => caught it ErrorException("help")


####################################################
## 4. Funções
####################################################

# A palavra chave 'function' cria novas funções
#function name(arglist)
#  corpo...
#end
function add(x, y)
    println("x is $x and y is $y")

	# Funções retornam o valor da sua ultima declaração
t    x + y
end

add(5, 6) # => 11 after printing out "x is 5 and y is 6"

# Você pode definir funções que tomam um numero incerto de
# argumentos 
function varargs(args...)
    return args
    # use a palavra chave return para retornar um valor em qualquer parte da função
end
# => varargs (generic function with 1 method)

varargs(1,2,3) # => (1,2,3)

# O ... é chamado de splat.
# Nós apenas o usamos na definição de uma função.
# Também pode ser usado na chamada de uma função,
# onde ela vai abrir um Array ou o conteúdo de um Tuple na lista de argumentos.
Set([1,2,3])    # => Set{Array{Int64,1}}([1,2,3]) # produz um Set de Arrays
Set([1,2,3]...) # => Set{Int64}(1,2,3) # isso é equivalente a Set(1,2,3)

x = (1,2,3)     # => (1,2,3)
Set(x)          # => Set{(Int64,Int64,Int64)}((1,2,3)) # um Set de Tuples
Set(x...)       # => Set{Int64}(2,3,1)

# Você pode definir funções com argumentos posicionais opcionais.
function defaults(a,b,x=5,y=6)
    return "$a $b and $x $y"
end

defaults('h','g') # => "h g and 5 6"
defaults('h','g','j') # => "h g and j 6"
defaults('h','g','j','k') # => "h g and j k"
try
    defaults('h') # => ERROR: no method defaults(Char,)
    defaults() # => ERROR: no methods defaults()
catch e
    println(e)
end

# Você pode definir funções que tomam argumentos como palavras chaves
function keyword_args(;k1=4,name2="hello") # note the ;
    return ["k1"=>k1,"name2"=>name2]
end

keyword_args(name2="ness") # => ["name2"=>"ness","k1"=>4]
keyword_args(k1="mine") # => ["k1"=>"mine","name2"=>"hello"]
keyword_args() # => ["name2"=>"hello","k1"=>4]

# Você pode combinar todos os tipos de argumentos em uma só função
function all_the_args(normal_arg, optional_positional_arg=2; keyword_arg="foo")
    println("normal arg: $normal_arg")
    println("optional arg: $optional_positional_arg")
    println("keyword arg: $keyword_arg")
end

all_the_args(1, 3, keyword_arg=4)
# exibe:
#   normal arg: 1
#   optional arg: 3
#   keyword arg: 4

# Julia tem funções de primeira classe
function create_adder(x)
    adder = function (y)
        return x + y
    end
    return adder
end

# Isso é "sintexe furiosa de lambda" pra criar funções anônimas.
(x -> x > 2)(3) # => true

#Esta função é identica a implementação da create_adder acima.
function create_adder(x)
    y -> x + y
end

# Você também pode nomear funções internas, se você quiser
function create_adder(x)
    function adder(y)
        x + y
    end
    adder
end

add_10 = create_adder(10)
add_10(3) # => 13


# Há 
# There are built-in higher order functions
map(add_10, [1,2,3]) # => [11, 12, 13]
filter(x -> x > 5, [3, 4, 5, 6, 7]) # => [6, 7]

# Nós podemos usar listas de compreensão para melhores mapeamentos
[add_10(i) for i=[1, 2, 3]] # => [11, 12, 13]
[add_10(i) for i in [1, 2, 3]] # => [11, 12, 13]

####################################################
## 5. Tipos
####################################################

#Julia tem um sistema de tipos.
# Todo valor tem um tipo. Variaveis não tem tipos próprios.
# Você pode usar a função 'typeof' para pegar o valor.
typeof(5) # => Int64

# Tipos são valores de primeira classe.
typeof(Int64) # => DataType
typeof(DataType) # => DataType
# DataType é o tipo que representa tipos, incluindo ele mesmo.

# Tipos são usados para documentação, optimização e envio
# Eles não são estaticamente checados.

# Usuários podem definir tipos
# Eles são como records ou structs em outras linguagens.
# Novos tipos são definidos usando a palavra chave 'type'

# type Name
#   field::OptionalType
#   ...
# end
type Tiger
  taillength::Float64
  coatcolor # não incluindo uma notação type é o mesmo que '::Any'
end

# Os argumentos padrões de um construtor são as propriedades
# do tipo na ordem que eles são listados na definição.
tigger = Tiger(3.5,"orange") # => Tiger(3.5,"orange")

# O tipo double como construtor de função para valores desse tipo
# The type doubles as the constructor function for values of that type
sherekhan = typeof(tigger)(5.6,"fire") # => Tiger(5.6,"fire")

# Esses tipos no estilo struct são chamados tipos concretos
# Eles podem ser instanciados, mas não podem ter subtipos.
# O outro tipo de tipos são os tipos abstratos.

# abstract Name
abstract Cat # apenas um nome e um ponto na hierarquia de tipo

# Tipos abstratos podem ser instanciados, mas não podem ter subtipos.
# Por exemplo, Number é um tipo abstrato
subtypes(Number) # => 6-element Array{Any,1}:
                 #     Complex{Float16}
                 #     Complex{Float32}
                 #     Complex{Float64}
                 #     Complex{T<:Real}
                 #     ImaginaryUnit
                 #     Real
subtypes(Cat) # => 0-element Array{Any,1}

# Todo tipo tem um super tipo; use a função 'super' para pegá-lo.
typeof(5) # => Int64
super(Int64) # => Signed
super(Signed) # => Real
super(Real) # => Number
super(Number) # => Any
super(super(Signed)) # => Number
super(Any) # => Any
# Todos esss tipos, exceto o Int64, são abstratos.

# <: é o operador de subtipagem
type Lion <: Cat # Lion é um subtipo de Cat
  mane_color
  roar::String
end

# Você pode definir mais construtores para seu tipo
# É só definir uma função com o mesmo nome do tipo
# e chamar um construtor existente para pegar o valor do tipo correto
Lion(roar::String) = Lion("green",roar)
# Isso é um construtor externo porque ele está fora da definição do tipo

type Panther <: Cat # Panther também é um subtipo de Cat
  eye_color
  Panther() = new("green")
# Panthers terão apenas esse construtor, e não construtor padrão.
end
# Usando construtores internos, como Panther faz, lhe da o controle
# sobre como os valores dos tipos são criados.
# Quando possivel, você deve usar construtores externos mais do que internos.

####################################################
## 6. Multiple-Dispatch
####################################################


# Em Julia todas as funções nomeadas são funções genericas
# Isso significa que elas são construidas de muitos métodos pequenos
# Cada construtor para Lion é um metodo da função genérica Lion.Lion.

# Para um exemplo sem construtor, vamos fazer a função meow

# Definição para Lion, Panther e Tiger
function meow(animal::Lion)
  animal.roar #propriedades do tipo de acesso usando a notação ponto '.'
end

function meow(animal::Panther)
  "grrr"
end

function meow(animal::Tiger)
  "rawwwr"
end

# Testando a função meow
meow(tigger) # => "rawwr"
meow(Lion("brown","ROAAR")) # => "ROAAR"
meow(Panther()) # => "grrr"

# Revendo o tipo local de hierarchy
issubtype(Tiger,Cat) # => false
issubtype(Lion,Cat) # => true
issubtype(Panther,Cat) # => true

# Definindo uma função que recebe Cats
function pet_cat(cat::Cat)
  println("The cat says $(meow(cat))")
end

pet_cat(Lion("42")) # => exibe "The cat says 42"
try
    pet_cat(tigger) # => ERROR: no method pet_cat(Tiger,)
catch e
    println(e)
end

# Em linguagens orientadas a objeto, envio unico é comúm
# isso significa que o método é selecionado baseado no tipo do seu primeiro argumento
# Em Julia todos os tipos de argumentos contribuem na seleção do melhor método


# Vamos definir uma função com mais argumentos, então poderemos ver a diferença
function fight(t::Tiger,c::Cat)
  println("The $(t.coatcolor) tiger wins!")
end
# => fight (generic function with 1 method)

fight(tigger,Panther()) # => exibe The orange tiger wins!
fight(tigger,Lion("ROAR")) # => exibir The orange tiger wins!

# Vamos mudar o comportamento quando o gato é especificamente um leão
fight(t::Tiger,l::Lion) = println("The $(l.mane_color)-maned lion wins!")
# => fight (generic function with 2 methods)

fight(tigger,Panther()) # => exobe The orange tiger wins!
fight(tigger,Lion("ROAR")) # => exobe The green-maned lion wins!

# Nós não precisamos de um tigre para brigar
fight(l::Lion,c::Cat) = println("The victorious cat says $(meow(c))")
# => fight (generic function with 3 methods)

fight(Lion("balooga!"),Panther()) # => exibe The victorious cat says grrr
try
  fight(Panther(),Lion("RAWR")) # => ERROR: no method fight(Panther,Lion)
catch
end

# Aliás, vamos deixar o gato ir primeiro
fight(c::Cat,l::Lion) = println("The cat beats the Lion")
# => Warning: New definition
#    fight(Cat,Lion) at none:1
# is ambiguous with
#    fight(Lion,Cat) at none:2.
# Make sure
#    fight(Lion,Lion)
# is defined first.
#fight (generic function with 4 methods)

# Este aviso é porque não está claro qual método fight será chamado em:
fight(Lion("RAR"),Lion("brown","rarrr")) # => exibe The victorious cat says rarrr
# O resultado pode ser diferente em outras versões de Julia

fight(l::Lion,l2::Lion) = println("The lions come to a tie")
fight(Lion("RAR"),Lion("brown","rarrr")) # => exibe The lions come to a tie


# Embaixo dos panos
# Você pode olhar o llvm e o código assembly gerado.

square_area(l) = l * l      # square_area (generic function with 1 method)

square_area(5) #25

# O que acontece quando alimentamos square_area com um inteiro?
# What happens when we feed square_area an integer?
code_native(square_area, (Int32,))  
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1              # Prólogo
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       movsxd  RAX, EDI        # Busca l na memoria?
    #       imul    RAX, RAX        # Faz o quadrado de l e armazena o resultado em RAX
    #       pop RBP                 # Restaura o ponteiro de base antigo
    #       ret                     # O resultado continua em RAX

code_native(square_area, (Float32,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       vmulss  XMM0, XMM0, XMM0  # Múltiplicação escalar unica de precisão (AVX)
    #       pop RBP
    #       ret

code_native(square_area, (Float64,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       vmulsd  XMM0, XMM0, XMM0 # Duplicação ecalar de precisão multipla(AVX)
    #       pop RBP
    #       ret
    #   
# Note que Julia usará instruções de ponto flutuante se quaser um dos
# argumentos forem float
# Vamos calcular a área de um circulo
circle_area(r) = pi * r * r     # circle_area (generic function with 1 method)
circle_area(5)                  # 78.53981633974483

code_native(circle_area, (Int32,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #   Source line: 1
    #       vcvtsi2sd   XMM0, XMM0, EDI          # Carrega inteiro (r) da memória
    #       movabs  RAX, 4593140240              # Carrega pi
    #       vmulsd  XMM1, XMM0, QWORD PTR [RAX]  # pi * r
    #       vmulsd  XMM0, XMM0, XMM1             # (pi * r) * r
    #       pop RBP
    #       ret
    #

code_native(circle_area, (Float64,))
    #       .section    __TEXT,__text,regular,pure_instructions
    #   Filename: none
    #   Source line: 1
    #       push    RBP
    #       mov RBP, RSP
    #       movabs  RAX, 4593140496
    #   Source line: 1
    #       vmulsd  XMM1, XMM0, QWORD PTR [RAX]
    #       vmulsd  XMM0, XMM1, XMM0
    #       pop RBP
    #       ret
    #   
```

## Extras

Você pode ver mais um monte de detalhes no [manual de Julia] (http://docs.julialang.org/en/latest/manual/)
O melhor lugar pra pedir ajuda em Julia é a (muito amigável) [mailing list](https://groups.google.com/forum/#!forum/julia-users).
