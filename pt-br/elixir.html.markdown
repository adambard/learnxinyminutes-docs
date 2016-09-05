---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
translators:
    - ["Rodrigo Muniz", "http://github.com/muniz95"]
lang: pt-br
filename: learnelixir-pt.ex
---

Elixir é uma linguagem funcional moderna construída no topo da Erlang VM.
É totalmente compatível com Erlang, porém conta com uma sintaxe mais padronizada
e muitos outros recursos.

```elixir

# Comentários de linha única começam com um símbolo de número.

# Não há comentários de múltiplas linhas,
# mas você pode empilhar os comentários.

# Para usar o shell do elixir use o comando `iex`.
# Compile seus módulos com o comando `elixirc`.

# Ambos devem estar em seu path se você instalou o Elixir corretamente.

## ---------------------------
## -- Tipos Básicos
## ---------------------------

# Há números
3    # integer
0x1F # integer
3.0  # float

# Atoms, que são literais, uma constante com nome. Elas começam com `:`.
:hello # atom

# Tuplas que são guardadas contiguamente em memória.
{1,2,3} # tupla

# Podemos acessar um elemento de uma tupla om a função `elem`:
elem({1, 2, 3}, 0) #=> 1

# Listas que são implementadas como listas ligadas.
[1,2,3] # lista

# Podemos acessar a primeira posição (head) e o resto (tail) de uma lista como a seguir:
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# Em elixir, bem como em Erlang, o sinal `=` denota pattern match,
# e não uma atribuição.
#
# Isto significa que o que estiver à esquerda (pattern) é comparado com o que
# estiver à direita.
#
# É assim que o exemplo acima de acesso à head e tail de uma lista funciona.

# Um pattern match retornará erro quando os lados não conferem, como neste exemplo
# onde as tuplas tem diferentes tamanhos.
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# Também há binários
<<1,2,3>> # binary

# Strings e char lists
"hello" # string
'hello' # char list

# Strings de múltiplas linhas
"""
Strings
de múltiplas
linhas.
"""
#=> "Strings\nde múltiplas\nlinhas"

# Strings são sempre codificadas em UTF-8:
"héllò" #=> "héllò"

# Strings são de fato apenas binários, e char lists apenas listas.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` em elixir retorna o valor ASCII para a letra `a`
?a #=> 97

# Para concatenar listas use `++`, para binários use `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

# Ranges são representados como `início..fim` (ambos inclusivos)
1..10 #=> 1..10
menor..maior = 1..10 # Pattern matching pode ser usada em ranges também
[lower, upper] #=> [1, 10]

## ---------------------------
## -- Operadores
## ---------------------------

# Matemática básica
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# Em elixir o operador `/` sempre retorna um float.

# Para divisão de inteiros use `div`
div(10, 2) #=> 5

# Para obter o resto da divisão use `rem`
rem(10, 3) #=> 1

# Há também operadores booleanos: `or`, `and` e `not`.
# Estes operadores esperam um booleano como primeiro argumento.
true and true #=> true
false or true #=> true
# 1 and true    #=> ** (ArgumentError) argument error

# Elixir também fornece `||`, `&&` e `!` que aceitam argumentos de qualquer tipo.
# Todos os valores exceto `false` e `nil` serão avaliados como true.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# Para comparações temos: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` e `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` e `!==` são mais estritos ao comparar integers e floats:
1 == 1.0  #=> true
1 === 1.0 #=> false

# Podemos comparar também dois tipos de dados diferentes:
1 < :hello #=> true

# A regra de ordenação no geral é definida abaixo:
# number < atom < reference < functions < port < pid < tuple < list < bit string

# Ao citar Joe Armstrong nisto: "A ordem de fato não é importante,
# mas que uma ordem total esteja bem definida é importante."

## ---------------------------
## -- Fluxo de Controle
## ---------------------------

# expressão `if` 
if false do
  "Isso nunca será visto"
else
  "Isso será"
end

# Também há `unless`
unless true do
  "Isso nunca será visto"
else
  "Isso será"
end

# Lembra do patter matching? Muitas estruturas de fluxo de controle em elixir contam com ela.

# `case` nos permite comparar um valor com muitos patterns:
case {:um, :dois} do
  {:quatro, :cinco} ->
    "Isso não corresponde"
  {:um, x} ->
    "Isso corresponde e vincula `x` a `:dois`"
  _ ->
    "Isso corresponde a qualquer valor"
end

# É comum vincular o valor a `_` se não precisamos dele.
# Por exemplo, se apenas a head de uma lista nos interessa:
[head | _] = [1,2,3]
head #=> 1

# Para melhor legibilidade podemos fazer o seguinte:
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond` nos permite verificar várias condições ao mesmo tempo.
# Use `cond` em vez de aninhar vários `if`'s.
cond do
  1 + 1 == 3 ->
    "Nunca serei visto"
  2 * 5 == 12 ->
    "Nem eu"
  1 + 2 == 3 ->
    "Mas eu serei"
end

# É comum definir a última condição igual a `true`, que sempre irá corresponder.
cond do
  1 + 1 == 3 ->
    "Nunca serei visto"
  2 * 5 == 12 ->
    "Nem eu"
  true ->
    "Mas eu serei (isso é essencialmente um else)"
end

# `try/catch` é usado para capturar valores que são lançados, também suporta uma
# cláusula `after` que é invocada havendo um valor capturado ou não.
try do
  throw(:hello)
catch
  message -> "Deu #{mensagem}."
after
  IO.puts("Sou o after.")
end
#=> Sou o after
# "Deu :hello"

## ---------------------------
## -- Módulos e Funções
## ---------------------------

# Funções Anônimas (repare o ponto)
square = fn(x) -> x * x end
square.(5) #=> 25

# Elas também aceitam várias cláusulas e guards.
# Guards permitem ajustes finos de pattern matching,
# sendo indicados pela palavra `when`:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir também fornece várias funções embutidas.
# Estas estão disponíveis no escopo atual.
is_number(10)    #=> true
is_list("ola") #=> false
elem({1,2,3}, 0) #=> 1

# Você pode agrupar algumas funções em um módulo. Dentro de um módulo use `def`
# para definir suas funções.
defmodule Math do
  def sum(a, b) do
    a + b
  end

  def square(x) do
    x * x
  end
end

Math.sum(1, 2)  #=> 3
Math.square(3) #=> 9

# Para compilar o módulo Math salve-o como `math.ex` e use `elixirc`
# em seu terminal: elixirc math.ex

# Dentro de um módulo podemos definir funções com `def` e funções privadas com `defp`.
# Uma função definida com `def` pode ser invocada por outros módulos,
# já uma função privada pode ser invocada apenas localmente.
defmodule PrivateMath do
  def sum(a, b) do
    do_sum(a, b)
  end

  defp do_sum(a, b) do
    a + b
  end
end

PrivateMath.sum(1, 2)    #=> 3
# PrivateMath.do_sum(1, 2) #=> ** (UndefinedFunctionError)

# Declarações de funções também suportam guards cláusulas múltiplas:
defmodule Geometry do
  def area({:rectangle, w, h}) do
    w * h
  end

  def area({:circle, r}) when is_number(r) do
    3.14 * r * r
  end
end

Geometry.area({:rectangle, 2, 3}) #=> 6
Geometry.area({:circle, 3})       #=> 28.25999999999999801048
# Geometry.area({:circle, "not_a_number"})
#=> ** (FunctionClauseError) no function clause matching in Geometry.area/1

# Devido à imutabilidade, recursão é uma grande parte do elixir
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Módulos do elixir suportam atributos, hpa atributos embutidos e você
# pode também adicionar os seus próprios.
defmodule MyMod do
  @moduledoc """
  Este é um atributo embutido em um módulo de exemplo.
  """

  @my_data 100 # Este é um atributo customizado.
  IO.inspect(@my_data) #=> 100
end

## ---------------------------
## -- Structs e Exceptions
## ---------------------------

# Structs são extensões no topo de mapas que trazem valores padrão,
# garantias em tempo de compilação e polimorfismo para o Elixir.
defmodule Pessoa do
  defstruct nome: nil, idade: 0, peso: 0
end

joe_info = %Pessoa{ nome: "Joe", idade: 30, peso: 180 }
#=> %Pessoa{idade: 30, peso: 180, nome: "Joe"}

# Acessa o valor de nome
joe_info.name #=> "Joe"

# Atualiza o valor de idade
older_joe_info = %{ joe_info | idade: 31 }
#=> %Pessoa{idade: 31, peso: 180, nome: "Joe"}

# O bloco `try` com a palavra `rescue` é usado para manipular exceções
try do
  raise "algum erro"
rescue
  RuntimeError -> "resgatado um erro em tempo de execução"
  _error -> "isso resgatará qualquer erro"
end

# Toda exceção possui uma mensagem
try do
  raise "algum erro"
rescue
  x in [RuntimeError] ->
    x.message
end

## ---------------------------
## -- Concorrência
## ---------------------------

# Elixir conta com o modelo de ator para concorrência. Tudo o que precisamos para
# escrever programas concorrentes em elixir são três primitivos: spawning processes,
# sending messages e receiving messages.

# Para iniciar um novo processo usamos a função `spawn`, a qual leva uma função
# como argumento.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` retorna um pid (process identifier), você pode usar esse pid para enviar
# mensagens ao processo. Para envio de mensagens usamos o operador `send`.
# Para tudo isso ser útil precisamos estar aptos a receber mensagens. Isto é
# realizado com o mecanismo `receive`:
defmodule Geometry do
  def area_loop do
    receive do
      {:rectangle, w, h} ->
        IO.puts("Area = #{w * h}")
        area_loop()
      {:circle, r} ->
        IO.puts("Area = #{3.14 * r * r}")
        area_loop()
    end
  end
end

# Compile o módulo e crie um processo que avalie `area_loop` no shell
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>

# Envia uma mensagem ao `pid` correspondente a um pattern na declaração de recebimento
send pid, {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# O shell também é um processo, você pode usar `self` para obter o pid atual
self() #=> #PID<0.27.0>
```

## Referências

* [Getting started guide](http://elixir-lang.org/getting_started/1.html) da [página do elixir](http://elixir-lang.org)
* [Elixir Documentation](http://elixir-lang.org/docs/master/)
* ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir) por Dave Thomas
* [Elixir Cheat Sheet](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) por Fred Hebert
* ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang) por Joe Armstrong
