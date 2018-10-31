---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
    - ["Ryan Plant", "https://github.com/ryanplant-au"]
translator:
    - ["Adrian Carrascal", "https://github.com/acarrascalgarcia"]
filename: learnelixir-es.ex
lang: es-es

---

Elixir es un lenguaje funcional moderno construido sobre la máquina virtual de Erlang.
Es completamente compatibe con Erlang, sin embargo, ofrece una sintaxis más estandar
y otras características más.

```elixir

# Los comentarios de única línea
# comienzan con un símbolo numérico.

# No hay comentarios multilinea,
# pero se pueden apilar varios comentarios.

# Para usar el shell de elixir se usa el comando `iex`.
# Los módulos se compilan con el comando `elixirc`.

# Ambos deberían estar en la ruta si elixir se instaló correctamente.

## ---------------------------
## -- Tipos básicos
## ---------------------------

# Hay números
3    # integer
0x1F # integer
3.0  # float

# Átomos, que son literales, una constante con nombre. Comienzan con `:`.
:hello # atom

# Tuples that are stored contiguously in memory.
# Tuplas que se almacenan contiguamente en memoria.
{1,2,3} # tuple

# Se puede acceder a un elemento de una tupla con la función `elem`:
elem({1, 2, 3}, 0) #=> 1

# Listas que se implementan como listas enlazadas.
[1,2,3] # list

# Se puede acceder al primer y último elemento de la lista como:
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# En elixir, solo como Erlang, el `=` denota la coincidencia de patrones y
# no una asignación.
#
# This is how the above example of accessing the head and tail of a list works.
# Así es como el ejemplo anterior de acceder al
# primer y último elemento de una lista trabaja.

# Una coincidencia de patrón errará cuando los lados no coincidan, en este ejemplo
# las tuplas tienen diferentes tamaños.
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# También hay binarios
<<1,2,3>> # binary

# Cadenas y listas de caracteres
"hello" # string
'hello' # char list

# Cadenas de varias lineas
"""
I'm a multi-line
string.
"""
#=> "I'm a multi-line\nstring.\n"

# Todas las cadenas se codifican en UTF-8:
"héllò" #=> "héllò"

# Las cadenas son solo binarios realmente, y la lista de caracteres solo listas.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` en elixir devuelve el valor ASCII para el caracter `a`
?a #=> 97

# Para concatenar listas se usa `++`, para binarios `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

# Los rangos se representan como `start..end` (Es inclusivo)
1..10 #=> 1..10
lower..upper = 1..10 # Se puede usar la coincidencia de patrones en los rangos también
[lower, upper] #=> [1, 10]

# Los mapas son pares de llave-valor
genders = %{"david" => "male", "gillian" => "female"}
genders["david"] #=> "male"

# Los mapas con llaves de tipo átomo se pueden usar como esto
genders = %{david: "male", gillian: "female"}
genders.gillian #=> "female"

## ---------------------------
## -- Opetadores
## ---------------------------

# Aritméticos
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# En elixir el operador `/` siempre devuelve un número flotante

# Para hacer la división de número entero se debe usar `div`
div(10, 2) #=> 5

# Para obtener el residuo de la división se debe usar `rem`
rem(10, 3) #=> 1

# También hay operadores lógicos: `or`, `and` y `not`.
# Estos operadores esperan un boolean como su primer argumento.
true and true #=> true
false or true #=> true
# 1 and true    #=> ** (ArgumentError) argument error

# Elixir también provee `||`, `&&` y `!` donde acepta argumentos de cualquier tipo.
# Todos los valores excepto `false` y `nil` se evaluarán como verdadero.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# Para comparaciones se tiene: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` y `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` y `!==` son más estrictos cuando comparan números:
1 == 1.0  #=> true
1 === 1.0 #=> false

# También se puede comparar dos tipos de datos diferentes:
1 < :hello #=> true

# No se necesita memorizar el orden pero es importante tenerlo en cuenta:
# number < atom < reference < functions < port < pid < tuple < list < bit string

## ---------------------------
## -- Control de flujo
## ---------------------------

# Expresión `if`
if false do
  "This will never be seen"
else
  "This will"
end

# También está la expresión `unless`
unless true do
  "This will never be seen"
else
  "This will"
end

# Se acuerda de la coincidencia de patrones?
# Muchas estructuras de control de flujo en elixir confían en ella.

# `case` permite comparar un valor con muchos patrones:
case {:one, :two} do
  {:four, :five} ->
    "This won't match"
  {:one, x} ->
    "This will match and bind `x` to `:two` in this clause"
  _ ->
    "This will match any value"
end

# Es común vincular el valor a `_` si no se necesita.
# Por ejemplo, si unicamente el primer elemento de la lista es importante:
[head | _] = [1,2,3]
head #=> 1

# Para una mejor lectura se puede hace lo siguiente:
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond` permite comprobar muchas condiciones al mismo tiempo.
# Usar `cond` en vez de muchas expresiones `if` anidadas.
cond do
  1 + 1 == 3 ->
    "I will never be seen"
  2 * 5 == 12 ->
    "Me neither"
  1 + 2 == 3 ->
    "But I will"
end

# Es común estabecer la última condición como `true`, donde siempre va a coincidir.
cond do
  1 + 1 == 3 ->
    "I will never be seen"
  2 * 5 == 12 ->
    "Me neither"
  true ->
    "But I will (this is essentially an else)"
end

# `try/catch` se usa para atrapar valores que se lanzan, también soporta una
# clausula `after` que se invoca sin importar si un valor se atrapó o no.
try do
  throw(:hello)
catch
  message -> "Got #{message}."
after
  IO.puts("I'm the after clause.")
end
#=> I'm the after clause
# "Got :hello"

## ---------------------------
## -- Módulos y Funciones
## ---------------------------

# Anonymous functions (notice the dot)
# Funciones anónimas (Ver el punto `.`)
square = fn(x) -> x * x end
square.(5) #=> 25

# También aceptan muchas cláusulas y guards.
# Los guards permiten afinar las coincidencias de patrones,
# se indican por la palabra reservada `when`:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir también provee muchas funciones incorporadas.
# Esas están disponibles en el ámbito actual.
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# Se pueden agrupar varias funciones en un módulo. Dentro de un módulo
# se usa `def` para definir las funciones.
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

# Para compilar el módulo simple de Math se guarda como `math.ex` y se usa `elixirc`
# en la terminal: elixirc math.ex

# Dentro de un módulo se puede definir funciones con `def` y funciones privadas con `defp`.
# Una función definida con `def` está disponible para ser invocada desde otros módulos,
# una función privada se puede solo invocar localmente.
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

# La declaración de funciones también soportan guards y múltiples cláusulas:
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

# Debido a la inmutabilidad, la recursión es una gran parte de elixir
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Los módulos de Elixir soportan atributos, hay atributos incorporados y
# se pueden agregar otros personalizados.
defmodule MyMod do
  @moduledoc """
  This is a built-in attribute on a example module.
  """

  @my_data 100 # This is a custom attribute.
  IO.inspect(@my_data) #=> 100
end

# El operador pipe |> permite que se pase la salida de una expresión
# como el primer parámetro en una función. 

Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## ---------------------------
## -- Structs and Excepciones
## ---------------------------

# Los Structs son extensiones de los mapas que traen valores por defecto,
# garantes en tiempo de compilación y polimorfismo en Elixir.
defmodule Person do
  defstruct name: nil, age: 0, height: 0
end

joe_info = %Person{ name: "Joe", age: 30, height: 180 }
#=> %Person{age: 30, height: 180, name: "Joe"}

# Acceder al valor de name
joe_info.name #=> "Joe"

# Actualizar el valor de age
older_joe_info = %{ joe_info | age: 31 }
#=> %Person{age: 31, height: 180, name: "Joe"}

# El bloque `try` con la palabra reservada `rescue` se usa para manejar excepciones
try do
  raise "some error"
rescue
  RuntimeError -> "rescued a runtime error"
  _error -> "this will rescue any error"
end
#=> "rescued a runtime error"

# Todas las excepciones tienen un mensaje
try do
  raise "some error"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "some error"

## ---------------------------
## -- Concurrencia
## ---------------------------

# Elixir confía en el modelo actor para la concurrencia. Todo lo que se necesita para escribir
# programas concurrentes en elixir son tres primitivas: procesos de desove,
# envío de mensajes y recepción de mensajes.

# Para empezar un nuevo proceso se usa la función `spawn`,
# donde toma una función como argumento.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` devuelve un pid (identificador de proceso), se puede usar este pid para enviar
# mensajes para el proceso. Para hacer que un mensaje pase se usa el operador `send`.
# Para que todo esto se útil se necesita estar disponibles para recibir mensajes. Esto se
# alcanza con el mecanismo `receive`:

# El bloque `receive do` se usa para escuchar los mensajes y procesarlos
# cuando se reciben. Un bloque `receive do` solo procesará
# un mensaje recibido. Para procesar múltiples mensajes,
# una función con un bloque `receive do` tiene que llamarse recursivamente
# para entrar en el bloque `receive do` otra vez.

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

# Compilar el módulo y crear un proceso que evalue `area_loop` en el shell
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# Como alternativa
pid = spawn(Geometry, :area_loop, [])

# Enviar un mensaje al `pid` que coincidirá con un patrón en el que recibe una sentencia
send pid, {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# El shell también es un proceso, se puede usar `self` para obtener el pid actual
self() #=> #PID<0.27.0>

## ---------------------------
## -- Agentes
## ---------------------------

# Un agente es un proceso que mantiene el seguimiento de algún valor cambiante

# Un agente se crea con `Agent.start_link`, introducuendole una función
# El estado inicial del agente será lo que sea que la función devuelva
{ok, my_agent} = Agent.start_link(fn -> ["red, green"] end)

# `Agent.get` toma un nombre de agente y un `fn` que se pasa como el estado actual
# Lo que sea que este `fn` devuelva es lo que se obtendrá de vuelta
Agent.get(my_agent, fn colors -> colors end) #=> ["red, "green"]

# El estado del agente se actualiza de la misma manera
Agent.update(my_agent, fn colors -> ["blue" | colors] end)
```

## Referencias

* [Getting started guide](http://elixir-lang.org/getting-started/introduction.html) from the [Elixir website](http://elixir-lang.org)
* [Elixir Documentation](http://elixir-lang.org/docs/master/)
* ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir) by Dave Thomas
* [Elixir Cheat Sheet](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) by Fred Hebert
* ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang) by Joe Armstrong
