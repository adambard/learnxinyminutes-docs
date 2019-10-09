---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
    - ["Ryan Plant", "https://github.com/ryanplant-au"]
    - ["Ev Bogdanov", "https://github.com/evbogdanov"]
translator:
    - ["Timothé Pardieu", "https://github.com/timprd"]
filename: learnelixir-fr.ex
lang: fr-fr
---
Elixir est un langage de programmation fonctionnel moderne reposant sur la machine virtuelle BEAM, qui héberge aussi Erlang.
Il est totalement compatible avec Erlang mais dispose d'une syntaxe plus agréable et apporte de nouvelles fonctionnalités. 


```elixir

# Un commentaire simple sur une seule ligne commence par un dièse.

# Il n'y a pas de commentaire multi-ligne,
# Mais il est possible de les empiler comme ici.

# La commande `iex` permet de lancer le shell Elixir.
# La commande `elixirc` permet de compiler vos modules.

# Les deux devraient être dans votre path si vous avez installé Elixir correctement.

## ---------------------------
## -- Types basiques
## ---------------------------

# Il y a les nombres
3    # Integer 
0x1F # Integer
3.0  # Float

# Les atomes, des littéraux, qui sont des constantes avec comme valeur leur nom. 
# Ils commencent par `:`.

:hello # atom

# Il existe également des n-uplets dont les valeurs sont stockés de manière contiguë 
# en mémoire.

{1,2,3} # tuple

# Il est possible d'accéder à un element d'un tuple avec la fonction 
# `elem`:
elem({1, 2, 3}, 0) #=> 1

# Les listes sont implémentées sous forme de listes chainées.
[1,2,3] # list

# La tête et le reste d'une liste peuvent être récupérés comme cela :
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# En Elixir, comme en Erlang, le `=` dénote un 'pattern matching' 
# (Filtrage par motif) et non une affectation.
# Cela signifie que la partie de gauche (pattern) est comparé (match) à 
# la partie de droite. 


# Une erreur sera lancée si aucun model (match) est trouvé.
# Dans cet exemple les tuples ont des tailles différentes
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# Il y a aussi les binaires
<<1,2,3>> # binary

# Chaine de caractères et liste de caractères
"hello" # string
'hello' # char list

# Chaine de caractères sur plusieurs lignes
"""
Je suis une chaine de caractères
sur plusieurs lignes.
"""
#=> "Je suis une chaine de caractères\nsur plusieurs lignes.\n"

# Les chaines de caractères sont encodées en UTF-8 :
"héllò" #=> "héllò"

# Les chaines de caractères sont des binaires tandis que
# les listes de caractères sont des listes.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` en Elixir retourne le code ASCII (Integer) de la lettre `a`
?a #=> 97

# Pour concaténer des listes il faut utiliser `++`, et `<>` pour les
# binaires
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

# Les intervalles sont représentés de cette sorte `début..fin`
# (tout deux inclusifs)
1..10 #=> 1..10
bas..haut = 1..10 # Possibilité d'utiliser le pattern matching sur les intervalles aussi.
[bas, haut] #=> [1, 10]

# Les Maps (Tableau associatif) sont des paires clée - valeur
genders = %{"david" => "male", "gillian" => "female"}
genders["david"] #=> "male"

# Les maps avec des atomes peuvent être utilisés comme cela
genders = %{david: "male", gillian: "female"}
genders.gillian #=> "female"

## ---------------------------
## -- Operateurs
## ---------------------------

# Mathématiques
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# En Elixir l'opérateur `/` retourne toujours un Float (virgule flottante).

# Pour faire une division avec entier il faut utiliser `div`
div(10, 2) #=> 5

# Pour obtenir le reste de la division il faut utiliser `rem`
rem(10, 3) #=> 1

# Il y a aussi les opérateurs booléen: `or`, `and` et `not`.
# Ces opérateurs attendent un booléen en premier argument.
true and true #=> true
false or true #=> true
# 1 and true
#=> ** (BadBooleanError) expected a booléens on left-side of "and", got: 1

# Elixir fournit aussi `||`, `&&` et `!` qui acceptent des arguments de
# tout type.
# Chaque valeur sauf `false` et `nil` seront évalués à vrai (true).
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# Pour les comparaisons il y a : `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` et `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` et `!==` sont plus stricts en comparant les Integers (entiers) 
# et les Floats (nombres à virgules) :
1 == 1.0  #=> true
1 === 1.0 #=> false

# On peut aussi comparer deux types de données différents :
1 < :hello #=> true

# L'ordre est défini de la sorte :
# number < atom < reference < functions < port < pid < tuple < list < bit string

# Pour citer Joe Armstrong : "The actual order is not important,
# but that a total ordering is well defined is important."

## ---------------------------
## -- Structure de contrôle
## ---------------------------

# Condition avec `if` (si)
if false do
  "Cela ne sera pas vu"
else
  "Cela le sera"
end

# Condition avec `unless` (sauf).
# Il correspond à la négation d'un `if` (si) 
unless true do
  "Cela ne sera pas vu"
else
  "Cela le sera"
end

# Beaucoup de structures en Elixir se basent sur le pattern matching.
# `case` permet de comparer une valeur à plusieurs modèles:
case {:one, :two} do
  {:four, :five} ->
    "Ne match pas"
  {:one, x} ->
    "Match et lie `x` à `:two` dans ce cas"
  _ ->
    "Match toutes les valeurs"
end

# Il est commun de lier la valeur à `_` si on ne l'utilise pas.
# Par exemple, si seulement la tête d'une liste nous intéresse:
[head | _] = [1,2,3]
head #=> 1

# Pour plus de lisibilité, ce procédé est utilisé:
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond` permet de vérifier plusieurs conditions à la fois.
# Il est conseillé d'utiliser `cond` plutôt que des `if` imbriqués.
cond do
  1 + 1 == 3 ->
    "Je ne serai pas vu"
  2 * 5 == 12 ->
    "Moi non plus"
  1 + 2 == 3 ->
    "Mais moi oui"
end

# Il est commun d'attribuer la dernière condition à true (vrai), qui
# matchera toujours.
cond do
  1 + 1 == 3 ->
    "Je ne serai pas vu"
  2 * 5 == 12 ->
    "Moi non plus"
  true ->
    "Mais moi oui (représente un else)" 
end

# `try/catch` est utilisé pour attraper les valeurs rejetées. 
# Il supporte aussi un 
# `after` qui est appelé autant si une valeur est jetée ou non.
try do
  throw(:hello)
catch
  message -> "Message : #{message}."
after
  IO.puts("Je suis la clause after (après).")
end
#=> Je suis la clause after (après).
# "Message : :hello"

## ---------------------------
## -- Modules et Fonctions
## ---------------------------

# Fonctions anonymes (notez le point).
square = fn(x) -> x * x end
square.(5) #=> 25

# Les fonctions anonymes acceptent aussi de nombreuses clauses et guards (gardes).
# Les guards permettent d'affiner le pattern matching,
# ils sont indiqués par le mot-clef `when` :
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir propose aussi de nombreuses fonctions internes.
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# Il est possible de grouper plusieurs fonctions dans un module.
# Dans un module, les fonctions sont définies par `def`
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

# Pour compiler notre module `Math`,
# il faut le sauvegarder en tant que `math.ex` et utiliser `elixirc`.
# Executez ainsi `elixirc math.ex` dans le terminal.

# Au sein d'un module, nous pouvons définir les fonctions avec `def`
# et `defp` pour les fonctions privées.
# Une fonction définie par `def` est disponible dans les autres
# modules. Une fonction privée est disponible localement seulement.
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

# La déclaration de fonction supporte également les guards (gardes) 
# et les clauses. 
# Quand une fonction avec plusieurs clauses est appelée,
# la première fonction dont la clause est satisfaite par les arguments sera appelée.
# Exemple: le code `area({:circle, 3})` appelle la deuxième fonction definie plus bas,
# et non la première car ses arguments correspondent à la signature de cette dernière:
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

# En raison de l'immutabilité, la récursivité est une grande partie
# d'Elixir
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Les modules Elixir supportent des attributs internes,
# ceux-ci peuvent aussi être personnalisés.
defmodule MyMod do
  @moduledoc """
  This is a built-in attribute on a example module.
  """

  @my_data 100 # Attribut personnel.
  IO.inspect(@my_data) #=> 100
end

# L'opérateur pipe (|>) permet de passer la sortie d'une expression
# en premier paramètre d'une fonction. 

Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## ---------------------------
## -- Structs et Exceptions
## ---------------------------

# Les Structs sont des extensions des Maps.
# Apportant en plus les valeurs par defaut, le polymorphisme et 
# la vérification à la compilation dans Elixir.
defmodule Person do
  defstruct name: nil, age: 0, height: 0
end

jean_info = %Person{ name: "Jean", age: 30, height: 180 }
#=> %Person{age: 30, height: 180, name: "Jean"}

# Access the value of name
jean_info.name #=> "Jean"

# Update the value of age
older_jean_info = %{ jean_info | age: 31 }
#=> %Person{age: 31, height: 180, name: "Jean"}

# Le bloc `try` avec le mot-clef `rescue` est utilisé pour gérer les exceptions
try do
  raise "some error"
rescue
  RuntimeError -> "rescued a runtime error"
  _error -> "this will rescue any error"
end
#=> "rescued a runtime error"

# Chaque exception possède un message
try do
  raise "some error"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "some error"

## ---------------------------
## -- Concurrence
## ---------------------------

# Elixir se repose sur le modèle d'acteur pour gérer la concurrence.
# Pour écrire un programme concurrent en Elixir il faut trois 
# primitives: spawning processes (création), sending messages (envoi) 
# et receiving messages (réception).

# Pour débuter un nouveau processus, il faut utiliser 
# la fonction `spawn` qui prend en argument une fonction.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` retourn un pid (identifiant de processus), il est possible 
# d'utiliser ce pid pour envoyer un message au processus.
# Pour faire parvenir le message il faut utiliser l'opérateur `send`.
# Pour que cela soit utile il faut être capable de recevoir les 
# messages.
# Cela est possible grâce au mechanisme de `receive`:

# Le bloc `receive do` est utilisé pour écouter les messages et les traiter
# au moment de la réception. Un bloc `receive do` pourra traiter un seul
# message reçu.
# Pour traiter plusieurs messages, une fonction avec un bloc `receive do` 
# doit s'appeler elle-même récursivement.

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

# Ceci compile le module et créer un processus qui évalue dans le terminal `area_loop`
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# Alternativement
pid = spawn(Geometry, :area_loop, [])

# On envoi un message au `pid` qui correspond à la régle de réception.
send pid, {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# Le shell est aussi un processus, il est possible d'utiliser `self`
# pour obtenir le pid du processus courant.
self() #=> #PID<0.27.0>

## ---------------------------
## -- Agents
## ---------------------------

# Un agent est un processus qui garde les traces des valeurs modifiées.

# Pour créer un agent on utilise `Agent.start_link` avec une fonction.
# L'état initial de l'agent sera ce que la fonction retourne
{ok, my_agent} = Agent.start_link(fn -> ["red", "green"] end)

# `Agent.get` prend un nom d'agent et une fonction (`fn`).
# Qu'importe ce que cette `fn` retourne, l'état sera ce qui est retourné.
Agent.get(my_agent, fn colors -> colors end) #=> ["red", "green"]

# Modification de l'état de l'agent
Agent.update(my_agent, fn colors -> ["blue" | colors] end)
```

## Références

* [Guide de debut](http://elixir-lang.org/getting-started/introduction.html) depuis le site [Elixir](http://elixir-lang.org)
* [Documentation Elixir ](https://elixir-lang.org/docs.html)
* ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir) de Dave Thomas
* [Elixir Cheat Sheet](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) de Fred Hebert
* ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang) de Joe Armstrong
