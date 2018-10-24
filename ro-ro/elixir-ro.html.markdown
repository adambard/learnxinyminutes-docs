---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
    - ["Ryan Plant", "https://github.com/ryanplant-au"]
    - ["Ev Bogdanov", "https://github.com/evbogdanov"]
translators:
    - ["Vitalie Lazu", "https://github.com/vitaliel"]
lang: ro-ro
filename: learnelixir-ro.ex
---

Elixir este un limbaj funcțional modern construit pe baza mașinii virtuale Erlang.
E total compatibil cu Erlang, dar are o sintaxă mai prietenoasă și propune mai multe
posibilități.

```elixir

# Comentariile de o linie încep cu simbolul diez.

# Pentru comentarii pe mai multe linii nu există sintaxă separată,
# de aceea folosiți mai multe linii cu comentarii.

# Pentru a folosi shell-ul elixir utilizați comanda `iex`.
# Compilați modulele cu comanda `elixirc`.

# Ambele comenzi vor lucra în terminal, dacă ați instalat Elixir corect.

## ---------------------------
## -- Tipuri de bază
## ---------------------------

# Numere
3    # număr întreg
0x1F # număr întreg
3.0  # număr cu virgulă mobilă

# Atomii, sunt constante nenumerice. Ei încep cu `:`.
:salut # atom

# Tuplele sunt păstrate în memorie consecutiv.
{1,2,3} # tuple

# Putem accesa elementul tuplelui folosind funcția `elem`:
elem({1, 2, 3}, 0) #=> 1

# Listele sunt implementate ca liste înlănțuite.
[1,2,3] # listă

# Fiecare listă ne vidă are cap (primul element al listei)
# și coadă (restul elementelor).
# Putem accesa capul și coada listei cum urmează:
[cap | coadă] = [1,2,3]
cap   #=> 1
coadă #=> [2, 3]

# În Elixir, ca și în Erlang, simbolul `=` denotă potrivirea șabloanelor și
# nu atribuire.
#
# Aceasta înseamnă că expresia din stînga (șablonul) se potrivește cu
# expresia din dreaptă.
#
# În modul acesta exemplul de mai sus lucrează accesînd capul și coada unei liste.

# Potrivirea șablonului va da eroare cînd expresiile din stînga și dreapta nu se
# potrivesc, în exemplu acesta tuplele au lungime diferită.
{a, b, c} = {1, 2} #=> ** (MatchError)

# Există și date binare
<<1,2,3>>

# Sunt două tipuri de șiruri de caractere
"salut" # șir de caractere Elixir
'salut' # listă de caractere Erlang

# Șir de caractere pe mai multe linii
"""
Sunt un șir de caractere
pe mai multe linii.
"""
#=> "Sunt un șir de caractere\npe mai multe linii..\n"

# Șirurile de caractere sunt codificate în UTF-8:
"Bună dimineața" #=> "Bună dimineața"

# Șirurile de caractere sunt date binare, listele de caractere doar liste.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` în Elixir întoarce codul ASCII pentru litera `a`
?a #=> 97

# Pentru a concatena listele folosiți `++`, pentru date binare - `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'Salut ' ++ 'lume'  #=> 'Salut lume'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"Salut " <> "lume"  #=> "Salut lume"

# Diapazoanele sunt reprezentate ca `început..sfîrșit` (inclusiv)
1..10 #=> 1..10
început..sfîrșit = 1..10 # Putem folosi potrivirea șabloanelor cu diapazoane de asemenea
[început, sfîrșit] #=> [1, 10]

# Dicţionarele stochează chei şi o valoare pentru fiecare cheie
genuri = %{"Ion" => "bărbat", "Maria" => "femeie"}
genuri["Ion"] #=> "bărbat"

# Dicționare cu chei de tip atom au sintaxă specială
genuri = %{ion: "bărbat", maria: "femeie"}
genuri.ion #=> "bărbat"

## ---------------------------
## -- Operatori
## ---------------------------

# Operații matematice
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# În Elixir operatorul `/` întotdeauna întoarce un număr cu virgulă mobilă.

# Folosiți `div` pentru împărțirea numerelor întregi
div(10, 2) #=> 5

# Pentru a obține restul de la împărțire utilizați `rem`
rem(10, 3) #=> 1

# Există și operatori booleni: `or`, `and` and `not`.
# Acești operatori așteaptă ca primul argument o expresie booleană.
true and true #=> true
false or true #=> true
1 and true    #=> ** (BadBooleanError)

# Elixir de asemenea  oferă `||`, `&&` și `!` care acceptă argumente de orice tip.
# Toate valorile în afară de `false` și `nil` se vor evalua ca `true`.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# Operatori de comparație: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` și `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` și `!==` au strictețe mai mare cînd comparăm numere întregi și reale:
1 == 1.0  #=> true
1 === 1.0 #=> false

# Putem compara de asemenea și date de diferite tipuri:
1 < :salut #=> true

# La compararea diferitor tipuri folosiți următoare prioritate:
# număr < atom < referință < funcție < port < proces < tuple < listă < șir de caractere

# Cităm pe Joe Armstrong în acest caz: "Ordinea actuală nu e importantă,
dar că ordinea totală este bine definită este important."

## ---------------------------
## -- Ordinea execuției
## ---------------------------

# expresia `if`
if false do
  "Aceasta nu veți vedea niciodată"
else
  "Aceasta veți vedea"
end

# expresia opusă `unless`
unless true do
  "Aceasta nu veți vedea niciodată"
else
  "Aceasta veți vedea"
end

# Țineți minte potrivirea șabloanelor? Multe structuri în Elixir se bazează pe ea.

# `case` ne permite să comparăm o valoare cu multe șabloane:
case {:unu, :doi} do
  {:patru, :cinci} ->
    "Aceasta nu se potrivește"
  {:unu, x} ->
    "Aceasta se potrivește și atribuie lui `x` `:doi` în acest bloc"
  _ ->
    "Aceasta se va potrivi cu orice valoare"
end

# Simbolul `_` se numește variabila anonimă.
# Folosiți-l pentru valori ce nu vă interesează.
# De exemplu, dacă doar capul listei ne intereseaza:
[cap | _] = [1,2,3]
cap #=> 1

# Pentru o citire mai bună putem scri:
[cap | _coadă] = [:a, :b, :c]
cap #=> :a

# `cond` ne permite să verificăm multe condiții de odată.
# Folosiți `cond` în schimbul la multe expresii `if`.
cond do
  1 + 1 == 3 ->
    "Aceasta nu veți vedea niciodată"
  2 * 5 == 12 ->
    "Pe mine la fel"
  1 + 2 == 3 ->
    "Aceasta veți vedea"
end

# Este obușnuit de setat ultima condiție cu `true`, care se va potrivi întotdeauna.
cond do
  1 + 1 == 3 ->
    "Aceasta nu veți vedea niciodată"
  2 * 5 == 12 ->
    "Pe mine la fel"
  true ->
    "Aceasta veți vedea (este else în esență)"
end

# Blocul `try/catch` se foloște pentru prelucrarea excepțiilor.
# Elixir suportă blocul `after` care se execută în orice caz.
try do
  throw(:salut)
catch
  mesaj -> "Am primit #{mesaj}."
after
  IO.puts("Sunt în blocul after.")
end
#=> Sunt în blocul after.
# "Am primit salut"

## ---------------------------
## -- Module și Funcții
## ---------------------------

# Funcții anonime (atenție la punct la apelarea funcției)
square = fn(x) -> x * x end
square.(5) #=> 25

# Ele de asemenea aceptă multe clauze și expresii de gardă.
# Expresiile de gardă vă permit să acordați potrivirea șabloanelor,
# ele sunt indicate după cuvîntul cheie `when`:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir de asemenea oferă multe funcții incorporate.
# Ele sunt accesibile în scopul curent.
is_number(10)    #=> true
is_list("salut") #=> false
elem({1,2,3}, 0) #=> 1

# Puteți grupa cîteva funcții într-un modul. În interiorul modulului folosiți `def`
# pentru a defini funcțiile necesare.
defmodule Math do
  def sum(a, b) do
    a + b
  end

  def square(x) do
    x * x
  end
end

Math.sum(1, 2)  #=> 3
Math.square(3)  #=> 9

# Pentru a compila modulul nostru simplu Math îl salvăm ca `math.ex` și utilizăm `elixirc`.
# în terminal: elixirc math.ex

# În interiorul modulului putem defini funcții cu `def` și funcții private cu `defp`.
defmodule PrivateMath do
  # O funcție definită cu `def` este accesibilă pentru apelare din alte module,
  def sum(a, b) do
    do_sum(a, b)
  end

  # O funcție privată poate fi apelată doar local.
  defp do_sum(a, b) do
    a + b
  end
end

PrivateMath.sum(1, 2)    #=> 3
PrivateMath.do_sum(1, 2) #=> ** (UndefinedFunctionError)

# Declarația funcției de asemenea suportă expresii de gardă și multe clauze:
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
Geometry.area({:circle, "not_a_number"}) #=> ** (FunctionClauseError)

# Din cauza variabilelor imutabile, un rol important îl ocupă funcțiile recursive
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Modulele în Elixir suportă atribute, există atribute incorporate și
# puteți adăuga altele.
defmodule MyMod do
  @moduledoc """
  Este un atribut incorporat
  """

  @my_data 100 # Acesta e atributul nostru
  IO.inspect(@my_data) #=> 100
end

# Operatorul |> permite transferarea rezultatului unei expresii din stînga
# ca primul argument al unei funcții din dreapta.
Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## ---------------------------
## -- Structuri și Excepții
## ---------------------------

# Structurile sunt extensii a dicționarelor ce au valori implicite,
# verificări în timpul compilării și polimorfism
defmodule Person do
  defstruct name: nil, age: 0, height: 0
end

joe_info = %Person{ name: "Joe", age: 30, height: 180 }
#=> %Person{age: 30, height: 180, name: "Joe"}

# Acesarea cîmpului din structură
joe_info.name #=> "Joe"

# Actualizarea valorii cîmpului
older_joe_info = %{ joe_info | age: 31 }
#=> %Person{age: 31, height: 180, name: "Joe"}

# Blocul `try` cu cuvîntul cheie `rescue` e folosit pentru a prinde excepții
try do
  raise "o eroare"
rescue
  RuntimeError -> "a fost prinsă o eroare runtime"
  _error -> "aici vor fi prinse toate erorile"
end
#=> "a fost prinsă o eroare runtime"

# Toate excepțiile au un mesaj
try do
  raise "o eroare"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "o eroare"

## ---------------------------
## -- Concurența
## ---------------------------

# Concurența în Elixir se bazează pe modelul actor. Pentru a scrie programe
# concurente avem nevoie de trei lucruri:
# 1. Crearea proceselor
# 2. Trimiterea mesajelor
# 3. Primirea mesajelor

# Un nou proces se crează folosind funcția `spawn`, care primește o funcție
# ca argument.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` întoarce identificatorul procesului pid, îl puteți folosi pentru
# a trimite mesaje procesului. Mesajele se transmit folosind operatorul `send`.  
# Pentru primirea mesajelor se folosește mecanismul `receive`:

# Blocul `receive do` este folosit pentru așteptarea mesajelor și prelucrarea lor
# cînd au fost primite. Blocul `receive do` va procesa doar un singur mesaj primit.
# Pentru a procesa mai multe mesaje, funcția cu blocul `receive do` trebuie
# recursiv să se auto apeleze.

defmodule Geometry do
  def area_loop do
    receive do
      {:rectangle, w, h} ->
        IO.puts("Aria = #{w * h}")
        area_loop()
      {:circle, r} ->
        IO.puts("Aria = #{3.14 * r * r}")
        area_loop()
    end
  end
end

# Compilați modulul și creați un proces
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# Un alt mod
pid = spawn(Geometry, :area_loop, [])

# Trimiteți un mesaj către `pid` care se va potrivi cu un șablon din blocul `receive`
send pid, {:rectangle, 2, 3}
#=> Aria = 6
#   {:rectangle,2,3}

send pid, {:circle, 2}
#=> Aria = 12.56000000000000049738
#   {:circle,2}

# Interpretatorul este de asemenea un proces, puteți folosi `self` 
# pentru a primi identificatorul de proces: 
self() #=> #PID<0.27.0>

## ---------------------------
## -- Agenții
## ---------------------------

# Un agent este un proces care urmărește careva valori ce se schimbă.

# Creați un agent cu `Agent.start_link`, transmițînd o funcție.
# Stare inițială a agentului va fi rezultatul funcției.
{ok, my_agent} = Agent.start_link(fn -> ["roșu", "verde"] end)

# `Agent.get` primește numele agentului și o `fn` care primește starea curentă
# Orice va întoarce `fn` este ceea ce veți primi înapoi: 
Agent.get(my_agent, fn colors -> colors end) #=> ["roșu", "verde"]

# Actualizați starea agentului în acelaș mod:
Agent.update(my_agent, fn colors -> ["albastru" | colors] end)
```

## Link-uri utile

* [Primii pași](http://elixir-lang.org/getting-started/introduction.html) de pe [situl Elixir](http://elixir-lang.org)
* [Documentația oficială Elixir](http://elixir-lang.org/docs/master/)
* [Un mic conspect pe Elixir](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* [Cartea "Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir) de Dave Thomas
* [Cartea "Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) de Fred Hebert
* [Cartea "Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang) de Joe Armstrong
