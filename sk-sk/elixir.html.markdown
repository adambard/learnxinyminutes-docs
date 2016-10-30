---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
    - ["Ryan Plant", "https://github.com/ryanplant-au"]
translators:
  - ["Peter Szatmary", "https://github.com/peterszatmary"]
lang: sk-sk
filename: learnelixir-sk.ex
---

Elixir je moderný funkcionálny jazyk vytvorený nad Erlang VM (virtuálnym
strojom). Je plne kompatibilný s Erlangom, ale ponúka viac štandardnú syntax
a množstvo funkcií.

```Elixir

# Jednoriadkový komentár začína symbolom #

# Neexistuje viacriadkový komentár, avšak je možné vyskladať za sebou viac
# jednoriadkových komentárov.

# Pre spustenie Elixir shellu zadajte príkaz `iex`
# Kompiláciu zdrojových kódov vykonáte príkazom `elixirc`

# Obe príkazy by sa už mali nachádzať v path pokiaľ ste nainštalovali elixir
# správne.

## ---------------------------
## -- Základné typy
## ---------------------------

# Čísla
3    # integer
0x1F # integer
3.0  # float

# Atómy, sú literály, konštanty s rovnakým menom. Začínajú s `:`.
:ahoj # atom

# Tzv. Tuples sú v pamäti uložené súvisle.
{1,2,3} # tuple

# Pristúpiť k tuple elementu vieme pomocou funkcie `elem`:
elem({1, 2, 3}, 0) #=> 1

# Zoznamy sú implementované ako linkované zoznamy.
[1,2,3] # zoznam

# Vieme pristúpiť k hlavičke (head) a chvostu (tail) zoznamu:
[head | tail] = [1,2,3]
head #=> 1
tail #=> [2,3]

# V Elixire, rovnako ako v Erlangu, `=` znamená pattern matching a nie
# klasické priradenie.
#
# To znamená, že ľavá strana (pattern / vzor) je postavená oproti pravej
# strane.
#
# Takto funguje aj príklad vyššie s čítaním hlavičky a chvosta zoznamu.

# Pattern match končí chybou ak sa obe strany nezhodujú, v tomto príklade majú
# tuples rôznu veľkosť.
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# Binárne typy
<<1,2,3>> # binary

# Reťazce a zoznamy znakov
"ahoj" # reťazec
'ahoj' # zoznam znakov

# Viacriadkový reťazec
"""
Ja som viacriadkový
reťazec.
"""
#=> "Ja som viacriadkový\nreťazec.\n"

# Reťazce sú kódované v UTF-8:
"héllò" #=> "héllò"

# Reťazce sú skutočne iba binárne typy, a zoznamy znakov sú iba zoznamy.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` v elixire vráti ASCII číselnú reprezentáciu pre znak `a`
?a #=> 97

# Pre spájanie zoznamov sa používa `++`, pre binárne typy `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'ahoj ' ++ 'svet'  #=> 'ahoj svet'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"Ahoj " <> "svet"  #=> "ahoj svet"

# Rozsahy sú reprezentované ako `začiatok..koniec` (obe inkluzívne)
1..10 #=> 1..10
dolna..horna = 1..10 # Na rozsahy možno použiť rovnako pattern matching
[dolna, horna] #=> [1, 10]

# Mapy sú páry kľúč-hodnota
pohlavia = %{"david" => "muž", "gillian" => "žena"}
pohlavia["david"] #=> "muž"

# Mapy s kľúčmi reprezentovanými atómami môžu byť použité takto
pohlavia = %{david: "muž", gillian: "žena"}
pohlavia.gillian #=> "žena"

## ---------------------------
## -- Operátory
## ---------------------------

# Trošku matematiky
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# V elixire operátor `/` vždy vráti float (reálne číslo).

# Pre celočíselné delenie sa používa `div`
div(10, 2) #=> 5

# Pre celočíselné delenie so zvyškom `rem`
rem(10, 3) #=> 1

# Boolean operátory: `or`, `and` a `not`.
# Tieto operátori očakávajú ako svoj prvý argument boolean.
true and true #=> true
false or true #=> true
# 1 and true    #=> ** (ArgumentError) argument error

# Elixir tiež poskytuje `||`, `&&` a `!` , ktoré akceptujú argumenty
# ľubovoľného typu.
# Všetky hodnoty okrem `false` a `nil` budú vyhodnotené ako true.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# Pre porovnávanie máme: `==`, `!=`, `===`, `!==`, `<=`,
`>=`, `<` a `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` a `!==` sú viac striktné pri porovnávaní celých čísel (integer) a
# desatinných čísel (float).
1 == 1.0  #=> true
1 === 1.0 #=> false

# Vieme porovnať dokonca dva rôzne údajové typy:
1 < :ahoj #=> true

# Celkové poradie triedenia:
#
# číslo < atom < referencia < funkcia < port < pid < tuple < zoznam < bitový
# string

# Výrok Joe Armstronga: "Aktuálne poradie nie je dôležité, ale
# dôležité je to, že celkové poradie je dobre definované."

## ---------------------------
## -- Riadenie toku
## ---------------------------

# `if` výraz
if false do
  "Toto nebude nikdy videné"
else
  "Toto bude"
end

# Existuje aj `unless`
unless true do
  "Toto nebude nikdy videné"
else
  "Toto bude"
end

# Pamätáte sa na pattern matching? Mnoho štruktúr pre riadenie toku v
# elixire sa spoliehajú práve na pattern matching.

# `case` dovolí nám porovnať hodnotu oproti mnohým vzorom:
case {:one, :two} do
  {:four, :five} ->
    "Toto nebude zhodné"
  {:one, x} ->
    "Toto bude zhodné a nastaví `x` na hodnotu `:two` "
  _ ->
    "Toto bude zhodné z ľubovoľnou hodnotou."
end

# Je zvyčajné nastaviť hodnotu do `_` ak ju nepotrebujete.
# Napríklad, ak je pre nás potrebná iba hlavička zoznamu (head):
[head | _] = [1,2,3]
head #=> 1

# Pre lepšiu čitateľnosť môžme urobiť nasledovné:
[head | _tail] = [:a, :b, :c]
head #=> :a

# `cond` dovoľuje kontrolovať viac podmienok naraz.
# Použite `cond` namiesto vnorovania mnohých `if` výrazov.
cond do
  1 + 1 == 3 ->
    "Nebudem nikdy videný"
  2 * 5 == 12 ->
    "Ani ja"
  1 + 2 == 3 ->
    "Ja budem"
end

# Je bežné nastaviť poslednú podmienku rovnajúcu sa `true` , ktorá bude vždy
zodpovedať.
cond do
  1 + 1 == 3 ->
    "Nebudem nikdy videný"
  2 * 5 == 12 ->
    "Ani ja"
  true ->
    "Ale ja budem (je to v podstate vetva else)"
end

# `try/catch` sa používa na zachytenie hodnôt, ktoré boli vyhodené, takisto
# podporuje `after` klauzulu, ktorá je zavolaná vždy, či bola hodnota
# zachytená alebo nie.
try do
  throw(:ahoj)
catch
  message -> "Mám #{message}."
after
  IO.puts("Som after klauzula.")
end
#=> Som after klauzula
# "Mám :ahoj"

## ---------------------------
## -- Moduly a funkcie
## ---------------------------

# Anonymné funkcie (všimnite si bodku)
stvorec = fn(x) -> x * x end
stvorec.(5) #=> 25

# Takisto akceptujú viax klauzúl a tzv. stráže (guards).
# Stráže vám umožnia pattern matching ešte viac zlepšiť, tieto časti sú
# označené kľúčovým slovom `when`:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir tiež poskytuje množstvo vstavaných funkcií.
# Tie sú dostupné v aktuálnom scope (viditeľnej oblasti).
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# Možno zgrupovať viac funkcií do jedného modulu. V module použite `def`
# na definíciu funkcie.
defmodule Matematika do
  def sucet(a, b) do
    a + b
  end

  def na_druhu(x) do
    x * x
  end
end

Matematika.sucet(1, 2)  #=> 3
Matematika.na_druhu(3) #=> 9

# Na zkompilovanie našeho Matematika modulu ho uložte ako `math.ex`  a použite
# `elixirc` v termináli: elixirc math.ex

# V module môžme definovať funkcie s `def` a privátne funkcie s `defp`.
# Funkcia definovaná s `def` je možné volať z iných modulov, privátne funkcie
# môžu byť volané iba lokálne.
defmodule SukromnaMatematika do
  def sucet(a, b) do
    rob_sucet(a, b)
  end

  defp rob_sucet(a, b) do
    a + b
  end
end

SukromnaMatematika.sucet(1, 2)    #=> 3
# SukromnaMatematika.rob_sucet(1, 2) #=> ** (UndefinedFunctionError)

# Deklarácie funkcií tiež podporujú stráže (guards) a viacnásobné klauzuly:

defmodule Geometria do
  def oblast({:obdlznik, w, h}) do
    w * h
  end

  def oblast({:kruh, r}) when is_number(r) do
    3.14 * r * r
  end
end

Geometria.oblast({:obdlznik, 2, 3}) #=> 6
Geometry.oblast({:kruh, 3})       #=> 28.25999999999999801048
# Geometria.oblast({:kruh, "nie_je_cislo"})
#=> ** (FunctionClauseError) no function clause matching in Geometria.oblast/1

# Vďaka nemeniteľnosti (immutability) je rekurzia významnou časťou elixiru
defmodule Recurzia do
  def sumuj_zoznam([hlavicka | schvost], acc) do
    sumuj_zoznam(chvost, acc + hlavicka)
  end

  def sumuj_zoznam([], acc) do
    acc
  end
end

Recurzia.sumuj_zoznam([1,2,3], 0) #=> 6

# Elixir moduly podporujú atribúty, existujú vstavané atribúty a takisto
# môžte pridávať vlastné.
defmodule MojModul do
  @moduledoc """
  Toto je vstavaný atribút v príkladovom module.
  """

  @moj_udaj 100 # Toto je vlastný atribút.
  IO.inspect(@moj_udaj) #=> 100
end

# Pipe operátor (rúra) |> umožnuje predať výsledok výrazu ako prvý parameter
# do ďalšej funkcie.

Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## ---------------------------
## -- Štruktúry a výnimky
## ---------------------------

# Štruktúry sú rozšírenia postavené na mapách, ktoré prinášajú defaultné
# hodnoty, garancie v čase kompilácie a polymorfizmus do Elixiru.
defmodule Osoba do
  defstruct meno: nil, vek: 0, vyska: 0
end

joe_info = %Osoba{ meno: "Joe", vek: 30, vyska: 180 }
#=> %Osoba{vek: 30, vyska: 180, meno: "Joe"}

# Prístup k hodnote mena
joe_info.meno #=> "Joe"

# Zmena hodnoty veku
starsi_joe_info = %{ joe_info | vek: 31 }
#=> %Osoba{vek: 31, vyska: 180, meno: "Joe"}

# `try` blok s kľúčovým slovom `rescue` sa používa na riadenie výnimiek
try do
  raise "nejaký error"
rescue
  RuntimeError -> "zachytí runtime error"
  _error -> "zachytí ľubovoľný iný error"
end
#=> "zachytí runtime error"

# Každá výnimka má správu
try do
  raise "nejaký error"
rescue
  x in [RuntimeError] ->
    x.message
end
#=> "nejaký error"

## ---------------------------
## -- Konkurencia
## ---------------------------

# Elixir sa pri konkurencii spolieha na Actor model. Všetko čo je
# potrebné na písanie konkuretných programov v elixire sú tri primitívy:
# spawning procesy, posielanie a prijímanie správ.

# Na spustnenie nového procesu použijeme `spawn` funkciu, ktorá má ako
# parameter funkciu.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` vracia pid (identifikátor procesu), tento pid možno použiť na
# posielanie správ procesu. Správu pošleme `send` operatorátorom.
# Aby všetko fungovalo ako má, potrebujeme byť schopný správu prijať. To
# dosiahneme s `receive` mechanizmom:

# `receive do` blok sa používa na počúvanie správ a ich spracúvavanie v čase
# prijatia. `receive do` blok spracuje iba jednu prijatú správu. Pre
# spracovanie viacerých správ, musí funkcia s `receive do` blokom rekurzívne
# volať samu seba, aby sa dostala opäť do `receive do` bloku.

defmodule Geometria do
  def slucka_oblasti do
    receive do
      {:obdlznik, w, h} ->
        IO.puts("Oblast = #{w * h}")
        slucka_oblasti()
      {:kruh, r} ->
        IO.puts("Oblast = #{3.14 * r * r}")
        slucka_oblasti()
    end
  end
end

# Kompiluj modul a vytvor proces, ktorý vyhodnotí `slucka_oblasti` v shelli

pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>
# Alternatívne
pid = spawn(Geometria, :slucka_oblasti, [])

# Pošli správu ku `pid`, ktorá bude v zhode so vzorom v receive časti
send pid, {:obdlznik, 2, 3}
#=> Oblast = 6
#   {:obdlznik,2,3}

send pid, {:kruh, 2}
#=> Oblast = 12.56000000000000049738
#   {:kruh,2}

# Shell je takisto proces, môžete použiť `self` pre zistenie aktuálneho pid-u
self() #=> #PID<0.27.0>

## ---------------------------
## -- Agenti
## ---------------------------

# Agent je proces, ktorý udržuje informácie o meniacej sa hodnote

# Vytvor agenta s `Agent.start_link` parametrom, ktorého je funkcia
# Iniciálny stav agenta bude čokoľvek, čo daná funkcia vráti
{ok, moj_agent} = Agent.start_link(fn -> ["cervena, zelena"] end)

# `Agent.get` vezme meno agenta a `fn` , ktorej je odovzdaný aktuálny stav
# Čokoľvek čo `fn` vráti je to, čo dostanete späť
Agent.get(moj_agent, fn farby -> farby end) #=> ["cervena, "zelena"]

# Zmena stavu agenta rovnakým spôsobom
Agent.update(moj_agent, fn farby -> ["modra" | farby] end)
```

## Referencie

* [Začíname](http://elixir-lang.org/getting-started/introduction.html) from the
[Elixir stránky](http://elixir-lang.org)
* [Elixir dokumentácia](http://elixir-lang.org/docs/master/)
* ["Elixir programovanie"](https://pragprog.com/book/elixir/programming-elixir)
 by Dave Thomas
* [Elixir ťahák](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Nauč sa kúsok Erlangu pre veľké dobro!"](http://learnyousomeerlang.com/) by
Fred Hebert
* ["Erlang programovanie: Softvér pre konkurentný svet"](https://pragprog
.com/book/jaerlang2/programming-erlang) by Joe Armstrong
