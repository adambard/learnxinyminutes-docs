---
language: elixir
contributors:
    - ["Joao Marques", "http://github.com/mrshankly"]
translators:
    - ["Gregor Große-Bölting", "http://www.ideen-und-soehne.de"]
filename: learnelixir-de.ex
lang: de-de
---

Elixir ist eine moderne, funktionale Sprache für die Erlang VM. Sie ist voll 
kompatibel mit Erlang, verfügt aber über eine freundlichere Syntax und bringt 
viele Features mit. 

```ruby

# Einzeilige Kommentare werden mit der Raute gesetzt.

# Es gibt keine mehrzeiligen Kommentare;
# es ist aber problemlos möglich mehrere einzeilige Kommentare hintereinander 
# zu setzen (so wie hier).

# Mit 'iex' ruft man die Elixir-Shell auf.
# Zum kompilieren von Modulen dient der Befehl 'elixirc'.

# Beide Befehle sollten als Umgebungsvariable gesetzt sein, wenn Elixir korrekt
# installiert wurde.

## ---------------------------
## -- Basistypen
## ---------------------------

# Es gibt Nummern:
3    # Integer
0x1F # Integer
3.0  # Float

# Atome, das sind Literale, sind Konstanten mit Namen. Sie starten mit einem 
# ':'.
:hello # Atom

# Außerdem gibt es Tupel, deren Werte im Arbeitsspeicher vorgehalten werden.
{1,2,3} # Tupel

# Die Werte innerhalb eines Tupels können mit der 'elem'-Funktion ausgelesen
# werden:
elem({1, 2, 3}, 0) # => 1

# Listen sind als verkettete Listen implementiert.
[1, 2, 3] # list

# Auf Kopf und Rest einer Liste kann wie folgt zugegriffen werden:
[ kopf | rest ] = [1,2,3]
kopf # => 1
rest # => [2, 3]

# In Elixir, wie auch in Erlang, kennzeichnet '=' ein 'pattern matching'
# (Musterabgleich) und keine Zuweisung.
# Das heißt, dass die linke Seite auf die rechte Seite 'abgeglichen' wird.
# Auf diese Weise kann im Beispiel oben auf Kopf und Rest der Liste zugegriffen
# werden.

# Ein Musterabgleich wird einen Fehler werfen, wenn die beiden Seiten nicht 
# zusammenpassen. 
# Im folgenden Beispiel haben die Tupel eine unterschiedliche Anzahl an
# Elementen:
{a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# Es gibt außerdem 'binaries', 
<<1,2,3>> # binary.

# Strings und 'char lists'
"hello" # String
'hello' # Char-Liste

# ... und mehrzeilige Strings
"""
Ich bin ein 
mehrzeiliger String.
"""
#=> "Ich bin ein\nmehrzeiliger String.\n"

# Alles Strings werden in UTF-8 enkodiert:
"héllò" #=> "héllò"

# Eigentlich sind Strings in Wahrheit nur binaries und 'char lists' einfach
# Listen.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# In Elixir gibt `?a` den ASCII-Integer für den Buchstaben zurück.
?a #=> 97

# Um Listen zu verbinden gibt es den Operator '++', für binaries nutzt man '<>'
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'hello ' ++ 'world'  #=> 'hello world'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"hello " <> "world"  #=> "hello world"

## ---------------------------
## -- Operatoren
## ---------------------------

# Einfache Arithmetik
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# In Elixir gibt der Operator '/' immer einen Float-Wert zurück.

# Für Division mit ganzzahligen Ergebnis gibt es 'div'
div(10, 2) #=> 5

# Um den Rest der ganzzahligen Division zu erhalten gibt es 'rem'
rem(10, 3) #=> 1

# Natürlich gibt es auch Operatoren für Booleans: 'or', 'and' und 'not'. Diese
# Operatoren erwarten einen Boolean als erstes Argument. 
true and true #=> true
false or true #=> true
# 1 and true    #=> ** (ArgumentError) argument error

# Elixir bietet auch '||', '&&' und '!', die Argumente jedweden Typs 
# akzeptieren. Alle Werte außer 'false' und 'nil' werden zu wahr evaluiert.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil

!true #=> false

# Für Vergleiche gibt es die Operatoren `==`, `!=`, `===`, `!==`, `<=`, `>=`,
# `<` und `>` 
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# '===' und '!==' sind strikter beim Vergleich von Integern und Floats:
1 == 1.0  #=> true
1 === 1.0 #=> false

# Es ist außerdem möglich zwei verschiedene Datentypen zu vergleichen:
1 < :hello #=> true

# Die gesamte Ordnung über die Datentypen ist wie folgt definiert:
# number < atom < reference < functions < port < pid < tuple < list < bitstring

# Um Joe Armstrong zu zitieren: "The actual order is not important, but that a
# total ordering is well defined is important." 

## ---------------------------
## -- Kontrollstrukturen
## ---------------------------

# Es gibt die `if`-Verzweigung
if false do
  "Dies wird nie jemand sehen..."
else
  "...aber dies!"
end

# ...und ebenso `unless`
unless true do
  "Dies wird nie jemand sehen..."
else
  "...aber dies!"
end

# Du erinnerst dich an 'pattern matching'? Viele Kontrollstrukturen in Elixir
# arbeiten damit.

# 'case' erlaubt es uns Werte mit vielerlei Mustern zu vergleichen.
case {:one, :two} do
  {:four, :five} ->
    "Das wird nicht passen"
  {:one, x} ->
    "Das schon und außerdem wird es ':two' dem Wert 'x' zuweisen."
  _ ->
    "Dieser Fall greift immer."
end

# Es ist eine übliche Praxis '_' einen Wert zuzuweisen, sofern dieser Wert
# nicht weiter verwendet wird.
# Wenn wir uns zum Beispiel nur für den Kopf einer Liste interessieren:
[kopf | _] = [1,2,3]
kopf #=> 1

# Für bessere Lesbarkeit können wir auch das Folgende machen:
[kopf | _rest] = [:a, :b, :c]
kopf #=> :a

# Mit 'cond' können diverse Bedingungen zur selben Zeit überprüft werden. Man
# benutzt 'cond' statt viele if-Verzweigungen zu verschachteln.
cond do
  1 + 1 == 3 ->
    "Ich werde nie aufgerufen."
  2 * 5 == 12 ->
    "Ich auch nicht."
  1 + 2 == 3 ->
    "Aber ich!"
end

# Es ist üblich eine letzte Bedingung einzufügen, die immer zu wahr evaluiert. 
cond do
  1 + 1 == 3 ->
    "Ich werde nie aufgerufen."
  2 * 5 == 12 ->
    "Ich auch nicht."
  true ->
    "Aber ich! (dies ist im Grunde ein 'else')"
end

# 'try/catch' wird verwendet um Werte zu fangen, die zuvor 'geworfen' wurden.
# Das Konstrukt unterstützt außerdem eine 'after'-Klausel die aufgerufen wird,
# egal ob zuvor ein Wert gefangen wurde.
try do
  throw(:hello)
catch
  nachricht -> "#{nachricht} gefangen."
after
  IO.puts("Ich bin die 'after'-Klausel.")
end
#=> Ich bin die 'after'-Klausel.
# ":hello gefangen"

## ---------------------------
## -- Module und Funktionen
## ---------------------------

# Anonyme Funktionen (man beachte den Punkt)
square = fn(x) -> x * x end
square.(5) #=> 25

# Anonyme Funktionen unterstützen auch 'pattern' und 'guards'. Guards erlauben
# es die Mustererkennung zu justieren und werden mit dem Schlüsselwort 'when'
# eingeführt:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir bietet zahlreiche eingebaute Funktionen. Diese sind im gleichen
# Geltungsbereich ('scope') verfügbar.
is_number(10)    #=> true
is_list("hello") #=> false
elem({1,2,3}, 0) #=> 1

# Mehrere Funktionen können in einem Modul gruppiert werden. Innerhalb eines
# Moduls ist es möglich mit dem Schlüsselwort 'def' eine Funktion zu
# definieren.
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

# Um unser einfaches Mathe-Modul zu kompilieren muss es unter 'math.ex'
# gesichert werden. Anschließend kann es mit 'elixirc' im Terminal aufgerufen
# werden: elixirc math.ex

# Innerhalb eines Moduls definieren wir private Funktionen mit 'defp'. Eine
# Funktion, die mit 'def' erstellt wurde, kann von anderen Modulen aufgerufen
# werden; eine private Funktion kann nur lokal angesprochen werden.
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

# Auch Funktionsdeklarationen unterstützen 'guards' und Mustererkennung:
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

# Wegen der Unveränderlichkeit von Variablen ist Rekursion ein wichtiger
# Bestandteil von Elixir.
defmodule Recursion do
  def sum_list([head | tail], acc) do
    sum_list(tail, acc + head)
  end

  def sum_list([], acc) do
    acc
  end
end

Recursion.sum_list([1,2,3], 0) #=> 6

# Elixir-Module unterstützen Attribute. Es gibt eingebaute Attribute, ebenso
# ist es möglich eigene Attribute hinzuzufügen.
defmodule MyMod do
  @moduledoc """
  Dies ist ein eingebautes Attribut in einem Beispiel-Modul
  """

  @my_data 100 # Dies ist ein selbst-definiertes Attribut.
  IO.inspect(@my_data) #=> 100
end

## ---------------------------
## -- 'Records' und Ausnahmebehandlung
## ---------------------------

# 'Records' sind im Grunde Strukturen, die es erlauben einem Wert einen eigenen
# Namen zuzuweisen.
defrecord Person, name: nil, age: 0, height: 0

joe_info = Person.new(name: "Joe", age: 30, height: 180)
#=> Person[name: "Joe", age: 30, height: 180]

# Zugriff auf den Wert von 'name'
joe_info.name #=> "Joe"

# Den Wert von 'age' überschreiben
joe_info = joe_info.age(31) #=> Person[name: "Joe", age: 31, height: 180]

# Der 'try'-Block wird zusammen mit dem 'rescue'-Schlüsselwort dazu verwendet,
# um Ausnahmen beziehungsweise Fehler zu behandeln. 
try do
  raise "Irgendein Fehler."
rescue
  RuntimeError -> "Laufzeit-Fehler gefangen."
  _error -> "Und dies fängt jeden Fehler."
end

# Alle Ausnahmen haben das Attribut 'message'
try do
  raise "ein Fehler"
rescue
  x in [RuntimeError] ->
    x.message
end

## ---------------------------
## -- Nebenläufigkeit
## ---------------------------

# Elixir beruht auf dem Aktoren-Model zur Behandlung der Nebenläufigkeit. Alles
# was man braucht um in Elixir nebenläufige Programme zu schreiben sind drei
# Primitive: Prozesse erzeugen, Nachrichten senden und Nachrichten empfangen. 

# Um einen neuen Prozess zu erzeugen nutzen wir die 'spawn'-Funktion, die
# wiederum eine Funktion als Argument entgegen nimmt.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# 'spawn' gibt eine pid (einen Identifikator des Prozesses) zurück. Diese kann
# nun verwendet werden, um Nachrichten an den Prozess zu senden. Um 
# zu senden nutzen wir den '<-' Operator. Damit das alles Sinn macht müssen wir
# in der Lage sein Nachrichten zu empfangen. Dies wird mit dem 
# 'receive'-Mechanismus sichergestellt:
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

# Kompiliere das Modul, starte einen Prozess und gib die 'area_loop' Funktion
# in der Shell mit, etwa so:
pid = spawn(fn -> Geometry.area_loop() end) #=> #PID<0.40.0>

# Sende eine Nachricht an die 'pid', die ein Muster im 'receive'-Ausdruck
# erfüllt:
pid <- {:rectangle, 2, 3}
#=> Area = 6
#   {:rectangle,2,3}

pid <- {:circle, 2}
#=> Area = 12.56000000000000049738
#   {:circle,2}

# Die Shell selbst ist ein Prozess und mit dem Schlüsselwort 'self' kann man
# die aktuelle pid herausfinden.
self() #=> #PID<0.27.0>

```

## Referenzen und weitere Lektüre

* [Getting started guide](http://elixir-lang.org/getting_started/1.html) auf der [elixir Website](http://elixir-lang.org)
* [Elixir Documentation](http://elixir-lang.org/docs/master/)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) von Fred Hebert
* "Programming Erlang: Software for a Concurrent World" von Joe Armstrong
