---
language: elixir
contributors:
    - ["Luca 'Kino' Maroni", "https://github.com/kino90"]
    - ["Joao Marques", "http://github.com/mrshankly"]
    - ["Dzianis Dashkevich", "https://github.com/dskecse"]
translators:
    - ["Tommaso Pifferi","http://github.com/neslinesli93"]
filename: learnelixir-it.ex
lang: it-it
---

Elixir è un linguaggio funzionale moderno, costruito sulla VM Erlang.
È totalmente compatibile con Erlang, ma con una sintassi più standard
e molte altre funzionalità.

```elixir

# I commenti su una riga iniziano con un cancelletto.

# Non esistono commenti multilinea,
# ma puoi concatenare più commenti.

# Per usare la shell di elixir usa il comando `iex`.
# Compila i tuoi moduli con il comando `elixirc`.

# Entrambi i comandi dovrebbero già essere nel tuo PATH se hai installato
# elixir correttamente.

## ---------------------------
## -- Tipi di base
## ---------------------------

# Numeri
3    # intero (Integer)
0x1F # intero
3.0  # decimale (Float)

# Atomi, che sono literals, una costante con un nome. Iniziano con `:`.
:ciao # atomo (Atom)

# Tuple che sono salvate in celle di memoria contigue.
{1,2,3} # tupla (Tuple)

# Possiamo accedere ad un elemento di una tupla con la funzione `elem`:
elem({1, 2, 3}, 0) #=> 1

# Liste, che sono implementate come liste concatenate (o linked list).
[1,2,3] # lista (List)

# Possiamo accedere alla testa (head) e alla coda (tail) delle liste così:
[testa | coda] = [1,2,3]
testa #=> 1
coda #=> [2,3]

# In Elixir, proprio come in Erlang, il simbolo `=` denota pattern matching e
# non un assegnamento.
#
# Questo significa che la parte sinistra (pattern) viene confrontata alla
# parte destra.
#
# Questo spiega il funzionamento dell'esempio dell'accesso alla lista di prima.

# Un pattern match darà errore quando le parti non combaciano, ad esempio se
# le tuple hanno dimensione differente.
# {a, b, c} = {1, 2} #=> ** (MatchError) no match of right hand side value: {1,2}

# Ci sono anche i binari
<<1,2,3>> # binari (Binary)

# Stringhe e liste di caratteri
"ciao" # stringa (String)
'ciao' # lista di caratteri (List)

# Stringhe multilinea
"""
Sono una stringa
multi-linea.
"""
#=> "Sono una stringa\nmulti-linea.\n"

# Le stringhe sono tutte codificate in UTF-8:
"cìaò"
#=> "cìaò"

# le stringhe in realtà sono dei binari, e le liste di caratteri sono liste.
<<?a, ?b, ?c>> #=> "abc"
[?a, ?b, ?c]   #=> 'abc'

# `?a` in elixir restituisce il valore ASCII della lettera `a`
?a #=> 97

# Per concatenare liste si usa `++`, per binari si usa `<>`
[1,2,3] ++ [4,5]     #=> [1,2,3,4,5]
'ciao ' ++ 'mondo'  #=> 'ciao mondo'

<<1,2,3>> <> <<4,5>> #=> <<1,2,3,4,5>>
"ciao " <> "mondo"  #=> "ciao mondo"

# Gli intervalli sono rappresentati come `inizio..fine` (estremi inclusi)
1..10 #=> 1..10 (Range)
minore..maggiore = 1..10 # Puoi fare pattern matching anche sugli intervalli
[minore, maggiore] #=> [1, 10]

## ---------------------------
## -- Operatori
## ---------------------------

# Un po' di matematica
1 + 1  #=> 2
10 - 5 #=> 5
5 * 2  #=> 10
10 / 2 #=> 5.0

# In elixir l'operatore `/` restituisce sempre un decimale.

# Per fare una divisione intera si usa `div`
div(10, 2) #=> 5

# Per ottenere il resto di una divisione si usa `rem`
rem(10, 3) #=> 1

# Ci sono anche gli operatori booleani: `or`, `and` e `not`.
# Questi operatori si aspettano un booleano come primo argomento.
true and true #=> true
false or true #=> true
# 1 and true
#=> ** (BadBooleanError) expected a boolean on left-side of "and", got: 1

# Elixir fornisce anche `||`, `&&` e `!` che accettano argomenti
# di qualsiasi tipo.
# Tutti i valori tranne `false` e `nil` saranno valutati come true.
1 || true  #=> 1
false && 1 #=> false
nil && 20  #=> nil
!true #=> false

# Per i confronti abbiamo: `==`, `!=`, `===`, `!==`, `<=`, `>=`, `<` e `>`
1 == 1 #=> true
1 != 1 #=> false
1 < 2  #=> true

# `===` e `!==` sono più rigidi quando si confrontano interi e decimali:
1 == 1.0  #=> true
1 === 1.0 #=> false

# Possiamo anche confrontare tipi di dato diversi:
1 < :ciao #=> true

# L'ordine generale è definito sotto:
# numeri < atomi < riferimenti < funzioni < porte < pid < tuple < liste
#   < stringhe di bit

# Per citare Joe Armstrong su questo: "L'ordine non è importante,
# ma è importante che sia definito un ordine."

## ---------------------------
## -- Controllo di flusso
## ---------------------------

# espressione `se` (`if`)
if false do
  "Questo non si vedrà mai"
else
  "Questo sì"
end

# c'è anche un `se non` (`unless`)
unless true do
  "Questo non si vedrà mai"
else
  "Questo sì"
end

# Ti ricordi il pattern matching?
# Moltre strutture di controllo di flusso in elixir si basano su di esso.

# `case` ci permette di confrontare un valore a diversi pattern:
case {:uno, :due} do
  {:quattro, :cinque} ->
    "Questo non farà match"
  {:uno, x} ->
    "Questo farà match e binderà `x` a `:due`"
  _ ->
    "Questo farà match con qualsiasi valore"
end

# Solitamente si usa `_` se non si ha bisogno di utilizzare un valore.
# Ad esempio, se ci serve solo la testa di una lista:
[testa | _] = [1,2,3]
testa #=> 1

# Per aumentare la leggibilità possiamo usarlo in questo modo:
[testa | _coda] = [:a, :b, :c]
testa #=> :a

# `cond` ci permette di verificare più condizioni allo stesso momento.
# Usa `cond` invece di innestare più espressioni `if`.
cond do
  1 + 1 == 3 ->
    "Questa stringa non si vedrà mai"
  2 * 5 == 12 ->
    "Nemmeno questa"
  1 + 2 == 3 ->
    "Questa sì!"
end

# È pratica comune mettere l'ultima condizione a `true`, che farà sempre match
cond do
  1 + 1 == 3 ->
    "Questa stringa non si vedrà mai"
  2 * 5 == 12 ->
    "Nemmeno questa"
  true ->
    "Questa sì! (essenzialmente funziona come un else)"
end

# `try/catch` si usa per gestire i valori lanciati (throw),
# Supporta anche una clausola `after` che è invocata in ogni caso.
try do
  throw(:ciao)
catch
  message -> "Ho ricevuto #{message}."
after
  IO.puts("Io sono la clausola 'after'.")
end
#=> Io sono la clausola 'after'
# "Ho ricevuto :ciao"

## ---------------------------
## -- Moduli e Funzioni
## ---------------------------

# Funzioni anonime (notare il punto)
quadrato = fn(x) -> x * x end
quadrato.(5) #=> 25

# Accettano anche guardie e condizioni multiple.
# le guardie ti permettono di perfezionare il tuo pattern matching,
# sono indicate dalla parola chiave `when`:
f = fn
  x, y when x > 0 -> x + y
  x, y -> x * y
end

f.(1, 3)  #=> 4
f.(-1, 3) #=> -3

# Elixir fornisce anche molte funzioni, disponibili nello scope corrente.
is_number(10)    #=> true
is_list("ciao") #=> false
elem({1,2,3}, 0) #=> 1

# Puoi raggruppare delle funzioni all'interno di un modulo.
# All'interno di un modulo usa `def` per definire le tue funzioni.
defmodule Matematica do
  def somma(a, b) do
    a + b
  end

  def quadrato(x) do
    x * x
  end
end

Matematica.somma(1, 2)  #=> 3
Matematica.quadrato(3) #=> 9

# Per compilare il modulo 'Matematica' salvalo come `matematica.ex` e usa
# `elixirc`.
# nel tuo terminale: elixirc matematica.ex

# All'interno di un modulo possiamo definire le funzioni con `def` e funzioni
# private con `defp`.
# Una funzione definita con `def` è disponibile per essere invocata anche da
# altri moduli, una funziona privata può essere invocata solo localmente.
defmodule MatematicaPrivata do
  def somma(a, b) do
    esegui_somma(a, b)
  end

  defp esegui_somma(a, b) do
    a + b
  end
end

MatematicaPrivata.somma(1, 2)    #=> 3
# MatematicaPrivata.esegui_somma(1, 2) #=> ** (UndefinedFunctionError)

# Anche le dichiarazioni di funzione supportano guardie e condizioni multiple.
# Quando viene chiamata una funzione dichiarata con più match, solo la prima
# che matcha viene effettivamente invocata.
# Ad esempio: chiamando area({:cerchio, 3}) vedrà invocata la seconda definizione
# di area mostrata sotto, non la prima:
defmodule Geometria do
  def area({:rettangolo, w, h}) do
    w * h
  end

  def area({:cerchio, r}) when is_number(r) do
    3.14 * r * r
  end
end

Geometria.area({:rettangolo, 2, 3}) #=> 6
Geometria.area({:cerchio, 3})       #=> 28.25999999999999801048
# Geometria.area({:cerchio, "non_un_numero"})
#=> ** (FunctionClauseError) no function clause matching in Geometria.area/1

# A causa dell'immutabilità dei dati, la ricorsione è molto frequente in elixir
defmodule Ricorsione do
  def somma_lista([testa | coda], accumulatore) do
    somma_lista(coda, accumulatore + testa)
  end

  def somma_lista([], accumulatore) do
    accumulatore
  end
end

Ricorsione.somma_lista([1,2,3], 0) #=> 6

# I moduli di Elixir supportano attributi. Ci sono degli attributi incorporati
# e puoi anche aggiungerne di personalizzati.
defmodule Modulo do
  @moduledoc """
  Questo è un attributo incorporato in un modulo di esempio.
  """

  @miei_dati 100 # Questo è un attributo personalizzato.
  IO.inspect(@miei_dati) #=> 100
end

# L'operatore pipe |> permette di passare l'output di una espressione
# come primo parametro di una funzione.
# Questo facilita operazioni quali pipeline di operazioni, composizione di
# funzioni, ecc.
Range.new(1,10)
|> Enum.map(fn x -> x * x end)
|> Enum.filter(fn x -> rem(x, 2) == 0 end)
#=> [4, 16, 36, 64, 100]

## ---------------------------
## -- Strutture ed Eccezioni
## ---------------------------


# Le Strutture (Structs) sono estensioni alle mappe che portano
# valori di default, garanzia alla compilazione e polimorfismo in Elixir.
defmodule Persona do
  defstruct nome: nil, eta: 0, altezza: 0
end

luca = %Persona{ nome: "Luca", eta: 24, altezza: 185 }
#=> %Persona{eta: 24, altezza: 185, nome: "Luca"}

# Legge al valore di 'nome'
luca.nome #=> "Luca"

# Modifica il valore di eta
luca_invecchiato = %{ luca | eta: 25 }
#=> %Persona{eta: 25, altezza: 185, nome: "Luca"}

# Il blocco `try` con la parola chiave `rescue` è usato per gestire le eccezioni
try do
  raise "un errore"
rescue
  RuntimeError -> "Salvato un errore di Runtime"
  _error -> "Questo salverà da qualsiasi errore"
end

# Tutte le eccezioni hanno un messaggio
try do
  raise "un errore"
rescue
  x in [RuntimeError] ->
    x.message
end

## ---------------------------
## -- Concorrenza
## ---------------------------

# Elixir si basa sul modello degli attori per la concorrenza.
# Tutto ciò di cui abbiamo bisogno per scrivere programmi concorrenti in elixir
# sono tre primitive: creare processi, inviare messaggi e ricevere messaggi.

# Per creare un nuovo processo si usa la funzione `spawn`, che riceve una
# funzione come argomento.
f = fn -> 2 * 2 end #=> #Function<erl_eval.20.80484245>
spawn(f) #=> #PID<0.40.0>

# `spawn` restituisce un pid (identificatore di processo). Puoi usare questo
# pid per inviare messaggi al processo.
# Per passare messaggi si usa l'operatore `send`.
# Perché tutto questo sia utile dobbiamo essere capaci di ricevere messaggi,
# oltre ad inviarli. Questo è realizzabile con `receive`:

# Il blocco `receive do` viene usato per mettersi in ascolto di messaggi
# ed elaborarli quando vengono ricevuti. Un blocco `receive do` elabora
# un solo messaggio ricevuto: per fare elaborazione multipla di messaggi,
# una funzione con un blocco `receive do` al suo intero dovrà chiamare
# ricorsivamente sé stessa per entrare di nuovo nel blocco `receive do`.
defmodule Geometria do
  def calcolo_area do
    receive do
      {:rettangolo, w, h} ->
        IO.puts("Area = #{w * h}")
        calcolo_area()
      {:cerchio, r} ->
        IO.puts("Area = #{3.14 * r * r}")
        calcolo_area()
    end
  end
end

# Compila il modulo e crea un processo che esegue `calcolo_area` nella shell
pid = spawn(fn -> Geometria.calcolo_area() end) #=> #PID<0.40.0>
# Alternativamente
pid = spawn(Geometria, :calcolo_area, [])

# Invia un messaggio a `pid` che farà match su un pattern nel blocco in receive
send pid, {:rettangolo, 2, 3}
#=> Area = 6
#   {:rettangolo,2,3}

send pid, {:cerchio, 2}
#=> Area = 12.56000000000000049738
#   {:cerchio,2}

# Anche la shell è un processo. Puoi usare `self` per ottenere il pid corrente
self() #=> #PID<0.27.0>
```

## Referenze

* [Getting started guide](http://elixir-lang.org/getting_started/1.html) dalla [pagina web ufficiale di elixir](http://elixir-lang.org)
* [Documentazione Elixir](https://elixir-lang.org/docs.html)
* ["Programming Elixir"](https://pragprog.com/book/elixir/programming-elixir) di Dave Thomas
* [Elixir Cheat Sheet](http://media.pragprog.com/titles/elixir/ElixirCheat.pdf)
* ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) di Fred Hebert
* ["Programming Erlang: Software for a Concurrent World"](https://pragprog.com/book/jaerlang2/programming-erlang) di Joe Armstrong
