---
name: Lua
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
translators:
    - ["valerioandreachiodi", "https://github.com/valerioandreachiodi"]
---

```lua
-- Due trattini iniziano un commento su una riga.

--[[
     Aggiungere due [ e ] crea un
     commento multi-riga.
--]]

----------------------------------------------------
-- 1. Variabili e controllo di flusso.
----------------------------------------------------

num = 42  -- I numeri possono essere interi o floating point.

s = 'walternate'  -- Stringhe immutabili come in Python.
t = "anche le virgolette doppie vanno bene"
u = [[ Le doppie parentesi quadre
        iniziano e finiscono
        stringhe multi-riga.]]
t = nil  -- Rende t indefinita; Lua ha il garbage collection.

-- I blocchi sono denotati con parole chiave come do/end:
while num < 50 do
  num = num + 1  -- Non esistono operatori tipo ++ o +=.
end

-- Clausole If:
if num > 40 then
  print('sopra i 40')
elseif s ~= 'walternate' then  -- ~= è "diverso da".
  -- Il controllo di uguaglianza è == come in Python; ok per le stringhe.
  io.write('non sopra i 40\n')  -- Default su stdout.
else
  -- Le variabili sono globali di default.
  thisIsGlobal = 5  -- Il camel case è comune.

  -- Come rendere una variabile locale:
  local line = io.read()  -- Legge la prossima riga da stdin.

  -- La concatenazione di stringhe usa l'operatore .. :
  print('L\'inverno sta arrivando, ' .. line)
end

-- Le variabili indefinite restituiscono nil.
-- Questo non è un errore:
foo = anUnknownVariable  -- Ora foo = nil.

aBoolValue = false

-- Solo nil e false sono falsy; 0 e '' sono true!
if not aBoolValue then print('era falso') end

-- 'or' e 'and' sono a corto circuito.
-- Simile all'operatore a?b:c in C/js:
ans = aBoolValue and 'yes' or 'no'  --> 'no'

karlSum = 0
for i = 1, 100 do  -- Il range include entrambi gli estremi.
  karlSum = karlSum + i
end

-- Usa "100, 1, -1" come range per contare alla rovescia:
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

-- In generale, il range è inizio, fine[, step].

-- Un altro costrutto di ciclo:
repeat
  print('la via del futuro')
  num = num - 1
until num == 0


----------------------------------------------------
-- 2. Funzioni.
----------------------------------------------------

function fib(n)
  if n < 2 then return 1 end
  return fib(n - 2) + fib(n - 1)
end

-- Closure e funzioni anonime vanno bene:
function adder(x)
  -- La funzione restituita viene creata quando adder viene
  -- chiamata, e ricorda il valore di x:
  return function (y) return x + y end
end
a1 = adder(9)
a2 = adder(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- Return, chiamate di funzione e assegnazioni funzionano tutti
-- con liste che possono avere lunghezze diverse.
-- I ricevitori senza corrispondenza sono nil;
-- i mandanti in eccesso vengono scartati.

x, y, z = 1, 2, 3, 4
-- Ora x = 1, y = 2, z = 3, e il 4 viene buttato via.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> stampa "zaphod  nil nil"
-- Ora x = 4, y = 8, i valori 15...42 sono scartati.

-- Le funzioni sono "first-class", possono essere locali/globali.
-- Queste sono la stessa cosa:
function f(x) return x * x end
f = function (x) return x * x end

-- E anche queste:
local function g(x) return math.sin(x) end
local g; g  = function (x) return math.sin(x) end
-- la dichiarazione 'local g' rende possibili i riferimenti a g-stessa.

-- Le funzioni trigonometriche lavorano in radianti, tra l'altro.

-- Le chiamate con un solo parametro stringa non necessitano di parentesi:
print 'hello'  -- Funziona benissimo.


----------------------------------------------------
-- 3. Tabelle (Tables).
----------------------------------------------------

-- Le Tabelle = l'unica struttura dati composta di Lua;
--           sono array associativi.
-- Simili agli array di PHP o agli oggetti JS, sono dizionari
-- con lookup hash che possono essere usati anche come liste.

-- Usare le tabelle come dizionari / mappe:

-- I letterali dei dizionari hanno chiavi stringa di default:
t = {key1 = 'value1', key2 = false}

-- Le chiavi stringa possono usare la notazione a punto tipo JS:
print(t.key1)  -- Stampa 'value1'.
t.newKey = {}  -- Aggiunge una nuova coppia chiave/valore.
t.key2 = nil   -- Rimuove key2 dalla tabella.

-- Notazione letterale per qualsiasi valore (non-nil) come chiave:
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- stampa "tau"

-- La corrispondenza delle chiavi è fondamentalmente per valore per numeri
-- e stringhe, ma per identità per le tabelle.
a = u['@!#']  -- Ora a = 'qbert'.
b = u[{}]     -- Potremmo aspettarci 1729, ma è nil:
-- b = nil perché il lookup fallisce. Fallisce
-- perché la chiave che abbiamo usato non è lo stesso oggetto
-- di quello usato per memorizzare il valore originale. Quindi
-- stringhe e numeri sono chiavi più portabili.

-- Una chiamata a funzione con un parametro tabella non serve parentesi:
function h(x) print(x.key1) end
h{key1 = 'Sonmi~451'}  -- Stampa 'Sonmi~451'.

for key, val in pairs(u) do  -- Iterazione della tabella.
  print(key, val)
end

-- _G è una tabella speciale di tutti i globali.
print(_G['_G'] == _G)  -- Stampa 'true'.

-- Usare le tabelle come liste / array:

-- I letterali lista impostano implicitamente chiavi intere:
v = {'value1', 'value2', 1.21, 'gigawatts'}
for i = 1, #v do  -- #v è la dimensione di v per le liste.
  print(v[i])  -- Gli indici partono da 1 !! FOLLIA!
end
-- Una 'list' non è un vero tipo. v è solo una tabella
-- con chiavi intere consecutive, trattata come una lista.

----------------------------------------------------
-- 3.1 Metatabelle e metametodi.
----------------------------------------------------

-- Una tabella può avere una metatabella che le conferisce un
-- comportamento simile all'overload degli operatori. Vedremo dopo
-- come le metatabelle supportano il comportamento prototipale di JS.

f1 = {a = 1, b = 2}  -- Rappresenta la frazione a/b.
f2 = {a = 2, b = 3}

-- Questo fallirebbe:
-- s = f1 + f2

metafraction = {}
function metafraction.__add(f1, f2)
  sum = {}
  sum.b = f1.b * f2.b
  sum.a = f1.a * f2.b + f2.a * f1.b
  return sum
end

setmetatable(f1, metafraction)
setmetatable(f2, metafraction)

s = f1 + f2  -- chiama __add(f1, f2) sulla metatabella di f1

-- f1, f2 non hanno chiavi per la loro metatabella, a differenza
-- dei prototipi in JS, quindi devi recuperarla come in
-- getmetatable(f1). La metatabella è una normale tabella
-- con chiavi che Lua conosce, come __add.

-- Ma la riga successiva fallisce perché s non ha una metatabella:
-- t = s + s
-- I pattern simili alle classi descritti sotto risolverebbero questo.

-- Un __index su una metatabella sovraccarica i lookup a punto:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- funziona! grazie, metatabella

-- I lookup diretti sulla tabella che falliscono riproveranno usando
-- il valore __index della metatabella, e questo è ricorsivo.

-- Un valore __index può anche essere una funzione(tbl, key)
-- per lookup più personalizzati.

-- I valori di __index, __add, ecc. sono chiamati metametodi.
-- Ecco alcuni tra i più usati:

-- __add(a, b)                     per a + b
-- __sub(a, b)                     per a - b
-- __mul(a, b)                     per a * b
-- __div(a, b)                     per a / b
-- __mod(a, b)                     per a % b
-- __pow(a, b)                     per a ^ b
-- __unm(a)                        per -a
-- __concat(a, b)                  per a .. b
-- __len(a)                        per #a
-- __eq(a, b)                      per a == b
-- __lt(a, b)                      per a < b
-- __le(a, b)                      per a <= b
-- __index(a, b)  <fn o tabella>   per a.b
-- __newindex(a, b, c)             per a.b = c
-- __call(a, ...)                  per a(...)

----------------------------------------------------
-- 3.2 Tabelle "simili a classi" ed ereditarietà.
----------------------------------------------------

-- Le classi non sono integrate; ci sono diversi modi
-- per crearle usando tabelle e metatabelle.

-- La spiegazione per questo esempio è riportata sotto.

Dog = {}                                   -- 1.

function Dog:new()                         -- 2.
  newObj = {sound = 'woof'}                -- 3.
  self.__index = self                      -- 4.
  return setmetatable(newObj, self)        -- 5.
end

function Dog:makeSound()                   -- 6.
  print('Io dico ' .. self.sound)
end

mrDog = Dog:new()                          -- 7.
mrDog:makeSound()  -- 'Io dico woof'       -- 8.

-- 1. Dog agisce come una classe; in realtà è una tabella.
-- 2. function tablename:fn(...) è uguale a
--    function tablename.fn(self, ...)
--    I : aggiungono solo un primo argomento chiamato self.
--    Leggi 7 e 8 sotto per come self ottiene il suo valore.
-- 3. newObj sarà un'istanza della classe Dog.
-- 4. self = la classe che viene istanziata. Spesso
--    self = Dog, ma l'ereditarietà può cambiarlo.
--    newObj ottiene le funzioni di self quando impostiamo sia
--    la metatabella di newObj che l'__index di self su self.
-- 5. Promemoria: setmetatable restituisce il suo primo argomento.
-- 6. I : funzionano come nel punto 2, ma stavolta ci aspettiamo
--    che self sia un'istanza invece di una classe.
-- 7. Uguale a Dog.new(Dog), quindi self = Dog in new().
-- 8. Uguale a mrDog.makeSound(mrDog); self = mrDog.

----------------------------------------------------

-- Esempio di ereditarietà:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  s = self.sound .. ' '                       -- 2.
  print(s .. s .. s)
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof'      -- 4.

-- 1. LoudDog ottiene i metodi e le variabili di Dog.
-- 2. self ha una chiave 'sound' da new(), vedi punto 3.
-- 3. Uguale a LoudDog.new(LoudDog), e convertito in
--    Dog.new(LoudDog) poiché LoudDog non ha una chiave 'new',
--    ma ha __index = Dog sulla sua metatabella.
--    Risultato: la metatabella di seymour è LoudDog, e
--    LoudDog.__index = LoudDog. Quindi seymour.chiave sarà
--    = seymour.chiave, LoudDog.chiave, Dog.chiave, a seconda di quale
--    tabella sia la prima a possedere la chiave data.
-- 4. La chiave 'makeSound' viene trovata in LoudDog; questo
--    è uguale a LoudDog.makeSound(seymour).

-- Se necessario, il new() di una sottoclasse è come quello della base:
function LoudDog:new()
  newObj = {}
  -- imposta newObj
  self.__index = self
  return setmetatable(newObj, self)
end

----------------------------------------------------
-- 4. Moduli.
----------------------------------------------------


--[[ Commento questa sezione in modo che il resto
--   di questo script rimanga eseguibile.
```

```lua
-- Supponiamo che il file mod.lua sia così:
local M = {}

local function sayMyName()
  print('Hrunkner')
end

function M.sayHello()
  print('Perbacco, ciao')
  sayMyName()
end

return M

-- Un altro file può usare le funzionalità di mod.lua:
local mod = require('mod')  -- Esegue il file mod.lua.

-- require è il modo standard per includere moduli.
-- require agisce come:     (se non in cache; vedi sotto)
local mod = (function ()
  <contenuti di mod.lua>
end)()
-- È come se mod.lua fosse il corpo di una funzione, così che
-- le variabili locali dentro mod.lua sono invisibili all'esterno.

-- Questo funziona perché mod qui = M in mod.lua:
mod.sayHello() -- Stampa: Perbacco, ciao Hrunkner

-- Questo è sbagliato; sayMyName esiste solo in mod.lua:
mod.sayMyName()  -- errore

-- I valori di ritorno di require sono messi in cache così un file
-- viene eseguito al massimo una volta, anche se richiesto più volte.

-- Supponiamo che mod2.lua contenga "print('Hi!')".
local a = require('mod2')  -- Stampa Hi!
local b = require('mod2')  -- Non stampa; a=b.

-- dofile è come require senza cache:
dofile('mod2.lua')  --> Hi!
dofile('mod2.lua')  --> Hi! (lo esegue di nuovo)

-- loadfile carica un file lua ma non lo esegue ancora.
f = loadfile('mod2.lua')  -- Chiama f() per eseguirlo.

-- load è loadfile per le stringhe.
-- (loadstring è deprecato, usa load al suo posto)
g = load('print(343)')  -- Restituisce una funzione.
g()  -- Stampa 343; nulla era stato stampato prima di ora.

--]]
```

## Comunità

Se hai bisogno di supporto unisciti alla [mailing list ufficiale di Lua](https://www.lua.org/lua-l.html), al [canale IRC](http://lua-users.org/wiki/IrcChannel), o al [forum](https://luaforum.com).

## Riferimento in italiano

Ho trovato questo [Lua manuale italiano PDF](https://www.vittal.it/wp-content/uploads/2023/03/lua.pdf) che puo essere utile.

## Riferimenti originali

Ero entusiasta di imparare Lua per poter creare giochi
con il [motore grafico LÖVE](http://love2d.org/). Questo è il "perché".

Ho iniziato con [Lua for programmers di BlackBulletIV](https://ebens.me/posts/lua-for-programmers-part-1/).
Poi ho letto il libro ufficiale [Programming in Lua](http://www.lua.org/pil/contents.html).
Questo è il "come".

Potrebbe essere utile dare un'occhiata alla [Lua short reference](http://lua-users.org/wiki/LuaShortReference) su lua-users.org.

Gli argomenti principali non trattati sono le librerie standard:

* [`string` library](https://www.google.com/search?q=%5Bhttp://lua-users.org/wiki/StringLibraryTutorial%5D(http://lua-users.org/wiki/StringLibraryTutorial))
* [`table` library](https://www.google.com/search?q=%5Bhttp://lua-users.org/wiki/TableLibraryTutorial%5D(http://lua-users.org/wiki/TableLibraryTutorial))
* [`math` library](https://www.google.com/search?q=%5Bhttp://lua-users.org/wiki/MathLibraryTutorial%5D(http://lua-users.org/wiki/MathLibraryTutorial))
* [`io` library](https://www.google.com/search?q=%5Bhttp://lua-users.org/wiki/IoLibraryTutorial%5D(http://lua-users.org/wiki/IoLibraryTutorial))
* [`os` library](https://www.google.com/search?q=%5Bhttp://lua-users.org/wiki/OsLibraryTutorial%5D(http://lua-users.org/wiki/OsLibraryTutorial))

Tra l'altro, l'intero file è codice Lua valido; salvalo
come learn.lua ed eseguilo con "`lua learn.lua`" !

Questo è stato scritto originariamente per tylerneylon.com, ed è
disponibile anche come [GitHub gist](https://gist.github.com/tylerneylon/5853042). Buon divertimento con Lua!

---
