---
language: Lua
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
translators:
    - ["Martin Schimandl", "https://github.com/Git-Jiro"]
filename: learnlua-de.lua
lang: de-de
---

```lua
-- Zwei Gedankenstriche starten ein einzeiliges Kommentar.

--[[
     Fügt man zwei '[' und ']' hinzu,
     erzeugt man einen mehrzeiligen Kommentar.
--]]
--------------------------------------------------------------------------------
-- 1. Variablen und Fluß-Kontrolle.
--------------------------------------------------------------------------------

num = 42  -- Alle Nummern sind vom Typ: Double.
-- Werd nicht nervös, 64-Bit Double haben 52 Bits zum Speichern von exakten
-- Ganzzahlen; Maschinen-Genauigkeit ist kein Problem für Ganzzahlen kleiner als 
-- 52 Bit.

s = 'walternate'  -- Zeichenketten sind unveränderlich, wie bei Python.
t = "Doppelte Anführungszeichen sind auch OK"
u = [[ Doppelte eckige Klammern
       beginnen und beenden
       mehrzeilige Zeichenketten.]]
t = nil  -- Undefineren von t; Lua hat einen Garbage Collection.

-- Blöcke werden durch Schlüsselwörter wie do/end markiert:
while num < 50 do
  num = num + 1  -- Es gibt Keine Operatoren wie ++ oder +=
end

-- If Bedingungen:
if num > 40 then
  print('over 40')
elseif s ~= 'walternate' then  -- ~= bedeutet ungleich
  -- Gleichheits-Check == wie bei Python; OK für Zeichenketten.
  io.write('not over 40\n')  -- Standard ist stdout.
else
  -- Variablen sind standardmäßig global.
  thisIsGlobal = 5  -- Camel case ist üblich.

  -- So macht man eine Variable lokal:
  local line = io.read()  -- Lies die nächste Zeile von stdin.

  -- Zeichenketten zusammenführen mit dem .. Operator:
  print('Winter is coming, ' .. line)
end

-- Undefinierte Variablen geben nil zurück.
-- Das ist kein Fehler:
foo = anUnknownVariable  -- Nun ist foo = nil.

aBoolValue = false

-- Nur nil und false sind unwahr; 0 and '' sind wahr!
if not aBoolValue then print('was false') end

-- 'or' und 'and' sind "kurz-geschlossen". Das ist so ähnlich wie der a?b:c
-- operator in C/js:
-- in C/js:
ans = aBoolValue and 'yes' or 'no'  --> 'no'

karlSum = 0
for i = 1, 100 do  -- Ein Bereich inkludiert beide Enden.
  karlSum = karlSum + i
end

-- Verwende "100, 1, -1" als Breich für Countdowns:
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

-- Im Allgemeinen besteht ein Bereich aus: Anfang, Ende, [, Schrittweite].

-- Ein anderes Schleifen-Konstrukt:
repeat
  print('Der Weg der Zukunft')
  num = num - 1
until num == 0

--------------------------------------------------------------------------------
-- 2. Funktionen.
--------------------------------------------------------------------------------

function fib(n)
  if n < 2 then return n end
  return fib(n - 2) + fib(n - 1)
end

-- Closures und anonyme Funktionen sind ok:
function adder(x)
  -- Die zurückgegebene Funktion wird erzeugt wenn addr aufgerufen wird und merkt
  -- sich den Wert von x:
  return function (y) return x + y end
end
a1 = adder(9)
a2 = adder(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- Rückgabewerte, Funktions-Aufrufe und Zuweisungen funktionieren alle mit
-- Listen die nicht immer gleich lang sein müssen. Überzählige Empfänger
-- bekommen nil; überzählige Sender werden ignoriert.

x, y, z = 1, 2, 3, 4
-- Nun ist x = 1, y = 2, z = 3, und 4 wird ignoriert.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> prints "zaphod  nil nil"
-- Nun ist x = 4, y = 8, die Werte 15..42 werden ignoriert.

-- Funktionen sind erste Klasse, und können lokal oder global sein.
-- Das ist alles das Gleiche:
function f(x) return x * x end
f = function (x) return x * x end

-- Das auch:
local function g(x) return math.sin(x) end
local g = function(x) return math.sin(x) end
-- Äquivalent zu local function g(x)..., außer das Referenzen auf g im
-- Funktions-Körper nicht wie erwartet funktionieren.
local g; g  = function (x) return math.sin(x) end
-- Die Deklaration 'local g' macht Selbst-Referenzen auf g OK.

-- Nebenbei gesagt, Trigonometrie-Funktionen verwenden Radianten.

-- Funktionsaufrufe mit nur einem Zeichenketten-Parameter brauch keine runden
-- Klammern.
print 'hello'  -- Funktioniert wunderbar.

-- Funktionsaufrufe mit einem Tabellen-Parameter brauchen auch keine runden
-- Klammern. Mehr zu Tabellen kommt später.
print {} -- Funktioniert auch wunderbar.

--------------------------------------------------------------------------------
-- 3. Tabellen.
--------------------------------------------------------------------------------

-- Tabellen sind die einzige zusammengesetzte Struktur in Lua. Sie sind
-- assoziative Arrays. Sie sind so ähnlich wie PHP arrays oder JavaScript
-- Objekte. Sie sind Hash-Lookup-Dictionaries die auch als Listen verwendet
-- werden können.

-- Verwenden von Tabellen als Dictionaries oder Maps:

-- Dict-Literale haben standardmäßig Zeichenketten als Schlüssel:
t = {key1 = 'value1', key2 = false}

-- Zeichenketten-Schlüssel verwenden eine JavaScript ähnliche Punkt-Notation.
print(t.key1)  -- Ausgabe 'value1'.
t.newKey = {}  -- Neues Schlüssel/Wert-Paar hinzufügen.
t.key2 = nil   -- key2 aus der Tabelle entfernen.

-- Literale notation für jeden (nicht-nil) Wert als Schlüssel:
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- Ausgabe "tau"

-- Schlüssel-Vergleiche funktionieren per Wert für Nummern und Zeichenketten,
-- aber über die Identität bei Tabellen.
a = u['@!#']  -- Nun ist a = 'qbert'.
b = u[{}]     -- Wir würden 1729 erwarten, aber es ist nil:
-- b = nil weil der Lookup fehlschlägt. Er schlägt Fehl, weil der Schlüssel
-- den wir verwendet haben nicht das gleiche Objekt ist das wir verwendet
-- haben um den original Wert zu speichern. Zahlen und Zeichnkette sind daher
-- die praktischeren Schlüssel.

-- Eine Funktion mit nur einem Tabellen-Parameter benötigt keine Klammern.
function h(x) print(x.key1) end
h{key1 = 'Sonmi~451'}  -- Ausgabe 'Sonmi~451'.

for key, val in pairs(u) do  -- Tabellen-Iteration.
  print(key, val)
end

-- _G ist eine spezielle Tabelle die alles Globale enthält.
print(_G['_G'] == _G)  -- Ausgabe 'true'.

-- Verwenden von Tabellen als Listen/Arrays:

-- Listen-Literale verwenden implizit Ganzzahlen als Schlüssel:
v = {'value1', 'value2', 1.21, 'gigawatts'}
for i = 1, #v do  -- #v ist die Größe von v für Listen.
  print(v[i])  -- Indices beginnen mit 1 !! SO VERRÜCKT!
end
-- Eine 'Liste' ist kein echter Typ. v ist nur eine Tabelle mit fortlaufenden
-- Ganzzahlen als Schlüssel, die behandelt wird wie eine Liste.

--------------------------------------------------------------------------------
-- 3.1 Metatabellen und Metamethoden
--------------------------------------------------------------------------------

-- Eine Tabelle kann eine Metatabelle haben. Diese verleiht ihr so etwas wie
-- Tabellen-Operator-Überladungs-Verhalten. Später sehen wir wie
-- Metatabellen js-prototypen artiges Verhalten unterstützen.

f1 = {a = 1, b = 2}  -- Repräsentiert den Bruch a/b.
f2 = {a = 2, b = 3}

-- Dies würde Fehlschlagen:
-- s = f1 + f2

metafraction = {}
function metafraction.__add(f1, f2)
  local sum = {}
  sum.b = f1.b * f2.b
  sum.a = f1.a * f2.b + f2.a * f1.b
  return sum
end

setmetatable(f1, metafraction)
setmetatable(f2, metafraction)

s = f1 + f2  -- Rufe __add(f1, f2) vom der Metatabelle von f1 auf.

-- f1 und f2 haben keine Schlüssel für ihre Metatabellen, anders als bei js
-- Prototypen. Daher muss mithilfe von getmetatable(f1) darauf zugegriffen
-- werden. Eine Metatabelle ist wie eine normale Tabelle mit Schlüsseln die
-- Lua bekannt sind, so wie __add.


-- Die nächste Zeile schlägt fehl weil s keine Metatabelle hat:
-- t = s + s
-- Mihilfe von Klassen ähnlichen Mustern kann das gelöst werden.
-- Siehe weiter unten.

-- Ein __index einer Metatabelle überlädt Punkt-Lookups:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- Funktioniert dank Metatabelle!

--------------------------------------------------------------------------------
-- Direkte Tabellen-Lookups die fehlschlagen werden mithilfe von __index der
-- Metatabelle wiederholt. Das geschieht rekursiv.

-- __index kann auch eine Funktion mit der Form function(tbl, key) sein.
-- Damit kann man Lookups weiter anpassen.

-- Werte wie __index,add, .. werden Metamethoden genannt.
-- HIer eine vollständige Liste aller Metamethoden.

-- __add(a, b)                     für a + b
-- __sub(a, b)                     für a - b
-- __mul(a, b)                     für a * b
-- __div(a, b)                     für a / b
-- __mod(a, b)                     für a % b
-- __pow(a, b)                     für a ^ b
-- __unm(a)                        für -a
-- __concat(a, b)                  für a .. b
-- __len(a)                        für #a
-- __eq(a, b)                      für a == b
-- __lt(a, b)                      für a < b
-- __le(a, b)                      für a <= b
-- __index(a, b)  <fn or a table>  für a.b
-- __newindex(a, b, c)             für a.b = c
-- __call(a, ...)                  für a(...)

--------------------------------------------------------------------------------
-- 3.2 Klassen-Artige Tabellen und Vererbung.
--------------------------------------------------------------------------------

-- Klassen sind in Lua nicht eingebaut. Es gibt verschieden Wege sie mithilfe
-- von Tabellen und Metatabellen zu erzeugen.

-- Die Erklärund des Beispiels erfolgt unterhalb.

Dog = {}                                   -- 1.

function Dog:new()                         -- 2.
  local newObj = {sound = 'woof'}          -- 3.
  self.__index = self                      -- 4.
  return setmetatable(newObj, self)        -- 5.
end

function Dog:makeSound()                   -- 6.
  print('I say ' .. self.sound)
end

mrDog = Dog:new()                          -- 7.
mrDog:makeSound()  -- 'I say woof'         -- 8.

-- 1. Dog verhält sich wie eine Klasse; Ist aber eine Tabelle.
-- 2. "function tablename:fn(...)" ist das gleiche wie
--    "function tablename.fn(self, ...)", Der : fügt nur ein Argument namens
--    self hinzu. Siehe 7 & 8 um zu sehen wie self seinen Wert bekommt.
-- 3. newObj wird eine Instanz von Dog.
-- 4. "self" ist die zu Instanzierende Klasse. Meistern ist self = Dog, aber
--    dies kann durch Vererbung geändert werden. newObj bekommt die Funktionen
--    von self wenn wir die Metatabelle von newObj und __index von self auf
--    self setzen.
-- 5. Zur Erinnerung: setmetatable gibt sein erstes Argument zurück.
-- 6. Der Doppelpunkt funktioniert wie bei 2, aber dieses Mal erwarten wir das
--    self eine Instanz ist und keine Klasse.
-- 7. Das Selbe wie Dog.new(Dog), also self = Dog in new().
-- 8. Das Selbe wie mrDog.makeSound(mrDog); self = mrDog.

--------------------------------------------------------------------------------

-- Vererbungs-Beispiel:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  local s = self.sound .. ' '                 -- 2.
  print(s .. s .. s)
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof'      -- 4.

--------------------------------------------------------------------------------
-- 1. LoudDog bekommt die Methoden und Variablen von Dog.
-- 2. self hat einen 'sound' Schlüssel von new(), siehe 3.
-- 3. Das Gleiche wie "LoudDog.new(LoudDog)", und umgewandelt zu "Dog.new(LoudDog)"
--    denn LoudDog hat keinen 'new' Schlüssel, aber "__index = Dog" steht in der
--    Metatabelle.
--    Ergebnis: Die Metatabelle von seymour ist LoudDog und "LoudDog.__index = Dog".
--    Daher ist seymour.key gleich seymour.key, LoudDog.key, Dog.key, je nachdem
--    welche Tabelle als erstes einen passenden Schlüssel hat.
-- 4. Der 'makeSound' Schlüssel wird in LoudDog gefunden: Das ist das Gleiche
--    wie "LoudDog.makeSound(seymour)".

-- Wenn nötig, sieht new() einer Sub-Klasse genau so aus wie new() der
-- Basis-Klasse:
function LoudDog:new()
  local newObj = {}
  -- set up newObj
  self.__index = self
  return setmetatable(newObj, self)
end

--------------------------------------------------------------------------------
-- 4. Module.
--------------------------------------------------------------------------------


--[[ Dieser Abschnitt ist auskommentiert damit der Rest des Skripts lauffähig
--   bleibt.
```

```lua
-- Angenommen mod.lua sieht so aus:
local M = {}

local function sayMyName()
  print('Hrunkner')
end

function M.sayHello()
  print('Why hello there')
  sayMyName()
end

return M

-- Eine andere Datei könnte die Funktionen in mod.lua so verwenden:
local mod = require('mod')  -- Führe mod.lua aus.

-- require ist der Standard-Weg um Module zu inkludieren.
-- require verhält sich wie: (Wenn nicht gecached wird; siehe später)
local mod = (function ()
  <Inhalt von mod.lua>
end)()
-- Es ist als ob mod.lua eine Funktion wäre, sodass lokale Variablen in
-- mod.lua ausserhalb unsichtbar sind.

-- Das funktioniert weil mod hier das Gleiche wie M in mod.lua ist:
mod.sayHello()  -- Says hello to Hrunkner.

-- Das ist Falsch: sayMyName existiert nur in mod.lua:
mod.sayMyName()  -- Fehler

-- Der Rückgabe-Wert von require wird zwischengespeichert. Sodass Module nur
-- einmal abgearbeitet werden, auch wenn sie mit require öfters eingebunden
-- werden.

-- Nehmen wir an mod2.lua enthält "print('Hi!')".
local a = require('mod2')  -- Ausgabe Hi!
local b = require('mod2')  -- Keine Ausgabe; a=b.

-- dofile ist wie require aber ohne Zwischenspeichern.
dofile('mod2')  --> Hi!
dofile('mod2')  --> Hi! (läuft nochmal, nicht wie require)

-- loadfile ladet eine lua Datei aber die Datei wird noch nicht abgearbeitet.
f = loadfile('mod2')  -- Sobald f() aufgerufen wird läuft mod2.lua.

-- loadstring ist loadfile für Zeichenketten
g = loadstring('print(343)')  -- Gibt eine Funktion zurück..
g()  -- Ausgabe 343; Vorher kam keine Ausgabe.

--]]

```
## Referenzen

Ich war so begeistert Lua zu lernen, damit ich Spiele mit <a href="http://love2d.org/">Love 2D game engine</a> programmieren konnte.

Ich habe angefangen mit <a href="http://nova-fusion.com/2012/08/27/lua-for-programmers-part-1/">BlackBulletIV's Lua for programmers</a>.
Danach habe ich das offizielle Lua Buch gelesen: <a href="http://www.lua.org/pil/contents.html">Programming in Lua</a>

Es kann auch hilfreich sein hier vorbeizuschauen: <a href="http://lua-users.org/files/wiki_insecure/users/thomasl/luarefv51.pdf">Lua short
reference</a>

Wichtige Themen die hier nicht angesprochen wurden; die Standard-Bibliotheken:

* <a href="http://lua-users.org/wiki/StringLibraryTutorial">string library</a>
* <a href="http://lua-users.org/wiki/TableLibraryTutorial">table library</a>
* <a href="http://lua-users.org/wiki/MathLibraryTutorial">math library</a>
* <a href="http://lua-users.org/wiki/IoLibraryTutorial">io library</a>
* <a href="http://lua-users.org/wiki/OsLibraryTutorial">os library</a>

Übrigends, die gesamte Datei ist gültiges Lua. Speichere sie als learn.lua und
starte sie als "lua learn.lua" !

Die Erstfassung ist von tylerneylon.com, und ist auch hier verfügbar: <a href="https://gist.github.com/tylerneylon/5853042">github gist</a>. Viel Spaß mit Lua!
