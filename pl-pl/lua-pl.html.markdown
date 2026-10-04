---
language: Lua
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
translators:
    - ["Aimless", "https://github.com/aimlesx"]
filename: learnlua-pl.lua
lang: pl-pl
---

```lua
-- Dwa myślniki rozpoczynają jednolinijkowy komentarz.

--[[
     Dodanie dwóch znaków [ oraz ] tworzy 
     komentarz wieloliniowy.
--]]

----------------------------------------------------
-- 1. Zmienne i sterowanie przepływem.
----------------------------------------------------

liczba = 42  -- Liczby mogą być całkowite lub zmiennoprzecinkowe.

s = 'walternate'  -- Niezmienne ciągi znaków jak w Pythonie.
t = "podwójne cudzysłowy też są w porządku"
u = [[ Podwójne nawiasy
       otwierają i zamykają
       wieloliniowe ciągi znaków.]]
t = nil  -- Usuwa zmienną t; Lua posiada garbage collector.

-- Bloki są oznaczane słowami kluczowymi, takimi jak do/end:
while liczba < 50 do
  liczba = liczba + 1  -- Brak operatorów ++ lub +=.
end

-- Instrukcje warunkowe if:
if liczba > 40 then
  print('powyżej 40')
elseif s ~= 'walternate' then  -- ~= oznacza "nie równe".
  -- Porównanie równości to == jak w Pythonie; działa dla stringów.
  io.write('nie powyżej 40\n')  -- Domyślnie wypisuje na stdout.
else
  -- Zmienne są domyślnie globalne.
  zmiennaGlobalna = 5  -- Popularny jest camel case.
    
  -- Tak deklaruje się zmienne lokalne:
  local linia = io.read()  -- Odczytuje kolejną linię wejścia standardowego.
    
  -- Łączenie ciągów znaków za pomocą operatora .. :
  print('Nadchodzi zima, ' .. linia)
end

-- Niezdefiniowane zmienne zwracają nil.
-- Nie jest to błędem:
foo = nieznanaZmienna  -- Teraz foo = nil.

wartoscLogiczna = false

-- Tylko nil i false są fałszywe; 0 i '' są prawdziwe!
if not wartoscLogiczna then print('było fałszywe') end

-- 'or' i 'and' podlegają ewaluacji short-circuit.
-- Działają podobnie do operatora a?b:c z C/js:
wynik = wartoscLogiczna and 'tak' or 'nie'  --> 'nie'

suma = 0
for i = 1, 100 do  -- Zakres obejmuje oba końce.
  suma = suma + i
end

-- Aby zliczać w dół użyj zakresu "100, 1, -1":
suma = 0
for j = 100, 1, -1 do suma = suma + j end

-- Ogólnie zakres to początek, koniec[, krok].

-- Inna konstrukcja pętli:
repeat
  print('droga przyszłości')
  liczba = liczba - 1
until liczba == 0


----------------------------------------------------
-- 2. Funkcje.
----------------------------------------------------

function fib(n)
  if n < 2 then return 1 end
  return fib(n - 2) + fib(n - 1)
end

-- Funkcje anonimowe i domknięcia (Closures w JS) są okej:
function sumator(x)
  -- Zwracana funkcja jest tworzona, gdy sumator jest
  -- wywoływany, i zapamiętuje wartość x:
  return function (y) return x + y end
end
a1 = sumator(9)
a2 = sumator(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- Zwracanie, wywołania funkcji i przypisania działają
-- z listami o potencjalnie różnych długościach.
-- Zmienne które nie dostaną wartości dostają wartość nil;
-- nadmiarowe wartości są odrzucane.

x, y, z = 1, 2, 3, 4
-- Teraz x = 1, y = 2, z = 3, a 4 jest odrzucone.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> wypisuje "zaphod nil nil"
-- Teraz x = 4, y = 8, wartości 15...42 są odrzucone.

-- Funkcje są obiektami pierwszej klasy, mogą być lokalne/globalne.
-- Obie linie poniżej mają identyczny efekt:
function f(x) return x * x end
f = function (x) return x * x end

-- Te również:
local function g(x) return math.sin(x) end
local g; g = function (x) return math.sin(x) end
-- deklaracja 'local g' sprawia, że odwołania do g są poprawne.

-- Tak swoją drogą, to funkcje trygonometryczne działają w radianach.

-- Wywołania z jednym parametrem stringowym nie wymagają nawiasów:
print 'cześć'  -- Działa poprawnie.


----------------------------------------------------
-- 3. Tabele.
----------------------------------------------------

-- Tabele = jedyna złożona struktura danych w Lua;
--          są to tablice asocjacyjne.
-- Podobne do tablic w PHP lub obiektów w JS, są to
-- słowniki (hashmap), które można używać także
-- jako listy.

-- Używanie tabel jako słowników / map:

-- Literały słowników domyślnie mają klucze typu string:
t = {klucz1 = 'wartość1', klucz2 = false}

-- Klucze typu string mogą używać notacji kropkowej (jak w JS):
print(t.key1)    -- Wypisuje 'wartość1'.
t.nowyKlucz = {} -- Dodaje nową parę klucz/wartość.
t.klucz2 = nil   -- Usuwa klucz2 z tabeli.

-- Notacja literału dla dowolnego klucza (nie będącego nil):
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- wypisuje "tau"

-- Dopasowanie klucza działa według wartości dla liczb
-- i ciągów, ale według tożsamości dla tabel.
a = u['@!#']  -- Teraz a = 'qbert'.
b = u[{}]     -- Można by oczekiwać 1729, ale wynik to nil:
-- b = nil, ponieważ wyszukiwanie nie powiodło się.
-- Dzieje się tak, ponieważ klucz, którego użyliśmy, nie jest
-- tym samym obiektem, który był użyty do przechowania
-- oryginalnej wartości. Dlatego ciągi i liczby są bardziej
-- przenośnymi kluczami.

-- Funkcje z jednym parametrem będącym tabelą nie wymagają nawiasów:
function h(x) print(x.klucz1) end
h{klucz1 = 'Sonmi~451'}  -- Wypisuje 'Sonmi~451'.

for key, val in pairs(u) do  -- Iteracja przez tabelę.
  print(key, val)
end

-- _G to specjalna tabela zawierająca wszystkie zmienne globalne.
print(_G['_G'] == _G)  -- Wypisuje 'true'.

-- Używanie tabel jako list / tablic:

-- Literały list ustawiają domyślnie klucze jako liczby całkowite:
v = {'wartość1', 'wartość2', 1.21, 'gigawaty'}
for i = 1, #v do  -- #v to rozmiar listy v.
  print(v[i])  -- Indeksy zaczynają się od 1 !! SZALONE!
end
-- "Lista" nie jest prawdziwym typem. v to po prostu tabela
-- z kolejnymi kluczami liczbowymi, traktowana jako lista.

----------------------------------------------------
-- 3.1 Metatabele i metametody.
----------------------------------------------------

-- Tabela może mieć metatabelę, która nadaje tabeli
-- funkcjonalność podobną do przeciążania operatorów.
-- Później zobaczymy, jak metatabele wspierają zachowanie 
-- przypominające prototypy z JS.

f1 = {a = 1, b = 2}  -- Reprezentuje ułamek a/b.
f2 = {a = 2, b = 3}

-- To by się nie powiodło:
-- s = f1 + f2

metaulamek = {}
function metaulamek.__add(f1, f2)
  sum = {}
  sum.b = f1.b * f2.b
  sum.a = f1.a * f2.b + f2.a * f1.b
  return sum
end

setmetatable(f1, metaulamek)
setmetatable(f2, metaulamek)

s = f1 + f2  -- Wywołuje __add(f1, f2) na metatabeli f1

-- f1, f2 nie mają klucza dla swojej metatabeli, w przeciwieństwie
-- do prototypów w JS, więc trzeba ją uzyskać za pomocą
-- getmetatable(f1). Metatabela to normalna tabela
-- z kluczami, które Lua rozpoznaje, np. __add.

-- Ale następna linia zawiedzie, ponieważ s nie ma metatabeli:
-- t = s + s
-- Wzorce przypominające klasy przedstawione poniżej
-- byłyby w stanie to naprawić.

-- __index w metatabeli przeciąża odwołania przez kropkę:
domyslnieUlubione = {zwierze = 'gru', jedzenie = 'pączki'}
mojeUlubione = {jedzenie = 'pizza'}
setmetatable(mojeUlubione, {__index = domyslnieUlubione})
zjedzonyPrzez = mojeUlubione.zwierze  -- działa! dzięki metatabelo

-- Bezpośrednie wyszukiwania w tabeli, które zawiodą, zostaną 
-- powtórzone przy użyciu wartości __index w metatabeli, co 
-- może się rekurencyjnie powtarzać.

-- Wartość __index może być także funkcją function(tabela, klucz)
-- dla bardziej zaawansowanych wyszukiwań.

-- Wartości __index, __add, itd. nazywane są metametodami.
-- Pełna lista. Tutaj a to tabela z metametodą.

-- __add(a, b)                     dla a + b
-- __sub(a, b)                     dla a - b
-- __mul(a, b)                     dla a * b
-- __div(a, b)                     dla a / b
-- __mod(a, b)                     dla a % b
-- __pow(a, b)                     dla a ^ b
-- __unm(a)                        dla -a
-- __concat(a, b)                  dla a .. b
-- __len(a)                        dla #a
-- __eq(a, b)                      dla a == b
-- __lt(a, b)                      dla a < b
-- __le(a, b)                      dla a <= b
-- __index(a, b)  <fn or a table>  dla a.b
-- __newindex(a, b, c)             dla a.b = c
-- __call(a, ...)                  dla a(...)

----------------------------------------------------
-- 3.2 Tabele przypominające klasy i dziedziczenie.
----------------------------------------------------

-- Klasy nie są wbudowane; istnieją różne sposoby
-- na ich tworzenie przy użyciu tabel i metatabel.

-- Wyjaśnienie dla tego przykładu znajduje się poniżej.

Pies = {}                                  -- 1.

function Pies:new()                        -- 2.
  nowyObiekt = {odglos = 'hau'}            -- 3.
  self.__index = self                      -- 4.
  return setmetatable(nowyObiekt, self)    -- 5.
end

function Pies:wydajOdglos()                -- 6.
  print('Ja mówię ' .. self.odglos)
end

panPies = Pies:new()                       -- 7.
panPies:wydajOdglos()  -- 'Ja mówię hau'   -- 8.

-- 1. Pies działa jak klasa; w rzeczywistości to tabela.
-- 2. function tablename:fn(...) to to samo, co
--    function tablename.fn(self, ...)
--    Dwukropek dodaje pierwszy argument o nazwie self.
--    Przeczytaj 7 i 8 poniżej, aby zrozumieć, jak self 
--    dostaje swoją wartość.
-- 3. nowyObiekt będzie instancją klasy Pies.
-- 4. self = klasa, która jest instancjonowana. Często
--    self = Pies, ale dziedziczenie może to zmienić.
--    nowyObiekt dostaje funkcje self, kiedy ustawiamy
--    metatabelę nowyObiekt i __index self na self.
-- 5. Przypomnienie: setmetatable zwraca swój pierwszy argument.
-- 6. Dwukropek działa jak w punkcie 2, ale tym razem
--    oczekujemy, że self będzie instancją a nie klasą.
-- 7. To samo, co Pies.new(Pies), więc self = Pies w new().
-- 8. To samo, co panPies.wydajOdglos(panPies); self = panPies.

----------------------------------------------------

-- Przykład dziedziczenia:

GlosnyPies = Dog:new()                         -- 1.

function GlosnyPies:wydajOdglos()
  s = self.odglos .. ' '                       -- 2.
  print(s .. s .. s)
end

seymour = GlosnyPies:new()                     -- 3.
seymour:wydajOdglos()  -- 'hau hau hau'        -- 4.

-- 1. GlosnyPies dziedziczy metody i zmienne od Pies.
-- 2. self ma klucz 'odglos' z new(), zobacz punkt 3.
-- 3. To samo, co GlosnyPies.new(GlosnyPies), przekształcone w
--    Pies.new(GlosnyPies), ponieważ GlosnyPies nie ma klucza 'new',
--    ale ma __index = Pies w swojej metatabeli.
--    Wynik: metatabela seymoura to GlosnyPies, a
--    GlosnyPies.__index = GlosnyPies. Więc seymour.klucz1 będzie
--    = seymour.klucz1, GlosnyPies.klucz1, Pies.klucz1, w zależności od tego,
--    która tabela pierwsza ma dany klucz.
-- 4. Klucz 'wydajOdglos' jest znaleziony w GlosnyPies; to to samo,
--    co GlosnyPies.wydajOdglos(seymour).

-- Jeśli potrzeba, new() w podklasie wygląda jak w bazowej:
function GlosnyPies:new()
  nowyObiekt = {}
  -- konfiguracja nowyObiekt
  self.__index = self
  return setmetatable(nowyObiekt, self)
end

----------------------------------------------------
-- 4. Moduły.
----------------------------------------------------


--[[ Komentuję tę sekcję, aby reszta
--   tego skryptu pozostała możliwa do uruchomienia.
```

```lua
-- Załóżmy, że plik mod.lua wygląda tak:
local M = {}

local function przedstawSie()
  print('Hrunkner')
end

function M.przywitajSie()
  print('Cześć ') 
  przedstawSie()
end

return M

-- Inny plik może użyć funkcjonalności z mod.lua:
local mod = require('mod')  -- Uruchamia plik mod.lua.

-- require to standardowy sposób na importowanie modułów.
-- require działa jak:     (jeśli nie jest w pamięci podręcznej; zob. poniżej)
local mod = (function ()
  <zawartość mod.lua>
end)()
-- To tak, jakby mod.lua był ciałem funkcji, więc
-- lokalne zmienne w mod.lua są niewidoczne na zewnątrz.

-- To działa, ponieważ mod tutaj = M w mod.lua:
mod.przywitajSie() -- Wypisuje: Cześć Hrunkner

-- To jest błędne; sayMyName istnieje tylko w mod.lua:
mod.przedstawSie()  -- błąd

-- Zwracane wartości z require są pamiętane, więc plik
-- jest uruchamiany maksymalnie raz, nawet jeśli jest
-- wielokrotnie wywoływany przez require.

-- Załóżmy, że mod2.lua zawiera "print('Witam!')".
local a = require('mod2')  -- Wypisuje Witam!
local b = require('mod2')  -- Nie wypisuje nic; a=b.

-- dofile działa jak require, ale bez pamiętania wartości zwracanej:
dofile('mod2.lua')  --> Witam!
dofile('mod2.lua')  --> Witam! (uruchamia ponownie)

-- loadfile ładuje plik Lua, ale jeszcze go nie uruchamia.
f = loadfile('mod2.lua')  -- Wywołaj f(), aby go uruchomić.

-- load to odpowiednik loadfile dla stringów.
-- (loadstring jest przestarzałe, używaj load)
g = load('print(343)')  -- Zwraca funkcję.
g()  -- Wypisuje 343; nic nie zostało wypisane wcześniej.
--]]
```

## Społeczność

Jeśli potrzebujesz wsparcia, dołącz do oficjalnej [listy mailingowej Lua](https://www.lua.org/lua-l.html), [kanału IRC](http://lua-users.org/wiki/IrcChannel) lub [forum](https://luaforum.com).

## Źródła

Chciałem nauczyć się Lua, aby tworzyć gry
przy użyciu [silnika gier LÖVE](http://love2d.org/). To był mój cel.

Zacząłem od [„Lua for programmers” autorstwa BlackBulletIV](https://ebens.me/posts/lua-for-programmers-part-1/).
Następnie przeczytałem oficjalną książkę [„Programming in Lua”](http://www.lua.org/pil/contents.html).
Tak wyglądała moja droga.

Może być pomocne zapoznanie się z [krótką dokumentacją Lua](http://lua-users.org/wiki/LuaShortReference) na lua-users.org.

Główne tematy, które nie zostały omówione, to standardowe biblioteki:

* [`string` (łańcuchy znaków)](http://lua-users.org/wiki/StringLibraryTutorial)
* [`table` (tabele)](http://lua-users.org/wiki/TableLibraryTutorial)
* [`math` (funkcje matematyczne)](http://lua-users.org/wiki/MathLibraryTutorial)
* [`io` (wejście/wyjście)](http://lua-users.org/wiki/IoLibraryTutorial)
* [`os` (funkcje systemowe)](http://lua-users.org/wiki/OsLibraryTutorial)

Przy okazji, cały plik jest poprawnym skryptem Lua; zapisz go
jako `learn.lua` i uruchom poleceniem "`lua learn.lua`"!

Ten tekst został pierwotnie napisany dla strony tylerneylon.com, a także
jest dostępny jako [GitHub gist](https://gist.github.com/tylerneylon/5853042). Baw się dobrze z Lua!
