---
language: Lua
filename: learnlua-fr.lua
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
translators:
    - ["Roland Yonaba", "http://github.com/Yonaba"]
lang: fr-fr
---

```lua
-- Les commentaires unilignes commencent par un double tiret.

--[[
     Les doubles crochets à la suite du double tiret 
     permettent d'insérer des commentaires multilignes.
--]]

----------------------------------------------------
-- 1. Variables et contrôle d'exécution.
----------------------------------------------------

num = 42  -- Tous les nombres sont de type double.
-- Rassurez vous cependant, les doubles stockés sur 64-bits
-- en réservent 52 pour la valeur exacte des entiers. La
-- précision n'est donc pas un problème pour tout entier qui
-- peut être codé sur moins de 52 bits.

s = 'walternate'  -- Chaines de caractères immuables comme en Python.
t = "une chaine avec des guillemets doubles"
u = [[les double crochets permettent
      d'avoir une chaine de caractères
      sur plusieurs lignes.]]
t = nil  -- Affecte la valeur nulle à t; Lua possède un ramasse-miettes

-- Le do/end définit un bloc de code
while num < 50 do
  num = num + 1  -- Pas d'opérateurs de type ++ ou +=.
end

-- Les structures en if:
if num > 40 then
  print('supérieur à 40')
elseif s ~= 'walternate' then  -- ~= : est différent de.
  -- Le test d'égalité se fait avec == comme en Python.
  io.write('inférieur à 40\n')  -- Écrit par defaut sur la sortie stdout.
else
  -- Les variables sont globales par défaut.
  thisIsGlobal = 5  -- le style camelCase est courant.

  -- Une variable locale est déclarée avec le mot-clé local:
  local line = io.read()  -- Permet de lire la ligne suivante dans stdin.

  -- .. est l'opérateur de concaténation:
  print("L'hiver approche, " .. line)
end

-- Les variables non définies reçoivent par défaut la valeur nil.
foo = anUnknownVariable  -- Maintenant, foo = nil.

aBoolValue = false

-- Seuls nil et false sont des valeurs fausses.
-- Mais 0 et '' sont des valeurs vraies!
if not aBoolValue then print('etait faux') end

-- L'évaluation du 'or' et du 'and' est court-circuité.
-- Comme avec les ternaires du C et du JS: a?b:c
ans = aBoolValue and 'oui' or 'non'  --> 'non'

karlSum = 0
for i = 1, 100 do  -- Les bornes sont incluses dans l'intervalle.
  karlSum = karlSum + i
end

-- Utilisez "100, 1, -1" pour la décrémentation:
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

-- En général, l'intervalle est début, fin[, pas].

-- Un autre type de boucle:
repeat
  print('the way of the future')
  num = num - 1
until num == 0


----------------------------------------------------
-- 2. Fonctions.
----------------------------------------------------

function fib(n)
  if n < 2 then return n end
  return fib(n - 2) + fib(n - 1)
end

-- Lua implémente les fermetures et les fonctions anonymes:
function adder(x)
  -- La fonction retournée est créée lorsque adder est appelé
  -- et elle se rappelle de la valeur de x.
  return function (y) return x + y end
end
a1 = adder(9)
a2 = adder(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- Les valeurs de retour, les appels de fonction, les assignations
-- supportent tous les listes qui peuvent ne pas correspondre en longueur.
-- Dans ce cas, les variables à assigner en supplément reçoivent nil
-- tandis que les valeurs à attribuer en supplément sont ignorées

x, y = 1, 2 -- x = 1 et y = 2
x, y, z = 1, 2 -- x = 1, y = 2 et z = nil
x, y, z = 1, 2, 3, 4 -- x = 1, y = 2, z = 3, et 4 est ignoré.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> affiche "zaphod nil nil"
-- x = 4, y = 8, les valeurs 15 à 42 sont ignorées.

-- Les fonctions sont des valeurs de première classe 
-- et peuvent être locales/globales.
-- Les déclarations suivantes sont identiques:
function f(x) return x * x end
f = function (x) return x * x end

-- Il en va de même pour les déclarations suivantes:
local function g(x) return math.sin(x) end
local g = function(x) return math.sin(x) end
-- Sauf que pour le dernier cas, même si local g = function(x)
-- est équivalent à local function g(x), il n'est pas possible
-- de faire appel à g à l'intérieur du corps de la fonction (récursion)

-- À moins de déclarer la fonction auparavant:
local g; g  = function (x) return math.sin(x) end

-- À propos, les fonctions trigonométriques interprètent 
-- leurs arguments en radians.
print(math.cos(math.pi)) -- affiche "-1"
print(math.sin(math.pi)) -- affiche "0"

-- Lorsqu'une fonction est appelée avec un seul argument qui est une chaine,
-- les parenthèses peuvent être omises:
print 'hello'  -- équivalent à print('hello').

-- Lorsqu'une fonction est appelée avec un seul argument qui est une table,
-- les parenthèses peuvent aussi être omises.
print {} -- équivalent à print({}).


----------------------------------------------------
-- 3. Tables.
----------------------------------------------------

-- Tables = Seule structure de données en Lua;
--          Ce sont des listes assotiatives.
-- Elles sont similaires aux tables PHP ou aux objets JS :
-- des tables-dictionnaires que l'on peut utiliser en tant que listes.

-- Tables en tant que dictionnaires:

-- Les clés sont des chaines de caractères par défaut:
t = {key1 = 'valeur1', key2 = false}

-- Elles peuvent être indexées avec la notation en point, comme en JS:
print(t.key1)  -- Affiche "valeur1".
t.newKey = {}  -- Ajoute une nouvelle paire clé/valeur.
t.key2 = nil   -- Supprime la clé "key2" de la table.

-- Notation littérale pour toute valeur non nulle en tant que clé:
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- affiche "tau"

-- La correspondance des clés se fait par valeur pour
-- les nombres et les chaines, mais par référence pour les tables.
a = u['@!#']  -- a = 'qbert'.
b = u[{}]     -- On pourrait s'attendre à 1729, mais l'on obtient nil:
-- b = nil car la clé utilisée n'est pas le même objet que celui
-- utilisé pour stocker la valeur originale 1729.

-- Si une fonction prend en argument une seule table, l'on peut
-- omettre les parenthèses:
function h(x) print(x.key1) end
h{key1 = 'Sonmi~451'}  -- Affiche 'Sonmi~451'.

for key, val in pairs(u) do  -- Parcours d'une table.
  print(key, val)
end

-- _G est une table spéciale contenant toutes les variables globales,
-- et donc elle même.
print(_G['_G'] == _G)  -- Affiche 'true'.

-- Tables en tant que listes:

-- De manière implicite, les clés sont des nombres entiers:
v = {'value1', 'value2', 1.21, 'gigawatts'}
for i = 1, #v do  -- #v retourne la taille de la table v si elle est une liste.
  print(v[i])  -- Attention, en Lua, les indices commencent à 1!
end
-- Il n'existe pas vraiment de type 'liste' en Lua, v est juste
-- une table avec des clés qui sont des nombres entiers consécutifs
-- commençant à 1. Lua le traite comme étant une liste.

----------------------------------------------------
-- 3.1 Métatables and métaméthodes.
----------------------------------------------------

-- Une table peut avoir une métatable qui confère à la table
-- un patron/prototype de conception (surcharge d'opération). Nous verrons
-- dans la suite comment les métatables imitent le prototypage du JS.

f1 = {a = 1, b = 2}  -- Représente la fraction a/b.
f2 = {a = 2, b = 3}

-- Ceci créée une erreur:
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

s = f1 + f2  -- appèle __add(f1, f2) de la métatable de f1

-- f1, f2 ne possèdent pas de clé qui pointent vers leur métatable, comme
-- avec les prototypes en JS. Mais l'on peut utiliser getmetatable(f1).
-- La métatable est une table normale avec des clés prédéfinies, comme __add.

-- Mais la ligne suivante génère une erreur puisque s n'a pas de métatable:
-- t = s + s
-- En implémentant de l'orienté objet, comme nous le verrons par la suite,
-- le problème est résolu.

-- Une clé __index dans une métatable mt surcharge l'indexation dans sa table t
-- si la clé est absente de cette table t:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- Affiche "gru"! merci à la métatable!

-- Ainsi donc, un accès direct à une valeur dans une table via une clé 
-- inexistante (ce qui normalement retourne "nil") conduira à exploiter
-- le champ __index de la métatable. Cela peut être récursif.

-- Le champ __index peut aussi être une fonction (tbl, clé)
-- ce qui permet une gestion plus souple des indexations.

-- Les clés __index, __add,... sont appelées métaméthodes.
-- En voici la liste complète:

-- __add(a, b)                   pour a + b
-- __sub(a, b)                   pour a - b
-- __mul(a, b)                   pour a * b
-- __div(a, b)                   pour a / b
-- __mod(a, b)                   pour a % b
-- __pow(a, b)                   pour a ^ b
-- __unm(a)                      pour -a
-- __concat(a, b)                pour a .. b
-- __len(a)                      pour #a
-- __eq(a, b)                    pour a == b
-- __lt(a, b)                    pour a < b
-- __le(a, b)                    pour a <= b
-- __index(a, b)  <fn ou table>  pour a.b
-- __newindex(a, b, c)           pour a.b = c
-- __call(a, ...)                pour a(...)

----------------------------------------------------
-- 3.2 Pseudo-orienté objet et héritage.
----------------------------------------------------

-- Lua n'implémente pas d'orienté objet par défaut.
-- Mais il reste possible d'imiter de plusieurs manières 
-- le concept de "classe" grâce aux tables et aux métatables.

-- L'explication pour l'exemple qui suit vient juste après.

Dog = {}                                   -- 1.

function Dog:new()                         -- 2.
  local newObj = {sound = 'woof'}          -- 3.
  self.__index = self                      -- 4.
  return setmetatable(newObj, self)        -- 5.
end

function Dog:makeSound()                   -- 6.
  print('Je dis: ' .. self.sound..'!')
end

mrDog = Dog:new()                          -- 7.
mrDog:makeSound()  -- 'Je dis: woof!       -- 8.

-- 1. Dog agit comme une classe; c'est une simple table.
-- 2. L'expression tbl:fn(...) est identique à 
--    tbl.fn(self, ...)
--    La notation : permet de passer par défaut un premier 
--    argument appelé "self" à la fonction tbl.fn
--    Voir 7 & 8 ci-après pour comprendre comment self prend
--    sa valeur.
-- 3. newObj sera une instance de la classe Dog.
-- 4. self = la classe instanciée. Souvent, self = Dog, mais
--    cela peut changer du fait de l'héritage.
--    newObj reçoit les fonctions de self si l'__index des
--    métatables de newObj et de self pointent vers self.
-- 5. Rappel: setmetatable retourne son premier argument.
-- 6. La notation : fonctionne comme au 2, mais cette fois, self
--    est une instance au lieu d'être une classe.
-- 7. Similaire à Dog.new(Dog), donc self = Dog dans new().
-- 8. Similaire à mrDog.makeSound(mrDog); self = mrDog.

----------------------------------------------------

-- Exemple d'héritage:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  local s = self.sound .. ' '                 -- 2.
  print(s .. s .. s..'!')
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof!'     -- 4.

-- 1. LoudDog reçoit les méthodes et les variables de Dog.
-- 2. self possède une clé 'sound', reçue de new(), voir 3.
-- 3. Similaire à LoudDog.new(LoudDog) et traduit en Dog.new(LoudDog),
--    puisque LoudDog ne possède pas de clé 'new', mais a une métatable
--    qui a la clé __index = Dog.
--    Résulat: la métatable de seymour est LoudDog, et
--    LoudDog.__index = LoudDog. Donc seymour.key deviendra soit égal à
--    seymour.key, LoudDog.key, Dog.key, selon le fait qu'il s'agira
--    de la première table ayant la clé 'key' en question, en remontant
--    dans la hiérarchie.
-- 4. La clé 'makeSound' est trouvée dans LoudDog; cela est similaire
--    à LoudDog.makeSound(seymour).

-- Si besoin est, la méthode new() de la sous-classe est
-- identique à la méthode new() de sa classe mère:
function LoudDog:new()
  local newObj = {}
  -- Prépare self à être la superclasse de newObj:
  self.__index = self
  return setmetatable(newObj, self)
end

----------------------------------------------------
-- 4. Modules.
----------------------------------------------------


--[[ Cette section est mise en commentaire afin que le reste du
--   ce script reste exécutable.
```

```lua
-- Supposons que le fichier mod.lua contienne ceci:
local M = {}

local function sayMyName()
  print('Hrunkner')
end

function M.sayHello()
  print('hello')
  sayMyName()
end

return M

--  Un autre fichier peut exploiter le contenu défini dans mod.lua's:
local mod = require('mod')  -- Exécute le fichier mod.lua.

-- require est le moyen par défaut d'inclure des modules.
-- require agit comme:     (si non trouvé en cache; voir ci-après)
local mod = (function ()
  <contenu de mod.lua>
end)()
-- Comme si le contenu de mod.lua était enveloppé dans le corps d'une fonction,
-- si bien que les variables locales contenues dans mod.lua sont 
-- inaccessibles en dehors de ce module.

-- Le code suivant fonctionne car mod = M (dans mod.lua):
mod.sayHello()  -- Dis bonjour à Hrunkner.

-- Le code suivant génère une erreur car sayMyName est local à mod.lua:
mod.sayMyName()  -- erreur!

-- Les valeurs retournées par require sont mises en cache, ce qui fait
-- qu'un module est toujours chargé une seule fois, même s'il est inclus
-- avec require à plusieurs reprises.

-- Supposons que mod2.lua contienne le code "print('Hi!')".
local a = require('mod2')  -- Affiche "Hi!"
local b = require('mod2')  -- N'affiche rien; et a = b.

-- dofile est identique à require, sauf qu'il ne fait pas de mise en cache:
dofile('mod2')  --> Hi!
dofile('mod2')  --> Hi! (le code de mod2.lua est encore exécuté)

-- loadfile charge le contenu d'un fichier, sans l'exécuter.
f = loadfile('mod2')  -- L'appel f() exécute le contenu de mod2.lua.

-- loadstring est similaire à loadfile, mais pour les chaines de caractères.
g = loadstring('print(343)')  -- Retourne une fonction.
g()  -- Affiche 343; Rien n'est affiché avant cet appel.

--]]

```
## Références

*Les références qui suivent sont en Anglais.*

Les sujets non abordés dans ce tutoriel sont couverts en intégralité par 
les librairies standard:

* La librairie <a href="http://lua-users.org/wiki/StringLibraryTutorial">string</a>
* La librairie <a href="http://lua-users.org/wiki/TableLibraryTutorial">table</a>
* La librairie <a href="http://lua-users.org/wiki/MathLibraryTutorial">math</a>
* La librairie <a href="http://lua-users.org/wiki/IoLibraryTutorial">io</a>
* La librairie <a href="http://lua-users.org/wiki/OsLibraryTutorial">os</a>

Autres références complémentaires:

* <a href="http://nova-fusion.com/2012/08/27/lua-for-programmers-part-1/">Lua pour programmeurs</a>
* <a href="lua-users.org/files/wiki_insecure/users/thomasl/luarefv51.pdf">Référence condensée de Lua</a>
* <a href="http://www.lua.org/pil/contents.html">Programmer en Lua</a>
* <a href="http://www.lua.org/manual/">Les manuels de référence Lua</a>

A propos, ce fichier est exécutable. Sauvegardez-le sous le nom *learn.lua* et
exécutez-le avec la commande `lua learn.lua` !

Ce tutoriel a été originalement écrit pour <a href="tylerneylon.com">tylerneylon.com</a> et est aussi 
disponible en tant que <a href="https://gist.github.com/tylerneylon/5853042">gist</a>.
Il a été traduit en français par Roland Yonaba (voir son <a href="http://github.com/Yonaba">github</a>).

Amusez-vous bien avec Lua!
