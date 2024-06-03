---
language: Lua
filename: learnlua-es.lua
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
translators:
    - ["Jorge Diaz", "https://github.com/jorgeldb"]
lang: es-es
---

```lua
-- Dos guiones inician un comentario de una única línea.

--[[
     Añadir dos corchetes [ y ] lo convierten
     en un comentario multi-línea
--]]

----------------------------------------------------
-- 1. Variables y control de flujo.
----------------------------------------------------

num = 42  -- Todos los números son flotantes
          -- de precisión doble (64 bits).
          -- Los dobles de 64 bits pueden tienen
          -- 52 bits para representación de valores
          -- enteros, así que no representa un
          -- problema para valores menores a 52 bits.

s = 'alternados'  -- Los string son imnutables, como en Python
t = "Las comillas dobles también son válidas"
u = [[  Los corchetes dobles inician
        y terminan strings de
        múltiples líneas. ]]
t = nil  -- Vuelve a t indefinido. Lua hace uso de Garbage Collector.

-- Los bloques se denotan con palabras claves como "do" o "end"

-- Ciclo while (do/end)
while num < 50 do
  num = num + 1  -- No existen operadores como ++ o +=
end

-- Sentencia if (then/end)
if num > 40 then
  print('mayor a 40')
elseif s ~= 'alternados' then  -- ~= significa "diferente de"
  -- == significa "igual a". Puede usarse en strings, igual que en Python

  io.write('no mayor a 40\n')  -- Por defecto, escribe
                               -- a la salida estándar stdout
else
  -- Las variables son globales por defecto
  estoEsGlobal = 5  -- Es común utilizar Camel Case.

  -- Se usa la palabra clave 'local' para declarar variables locales
  local line = io.read()  -- Lee la próxima línea de la entrada
                          -- estándar stdin

  -- Para concatenar strings se usa el operador ".."
  print('Viene el invierno, ' .. line)
end

-- Las variables indefinidas retornan nil
-- Esto no es un error
foo = unaVariableDesconocida  -- Ahora foo = nil.

unValorBooleano = false

-- Sólo 'nil' y 'false' son valores falsos. ¡0 y "" son verdaderos!
if not unValorBooleano then print('era falso') end

-- 'or' y 'and' son operadores corto-circuito
-- Esto es similar al operador ternario en C/JavaScript
ans = unValorBooleano and 'sí' or 'no'  --> 'no'

karlSum = 0
-- El rango es inclusivo, esto empieza en 1 y termina en 100
for i = 1, 100 do  karlSum = karlSum + i
end

-- Se puede usar "100, 1, -1" con paso negativo como rango decremental
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

-- En general, los rangos son: inicio, fin[, paso].

-- Otra manera de hacer bucle, similar a una sentencia do/while en C/Java
repeat
  print('el camino del futuro')
  num = num - 1
until num == 0


----------------------------------------------------
-- 2. Funciones.
----------------------------------------------------

-- Las funciones se declaran con "function"
function fib(n)
  if n < 2 then return 1 end
  return fib(n - 2) + fib(n - 1) -- ¡Pueden ser recursivas!
end

-- Las clausuras y funciones anónimas están permitidas:
function adder(x)
  -- La función retornada es creada al invocar "caller"
  -- y recuerda el valor de x.
  return function (y) return x + y end
end
a1 = adder(9)
a2 = adder(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- Los retornos, llamados de función y asignaciones
-- admiten listas que pueden ser diferentes en
-- tamaño.
-- Los receptores sin valor asociado son nil.
-- Los valores sin receptores son descartados.

x, y, z = 1, 2, 3, 4
-- Ahora, x = 1, y = 2, z = 3. El 4 es descartado.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> Esto imprime "zaphod  nil nil"
-- Ahora x = 4, y = 8, y los valores 15, 16, 23 y 42 son descartados.

-- Las funciones son de primera clase, pueden ser globales o locales:
-- Estas 2 líneas hacen lo mismo:
function f(x) return x * x end
f = function (x) return x * x end

-- Al igual que estas 2 líneas:
local function g(x) return math.sin(x) end
local g; g  = function (x) return math.sin(x) end
-- La declaración 'local g' hace que las autorreferencias de g sean válidas

-- Por cierto, las funciones trigonométricas trabajan en radianes.

-- Los llamados de funciones con un único string no requieren paréntesis.
-- Estas 2 líneas de código hacen lo mismo:
print 'hello'
print('hello')


----------------------------------------------------
-- 3. Tablas.
----------------------------------------------------

-- Las tablas son la única estructura de datos compuesta:
--   Son arreglos asociativos.
-- De manera similar a los arreglos de PHP u objetos de JS,
-- son diccionarios de búsqueda de hash que también pueden
-- ser usados como listas.

-- Usando tablas como diccionarios / mapas:

-- Los literales de diccionarios usan strings como llaves por defecto:
t = {key1 = 'value1', key2 = false}

-- Se puede acceder a 'key1' usando corchetes '[' y ']':
print(t['key1']) -- => 'value1'

-- Las llaves tipo string pueden usar notación de punto como JS:
print(t.key1)  -- Imprime 'value1'.
t.newKey = {}  -- Añade un nuevo par llave/valor
t.key2 = nil   -- Elimina key2 de la tabla

-- Cualquier literal no nulo puede ser una llave:
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- imprime "tau"

-- La correspondencia de llave es por valor para números
-- y strings, pero es por identidad para tablas.
a = u['@!#']  -- 'a' tiene el valor 'qbert'
b = u[{}]     -- 'b' tiene valor nil
-- 'b' es nil debido a que la búsqueda falló. Esta
-- búsqueda falla porque la llave que usamos es un
-- objeto diferente al que usamos para crear la llave
-- original. Los números y strings son llaves más portables
-- para este propósito.

-- Una llamada de función con un único parámetro tipo tabla no
-- requiere paréntesis.
function h(x) print(x.key1) end
h{key1 = 'Sonmi~451'}  -- Imprime 'Sonmi~451'.

for key, val in pairs(u) do  -- Iteración llave/valor sobre una tabla
  print(key, val)
end

-- _G es una tabla especial para todos los globales
print(_G['_G'] == _G)  -- Imprime 'true'.

-- En este caso, la variable global t se puede consultar de esta manera
t = 6
print(_G['t']) -- Imprime '6'

-- Usando tablas como listas / arreglos:

-- Las listas de literales usan implícitamente enteros como llaves
v = {'value1', 'value2', 1.21, 'gigawatts'}
for i = 1, #v do  -- #v es el tamaño de la lista v
  print(v[i])  -- Los índices inician en 1. ¡Qué locura!
end
-- No existe un tipo de dato "Lista". v es sólo una
-- tabla con llaves enteras consecutivas.

----------------------------------------------------
-- 3.1 Metatablas y Metamétodos.
----------------------------------------------------

-- Una tabla puede tener una metatabla que otorga a la tabla
-- comportamientos similares a sobrecarga de operadores. Más
-- tarde veremos cómo las metatablas soportan el comportamiento
-- de prototipos de JavaScript.

f1 = {a = 1, b = 2}  -- Representa la fracción a / b
f2 = {a = 2, b = 3}

-- Esto puede fallar:
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

s = f1 + f2  -- Esto llama la función __add(f1, f2) de la metatabla

-- f1 y f2 no tienen llave para su metatabla, a diferencia
-- de los prototipos de JS, así que se debe recuperar usando
-- getmetatable(f1). La metatabla es sólo una tabla normal con
-- llave que Lua reconoce, como "__add".

-- Pero la siguiente línea falla ya que s no tiene metatabla.
-- t = s + s
-- Los patrones tipo clase a continuación solucionan ese problema.

-- Una llave __index en una metatabla sobrecarga las consultas de punto:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- ¡Funciona! Gracias, metatabla.

-- Las consultas a la tabla que fallen serán reintentadas
-- en el valor __index de la metatabla, de manera recursiva.

-- Un valor __index también puede ser una function(tbl, key)
-- para consultas más avanzadas.

-- Los valores de __index, __add... son llamados metamétodos.
-- Acá hay una lista completa con los metamétodos:

-- __add(a, b)                     para a + b
-- __sub(a, b)                     para a - b
-- __mul(a, b)                     para a * b
-- __div(a, b)                     para a / b
-- __mod(a, b)                     para a % b
-- __pow(a, b)                     para a ^ b
-- __unm(a)                        para -a
-- __concat(a, b)                  para a .. b
-- __len(a)                        para #a
-- __eq(a, b)                      para a == b
-- __lt(a, b)                      para a < b
-- __le(a, b)                      para a <= b
-- __index(a, b)  <fn or a table>  para a.b
-- __newindex(a, b, c)             para a.b = c
-- __call(a, ...)                  para a(...)

----------------------------------------------------
-- 3.2 Tablas como clases y herencia.
----------------------------------------------------

-- Aunque las clases no están incorporadas, existen maneras
-- diferentes de hacerlas usando tablas y metatablas.

-- La explicación de este ejemplo está justo debajo:

Dog = {}                                   -- 1.

function Dog:new()                         -- 2.
  newObj = {sound = 'woof'}                -- 3.
  self.__index = self                      -- 4.
  return setmetatable(newObj, self)        -- 5.
end

function Dog:makeSound()                   -- 6.
  print('I say ' .. self.sound)
end

mrDog = Dog:new()                          -- 7.
mrDog:makeSound()  -- 'I say woof'         -- 8.

-- 1. Dog actúa como una clase, aunque es sólo una tabla
-- 2. function table:fn(...) es lo mismo que
--    function table.fn(self, ...)
--    El operador ':' añade un primer argumento llamado self.
--    Lea 7 y 8 para entender cómo self obtiene su valor.
-- 3. newObj será una instancia de clase Dog
-- 4. self = la clase siendo instanciada. Usualmente,
--    self sería Dog, pero la herencia puede cambiar eso.
--    newObj obtiene las funciones de self cuando establecemos
--    la metatabla e __index de newObj a self.
-- 5. Recordatorio: setmetatable retorna su primer argumento.
-- 6. El operador ':' funciona igual que en 2, pero esta vez
--    esperamos que self sea una instancia de la clase.
-- 7. Lo mismo que Dog.new(Dog), por lo tanto self = Dog en new().
-- 8. Lo mismo que mrDog.makeSound(mrDog), self = mrDog.

----------------------------------------------------

-- Ejemplo de herencia:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  s = self.sound .. ' '                       -- 2.
  print(s .. s .. s)
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof'      -- 4.

-- 1. Loud dog obtiene los métodos y variables de Dog
-- 2. self tiene una llave 'sound' obtenido de new(), ver 3.
-- 3. Lo mismo que LoudDog.new(LoudDog), y convertido a
--    Dog.new(LoudDog) ya que LoudDog no tiene llave 'new',
--    pero tiene __index = Dog en su metatabla.
--    Resultado: La metatabla de seymour es LoudDog, y
--    LoudDog.__index = LoudDog. Así que seymour.key
--    = seymour.key, LoudDog.key o Dog.key, dependiendo de
--    cuál tabla sea la primera con la llave dada.
-- 4. La llave 'makeSound' se encuentra en LoudDog:
--    Es lo mismo que LoudDog.makeSound(seymour).

-- Si es requerido, el 'new()' de una subclase es igual
-- al de la clase base.
function LoudDog:new()
  newObj = {}
  -- set up newObj
  self.__index = self
  return setmetatable(newObj, self)
end

----------------------------------------------------
-- 4. Módulos.
----------------------------------------------------


--[[ Comento esta sección del código para que el resto
     del script siga siendo ejecutable
```

```lua
-- Supongamos que el archivo mod.lua se ve así:
local M = {}

local function sayMyName()
  print('Hrunkner')
end

function M.sayHello()
  print('Why hello there')
  sayMyName()
end

return M

-- Otro archivo puede usar las funcionalidades de mod.lua
local mod = require('mod')  -- Corre el archivo mod.lua

-- 'require' es la función estándar para incluir módulos
-- 'require' funciona así (si no ha sido almacenado en caché, ver abajo)
local mod = (function ()
  <contenidos de mod.lua>
end)()
-- Es como si mod.lua fuese el cuerpo de una función, de tal manera
-- que los locales de mod.lua son invisibles fuera de él.

-- Esto funciona porque mod es igual a M dentro de mod.lua
mod.sayHello() -- Imprime: Why hello there Hrunkner

-- Esto es erróneo. sayMyName sólo existe dentro de mod.lua:
mod.sayMyName()  -- error
-- El valor de 'require' es guardado en caché, así que cada archivo
-- se ejecuta máximo una vez, incluso si se usa 'require' varias veces.

-- Suponga que mod2.lua contiene "print('Hi!')"
local a = require('mod2')  -- Imprime 'Hi!'
local b = require('mod2')  -- No imprime. También, a = b

-- 'dofile' es similar a require pero no usa caché.
dofile('mod2.lua')  --> Hi!
dofile('mod2.lua')  --> Hi! (lo ejecuta nuevamente)

-- 'loadfile' carga un archivo lua, pero no lo ejecuta
f = loadfile('mod2.lua')  -- Se puede llamar f() para ejecutarlo.

-- 'load' es como 'loadfile' para strings que contengan código lua
-- ('loadstring' es obsoleto, se prefiere el uso de 'load')
g = load('print(343)')  -- Retorna una función
g()  -- Imprime '343', nada es impreso antes de esto.

--]]
```

## Referencias

Estaba emocionado por aprender lua para poder crear juegos
con el motor de juegos [LÖVE](http://love2d.org/). Ese es el por qué.

Empecé con [BlackBulletIV para programadores Lua](https://ebens.me/posts/lua-for-programmers-part-1/).
Luego, leí el libro oficial de [Programación en Lua](http://www.lua.org/pil/contents.html).
Ese es el cómo.

Podría serle útil darle un vistazo a
[Lua Short Reference](http://lua-users.org/wiki/LuaShortReference) en lua-users.org.

Los principales temas no cubiertos son las librerías estándar:

* [Librería de strings](http://lua-users.org/wiki/StringLibraryTutorial)
* [Librería de tablas](http://lua-users.org/wiki/TableLibraryTutorial)
* [Librería de matemáticas](http://lua-users.org/wiki/MathLibraryTutorial)
* [Librería de Entrada/Salida (`io`)](http://lua-users.org/wiki/IoLibraryTutorial)
* [Libreria de Sistema Operativo (`os`)](http://lua-users.org/wiki/OsLibraryTutorial)

Por cierto, el archivo entero es código Lua válido. ¡Guárdelo como
aprendiendo.lua y ejecútelo con el comando "`lua aprendiendo.lua`" !

¡Que se divierta con lua!
