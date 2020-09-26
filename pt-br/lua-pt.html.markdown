---
language: Lua
contributors:
    - ["Tyler Neylon", "http://tylerneylon.com/"]
filename: learnlua.lua
translators:
    - ["Iaan Mesquita", "https://github.com/ianitow"]
lang: pt-br
---

```lua
-- Dois hífens começam um comentário de uma linha.

--[[
     Adicionar dois [  ] (colchetes) criam um comentário
     de múltiplas linhas.
--]]

----------------------------------------------------
-- 1. Variáveis e fluxo de controle.
----------------------------------------------------

num = 42  -- Todos os números são doubles.
-- Não se preocupe, doubles de 64-bits contém 52 bits para
-- armazenar corretamente valores int; a precisão da máquina
-- não é um problema para ints que são < 52 bits.

s = 'alternados'  -- String são imutáveis, como em Python.
t = "Aspas duplas também são válidas"
u = [[ Dois colchetes
       começam e terminam
       strings de múltiplas linhas.]]
t = nil  -- Torna t undefined(indefinido); Lua tem um Garbage Collector.

-- Blocos são representados com palavras do/end:
while num < 50 do
  num = num + 1  -- Sem operadores do tipo ++ ou +=
end

--Cláusula If :
if num > 40 then
  print('over 40')
elseif s ~= 'walternate' then  -- ~= signfica não é igual.
  -- Para fazer checagem use == como em Python; Funciona para comparar strings também.
  io.write('not over 40\n')  -- Padrão para saídas.
else
  -- Variáveis são globais por padrão.
  thisIsGlobal = 5  -- Camel case é o comum.

  -- Como fazer variáveis locais:
  local line = io.read()  -- Leia a proxima linha de entrada.

  -- Para concatenação de strings use o operador .. :
  print('Winter is coming, ' .. line)
end

-- Variáveis indefinidas são do tipo nil.
-- Isso não é um erro:
foo = anUnknownVariable  -- Agora foo = nil.

aBoolValue = false

-- Apenas nil e false são do tipo falso; 0 e '' são verdadeiros!
if not aBoolValue then print('twas false') end

-- 'or' e 'and' são operadores lógicos.
-- Esse operador em C/JS a?b:c , em lua seria o mesmo que:
ans = aBoolValue and 'yes' or 'no'  --> 'no'

karlSum = 0
for i = 1, 100 do  -- O intervalo inclui inicio e fim.
  karlSum = karlSum + i
end

-- Use "100, 1, -1" para um intervalo que diminui:
fredSum = 0
for j = 100, 1, -1 do fredSum = fredSum + j end

-- Em geral, o intervalo é começo, fim[, etapas].

-- Outro construtor de loop:
repeat
  print('A estrada do futuro.')
  num = num - 1
until num == 0


----------------------------------------------------
-- 2. Funções.
----------------------------------------------------

function fib(n)
  if n < 2 then return 1 end
  return fib(n - 2) + fib(n - 1)
end

-- Closures e Funções anônimas são permitidas:
function adder(x)
  -- O retorno da função é criado quando adder é
  -- chamado, e ele sabe o valor de x:
  return function (y) return x + y end
end
a1 = adder(9)
a2 = adder(36)
print(a1(16))  --> 25
print(a2(64))  --> 100

-- Retornos, chamadas de funções e atribuições, todos eles trabalham
-- com listas que podem ter tamanhos incompatíveis.
-- Receptores incompatpiveis serão nil;
-- Destinos incompatíveis serão descartados.

x, y, z = 1, 2, 3, 4
-- Agora x = 1, y = 2, z = 3, e 4 é jogado fora.

function bar(a, b, c)
  print(a, b, c)
  return 4, 8, 15, 16, 23, 42
end

x, y = bar('zaphod')  --> imprime "zaphod  nil nil"
-- Agora x = 4, y = 8, os valores 15...42 foram descartados.

-- Funções são de primeira-classe, portanto podem ser local/global.
-- Estes exemplos são equivalentes:
function f(x) return x * x end
f = function (x) return x * x end

-- Logo, estes são equivalentes também:
local function g(x) return math.sin(x) end
local g; g  = function (x) return math.sin(x) end
--  'local g' essa declaração de auto-referência é válida.

-- A propósito, as funções de trigonometria trabalham em radianos.

-- Chamadas de função com apenas um parâmetro de string não precisam de parênteses:
print 'hello'  -- Funciona perfeitamente.


----------------------------------------------------
-- 3. Tabelas.
----------------------------------------------------

-- Tabelas = A unica estrutura de dados composta em Lua;
--          elas são matrizes associativas.
-- Semelhantes aos arrays de PHP ou objetos de javascript, eles são:
-- hash-lookup(chave:valor) que também podem ser usados como listas.

-- Usando tabelas como dicionário / mapas:

-- Dicionários literais tem strings como chaves por padrão:
t = {key1 = 'value1', key2 = false}

-- As chaves do tipo string podem usar notação de ponto,semelhante a javascript:
print(t.key1)  -- Imprime 'value1'.
t.newKey = {}  -- Adiciona um novo par chave/valor.
t.key2 = nil   -- Remove key2 da tabela.

-- Qualquer notação literal (não-nulo) pode ser uma chave:
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'}
print(u[6.28])  -- imprime "tau"

-- A correspondência de chave é basicamente o valor para números
-- e strings, mas por identidade para tabelas.
a = u['@!#']  -- Agora a = 'qbert'.
b = u[{}]     -- Nós esperavamso o resultado 1729, mas ele é nil:
-- b = nil já que a busca falha. Ela falha
-- porque a chave que usamos não é a mesma que o objeto
-- como aquele que usamos para guardar o valor original. Por isso
-- strings & numeros são chaves mais recomendadas.

-- Uma chamada de função de apenas um paramêtro de tabela, não precisa de parênteses:

function h(x) print(x.key1) end
h{key1 = 'Sonmi~451'}  -- Imprime 'Sonmi~451'.

for key, val in pairs(u) do  -- Iteração de tabela.
  print(key, val)
end

-- _G é uma tabela especial que guarda tudo que é global.
print(_G['_G'] == _G)  -- Imprime 'true'.

-- Usando tabelas como listas / arrays:

-- Listas literais com chaves int implícitas:
v = {'value1', 'value2', 1.21, 'gigawatts'}
for i = 1, #v do  -- #v é o tamanho de v
  print(v[i])  -- Índices começam em 1 !! MUITO LOCO!
end
-- Uma 'list' não é um tipo real. v é apenas uma tabela
-- com chaves int consecutivas, tratando ela como uma lista.

----------------------------------------------------
-- 3.1 Metatabelas e metamétodos.
----------------------------------------------------

-- Uma tabela pode ter uma metatabela que fornece à tabela
-- um compotamento de sobrecarga de operador. Depois veremos
-- como metatabelas suportam o comportamento do Javascript-prototype.

f1 = {a = 1, b = 2}  -- Representa uma fração de a/b.
f2 = {a = 2, b = 3}

-- Isso falharia:
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

s = f1 + f2  -- chama __add(f1, f2) na metatabela de f1

-- f1, f2 não tem chave para sua metatabela, ao contrário de
-- prototypes em javascript, então você deve recuperá-lo com
-- getmetatable(f1). A metatabela é uma tabela normal
-- com chaves que Lua reconhece, como __add.

-- Mas a proxima linha irá falhar porque s não tem uma metatabela:
-- t = s + s
-- O padrão de Classes abaixo consertam esse problema.

-- Uma __index em uma metatable sobrecarrega pesquisas de ponto:
defaultFavs = {animal = 'gru', food = 'donuts'}
myFavs = {food = 'pizza'}
setmetatable(myFavs, {__index = defaultFavs})
eatenBy = myFavs.animal  -- Funciona! obrigado, metatabela.

-- As pesquisas diretas de tabela que falham tentarão pesquisar novamente usando
-- o __index da metatabela, e isso é recursivo.

-- Um valor de __index também pode ser uma function(tbl, key)
-- para pesquisas mais personalizadas.

-- Valores do tipo __index,add, .. são chamados de metamétodos.
-- Uma lista completa com os metamétodos.

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
-- 3.2 Tabelas como Classes e sua herança.
----------------------------------------------------

-- Classes não são disseminadas; existem maneiras diferentes
-- para fazer isso usando tabelas e metamétodos...

-- A explicação para este exemplo está logo abaixo.

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

-- 1. Dog atua como uma classe; mas na verdade, é uma tabela.
-- 2. function tablename:fn(...) é a mesma coisa que
--    function tablename.fn(self, ...)
--    O : apenas adiciona um primeiro argumento chamado self.
--    Leia 7 & 8 abaixo para ver como self obtém seu valor.
-- 3. newObj será uma instância da classe Dog.
-- 4. self = a classe que que foi instanciada. Regularmente
--    self = Dog, mas a herança pode mudar isso.
--    newObj recebe as funções de self como se tivessimos definido em ambos
--    a metatabela de newObj e self __index para self.
-- 5. Lembre-se: setmetatable retorna seu primeiro argumento definido.
-- 6. O : funciona como em 2, mas desta vez esperamos que
--    self seja uma instância já instanciada da classe.
-- 7. Igual a Dog.new(Dog), logo self = Dog no new().
-- 8. Igual a mrDog.makeSound(mrDog); self = mrDog.

----------------------------------------------------

-- Heranças exemplos:

LoudDog = Dog:new()                           -- 1.

function LoudDog:makeSound()
  s = self.sound .. ' '                       -- 2.
  print(s .. s .. s)
end

seymour = LoudDog:new()                       -- 3.
seymour:makeSound()  -- 'woof woof woof'      -- 4.

-- 1. LoudDog recebe os metodos e variáveis de Dog.
-- 2. self tem uma chave 'sound' vindo de new(), veja o 3.
-- 3. Mesma coisa que LoudDog.new(LoudDog), convertido para
--    Dog.new(LoudDog) como LoudDog não tem uma chave 'new',
--    mas tem uma chave __index = Dog na sua metatabela o
--    resultado será: a metabela de seymour é a LoudDog, e
--    LoudDog.__index = LoudDog. Então seymour.key será
--    = seymour.key, LoudDog.key, Dog.key,seja qual for a primeira
--    chave fornecida.
-- 4. A chave 'makeSound' foi encontrada em LoudDog; isto
--    é a mesma coisa que LoudDog.makeSound(seymour).

-- Se precisar de, uma subclasse de new() como uma base:
function LoudDog:new()
  newObj = {}
  -- define newObj
  self.__index = self
  return setmetatable(newObj, self)
end

----------------------------------------------------
-- 4. Módulos.
----------------------------------------------------


--[[ Estou comentando esta seção, então o resto
--   desse script é executável.
```

```lua
-- Suponhamos que o arquivo mod.lua se pareça com isso:
local M = {}

local function sayMyName()
  print('Hrunkner')
end

function M.sayHello()
  print('Why hello there')
  sayMyName()
end

return M

-- Outro arquivo pode usar as funcionalidades de mod.lua:
local mod = require('mod')  -- Roda o arquivo mod.lua.

-- require é a forma que usamos para incluir módulos.
-- require atua como:     (se não for cacheado; veja abaixo)
local mod = (function ()
  <contents of mod.lua>
end)()
-- É como se mod.lua fosse um corpo de uma função, então
-- os locais dentro de mod.lua são invisíveis fora dele.

-- Isso irá funcionar porque mod aqui = M dentro de mod.lua:
mod.sayHello()  -- Diz olá para Hrunkner.

-- Isso aqui é errado; sayMyName existe apenas em mod.lua:
mod.sayMyName()  -- erro

-- valores retornados de require são armazenados em cache para que um arquivo seja
-- execute no máximo uma vez, mesmo quando é exigidos várias vezes.

-- Suponhamos que mod2.lua contém "print('Hi!')".
local a = require('mod2')  -- Imprime Hi!
local b = require('mod2')  -- Não imprime;pois a=b.

-- dofile é parecido com require, porém sem cacheamento:
dofile('mod2.lua')  --> Hi!
dofile('mod2.lua')  --> Hi! (roda novamente)

-- loadfile carrega um arquivo lua, porém não o executa.
f = loadfile('mod2.lua')  -- Chame f() para executar.

-- loadstring é um loadfile para strings.
g = loadstring('print(343)')  -- Retorna uma função.
g()  -- Imprime 343; nada foi impresso antes disso.

--]]

```

## Referências

Fiquei bastante animado para aprender Lua pois consegui fazer jogos
com a <a href="http://love2d.org/">Love 2D engine de jogos</a>.

Eu comecei com <a href="http://nova-fusion.com/2012/08/27/lua-for-programmers-part-1/">BlackBulletIV's para programadores LUA</a>.
Em seguida, eu li a documentação oficial <a href="https://www.lua.org/manual/5.1/pt/index.html#contents">Programando em Lua</a>.
É assim que se começa.

Pode ser útil conferir <a href="http://lua-users.org/files/wiki_insecure/users/thomasl/luarefv51.pdf">Uma pequena referencia sobre LUA</a> em lua-users.org.

Os principais tópicos não cobertos, são as bibliotecas padrões:

- <a href="http://lua-users.org/wiki/StringLibraryTutorial">Biblioteca de strings</a>
- <a href="http://lua-users.org/wiki/TableLibraryTutorial">Biblioteca de tabelas</a>
- <a href="http://lua-users.org/wiki/MathLibraryTutorial">Biblioteca de matemática</a>
- <a href="http://lua-users.org/wiki/IoLibraryTutorial">Biblioteca de entrada/saída</a>
- <a href="http://lua-users.org/wiki/OsLibraryTutorial">Biblioteca do sistema operacional</a>

A propósito, todo este arquivo é um código LUA válido, salve-o como
aprenda.lua e rode-o com "lua aprenda.lua" !

Este guia foi escrito pela primeira vez por tylerneylon.com, e agora
também disponível em <a href="https://gist.github.com/tylerneylon/5853042">github gist</a>. E também em português.

Se divirta com lua
