---
language: Lua
lang: zh-cn
contributors: 
    - ["Tyler Neylon", "http://tylerneylon.com/"]
    - ["Rob Hoelz", "http://hoelz.ro"]
    - ["Jakukyo Friel", "http://weakish.github.io"]
    - ["Craig Roddin", "craig.roddin@gmail.com"]
    - ["Amr Tamimi", "https://amrtamimi.com"]
translators:
    - ["Jakukyo Friel", "http://weakish.github.io"]
filename: lua-cn.lua
---

```lua
-- 单行注释以两个连字符开头 

--[[ 
     多行注释
--]]

---------------------------------------------------- 
-- 1. 变量和流程控制
---------------------------------------------------- 

num = 42  -- 所有的数字都是双精度浮点型。
-- 别害怕，64位的双精度浮点型数字中有52位用于 
-- 保存精确的整型值; 对于52位以内的整型值， 
-- 不用担心精度问题。

s = 'walternate'  -- 和Python一样，字符串不可变。 
t = "也可以用双引号" 
u = [[ 多行的字符串
       以两个方括号
       开始和结尾。]] 
t = nil  -- 撤销t的定义; Lua 支持垃圾回收。 

-- 块使用do/end之类的关键字标识： 
while num < 50 do 
  num = num + 1  -- 不支持 ++ 或 += 运算符。 
end 

-- If语句： 
if num > 40 then 
  print('over 40') 
elseif s ~= 'walternate' then  -- ~= 表示不等于。 
  -- 像Python一样，用 == 检查是否相等 ；字符串同样适用。 
  io.write('not over 40\n')  -- 默认标准输出。
else 
  -- 默认全局变量。 
  thisIsGlobal = 5  -- 通常使用驼峰。

  -- 如何定义局部变量： 
  local line = io.read()  -- 读取标准输入的下一行。 

  -- ..操作符用于连接字符串： 
  print('Winter is coming, ' .. line) 
end 

-- 未定义的变量返回nil。 
-- 这不是错误： 
foo = anUnknownVariable  -- 现在 foo = nil. 

aBoolValue = false 

--只有nil和false为假; 0和 ''均为真！ 
if not aBoolValue then print('false') end 

-- 'or'和 'and'短路 
-- 类似于C/js里的 a?b:c 操作符： 
ans = aBoolValue and 'yes' or 'no'  --> 'no' 

karlSum = 0 
for i = 1, 100 do  -- 范围包含两端 
  karlSum = karlSum + i 
end 

-- 使用 "100, 1, -1" 表示递减的范围： 
fredSum = 0 
for j = 100, 1, -1 do fredSum = fredSum + j end 

-- 通常，范围表达式为begin, end[, step]. 

-- 循环的另一种结构： 
repeat 
  print('the way of the future') 
  num = num - 1 
until num == 0 

---------------------------------------------------- 
-- 2. 函数。 
---------------------------------------------------- 

function fib(n)
  if n < 2 then return n end
  return fib(n - 2) + fib(n - 1)
end

-- 支持闭包及匿名函数： 
function adder(x) 
  -- 调用adder时，会创建返回的函数，
  -- 并且会记住x的值： 
  return function (y) return x + y end 
end 
a1 = adder(9) 
a2 = adder(36) 
print(a1(16))  --> 25 
print(a2(64))  --> 100 

-- 返回值、函数调用和赋值都可以
-- 使用长度不匹配的list。 
-- 不匹配的接收方会被赋值nil； 
-- 不匹配的发送方会被丢弃。 

x, y, z = 1, 2, 3, 4 
-- x = 1、y = 2、z = 3, 而 4 会被丢弃。 

function bar(a, b, c) 
  print(a, b, c) 
  return 4, 8, 15, 16, 23, 42 
end 

x, y = bar('zaphod')  --> 打印 "zaphod  nil nil" 
-- 现在 x = 4, y = 8, 而值15..42被丢弃。 

-- 函数是一等公民，可以是局部的，也可以是全局的。 
-- 以下表达式等价： 
function f(x) return x * x end 
f = function (x) return x * x end 

-- 这些也是等价的： 
local function g(x) return math.sin(x) end
local g; g = function (x) return math.sin(x) end
-- 以上均因'local g'，使得g可以自引用。
local g = function(x) return math.sin(x) end
-- 等价于 local function g(x)..., 但函数体中g不可自引用

-- 顺便提下，三角函数以弧度为单位。 

-- 用一个字符串参数调用函数，可以省略括号： 
print 'hello'  --可以工作。 

-- 调用函数时，如果只有一个table参数，
-- 同样可以省略括号（table详情见下）：
print {} -- 一样可以工作。

---------------------------------------------------- 
-- 3. Table。 
---------------------------------------------------- 

-- Table = Lua唯一的组合数据结构; 
--         它们是关联数组。 
-- 类似于PHP的数组或者js的对象， 
-- 它们是哈希表或者字典，也可以当列表使用。 

-- 按字典/map的方式使用Table： 

-- Dict字面量默认使用字符串类型的key： 
t = {key1 = 'value1', key2 = false} 

-- 字符串key可以使用类似js的点标记： 
print(t.key1)  -- 打印 'value1'. 
t.newKey = {}  -- 添加新的键值对。 
t.key2 = nil   -- 从table删除 key2。 

-- 使用任何非nil的值作为key： 
u = {['@!#'] = 'qbert', [{}] = 1729, [6.28] = 'tau'} 
print(u[6.28])  -- 打印 "tau" 

-- 数字和字符串的key按值匹配的
-- table按id匹配。 
a = u['@!#']  -- 现在 a = 'qbert'. 
b = u[{}]     -- 我们或许期待的是 1729,  但是得到的是nil: 
-- b = nil ，因为没有找到。 
-- 之所以没找到，是因为我们用的key与保存数据时用的不是同
-- 一个对象。 
-- 所以字符串和数字是移植性更好的key。 

-- 只需要一个table参数的函数调用不需要括号： 
function h(x) print(x.key1) end 
h{key1 = 'Sonmi~451'}  -- 打印'Sonmi~451'. 

for key, val in pairs(u) do  -- 遍历Table
  print(key, val) 
end 

-- _G 是一个特殊的table，用于保存所有的全局变量 
print(_G['_G'] == _G)  -- 打印'true'. 

-- 按列表/数组的方式使用： 

-- 列表字面量隐式添加整数键： 
v = {'value1', 'value2', 1.21, 'gigawatts'} 
for i = 1, #v do  -- #v 是列表的大小
  print(v[i])  -- 索引从 1 开始!! 太疯狂了！ 
end
-- 'list'并非真正的类型，v 其实是一个table， 
-- 只不过它用连续的整数作为key，可以像list那样去使用。 

---------------------------------------------------- 
-- 3.1 元表（metatable） 和元方法（metamethod）。 
---------------------------------------------------- 

-- table的元表提供了一种机制，支持类似操作符重载的行为。
-- 稍后我们会看到元表如何支持类似js prototype的行为。 

f1 = {a = 1, b = 2}  -- 表示一个分数 a/b. 
f2 = {a = 2, b = 3} 

-- 这会失败：
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

s = f1 + f2  -- 调用在f1的元表上的__add(f1, f2) 方法 

-- f1, f2 没有关于元表的key，这点和js的prototype不一样。 
-- 因此你必须用getmetatable(f1)获取元表。
-- 元表是一个普通的table， 
-- 元表的key是普通的Lua中的key，例如__add。 

-- 但是下面一行代码会失败，因为s没有元表： 
-- t = s + s 
-- 下面提供的与类相似的模式可以解决这个问题： 

-- 元表的__index 可以重载用于查找的点操作符： 
defaultFavs = {animal = 'gru', food = 'donuts'} 
myFavs = {food = 'pizza'} 
setmetatable(myFavs, {__index = defaultFavs}) 
eatenBy = myFavs.animal  -- 可以工作！感谢元表 

-- 如果在table中直接查找key失败，会使用
-- 元表的__index 递归地重试。

-- __index的值也可以是function(tbl, key)
-- 这样可以支持自定义查找。 

-- __index、__add等的值，被称为元方法。 
-- 这里是一个table元方法的清单： 

-- __add(a, b)                     for a + b 
-- __sub(a, b)                     for a - b 
-- __mul(a, b)                     for a * b 
-- __div(a, b)                     for a / b 
-- __mod(a, b)                     for a % b 
-- __pow(a, b)                     for a ^ b 
-- __unm(a)                        for -a 
-- __concat(a, b)                  for a .. b 
-- __len(a)                        for #a 
-- __eq(a, b)                      for a == b 
-- __lt(a, b)                      for a < b 
-- __le(a, b)                      for a <= b 
-- __index(a, b)  <fn or a table>  for a.b 
-- __newindex(a, b, c)             for a.b = c 
-- __call(a, ...)                  for a(...) 

---------------------------------------------------- 
-- 3.2 与类相似的table和继承。 
---------------------------------------------------- 

-- Lua没有内建的类；可以通过不同的方法，利用表和元表
-- 来实现类。 

-- 下面是一个例子，解释在后面： 

Dog = {}                                   -- 1. 

function Dog:new()                         -- 2. 
  local newObj = {sound = 'woof'}                -- 3. 
  self.__index = self                      -- 4. 
  return setmetatable(newObj, self)        -- 5. 
end 

function Dog:makeSound()                   -- 6. 
  print('I say ' .. self.sound) 
end 

mrDog = Dog:new()                          -- 7. 
mrDog:makeSound()  -- 'I say woof'         -- 8. 

-- 1. Dog看上去像一个类；其实它是一个table。 
-- 2. 函数tablename:fn(...) 等价于
--    函数tablename.fn(self, ...)
--    冒号（:）只是添加了self作为第一个参数。 
--    阅读7 & 8条 了解self变量是如何得到其值的。 
-- 3. newObj是类Dog的一个实例。 
-- 4. self = 被继承的类。通常self = Dog，不过继承可以改变它。 
--    如果把newObj的元表和__index都设置为self， 
--    newObj就可以得到self的函数。 
-- 5. 备忘：setmetatable返回其第一个参数。 
-- 6. 冒号（：）的作用和第2条一样，不过这里 
--    self是一个实例，而不是类 
-- 7. 等价于Dog.new(Dog)，所以在new()中，self = Dog。 
-- 8. 等价于mrDog.makeSound(mrDog); self = mrDog。 

---------------------------------------------------- 

-- 继承的例子： 

LoudDog = Dog:new()                           -- 1. 

function LoudDog:makeSound() 
  local s = self.sound .. ' '                       -- 2. 
  print(s .. s .. s) 
end 

seymour = LoudDog:new()                       -- 3. 
seymour:makeSound()  -- 'woof woof woof'      -- 4. 

-- 1. LoudDog获得Dog的方法和变量列表。 
-- 2. 因为new()的缘故，self拥有了一个'sound' key，参见第3条。 
-- 3. 等价于LoudDog.new(LoudDog)，转换一下就是 
--    Dog.new(LoudDog)，这是因为LoudDog没有'new' key， 
--    但是它的元表中有 __index = Dog。 
--    结果: seymour的元表是LoudDog，并且 
--    LoudDog.__index = Dog。所以有seymour.key 
--    = seymour.key, LoudDog.key, Dog.key 
--    从其中第一个有指定key的table获取。 
-- 4. 在LoudDog可以找到'makeSound'的key； 
--    等价于LoudDog.makeSound(seymour)。 

-- 如果有必要，子类也可以有new()，与基类相似： 
function LoudDog:new() 
  local newObj = {} 
  -- 初始化newObj 
  self.__index = self 
  return setmetatable(newObj, self) 
end 

---------------------------------------------------- 
-- 4. 模块 
---------------------------------------------------- 


--[[ 我把这部分给注释了，这样脚本剩下的部分可以运行 
```

```lua
-- 假设文件mod.lua的内容类似这样： 
local M = {} 

local function sayMyName() 
  print('Hrunkner') 
end 

function M.sayHello() 
  print('Why hello there') 
  sayMyName() 
end 

return M 

-- 另一个文件可以使用mod.lua的功能： 
local mod = require('mod')  -- 运行文件mod.lua. 

-- require是包含模块的标准做法。 
-- require等价于:     (针对没有被缓存的情况；参见后面的内容) 
local mod = (function () 
  <contents of mod.lua> 
end)() 
-- mod.lua被包在一个函数体中，因此mod.lua的局部变量
-- 对外不可见。 

-- 下面的代码可以工作，因为在这里mod = mod.lua 中的 M： 
mod.sayHello()  -- Says hello to Hrunkner. 

-- 这是错误的；sayMyName只在mod.lua中存在： 
mod.sayMyName()  -- 错误 

-- require返回的值会被缓存，所以一个文件只会被运行一次， 
-- 即使它被require了多次。 

-- 假设mod2.lua包含代码"print('Hi!')"。 
local a = require('mod2')  -- 打印Hi! 
local b = require('mod2')  -- 不再打印; a=b. 

-- dofile与require类似，但是不缓存： 
dofile('mod2')  --> Hi! 
dofile('mod2')  --> Hi! (再次运行，与require不同) 

-- loadfile加载一个lua文件，但是并不运行它。 
f = loadfile('mod2')  -- Calling f() runs mod2.lua. 

-- loadstring是loadfile的字符串版本。 
g = loadstring('print(343)')  --返回一个函数。 
g()  -- 打印343; 在此之前什么也不打印。 

--]] 
```

## 参考



为什么？我非常兴奋地学习lua， 这样我就可以使用[Löve 2D游戏引擎](http://love2d.org/)来编游戏。

怎么做？我从[BlackBulletIV的面向程序员的Lua指南](http://nova-fusion.com/2012/08/27/lua-for-programmers-part-1/)入门。接着我阅读了官方的[Lua编程](http://www.lua.org/pil/contents.html)一书。

lua-users.org上的[Lua简明参考](http://lua-users.org/files/wiki_insecure/users/thomasl/luarefv51.pdf)应该值得一看。

本文没有涉及标准库的内容：
   
* <a href="http://lua-users.org/wiki/StringLibraryTutorial">string library</a>
* <a href="http://lua-users.org/wiki/TableLibraryTutorial">table library</a>
* <a href="http://lua-users.org/wiki/MathLibraryTutorial">math library</a>
* <a href="http://lua-users.org/wiki/IoLibraryTutorial">io library</a>
* <a href="http://lua-users.org/wiki/OsLibraryTutorial">os library</a>

顺便说一下，整个文件是可运行的Lua; 
保存为 learn-cn.lua 用命令 `lua learn-cn.lua` 启动吧！

本文首次撰写于 [tylerneylon.com](http://tylerneylon.com) 同时也有 [github gist](https://gist.github.com/tylerneylon/5853042) 版.

使用Lua，欢乐常在！
