---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
translators:
  - ["asaskevich", "http://github.com/asaskevich"]
filename: learncoffee-ru.coffee
lang: ru-ru
---

CoffeeScript - это небольшой язык, который компилируется один-в-один в эквивалентный код на языке JavaScript, а потому он не интерпретируется во время исполнения JavaScript кода.
Ключевой особенностью CoffeeScript является то, что он пытается создать читабельный, качественно оформленный и плавный JavaScript код, прекрасно работающий в любой среде JavaScript.

Также загляните на официальный сайт [языка](http://coffeescript.org/), где можно найти весьма полное учебное пособие по CoffeeScript.

```coffeescript
# CoffeeScript - язык хипстеров.
# Язык использует самое модное из множества современных языков.
# Эти комментарии по стилю похожи на комментарии Ruby или Python, они используют "решетку" в качестве знака комментария.

###
Блоки комментариев выделяются тремя символами "решетки", в результирующем JavaScript коде они будут преобразованы в  '/ * и '* /'.

Перед тем, как идти далее, Вам нужно понимать семантику JavaScript.
###

# Присвоение:
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# Условия:
number = -42 if opposite #=> if(opposite) { number = -42; }

# Функции:
square = (x) -> x * x #=> var square = function(x) { return x * x; }

fill = (container, liquid = "coffee") ->
  "Заполняем #{container} жидкостью #{liquid}..."
#=>var fill;
#
#fill = function(container, liquid) {
#  if (liquid == null) {
#    liquid = "coffee";
#  }
#  return "Заполняем " + container + " жидкостью " + liquid + "...";
#};

# Списки и диапазоны:
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# Объекты:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#  "root": Math.sqrt,
#  "square": square,
#  "cube": function(x) { return x * square(x); }
#}

# Многоточия:
race = (winner, runners...) ->
  print winner, runners
#=>race = function() {
#  var runners, winner;
#  winner = arguments[0], runners = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#  return print(winner, runners);
#};

# Проверка на существование объекта:
alert "Так и знал!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("Так и знал!"); }

# Итерации по массивам:
cubes = (math.cube num for num in list) 
#=>cubes = (function() {
#	var _i, _len, _results;
#	_results = [];
# 	for (_i = 0, _len = list.length; _i < _len; _i++) {
#		num = list[_i];
#		_results.push(math.cube(num));
#	}
#	return _results;
#  })();

foods = ['broccoli', 'spinach', 'chocolate']
eat food for food in foods when food isnt 'chocolate'
#=>foods = ['broccoli', 'spinach', 'chocolate'];
#
#for (_k = 0, _len2 = foods.length; _k < _len2; _k++) {
#  food = foods[_k];
#  if (food !== 'chocolate') {
#    eat(food);
#  }
#}
```

## На почитать

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
- [CoffeeScript на русском](http://cidocs.ru/coffeescript/)
