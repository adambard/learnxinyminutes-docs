---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
filename: coffeescript-ms.coffee
translators:
    - ["hack1m", "https://github.com/hack1m"]
lang: ms-my
---

CoffeeScript adalah bahasa kecil yang menyusun/kompil satu-per-satu menjadi setara JavaScript, dan tidak ada interpretasi di runtime.
Sebagai salah satu pengganti kepada JavaScript, CoffeeScript mencuba yang terbaik untuk output kod JavaScript yang mudah dibaca, cantik-dicetak dan berfungsi lancar, yang mana berfungsi baik pada setiap runtime JavaScript.

Lihat juga [Laman sesawang CoffeeScript](http://coffeescript.org/), yang mana ada tutorial lengkap untuk CoffeeScript.

```coffeescript
# CoffeeScript adalah bahasa hipster.
# Ia beredar mengikut trend kebanyakkan bahasa moden.
# Jadi komen sama seperti Ruby dan Python, ia menggunakan simbol nombor.

###
Blok komen seperti ini, dan ia terjemah terus ke '/ *'s dan '* /'s
untuk keputusan kod JavaScript.

Sebelum meneruskan anda perlu faham kebanyakkan daripada
JavaScript adalah semantik.
###

# Menetapkan:
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# Bersyarat:
number = -42 if opposite #=> if(opposite) { number = -42; }

# Fungsi:
square = (x) -> x * x #=> var square = function(x) { return x * x; }

fill = (container, liquid = "coffee") ->
  "Filling the #{container} with #{liquid}..."
#=>var fill;
#
#fill = function(container, liquid) {
#  if (liquid == null) {
#    liquid = "coffee";
#  }
#  return "Filling the " + container + " with " + liquid + "...";
#};

# Julat:
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# Objek:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#    "root": Math.sqrt,
#    "square": square,
#    "cube": function(x) { return x * square(x); }
#   };

# Splats:
race = (winner, runners...) ->
  print winner, runners
#=>race = function() {
#    var runners, winner;
#    winner = arguments[0], runners = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#    return print(winner, runners);
#  };

# Kewujudan:
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# Pemahaman array:
cubes = (math.cube num for num in list)
#=>cubes = (function() {
#	  var _i, _len, _results;
#	  _results = [];
# 	for (_i = 0, _len = list.length; _i < _len; _i++) {
#		  num = list[_i];
#		  _results.push(math.cube(num));
#	  }
#	  return _results;
# })();

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

## Sumber tambahan

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
