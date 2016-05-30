---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
translators:
  - ["Tamás Diószegi", "http://github.com/ditam"]
lang: hu-hu
filename: coffeescript-hu.coffee
---

A CoffeeScript egy apró nyelv ami egy-az-egyben egyenértékű Javascript kódra fordul, és így futásidőben már nem szükséges interpretálni.
Mint a JavaScript egyik követője, a CoffeeScript mindent megtesz azért, hogy olvasható, jól formázott és jól futó JavaScript kódot állítson elő, ami minden JavaScript futtatókörnyezetben jól működik.

Rézletekért lásd még a [CoffeeScript weboldalát](http://coffeescript.org/), ahol egy teljes CoffeScript tutorial is található.

```coffeescript
# A CoffeeScript egy hipszter nyelv.
# Követi több modern nyelv trendjeit.
# Így a kommentek, mint Ruby-ban és Python-ban, a szám szimbólummal kezdődnek.

###
A komment blokkok ilyenek, és közvetlenül '/ *' és '* /' jelekre fordítódnak
az eredményül kapott JavaScript kódban.

Mielőtt tovább olvasol, jobb, ha a JavaScript alapvető szemantikájával
tisztában vagy.

(A kód példák alatt kommentként látható a fordítás után kapott JavaScript kód.)
###

# Értékadás:
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# Feltételes utasítások:
number = -42 if opposite #=> if(opposite) { number = -42; }

# Függvények:
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

# Szám tartományok:
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# Objektumok:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#    "root": Math.sqrt,
#    "square": square,
#    "cube": function(x) { return x * square(x); }
#   };

# "Splat" jellegű függvény-paraméterek:
race = (winner, runners...) ->
  print winner, runners
#=>race = function() {
#    var runners, winner;
#    winner = arguments[0], runners = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#    return print(winner, runners);
#  };

# Létezés-vizsgálat:
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# Tömb értelmezések: (array comprehensions)
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

## További források

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
