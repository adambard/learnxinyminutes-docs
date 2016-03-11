---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
translators:
    - ["Bogdan Lazar", "http://twitter.com/tricinel"]
filename: coffeescript-ro.coffee
lang: ro-ro
---

CoffeeScript este un limbaj de programare care este compilat in Javascript. Nu exista un interpretator la runtime-ul aplicatiei. Fiind unul din successorii Javascript, CoffeeScript incearca sa compileze Javascript usor de citit si performant.

Mai cititi si [website-ul CoffeeScript](http://coffeescript.org/), care contine un tutorial complet Coffeescript.

```coffeescript
# CoffeeScript este un limbaj de hipster.
# Se foloseste de trendurile multor limbaje moderne de programare.
# Comentarii sunt ca in Ruby sau Python.

###
Comentariile in bloc sunt create cu `###`, iar acestea sunt transformate in `/*` si `*/` pentru Javascript

Ar trebuie sa intelegeti Javascript pentru a continua cu acest ghid.
###

# Atribuirea valorilor:
numar   = 42 #=> var numar = 42;
opus = true #=> var opus = true;

# Conditii:
numar = -42 if opus #=> if(opus) { numar = -42; }

# Functii:
laPatrat = (x) -> x * x #=> var laPatrat = function(x) { return x * x; }

plin = (recipient, lichid = "cafea") ->
  "Umplem #{recipient} cu #{cafea}..."
#=>var plin;
#
#plin = function(recipient, lichid) {
#  if (lichid == null) {
#    lichid = "cafea";
#  }
#  return "Umplem " + recipient + " cu " + lichid + "...";
#};

# Liste:
lista = [1..5] #=> var lista = [1, 2, 3, 4, 5];

# Obiecte:
matematica =
  radacina:   Math.sqrt
  laPatrat: laPatrat
  cub:   (x) -> x * square x
#=> var matematica = {
#    "radacina": Math.sqrt,
#    "laPatrat": laPatrat,
#    "cub": function(x) { return x * square(x); }
#   };

# Splats:
cursa = (castigator, alergatori...) ->
  print castigator, alergatori
#=>cursa = function() {
#    var alergatori, castigator;
#    castigator = arguments[0], alergatori = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#    return print(castigator, alergatori);
#  };

# Verificarea existentei:
alert "Stiam eu!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("Stiam eu!"); }

# Operatiuni cu matrice:
cuburi = (math.cube num for num in list)
#=>cuburi = (function() {
#	  var _i, _len, _results;
#	  _results = [];
# 	for (_i = 0, _len = list.length; _i < _len; _i++) {
#		  num = list[_i];
#		  _results.push(math.cube(num));
#	  }
#	  return _results;
# })();

alimente = ['broccoli', 'spanac', 'ciocolata']
mananca aliment for aliment in alimente when aliment isnt 'ciocolata'
#=>alimente = ['broccoli', 'spanac', 'ciocolata'];
#
#for (_k = 0, _len2 = alimente.length; _k < _len2; _k++) {
#  aliment = alimente[_k];
#  if (aliment !== 'ciocolata') {
#    eat(aliment);
#  }
#}
```

## Resurse aditionale

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
