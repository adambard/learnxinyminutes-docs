---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
translators:
  - ["Juraj Kostolanský", "http://www.kostolansky.sk"]
lang: sk-sk
filename: coffeescript-fr.coffee
---

CoffeeScript je jazyk, ktorý sa kompiluje do ekvivalentného JavaScriptu,
neexistuje peňho interpretácia počas behu programu (runtime).
CoffeeScript sa snaží vytvárať čitateľný, pekne formátovaný a optimalizovaný
JavaScriptový kód pracujúci pod každým JavaScriptovým prostredím.

Pozri tiež [stránku CoffeeScript](http://coffeescript.org/), ktoré obsahuje kompletný tutoriál o CoffeeScripte.

```coffeescript
# CoffeeScript je jazyk hipsterov.
# Ide s trendom mnohých moderných jazykov.
# Komentáre sú podobné tým v Ruby a Pythone, používajú symbol #.

###
Blokové komentáre vyzerajú takto, prekladajú sa priamo do '/ * ... * /'
pre výsledný kód JavaScriptu.

Predtým, než budeš pokračovať, mal by si rozumieť sémantike JavaScriptu.
###

# Priradenia:
cislo = 42   #=> var cislo = 42;
opak  = true #=> var opak = true;

# Podmienky:
cislo = -42 if opak #=> if(opak) { cislo = -42; }

# Funkcie:
stvorec = (x) -> x * x #=> var stvorec = function(x) { return x * x; }

vypln = (nadoba, tekutina = "káva") ->
  "#{nadoba} sa napĺňa tekutinou #{tekutina}..."
#=>var vypln;
#
#vypln = function(nadoba, tekutina) {
#  if (tekutina == null) {
#    tekutina = "káva";
#  }
#  return nadoba + " sa napĺňa tekutinou " + tekutina + "...";
#};

# Rozsahy:
zoznam = [1..5] #=> var zoznam = [1, 2, 3, 4, 5];

# Objekty:
matika =
  zaklad:  Math.sqrt
  stvorec: square
  kocka:   (x) -> x * square x
#=> var matika = {
#  "zaklad": Math.sqrt,
#  "stvorec": square,
#  "kocka": function(x) { return x * square(x); }
#}

# Splat operátor:
zavod = (vitaz, bezci...) ->
  print vitaz, bezci
#=>zavod = function() {
#  var vitaz, bezci;
#  vitaz = arguments[0],
#  bezci = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#  return print(vitaz, bezci);
#};

# Existencia:
alert "Vedel som to!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null)
# { alert("Vedel som to!"); }

# Pole:
kocky = (matika.kocka cislo for cislo in zoznam)
#=>kocky = (function() {
#	var _i, _len, _results;
#	_results = [];
# 	for (_i = 0, _len = zoznam.length; _i < _len; _i++) {
#		cislo = zoznam[_i];
#		_results.push(matika.kocka(cislo));
#	}
#	return _results;
#  })();

jedla = ['brokolica', 'špenát', 'čokoláda']
zjedz jedlo for jedlo in jedla when jedlo isnt 'čokoláda'
#=>jedla = ['brokolica', 'špenát', 'čokoláda'];
#
#for (_k = 0, _len2 = jedla.length; _k < _len2; _k++) {
#  jedlo = jedla[_k];
#  if (jedlo !== 'čokoláda') {
#    zjedz(jedlo);
#  }
#}
```

## Ďalšie zdroje

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
