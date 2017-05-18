---
language: coffeescript
contributors:
  - ["Luca 'Kino' Maroni", "http://github.com/kino90"]
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
filename: coffeescript-it.coffee
lang: it-it
---

CoffeeScript è un piccolo linguaggio che compila direttamente nell'equivalente
JavaScript, non c'è nessuna interpretazione a runtime. Come possibile 
successore di Javascript, CoffeeScript fa il suo meglio per restituire 
un codice leggibile, ben stampato e performante in ogni ambiente JavaScript.

Guarda anche [il sito di CoffeeScript](http://coffeescript.org/), che ha una 
guida completa a CoffeeScript.

```coffeescript
# CoffeeScript è un linguaggio hipster.
# Segue le mode di alcuni linguaggi moderni.
# Quindi i commenti sono come quelli di Ruby e Python, usano il cancelletto.

###
I blocchi di commenti sono definiti con tre cancelletti, che vengono tradotti 
direttamente in `/*` e `*/` nel codice JavaScript risultante.

Prima di continuare devi conoscere la maggior parte
delle semantiche JavaScript.
###

# Assegnamento:
numero   = 42 #=> var numero = 42;
contrario = true #=> var contrario = true;

# Condizioni:
numero = -42 if contrario #=> if(contrario) { numero = -42; }

# Funzioni:
quadrato = (x) -> x * x #=> var quadrato = function(x) { return x * x; }

riempi = (contenitore, liquido = "caffè") ->
  "Sto riempiendo #{contenitore} con #{liquido}..."
#=>var riempi;
#
#riempi = function(contenitore, liquido) {
#  if (liquido == null) {
#    liquido = "caffè";
#  }
#  return "Sto riempiendo " + contenitore + " con " + liquido + "...";
#};

# Intervalli:
lista = [1..5] #=> var lista = [1, 2, 3, 4, 5];

# Oggetti:
matematica =
  radice:   Math.sqrt
  quadrato: quadrato
  cubo:   (x) -> x * quadrato x
#=> var matematica = {
#     "radice": Math.sqrt,
#     "quadrato": quadrato,
#     "cubo": function(x) { return x * quadrato(x); }
#   }

# Splats:
gara = (vincitore, partecipanti...) ->
  print vincitore, partecipanti
#=>gara = function() {
#    var partecipanti, vincitore;
#    vincitore = arguments[0], partecipanti = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#    return print(vincitore, partecipanti);
#  };

# Esistenza:
alert "Lo sapevo!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("Lo sapevo!"); }

# Comprensione degli Array:
cubi = (matematica.cubo num for num in lista)
#=>cubi = (function() {
#	   var _i, _len, _results;
#	   _results = [];
#    for (_i = 0, _len = lista.length; _i < _len; _i++) {
#      num = lista[_i];
#      _results.push(matematica.cubo(num));
#    }
#    return _results;
#  })();

cibi = ['broccoli', 'spinaci', 'cioccolato']
mangia cibo for cibo in cibi when cibo isnt 'cioccolato'
#=>cibi = ['broccoli', 'spinaci', 'cioccolato'];
#
#for (_k = 0, _len2 = cibi.length; _k < _len2; _k++) {
#  cibo = cibi[_k];
#  if (cibo !== 'cioccolato') {
#    mangia(cibo);
#  }
#}
```

## Altre risorse

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
