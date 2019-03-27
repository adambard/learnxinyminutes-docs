---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
  - ["Claudio Busatto", "http://github.com/cjcbusatto"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
filename: learncoffeescript-pt.coffee
---

CoffeeScript é uma pequena linguagem que compila um-para-um para o JavaScript
equivalente, e não há interpretação em tempo de execução. Como um dos sucessores
de JavaScript, CoffeeScript tenta o seu melhor para exibir uma saída legível,
bem-impressa e bom funcionamento dos códigos JavaScript em todo o tempo de
execução JavaScript.

Veja também [site do CoffeeScript](http://coffeescript.org/), que tem um tutorial
completo sobre CoffeeScript.

``` coffeescript
#CoffeeScript é uma linguagem moderna
#Segue as tendências de muitas linguagens modernas
#Assim, os comentários são iguais a Ruby e Python, eles usam símbolos numéricos.

###
Os comentários em bloco são como estes, e eles traduzem diretamente para '/ *'s e
'* /'s para o código JavaScript que resulta...

Você deveria entender mais de semântica de JavaScript antes de continuar...
###

# Tarefa:
numero = 42 #=> var numero = 42;
oposto = true #=> var oposto = true;

# Condições:
numero = -42 if oposto #=> if (oposto) {numero = -42;}

# Funções:
quadrado = (x) -> x * x #=> var quadrado = function (x) {return x * x;}

preencher = (recipiente, liquido = "coffee") ->
  "Preenchendo o #{recipiente} with #{liquido}..."
#=>var preencher;
#
#preencher = function(recipiente, liquido) {
#  if (liquido == null) {
#    liquido = "coffee";
#  }
#  return "Preenchendo o " + recipiente + " with " + liquido + "...";
#};

# Alcances:
list = [1 .. 5] #=> lista var = [1, 2, 3, 4, 5];

# Objetos:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#  "root": Math.sqrt,
#  "square": square,
#  "cube": function(x) { return x * square(x); }
#}

# Splats:
corrida = (vencedor, corredores...) ->
  print vencedor, corredores
#=>corrida = function() {
#  var corredores, vencedor;
#  vencedor = arguments[0], corredores = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#  return print(vencedor, corredores);
#};

# Existências:
alert "Eu sabia!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("Eu sabia!"); }

# Compressão de Matrizes:
cubes = (math.cube num for num in list)
#=>cubes = (function() {
#   var _i, _len, _results;
#   _results = [];
#   for (_i = 0, _len = list.length; _i < _len; _i++) {
#       num = list[_i];
#       _results.push(math.cube(num));
#   }
#   return _results;
#  })();

comidas = ['brócolis', 'espinafre', 'chocolate']
eat alimento for alimento in comidas when alimento isnt 'chocolate'
#=>comidas = ['brócolis', 'espinafre', 'chocolate'];
#
#for (_k = 0, _len2 = comidas.length; _k < _len2; _k++) {
#  alimento = comidas[_k];
#  if (alimento !== 'chocolate') {
#    eat(alimento);
#  }
```

## Recursos adicionais

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
