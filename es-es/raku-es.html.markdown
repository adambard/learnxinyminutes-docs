---
name: perl6
category: language
language: Raku
filename: learnraku-es.raku
contributors:
    - ["vendethiel", "http://github.com/vendethiel"]
    - ["Samantha McVey", "https://cry.nu"]
translators:
    - ["Luis F. Uceta", "https://github.com/uzluisf"]
lang: es-es
---

Raku es un lenguaje de programación altamente capaz y con características
abundantes para hacerlo el lenguage ideal por los próximos 100 años.

El compilador primario de Raku se llama [Rakudo](http://rakudo.org), el cual
se ejecuta en JVM y en [MoarVM](http://moarvm.com).

Meta-nota: dos signos de números (##) son usados para indicar párrafos,
mientras que un solo signo de número (#) indica notas.

`#=>` representa la salida de un comando.

```perl6
# Un comentario de una sola línea comienza con un signo de número

#`(
  Comentarios multilíneas usan #` y signos de encerradura tales
  como (), [], {}, 「」, etc.
)
```

## Variables

```perl6
## En Raku, se declara una variable lexical usando `my`
my $variable;
## Raku tiene 3 tipos básicos de variables: escalares, arrays, y hashes.
```

### Escalares

```perl6
# Un escalar representa un solo valor. Variables escalares comienzan
# con un `$`

my $str = 'Cadena';
# Las comillas inglesas ("") permiten la intepolación (lo cual veremos
# luego):
my $str2 = "Cadena";

## Los nombres de variables pueden contener pero no terminar con comillas
## simples y guiones. Sin embargo, pueden contener
## (y terminar con) guiones bajos (_):
my $nombre'de-variable_ = 5; # Esto funciona!

my $booleano = True; # `True` y `False` son valores booleanos en Raku.
my $inverso = !$booleano; # Puedes invertir un booleano con el operador prefijo `!`
my $bool-forzado = so $str; # Y puedes usar el operador prefijo `so` que
						   # convierte su operador en un Bool
```

### Arrays y Listas

```perl6
## Un array representa varios valores. Variables arrays comienzan con `@`.
## Las listas son similares pero son un tipo inmutable.

my @array = 'a', 'b', 'c';
# equivalente a:
my @letras = <a b c>;  # array de palabras, delimitado por espacios.
                       # Similar al qw de perl, o el %w de Ruby.
my @array = 1, 2, 3;

say @array[2]; # Los índices de un array empiezan por el 0 -- Este es
			   # el tercer elemento.

say "Interpola todos los elementos de un array usando [] : @array[]";
#=> Interpola todos los elementos de un array usando [] : 1 2 3

@array[0] = -1;      # Asigna un nuevo valor a un índice del array
@array[0, 1] = 5, 6; # Asigna varios valores

my @llaves = 0, 2;
@array[@llaves] = @letras; # Asignación usando un array que contiene valores
						   # índices
say @array; #=> a 6 b
```

### Hashes, o Pairs (pares) de llaves-valores.

```perl6
## Un hash contiene parejas de llaves y valores.
## Puedes construir un objeto Pair usando la sintaxis `LLave => Valor`.
## Tablas de hashes son bien rápidas para búsqueda, y son almacenadas
## sin ningún orden.
## Ten en cuenta que las llaves son "aplanadas" en contexto de hash, y
## cualquier llave duplicada es deduplicada.
my %hash = 1 => 2,
           3 => 4;
my %hash = foo => "bar", 			 # las llaves reciben sus comillas
                                     # automáticamente.
           "some other" => "value", # las comas colgantes estań bien.
           ;

## Aunque los hashes son almacenados internamente de forma diferente a los
## arrays, Raku te permite crear un hash usando un array
## con un número par de elementos fácilmente.
my %hash = <llave1 valor1 llave2 valor2>;

my %hash = llave1 => 'valor1', llave2 => 'valor2'; # ¡el mismo resultado!

## También puedes usar la sintaxis "pareja con dos puntos":
## (especialmente útil para parámetros nombrados que verás más adelante)
my %hash = :w(1),    # equivalente a `w => 1`
                     # esto es útil para el atajo `True`:
           :truey,   # equivalente a `:truey(True)`, o `truey => True`
                     # y para el `False`:
           :!falsey, # equivalente a `:falsey(False)`, o `falsey => False`
           ;

say %hash{'llave1'}; # Puedes usar {} para obtener el valor de una llave
say %hash<llave2>;   # Si es una cadena de texto, puedes actualmente usar <>
                     # (`{llave1}` no funciona, debido a que Raku no tiene
                     # palabras desnudas (barewords en inglés))
```

## Subrutinas

```perl6
## Subrutinas, o funciones como otros lenguajes las llaman, son
## creadas con la palabra clave `sub`.
sub di-hola { say "¡Hola, mundo!" }

## Puedes proveer argumentos (tipados). Si especificado,
## el tipo será chequeado al tiempo de compilación si es posible.
## De lo contrario, al tiempo de ejecución.
sub di-hola-a(Str $nombre) {
    say "¡Hola, $nombre!";
}

## Una subrutina devuelve el último valor evaluado del bloque.
sub devolver-valor {
    5;
}
say devolver-valor; # imprime 5
sub devolver-vacio {
}
say devolver-vacio; # imprime Nil

## Algunas estructuras de control producen un valor. Por ejemplo if:
sub devuelva-si {
	if True {
		"Truthy";
	}
}
say devuelva-si; # imprime Truthy

## Otras no, como un bucle for:
sub return-for {
    for 1, 2, 3 { }
}
say return-for; # imprime Nil

## Una subrutina puede tener argumentos opcionales:
sub con-opcional($arg?) { # el signo "?" marca el argumento opcional
  say "Podría returnar `(Any)` (valor de Perl parecido al 'null') si no me pasan
		un argumento, o returnaré mi argumento";
  $arg;
}
con-opcional;    # devuelve Any
con-opcional();  # devuelve Any
con-opcional(1); # devuelve 1

## También puedes proveer un argumento por defecto para
## cuando los argumentos no son proveídos:
sub hola-a($nombre = "Mundo") {
  say "¡Hola, $nombre!";
}
hola-a; 		#=> ¡Hola, Mundo!
hola-a(); 		#=> ¡Hola, Mundo!
hola-a('Tú'); 	#=> ¡Hola, Tú!

## De igual manera, al usar la sintaxis parecida a la de los hashes
## (¡Hurra, sintaxis unificada!), puedes pasar argumentos *nombrados*
## a una subrutina. Ellos son opcionales, y por defecto son del tipo "Any".
sub con-nombre($arg-normal, :$nombrado) {
  say $arg-normal + $nombrado;
}
con-nombre(1, nombrado => 6); #=> 7
## Sin embargo, debes tener algo en cuenta aquí:
## Si pones comillas alrededor de tu llave, Raku no será capaz de verla
## al tiempo de compilación, y entonces tendrás un solo objeto Pair como
## un argumento posicional, lo que significa que el siguiente ejemplo
## falla:
con-nombre(1, 'nombrado' => 6);

con-nombre(2, :nombrado(5)); #=> 7

## Para hacer un argumento nombrado mandatorio, puedes utilizar el
## inverso de `?`, `!`:
sub con-nombre-mandatorio(:$str!)  {
  say "$str!";
}
con-nombre-mandatorio(str => "Mi texto"); #=> Mi texto!
con-nombre-mandatorio;   # error al tiempo de ejecución:
						 # "Required named parameter not passed"
						 # ("Parámetro nombrado requerido no proveído")
con-nombre-mandatorio(3);# error al tiempo de ejecución:
						 # "Too many positional parameters passed"
						 # ("Demasiados argumentos posicionales proveídos")

## Si una subrutina toma un argumento booleano nombrado ...
sub toma-un-bool($nombre, :$bool) {
  say "$nombre toma $bool";
}
## ... puedes usar la misma sintaxis de hash de un "booleano corto":
takes-a-bool('config', :bool);  # config toma True
takes-a-bool('config', :!bool); # config toma False

## También puedes proveer tus argumentos nombrados con valores por defecto:
sub nombrado-definido(:$def = 5) {
  say $def;
}
nombrado-definido; #=> 5
nombrado-definido(def => 15); #=> 15

## Dado que puedes omitir los paréntesis para invocar una función sin
## argumentos, necesitas usar "&" en el nombre para almacenar la función
## `di-hola` en una variable.
my &s = &di-hola;
my &otra-s = sub { say "¡Función anónima!" }

## Una subrutina puede tener un parámetro "slurpy", o "no importa cuantos",
## indicando que la función puede recibir cualquier número de parámetros.
sub muchos($principal, *@resto) {  #`*@` (slurpy) consumirá lo restante
## Nota: Puedes tener parámetros *antes que* un parámetro "slurpy" (como
## aquí) pero no *después* de uno.
  say @resto.join(' / ') ~ "!";
}
say muchos('Feliz', 'Cumpleaño', 'Cumpleaño'); #=> Feliz / Cumpleaño!
                                           # Nota que el asterisco (*) no
                                           # consumió el parámetro frontal.

## Puedes invocar un función con un array usando el
## operador "aplanador de lista de argumento" `|`
## (actualmente no es el único rol de este operador pero es uno de ellos)
sub concat3($a, $b, $c) {
  say "$a, $b, $c";
}
concat3(|@array); #=> a, b, c
                  # `@array` fue "aplanado" como parte de la lista de argumento
```

## Contenedores

```perl6
## En Raku, valores son actualmente almacenados en "contenedores".
## El operador de asignación le pregunta al contenedor en su izquierda
## almacenar el valor a su derecha. Cuando se pasan alrededor, contenedores
## son marcados como inmutables. Esto significa que, en una función, tu
## tendrás un error si tratas de mutar uno de tus argumentos.
## Si realmente necesitas hacerlo, puedes preguntar por un contenedor
## mutable usando `is rw`:
sub mutar($n is rw) {
  $n++;
  say "¡\$n es ahora $n!";
}

my $m = 42;
mutar $m; # ¡$n es ahora 43!

## Esto funciona porque estamos pasando el contenedor $m para mutarlo. Si
## intentamos pasar un número en vez de pasar una variable, no funcionará
## dado que no contenedor ha sido pasado y números enteros son inmutables
## por naturaleza:

mutar 42; # Parámetro '$n' esperaba un contenedor mutable,
		  # pero recibió un valor Int

## Si en cambio quieres una copia, debes usar `is copy`.

## Por si misma, una subrutina devuelve un contenedor, lo que significa
## que puede ser marcada con rw:
my $x = 42;
sub x-almacena() is rw { $x }
x-almacena() = 52; # En este caso, los paréntesis son mandatorios
                # (porque de otra forma, Raku piensa que la función
				# `x-almacena` es un identificador).
say $x; #=> 52
```

## Estructuras de control
### Condicionales

```perl6
## - `if`
## Antes de hablar acerca de `if`, necesitamos saber cuales valores son
## "Truthy" (representa True (verdadero)), y cuales son "Falsey"
## (o "Falsy") -- representa False (falso). Solo estos valores son
## Falsey: 0, (), {}, "", Nil, un tipo (como `Str` o`Int`) y
## por supuesto False. Todos los valores son Truthy.
if True {
  say "¡Es verdadero!";
}

unless False {
  say "¡No es falso!";
}

## Como puedes observar, no necesitas paréntesis alrededor de condiciones.
## Sin embargo, necesitas las llaves `{}` alrededor del cuerpo de un bloque:
# if (true) say; # !Esto no funciona!

## También puedes usar sus versiones sufijos seguidas por la palabra clave:
say "Un poco verdadero" if True;

## - La condicional ternaria, "?? !!" (como `x ? y : z` en otros lenguajes)
##   devuelve $valor-si-verdadera si la condición es verdadera y
##   $valor-si-falsa si es falsa.
##   my $resultado = $valor condición ?? $valor-si-verdadera !! $valor-si-falsa;

my $edad = 30;
say $edad > 18 ?? "Eres un adulto" !! "Eres menor de 18";
```

### given/when, ó switch

```perl6
## - `given`-`when` se parece al `switch` de otros lenguajes, pero es más
## poderoso gracias a la coincidencia inteligente ("smart matching" en inglés)
## y la "variable tópica" $_ de Perl.
##
## Esta variable ($_) contiene los argumentos por defecto de un bloque,
## la iteración actual de un loop (a menos que sea explícitamente
## nombrado), etc.
##
## `given` simplemente pone su argumento en `$_` (como un bloque lo haría),
## y `when` lo compara usando el operador de "coincidencia inteligente" (`~~`).
##
## Dado que otras construcciones de Raku usan esta variable (por ejemplo,
## el bucle `for`, bloques, etc), esto se significa que el poderoso `when` no
## solo se aplica con un `given`, sino que se puede usar en cualquier
## lugar donde exista una variable `$_`.

given "foo bar" {
  say $_; #=> foo bar
  when /foo/ { # No te preocupies acerca de la coincidencia inteligente –
			   # solo ten presente que `when` la usa.
               # Esto es equivalente a `if $_ ~~ /foo/`.
    say "¡Yay!";
  }
  when $_.chars > 50 { # coincidencia inteligente con cualquier cosa True es True,
                       # i.e. (`$a ~~ True`)
                       # por lo tanto puedes también poner condiciones "normales".
                       # Este `when` es equivalente a este `if`:
                       #  if $_ ~~ ($_.chars > 50) {...}
                       # que significa:
                       #  if $_.chars > 50 {...}
    say "¡Una cadena de texto bien larga!";
  }
  default { # lo mismo que `when *` (usando la Whatever Star)
    say "Algo más";
  }
}
```

### Construcciones de bucle

```perl6
## - `loop` es un bucle infinito si no le pasas sus argumentos,
## pero también puede ser un bucle for al estilo de C:
loop {
  say "¡Este es un bucle infinito!";
  last; # last interrumpe el bucle, como la palabra clave `break`
        # en otros lenguajes.
}

loop (my $i = 0; $i < 5; $i++) {
  next if $i == 3;  # `next` salta a la siguiente iteración, al igual
					 # que `continue` en otros lenguajes. Ten en cuenta que
					 # también puedes usar la condicionales postfix (sufijas)
                     # bucles, etc.
  say "¡Este es un bucle al estilo de C!";
}

## - `for` - Hace iteraciones en un array
for @array -> $variable {
  say "¡He conseguido una $variable!";
}

## Como vimos con `given`, la variable de una "iteración actual" por defecto
## es `$_`. Esto significa que puedes usar `when` en un bucle `for` como
## normalmente lo harías con `given`.
for @array {
  say "he conseguido a $_";

  .say; # Esto es también permitido.
        # Una invocación con punto (dot call) sin "tópico" (recibidor) es
		# enviada a `$_` por defecto.
  $_.say; # lo mismo de arriba, lo cual es equivalente.
}

for @array {
  # Puedes...
  next if $_ == 3; # Saltar a la siguiente iteración (`continue` en
				   # lenguages parecido a C)
  redo if $_ == 4; # Re-hacer la iteración, manteniendo la
				   # misma variable tópica (`$_`)
  last if $_ == 5; # Salir fuera del bucle (como `break`
				   # en lenguages parecido a C)
}

## La sintaxis de "bloque puntiagudo" no es específica al bucle for.
## Es solo una manera de expresar un bloque en Raku.
if computación-larga() -> $resultado {
  say "El resultado es $resultado";
}
```

## Operadores

```perl6
## Dados que los lenguajes de la familia Perl son lenguages basados
## mayormente en operadores, los operadores de Raku son actualmente
## subrutinas un poco cómicas en las categorías sintácticas. Por ejemplo,
## infix:<+> (adición) o prefix:<!> (bool not).

## Las categorías son:
## - "prefix"  (prefijo): anterior a (como `!` en `!True`).
## - "postfix" (sufijo):  posterior a (como `++` en `$a++`).
## - "infix"   (infijo):  en medio de (como `*` en `4 * 3`).
## - "circumfix" (circunfijo): alrededor de (como `[`-`]` en `[1, 2]`).
## - "post-circumfix" (pos-circunfijo): alrededor de un término,
##                     				   posterior a otro término.
##                     (como `{`-`}` en `%hash{'key'}`)

## La lista de asociatividad y precedencia se explica más abajo.

## ¡Bueno, ya estás listo(a)!

## * Chequeando igualdad

## - `==` se usa en comparaciones numéricas.
3 == 4; # Falso
3 != 4; # Verdadero

## - `eq` se usa en comparaciones de cadenas de texto.
'a' eq 'b';
'a' ne 'b';  # no igual
'a' !eq 'b'; # lo mismo que lo anterior

## - `eqv` es equivalencia canónica (or "igualdad profunda")
(1, 2) eqv (1, 3);

## - Operador de coincidencia inteligente (smart matching): `~~`
## Asocia (aliasing en inglés) el lado izquierda a la variable $_
## y después evalúa el lado derecho.
## Aquí algunas comparaciones semánticas comunes:

## Igualdad de cadena de texto o numérica

'Foo' ~~ 'Foo'; # True si las cadenas de texto son iguales.
12.5 ~~ 12.50; # True si los números son iguales.

## Regex - Para la comparación de una expresión regular en contra
## del lado izquierdo. Devuelve un objeto (Match), el cual evalúa
## como True si el regex coincide con el patrón.

my $obj = 'abc' ~~ /a/;
say $obj; # ｢a｣
say $obj.WHAT; # (Match)

## Hashes
'llave' ~~ %hash; # True si la llave existe en el hash

## Tipo - Chequea si el lado izquierdo "tiene un tipo" (puede chequear
## superclases y roles)

1 ~~ Int; # True (1 es un número entero)

## Coincidencia inteligente contra un booleano siempre devuelve ese
## booleano (y lanzará una advertencia).

1 ~~ True; # True
False ~~ True; # True

## La sintaxis general es $arg ~~ &función-returnando-bool;
## Para una lista completa de combinaciones, usa esta tabla:
## http://perlcabal.org/syn/S03.html#Smart_matching

## También, por supuesto, tienes `<`, `<=`, `>`, `>=`.
## Sus equivalentes para cadenas de texto están disponibles:
## `lt`, `le`, `gt`, `ge`.
3 > 4;

## * Constructores de rango
3 .. 7; # 3 a 7, ambos incluidos
## `^` en cualquier lado excluye a ese lado:
3 ^..^ 7; # 3 a 7, no incluidos (básicamente `4 .. 6`)
## Esto también funciona como un atajo para `0..^N`:
^10; # significa 0..^10

## Esto también nos permite demostrar que Raku tiene arrays
## ociosos/infinitos, usando la Whatever Star:
my @array = 1..*; # 1 al Infinito! `1..Inf` es lo mismo.
say @array[^10]; # puedes pasar arrays como subíndices y devolverá
   				 # un array de resultados. Esto imprimirá
				 # "1 2 3 4 5 6 7 8 9 10" (y no se quedaré sin memoria!)
## Nota: Al leer una lista infinita, Raku "cosificará" los elementos que
## necesita y los mantendrá en la memoria. Ellos no serán calculados más de
## una vez. Tampoco calculará más elementos de los que necesita.

## Un índice de array también puede ser una clausura ("closure" en inglés).
## Será llamada con la longitud como el argumento
say join(' ', @array[15..*]); #=> 15 16 17 18 19
## lo que es equivalente a:
say join(' ', @array[-> $n { 15..$n }]);
## Nota: Si tratas de hacer cualquiera de esos con un array infinito,
##		  provocará un array infinito (tu programa nunca terminará)

## Puedes usar eso en los lugares que esperaría, como durante la asignación
## a un array
my @números = ^20;

## Aquí los números son incrementados por "6"; más acerca del
## operador `...` adelante.
my @seq =  3, 9 ... * > 95; # 3 9 15 21 27 [...] 81 87 93 99;
@números[5..*] = 3, 9 ... *; # aunque la secuencia es infinita,
                             # solo los 15 números necesarios será calculados.
say @números; #=> 0 1 2 3 4 3 9 15 21 [...] 81 87
              # (solamente 20 valores)

## * And &&, Or ||
3 && 4; # 4, el cual es Truthy. Invoca `.Bool` en `4` y obtiene `True`.
0 || False; # False. Invoca `.Bool` en `0`

## * Versiones circuito corto de lo de arriba
## && Devuelve el primer argumento que evalúa a False, o el último.

my ( $a, $b, $c ) = 1, 0, 2;
$a && $b && $c; # Devuelve 0, el primer valor que es False

## || Devuelve el primer argumento que evalúa a True.
$b || $a; # 1

## Y porque tu lo querrás, también tienes operadores de asignación
## compuestos:
$a *= 2;  # multiplica y asigna. Equivalente a $a = $a * 2;
$b %%= 5; # divisible por y asignación. Equivalente $b = $b %% 5;
@array .= sort; # invoca el método `sort` y asigna el resultado devuelto.
```

## ¡Más sobre subrutinas!

```perl6
## Como dijimos anteriormente, Raku tiene subrutinas realmente poderosas.
## Veremos unos conceptos claves que la hacen mejores que en cualquier otro
## lenguaje :-).
```

### !Desempacado!

```perl6
## Es la abilidad de extraer arrays y llaves (También conocido como
## "destructuring"). También funcionará en `my` y en las listas de parámetros.
my ($f, $g) = 1, 2;
say $f; #=> 1
my ($, $, $h) = 1, 2, 3; # mantiene los anónimos no interesante
say $h; #=> 3

my ($cabeza, *@cola) = 1, 2, 3; # Sí, es lo mismo que con subrutinas "slurpy"
my (*@small) = 1;

sub desempacar_array(@array [$fst, $snd]) {
  say "Mi primero es $fst, mi segundo es $snd! De todo en todo, soy un @array[].";
  # (^ recuerda que `[]` interpola el array)
}
desempacar_array(@cola); #=> My first is 2, my second is 3 ! All in all, I'm 2 3


## Si no está usando el array, puedes también mantenerlo anónimo, como un
## escalar:
sub primero-de-array(@ [$fst]) { $fst }
primero-de-array(@small); #=> 1
primero-de-array(@tail); # Lanza un error "Demasiados argumentos posicionales
                         # proveídos"
                         # (lo que significa que el array es muy grande).

## También puedes usar un slurp ...
sub slurp-en-array(@ [$fst, *@rest]) { # Podrías mantener `*@rest` anónimos
  say $fst + @rest.elems; # `.elems` returna la longitud de una lista.
                          # Aquí, `@rest`  es `(3,)`, since `$fst` holds the `2`.
}
slurp-en-array(@tail); #=> 3

## Hasta podrías hacer un extracción usando una slurpy (pero no sería útil ;-).)
sub fst(*@ [$fst]) { # o simplemente: `sub fst($fst) { ... }`
  say $fst;
}
fst(1); #=> 1
fst(1, 2); # errores con "Too many positional parameters passed"

## También puedes desestructurar hashes (y clases, las cuales
## veremos adelante). La sintaxis es básicamente
## `%nombre-del-hash (:llave($variable-para-almacenar))`.
## El hash puede permanecer anónimos si solo necesitas los valores extraídos.
sub llave-de(% (:azul($val1), :red($val2))) {
  say "Valores: $val1, $val2.";
}
## Después invócala con un hash: (necesitas mantener las llaves
## de los parejas de llave y valor para ser un hash)
llave-de({azul => 'blue', rojo => "red"});
#llave-de(%hash); # lo mismo (para un `%hash` equivalente)

## La última expresión de una subrutina es devuelta inmediatamente
## (aunque puedes usar la palabra clave `return`):
sub siguiente-indice($n) {
  $n + 1;
}
my $nuevo-n= siguiente-indice(3); # $nuevo-n es ahora 4

## Este es cierto para todo, excepto para las construcciones de bucles
## (debido a razones de rendimiento): Hay una razón de construir una lista
## si la vamos a desechar todos los resultados.
## Si todavías quieres construir una, puedes usar la sentencia prefijo `do`:
## (o el prefijo `gather`, el cual veremos luego)
sub lista-de($n) {
  do for ^$n { # nota el uso del operador de rango `^` (`0..^N`)
    $_        # iteración de bucle actual
  }
}
my @list3 = lista-de(3); #=> (0, 1, 2)
```

### lambdas

```perl6
## Puedes crear una lambda con `-> {}` ("bloque puntiagudo") o `{}` ("bloque")
my &lambda = -> $argumento { "El argumento pasado a esta lambda es $argumento" }
## `-> {}` y `{}` son casi la misma cosa, excepto que la primerra puede
## tomar argumentos, y la segunda puede ser malinterpretada como un hash
## por el parseador.

## Podemos, por ejemplo, agregar 3  a cada valor de un array usando map:
my @arraymas3 = map({ $_ + 3 }, @array); # $_ es el argumento implícito

## Una subrutina (`sub {}`) tiene semánticas diferentes a un
## bloque (`{}` or `-> {}`): Un bloque no tiene "contexto funcional"
## (aunque puede tener argumentos), lo que significa que si quieres devolver
## algo desde un bloque, vas a returnar desde la función parental. Compara:  
sub is-in(@array, $elem) {
  # esto `devolverá` desde la subrutina `is-in`
  # Una vez que la condición evalúa a True, el bucle terminará
  map({ return True if $_ == $elem }, @array);
}
sub truthy-array(@array) {
  # esto producirá un array de `True` Y `False`:
  # (también puedes decir `anon sub` para "subrutina anónima")
  map(sub ($i) { if $i { return True } else { return False } }, @array);
  # ^ el `return` solo devuelve desde la `sub`
}

## También puedes usar la "whatever star" para crear una función anónima
## (terminará con el último operador en la expresión actual)
my @arraymas3 = map(*+3, @array); # `*+3` es lo mismo que `{ $_ + 3 }`
my @arraymas3 = map(*+*+3, @array); # lo mismo que `-> $a, $b { $a + $b + 3 }`
                                     # también `sub ($a, $b) { $a + $b + 3 }`
say (*/2)(4); #=> 2
                  # Inmediatamente ejecuta la función que Whatever creó.
say ((*+3)/5)(5); #=> 1.6
                  # ¡funciona hasta con los paréntesis!

## Pero si necesitas más que un argumento (`$_`) en un bloque
## (sin depender en `-> {}`), también puedes usar la sintaxis implícita
## de argumento, `$` :
map({ $^a + $^b + 3 }, @array); # equivalente a lo siguiente:
map(sub ($a, $b) { $a + $b + 3 }, @array); # (aquí con `sub`)

## Nota : Esos son ordernados lexicográficamente.
# `{ $^b / $^a }` es como `-> $a, $b { $b / $a }`
```

### Acerca de tipos...

```perl6
## Raku es gradualmente tipado. Esto quiere decir que tu especifica el
## tipo de tus variables/argumentos/devoluciones (return), o puedes omitirlos
## y serán "Any" por defecto.
## Obviamente tienes acceso a algunas tipos básicos, como Int y Str.
## Las construcciones para declarar tipos son "class", "role", lo cual
## verás más adelante.

## Por ahora, examinemos "subset" (subconjunto).
## Un "subset" es un "sub-tipo" con chequeos adicionales.
## Por ejemplo: "un número entero bien grande es un Int que es mayor que 500"
## Puedes especificar el tipo del que creas el subconjunto (por defecto, Any),
## y añadir chequeos adicionales con la palabra clave "where" (donde):
subset EnteroGrande of Int where * > 500;
```

### Despacho Múltiple (Multiple Dispatch)

```perl6
## Raku puede decidir que variante de una subrutina invocar basado en el
## tipo de los argumento, o precondiciones arbitrarias, como con un tipo o
## un `where`:

## con tipos
multi sub dilo(Int $n) { # nota la palabra clave `multi` aquí
  say "Número: $n";
}
multi dilo(Str $s) { # un multi es una subrutina por defecto
  say "Cadena de texto: $s";
}
dilo("azul"); # prints "Cadena de texto: azul"
dilo(True); # falla al *tiempo de compilación* con
            # "calling 'dilo' will never work with arguments of types ..."
			# (invocar 'dilo' nunca funcionará con argumentos de tipos ...")
## con precondición arbitraria (¿recuerdas los subconjuntos?):
multi es-grande(Int $n where * > 50) { "¡Sí!" } # usando una clausura
multi es-grande(Int $ where 10..50) { "Tal vez." } # Usando coincidencia inteligente
                                              # (podrías usar un regexp, etc)
multi es-grande(Int $) { "No" }

subset Par of Int where * %% 2;

multi inpar-o-par(Par) { "Par" } # El caso principal usando el tipo.
                                   # No nombramos los argumentos,
multi inpar-o-par($) { "Inpar" } # "else"

## ¡Podrías despachar basado en la presencia de argumentos posicionales!
multi sin_ti-o-contigo(:$with!) { # Necesitas hacerlo mandatorio
                                  # para despachar en contra del argumento.
  say "¡Puedo vivir! Actualmente, no puedo.";
}
multi sin_ti-o-contigo {
  say "Definitivamente no puedo vivir.";
}
## Esto es muy útil para muchos propósitos, como subrutinas `MAIN` (de las
## cuales hablaremos luego), y hasta el mismo lenguaje la está usando
## en muchos lugares.
##
## - `is`, por ejemplo, es actualmente un `multi sub` llamado
##	`trait_mod:<is>`.
## - `is rw`, es simplemente un despacho a una función con esta signatura:
## sub trait_mod:<is>(Routine $r, :$rw!) {}
##
## (¡lo pusimos en un comentario dado que ejecutando esto sería una terrible
## idea!)
```

## Ámbito (Scoping)

```perl6
## En Raku, a diferencia de otros lenguajes de scripting, (tales como
## (Python, Ruby, PHP), debes declarar tus variables antes de usarlas. El
## declarador `my`, del cual aprendiste anteriormente, usa "ámbito léxical".
## Hay otros declaradores (`our`, `state`, ..., ) los cuales veremos luego.
## Esto se llama "ámbito léxico", donde en los bloques internos,
## puedes acceder variables de los bloques externos.
my $archivo-en-ámbito = 'Foo';
sub externo {
  my $ámbito-externo = 'Bar';
  sub interno {
    say "$archivo-en-ámbito $ámbito-externo";
  }
  &interno; # devuelve la función
}
outer()(); #=> 'Foo Bar'

## Como puedes ver, `$archivo-en-ámbito` y `$ámbito-externo`
## fueron capturados. Pero si intentaramos usar `$bar` fuera de `foo`,
## la variable estaría indefinida (y obtendrías un error al tiempo de
## compilación).
```

## Twigils

```perl6
## Hay muchos `twigils` especiales (sigilos compuestos) en Raku.
## Los twigils definen el ámbito de las variables.
## Los twigils * y ? funcionan con variables regulares:
## * Variable dinámica
## ? Variable al tiempo de compilación
## Los twigils ! y . son usados con los objetos de Raku:
## ! Atributo (miembro de la clase)
## . Método (no una variable realmente)

## El twigil `*`: Ámbito dinámico
## Estas variables usan el twigil `*` para marcar variables con ámbito
## dinámico. Variables con ámbito dinámico son buscadas a través del
## invocador, no a través del ámbito externo.

my $*ambito_din_1 = 1;
my $*ambito_din_2 = 10;

sub di_ambito {
  say "$*ambito_din_1 $*ambito_din_2";
}

sub invoca_a_di_ambito {
  my $*ambito_din_1 = 25; # Define a $*ambito_din_1 solo en esta subrutina.
  $*ambito_din_2 = 100; # Cambiará el valor de la variable en ámbito.
  di_ambito(); #=> 25 100 $*ambito_din_1 y 2 serán buscadas en la invocación.
             # Se usa el valor de $*ambito_din_1 desde el ámbito léxico de esta
             # subrutina aunque los bloques no están anidados (están anidados por
             # invocación).
}
di_ambito(); #=> 1 10
invoca_a_di_ambito(); #=> 25 100
                # Se usa a $*ambito_din_1 como fue definida en invoca_a_di_ambito
                # aunque la estamos invocando desde afuera.
di_ambito(); #=> 1 100 Cambiamos el valor de $*ambito_din_2 en invoca_a_di_ambito
             #         por lo tanto su valor a cambiado.
```

## Modelo de Objeto

```perl6
## Para invocar a un método en un objeto, agrega un punto seguido por el
## nombre del objeto:
## => $object.method
## Las classes son declaradas usando la palabra clave `class`. Los atributos
## son declarados con la palabra clave `has`, y los métodos con `method`.
## Cada atributo que es privado usa el twigil `!`. Por ejemplo: `$!attr`.
## Atributos públicos inmutables usan el twigil `.` (los puedes hacer
## mutables con `is rw`).
## La manera más fácil de recordar el twigil `$.` is comparándolo
## con como los métodos son llamados.

## El modelo de objeto de Raku ("SixModel") es muy flexible, y te permite
## agregar métodos dinámicamente, cambiar la semántica, etc ...
## (no hablaremos de todo esto aquí. Por lo tanto, refiérete a:
## https://docs.raku.org/language/objects.html).

class Clase-Atrib {
  has $.atrib; # `$.atrib` es inmutable.
               # Desde dentro de la clase, usa `$!atrib` para modificarlo.
  has $.otro-atrib is rw; # Puedes marcar un atributo como público con `rw`.
  has Int $!atrib-privado = 10;

  method devolver-valor {
    $.atrib + $!atrib-privado;
  }

  method asignar-valor($param) { # Métodos pueden tomar parámetros.
    $!attrib = $param;   # Esto funciona porque `$!` es siempre mutable.
	# $.attrib = $param; # Incorrecto: No puedes usar la versión inmutable `$.`.

    $.otro-atrib = 5; # Esto funciona porque `$.otro-atrib` es `rw`.
  }

  method !metodo-privado {
    say "Este método es privado para la clase !";
  }
};

## Crear una nueva instancia de Clase-Atrib con $.atrib asignado con 5:
## Nota: No puedes asignarle un valor a atrib-privado desde aquí (más de
## esto adelante).
my $class-obj = Clase-Atrib.new(atrib => 5);
say $class-obj.devolver-valor; #=> 5
# $class-obj.atrib = 5; # Esto falla porque `has $.atrib` es inmutable
$class-obj.otro-atrib = 10; # En cambio, esto funciona porque el atributo
							# público es mutable (`rw`).
```

### Herencia de Objeto

```perl6
## Raku también tiene herencia (junto a herencia múltiple)
## Mientras los métodos declarados con `method` son heredados, aquellos
## declarados con `submethod` no lo son.
## Submétodos son útiles para la construcción y destrucción de tareas,
## tales como BUILD, o métodos que deben ser anulados por subtipos.
## Aprenderemos acerca de BUILD más adelante.

class Padre {
  has $.edad;
  has $.nombre;
  # Este submétodo no será heredado por la clase Niño.
  submethod color-favorito {
    say "Mi color favorito es Azul";
  }
  # Este método será heredado
  method hablar { say "Hola, mi nombre es $!nombre" }
}
# Herencia usa la palabra clave `is`
class Niño is Padre {
  method hablar { say "Goo goo ga ga" }
  # Este método opaca el método `hablar` de Padre.
  # Este niño no ha aprendido a hablar todavía.
}
my Padre $Richard .= new(edad => 40, nombre => 'Richard');
$Richard.color-favorito; #=> "Mi color favorito es Azul"
$Richard.hablar; #=> "Hola, mi nombre es Richard"
## $Richard es capaz de acceder el submétodo; él sabe como decir su nombre.

my Niño $Madison .= new(edad => 1, nombre => 'Madison');
$Madison.hablar; # imprime "Goo goo ga ga" dado que el método fue cambiado
				 # en la clase Niño.
# $Madison.color-favorito # no funciona porque no es heredado

## Cuando se usa `my T $var` (donde `T` es el nombre de la clase), `$var`
## inicia con `T` en si misma, por lo tanto puedes invocar `new` en `$var`.
## (`.=` es sólo la invocación por punto y el operador de asignación:
## `$a .= b` es lo mismo que `$a = $a.b`)
## Por ejemplo, la instancia $Richard pudo también haber sido declarada así:
## my $Richard = Padre.new(edad => 40, nombre => 'Richard');

## También observa que `BUILD` (el método invocado dentro de `new`)
## asignará propiedades de la clase padre, por lo que puedes pasar
## `val => 5`.
```

### Roles, o Mixins

```perl6
## Roles son suportados también (comúnmente llamados Mixins en otros
## lenguajes)
role PrintableVal {
  has $!counter = 0;
  method print {
    say $.val;
  }
}

## Se "importa" un mixin (un "role") con "does":
class Item does PrintableVal {
  has $.val;

  ## Cuando se utiliza `does`, un `rol` se mezcla en al clase literalmente:
  ## los métodos y atributos se ponen juntos, lo que significa que una clase
  ## puede acceder los métodos y atributos privados de su rol (pero no lo inverso!):
  method access {
    say $!counter++;
  }

  ## Sin embargo, esto:
  ## method print {}
  ## es SÓLO válido cuando `print` no es una `multi` con el mismo dispacho.
  ## (esto significa que una clase padre puede opacar una `multi print() {}`
  ## de su clase hijo/a, pero es un error sin un rol lo hace)

  ## NOTA: Puedes usar un rol como una clase (con `is ROLE`). En este caso,
  ##       métodos serán opacados, dado que el compilador considerará `ROLE`
  ##       como una clase.
}
```

## Excepciones

```perl6
## Excepciones están construidas al tope de las clases, en el paquete
## `X` (como `X::IO`).
## En Raku, excepciones son lanzadas automáticamente.
open 'foo'; #=> Failed to open file foo: no such file or directory
## También imprimirá la línea donde el error fue lanzado y otra información
## concerniente al error.

## Puedes lanzar una excepción usando `die`:
die 'Error!'; #=> Error!

## O más explícitamente:
die X::AdHoc.new(payload => 'Error!');

## En Raku, `orelse` es similar al operador `or`, excepto que solamente
## coincide con variables indefinidas, en cambio de cualquier cosa
## que evalúa a falso.
## Valores indefinidos incluyen: `Nil`, `Mu` y `Failure`, también como
## `Int`, `Str` y otros tipos que no han sido inicializados a ningún valor
## todavía.
## Puedes chequear si algo está definido o no usando el método defined:
my $no-inicializada;
say $no-inicializada.defined; #=> False
## Al usar `orelse`, se desarmará la excepción y creará un alias de dicho
## fallo en $_
## Esto evitará que sea automáticamente manejado e imprima una marejada de
## mensajes de errores en la pantalla.
## Podemos usar el método de excepción en $_ para acceder la excepción:
open 'foo' orelse say "Algo pasó {.exception}";

## Esto también funciona:
open 'foo' orelse say "Algo pasó $_"; #=> Algo pasó
              #=>  Failed to open file foo: no such file or directory
## Ambos ejemplos anteriores funcionan pero en caso de que consigamos un
## objeto desde el lado izquierdo que no es un fallo, probablemente
## obtendremos una advertencia. Más abajo vemos como usar `try` y `CATCH`
## para ser más expecíficos con las excepciones que capturamos.
```

### Usando `try` y `CATCH`

```perl6
## Al usar `try` y `CATCH`, puedes contener y manejar excepciones sin
## interrumpir el resto del programa. `try` asignará la última excepción
## a la variable especial `$!`.
## Nota: Esto no tiene ninguna relación con las variables $!.

try open 'foo';
say "Bueno, lo intenté! $!" if defined $!; #=> Bueno, lo intenté! Failed to open file
                                       #foo: no such file or directory
## Ahora, ¿qué debemos hacer si queremos más control sobre la excepción?
## A diferencia de otros lenguajes, en Raku se pone el bloque `CATCH`
## *dentro* del bloque a intentar (`try`). Similarmente como $_ fue asignada
## cuando 'disarmamos' la excepción con `orelse`, también usamos $_ en el
## bloque CATCH.
## Nota: ($! es solo asignada *después* del bloque `try`)
## Por defecto, un bloque `try` tiene un bloque `CATCH` que captura
## cualquier excepción (`CATCH { default {} }`).

try { my $a = (0 %% 0);  CATCH { say "Algo pasó: $_" } }
 #=> Algo pasó: Attempt to divide by zero using infix:<%%>

## Puedes redefinir lo anterior usando `when` y (`default`)
## para manejar las excepciones que desees:
try {
  open 'foo';
  CATCH {     # En el bloque `CATCH`, la excepción es asignada a $_
    when X::AdHoc { say "Error: $_" }
     #=>Error: Failed to open file /dir/foo: no such file or directory

	## Cualquier otra excepción será levantada de nuevo, dado que no
	## tenemos un `default`.
	## Básicamente, si un `when`
    ## Basically, if a `when` matches (or there's a `default`) marks the
	  ## exception as
    ## "handled" so that it doesn't get re-thrown from the `CATCH`.
    ## You still can re-throw the exception (see below) by hand.
  }
}

## En Raku, excepciones poseen ciertas sutilezas. Algunas
## subrutinas en Raku devuelven un `Failure`, el cual es un tipo de
## "excepción no levantada". Ellas no son levantadas hasta que tu intentas
## mirar a sus contenidos, a menos que invoques `.Bool`/`.defined` sobre
## ellas - entonces, son manejadas.
## (el método `.handled` es `rw`, por lo que puedes marcarlo como `False`
## por ti mismo)
## Puedes levantar un `Failure` usando `fail`. Nota que si el pragma
## `use fatal` estás siendo utilizado, `fail` levantará una excepión (como
## `die`).
fail "foo"; # No estamos intentando acceder el valor, por lo tanto no problema.
try {
  fail "foo";
  CATCH {
    default { say "Levantó un error porque intentamos acceder el valor del fallo!" }
  }
}

## También hay otro tipo de excepción: Excepciones de control.
## Esas son excepciones "buenas", las cuales suceden cuando cambias el flujo
## de tu programa, usando operadores como `return`, `next` or `last`.
## Puedes capturarlas con `CONTROL` (no lista un 100% en Rakudo todavía).
```

## Paquetes

```perl6
## Paquetes son una manera de reusar código. Paquetes son como
## "espacio de nombres" (namespaces en inglés), y cualquier elemento del
## modelo seis (`module`, `role`, `class`, `grammar`, `subset` y `enum`)
## son paquetes por ellos mismos. (Los paquetes son como el mínimo común
## denominador)
## Los paquetes son importantes - especialmente dado que Perl es bien
## reconocido por CPAN, the Comprehensive Perl Archive Nertwork.

## Puedes usar un módulo (traer sus declaraciones al ámbito) con `use`
use JSON::Tiny; # si intalaste Rakudo* o Panda, tendrás este módulo
say from-json('[1]').perl; #=> [1]

## A diferencia de Perl, no deberías declarar paquetes usando
## la palabra clave `package`. En vez, usa `class Nombre::Paquete::Aquí;`
## para declarar una clase, o si solamente quieres exportar
## variables/subrutinas, puedes usar `module`.

module Hello::World { # forma de llaves
                      # Si `Hello` no existe todavía, solamente será una cola ("stub"),
                      #  que puede ser redeclarada más tarde.
  # ... declaraciones aquí ...
}
unit module Parse::Text; # forma de ámbito de archivo

grammar Parse::Text::Grammar { # Una gramática (grammar en inglés) es un paquete,
							   # en el cual puedes usar `use`
}                    # Aprenderás más acerca de gramáticas en la sección de regex

## Como se dijo anteriormente, cualquier parte del modelo seis es también un
## paquete. Dado que `JSON::Tiny` usa su propia clase `JSON::Tiny::Actions`,
## tu puedes usarla de la manera siguiente:
my $acciones = JSON::Tiny::Actions.new;

## Veremos como exportar variables y subrutinas en la siguiente parte:
```

## Declaradores

```perl6
## En Raku, tu obtienes diferentes comportamientos basado en como declaras
## una variable.
## Ya has visto `my` y `has`, ahora exploraremos el resto.

## * las declaraciones `our` ocurren al tiempo `INIT` (ve "Phasers" más abajo)
## Es como `my`, pero también crea una variable paquete.
## (Todas las cosas relacionadas con paquetes (`class`, `role`, etc) son
## `our` por defecto)
module Var::Incrementar {
  our $nuestra-var = 1; # Nota: No puedes colocar una restricción de tipo
  my $mi-var = 22;      # como Int (por ejemplo) en una variable `our`.
  our sub Inc {

    our sub disponible { # Si tratas de hacer subrutinas internas `our`...
                        # Mejor que sepas lo que haces (No lo haga!).
      say "No hagas eso. En serio. Estás jugando con fuego y te quemarás.";
    }

    my sub no-disponible { # `my sub` es por defecto
      say "No puedes acceder aquí desde fuera. Soy 'my'!";
    }
    say ++$nuestra-var; # Incrementa la variable paquete y muestra su valor
  }

}
say $Var::Incrementar::nuestra-var; #=> 1 Esto funciona
say $Var::Incrementar::mi-var; #=> (Any) Esto no funcionará.

Var::Incrementar::Inc; #=> 2
Var::Incrementar::Inc; #=> 3 # Nota como el valor de $nuestra-var fue
                            # retenido
Var::Incrementar::no-disponible; #=> Could not find symbol '&no-disponible'

## * `constant` (ocurre al tiempo `BEGIN`)
## Puedes usar la palabra clave `constant` para declarar una
## variable/símbolo al tiempo de compilación:
constant Pi = 3.14;
constant $var = 1;

## Y por si te estás preguntando, sí, también puede contener listas infinitas.
constant porque-no = 5, 15 ... *;
say porque-no[^5]; #=> 5 15 25 35 45

## * `state` (ocurre al tiempo de ejecución, pero una sola vez)
## Variables "states" son solo inicializadas una vez.
## (ellas existen en otros lenguaje como `static` en C)
sub aleatorio-fijo {
  state $valor = rand;
  say $valor;
}
aleatorio-fijo for ^10; # imprimirá el mismo número 10 veces

## Nota, sin embargo, que ellas existen separadamente en diferentes contextos.
## Si declaras una función con un `state` dentro de un bucle, recreará la
## variable por cada iteración del bucle. Observa:
for ^5 -> $a {
  sub foo {
    state $valor = rand; # Esto imprimirá un valor diferente
                         # por cada valor de `$a`
  }
  for ^5 -> $b {
    say foo; # Esto imprimirá el mismo valor 5 veces, pero sólo 5.
             # La siguiente iteración ejecutará `rand` nuevamente.
  }
}
```

## Phasers

```perl6
## Un phaser en Raku es un bloque que ocurre a determinados puntos de tiempo
## en tu programa. Se les llama phaser porque marca un cambio en la fase de
## de tu programa. Por ejemplo, cuando el programa es compilado, un bucle
## for se ejecuta, dejas un bloque, o una excepción se levanta.
## (¡`CATCH` es actualmente un phaser!)
## Algunos de ellos pueden ser utilizados por sus valores devueltos, otros
## no pueden (aquellos que tiene un "[*]" al inicio de su texto de
## explicación).
## ¡Tomemos una mirada!

## * Phasers al tiempo de compilación
BEGIN { say "[*] Se ejecuta al tiempo de compilación, " ~
		    "tan pronto como sea posible, una sola vez" }
CHECK { say "[*] Se ejecuta al tiempo de compilación, " ~
			"tan tarde como sea posible, una sola vez" }

## * Phasers al tiempo de ejecución
INIT { say "[*] Se ejecuta al tiempo de ejecución, " ~
		   "tan pronto como sea posible, una sola vez" }
END { say "Se ejecuta al tiempo de ejecución, " ~
          "tan tarde como sea posible, una sola vez" }

## * Phasers de bloques
ENTER { say "[*] Se ejecuta cada vez que entra en un bloque, " ~
		"se repite en bloques de bucle" }
LEAVE { say "Se ejecuta cada vez que abandona un bloque, incluyendo " ~
            "cuando una excepción ocurre. Se repite en bloques de bucle"}

PRE {
    say "Impone una precondición a cada entrada de un bloque, " ~
        "antes que ENTER (especialmente útil para bucles)";
    say "Si este bloque no returna un valor truthy, " ~
        "una excepción del tipo X::Phaser::PrePost será levantada.";
}

## Ejemplos:
for 0..2 {
    PRE { $_ > 1 } # Esto fallará con un "Precondition failed"
}

POST {
    say "Impone una postcondAsserts a poscondición a la salida de un bloque, " ~
        "después de LEAVE (especialmente útil para bucles)";
    say "Si este bloque no returna un valor truthy, " ~
        "una excepción del tipo X::Phaser::PrePost será levantada, como con PRE.";
}
for 0..2 {
    POST { $_ < 2 } # Esto fallará con un "Postcondition failed"
}

## * Phasers de bloques/excepciones
sub {
    KEEP { say "Se ejecuta cuando sales de un bloque exitosamente
                (sin lanzar un excepción)" }
    UNDO { say "Se ejecuta cuando sale de bloque sin éxito
                (al lanzar una excepción)" }
}

## * Phasers de bucle
for ^5 {
  FIRST { say "[*] La primera vez que un bucle se ejecuta, antes que ENTER" }
  NEXT { say "Al tiempo de la continuación del bucle, antes que LEAVE" }
  LAST { say "Al tiempo de la terminación del bucle, después de LEAVE" }
}

## * Phasers de rol/clase
COMPOSE { "Cuando un rol es compuesto en una clase. /!\ NO IMPLEMENTADO TODAVÍA" }

## Ellos permite pequeños trucos o código brillante...:
say "Este código tomó " ~ (time - CHECK time) ~ "s para compilar";

## ... o brillante organización:
sub do-db-stuff {
  $db.start-transaction; # comienza una transacción nueva
  KEEP $db.commit; # commit (procede con) la transacción si todo estuvo bien
  UNDO $db.rollback; # o retrocede si todo falló
}
```

## Prefijos de sentencias

```perl6
## Los prefijos de sentencias actúan como los phasers: Ellos afectan el
## comportamiento del siguiente código.
## Debido a que son ejecutados en línea con el código ejecutable, ellos
## se escriben en letras minúsculas. (`try` and `start` están teoréticamente
## en esa lista, pero serán explicados en otra parte)
## Nota: Ningunos de estos (excepto `start`) necesitan las llaves `{` y `}`.

## - `do` (el cual ya viste) - ejecuta un bloque o una sentencia como un
## término.
## Normalmente no puedes usar una sentencia como un valor (o término):
##
##    my $valor = if True { 1 } # `if` es una sentencia - error del parseador
##
## Esto funciona:
my $a = do if True { 5 } # con `do`, `if` ahora se comporta como un término.

## - `once` - se asegura que una porción de código se ejecute una sola vez.
for ^5 { once say 1 }; #=> 1
                       # solo imprime ... una sola vez.
## Al igual que `state`, ellos son clonados por ámbito
for ^5 { sub { once say 1 }() } #=> 1 1 1 1 1
                                # Imprime una sola vez por ámbito léxico

## - `gather` - Hilo de co-rutina
## `gather` te permite tomar (`take`) varios valores en un array,
##  al igual que `do`. Encima de esto, te permite tomar cualquier expresión.
say gather for ^5 {
  take $_ * 3 - 1;
  take $_ * 3 + 1;
} #=> -1 1 2 4 5 7 8 10 11 13
say join ',', gather if False {
  take 1;
  take 2;
  take 3;
} # no imprime nada.

## - `eager` - Evalúa una sentencia ávidamente (forza contexto ávido)
## No intentes esto en casa:
##
##    eager 1..*; # esto probablemente se colgará por un momento
##                # (y podría fallar...).
##
## Pero considera lo siguiente:
constant tres-veces = gather for ^3 { say take $_ }; # No imprime nada

## frente a esto:
constant tres-veces = eager gather for ^3 { say take $_ }; #=> 0 1 2
```

## Iterables

```perl6
## En Raku, los iterables son objetos que pueden ser iterados similar
## a la construcción `for`.
## `flat`, aplana iterables:
say (1, 10, (20, 10) ); #=> (1 10 (20 10)) Nota como la agrupación se mantiene
say (1, 10, (20, 10) ).flat; #=> (1 10 20 10) Ahora el iterable es plano

## - `lazy` - Aplaza la evaluación actual hasta que el valor sea requirido
## (forza contexto perezoso)
my @lazy-array = (1..100).lazy;
say @lazy-array.is-lazy; #=> True # Chequea por "pereza" con el método `is-lazy`.
say @lazy-array; #=> [...] No se ha iterado sobre la lista
for @lazy-array { .print }; # Esto funciona y hará tanto trabajo como sea necesario.

[//]: # ( TODO explica que gather/take y map son todos perezosos)
## - `sink` - Un `eager` que desecha los resultados (forza el contexto sink)
constant nilthingie = sink for ^3 { .say } #=> 0 1 2
say nilthingie.perl; #=> Nil

## - `quietly` - Un bloque `quietly` reprime las advertencias:
quietly { warn 'Esto es una advertencia!' }; #=> No salida

## - `contend` - Intenta efectos secundarios debajo de STM
## ¡No implementado todavía!
```

## ¡Más operadores!

```perl6
## ¡Todo el mundo ama los operadores! Tengamos más de ellos.

## La lista de precedencia puede ser encontrada aquí:
## https://docs.raku.org/language/operators#Operator_Precedence
## Pero primero, necesitamos un poco de explicación acerca
## de la asociatividad:

## * Operadores binarios:
$a ! $b ! $c; # con asociatividad izquierda `!`, esto es `($a ! $b) ! $c`
$a ! $b ! $c; # con asociatividad derecha `!`, esto es `$a ! ($b ! $c)`
$a ! $b ! $c; # sin asociatividad `!`, esto es ilegal
$a ! $b ! $c; # con una cadena de asociatividad `!`, esto es `($a ! $b) and ($b ! $c)`
$a ! $b ! $c; # con asociatividad de lista `!`, esto es `infix:<>`

## * Operadores unarios:
!$a! # con asociatividad izquierda `!`, esto es `(!$a)!`
!$a! # con asociatividad derecha `!`, esto es `!($a!)`
!$a! # sin asociatividad `!`, esto es ilegal
```

### ¡Crea tus propios operadores!

```perl6
## Okay, has leído todo esto y me imagino que debería mostrarte
## algo interesante.
## Te mostraré un pequeño secreto (o algo no tan secreto):
## En Raku, todos los operadores son actualmente solo subrutinas.

## Puedes declarar un operador como declaras una subrutina:
sub prefix:<ganar>($ganador) { # se refiere a las categorías de los operadores
                               # (exacto, es el "operador de palabras" `<>`)
  say "¡$ganador ganó!";
}
ganar "El Rey"; #=> ¡El Rey Ganó!
                # (prefijo se pone delante)

## todavías puedes invocar la subrutina con su "nombre completo":
say prefix:<!>(True); #=> False

sub postfix:<!>(Int $n) {
  [*] 2..$n; # usando el meta-operador reduce ... Ve más abajo!
}
say 5!; #=> 120
        # Operadores sufijos (postfix) van *directamente* después del témino.
        # No espacios en blanco. Puedes usar paréntesis para disambiguar,
        # i.e. `(5!)!`


sub infix:<veces>(Int $n, Block $r) { # infijo va en el medio
  for ^$n {
    $r(); # Necesitas los paréntesis explícitos para invocar la función
		  # almacenada en la variable `$r`. De lo contrario, te estaría
		  # refiriendo a la variable (no a la función), como con `&r`.
  }
}
3 veces -> { say "hola" }; #=> hola
                            #=> hola
                            #=> hola
                            # Se te recomienda que ponga espacios
                            # alrededor de la invocación de operador infijo.

## Para los circunfijos y pos-circunfijos
sub circumfix:<[ ]>(Int $n) {
  $n ** $n
}
say [5]; #=> 3125
         # un circunfijo va alrededor. De nuevo, no espacios en blanco.

sub postcircumfix:<{ }>(Str $s, Int $idx) {
  ## un pos-circunfijo es
  ## "después de un término y alrededor de algo"
  $s.substr($idx, 1);
}
say "abc"{1}; #=> b
              # depués del término `"abc"`, y alrededor del índice (1)

## Esto es de gran valor -- porque todo en Raku usa esto.
## Por ejemplo, para eliminar una llave de un hash, tu usas el adverbio
## `:delete` (un simple argumento con nombre debajo):
%h{$llave}:delete;
## es equivalente a:
postcircumfix:<{ }>(%h, $llave, :delete); # (puedes invocar
										  # operadores de esta forma)
## ¡*Todos* usan los mismos bloques básicos!
## Categorías sintácticas (prefix, infix, ...), argumentos nombrados
## (adverbios), ... - usados para construir el lenguaje - están al alcance
## de tus manos y disponibles para ti.
## (obviamente, no se te recomienda que hagas un operador de *cualquier
## cosa* -- Un gran poder conlleva una gran responsabilidad.)
```

### Meta-operadores!

```perl6
## ¡Prepárate! Prepárate porque nos estamos metiendo bien hondo
## en el agujero del conejo, y probablemente no querrás regresar a
## otros lenguajes después de leer esto.
## (Me imagino que ya no quieres a este punto).
## Meta-operadores, como su nombre lo sugiere, son operadores *compuestos*.
## Básicamente, ellos son operadores que se aplican a otros operadores.

## * El meta-operador reduce (reducir)
## Es un meta-operador prefijo que toman una función binaria y
## una o varias listas. Sino se pasa ningún argumento,
## returna un "valor por defecto" para este operador
## (un valor sin significado) o `Any` si no hay ningún valor.
##
## De lo contrario, remueve un elemento de la(s) lista(s) uno a uno, y
## aplica la función binaria al último resultado (o al primer elemento de
## la lista y el elemento que ha sido removido).
##
## Para sumar una lista, podrías usar el meta-operador "reduce" con `+`,
## i.e.:
say [+] 1, 2, 3; #=> 6
## es equivalente a `(1+2)+3`

say [*] 1..5; #=> 120
## es equivalente a `((((1*2)*3)*4)*5)`.

## Puedes reducir con cualquier operador, no solo con operadores matemáticos.
## Por ejemplo, podrías reducir con `//` para conseguir
## el primer elemento definido de una lista:
say [//] Nil, Any, False, 1, 5; #=> False
                                # (Falsey, pero definido)

## Ejemplos con valores por defecto:
say [*] (); #=> 1
say [+] (); #=> 0
            # valores sin significado, dado que N*1=N y N+0=N.
say [//];   #=> (Any)
            # No hay valor por defecto para `//`.
## También puedes invocarlo con una función de tu creación usando
## los dobles corchetes:
sub add($a, $b) { $a + $b }
say [[&add]] 1, 2, 3; #=> 6

## * El meta-operador zip
## Este es un meta-operador infijo que también puede ser usado como un
## operador "normal". Toma una función binaria opcional (por defecto, solo
## crear un par), y remueve un valor de cada array e invoca su función
## binaria hasta que no tenga más elementos disponibles. Al final, returna
## un array con todos estos nuevos elementos.
(1, 2) Z (3, 4); # ((1, 3), (2, 4)), dado que por defecto, la función
                 # crea un array.
1..3 Z+ 4..6; # (5, 7, 9), usando la función personalizada infix:<+>

## Dado que `Z` tiene asociatividad de lista (ve la lista más arriba),
## puedes usarlo en más de una lista
(True, False) Z|| (False, False) Z|| (False, False); # (True, False)

## Y pasa que también puedes usarlo con el meta-operador reduce:
[Z||] (True, False), (False, False), (False, False); # (True, False)


## Y para terminar la lista de operadores:

## * El operador secuencia
## El operador secuencia es uno de la más poderosas características de
## Raku: Está compuesto, en la izquierda, de la lista que quieres que
## Raku use para deducir (y podría incluir una clausura), y en la derecha,
## un valor o el predicado que dice cuando parar (o Whatever para una
## lista infinita perezosa).
my @list = 1, 2, 3 ... 10; # deducción básica
#my @list = 1, 3, 6 ... 10; # esto muere porque Raku no puede deducir el final
my @list = 1, 2, 3 ...^ 10; # como con rangos, puedes excluir el último elemento
                            # (la iteración cuando el predicado iguala).
my @list = 1, 3, 9 ... * > 30; # puedes usar un predicado
                               # (con la Whatever Star, aquí).
my @list = 1, 3, 9 ... { $_ > 30 }; # (equivalente a lo de arriba)

my @fib = 1, 1, *+* ... *; # lista infinita perezosa de la serie fibonacci,
                           #  computada usando una clausura!
my @fib = 1, 1, -> $a, $b { $a + $b } ... *; # (equivalene a lo de arriba)
my @fib = 1, 1, { $^a + $^b } ... *; #(... también equivalene a lo de arriba)
## $a and $b siempre tomarán el valor anterior, queriendo decir que
## ellos comenzarán con $a = 1 y $b = 1 (valores que hemos asignado
## de antemano). Por lo tanto, $a = 1 y $b = 2 (resultado del anterior $a+$b),
## etc.

say @fib[^10]; #=> 1 1 2 3 5 8 13 21 34 55
               # (usandi un rango como el índice)
## Nota: Los elementos de un rango, una vez cosificados, no son re-calculados.
## Esta es la razón por la cual `@primes[^100]` tomará más tiempo la primera
## vez que se imprime. Después de esto, será hará en un instante.
```

## Expresiones Regulares

```perl6
## Estoy seguro que has estado esperando por esta parte. Bien, ahora que
## sabes algo acerca de Raku, podemos comenzar. Primeramente, tendrás
## que olvidarte acerca de "PCRE regexps" (perl-compatible regexps)
## (expresiones regulares compatible de perl).
##
## IMPORTANTE: No salte esto porque ya sabes acerca de PCRE. Son totalmente
## distintos. Algunas cosas son las mismas (como `?`, `+`, y `*`) pero
## algunas veces la semántica cambia (`|`). Asegúrate de leer esto
## cuidadosamente porque podrías trospezarte sino lo haces.
##
## Raku tiene muchas características relacionadas con RegExps. Después de
## todo, Rakudo se parsea a si mismo. Primero vamos a estudiar la sintaxis
## por si misma, después hablaremos acerca de gramáticas (parecido a PEG),
## las diferencias entre los declaradores `token`, `regex`, y `rule` y
## mucho más.
## Nota aparte: Todavía tienes acceso a los regexes PCRE usando el
## mofificador `:P5` (Sin embargo, no lo discutiremos en este tutorial).
##
## En esencia, Raku implementa PEG ("Parsing Expression Grammars")
## ("Parseado de Expresiones de Gramáticas") nativamente. El orden jerárquico
## para los parseos ambiguos es determinado por un examen multi-nivel de
## desempate:
##  - La coincidencia de token más larga. `foo\s+` le gana a `foo`
## 	  (por 2 o más posiciones)
##  - El prefijo literal más largo. `food\w*` le gana a `foo\w*` (por 1)
##  - Declaración desde la gramática más derivada a la menos derivada
##     (las gramáticas son actualmente clases)
##  - La declaración más temprana gana
say so 'a' ~~ /a/; #=> True
say so 'a' ~~ / a /; #=> True #  ¡Más legible con los espacios!

## Nota al lector (del traductor):
## Como pudiste haber notado, he decidido traducir "match" y sus diferentes
## formas verbales como "coincidir" y sus diferentes formas. Cuando digo que
## un regex (o regexp) coincide con cierto texto, me refiero a que el regex
## describe cierto patrón dentro del texto. Por ejemplo, el regex "cencia"
## coincide con el texto "reminiscencia", lo que significa que dentro del
## texto aparece ese patrón de caracteres (una `c`, seguida de una `e`,
## (seguida de una `n`, etc.)

## En todos nuestros ejemplos, vamos a usar el operador de
## "coincidencia inteligente" contra una expresión regular ("regexp" or
## "regex" de aquí en adelante). Estamos convirtiendo el resultado usando `so`,
## pero en efecto, está devolviendo un objeto Match. Ellos saben como responder
## a la indexación de lista, indexación de hash, y devolver la cadena de
## texto coincidente.
## Los resultados de la coincidencia están disponible como `$/` (en
## ámbito implícito lexical). También puedes usar las variables de captura
## las cuales comienzan con 0:
##    `$0`, `$1', `$2`...
##
## Nota que `~~` no hace un chequeo de inicio/final (es decir,
## el regexp puede coincider con solo un carácter de la cadena de texto).
## Explicaremos luego como hacerlo.

## En Raku, puedes tener un carácter alfanumérico como un literal,
## todo lo demás debe escaparse usando una barra invertida o comillas.
say so 'a|b' ~~ / a '|' b /; # `True`. No sería lo mismo si no se escapara `|`
say so 'a|b' ~~ / a \| b /;  # `True`. Otra forma de escaparlo

## El espacio en blanco actualmente no se significa nada en un regexp,
## a menos que uses el adverbio `:s` (`:sigspace`, espacio significante).
say so 'a b c' ~~ / a  b  c /; #=> `False`. Espacio no significa nada aquí.
say so 'a b c' ~~ /:s a b c /; #=> `True`. Agregamos el modificador `:s` aquí.
## Si usamos solo un espacio entre cadenas de texto en un regexp, Raku
## nos advertirá:
say so 'a b c' ~~ / a b c /; #=> 'False' # Espacio no significa nada aquí.
## Por favor usa comillas o el modificador :s (:sigspace) para suprimir
## esta advertencia, omitir el espacio, o cambiar el espaciamiento. Para
## arreglar esto y hacer los espacios menos ambiguos, usa por lo menos
## dos espacios entre las cadenas de texto o usa el adverbio `:s`.

## Como vimos anteriormente, podemos incorporar `:s` dentro de los
## delimitadores de barras. También podemos ponerlos fuera de ellos si
## especificamos `m` for `match` (coincidencia):
say so 'a b c' ~~ m:s/a  b  c/; #=> `True`
## Al usar `m` para especificar 'match', podemos también otros delimitadore:
say so 'abc' ~~ m{a  b  c}; #=> `True`
say so 'abc' ~~ m[a  b  c]; #=> `True`

## Usa el adverbio :i para especificar que no debería haber distinción entre
## minúsculas y mayúsculas:
say so 'ABC' ~~ m:i{a  b  c}; #=> `True`

## Sin embargo, es importante para como los modificadores son aplicados
## (lo cual verás más abajo)...

## Cuantificando - `?`, `+`, `*` y `**`.
## - `?` - 0 o 1
so 'ac' ~~ / a  b  c /; # `False`
so 'ac' ~~ / a  b?  c /; # `True`, la "b" coincidió (apareció) 0 veces.
so 'abc' ~~ / a  b?  c /; # `True`, la "b" coincidió 1 vez.

## ... Como debes saber, espacio en blancos son importante porque
## determinan en que parte del regexp es el objetivo del modificador:
so 'def' ~~ / a  b  c? /; # `False`. Solamente la `c` es opcional
so 'def' ~~ / a  b?  c /; # `False`. Espacio en blanco no es significante
so 'def' ~~ / 'abc'? /; # `True`. El grupo "abc"completo es opcional.

## Aquí (y más abajo) el cuantificador aplica solamente a la `b`

## - `+` - 1 o más
so 'ac' ~~ / a  b+  c /; # `False`; `+` quiere por lo menos una coincidencia
so 'abc' ~~ / a  b+  c /; # `True`; una es suficiente
so 'abbbbc' ~~ / a  b+  c /; # `True`, coincidió con 4 "b"s

## - `*` - 0 o más
so 'ac' ~~ / a  b*  c /; # `True`, todos son opcionales.
so 'abc' ~~ / a  b*  c /; # `True`
so 'abbbbc' ~~ / a  b*  c /; # `True`
so 'aec' ~~ / a  b*  c /; # `False`. "b"(s) son opcionales, no reemplazables.

## - `**` - Cuantificador (sin límites)
## Si entrecierras los ojos lo suficiente, pueder ser que entiendas
## por qué la exponenciación es usada para la cantidad.
so 'abc' ~~ / a  b**1  c /; # `True` (exactamente una vez)
so 'abc' ~~ / a  b**1..3  c /; # `True` (entre una y tres veces)
so 'abbbc' ~~ / a  b**1..3  c /; # `True`
so 'abbbbbbc' ~~ / a  b**1..3  c /; # `False` (demasiado)
so 'abbbbbbc' ~~ / a  b**3..*  c /; # `True` (rangos infinitos no son un problema)

## - `<[]>` - Clases de carácteres
## Las clases de carácteres son equivalentes a las clases `[]` de PCRE,
## pero usan una sintaxis de Raku:
say 'fooa' ~~ / f <[ o a ]>+ /; #=> 'fooa'

## Puedes usar rangos:
say 'aeiou' ~~ / a <[ e..w ]> /; #=> 'ae'

## Al igual que regexes normales, si quieres usar un carácter especial,
## escápalo (el último está escapando un espacio)
say 'he-he !' ~~ / 'he-' <[ a..z \! \  ]> + /; #=> 'he-he !'

## Obtendrás una advertencia si pones nombres duplicados
## (lo cual tiene el efecto de capturar la frase escrita)
'he he' ~~ / <[ h e ' ' ]> /; # Advierte "Repeated characters found in characters
                              # class"

## También puedes negarlos... (equivalenta a `[^]` en PCRE)
so 'foo' ~~ / <-[ f o ]> + /; # False

## ... y componerlos:
so 'foo' ~~ / <[ a..z ] - [ f o ]> + /;   # False (cualquier letra excepto f y o)
so 'foo' ~~ / <-[ a..z ] + [ f o ]> + /;  # True (no letra excepto f and o)
so 'foo!' ~~ / <-[ a..z ] + [ f o ]> + /; # True (el signo + no reemplaza la
                                          # parte de la izquierda)
```

### Grupos y Capturas

```perl6
## Grupo: Puedes agrupar partes de tu regexp con `[]`.
## Estos grupos *no son* capturados (como con `(?:)` en PCRE).
so 'abc' ~~ / a [ b ] c /; # `True`. El agrupamiento no hace casi nada
so 'foo012012bar' ~~ / foo [ '01' <[0..9]> ] + bar /;
## La línea anterior returna `True`.
## Coincidimos (o encotramos el patrón) "012" una o más de una vez (
## (el signo `+` fue aplicado al grupo).
## Pero esto no va demasiado lejos, porque no podemos actualmente obtener
## devuelta el patrón que coincidió.

## Captura: Podemos actualmente *capturar* los resultados del regexp,
## usando paréntesis.
so 'fooABCABCbar' ~~ / foo ( 'A' <[A..Z]> 'C' ) + bar /; # `True`. (usando `so`
                                                         #  aquí, `$/` más abajo)

## Ok. Comenzando con las explicaciones de grupos. Como dijimos,
### nuestra objeto `Match` está disponible en la variable `$/`:
say $/; # Imprimirá algo extraño (explicaremos luego) o
		# "Nil" si nada coincidió

## Como dijimos anteriormente, un objeto Match tiene indexación de array:
say $/[0]; #=> ｢ABC｣ ｢ABC｣
           # Estos corchetes extranos son los objetos `Match`.
           # Aquí, tenemos un array de ellos.
say $0; # Lo mismo que lo anterior.

## Nuestra captura es `$0` porque es la primera y única captura en el
## regexp. Podrías estarte preguntando porque un array y la respuesta es
## simple: Algunas capturas (indezadas usando `$0`, `$/[0]` o una nombrada)
## será un array si y solo si puedes tener más de un elemento.
## (Así que, con `*`, `+` y `**` (cualquiera los operandos), pero no con `?`).
## Usemos algunos ejemplos para ver como funciona:

## Nota: Pusimos A B C entre comillas para demostrar que el espacio en blanco
## entre ellos no es significante. Si queremos que el espacio en blanco
## *sea* significante, podemos utilizar el modificador `:sigspace`.
so 'fooABCbar' ~~ / foo ( "A" "B" "C" )? bar /; # `True`
say $/[0]; #=> ｢ABC｣
say $0.WHAT; #=> (Match)
             # Puede haber más de uno, por lo tanto es solo un solo objeto match.
so 'foobar' ~~ / foo ( "A" "B" "C" )? bar /; #=> True
say $0.WHAT; #=> (Any)
             # Esta captura no coincidió, por lo tanto está vacía
so 'foobar' ~~ / foo ( "A" "B" "C" ) ** 0..1 bar /; # `True`
say $0.WHAT; #=> (Array)
             # Un cuantificador específico siempre capturará un Array,
             #  puede ser un rango o un valor específico (hasta 1).

## Las capturas son indezadas por anidación. Esto quiere decir que un grupo
## dentro de un grup estará anidado dentro de su grupo padre: `$/[0][0]`,
## para este código:
'hello-~-world' ~~ / ( 'hello' ( <[ \- \~ ]> + ) ) 'world' /;
say $/[0].Str; #=> hello~
say $/[0][0].Str; #=> ~

## Esto se origina de un hecho bien simple: `$/` no contiene cadenas de
## texto, números enteros o arrays sino que solo contiene objetos Match.
## Estos objetos contienen los métodos `.list`, `.hash` y `.Str`. (Pero
## también puedes usar `match<llave>` para accesar un hash y `match[indice]`
## para accesar un array.
say $/[0].list.perl; #=> (Match.new(...),).list
                     # Podemos ver que es una lista de objetos Match.
                     # Estos contienen un montón de información: dónde la
					 # coincidencia comenzó o terminó, el "ast"
					 # (chequea las acciones más abajo), etc.
					 # Verás capturas nombradas más abajo con las gramáticas.

## Alternativas - el `or` de regexes
## Advertencia: Es diferente a los regexes de PCRE.
so 'abc' ~~ / a [ b | y ] c /; # `True`. o "b" o "y".
so 'ayc' ~~ / a [ b | y ] c /; # `True`. Obviamente suficiente...

## La diferencia entre este `|` y el otro al que estás acustombrado es LTM.
## LTM significa "Longest Token Matching", traducido libremente como
## "Coincidencia de Token Más Larga". Esto significa que el motor ("engine")
## siempre intentará coindidir tanto como sea posible en la cadena de texto.
## Básicamente, intentará el patrón más largo que concuerde con el regexp.
'foo' ~~ / fo | foo /; # `foo` porque es más largo.
## Para decidir cual parte es la "más larga", primero separa el regex en
## dos partes:
## El "prefijo declarativo" (la parte que puede ser analizada estáticamente)
## y las partes procedimentales.
## Los prefijos declarativos incluyen alternaciones (`|`), conjunciones (`&`),
## invocaciones de sub-reglas (no han sido introducidos todavía), clases de
## caracteres y cuantificadores.
## Las partes procidimentales incluyen todo lo demás: referencias a elementos
## anteriores, aserciones de código, y otras cosas que tradicionalmente no pueden
## ser representadas por regexes normales.
##
## Entonces, todas las alternativas se intentan al mismo tiempo, y la
## más larga gana.
## Ejemplos:
## DECLARATIVO | PROCEDIMENTAL
/ 'foo' \d+     [ <subrule1> || <subrule2> ] /;
## DECLARATIVO (grupos anidados no son un problema)
/ \s* [ \w & b ] [ c | d ] /;
## Sin embargo, las clausuras y la recursión (de regexes nombrados)
## son procedimentales.
## ... Hay más reglas complicadas, como la especifidad (los literales ganan
## son las clases de caracteres)
+
## Nota: la primera coincidencia `or` todavía existen, pero ahora se
## deletrea `||`
'foo' ~~ / fo || foo /; # `fo` ahora.
```

## Extra: la subrutina MAIN

```perl6
## La subrutina `MAIN` se invoca cuando tu ejecuta un archivo de Raku
## directamente. Es realmente poderosa porque Raku actualmente parsea
## los argumentos y los pasas a la subrutina. También maneja argumentos
## nombrados (`--foo`) y hasta autogenerará un `--help`.
sub MAIN($nombre) { say "¡Hola, $nombre!" }
## Esto produce:
##    $ raku cli.pl
##    Uso:
##      t.pl <nombre>

## Y dado que una subrutina regular en Raku, puedes tener múltiples
## despachos:
## (usando un "Bool" por un argumento nombrado para que podamos hacer
## `--replace` a cambio de `--replace=1`)
subset File of Str where *.IO.d; # convierte a un objeto IO para chequear si
							     # un archivo existe

multi MAIN('add', $key, $value, Bool :$replace) { ... }
multi MAIN('remove', $key) { ... }
multi MAIN('import', File, Str :$as) { ... } # omitiendo parámetros nombrados
## Esto produce:
##    $ raku cli.pl
##    Uso:
##      t.pl [--replace] add <key> <value>
##      t.pl remove <key>
##      t.pl [--as=<Str>] import (File)
## Como puedes ver, esto es *realmente* poderoso.
## Fue tan lejos como para mostrar las constantes en líneas.
## (el tipo solo se muestra cuando el argumento `$`/ es nombrado)
```

## APÉNDICE A:
### Lista de cosas

```perl6
## Consideramos que por ahora ya sabes lo básico de Raku.
## Esta sección es solo para listar algunas operaciones comunes
## las cuales no están en la "parte principal" del tutorial.

## Operadores

## * Comparación para ordenar
## Ellos returnan un valor de los enum `Order`:  `Less`, `Same` y `More`
##  (los cuales representan los números -1, 0 o +1).
1 <=> 4; # comparación de orden para caracteres numéricos
'a' leg 'b'; # comparación de orden para cadenas de texto
$obj eqv $obj2; # comparación de orden usando la semántica eqv

## * Ordenación genérica
3 before 4; # True
'b' after 'a'; # True

## * Operador (por defecto) de circuito corto
## Al igual que `or` y `||`, pero devuelve el primer valor *defined*
## (definido):
say Any // Nil // 0 // 5; #=> 0

## * Circuito corto exclusivo or (XOR)
## Devuelve `True` si uno (y solo uno) de sus argumentos es verdadero:
say True ^^ False; #=> True

## * Flip Flop
## Los operadores flip flop (`ff` y `fff`, equivalente a `..`/`...` en P5)
## son operadores que toman dos predicados para evalualarlos:
## Ellos son `False` hasta que su lado izquierdo devuelve `True`, entonces
## son `True` hasta que su lado derecho devuelve `True`.
## Como los rangos, tu puedes excluir la iteración cuando se convierte en
## `True`/`False` usando `^` en cualquier lado.
## Comencemos con un ejemplo:
for <well met young hero we shall meet later> {
  # por defecto, `ff`/`fff` hace coincidencia inteligente (`~~`) contra `$_`:
  if 'met' ^ff 'meet' { # no entrará el bucle if por "met"
                        # (se explica más abajo).
    .say
  }

  if rand == 0 ff rand == 1 { # compara variables más que `$_`
    say "Esto ... probablemente nunca se ejecutará ...";
  }
}
## Esto imprimirá "young hero we shall meet" (exluyendo "met"):
## el flip-flop comenzará devolviendo `True` cuando primero encuentra "met"
## (pero no returnará `False` por "met" dabido al `^` al frente de `ff`),
## hasta que ve "meet", lo cual es cuando comenzará devolviendo `False`.

## La diferencia entre `ff` (al estilo de awk) y `fff` (al estilo de sed)
## es que `ff` probará su lado derecho cuando su lado izquierdo cambia
## a `True`, y puede returnar a `False` inmediamente (*excepto* que será
## `True` por la iteración con la cual coincidió). Por lo contrario,
## `fff` esperará por la próxima iteración para intentar su lado
## derecho, una vez que su lado izquierdo ha cambiado:
.say if 'B' ff 'B' for <A B C B A>; #=> B B
                                    # porque el lado derecho se puso a prueba
                                    # directamente (y returnó `True`).
                                    # Las "B"s se imprimen dadó que coincidió
                                    # en ese momento (returnó a `False`
                                    # inmediatamente).
.say if 'B' fff 'B' for <A B C B A>; #=> B C B
                                     # El lado derecho no se puso a prueba
                                     # hasta que `$_` se convirtió en "C"
                                     # (y por lo tanto no coincidió
                                     # inmediamente).

## Un flip-flop puede cambiar estado cuantas veces se necesite:
for <test start print it stop not printing start print again stop not anymore> {
  .say if $_ eq 'start' ^ff^ $_ eq 'stop'; # excluye a "start" y "stop",
                                           #=> "print it print again"
}

## También podrías usar una Whatever Star, lo cual es equivalente
## a `True` para el lado izquierdo o `False` para el lado derecho:
for (1, 3, 60, 3, 40, 60) { # Nota: los paréntesis son superfluos aquí
                            # (algunas veces se les llaman "paréntesis superticiosos")
 .say if $_ > 50 ff *; # Una vez el flip-flop alcanza un número mayor que 50,
                       # no returnará jamás a `False`
                       #=> 60 3 40 60
}

## También puedes usar esta propiedad para crear un `If`
## que no pasará la primera vez:
for <a b c> {
  .say if * ^ff *; # el flip-flop es `True` y nunca returna a `False`,
                   # pero el `^` lo hace *que no se ejecute* en la
                   # primera iteración
                   #=> b c
}

## - `===` es la identidad de valor y usa `.WHICH`
## en los objetos para compararlos.
## - `=:=` es la identidad de contenedor y usa `VAR()`
## en los objetos para compararlos.

```
Si quieres ir más allá de lo que se muestra aquí, puedes:

 - Leer la [documentación de Raku](https://docs.raku.org/). Esto es un recurso
 grandioso acerca de Raku. Si estás buscando por algo en particular, usa la
 barra de búsquedas. Esto te dará un menú de todas las páginas concernientes
 a tu término de búsqueda (¡Es mucho mejor que usar Google para encontrar
 documentos acerca de Raku!)
 - Leer el [Raku Advent Calendar](https://rakuadventcalendar.wordpress.com/). Este es
 un gran recurso de fragmentos de código de Raku y explicaciones. Si la documentación
 no describe algo lo suficientemente bien, puedes encontrar información más detallada
 aquí. Esta información puede ser un poquito más antigua pero hay muchos ejemplos y
 explicaciones. Las publicaciones fueron suspendidas al final del 2015 cuando
 el lenguaje fue declarado estable y Raku.c fue lanzado.
 - Unirte a `#raku` en `irc.freenode.net`. Las personas aquí son siempre serviciales.
 - Chequear la [fuente de las funciones y clases de Raku
 ](https://github.com/rakudo/rakudo/tree/master/src/core.c). Rakudo está principalmente
 escrito en Raku (con mucho de NQP, "Not Quite Perl" ("No Perl Todavía"), un
 subconjunto de Raku que es más fácil de implementar y optimizar).
 - Leer [documentos acerca del diseño del lenguaje](http://design.raku.org).
 Estos explican P6 desde la perspectiva de un implementador, lo cual es bastante
 interesante.
