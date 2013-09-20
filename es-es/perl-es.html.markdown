---
name: perl
category: language
language: perl
filename: learnperl-es.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
translators:
    - ["Francisco Gomez", "http://github.com/frncscgmz"]
lang: es-es
---

Perl 5 es un lenguaje de programación altamente capaz, rico en características con mas de 25 años de desarrollo.

Perl 5 corre en mas de 100 plataformas desde portales hasta mainframes y es adecuado para realizar prototipos rápidos hasta desarrollar proyectos a gran escala.

```perl
# Comentarios de una sola linea con un carácter hash.

#### Tipos de variables en Perl

# Las variables comienzan con el símbolo $.
# Un nombre de variable valido empieza con una letra o un guión bajo,
# seguido por cualquier numero de letras, números o guiones bajos.

### Perl tiene tres tipos principales de variables: escalares, arreglos y hashes.

## Escalares
# Un escalar representa un solo valor:
my $animal    = "camello";
my $respuesta = 42;

# Los valores escalares pueden ser cadenas de caracteres, números enteros o 
# de punto flotante, Perl automáticamente los convertirá como sea requerido.

## Arreglos
# Un arreglo representa una lista de valores:
my @animales = {"camello","llama","buho"};
my @numeros  = {23,42,69};
my @mixto    = {"camello",42,1.23};



## Hashes
#   Un hash representa un conjunto de pares llave/valor:

my %color_fruta = {"manzana","rojo","banana","amarillo"};

#  Puedes usar un espacio en blanco y el operador "=>" para asignarlos mas
#  fácilmente.

my %color_fruta = (
   manzana => "rojo",
   banana  => "amarillo",
   );
# Los escalares, arreglos y hashes están mas documentados en perldata. (perldoc perldata).

# Los tipos de datos mas complejos pueden ser construidos utilizando 
# referencias, las cuales te permiten construir listas y hashes dentro 
# de listas y hashes.

#### Estructuras condicionales y de ciclos

# Perl tiene la mayoría de las estructuras condicionales y de ciclos mas comunes.

if ( $var ) {
     ...
} elsif ( $var eq 'bar' ) { 
     ...
} else {
     ...
}

unless ( condicion ) {
                  ...
              }
# Esto es proporcionado como una version mas fácil de leer que "if (!condición)"

# La post condición al modo Perl
print "Yow!" if $zippy;
print "No tenemos bananas" unless $bananas;

# while
  while ( condicion ) {
                  ...
               }


# for y foreach
for ($i = 0; $i <= $max; $i++) {
                  ...
               }

foreach (@array) {
                   print "Este elemento es $_\n";
               }


#### Expresiones regulares

# El soporte de expresiones regulares en Perl es muy amplio y profundo, y es 
# sujeto a una extensa documentación en perlrequick, perlretut, entre otros.
# Sin embargo, resumiendo:

# Pareo simple
if (/foo/)       { ... }  # verdadero si $_ contiene "foo"
if ($a =~ /foo/) { ... }  # verdadero si $a contiene "foo"

# Substitución simple
$a =~ s/foo/bar/;         # remplaza foo con bar en $a
$a =~ s/foo/bar/g;        # remplaza TODAS LAS INSTANCIAS de foo con bar en $a


#### Archivos e I/O

# Puedes abrir un archivo para obtener datos o escribirlos utilizando la 
# función "open()".

open(my $entrada, "<"  "entrada.txt") or die "No es posible abrir entrada.txt: $!";
open(my $salida,  ">", "salida.txt")  or die "No es posible abrir salida.txt: $!";
open(my $log,     ">>", "mi.log")     or die "No es posible abrir mi.log: $!";

# Es posible leer desde un gestor de archivo abierto utilizando el operador "<>" 
# operador. En contexto escalar leer una sola linea desde el gestor de 
# archivo, y en contexto de lista leer el archivo completo en donde, asigna 
# cada linea a un elemento de la lista.

my $linea  = <$entrada>;
my @lineas = <$entrada>;

#### Escribiendo subrutinas

# Escribir subrutinas es fácil:

sub logger {
   my $mensajelog = shift;
   open my $archivolog, ">>", "mi.log" or die "No es posible abrir mi.log: $!";
   print $archivolog $mensajelog;
}

# Ahora podemos utilizar la subrutina al igual que cualquier otra función 
# incorporada:

logger("Tenemos una subrutina logger!");


```

#### Utilizando módulos Perl

Los módulos en Perl proveen una gama de funciones que te pueden ayudar a evitar reinventar la rueda, estas pueden ser descargadas desde CPAN( http://www.cpan.org/ ). Algunos de los módulos mas populares ya están incluidos con la misma distribución de Perl. 

perlfaq contiene preguntas y respuestas relacionadas con muchas tareas comunes, y algunas veces provee sugerencias sobre buenos módulos CPAN para usar.

#### Material de Lectura

     - [perl-tutorial](http://perl-tutorial.org/)
     - [Aprende en www.perl.com](http://www.perl.org/learn.html)
     - [perldoc](http://perldoc.perl.org/)
     - y perl incorporado: `perldoc perlintro`
