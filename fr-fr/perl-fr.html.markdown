---
name: perl
category: language
language: perl
filename: learnperl-fr.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
    - ["Matteo Taroli", "http://www.matteotaroli.be"]
translators:
    - ["Matteo Taroli", "http://www.matteotaroli.be"]
lang: fr-fr
---
Perl 5 est un langage de programmation riche en fonctionnalité, avec plus de 25 ans de développement.

Perl 5 fonctionne sur plus de 100 plateformes, allant des pc portables aux mainframes et
est autant adapté à un prototypage rapide qu'à des projets de grande envergure.

```perl
#  Les commentaires en une ligne commencent par un dièse


#### Types de variables de Perl

# Les variables comment par un symbole précisant le type.
# Un nom de variable valide commence par une lettre ou un underscore,
# suivi d'un nombre quelconque de lettres, chiffres ou underscores.

### Perl a trois types principaux de variables: $scalaire, @tableau and %hash

## Scalaires
#  Un scalaire représente une valeur unique :
my $animal = "chameau";
my $reponse = 42;

# Les valeurs scalaires peuvent être des strings, des entiers ou des nombres à virgule flottante
# et Perl les convertira automatiquement entre elles quand nécessaire.

## Tableaux
#  Un tableau représente une liste de valeurs :
my @animaux = ("chameau", "lama", "chouette");
my @nombres = (23, 42, 69);
my @melange = ("chameau", 42, 1.23);

## Hashes
#  Un hash représente un ensemble de paires de clé/valeur :
my %fruit_couleur = ("pomme", "rouge", "banane", "jaune");

# Vous pouvez utiliser des espaces et l'opérateur "=>" pour les disposer plus joliment :

my %fruit_couleur = (
	pomme  => "rouge",
	banane => "jaune"
);

# Les scalaires, tableaux et hashes sont plus amplement documentés dans le perldata
# (perldoc perldata)

# Des types de données plus complexes peuvent être construits en utilisant des références,
# vous permettant de construire des listes et des hashes à l'intérieur d'autres listes et hashes.

#### Conditions et boucles

# Perl possède la plupart des conditions et boucles habituelles.

if ($var) {
  ...
} elsif ($var eq 'bar') {
  ...
} else {
  ...
}

unless (condition) {
  ...
}
# Ceci est fourni en tant que version plus lisible de "if (!condition)"

# la postcondition à la sauce Perl

print "Yow!" if $zippy;
print "Nous n'avons pas de banane." unless $bananes;

#  while
while (condition) {
  ...
}

# boucle for et iteration
for (my $i = 0; $i < $max; $i++) {
  print "l'index est $i";
}

for (my $i = 0; $i < @elements; $i++) {
  print "L'élément courant est " . $elements[$i];
}

for my $element (@elements) {
  print $element;
}

# implicitement

# La variable de contexte scalaire $_ est utilisée par défaut dans différentes
# situations, comme par exemple dans la boucle foreach ou en argument par défaut
# de la plupart des fonctions pour en simplifier l'écriture.

# Dans l'exemple suivant, $_ prends successivement la valeur de
# chaque élément de la liste.  

for (@elements) {
  print; # affiche le contenu de $_
}


#### Expressions régulières

# Le support des expressions régulières par Perl est aussi large que profond
# et est sujet à une longue documentation sur perlrequick, perlretut et ailleurs.
# Cependant, pour faire court :

# Simple correspondance
if (/foo/)       { ... }  # vrai si $_ contient "foo"
if ($a =~ /foo/) { ... }  # vrai si $a contient "foo"

# Simple substitution

$a =~ s/foo/bar/;         # remplace le premier foo par bar dans $a
$a =~ s/foo/bar/g;        # remplace TOUTES LES INSTANCES de foo par bar dans $a


#### Fichiers et E/S

# Vous pouvez ouvrir un fichier pour y écrire ou pour le lire avec la fonction "open()".

open(my $in,  "<",  "input.txt")  or die "Impossible d'ouvrir input.txt: $!";
open(my $out, ">",  "output.txt") or die "Impossible d'ouvrir output.txt: $!";
open(my $log, ">>", "my.log")     or die "Impossible d'ouvrir my.log: $!";

# Vous pouvez lire depuis un descripteur de fichier grâce à l'opérateur "<>".
# Dans un contexte scalaire, il lit une seule ligne depuis le descripteur de fichier
# et dans un contexte de liste, il lit le fichier complet, assignant chaque ligne à un
# élément de la liste :

my $ligne = <$in>
my $lignes = <$in>

#### Ecrire des fonctions

# Ecrire des fonctions est facile :

sub logger {
  my $logmessage = shift;

  open my $logfile, ">>", "my.log" or die "Impossible d'ouvrir my.log: $!";

  print $logfile $logmessage;
}

# Maintenant, nous pouvons utiliser cette fonction comme n'importe quelle fonction intégrée :

logger("On a une fonction de logging!!");
```

#### Utiliser des modules Perl

Les modules Perl fournissent une palette de fonctionnalités vous évitant de réinventer la roue et peuvent être téléchargés depuis CPAN (http://www.cpan.org/). Un certain nombre de modules populaires sont inclus dans la distribution même de Perl.

Perlfaq contiens des questions et réponses liées aux tâches habituelles et propose souvent des suggestions quant aux bons modules à utiliser.

#### Pour en savoir plus
 - [perl-tutorial](http://perl-tutorial.org/)
 - [Learn at www.perl.com](http://www.perl.org/learn.html)
 - [perldoc](http://perldoc.perl.org/)
 - and perl built-in : `perldoc perlintro`
