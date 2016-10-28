---
language: D
filename: learnd-fr.d
contributors:
    - ["Nick Papanastasiou", "www.nickpapanastasiou.github.io"]
translators:
    - ["Quentin Ladeveze", "aceawan.eu"]
lang: fr-fr
---

```c
// Commençons par un classique
module hello;

import std.stdio;

// args n'est pas obligatoire
void main(string[] args) {
    writeln("Bonjour le monde !");
}
```

Si vous êtes comme moi et que vous passez beaucoup trop de temps sur internet, il y a
de grandes chances pour que vous ayez déjà entendu parler du [D](http://dlang.org/).
D est un langage de programmation moderne, généraliste, multi-paradigmes qui contient
des fonctionnalités aussi bien de bas niveau que de haut niveau.

D est activement développé par de nombreuses personnes très intelligents, guidées par
[Walter Bright](https://fr.wikipedia.org/wiki/Walter_Bright))) et
[Andrei Alexandrescu](https://fr.wikipedia.org/wiki/Andrei_Alexandrescu).
Après cette petite introduction, jetons un coup d'oeil à quelques exemples.

```c
import std.stdio;

void main() {
    //Les conditions et les boucles sont classiques.
    for(int i = 0; i < 10000; i++) {
        writeln(i);
    }

    // On peut utiliser auto pour inférer automatiquement le
    // type d'une variable.
    auto n = 1;

    // On peut faciliter la lecture des valeurs numériques
    // en y insérant des `_`.
    while(n < 10_000) {
        n += n;
    }

    do {
        n -= (n / 2);
    } while(n > 0);

    // For et while sont très utiles, mais en D, on préfère foreach.
    // Les deux points : '..', créent un intervalle continue de valeurs
    // incluant la première mais excluant la dernière.
    foreach(i; 1..1_000_000) {
        if(n % 2 == 0)
            writeln(i);
    }

    // On peut également utiliser foreach_reverse pour itérer à l'envers.
    foreach_reverse(i; 1..int.max) {
        if(n % 2 == 1) {
            writeln(i);
        } else {
            writeln("Non !");
        }
    }
}
```
On peut définir de nouveaux types avec les mots-clés `struct`, `class`,
`union` et `enum`. Ces types sont passés au fonction par valeur (ils sont copiés)
De plus, on peut utiliser les templates pour rendre toutes ces abstractions génériques.

```c
// Ici, 'T' est un paramètre de type. Il est similaire au <T> de C++/C#/Java.
struct LinkedList(T) {
    T data = null;

	// Utilisez '!' pour instancier un type paramétré.
	// Encore une fois semblable à '<T>'
    LinkedList!(T)* next;
}

class BinTree(T) {
    T data = null;

    // Si il n'y a qu'un seul paramètre de template,
    // on peut s'abstenir de mettre des parenthèses.
    BinTree!T left;
    BinTree!T right;
}

enum Day {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
}

// Utilisez alias pour créer des abreviations pour les types.
alias IntList = LinkedList!int;
alias NumTree = BinTree!double;

// On peut tout aussi bien créer des templates de function !
T max(T)(T a, T b) {
    if(a < b)
        return b;

    return a;
}

// On peut utiliser le mot-clé ref pour s'assurer que quelque chose est passé
// par référence, et ceci, même si a et b sont d'ordinaire passés par valeur.
// Ici ils seront toujours passés par référence à 'swap()'.
void swap(T)(ref T a, ref T b) {
    auto temp = a;

    a = b;
    b = temp;
}

// Avec les templates, on peut également passer des valeurs en paramètres.
class Matrix(uint m, uint n, T = int) {
    T[m] rows;
    T[n] columns;
}

auto mat = new Matrix!(3, 3); // T est 'int' par défaut

```
À propos de classes, parlons des propriétés. Une propriété est, en gros,
une méthode qui peut se comporter comme une lvalue. On peut donc utiliser
la syntaxe des structures classiques (`struct.x = 7`) comme si il
s'agissait de méthodes getter ou setter.

```c
// Considérons une classe paramétrée avec les types 'T' et 'U'
class MyClass(T, U) {
    T _data;
    U _other;
}

// Et des méthodes "getter" et "setter" comme suit:
class MyClass(T, U) {
    T _data;
    U _other;

	// Les constructeurs s'apellent toujours 'this'.
    this(T t, U u) {
		// Ceci va appeller les setters ci-dessous.
        data = t;
        other = u;
    }

    // getters
    @property T data() {
        return _data;
    }

    @property U other() {
        return _other;
    }

    // setters
    @property void data(T t) {
        _data = t;
    }

    @property void other(U u) {
        _other = u;
    }
}

// Et on l'utilise de cette façon:
void main() {
    auto mc = new MyClass!(int, string)(7, "seven");

	// Importer le module 'stdio' de la bibliothèque standard permet
	// d'écrire dans la console (les imports peuvent être locaux à une portée)
    import std.stdio;

	// On appelle les getters pour obtenir les valeurs.
    writefln("Earlier: data = %d, str = %s", mc.data, mc.other);

	// On appelle les setter pour assigner de nouvelles valeurs.
    mc.data = 8;
    mc.other = "eight";

	// On appelle les setter pour obtenir les nouvelles valeurs.
    writefln("Later: data = %d, str = %s", mc.data, mc.other);
}
```
Avec les propriétés, on peut constuire nos setters et nos getters
comme on le souhaite, tout en gardant un syntaxe très propre,
comme si on accédait directement à des membres de la classe.

Les autres fonctionnalités orientées objets à notre disposition
incluent les interfaces, les classes abstraites, et la surcharge
de méthodes. D gère l'héritage comme Java: On ne peut hériter que
d'une seule classe et implémenter autant d'interface que voulu.

Nous venons d'explorer les fonctionnalités objet du D, mais changeons
un peu de domaine. D permet la programmation fonctionelle, avec les fonctions
de premier ordre, les fonctions `pure` et les données immuables.
De plus, tout vos algorithmes fonctionelles favoris (map, reduce, filter)
sont disponibles dans le module `std.algorithm`.

```c
import std.algorithm : map, filter, reduce;
import std.range : iota; // construit un intervalle excluant la dernière valeur.

void main() {
	// On veut un algorithm qui affiche la somme de la listes des carrés
	// des entiers paires de 1 à 100. Un jeu d'enfant !

	// On se content de passer des expressions lambda en paramètre à des templates.
	// On peut fournier au template n'importe quelle fonction, mais dans notre
	// cas, les lambdas sont pratiques.
    auto num = iota(1, 101).filter!(x => x % 2 == 0)
                           .map!(y => y ^^ 2)
                           .reduce!((a, b) => a + b);

    writeln(num);
}
```

Vous voyez comme on a calculé `num` comme on le ferait en haskell par exemple ?
C'est grâce à une innvoation de D qu'on appelle "Uniform Function Call Syntax".
Avec l'UFCS, on peut choisir d'écrire un appelle à une fonction de manière
classique, ou comme un appelle à une méthode. Walter Brighter a écrit un
article en anglais sur l'UFCS [ici.](http://www.drdobbs.com/cpp/uniform-function-call-syntax/232700394)
Pour faire court, on peut appeller une fonction dont le premier paramètre
est de type A, comme si c'était une méthode de A.

J'aime le parallélisme. Vous aimez les parallélisme ? Bien sur que vous aimez ça
Voyons comment on le fait en D !

```c
import std.stdio;
import std.parallelism : parallel;
import std.math : sqrt;

void main() {
    // On veut calculer la racine carré de tous les nombres
    // dans notre tableau, et profiter de tous les coeurs
    // à notre disposition.
    auto arr = new double[1_000_000];

    // On utilise un index et une référence à chaque élément du tableau.
    // On appelle juste la fonction parallel sur notre tableau !
    foreach(i, ref elem; parallel(arr)) {
        ref = sqrt(i + 1.0);
    }
}


```
