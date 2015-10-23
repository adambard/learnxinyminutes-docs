---
language: Scala
contributors:
    - ["George Petrov", "http://github.com/petrovg"]
    - ["Dominic Bou-Samra", "http://dbousamra.github.com"]
translators:
    - ["Anne-Catherine Dehier", "https://github.com/spellart"]
filename: learnscala-fr.scala
lang: fr-fr
---

### Scala - le langage évolutif

```scala

/*
  Pour vous préparer :

  1) (Téléchargez Scala)[http://www.scala-lang.org/downloads]
  2) Dézippez/décompressez dans votre endroit préféré
  et ajoutez le chemin du sous-répertoire bin au chemin du système
  3) Commencez un REPL de Scala en tapant juste scala. Vous devriez voir le prompteur :

  scala>

  C'est ce qu'on appelle un REPL (Read-Eval-Print-Loop), c'est une interface de programmation interactive.
  Vous pouvez y exécuter des commandes.
  Allons-y :
*/

println(10) // affiche l'integer 10

println("Boo!") // affiche avec retour à la ligne la chaîne de caractère Boo!


// Quelques basiques

// Imprimer et forcer une nouvelle ligne à la prochaine impression
println("Hello world!")
// Imprimer sans forcer une nouvelle ligne à la prochaine impression
print("Hello world")

// Pour déclarer des valeurs on utilise var ou val
// Les déclarations val sont immuables, tandis que les var sont muables.
// L'immuabilité est une bonne chose.

val x = 10 // x vaut maintenant 10
x = 20 // erreur : réaffectation à val
var x = 10
x = 20 // x vaut maintenant 20

// Les commentaires d'une ligne commencent par deux slashs

/*
Les commentaires multilignes ressemblent à ça.
*/

// les valeurs booléennes
true
false

// Les opérateurs booléens
!true // false
!false // true
true == false // false
10 > 5 // true

// Les opérateurs mathématiques sont habituels
1 + 1 // 2
2 - 1 // 1
5 * 3 // 15
6 / 2 // 3


// Le REPL donne le type et la valeur du résultat quand vous évaluez une commande

1 + 7

/* Les lignes ci-dessous donnent les résultats :

  scala> 1 + 7
  res29: Int = 8

  Ça signifie que le résultat de l'évaluation 1 + 7 est un objet de
  type Int avec une valeur de 8

  1+7 donnera le même résultat
*/


// Tout est un objet, même une fonction. Tapez ceci dans le REPL :

7 // donne res30: Int = 7 (res30 est seulement un nom de variable généré pour le résultat)


// La ligne suivante est une fonction qui prend un Int et retourne son carré
(x:Int) => x * x


// On peut assigner cette fonction à un identifieur comme ceci :
val sq = (x:Int) => x * x

/* La ligne suivante nous dit :

   sq: Int => Int = <function1>

   Ce qui signifie que cette fois-ci nous avons donné un nom explicite à la valeur.
   sq est une fonction qui prend un Int et retourne un Int.


   sq peut être exécutée comme ci-dessous :
*/

sq(10) // donne comme résultat : res33: Int = 100.


// les deux-points définissent explicitement le type de la valeur,
// dans ce cas une fonction qui prend un Int et retourne un Int.
val add10: Int => Int = _ + 10

// Scala autorise des méthodes et des fonctions à retourner
// ou prendre comme paramètres des autres fonctions ou méthodes


List(1, 2, 3) map add10 // List(11, 12, 13) - add10 est appliqué à chaque éléments


// Les fonctions anonymes peuvent être utilisées à la place des fonctions nommées :
List(1, 2, 3) map (x => x + 10)




// Le tiret du bas peut être utilisé si la fonction anonyme ne prend qu'un paramètre.
// Il se comporte comme une variable
List(1, 2, 3) map (_ + 10)



// Si le bloc et la fonction anonyme prennent tous les deux un seul argument,
// vous pouvez omettre le tiret du bas
List("Dom", "Bob", "Natalia") foreach println



// Les structures de données

val a = Array(1, 2, 3, 5, 8, 13)
a(0)
a(3)
a(21)    // Lance une exception

val m = Map("fork" -> "tenedor", "spoon" -> "cuchara", "knife" -> "cuchillo")
m("fork")
m("spoon")
m("bottle")       // Lance une exception

val safeM = m.withDefaultValue("no lo se")
safeM("bottle")

val s = Set(1, 3, 7)
s(0)
s(1)

/* Jetez un oeil sur la documentation de map ici -
 * http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.Map
 */


// Tuples

(1, 2)

(4, 3, 2)

(1, 2, "three")

(a, 2, "three")

// Exemple d'utilisation
val divideInts = (x:Int, y:Int) => (x / y, x % y)


divideInts(10,3) // La fonction divideInts donne le résultat et le reste de la division

// Pour accéder à un élément d'un tuple, utilisez _._n
// où n est l'index de base 1 de l'élément
val d = divideInts(10,3)

d._1

d._2



// Des combinaisons

s.map(sq)

val sSquared = s. map(sq)

sSquared.filter(_ < 10)

sSquared.reduce (_+_)



// La fonction filter prend un prédicat (une fonction de type A -> Booléen) et
// sélectionne tous les éléments qui satisfont ce prédicat
List(1, 2, 3) filter (_ > 2) // List(3)
case class Person(name: String, age: Int)
List(
  Person(name = "Dom", age = 23),
  Person(name = "Bob", age = 30)
).filter(_.age > 25) // List(Person("Bob", 30))



// Scala a une méthode foreach définie pour certaines collections
// qui prend en argument une fonction renvoyant Unit (une méthode void)
val aListOfNumbers = List(1, 2, 3, 4, 10, 20, 100)
aListOfNumbers foreach (x => println(x))
aListOfNumbers foreach println




// Compréhensions de listes

for { n <- s } yield sq(n)

val nSquared2 = for { n <- s } yield sq(n)

for { n <- nSquared2 if n < 10 } yield n

for { n <- s; nSquared = n * n if nSquared < 10} yield nSquared



/* Les exemples précédents ne sont pas des boucles for. La sémantique des boucles for
   est "répète", alors qu'une for-compréhension définit une relation
   entre deux ensembles de données. */



// Boucles et itération

1 to 5
val r = 1 to 5
r.foreach( println )

r foreach println
// NB: Scala est vraiment tolérant par rapport aux points et aux parenthèses en étudiant les roles séparément.
// Ça aide pour écrire des DSL ou des API qui se lisent comme en anglais.


(5 to 1 by -1) foreach ( println )

// Une boucle while
var i = 0
while (i < 10) {  println("i " + i); i+=1  }

while (i < 10) {  println("i " + i); i+=1  }  // Oui, encore. Qu'est-ce qui s'est passé ? Pourquoi ?






i    // Montre la valeur de i. Notez que while est une boucle au sens classique.
     // Il exécute séquentiellement pendant que la variable de boucle change.
     // While est très rapide,
     // mais utiliser des combinateurs et des compréhensions comme ci-dessus est plus
     // facile pour comprendre et pour faire la parallélisation

i = 0
// La boucle do while
do {
  println("x is still less then 10");
  i += 1
} while (i < 10)


// La récursivité est un moyen idiomatique de faire une chose répétitive en Scala.
// Les fonctions récursives ont besoin d'un type de retour explicite,
// le compilateur ne peut pas le déduire.
// Ici c'est Unit.
def showNumbersInRange(a:Int, b:Int):Unit = {
  print(a)
  if (a < b)
    showNumbersInRange(a + 1, b)
}



// Structures de contrôle

val x = 10

if (x == 1) println("yeah")
if (x == 10) println("yeah")
if (x == 11) println("yeah")
if (x == 11) println ("yeah") else println("nay")

println(if (x == 10) "yeah" else "nope")
val text = if (x == 10) "yeah" else "nope"

var i = 0
while (i < 10) { println("i " + i); i+=1  }



// Les caractéristiques "Orienté Objet"

// Création d'une classe Dog
class Dog {
  // Une méthode appelée bark qui retourne une chaîne de caractère
  def bark: String = {
    // le corps de la méthode
    "Woof, woof!"
  }
}


// Les classes peuvent contenir presque n'importe quelle autre construction, incluant d'autres classes,
// des fonctions, des méthodes, des objets, des classes case, des traits, etc ...



// Les classes case

case class Person(name:String, phoneNumber:String)

Person("George", "1234") == Person("Kate", "1236")




// Correspondances de motifs

val me = Person("George", "1234")

me match { case Person(name, number) => {
            "We matched someone : " + name + ", phone : " + number }}

me match { case Person(name, number) => "Match : " + name; case _ => "Hm..." }

me match { case Person("George", number) => "Match"; case _ => "Hm..." }

me match { case Person("Kate", number) => "Match"; case _ => "Hm..." }

me match { case Person("Kate", _) => "Girl"; case Person("George", _) => "Boy" }

val kate = Person("Kate", "1234")

kate match { case Person("Kate", _) => "Girl"; case Person("George", _) => "Boy" }



// Expressions régulières

val email = "(.*)@(.*)".r  // On fait une Regex en invoquant r sur la chaîne de caractère

val email(user, domain) = "henry@zkpr.com"

"mrbean@pyahoo.com" match {
  case email(name, domain) => "I know your name, " + name
}



// Les chaînes de caractères

"Les chaînes de caractères Scala sont entourées de doubles guillements"
'a' // Un caractère de Scala
// 'Les simples guillemets n'existent pas en Scala' // Erreur
"Les chaînes de caractères possèdent les méthodes usuelles de Java".length
"Il y a aussi quelques méthodes extra de Scala.".reverse

// Voir également : scala.collection.immutable.StringOps

println("ABCDEF".length)
println("ABCDEF".substring(2, 6))
println("ABCDEF".replace("C", "3"))

val n = 45
println(s"We have $n apples")

val a = Array(11, 9, 6)
println(s"My second daughter is ${a(2-1)} years old")

// Certains caractères ont besoin d'être "échappés",
// ex un guillemet à l'intérieur d'une chaîne de caractères :
val a = "They stood outside the \"Rose and Crown\""

// Les triples guillemets permettent d'écrire des chaînes de caractères
// sur plusieurs lignes et peuvent contenir des guillemets

val html = """<form id="daform">
                <p>Press belo', Joe</p>
             |  <input type="submit">
              </form>"""



// Structure et organisation d'une application

// Importer des chaînes de caratères
import scala.collection.immutable.List

// Importer tous les sous-paquets
import scala.collection.immutable._

// Importer plusieurs classes en une seule instruction
import scala.collection.immutable.{List, Map}

// Renommer un import en utilisant '=>'
import scala.collection.immutable.{ List => ImmutableList }

// Importer toutes les classes, à l'exception de certaines.
// La ligne suivante exclut Map et Set :
import scala.collection.immutable.{Map => _, Set => _, _}

// Le point d'entrée du programme est défini dans un fichier scala
// utilisant un objet, avec une simple méthode main :
object Application {
  def main(args: Array[String]): Unit = {
    // Votre code ici.
  }
}

// Les fichiers peuvent contenir plusieurs classes et plusieurs objets.
// On les compile avec scalac




// Entrée et Sortie

// Pour lire un fichier ligne par ligne
import scala.io.Source
for(line <- Source.fromFile("myfile.txt").getLines())
  println(line)

// On utilise le PrintWriter de Java pour écrire un fichier


```

## Autres ressources

[Scala for the impatient](http://horstmann.com/scala/)

[Twitter Scala school](http://twitter.github.io/scala_school/)

[The scala documentation](http://docs.scala-lang.org/)

[Try Scala in your browser](http://scalatutorials.com/tour/)

Rejoindre le [Scala user group](https://groups.google.com/forum/#!forum/scala-user)
