---
name: Go
category: language
language: Go
lang: fr-fr
filename: learngo-fr.go
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["Christopher Bess", "https://github.com/cbess"]
    - ["Jesse Johnson", "https://github.com/holocronweaver"]
    - ["Quint Guvernator", "https://github.com/qguv"]
    - ["Jose Donizetti", "https://github.com/josedonizetti"]
    - ["Alexej Friesen", "https://github.com/heyalexej"]
    - ["Jean-Philippe Monette", "http://blogue.jpmonette.net/"]
---

Go a été créé dans l'optique de développer de façon efficace. Ce n'est pas la
dernière tendance en ce qui est au développement, mais c'est la nouvelle façon
de régler des défis réels de façon rapide.

Le langage possède des concepts familiers à la programmation impérative avec
typage. Il est rapide à compiler et exécuter, ajoute une concurrence facile à
comprendre, pour les processeurs multi coeurs d'aujourd'hui et apporte des
fonctionnalités facilitant le développement à grande échelle.

Développer avec Go, c'est bénéficier d'une riche bibliothèque standard et d'une
communauté active.

```go
// Commentaire ligne simple
/* Commentaire
 multiligne */

// Un paquet débute avec une clause "package"
// "Main" est un nom spécial déclarant un paquet de type exécutable plutôt
// qu'une bibliothèque
package main

// "Import" déclare les paquets référencés dans ce fichier.
import (
  "fmt"       // Un paquet dans la bibliothèque standard.
  "io/ioutil" // Implémente des fonctions utilitaires I/O.
  m "math"    // Bibliothèque mathématique utilisant un alias local "m".
  "net/http"  // Un serveur Web!
  "strconv"   // Bibliothèque pour convertir les chaînes de caractères.
)

// Une définition de fonction. La fonction "main" est spéciale - c'est le point
// d'entrée du binaire.
func main() {
  // Println retournera la valeur à la console.
  // Associez la fonction avec son paquet respectif, fmt.
  fmt.Println("Hello world!")

  // Appelez une fonction différente à partir de ce paquet.
  beyondHello()
}

// Les fonctions ont des paramètres entre parenthèses.
// Les parenthèses sont nécessaires avec ou sans paramètre.
func beyondHello() {
  var x int // Déclaration de variable. Les variables doivent être déclarées
            // avant leur utilisation.
  x = 3     // Assignation de valeur.
  // Les déclarations courtes utilisent := pour inférer le type, déclarer et
  // assigner.
  y := 4
  sum, prod := learnMultiple(x, y)        // La fonction retourne deux valeurs.
  fmt.Println("sum:", sum, "prod:", prod) // Affichage simple.
  learnTypes()                            // < y minutes, en savoir plus!
}

// Les fonctions peuvent avoir des paramètres et plusieurs valeurs retournées.
func learnMultiple(x, y int) (sum, prod int) {
  return x + y, x * y // Deux valeurs retournées.
}

// Quelques types inclus et littéraux.
func learnTypes() {
  // Une déclaration courte infère généralement le type désiré.
  str := "Learn Go!" // Type string.

  s2 := `Une chaîne de caractères peut contenir des
sauts de ligne.` // Chaîne de caractère.

  // Littéral non-ASCII. Les sources Go utilisent le charset UTF-8.
  g := 'Σ' // type rune, un alias pour le type int32, contenant un caractère
           // unicode.

  f := 3.14195 // float64, un nombre flottant IEEE-754 de 64-bit.
  c := 3 + 4i  // complex128, considéré comme deux float64 par le compilateur.

  // Syntaxe "var" avec une valeur d'initialisation.
  var u uint = 7 // Non signé, mais la taille dépend selon l'entier.
  var pi float32 = 22. / 7

  // Conversion avec syntaxe courte.
  n := byte('\n') // byte est un alias du type uint8.

  // Les tableaux ont une taille fixe déclarée à la compilation.
  var a4 [4]int           // Un tableau de 4 ints, tous initialisés à 0.
  a3 := [...]int{3, 1, 5} // Un tableau initialisé avec une taille fixe de 3
  // éléments, contenant les valeurs 3, 1 et 5.

  // Les slices ont des tailles dynamiques. Les tableaux et slices ont chacun
  // des avantages, mais les cas d'utilisation des slices sont plus fréquents.
  s3 := []int{4, 5, 9}    // Comparable à a3.
  s4 := make([]int, 4)    // Alloue un slice de 4 ints, initialisés à 0.
  var d2 [][]float64      // Déclaration seulement, sans allocation de mémoire.
  bs := []byte("a slice") // Conversion d'une chaîne en slice de bytes.

  // Parce qu'elles sont dynamiques, les slices peuvent être jointes sur
  // demande. Pour joindre un élément à une slice, la fonction standard append()
  // est utilisée. Le premier argument est la slice à utiliser. Habituellement,
  // la variable tableau est mise à jour sur place, voir ci-bas.
  s := []int{1, 2, 3}     // Le résultat est une slice de taille 3.
  s = append(s, 4, 5, 6)  // Ajout de 3 valeurs. La taille est de 6.
  fmt.Println(s)          // La valeur est de [1 2 3 4 5 6]

  // Pour ajouter une slice à une autre, au lieu d'utiliser une liste de valeurs
  // atomiques, il est possible de mettre en argument une référence de
  // slice littérale grâce aux points de suspension.
  s = append(s, []int{7, 8, 9}...) // Le deuxième argument est une slice
                                   // littérale.
  fmt.Println(s)  // La slice contient [1 2 3 4 5 6 7 8 9]

  p, q := learnMemory() // Déclare p, q comme étant des pointeurs de type int.
  fmt.Println(*p, *q)   // * suit un pointeur. Ceci retourne deux ints.

  // Les maps sont des tableaux associatifs de taille dynamique, comme les
  // hash ou les types dictionnaires de certains langages.
  m := map[string]int{"trois": 3, "quatre": 4}
  m["un"] = 1

  // Les valeurs inutilisées sont considérées comme des erreurs en Go.
  // Un tiret bas permet d'ignorer une valeur inutilisée, évitant une erreur.
  _, _, _, _, _, _, _, _, _, _ = str, s2, g, f, u, pi, n, a3, s4, bs

  // Cependant, son affichage en console est considéré comme une utilisation,
  // ce qui ne sera pas considéré comme une erreur à la compilation.
  fmt.Println(s, c, a4, s3, d2, m)

  learnFlowControl() // De retour dans le flux.
}

// Il est possible, à l'opposé de plusieurs autres langages, de retourner des
// variables par leur nom à partir de fonctions.
// Assigner un nom à un type retourné par une fonction permet de retrouver sa
// valeur ainsi que d'utiliser le mot-clé "return" uniquement, sans plus.
func learnNamedReturns(x, y int) (z int) {
  z = x * y
  return // z est implicite, car la variable a été définie précédemment.
}

// La récupération de la mémoire est automatique en Go. Le langage possède des
// pointeurs, mais aucune arithmétique des pointeurs (*(a + b) en C). Vous
// pouvez produire une erreur avec un pointeur nil, mais pas en incrémentant un
// pointeur.
func learnMemory() (p, q *int) {
  // Les valeurs retournées p et q auront le type pointeur int.
  p = new(int) // Fonction standard "new" alloue la mémoire.
  // Le int alloué est initialisé à 0, p n'est plus nil.
  s := make([]int, 20) // Alloue 20 ints en un seul bloc de mémoire.
  s[3] = 7             // Assigne l'un des entiers.
  r := -2              // Déclare une autre variable locale.
  return &s[3], &r     // & retourne l'adresse d'un objet.
}

func expensiveComputation() float64 {
  return m.Exp(10)
}

func learnFlowControl() {
  // Bien que les "if" requièrent des accolades, les parenthèses ne sont pas
  // nécessaires pour contenir le test booléen.
  if true {
    fmt.Println("voilà!")
  }
  // Le formatage du code est standardisé par la commande shell "go fmt."
  if false {
    // bing.
  } else {
    // bang.
  }
  // Utilisez "switch" au lieu des "if" en chaîne
  x := 42.0
  switch x {
  case 0:
  case 1:
  case 42:
    // Les "case" n'ont pas besoin de "break;".
  case 43:
    // Non-exécuté.
  }
  // Comme les "if", les "for" n'utilisent pas de parenthèses.
  // Les variables déclarées dans les "for" et les "if" sont locales à leur
  // portée.
  for x := 0; x < 3; x++ { // ++ est une incrémentation.
    fmt.Println("itération ", x)
  }
  // x == 42 ici.

  // "For" est le seul type de boucle en Go, mais possède différentes formes.
  for { // Boucle infinie
    break    // C'est une farce
    continue // Non atteint.
  }

  // Vous pouvez utiliser une "range" pour itérer dans un tableau, une slice, une
  // chaîne, une map ou un canal. Les "range" retournent un canal ou deux
  // valeurs (tableau, slice, chaîne et map).
  for key, value := range map[string]int{"une": 1, "deux": 2, "trois": 3} {
    // pour chaque pair dans une map, affichage de la valeur et clé
    fmt.Printf("clé=%s, valeur=%d\n", key, value)
  }

  // À l'opposé du "for", := dans un "if" signifie la déclaration et
  // l'assignation y en premier, et ensuite y > x
  if y := expensiveComputation(); y > x {
    x = y
  }
  // Les fonctions littérales sont des fermetures.
  xBig := func() bool {
    return x > 10000
  }
  fmt.Println("xBig:", xBig()) // true (la valeur e^10 a été assignée à x).
  x = 1.3e3                    // Ceci fait x == 1300
  fmt.Println("xBig:", xBig()) // Maintenant false.

  // De plus, les fonctions littérales peuvent être définies et appelées
  // sur la même ligne, agissant comme argument à cette fonction, tant que:
  // a) la fonction littérale est appelée suite à (),
  // b) le résultat correspond au type de l'argument.
  fmt.Println("Ajoute + multiplie deux nombres : ",
    func(a, b int) int {
      return (a + b) * 2
    }(10, 2)) // Appelle la fonction avec les arguments 10 et 2
  // => Ajoute + double deux nombres : 24

  // Quand vous en aurez besoin, vous allez l'adorer.
  goto love
love:

  learnFunctionFactory() // func retournant func correspondant à fun(3)(3).
  learnDefer()           // Un survol de cette instruction importante.
  learnInterfaces()      // Incontournable !
}

func learnFunctionFactory() {
  // Les deux syntaxes sont identiques, bien que la seconde soit plus pratique.
  fmt.Println(sentenceFactory("été")("Une matinée d'", "agréable!"))

  d := sentenceFactory("été")
  fmt.Println(d("Une matinée d'", "agréable!"))
  fmt.Println(d("Une soirée d'", "relaxante!"))
}

// Le décorateur est un patron de conception commun dans d'autres langages.
// Il est possible de faire de même en Go avec des fonctions littérales
// acceptant des arguments.
func sentenceFactory(mystring string) func(before, after string) string {
  return func(before, after string) string {
    return fmt.Sprintf("%s %s %s", before, mystring, after) // nouvelle chaîne
  }
}

func learnDefer() (ok bool) {
  // Les déclarations différées sont exécutées avant la sortie d'une fonction.
  defer fmt.Println("les déclarations différées s'exécutent en ordre LIFO.")
  defer fmt.Println("\nCette ligne est affichée en premier parce que")
  // Les déclarations différées sont utilisées fréquemment pour fermer un
  // fichier, afin que la fonction ferme le fichier en fin d'exécution.
  return true
}

// Défini Stringer comme étant une interface avec une méthode, String.
type Stringer interface {
  String() string
}

// Défini pair comme étant une structure contenant deux entiers, x et y.
type pair struct {
  x, y int
}

// Défini une méthode associée au type pair. Pair implémente maintenant Stringer
func (p pair) String() string { // p s'appelle le "destinataire"
  // Sprintf est une autre fonction publique dans le paquet fmt.
  // La syntaxe avec point permet de faire référence aux valeurs de p.
  return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
  // La syntaxe avec accolade défini une "structure littérale". Celle-ci
  // s'évalue comme étant une structure. La syntaxe := déclare et initialise p
  // comme étant une instance.
  p := pair{3, 4}
  fmt.Println(p.String()) // Appelle la méthode String de p, de type pair.
  var i Stringer          // Déclare i instance de l'interface Stringer.
  i = p                   // Valide, car pair implémente Stringer.
  // Appelle la méthode String de i, de type Stringer. Retourne la même valeur
  // que ci-haut.
  fmt.Println(i.String())

  // Les fonctions dans le paquet fmt appellent la méthode String, demandant
  // aux objets d'afficher une représentation de leur structure.
  fmt.Println(p) // Affiche la même chose que ci-haut. Println appelle la
                 // méthode String.
  fmt.Println(i) // Affiche la même chose que ci-haut.

  learnVariadicParams("apprentissage", "génial", "ici!")
}

// Les fonctions peuvent être définie de façon à accepter un ou plusieurs
// paramètres grâce aux points de suspension, offrant une flexibilité lors de
// son appel.
func learnVariadicParams(myStrings ...interface{}) {
  // Itère chaque paramètre dans la range.
  // Le tiret bas sert à ignorer l'index retourné du tableau.
  for _, param := range myStrings {
    fmt.Println("paramètre:", param)
  }

  // Passe une valeur variadique comme paramètre variadique.
  fmt.Println("paramètres:", fmt.Sprintln(myStrings...))

  learnErrorHandling()
}

func learnErrorHandling() {
  // ", ok" idiome utilisée pour définir si l'opération s'est déroulée avec
  // succès ou non
  m := map[int]string{3: "trois", 4: "quatre"}
  if x, ok := m[1]; !ok { // ok sera faux, car 1 n'est pas dans la map.
    fmt.Println("inexistant")
  } else {
    fmt.Print(x) // x serait la valeur, si elle se trouvait dans la map.
  }
  // Une erreur ne retourne qu'un "ok", mais également plus d'information
  // par rapport à un problème survenu.
  if _, err := strconv.Atoi("non-int"); err != nil { // _ discarte la valeur
    // retourne: 'strconv.ParseInt: parsing "non-int": invalid syntax'
    fmt.Println(err)
  }
  // Nous réviserons les interfaces un peu plus tard. Pour l'instant,
  learnConcurrency()
}

// c est un canal, un objet permettant de communiquer en simultané de façon
// sécurisée.
func inc(i int, c chan int) {
  c <- i + 1 // <- est l'opérateur "envoi" quand un canal apparaît à
             // gauche.
}

// Nous utiliserons inc pour incrémenter des nombres en même temps.
func learnConcurrency() {
  // La fonction "make" utilisée précédemment pour générer un slice. Elle
  // alloue et initialise les slices, maps et les canaux.
  c := make(chan int)
  // Démarrage de trois goroutines simultanées. Les nombres seront incrémentés
  // simultanément, peut-être en paralèle si la machine le permet et configurée
  // correctement. Les trois utilisent le même canal.
  go inc(0, c) // go est une instruction démarrant une nouvelle goroutine.
  go inc(10, c)
  go inc(-805, c)
  // Lis et affiche trois résultats du canal - impossible de savoir dans quel
  // ordre !
  fmt.Println(<-c, <-c, <-c) // Canal à droite, <- est l'opérateur de
                             // "réception".

  cs := make(chan string)       // Un autre canal, celui-ci gère des chaînes.
  ccs := make(chan chan string) // Un canal de canaux de chaînes.
  go func() { c <- 84 }()       // Démarre une nouvelle goroutine, pour
                                // envoyer une valeur.
  go func() { cs <- "wordy" }() // De nouveau, pour cs cette fois-ci.
  // Select possède une syntaxe similaire au switch, mais chaque cas requiert
  // une opération impliquant un canal. Il sélectionne un cas aléatoirement
  // prêt à communiquer.
  select {
  case i := <-c: // La valeur reçue peut être assignée à une variable,
    fmt.Printf("c'est un %T", i)
  case <-cs: // ou la valeur reçue peut être ignorée.
    fmt.Println("c'est une chaîne")
  case <-ccs: // Un canal vide, indisponible à la communication.
    fmt.Println("ne surviendra pas.")
  }
  // À ce point, une valeur a été prise de c ou cs. L'une des deux goroutines
  // démarrée plus haut a complétée, la seconde restera bloquée.

  learnWebProgramming() // Go permet la programmation Web.
}

// Une seule fonction du paquet http démarre un serveur Web.
func learnWebProgramming() {

  // Le premier paramètre de ListenAndServe est une adresse TCP à écouter.
  // Le second est une interface, de type http.Handler.
  go func() {
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // n'ignorez pas les erreurs !
  }()

  requestServer()
}

// Implémente la méthode ServeHTTP de http.Handler à pair, la rendant compatible
// avec les opérations utilisant l'interface http.Handler.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  // Répondez à une requête à l'aide de la méthode http.ResponseWriter.
  w.Write([]byte("Vous avez appris Go en Y minutes!"))
}

func requestServer() {
  resp, err := http.Get("http://localhost:8080")
  fmt.Println(err)
  defer resp.Body.Close()
  body, err := ioutil.ReadAll(resp.Body)
  fmt.Printf("\nLe serveur Web a dit: `%s`", string(body))
}
```

## En savoir plus

La référence Go se trouve sur [le site officiel de Go](http://golang.org/).
Vous pourrez y suivre le tutoriel interactif et en apprendre beaucoup plus.

Une lecture de la documentation du langage est grandement conseillée. C'est
facile à lire et très court (comparé aux autres langages).

Vous pouvez exécuter et modifier le code sur [Go playground](https://play.golang.org/p/tnWMjr16Mm). Essayez de le modifier et de l'exécuter à partir de votre navigateur! Prennez en note que vous pouvez utiliser [https://play.golang.org](https://play.golang.org) comme un [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop) pour tester et coder dans votre navigateur, sans même avoir à installer Go.

Sur la liste de lecteur des étudiants de Go se trouve le [code source de la
librairie standard](http://golang.org/src/pkg/). Bien documentée, elle démontre
le meilleur de la clarté de Go, le style ainsi que ses expressions. Sinon, vous
pouvez cliquer sur le nom d'une fonction dans [la
documentation](http://golang.org/pkg/) et le code source apparaît!

Une autre excellente ressource pour apprendre est [Go par l'exemple](https://gobyexample.com/).
