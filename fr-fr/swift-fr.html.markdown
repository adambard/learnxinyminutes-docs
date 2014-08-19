---
language: swift
contributors:
  - ["Grant Timmerman", "http://github.com/grant"]
translators:
  - ["@prrrnd", "https://github.com/prrrnd"]
lang: fr-fr
---

Swift est un langage de programmation crée par Apple pour iOS et OS X. Swift a été introduit en 2014 à la conférence WWDC d'Apple. Il est construit avec le compilateur LLVM inclus dans la version bétâ de Xcode 6.

Pour plus d'informations, en anglais, regardez le [guide d'Apple](https://developer.apple.com/library/prerelease/ios/referencelibrary/GettingStarted/LandingPage/index.html), qui inclus un tutoriel complet sur Swift.

```js
//
// Bases
//

println("Hello, world")
var myVariable = 42
let myConstant = 3.1415926
let explicitDouble: Double = 70
let label = "du texte " + String(myVariable)      // Cast
let piText = "Pi = \(myConstant)"                 // Interpolation
var optionalString: String? = "optional"          // Peut être nil
optionalString = nil


//
// Tableaux et dictionnaires
//

// Tableau
var shoppingList = ["poisson", "eau", "citrons"]
shoppingList[1] = "bouteille d'eau"
let tableauVide = [String]()

// Dictionnaire
var occupations = [
  "Malcolm": "Capitaine",
  "kaylee": "Mécanicien"
]
occupations["Jayne"] = "Secretaire"
let dicoVide = Dictionary<String, Float>()


//
// Contrôle et boucles
//

// Boucle for (tableau)
let monTableau = [1, 1, 2, 3, 5]
for value in monTableau {
  if value == 1 {
    println("Un!")
  } else {
    println("Pas un!")
  }
}

// Boucle for (dictionnaire)
for (key, value) in dict {
  println("\(key): \(value)")
}

// Boucle for (interval)
for i in -1...1 { // [-1, 0, 1]
  println(i)
}
// utilisez ..< pour exclure le dernier élement

// Boucle while
var i = 1
while i < 1000 {
  i *= 2
}

// Boucle do-while
do {
  println("bonjour")
} while 1 == 2

// Switch
let legume = "haricot"
switch legume {
case "haricot":
  // ...
case "concombre", "patate":
  // ...
default: // requis afin de couvrir toutes les possibilités
  // ...
}


//
// Fonctions
//

// Les fonctions sont de type primitif, ce qui veut dire qu'elles peuvent être incluses dans d'autres fonctions

// Fonction
func direBonjour(name: String, day: String) -> String {
  return "Bonjour \(name), on est \(day) aujourd'hui."
}
direBonjour("Bob", "mardi")

// Fonction qui retourne plusieurs valeurs dans un tuple
func getPrix() -> (Double, Double, Double) {
  return (3.59, 3.69, 3.79)
}

// Arguments
func setup(nombres: Int...) {}

// Passer et retourner des fonctions
func augmenter() -> (Int -> Int) {
  func ajouterUn(nombre: Int) -> Int {
    return 1 + nombre
  }
  return ajouterUn
}
var increment = augmenter()
increment(7)


//
// Closures
//
var nombres = [1, 2, 6]

// Les fonctions sont des cas de closures spéciales ({})

// Exemple de closure.
// `->` sépare les arguments et le type de retour
// `in` sépare l'en-tête de closure de son corps
nombres.map({
  (nombre: Int) -> Int in
  let resultat = 3 * nombre
  return resultat
  })

// Lorsque le type est connu, comme ci-dessus, on peut faire ceci
nombres = nombres.map({ nombre in 3 * nombre })
//Ou cela
//nombres = nombres.map({ $0 * 3 })

print(nombres) // [3, 6, 18]


//
// Classes
//

// Toutes les méthodes et propriétés d'une classe sont publiques.
// Si vous avez juste besoin de stocker des données dans un
// objet structuré, vous devez utiliser une structure

// Une classe `Square` hérite d'une classe `Shape`
class Rect: Shape {
  var longueurCote: Int = 1

  // Custom getter and setter property
  var perimeter: Int {
    get {
      return 4 * longueurCote
    }
    set {
      longueurCote = newValue / 4
    }
  }

  init(longueurCote: Int) {
    super.init()
    self.longueurCote = longueurCote
  }

  func shrink() {
    if longueurCote > 0 {
      --longueurCote
    }
  }

  override func getArea() -> Int {
    return longueurCote * longueurCote
  }
}
var monCarre = new Square(longueurCote: 5)
print(monCarre.getArea()) // 25
monCarre.shrink()
print(monCarre.longueurCote) // 4

// If you don't need a custom getter and setter,
// but still want to run code before and after getting or setting
// a property, you can use `willSet` and `didSet`


//
// Enumerations
//

// Les énumerations peuvent être d'un type spécifique ou non.
// Elles peuvent contenir méthodes et classes

enum Suit {
  case Pic, Coeur, Carre, Trefle
  func getIcon() -> String {
    switch self {
    case .Pic: return "♤"
    case .Coeur: return "♡"
    case .Carre: return "♢"
    case .Trefle: return "♧"
    }
  }
}


//
// Autres
//

// `protocol`: Similaire aux interfaces en Java
// `extension`s: Permet d'ajouter des fonctionnalités à un type existant
// Generics: Similaire à Java. Utilisez le mot clé `where` pour specifier les pré-requis du generic

```
