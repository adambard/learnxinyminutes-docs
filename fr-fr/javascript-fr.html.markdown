---
language: javascript
contributors:
    - ['Adam Brenecki', 'http://adam.brenecki.id.au']
    - ['Ariel Krakowski', 'http://www.learneroo.com']
filename: javascript-fr.js
translators:
    - ['@nbrugneaux', 'https://nicolasbrugneaux.me']
    - ['Michel Antoine', 'https://github.com/antoin-m']
lang: fr-fr
---

JavaScript a été créé par Brendan Eich, travaillant alors a Netscape, en 1995.
Le langage avait à l'origine pour but d'être un langage de scripting simple
pour les sites web, complétant le Java (à ne pas confondre avec JavaScript)
pour des applications web complexes. Mais son intégration très proche et
simple des pages web, ainsi que le support natif des navigateurs a rendu
le JavaScript incontournable aujourd'hui tant bien dans le front-end que
dans le back-end.

En effet, le JavaScript n'est plus uniquement limité aux navigateurs, grâce à
Node.JS, un projet qui offre un environnement indépendant dans lequel un
interpréteur Javascript, basé sur le célèbre moteur V8 de Google Chrome,
peut être utilisé directement côté serveur pour exécuter des programmes écrits
en JavaScript.

ECMAScript (la norme du langage Javascript) entre en version 6. Cette version introduit de nombreuses mises à jour tout en restant rétrocompatible. L'implémentation de ces nouvelles fonctionnalités est en cours et celles-ci ne sont donc pas forcément compatibles avec tous les navigateurs.

```js
// Les commentaires sont comme en C. Les commentaires mono-ligne commencent par 2 slashs,
/* et les commentaires sur plusieurs lignes commencent avec slash-étoile
   et finissent avec étoile-slash */

// Toutes les expressions peuvent finir par ;
doStuff();

// ... mais n'en n'ont pas forcément besoin, les point-virgules sont ajoutés
// lors de l’interprétation aux sauts de ligne, sauf exceptions
doStuff()

// Parce que ces cas peuvent produire des effets inattendus, nous utiliserons
// des point-virgules dans ce guide.


///////////////////////////////////
// 1. Nombres, Chaines de caractères et Opérateurs

// JavaScript a un seul type de nombre (qui est un 64-bit IEEE 754 double (décimaux))
// Comme avec le Lua, ne paniquez pas à cause du manque d'int (entiers) : les
// doubles ont un mantisse de 52 bits, ce qui est assez pour stocker des int jusqu'à
// 9 x 10¹⁵ exactement.
3; // = 3
1.5; // = 1.5

// L'arithmétique de base fonctionne comme vous vous y attendriez
1 + 1; // = 2
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Ainsi que les divisions non-entières
5 / 2; // = 2.5

// Les opérations bits à bits fonctionnent aussi, quand vous effectuez une opération
// bits à bits, votre nombre décimal est converti en entier *jusqu'à* 32 bits.
1 << 2; // = 4

// Comme en mathématiques, la priorité est donnée aux parenthèses.
(1 + 3) * 2; // = 8

// Il existe 3 valeurs spéciales pour les nombres:
Infinity; // le résultat de 1/0 par exemple
-Infinity; // le résultat de -1/0 par exemple
NaN; // le résultat de 0/0 par exemple

// Il existe également le type booléen.
true; // vrai
false; // faux

// Les chaines de caractères (strings) sont créees avec " ou ' indifféremment, la seule
// raison de choisir l'un ou l'autre est la cohérence dans votre code.
"abc";
'Hello, world';

// *ES6:* Les chaines de caractères peuvent être crées en utilisant un modèle
// entouré des quotes inverses (`) à la place des quotes classiques (' ou ").
// Les variables sont interprétées avec ${var}
let banta = "Harry", santa = "Hermione";
`${banta}, your santa is ${santa}.` // = "Harry, your santa is Hermione."

// La négation utilise le symbole !
!true; // = false
!false; // = true

// L'égalité est === ou ==
// === compare la valeur exacte 2 === '2' // = false
// == convertit la valeur pour comparer 2 === '2' // = true
// En général, il vaut mieux utiliser === pour ne pas faire d'erreur.
1 === 1; // = true
2 === 1; // = false

// L'inégalité est !== ou !=, basé sur le même principe qu'avant.
1 !== 1; // = false
2 !== 1; // = true

// Plus de comparaisons :
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Les chaines de caractères se concatènent avec +
'Hello ' + 'world!'; // = 'Hello world!'

// et peuvent être comparées alphabétiquement avec < et >
'a' < 'b'; // = true

// Vous pouvez accéder les caractères dans une string avec charAt
'This is a string'.charAt(0);  // = 'T'

// ... ou utiliser substring pour avoir un plus gros morceau
'Hello world'.substring(0, 5); // = 'Hello'

// la longueur, length, est une propriété, donc n'utilisez pas de ()
'Hello'.length; // = 5

// Il y a également null et undefined
null; // utilisé pour une non-valeur
undefined; // utilisé pour une valeur actuellement non présente (cependant,
           // undefined est aussi une valeur valide)

// false, null, undefined, NaN, 0 and '' sont 'presque-faux' (falsy), tout le reste
// est 'presque-vrai' (truthy)
// Notez que 0 est falsy mais '0' est truthy, alors même que 0 == '0' (mais 0 !== '0')

// *ES6:* Introduction d'un nouveau type primitif : Symbol
var symbol_one = Symbol();
var symbol_two = Symbol('This is optional description, for debugging');
typeof symbol_one === 'symbol' // = true

// *ES6:* Un Symbol est immutable et unique
Symbol() === Symbol() // = false
Symbol('learnx') === Symbol('learnx') // = false

///////////////////////////////////
// 2. Variables, Tableaux, Objets, Maps et Sets

// Les variables sont déclarées avec le mot clé var. Le typage en JavaScript est
// dynamique, donc pas besoin de spécifier le type. L'assignement utilise un seul =.
var someVar = 5;

// si vous oubliez le mot clé var, vous n'aurez pas d'erreur (sauf en mode strict)
someOtherVar = 10;

// ... mais la variable aura une portée globale (plus communément trouvé en tant
// que "global scope" en anglais), et non pas une portée limitée à la fonction
// dans laquelle vous l'aviez définie.

// Les variables déclarées et non assignées sont undefined par défaut
var someThirdVar;
var someThirdVar = undefined;

// ... sont deux déclarations identiques.

// Il est possible de déclarer plusieurs variables en séparant leur déclaration
// avec l'opérateur virgule
var someFourthVar = 2, someFifthVar = 4;

// *ES6:* Les variables peuvent maintenant être déclarées avec les mots-clés
// `let` et `const`
let someSixthVar = 6;
const someSeventhVar = 7;

// *ES6:* Le mot-clé `let` attache la variable au block de code et non à la fonction
// à l'inverse de `var`
for (let i = 0; i < 10; i++) {
    x += 10;
}
i; // = raises ReferenceError

// *ES6:* Les variables "const" doivent être assignées lors de l'initialisation
const someEighthVar = 7;
const someNinthVar; // raises SyntaxError

// *ES6:* Modifier une variable constante ne lève par d'erreur mais échoue
// silencieusement
const someNinthVar = 9;
someNinthVar = 10;
someNinthVar; // = 9

// Il y a des raccourcis pour les opérations mathématiques:
someVar += 5; // équivalent pour someVar = someVar + 5;
someVar *= 10; // de même, someVar = someVar * 10;
someVar++;    // = someVar += 1;
someVar--;  // = someVar -= 1;

// Les tableaux (Arrays) sont des listes ordonnées de valeurs, de tous types.
var myArray = ['Hello', 45, true];

// Leurs membres peuvent être accédés en utilisant les crochets
// Les indices commencent à 0.
myArray[1]; // = 45

// Les tableaux sont modifiables, ainsi que leurs longueurs
myArray.push( 'World' );
myArray.length; // = 4

// Ajout/Modification à un index spécifique
myArray[3] = 'Hello';

// *ES6:* Les Arrays peuvent maintenant être déstructurés en utilisant le pattern matching
var [a, b] = [1, 2];
var [a, , b] = [1, -2, 2]

a; // = 1
b; // = 2

// *ES6:* La déstructuration peut échouer silencieusement.
// Il est aussi possible d'utiliser des valeurs par défaut
var [a] = [];
a; // = undefined;
var [a = 1] = [];
a; // = 1;
var [a = 1] = [2];
a; // = 2;

// Les objets JavaScript sont appelés 'dictionnaires' ou 'maps' dans certains autres
// langages : ils sont une liste non-ordonnée de paires clé-valeur.
var myObj = {key1: 'Hello', key2: 'World'};

// Les clés sont des strings, mais les ' ou " sont optionels si elles sont des
// noms valides en JavaScript. Les valeurs peuvent être de n'importe quel type.
var myObj = {myKey: 'myValue', 'my other key': 4};

// Les attributs d'objets peuvent être accédés avec les crochets
myObj['my other key']; // = 4

// .. ou avec un point si la clé est un identifiant valide.
myObj.myKey; // = 'myValue'

// *ES6:* Un Symbol peut être utilisé en tant que clé. Puisque ceux-ci sont uniques,
// le seul moyen d'accéder à la propriété est d'avoir une référence sur ce Symbol.
myObj["key"] = "public value";
myObj[Symbol("key")] = "secret value";
myObj[Symbol("key")]; // = undefined

// Les objets sont eux aussi modifiables.
myObj.myThirdKey = true;

// Si vous essayez d'accéder à une valeur non-définie, vous obtiendrez undefined
myObj.myFourthKey; // = undefined

// *ES6:* Comme les Arrays, les Objects peuvent être déstructurés en utilisant le pattern matching
var {foo} = {foo: "bar"};
foo // = "bar"

// *ES6:* Les Objects déstructurés peuvent utiliser des noms de variables différents
// de ceux d'origine grâce au pattern matching
var {foo, moo: baz} = {foo: "bar", moo: "car"};
foo // = "bar"
baz // = "car"

// *ES6:* Il est possible d'utiliser des valeurs par défaut lor de la déstructuration d'un Object
var {foo="bar"} = {moo: "car"};
foo // = "bar"

// *ES6:* Une erreur lors de la déstructuration restera silencieuse
var {foo} = {};
foo // = undefined

// *ES6:* Les Maps sont des objets itérables de type clé-valeur.
// Il est possible de créer une nouvelle map en utilisant `new Map()`
var myMap = new Map();

// *ES6:* Il est possible d'ajouter un couple clé-valeur avec la méthode `.set()`,
// de récupérer une valeur avec `.get()`,
// de vérifier qu'une clé existe avec `.has()`
// et enfin de supprimer un couple clé-valeur avec `.delete()`

myMap.set("name", "Douglas");
myMap.get("name"); // = "Douglas"
myMap.has("name"); // = true
myMap.delete("name");

// *ES6:* Les Sets sont des ensembles de valeurs uniques.
// Il est possible de créer un set avec `new Set()`.
// Toute valeur non unique est ignorée.
var mySet = new Set([1,2,2]);
console.log([...mySet]); // = [1,2]

///////////////////////////////////
// 3. Logique et structures de contrôle

// Les si (if) fonctionnent comme vous vous y attendez.
var count = 1;
if (count === 3) {
  // seulement quand count est 3
}
else if (count === 4) {
  // uniquement quand count est 4
}
else {
  // le reste du temps, si ni 3, ni 4.
}

// De même pour while.
while (true) {
  // Une boucle infinie !
}

// Les boucles do-while sont pareilles, mais sont exécutées au moins une fois.
var input
do {
  input = getInput();
} while (!isValid(input))

// La boucle for est la même qu'en C ou en Java:
// initialisation; condition pour continuer; itération
for (var i = 0; i < 5; i++){
    // sera exécutée 5 fois
}

// La boucle for...in permet d'itérer sur les noms des propriétés d'un objet
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    description += person[x] + " ";
}
description; // = "Paul Ken 18 "

// *ES6:* La boucle for...of permet d'itérer sur les propriétés d'un objet
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x of person){
    description += x + " ";
}
description; // = "Paul Ken 18 "

// && est le "et" logique, || est le "ou" logique
if (house.size === 'big' && house.colour === 'blue'){
    house.contains = 'bear';
}
if (colour === 'red' || colour === 'blue'){
    // colour est soit 'red' soit 'blue'
}

// Les raccourcis && et || sont pratiques pour instancier des valeurs par defaut.
var name = otherName || 'default';

// Ceci est l'équivalent de
var name = otherName;
if (!name){
  name = 'default';
}

// Le switch vérifie les égalités avec ===
// utilisez un "break" à la fin de chaque cas
// ou les cas suivants seront eux aussi exécutés
grade = 'B';
switch (grade) {
  case 'A':
    console.log('Great job');
    break;
  case 'B':
    console.log('OK job');
    break;
  case 'C':
    console.log('You can do better');
    break;
  default:
    console.log('Oy vey');
    break;
}


///////////////////////////////////
// 4. Fonctions, Scope (Environnement) et Closures

// Les fonctions sont déclarées avec le mot clé function
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction('foo'); // = 'FOO'

// Attention, la valeur à retourner doit se trouver sur la même ligne que
// le mot-clé `return` sinon la fonction retournera systématiquement `undefined`
function myFunction(){
    return // <- semicolon automatically inserted here
    {thisIsAn: 'object literal'}
}
myFunction(); // = undefined

// *ES6:* Les paramètres des fonctions peuvent désormais avoir des valeurs par défaut
function default(x, y = 2) {
    return x + y;
}
default(10); // == 12

// Les fonctions JavaScript sont des objets de première classe, donc peuvent
// être réassignées à d'autres variables et passées en tant que paramètres pour
// d'autres fonctions
function myFunction(){
    // ce code s'exécutera dans 5 secondes
}
setTimeout(myFunction, 5000);
// Note: setTimeout ne fait pas parti du langage, mais les navigateurs ainsi
// que Node.js le rendent disponible

// Les fonctions n'ont pas nécessairement besoin d'un nom, elles peuvent être
// anonymes
setTimeout(function(){
    // ce code s'exécutera dans 5 secondes
}, 5000);

// *ES6:* Introduction d'un sucre syntaxique permettant de créer
// une fonction anonyme de la forme : `param => returnValue`.
setTimeout(() => console.log('5 seconds, are up.'), 5000);

// Le Javascript crée uniquement un scope, une portée d'action limitée, pour
// les fonctions, et pas dans les autres blocs.
if (true){
    var i = 5;
}
i; // = 5 - et non undefined comme vous pourriez vous y attendre

// Cela a mené à un style commun de fonctions anonymes immédiatement exécutée;
// ou "immediately-executing anonymous functions"
(function(){
    var temporary = 5;
    // Nous pouvons accéder au scope global en assignant à l'objet global,
    // qui dans les navigateurs est "window". Il est différent dans Node.js,
    // le scope global sera en fait local au module dans lequel vous
    // vous trouvez. http://nodejs.org/api/globals.html
    window.permanent = 10;
})();
// Cela permet de ne pas avoir de fuites de variables qui polluent
// l’environnement global.
temporary; // raises ReferenceError
permanent; // = 10

// Une des fonctionnalités les plus puissantes de Javascript est le système de
// closures. Si une fonction est définie dans une autre fonction, alors la
// fonction interne aura accès aux variables de la fonction parente, même si
// celle-ci a déjà finie son exécution.
function sayHelloInFiveSeconds(name){
    var prompt = 'Hello, ' + name + '!';
    // Fonction interne
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout is asynchrone, donc la fonction sayHelloInFiveSeconds quittera
    // immédiatement, et setTimeout appelera inner après.
}
sayHelloInFiveSeconds('Adam'); // ouvre un popup avec 'Hello, Adam!' dans 5sec

// *ES6:* Les paramètres des fonctions appelées avec un tableau en entré
// préfixé par `...` vont se peupler avec les éléments du tableau
function spread(x, y, z) {
    return x + y + z;
}
spread(...[1,2,3]); // == 6

// *ES6:* Les fonctions peuvent recevoir les paramètres dans un tableau en utilisant l'opérateur `...`
function spread(x, y, z) {
    return x + y + z;
}
spread(...[1,2,3]); // == 6

///////////////////////////////////
// 5. Encore plus à propos des Objets; Constructeurs and Prototypes

// Les objets peuvent contenir des fonctions.
var myObj = {
    myFunc: function(){
        return 'Hello world!';
    }
};
myObj.myFunc(); // = 'Hello world!'

// Lorsqu'une fonction attachée à un objet est appelée, elle peut accéder à
// l'objet avec le mot clé this.
myObj = {
    myString: 'Hello world!',
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = 'Hello world!'

// La valeur de "this" change de par l'endroit où la fonction est appelée, et
// non de l'endroit où elle est définie. Donc elle ne fonctionnera pas si elle
// est appelée hors du contexte l'objet.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// A l'inverse, une fonction peut être attachée à un objet et y gagner l'accès
// au travers de "this"
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}

myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = 'HELLO WORLD!'

// Le contexte correspond à la valeur de "this".
// Nous pouvons aussi spécifier un contexte, forcer la valeur de "this,
// pour une fonction quand elle est appelée grâce à "call" ou "apply".
var anotherFunc = function(s){
    return this.myString + s;
}
anotherFunc.call(myObj, ' And Hello Moon!'); // = 'Hello World! And Hello Moon!'

// 'apply' est presque identique, mais prend un tableau comme liste d’arguments.

anotherFunc.apply(myObj, [' And Hello Sun!']); // = 'Hello World! And Hello Sun!'

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Mais, "call" and "apply" fonctionnenent uniquement au moment de l'appel de la
// fonction. Pour lier le contexte de façon permanente, nous pouvons utiliser
// "bind" pour garder une référence à la fonction avec ce "this".
var boundFunc = anotherFunc.bind(myObj);
boundFunc(' And Hello Saturn!'); // = 'Hello World! And Hello Saturn!'

// "bind" peut aussi être utilisé pour créer une application partielle de la
// fonction (curry)
var product = function(a, b){ return a * b; }
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Lorsque vous appelez une fonction avec le mot clé "new", un nouvel objet est
// crée et mis à disposition de la fonction via "this". Ces fonctions sont
// communément appelées constructeurs.
var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Chaque objet en Javascript a un "prototype". Quand vous essayez d'accéder à
// une propriété que l'objet n'a pas, l'interpréteur va regarder son prototype.

// Quelques implémentations de JS vous laissent accéder au prototype avec la
// propriété "magique" __proto__. Ceci peut être utile, mais n'est pas standard
// et ne fonctionne pas dans certains des navigateurs actuels.
var myObj = {
    myString: 'Hello world!'
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42
myObj.myFunc(); // = 'hello world!'

myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true


// Pour obtenir le prototype il existe également Object.getPrototypeOf
Object.getPrototypeOf( myObj ) // = {meaningOfLife: 42, myFunc: function}

// Il n'y a pas de copie ici. Chacun des objets stocke une référence à son
// prototype. Cela veut dire que l'on peut le modifier et cela se répercutera
// partout.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// L'inverse n'est cependant pas vrai. Changer la propriété d'un objet ne change
// pas la chaine prototypale.
myObj.meaningOfLife = 42;
myPrototype.meaningOfLife; // = 43

// Comme précédemment dit, __proto__ n'est pas standard et ne devrait pas être
// utilisé. Il y a deux autres moyen de créer un nouvel objet avec un prototype
// donné.

// Le premier est Object.create, mais c'est assez récent et risque de ne pas
// fonctionner dans tous les navigateurs.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// Le deuxième moyen, qui marche partout, fonctionne avec les constructeurs.
// Les constructeurs ont un propriété appelée prototype. Ce n'est *pas* le
// prototype du constructeur de la fonction elle-même, c'est le prototype que
// les nouveaux objets crées grâce à ce constructeur avec "new" auront.
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function(){
        return this.myNumber;
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6
myNewObj2.getMyNumber(); // = 6

// Les types pré-définis tels que les strings ou nombres ont aussi des
// constructeurs
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// ... mais ils ne sont pas exactement équivalent.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // 0 est falsy, le code ne fonctionnera pas.
}

// Cependant, vous pouvez ajouter des fonctionnalités aux types de bases grâce à
// cette particularité.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
'abc'.firstCharacter(); // = 'a'

// C'est très souvent utilisé pour le "polyfilling", qui implémente des nouvelles
// fonctionnalités de JavaScript dans de plus anciens environnements, tels que
// les vieux navigateurs.

//Par exemple, Object.create est assez récent, mais peut être implémenté grâce à
// ce polyfill
if (Object.create === undefined){ // pour ne pas reécrire si la fonction existe déjà
    Object.create = function(proto){
        // on crée un constructeur temporaire avec le bon prototype
        var Constructor = function(){};
        Constructor.prototype = proto;
        // puis on utilise "new" pour créer un object avec ce même prototype
        return new Constructor();
    }
}

// *ES6:* Les objets peuvent être équipés de proxies qui permettent d'intercepter
// les actions sur leurs propriétés. Voici comment créer un proxy sur un objet :
var proxyObject = new Proxy(object, handler);

// *ES6:* Les méthodes d'un objet handler sont appelées lors de l'interception d'une action.
// La méthode `.get()` est appelée à chaque lecture d'une propriété
// tandis que la méthode `.set()` est appelée à chaque écriture.
var handler = {
  get (target, key) {
    console.info('Get on property' + key);
    return target[key];
  },
  set (target, key, value) {
    console.info('Set on property' + key);
    return true;
  }
}

// *ES6:* Les classes peuvent désormais être définies en utilisant le mot-clé `class`.
// Le constructeur s'appelle `constructor` et les méthodes statiques utilisent le mot-clé `static`
class Foo {
    constructor() {console.log("constructing Foo");}
    bar() {return "bar";}
    static baz() {return "baz";}
}

// *ES6:* Les objets issus des classes sont initialisés avec le mot-clé `new`.
// Il est possible d'hériter d'une classe avec le mot-clé `extends`
var FooObject = new Foo(); // = "constructing Foo"
class Zoo extends Foo {}

// *ES6:* Les méthodes statiques doivent être appelées par la classe, les autres méthodes par l'objet
Foo.baz() // = "baz"
FooObject.bar() // = "bar"

// *ES6:* Il est désormais possible d'exporter des valeurs en tant que module.
// Les exports peuvent être n'importe quel objet, valeur ou fonction.
var api = {
  foo: "bar",
  baz: "ponyfoo"
};
export default api;

// *ES6:* La syntaxe `export default` permet d'exporter l'objet sans en changer le nom.
// Il y a plusieurs façons de l'importer:
import coolapi from "api"; // = importe le module dans la variable `coolapi`
import {foo, baz} from "api"; // = importe les attributs `foo` et `baz` du module
import {foo as moo, baz} from "api"; // = importe les attributs `foo` (en le renommant `moo`) et `baz` du module
import _, {map} from "api"; // = importe les exports par défaut ET `map`
import * as coolapi from "api"; // = importe le namespace global du module

```

## Pour aller plus loin (en anglais)

The [Mozilla Developer
Network](https://developer.mozilla.org/fr-FR/docs/Web/JavaScript) expose une
excellente documentation pour le Javascript dans les navigateurs. Et contient
également un wiki pour s'entraider.

MDN's [A re-introduction to
JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
recouvre les principaux sujets vus ici. Le guide est délibérément uniquement
à propos du JavaScript, et ne parle pas des navigateurs; pour cela, dirigez vous
plutôt ici :
[Document Object
Model](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Learn Javascript by Example and with Challenges](http://www.learneroo.com/modules/64/nodes/350) quelques challenges.

[JavaScript Garden](http://bonsaiden.github.io/JavaScript-Garden/) is an in-depth
un guide pour vous éviter les faux-amis dans le JavaScript.

[JavaScript: The Definitive Guide](http://www.amazon.com/gp/product/0596805527/) un classique. A lire.

En addition aux contributeurs de cet article, du contenu provient du
"Python tutorial" de Louie Dinh, et de [JS
Tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
sur le réseau Mozilla.
