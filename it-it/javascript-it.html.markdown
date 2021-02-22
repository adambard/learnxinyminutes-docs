---
language: javascript
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
    - ["vinniec", "https://github.com/vinniec"]
filename: javascript-it.js
lang: it-it
---

JavaScript è stato creato da Netscape di Brendan Eich nel 1995. È stato originariamente pensato come un semplice linguaggio di scripting per i siti web, complementare all'uso di java per applicazioni più complesse ma la sua stretta integrazione con le pagine Web e il supporto integrato con esse ha causato il suo divenire più comune di Java per i frontend web.

Tuttavia JavaScript non è semplicemente limitato ai web browser: Node.js è un progetto che fornisce una runtime standalone dell'engine JavaScript V8 per Google Chrome, sta diventando sempre più popolare.

JavaScript ha una sintassi C-like, quindi se usate linguaggi come C o Java, molta della sintassi di base sarà già familiare. A dispetto di questo, e a dispetto del nome similare, il modello a oggetti di JavaScript è significativamente diverso da quello di Java.

```js
// I commenti a singola linea iniziano con due slash.
/* I commenti multilinea cominciano con uno slash e un asterisco,
   e terminano con un asterisco e uno slash */

// Le istruzioni possono essere terminate con ;
doStuff();

// ... ma non devono esserci per forza, i punti e virgola vengono automaticamente inseriti
// dove c'è un newline, ad eccezione di alcuni casi.
doStuff()

// Poiché questi casi possono causare risultati inaspettati, noi continueremo ad usare
// i punti e virgola in questa guida.

///////////////////////////////////
// 1. Numeri, Stringe e Operatori

// JavaScript ha un tipo numero (che è a 64-bit IEEE 754 double).
// Double ha una mantissa di 52-bit che è abbastanza per memorizzare interi
// fino a 9x10¹⁵ per essere precisi.
3; // = 3
1.5; // = 1.5

// Alcuni lavori aritmetici di base come ci si può aspettare.
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// inclusa la divisione con la virgola.
5 / 2; // = 2.5

// E il modulo.
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Anche le operazioni binarie funzionano; quando effettuate una operazione binaria il vostro numero decimale
// è convertito in un intero con segno *fino a* 32 bit..
1 << 2; // = 4

// Le precedenza è subordinata dalle parentesi.
(1 + 3) * 2; // = 8

// Ci sono tre valori speciali che non sono numeri reali:
Infinity; // ad esempio il risultato di 1/0
-Infinity; // ad esempio il risultato di -1/0
NaN; // ad esempio il risultato di 0/0, sta per 'Not a Number'

// Ci sono anche i tipi booleani.
true;
false;

// Le stringe sono create con ' oppure ".
'abc';
"Hello, world";

// La negazione usa il ! simbolo
!true; // = false
!false; // = true

// L'uguaglianza è ===
1 === 1; // = true
2 === 1; // = false

// L'inuguaglianza è !==
1 !== 1; // = false
2 !== 1; // = true

// Altre comparazioni
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Le stringhe si concatenano con il +
"Hello " + "world!"; // = "Hello world!"

// ... che funziona con qualcosa in più delle semplici stringhe
"1, 2, " + 3; // = "1, 2, 3"
"Hello " + ["world", "!"]; // = "Hello world,!"

// e sono comparate con < e >
"a" < "b"; // = true

// La comparazione con conversione implicita si fa con il doppio uguale...
"5" == 5; // = true
null == undefined; // = true

// ...ammenoché non si usi ===
"5" === 5; // = false
null === undefined; // = false

// ...che può provocare strani comportamenti...
13 + !0; // 14
"13" + !0; // '13true'

// Si può accedere ai caratteri di una stringa con  `charAt`
"This is a string".charAt(0);  // = 'T'

// ...o usando le `substring` per ottenere una parte.
"Hello world".substring(0, 5); // = "Hello"

// `length` è una proprietà, quindi non usate le ().
"Hello".length; // = 5

// Ci sono anche `null` e `undefined`.
null;      // usato per indicato deliberatamente un non-valore
undefined; // usato per indicare un valore che attualmente non è presente (sebbene
           // `undefined` sia un valore a sua stessa volta)

// false, null, undefined, NaN, 0 e "" sono falsi; tutto il resto è vero.
// Notare che 0 è falso e "0" è vero, nonostante 0 == "0".

///////////////////////////////////
// 2. Variabili, Array e Oggetti

// Le variabili sono dichiarate con la parola chiave `var`. JavaScript è tipato
// dinamicamente, quindi non serve specificare il tipo. L'assegnamento usa un carattere `=`
// singolo.
var someVar = 5;

// Se si toglie la parola chiave var non si otterrà un errore...
someOtherVar = 10;

// ...ma la tua variabile sarà creata con visibilità globale e non
// nel blocco dove la si è definita.

// Le variabili dichiarate senza essere definite vengono impostate come undefined.
var someThirdVar; // = undefined

// Se si vuole dichiarare una coppia di variabili, lo si può fare usando una virgola
// come separatore
var someFourthVar = 2, someFifthVar = 4;

// C'è una scorciatoia per effettuare operazioni matematiche sulle variabili:
someVar += 5; // equivalente di someVar = someVar + 5; someVar vale 10 ora
someVar *= 10; // ora someVar è 100

// e un ulteriore scorciatoia per aggiungere o sottrarre 1
someVar++; // ora someVar è 101
someVar--; // di nuovo 100

// Gli array sono liste ordinati di valori, di qualsiasi tipo.
var myArray = ["Hello", 45, true];

// Si può accedere ai loro membri usando la sintassi sottoscritta con le parentesi quadra.
// Gli indici degli array iniziano a zero.
myArray[1]; // = 45

// Gli Array sono mutabili e di dimensione variabile.
myArray.push("World");
myArray.length; // = 4

// Aggiungere/Modificare in un indice preciso
myArray[3] = "Hello";

// Aggiungere e rimovere un elemento dall'inizio o dalla fine di un array
myArray.unshift(3); // Aggiungere come primo elemento
someVar = myArray.shift(); // Rimuovere il primo elemento e restituirlo
myArray.push(3); // Aggiungere come ultimo elemento
someVar = myArray.pop(); // Rimuovere l'ultimo elemento e restituirlo

// Unire tutti gli elementi di un array con un punto e virgola
var myArray0 = [32,false,"js",12,56,90];
myArray0.join(";") // = "32;false;js;12;56;90"

// Ottenere un subarray di elementi dall'indice 1 (incluso) al 4 (escluso)
myArray0.slice(1,4); // = [false,"js",12]

// Rimuovere 4 elementi partendo dall'indice 2 e inserirci delle stringhe
// "hi","wr" e "ld"; restituiscono i subarray rimossi
myArray0.splice(2,4,"hi","wr","ld"); // = ["js",12,56,90]
// myArray0 === [32,false,"hi","wr","ld"]

// Gli oggetti di JavaScript sono equivalenti ai "dizionari" o "mappe" in altri
// linguaggi: una collezione non ordinata di coppie di chiave-valore.
var myObj = {key1: "Hello", key2: "World"};

// Le chiavi sono stringhe, ma non è necessario quotarle se sono identificatori
// JavaScript validi. I valori possono essere di ogni tipo.
var myObj = {myKey: "myValue", "my other key": 4};

// Gli attributi degli oggetti possono essere acceduti usando la sintassi "subscript",
myObj["my other key"]; // = 4

// ... o usando la notazione puntata fornendo una chiave che sia un identificatore valido.
myObj.myKey; // = "myValue"

// Gli oggetti sono mutabilil; i valori possono essere cambiati e nuove chiavi possono essere aggiunte.
myObj.myThirdKey = true;

// se si prova ad accedere ad un valore che non è stato ancora impostato, si otterrà undefined.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Strutture logiche e di controllo.

// La struttura `if` funziona come ci si aspetta.
var count = 1;
if (count == 3){
    // eseguito se count vale 3
} else if (count == 4){
    // eseguito se count vale 4
} else {
    // eseguito se count non è né 3 e né 4
}

// Così come il `while`.
while (true){
    // Un ciclo infinito!
}

// I cicli do-while sono come i cicli while ad eccezione che loro iterano almeno una volta.
var input;
do {
    input = getInput();
} while (!isValid(input));

// Il ciclo `for` è lo stesso di C e di Java:
// inizializzazione, condizione di proseguimento; iterazione.
for (var i = 0; i < 5; i++){
    // verrà eseguito 5 volte
}

// Uscire forzatamente da un un ciclo etichettato è simile a java:
outer:
for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j ==5) {
            break outer;
            // esce fuori dal ciclo outer invece che solo da quello più interno
        }
    }
}

// L'istruzione for/in permette l'iterazione sulle proprietà di un oggetto.
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    description += person[x] + " ";
} // description = 'Paul Ken 18 '

// L'istruzione for/of permette l'iterazione su oggetti iterabili (inclusi i built-in String,
// Array, es. gli argomenti Array-like o gli oggetti NodeList, TypedArray, Map e Set,
// e gli iterabili decisi dall'utente).
var myPets = "";
var pets = ["cat", "dog", "hamster", "hedgehog"];
for (var pet of pets){
    myPets += pet + " ";
} // myPets = 'cat dog hamster hedgehog '

// && è la congiunzione logica, || è la disgiunione logica
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // i colori sono sia rossi che blu
}

// && e || "short circuit", utili per impostare i valori di default.
var name = otherName || "default";

// L'istruzione `switch` controlla l'uguaglianza con `===`.
// Usare 'break' dopo ogni caso
// oppure i casi dopo quello corretto verranno eseguiti comunque.
grade = 'B';
switch (grade) {
  case 'A':
    console.log("Great job");
    break;
  case 'B':
    console.log("OK job");
    break;
  case 'C':
    console.log("You can do better");
    break;
  default:
    console.log("Oy vey");
    break;
}


///////////////////////////////////
// 4. Funzioni, Visibilità e Closure

// Le funzioni di JavaScript sono dichiarate con la parolachiave `function`.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// Nota che il valore da restituire deve iniziare nella stessa riga della
// keyword `return`, altrimenti verrà sempre restituito `undefined` per via due to
// dell'inserimento automatico dei punti e virgola. Fare attenzione a questo quando si usa lo stile Allman.
function myFunction(){
    return // <- punto e virgola automaticamente inserito qui
    {thisIsAn: 'object literal'};
}
myFunction(); // = undefined

// Le funzioni di JavaScript sono oggetti di prima classe, quindi possono essere riassegnate
// a diversi nomi di variabili e passate ad altre funzioni come argomenti - per esempio,
// mentre si fornisce un gestore di eventi:
function myFunction(){
    // questo codice sarà chiamato in 5 secondi
}
setTimeout(myFunction, 5000);
// Nota: setTimeout non è parte del linguaggio JS, ma è fornito dai browser
// e da Node.js.

// Un altra funzione fornita dai browser è setInterval
function myFunction(){
    // questo codice verrà chiamato ogni 5 secondi
}
setInterval(myFunction, 5000);

// Gli oggetti funzione non devono essere dichiarati con un nome - potete scrivere
// la definizione di una funzione anonima direttamente come argomento di un'altra.
setTimeout(function(){
    // questo codice sarà chiamato in 5 secondi
}, 5000);

// In JavaScript le funzioni hanno una propria visibilità; le funzioni hanno
// il loro scope ma gli altri blocchi no.
if (true){
    var i = 5;
}
i; // = 5 - non è undefined come ci si potrebbe aspettare in un linguaggio con una propria visibilità per blocco

// Questo ha portato ad un pattern comune di "esecuzione immediata di funzioni
// anonime", che previene alle variabili temporanee di finire nella
// visibilità globale.
(function(){
    var temporary = 5;
    // Noi possiamo accedere alla visibilità globale assegnando all' "oggetto globale", che
    // in un browser web è sempre `windows`. L'oggetto globale potrebbe avere
    // nomi differenti in ambienti diverso dal browser come Node.js.
    window.permanent = 10;
})();
temporary; // solleva ReferenceError
permanent; // = 10

// Una delle più potenti caratteristiche di javascript sono le closure. Se una funzione è
// definita dentro un'altra funzione, la funzione interna ha accesso a le variabili
// della funzione esterna, anche dopo essere uscita dalla funzione esterna.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Le funzioni interne sono messe nella visibilità locale in modo predefinito, anche se vengono
    // dichiarate con `var`.
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout è asincrono, quindi la funzione sayHelloInFiveSeconds
    // esce immediatamente e setTimeout chiamera inner successivamente. Tuttavia,
    // poiché inner è "chiuso prima" di sayHelloInFiveSeconds, inner ha ancora
    // accesso alla variabile `prompt` quando viene finalmente richiamato.
}
sayHelloInFiveSeconds("Adam"); // aprirà un popup con "Hello, Adam!" in 5s

///////////////////////////////////
// 5. Di più sugli oggetti, costruttori e prototipi.

// Gli oggetti possono contenere funzioni.
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

// Quando una funzione contenuta in un oggetto viene chiamata, essa può accedere a questo oggetto
// possono farlo usando la parola chiave `this`.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

// Questo ha a che fare con come la funzione è chiamata, non con dove
// è definita. Quindi, la nostra funzione non funziona se non è chiamata
// nel contesto dell'oggetto.
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// Al contrario, una funzione può essere assegnata ad un oggetto e poi accedere ad esso
// attraverso `this`, anche se non è stata inserita durante la definizione.
var myOtherFunc = function(){
    return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// Possiamo anche specificare un contesto per una funzione da eseguire quando la invochiamo
// usando `call` o `apply`.


var anotherFunc = function(s){
    return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// La funzione `apply` è quasi identica, ma prende un array come lista
// di argomenti.

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// Questo è utile quanso si lavora con una funzione che accetta una sequenza di
// argomenti e si vuole passare un array.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// Ma, `call` e `apply` sono solo temporanei. Quando vogliamo incollarli, possiamo
// usare `bind`

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` può essere anche usato per applicare parzialmente (curry) una funzione.

var product = function(a, b){ return a * b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// Quando si chiama una funzione con la parola chiave `new`, un nuovo oggetto viene creato
// e reso disponibile alla funzione attraverso la parola chiave `this`. Le funzioni progettate per essere
// invocate in questo modo sono chiamate costruttrici.

var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// Diversamente dalla molti degli altri linguaggi orientati agli oggetti, Javascript non ha
// il concetto di 'istanze' create sull'impronta di una 'classe'; invece Javascript
// combina l'instanziamento e l'ereditarietà in un singolo concetto: il 'prototipo'.

// Ogni oggetto Javascript ha un 'prototipo'. Quando si cerca di accedere a una proprietà
// su un oggetto che non la contiene, l'interprete
// guarderà i suoi prototipi.

// Alcune implementazioni di JS faranno accedere al propotipo di un oggetto con la proprietà
// magica `__proto__`: Anche se questo è utile per spiegare i prototipi, non è
// parte dello standard; capiremo più avanti come usare i prototipi in modo standard.
var myObj = {
    myString: "Hello world!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase();
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// Questo funziona anche per le funzioni.
myObj.myFunc(); // = "hello world!"

// Ovviamente, se la proprietà non è nel prototipo, il prototipo
// del prototipo viene ricercato, e così via.
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// Non c'è nessuna copia coinvolta qui; ogni oggetto mantiene una referenza al suo
// prototipo. Questo significa che possiamo modificare il prototipo e i nostri cambiamenti
// si rifletteranno da ogni parte.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// L'istruzione for/in permette di iterare sulle proprietà di un oggetto,
// risalendo la catena dei prototipi finché non trova un prototipo null.
for (var x in myObj){
    console.log(myObj[x]);
}
///stampa:
// Hello world!
// 43
// [Function: myFunc]
// true 

// Per considerare solamente le proprietà inserite nell'oggetto stesso
// e non i loro prototipi, usare il check `hasOwnProperty()`.
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
///stampa:
// Hello world!

// Abbiamo menzionato che `__proto__` non è standard, e non c'è nessun modo standard per
// cambiare il prototipo di un oggetto esistente. Tuttavia, ci sono due strade per
// creare un nuovo oggetto con un dato prototipo.

// La prima è Object.create, che è una recente aggiunta a JS, e che quindi
// non è disponibile ancora in tutte le implementazioni.
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// La seconda strada, che funziona ovunque, ha a che fare con i costruttori.
// I costruttori hanno una proprietà chiamata prototype. Questo *non* è il prototipo del
// costruttore della stessa funzione; invece è il prototipo del nuovo oggetto
// che gli viene conferito alla creazione con quel costruttore e la parola chiave new.
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function(){
        return this.myNumber;
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6;
myNewObj2.getMyNumber(); // = 6

// I tipi built-in come stringhe e numeri hanno anche costruttori che creano
// oggetti wrapper equivalenti.
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// Eccezione, loro non sono esattamente equivalenti.
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // Questo codice non verrà eseguito perchè 0 è falso.
}
if (new Number(0)){
   // Questo codice verrà eseguito poiché i numeri wrappati sono oggetti e gli oggetti
   // sono sempre veri.
}

// Tuttavia, gli oggetti wrapper e i regolari built-in condividono un prototipo, quindi
// si possono aggiungere funzionalità ad una stringa, per esempio.
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// Questa caratteristica viene spesso usata nel "polyfilling", che implementa nuovi
// aspetti in un vecchio sottoinsieme di JavaScript, in modo che si possano
// usare in vecchi ambienti come browser non aggiornati.

// Per esempio, abbiamo menzionato che Object.create non è disponibile in tutte le
// implementazioni, ma possiamo ancora usarlo con questo polyfill:
if (Object.create === undefined){ // non lo sovrascrive se esiste già
    Object.create = function(proto){
        // crea un costruttore temporaneo con il giusto prototipo
        var Constructor = function(){};
        Constructor.prototype = proto;
        // quindi lo usa per creare un nuovo, propriamente-prototipato oggetto
        return new Constructor();
    };
}
```

## Approfondimenti

Il [Mozilla Developer Networ][1] fornisce una documentazione eccellente su come Javascript è utilizzato nei browsers. In più è un wiki, quindi si può imparare di più aiutando gli altri condividendo la propria conoscenza.

MDN's [A re-introduction to JavaScript][2] copre molti dei concetti qui trattati in maggiore dettaglio. Questa guida ha deliberatamente coperto solamente il linguaggio JavaScript; se volete sapere di più su come usare JavaScript in una pagina web, iniziate leggendo il [Document Object Model][3].

[Learn Javascript by Example and with Challenges][4] è una variante di questo referenziario con integrate delle sfide.

[Javascript Garden][5] è una guida approfondita di tutte le parti controintuitive del linguaggio.

[JavaScript: The Definitive Guide][6] è una guida classica e referenziario.

[Eloqunt Javascript][8] di Marijn Haverbeke è un ottimo libro/ebook JS con terminale annesso

[Javascript: The Right Way][10] è una guida dedicata all'introduzione dei nuovi sviluppatori a JavaScript e come aiuto agli sviluppatori esperti per imparare di più sulle best practice.

[Javascript:info][11] è un moderno tutorial su javascript che copre le basi (linguaggio principale e lavorazione con un browser) come anche argomenti avanzati con spiegazioni concise.


In aggiunta ai contributori di questo articolo, alcuni contenuti sono adattati dal Louie Dinh's Python tutorial su questo sito, e da [JS Tutorial][7] sul Mozilla Developer Network.


[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[3]: https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core
[4]: http://www.learneroo.com/modules/64/nodes/350
[5]: http://bonsaiden.github.io/JavaScript-Garden/
[6]: http://www.amazon.com/gp/product/0596805527/
[7]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[8]: http://eloquentjavascript.net/
[10]: http://jstherightway.org/
[11]: https://javascript.info/
