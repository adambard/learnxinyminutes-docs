---
language: javascript
contributors:
    - ["Adam Brenecki", "http://adam.brenecki.id.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
    - ["Michal Martinek", "https://github.com/MichalMartinek"]
lang: cs-cz
filename: javascript-cz.js
---

JavaScript byl vytvořen Brendan Eichem v roce 1995 pro Netscape. Byl původně
zamýšlen jako jednoduchý skriptovací jazyk pro webové stránky, jako doplněk Javy,
která byla zamýšlena pro více komplexní webové aplikace, ale jeho úzké propojení
s webovými stránkami a vestavěná podpora v prohlížečích způsobila, že se stala
více běžná ve webovém frontendu než Java.


JavaScript není omezen pouze na webové prohlížeče, např. projekt Node.js,
který zprostředkovává samostatně běžící prostředí V8 JavaScriptového enginu z
Google Chrome se stává více a více oblíbený pro serverovou část webových aplikací.

Zpětná vazba je velmi ceněná. Autora článku můžete kontaktovat (anglicky) na
[@adambrenecki](https://twitter.com/adambrenecki), nebo
[adam@brenecki.id.au](mailto:adam@brenecki.id.au), nebo mě, jakožto překladatele,
na [martinek@ludis.me](mailto:martinek@ludis.me).

```js
// Komentáře jsou jako v zayku C. Jednořádkové komentáře začínájí dvojitým lomítkem,
/* a víceřádkové komentáře začínají lomítkem s hvězdičkou
   a končí hvězdičkou s lomítkem */

// Vyrazu můžou být spuštěny pomocí ;
delejNeco();

// ... ale nemusí, středníky jsou automaticky vloženy kdekoliv,
// kde končí řádka, kromě pár speciálních případů
delejNeco()

// Protože tyto případy můžou způsobit neočekávané výsledky, budeme
// středníky v našem návodu používat.

/////////////////////////////////
// 1. Čísla, řetězce a operátory

// JavaScript má jeden číselný typ (čímž je 64-bitový IEEE 754 double).
// Double má 52-bit přesnost, což je dostatečně přesné pro ukládání celých čísel
// do 9✕10¹⁵.
3; // = 3
1.5; // = 1.5

// Základní matematické operace fungují, jak byste očekávali
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// Včetně dělení
5 / 2; // = 2.5

// A také dělení modulo
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// Bitové operace také fungují; když provádíte bitové operace, desetinné číslo  
// (float) se převede na celé číslo (int) se znaménkem *do* 32 bitů
1 << 2; // = 4

// Přednost se vynucuje závorkami.
(1 + 3) * 2; // = 8

// Existují 3 hodnoty mimo obor reálných čísel
Infinity; // + nekonečno; výsledek např. 1/0
-Infinity; // - nekonečno; výsledek např. -1/0
NaN; // výsledek např. 0/0, znamená, že výsledek není číslo ('Not a Number')

// Také existují hodnoty typu bool
true; // pravda
false; // nepravda

// Řetězce znaků jsou obaleny  ' nebo ".
'abc';
"Ahoj světe!";

// Negace se tvoří pomocí !
!true; // = false
!false; // = true

// Rovnost se porovnává ===
1 === 1; // = true
2 === 1; // = false

// Nerovnost zase pomocí !==
1 !== 1; // = false
2 !== 1; // = true

// Další srovnávání
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// Řetězce znaků se spojují pomocí +
"Ahoj " + "světe!"; // = "Ahoj světe!"

// ... což funguje nejenom s řetězci
"1, 2, " + 3; // = "1, 2, 3"
"Ahoj " + ["světe", "!"] // = "Ahoj světe,!"

// a porovnávají se pomocí < nebo >
"a" < "b"; // = true

// Rovnost s převodem typů se dělá pomocí == ...
"5" == 5; // = true
null == undefined; // = true

// ...dokud nepoužijete ===
"5" === 5; // = false
null === undefined; // = false

// ...což může občas způsobit divné chování...
13 + !0; // 14
"13" + !0; // '13true'

// Můžeme přistupovat k jednotlivým znakům v řetězci pomocí charAt`
"Toto je řetězec".charAt(0);  // = 'T'

// ...nebo použít `substring` k získání podřetězce
"Ahoj světe".substring(0, 4); // = "Ahoj"

// `length` znamená délka a je to vlastnost, takže nepoužívejte ()
"Ahoj".length; // = 4

// Existují také typy `null` a `undefined`.
null;      // značí, že žádnou hodnotu
undefined; // značí, že hodnota nebyla definovaná (ikdyž
           // `undefined` je hodnota sama o sobě)

// false, null, undefined, NaN, 0 and "" vrací nepravdu (false). Všechno ostatní
// vrací pravdu (true)..
// Všimněte si, že 0 vrací nepravdu, ale "0" vrací pravdu, ikdyž 0 == "0"
// vrací pravdu

///////////////////////////////////
// 2. Proměnné, pole a objekty

// Proměnné jsou deklarovány pomocí slůvka `var`. JavaScript je dynamicky
// typovaný, takže nemusíme specifikovat typ. K přiřazení hodnoty se používá
// znak `=`.
var promenna = 5;

// když vynecháte slůvko 'var' nedostanete chybovou hlášku...
jinaPromenna = 10;

// ...ale vaše proměnná bude vytvořena globálně, bude vytvořena  v globálním
// oblasti působnosti, ne jenom v lokálním tam, kde jste ji vytvořili

// Proměnné vytvořené bez přiřazení obsahují hodnotu undefined.
var dalsiPromenna; // = undefined

// Pokud chcete vytvořit několik proměnných najednou, můžete je oddělit čárkou
var someFourthVar = 2, someFifthVar = 4;

// Existuje kratší forma pro matematické operace na proměnné
promenna += 5; // se provede stejně jako promenna = promenna + 5;
// promenna je ted 10
promenna *= 10; // teď je promenna rovna 100

// a tohle je způsob, jak přičítat a odečítat 1
promenna++; // teď je promenna 101
promenna--; // zpět na 100

// Pole jsou uspořádané seznamy hodnot jakéhokoliv typu
var mojePole = ["Ahoj", 45, true];

// Jednotlivé hodnoty jsou přístupné přes hranaté závorky.
// Členové pole se začínají počítat na nule.
myArray[1]; // = 45

// Pole je proměnlivé délky a členové se můžou měnit
myArray.push("Světe");
myArray.length; // = 4

// Přidání/změna na specifickém indexu
myArray[3] = "Hello";

// JavaScriptové objekty jsou stejné jako asociativní pole v jinných programovacích
// jazycích: je to neuspořádaná množina páru hodnot - klíč:hodnota.
var mujObjekt = {klic1: "Ahoj", klic2: "světe"};

// Klíče jsou řetězce, ale nejsou povinné uvozovky, pokud jsou validní
// JavaScriptové identifikátory. Hodnoty můžou být jakéhokoliv typu-
var mujObjekt = {klic: "mojeHodnota", "muj jiny klic": 4};

// K hodnotám můžeme přistupovat opět pomocí hranatých závorek
myObj["muj jiny klic"]; // = 4

// ... nebo pokud je klíč platným identifikátorem, můžeme přistupovat k
// hodnotám i přes tečku
mujObjekt.klic; // = "mojeHodnota"

// Objekty jsou měnitelné, můžeme upravit hodnoty, nebo přidat nové klíče.
myObj.mujDalsiKlic = true;

// Pokud se snažíte přistoupit ke klíči, který není nastaven, dostanete undefined
myObj.dalsiKlic; // = undefined

///////////////////////////////////
// 3. Řízení toku programu

// Syntaxe pro tuto sekci je prakticky stejná jako pro Javu

// `if` (když) funguje, jak byste čekali.
var pocet = 1;
if (pocet == 3){
    // provede, když se pocet rovná 3
} else if (pocet == 4){
    // provede, když se pocet rovná 4
} else {
    // provede, když je pocet cokoliv jinného
}

// Stejně tak cyklus while
while (true){
    // nekonečný cyklus
}

// Do-while cyklus je stejný jako while, akorát se vždy provede aspoň jednou
var vstup;
do {
    vstup = nactiVstup();
} while (!jeValidni(vstup))

// Cyklus for je stejný jako v Javě nebo jazyku C
// inicializace; podmínka pro pokračování; iterace.
for (var i = 0; i < 3; i++){
    // provede třikrát
}

// Cyklus For-in iteruje přes každo vlastnost prototypu
var popis = "";
var osoba = {prijmeni:"Paul", jmeno:"Ken", vek:18};
for (var x in osoba){
    popis += osoba[x] + " ";
}

//Když chcete iterovat přes vlastnosti, které jsou přímo na objektu a nejsou
//zděněné z prototypů, kontrolujte vlastnosti přes hasOwnProperty()
var popis = "";
var osoba = {prijmeni:"Jan", jmeno:"Novák", vek:18};
for (var x in osoba){
    if (osoba.hasOwnProperty(x)){
        popis += osoba[x] + " ";
    }
}

// for-in by neměl být použit pro pole, pokud záleží na pořadí indexů.
// Neexistuje jistota, že for-in je vrátí ve správném pořadí.

// && je logické a, || je logické nebo
if (dum.velikost == "velký" && dum.barva == "modrá"){
    dum.obsahuje = "medvěd";
}
if (barva == "červená" || barva == "modrá"){
    // barva je červená nebo modtrá
}

// && a || jsou praktické i pro nastavení základních hodnot
var jmeno = nejakeJmeno || "default";


// `switch` zkoumá přesnou rovnost (===)
// Používejte 'break;' po každé možnosti, jinak se provede i možnost za ní.
znamka = 'B';
switch (znamka) {
  case 'A':
    console.log("Výborná práce");
    break;
  case 'B':
    console.log("Dobrá práce");
    break;
  case 'C':
    console.log("Dokážeš to i lépe");
    break;
  default:
    console.log("Ale ne");
    break;
}

////////////////////////////////////////////////////////
// 4. Funckce, Oblast platnosti (scope) a Vnitřní funkce

// JavaScriptové funkce jsou definovány slůvkem `function`.
function funkce(text){
    return text.toUpperCase();
}
funkce("něco"); // = "NĚCO"

// Dávejte si pozor na to, že hodnota k vrácení musí začínat na stejné řádce
// jako slůvko return, jinak se vrátí 'undefined', kvůli automatickému vkládání
// středníků. Platí to zejména pro Allmanův styl zápisu.

function funkce()
{
    return // <- zde je automaticky vložen středník
    {
        tohleJe: "vlastnost objektu"
    }
}
funkce(); // = undefined

// JavaScriptové funkce jsou objekty, takže můžou být přiřazeny různým proměnným
// a předány dalším funkcím jako argumenty, na příklad:
function funkce(){
    // tento kód bude zavolán za 5 vteřin
}
setTimeout(funkce, 5000);
// Poznámka: setTimeout není část JS jazyka, ale funkce poskytována
// prohlížeči a NodeJS

// Další funkce poskytovaná prohlížeči je je setInterval
function myFunction(){
    // tento kód bude volán každých 5 vteřin
}
setInterval(myFunction, 5000);

// Objekty funkcí nemusíme ani deklarovat pomocí jména, můžeme je napsat jako
// ananymní funkci přímo vloženou jako argument
setTimeout(function(){
  // tento kód bude zavolán za 5 vteřin    
}, 5000);

// JavaScript má oblast platnosti funkce, funkce ho mají, ale jiné bloky ne
if (true){
    var i = 5;
}
i; // = 5 - ne undefined, jak byste očekávali v jazyku, kde mají bloky svůj
// rámec působnosti

// Toto je běžný model,který chrání před únikem dočasných proměnných do
//globální oblasti
(function(){
    var docasna = 5;
    // Můžeme přistupovat k globálního oblasti přes přiřazování globalním
    // objektům. Ve webovém prohlížeči je to vždy 'window`. Globální objekt
    // může mít v jiných prostředích jako Node.js jinné jméno.
    window.trvala = 10;
})();
docasna; // způsobí ReferenceError
trvala; // = 10

// Jedna z nejvice mocných vlastnosti JavaScriptu je vnitřní funkce. Je to funkce
// definovaná v jinné funkci, vnitřní funkce má přístup ke všem proměnným ve
// vnější funkci, dokonce i poté, co funkce skončí
function ahojPoPetiVterinach(jmeno){
    var prompt = "Ahoj, " + jmeno + "!";
    // Vnitřní funkce je dána do lokální oblasti platnosti, jako kdyby byla
    // deklarovaná slůvkem 'var'
    function vnitrni(){
        alert(prompt);
    }
    setTimeout(vnitrni, 5000);
    // setTimeout je asynchronní, takže funkce ahojPoPetiVterinach se ukončí
    // okamžitě, ale setTimeout zavolá funkci vnitrni až poté. Avšak protože
    // vnitrni je definována přes ahojPoPetiVterinach, má pořád přístup k
    // proměnné prompt, když je konečně zavolána.
}
ahojPoPetiVterinach("Adam"); // otevře popup s  "Ahoj, Adam!" za 5s

///////////////////////////////////////////////////
// 5. Více o objektech, konstuktorech a prototypech

// Objekty můžou obsahovat funkce
var mujObjekt = {
    mojeFunkce: function(){
        return "Ahoj světe!";
    }
};
mujObjekt.mojeFunkce(); // = "Ahoj světe!"

// Když jsou funkce z objektu zavolány, můžou přistupovat k objektu přes slůvko
// 'this''
var mujObjekt = {
    text: "Ahoj světe!",
    mojeFunkce: function(){
        return this.text;
    }
};
mujObjekt.mojeFunkce(); // = "Ahoj světe!"

// Slůvko this je nastaveno k tomu, kde je voláno, ne k tomu, kde je definováno
// Takže naše funkce nebude fungovat, když nebude v kontextu objektu.
var mojeFunkce = mujObjekt.mojeFunkce;
mojeFunkce(); // = undefined

// Opačně, funkce může být přiřazena objektu a může přistupovat k objektu přes
// this, i když nebyla přímo v definici-
var mojeDalsiFunkce = function(){
    return this.text.toUpperCase();
}
mujObjekt.mojeDalsiFunkce = mojeDalsiFunkce;
mujObjekt.mojeDalsiFunkce(); // = "AHOJ SVĚTE!"

// Můžeme také specifikovat, v jakém kontextu má být funkce volána pomocí
// `call` nebo `apply`.

var dalsiFunkce = function(s){
    return this.text + s;
}
dalsiFunkce.call(mujObjekt, " A ahoj měsíci!"); // = "Ahoj světe! A ahoj měsíci!"

// Funkce `apply`je velmi podobná, akorát bere jako druhý argument pole argumentů
dalsiFunkce.apply(mujObjekt, [" A ahoj slunce!"]); // = "Ahoj světe! A ahoj slunce!"

// To je praktické, když pracujete s funkcí, která bere sekvenci argumentů a
// chcete předat pole.

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN
Math.min.apply(Math, [42, 6, 27]); // = 6

// Ale `call` a `apply` jsou pouze dočasné. Pokud je chcete připojit trvale
// použijte `bind`.

var pripojenaFunkce = dalsiFunkce.bind(mujObjekt);
pripojenaFunkce(" A ahoj Saturne!"); // = "Ahoj světe! A ahoj Saturne!"

// `bind` může být použito čatečně částečně i k používání

var nasobeni = function(a, b){ return a * b; }
var zdvojeni = nasobeni.bind(this, 2);
zdvojeni(8); // = 16

// Když zavoláte funkci se slůvkem 'new', vytvoří se nový objekt a
// a udělá se dostupný funkcím skrz slůvko 'this'. Funkcím volaným takto se říká
// konstruktory

var MujKonstruktor = function(){
    this.mojeCislo = 5;
}
mujObjekt = new MujKonstruktor(); // = {mojeCislo: 5}
mujObjekt.mojeCislo; // = 5

// Každý JsavaScriptový objekt má prototyp. Když budete přistupovat k vlasnosti
// objektu, který neexistuje na objektu, tak se JS koukne do prototypu.

// Některé JS implementace vám umožní přistupovat k prototypu přes magickou
// vlastnost '__proto__'. I když je toto užitečné k vysvětlování prototypů, není
// to součást standardu, ke standartní způsobu k používání prototypu se dostaneme
// později.
var mujObjekt = {
    mujText: "Ahoj svete!"
};
var mujPrototyp = {
    smyslZivota: 42,
    mojeFunkce: function(){
        return this.mujText.toLowerCase()
    }
};

mujObjekt.__proto__ = mujPrototyp;
mujObjekt.smyslZivota; // = 42

// Toto funguje i pro funkce
mujObjekt.mojeFunkce(); // = "Ahoj světe!"

// Samozřejmě, pokud není vlastnost na vašem prototypu, tak se hledá na
// prototypu od prototypu atd.
mujPrototyp.__proto__ = {
    mujBoolean: true
};
mujObjekt.mujBoolean; // = true


// Zde neni žádné kopírování; každý objekt ukládá referenci na svůj prototyp
// Toto znamená, že můžeme měnit prototyp a změny se projeví všude
mujPrototyp.smyslZivota = 43;
mujObjekt.smyslZivota // = 43

// Zmínili jsme již předtím, že '__proto__' není ve standardu a není cesta, jak
// měnit prototyp existujícího objektu. Avšak existují možnosti, jak vytvořit
// nový objekt s daným prototypem

// První je Object.create, což je nedávný přídavek do JS a není dostupný zatím
// ve všech implementacích.
var mujObjekt = Object.create(mujPrototyp);
mujObjekt.smyslZivota // = 43

// Druhý způsob, který funguje všude je pomocí konstuktoru. Konstruktor má
// vlastnost jménem prototype. Toto *není* prototyp samotného konstruktoru, ale
// prototyp nového objektu.
MujKonstruktor.prototype = {
    mojeCislo: 5,
    ziskejMojeCislo: function(){
        return this.mojeCislo;
    }
};
var mujObjekt2 = new MujKonstruktor();
mujObjekt2.ziskejMojeCislo(); // = 5
mujObjekt2.mojeCislo = 6
mujObjekt2.ziskejMojeCislo(); // = 6

// Vestavěnné typy jako čísla nebo řetězce mají také konstruktory, které vytváří
// ekvivalentní obalovací objekty (wrappery).
var mojeCislo = 12;
var mojeCisloObj = new Number(12);
mojeCislo == mojeCisloObj; // = true

// Avšak nejsou úplně přesně stejné
typeof mojeCislo; // = 'number'
typeof mojeCisloObj; // = 'object'
mojeCislo === mojeCisloObj; // = false
if (0){
    // Tento kód se nespustí, protože 0 je nepravdivá (false)
}

if (new Number(0)){
   // Tento kód se spustí, protože obalená čísla jsou objekty,
   // a objekty jsou vždy pravdivé
}

// Avšak, obalovací objekty a normální vestavěnné typy sdílejí prototyp, takže
// můžete přidat funkcionalitu k řetězci
String.prototype.prvniZnak = function(){
    return this.charAt(0);
}
"abc".prvniZnak(); // = "a"

// Tento fakt je často používán v polyfillech, což je implementace novějších
// vlastností JavaScriptu do starších variant, takže je můžete používat třeba
// ve starých prohlížečích

// Pro příkklad, zmínili jsme, že Object.create není dostupný ve všech
// implementacích, můžeme si avšak přidat pomocí polyfillu
if (Object.create === undefined){ // nebudeme ho přepisovat, když existuje
    Object.create = function(proto){
        // vytvoříme dočasný konstruktor
        var Constructor = function(){};
        Constructor.prototype = proto;
        // ten použijeme k vytvoření nového s prototypem
        return new Constructor();
    }
}
```

## Kam dál

[Mozilla Developer
Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript) obsahuje
perfektní dokumentaci pro JavaScript, který je používaný v prohlížečích. Navíc
je to i wiki, takže jakmile se naučíte více, můžete pomoci ostatním, tím, že
přispějete svými znalostmi.

MDN's [A re-introduction to
JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
pojednává o konceptech vysvětlených zde v mnohem větší hloubce. Tento návod
pokrývá hlavně JavaScript sám o sobě. Pokud se chcete naučit více, jak se používá
na webových stránkách, začněte tím, že se kouknete na [DOM](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Learn Javascript by Example and with Challenges](http://www.learneroo.com/modules/64/nodes/350) je varianta tohoto
návodu i s úkoly-

[JavaScript Garden](http://bonsaiden.github.io/JavaScript-Garden/) je sbírka
příkladů těch nejvíce nepředvídatelných částí tohoto jazyka.

[JavaScript: The Definitive Guide](http://www.amazon.com/gp/product/0596805527/)
je klasická výuková kniha.

Jako dodatek k přímým autorům tohoto článku, některý obsah byl přizpůsoben z
Pythoního tutoriálu od Louie Dinh na této stráce, a z [JS
Tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
z Mozilla Developer Network.
