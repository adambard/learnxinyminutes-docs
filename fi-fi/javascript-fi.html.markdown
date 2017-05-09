---
kieli: javascript
avustajat:
    - ["Adam Brenecki", "http://adam.brenecki.id.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
kääntäkä:
    - ["Jarkko Laaksonen","https://github.com/Bunjl"]
filename: javascript-fi.js
lang: fi-fi
---

JavaScriptin kehitti Netscapen Brendan Eich vuonna 1995. Se oli alun perin
tarkoitettu yksinkertaisemmaksi skriptikieleksi sivustoille, täydentämään
Javalla kehitettyjä monimutkaisempia web-sovelluksia, mutta sen tiukka integrointi Web-sivuille
ja sisäänrakennettu tuki selaimissa on on tehnyt javascriptistä yleisemmän kuin
Java Webin.

Javascript ei ole rajoitu toimimaan vain selaimissa: Node.js, projekti, jonka
tarjoaa standalone runtime Google Chromen V8 JavaScript-moottori, on
yhä suositumpi, ulottaen javascriptin selaimesta täysivaltaiseksi kieleksi myös palvelin puolelle.

JavaScript on C-syntaksi, joten jos olet käyttänyt kieliä kuten C tai Java,
useat Syntaksit ovat jo tuttuja. Tästä huolimatta, ja vaikka
samankaltaisesta nimestä, JavaScriptin oliomalli on merkittävästi erilainen kuin
Javan.

`` `Js
// Yhden rivin kommentit alkavat vinoviivojen.
/ * Multiline kommentit alkavat slash tähden,
   ja päättyvät tähti-slash * /
```js

// Lausunnot voidaan irtisanoa;
doStuff ();

// ... Mutta niitä ei tarvitse olla, puolipisteet lisätään automaattisesti
// kun tapahtuu rivinvaihto, paitsi tietyissä tapauksissa.
doStuff()

// Koska nämä tapaukset voivat aiheuttaa odottamattomia tuloksia, me jatkamme
// puolipisteiden käyttä tässä oppaassa.

///////////////////////////////////
// 1. Numerot, Stringit ja Operaattorit

// JavaScriptissä on yksi numero tyyppi (joka on 64-bittinen IEEE 754 double).
// Doubles:lla on 52-bittinen mantissa, mikä riittää tallentaa kokonaislukuja
// Jopa noin 9✕10¹⁵ tarkkuudella.
3; // = 3
1.5; // = 1,5

// Jotkin peruslaskutoimituksia toimivat kuten voisit olettaa.
1 + 1; // = 2
0,1 + 0,2; // = ,30000000000000004
8-1; // = 7
10 * 2; // = 20
35/5; // = 7

// Sisältäem epätasaisen jaon.
5/2; // = 2,5

// Ja modulo jako.
10% 2; // = 0
30% 4; // = 2
18,5% 7; // = 4,5

// Bittioperaatiot myös toimivat; kun teet bittioperaation float
// muunnetaan allekirjoitetuksi int * jopa * 32 bittiä.
1 << 2; // = 4

// Etuusjärjestys toimii myös suluissa.
(1 + 3) * 2; // = 8

// On kolme erityistä ei--real-numero arvoa:
Infinity; // Tuloksena esim. 1/0
Ääretön; // Tuloksena esim. -1/0
NaN; // Tuloksena esim. 0/0, tarkoittaa "Not a Number"

// Mukana on myös boolean tyyppi.
true;
false;

// Stringsit luodaan ' tai ".
'abc';
"Hello, world";

// Negaatio käyttää! symbolia
!true; // = false
!false; // = true

// Yhdenvertaisuus ===
1 === 1; // = True
2 === 1; // = False

// Eriarvoisuus on !==
1! == 1; // = False
2! == 1; // = True

// Lisää vertailuja
1 <10; // = True
1> 10; // = False
2 <= 2; // = True
2> = 2; // = True

// Stringit ketjutetaan +:lla
"Hello " + "world!"; // = "Hello world!"

// ... Joka toimii enemmän kuin vain Stringeille
"1, 2, " + 3; // = "1, 2, 3"
"Hello " + ["world", "!"] // = "Hello world,!"

// Ja verrataan < and >
"a" < "b"; // = true

// Tyypin pakottaminen suoritetaan vertailemalla kaksinkertaisella yhtäsuuruusmerkillä ...
"5" == 5; // = true
null == undefined; // = true

// ... Jos käytät ===
"5" === 5; // = false
null === undefined; // = false

// ... Mikä voi aiheuttaa joskus outoa käytöstä...
13 +! 0; // 14
"13" +! 0; // "13true"

// Voit käyttää merkkijonon merkkien kanssa `charAt`
"This is a string".charAt(0);  // = 'T'

// ... Tai käytä `substring` saadaksesi suurempia kappaleita.
"Hello world".substring(0, 5); // = "Hello"

// `Pituudella` on ominaisuus, joten älä käytä ().
"Hello".length; // = 5

// Mukana on myös `null` ja` undefined`.
null; // Käytetään osoittamaan tarkoituksellisesti arvotonta arvoa
undefined; // Käytetään ilmaisemaan arvoa ei tällä hetkellä määritelty (vaikka
           // `undefined` on todella arvo itsessään)

// false, null, undefined, NaN, 0 ja "" ovat falseja; kaikki muu on truthyjä.
// Huomaa, että 0 on falsy ja "0" on truthy, vaikka 0 == "0".

///////////////////////////////////
// 2. Muuttujat, Taulukot ja objektit

// Muuttujat julistetaan `var` avainsanalla. JavaScript on dynaamisesti
// Kirjoitettu, joten sinun ei tarvitse määrittää tyyppiä. Määrite käyttää yhtä `=`
// merkkiä.
var someVar = 5;

// Jos jätät var avainsanan pois, et saa virhettä ...
someOtherVar = 10;

// ... Mutta muuttuja luodaan globaaliksi, eivät kuulu
// siihen scopeen johon olet määrittänyt sen.

// Muuttujat jotka julistetaan määrittelemättömiä ovat undefined.
var someThirdVar; // = undefined

// Jos haluat ilmoittaa pari muuttujia, niin voit käyttää pilkkua
// erottimena
var someFourthVar = 2, someFifthVar = 4;

// On  olemassa lyhenne matemaattisten toimintojen suorittamiseksi muuttujista:
someVar + = 5; // Vastaa someVar = someVar + 5; someVar on 10 nyt
someVar * = 10; // Nyt someVar on 100

// Ja lyhyempi muoto lisätä tai vähentää 1
someVar ++; // Nyt someVar on 101
someVar--; // nyt someVar on 100

// Taulukot ovat luettelo arvoja, mitä tyyppiä tahansa.
var myArray = ["Hei", 45, todellinen];

// Niiden jäseniä pääsee käyttämällä neliösuluissa alaindeksi syntaksia.
// Array indeksit alkavat nollasta.
myArray [1]; // = 45

// Taulukot ovat muuttuvia ja omaavat pituuden.
myArray.push ("World");
myArray.length; // = 4

// Lisää / Muuta tiettyyn indeksiin
myArray [3] = "Hei";

// JavaScript objekteja vastaavat "sanakirjat" tai "kartat" muissa
// kielissä: järjestämättöminää kokoelmina avainarvopareja.
var myObj = {key1: "Hello", key2: "World"};

// Avaimet ovat Stringejä, mutta lainausmerkkejä ei tarvita, jos ne ovat oikeita
// JavaScript tunnisteita. Arvot voivat olla mitä tahansa.
var myObj = {myKey: "myValue", "my other key": 4};


// Objektin ominaisuuksiin pääsee myös käyttämällä alaindeksi syntaksia,
myObj["my other key"]; // = 4

// ... Tai käyttämällä piste syntaksia, jos avain on kelvollinen tunniste.
myObj.myKey; // = "myValue"

// Objektit ovat vaihteleva; arvoja voidaan muuttaa ja uusia avaimia lisätä.
myObj.myThirdKey = true;

// Jos yrität käyttää arvoa, joka ei ole vielä asetettu, saat undefined.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. Logiikka ja valvontarakenteet

// `If` rakenne toimii kuten voisit odottaa.
var count = 1;
if (count == 3) {
    // Arvioitiin jos määrä on 3
} Else if (count == 4) {
    // Arvioitiin jos määrä on 4
} Else {
    // Arvioitu jos se ei ole myöskään 3 tai 4
}

// Samoin `while`.
while (true) {
    // Loputon silmukka!
}

// Do-while silmukat ovat kuin for-silmukat, paitsi ne tapahtuvat aina vähintään kerran.
var input;
do {
    input = getInput ();
} While (! IsValid (input))

// `For` silmukka on sama kuin C:llä ja Java:lla :
// Alustus; jatkaa ehtoa; iteroi.
for (var i = 0; i <5; i ++) {
    // Ajaa 5 kertaa
}

// For/in iteroi yli jokaisen ominaisuuden koko prototyyppi ketjussa.
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    description += person[x] + " ";
}


// Jos haluat vain katsoa ominaisuuksiin kiinnitetty objekteja
// etkä sen prototyyppejä, voit käyttää `hasOwnProperty ()` tarkistusta.
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    if (person.hasOwnProperty(x)){
        description += person[x] + " ";
    }
}

// For/in looppia ei tule käyttää iteroimaan taulukkoa jos indeksi järjestys
// nn tärkeä, koska ei ole mitään takeita, for/in palauttaa indeksit
// Missään tietyssä järjestyksessä.

// && On looginen ja, || on looginen tai
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // Väri on joko punainen tai sininen
}

// && ja || "OR", joka on hyödyllinen asettamaan oletusarvot.
var name = otherName || "default";


// `Switch` lausunto tarkistaa oikeellisuuta` === `.
// Käytä "break" kussakin tapauksessa
// tai myöhemmätkin tapaukset ajetaan myös.
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
// 4. Toiminnot, laajuus ja lopetukset

// JavaScript funktiot on alustettu `function` avainsanalla.
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"


// Huomaa, että arvo joka palautetaan on aloitettava samalla rivillä kuin
// `Return` avainsanan, muuten sinun aina palauttaa` undefined`, 
// automaattinen puolipisteen lisäyksen takia. Varo tätä käytettäessä Allman tyyliä.
function myFunction(){
    return // <- semicolon automatically inserted here
    {thisIsAn: 'object literal'}
}
myFunction(); // = undefined

// JavaScript toiminnot ovat ensiluokkaisia objekteja, joten niissä voidaan uudelleen määrittää
// eri muuttujanimet ja toimitettaa muihin funktioihin argumentteina.
// Esimerkiksi toimittaessaan tapahtumakäsittely:
function myFunction(){
    // Tämä koodi kutsutaan 5 sekunnin kuluttua
}
setTimeout (myFunction, 5000);
// Huomautus: setTimeout ei kuulu JS kielen, mutta sen tarjoaa selaimet
// ja Node.js.

// Toinen funktio joka tarjotaa selaimissa on setInterval
function myFunction(){
    // Tämä koodi on nimeltään 5 sekunnin välein
}
setInterval (myFunction, 5000);

//Funktoin objekteja ei edes tarvitse ilmoittaa, nimeltä - voit kirjoittaa
// anonyymin funktion määritelmällä suoraan väitteet toiseen.
setTimeout(function(){
    // Tämä koodi kutsutaan 5 sekunnin kuluttua
}, 5000);

// JavaScriptin funktioilla on soveltamisala; toiminnot saavat oman soveltamisalaa mutta muiden lohkojen
// eivät.
if (true){
    var i = 5;
}
i; // = 5 - ei määrittelemättömän kuten voi odottaa block-scoped kieleltä

// Tämä on johtanut yhteiseen malliin "välittömästi-täytäntöönpantavista anonyymeistä
// funktioista", jotka estävät väliaikaisia muuttujia vuotamasta globaaliin
// soveltamisalaan.
(function(){
    var temporary = 5;
    // Voimme käyttää yelistä soveltamisalaan osoittamalla "globaali objekti", jotka
    // Selaimella on aina `window`. Globaali esine voi olla
    // Eri nimi ei-selain ympäristöissä, kuten Node.js.
    window.permanent = 10;
}) ();
temporary; // aiheuttaa ReferenceError
permanent; // = 10

// Yksi JavaScriptin n tehokkaimmista ominaisuuksista on sulkemiset. Jos toiminto on
// Määritelty toisen funktion sisällä, sisemmällä funktiolla on pääsy kaikkiin
// ulomman funktion muuttujiin, vaikka ulompi funktio poistuu.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Sisäisen funktio ottaa käyttöön paikallisen soveltamisala oletusarvoisesti, aivan kuin ne olisivat julisitettu 'var' muodossa
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // SetTimeout on asynkroninen, joten sayHelloInFiveSeconds toiminto
    // lopettaa heti, ja setTimeout herättää sisäistä funktiota jälkeenpäin. Kuitenkin,
    // Koska sisäinen on "suljettu yli" sayHelloInFiveSeconds, sisemmällä on vielä
    // Pääsy `prompt` muuttujaan, kun se vihdoin kutsutaan.
}
sayHelloInFiveSeconds ("Adam"); // Avaa popupin "Hei, Adam!" 5. seknnin kuluttua.

///////////////////////////////////
// 5. Lisätietoja Esineet; Rakentajat ja Prototyypit

var MyConstructor = toiminto () {
    this.myNumber = 5;
}
myNewObj = uusi MyConstructor (); // = {MyNumber: 5}
myNewObj.myNumber; // = 5

// Jokaisella JavaScript oliolla on "prototyyppi". Kun menet tarkistelemaan oliota
// Objektista JOKA EI löydy varsinaisesta objektista, Tulkki
// Kehottaa katsomaan Sen prototyyppiä.

// Joidenkin JS toteutusten avulla VOIT käsitellä objektin prototyyppiä,
// Omaisuudella `__proto__`. Vaikka This on hyödyllinen selitettäessä prototyyppejä SE Ei ole
// OSA standardia; standardin mukaisia ​​tapoja hyödyntää prototyyppejä tarkastellaan myöhemmin.
var myObj = {
myString: "Hei maailma!"
};
var myPrototype = {
    meaningOfLife: 42,
    myfunc: toiminto () {
        palata this.myString.toLowerCase ()
    }
};

myObj .__ proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// This Toimii functiolle.
myObj.myFunc (); // = "Hei maailma!"

// Tietenkin, Jos ominaisuutena Ei ole kyseinen prototyyppi,
// Prototyyppi etsitään, ja Niin edelleen.
myPrototype .__ proto__ = {
    myBoolean: totta
};
myObj.myBoolean; // = True

// Ei ole kopiointia Alone Täällä; jokainen objekti tallentaa viittauksen Sen
// Prototyyppiin. This tarkoittaa, Että voimme Muuttaa prototyyppiä ja Meidän muutokset Ovat
// Näkyvissä kaikkialla.
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// Me mainitsimme, Etta `__proto__` OLI EI-Standardi, ja Ei ole standardia hän tapaa
// Muuttaa olemassa olevan objektin prototyyppiä. On kuitenkin olemassa Kaksi hän tapaa
// Luoda uusi objektin tietyllä prototyypillä.

// Ensimmäinen on Object.create, JOKA on viimeisin Appendix JS: n, ja siksi
// Ei ole käytettävissä kaikissa toteutuksissa Vielä.
var myObj = Object.create (myPrototype);
myObj.meaningOfLife; // = 43

// Toinen Tapa, JOKA Toimii Missä tahansa, on tekemistä rakentajien KANSSA.
// Rakentajilla on ominaisuus nimeltä prototyyppi. This * Ei ole * Rakentajan prototyyppi
// Toiminto itsessään; Sen sijaan se on Uuden objektin prototyyppi, JOKA
// Annetaan KUN luotu SEKA Rakentaja Että uusi avainsana.
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: toiminto () {
        palata this.myNumber;
    }
};
var myNewObj2 = uusi MyConstructor ();
myNewObj2.getMyNumber (); // = 5
myNewObj2.myNumber = 6
myNewObj2.getMyNumber (); // = 6

// Sisäänrakennetut Tyypit kuten Stringit ja Numerot on MYÖS rakentajille, olevissa asioissa Luovat
// Vastaavia kääre objekteja.
var myNumber = 12;
var myNumberObj = uusi numero (12);
myNumber == myNumberObj; // = True

// Huom! eivät Ole täysin vastaavia.
typeof myNumber; // = 'Numero'
typeof myNumberObj; // = 'Esine'
myNumber === myNumberObj; // = False
jos (0) {
    // This Koodi EI toimisi, KOSKA 0 epätosi.
}
jos (uusi numero (0)) {
   // This Koodi Toimii, KOSKA wrapperoidut Numerot
   // Ovat objekteja ja objektit Ovat Aina Tosia.
}

// Kuitenkin kääre objektit ja säännölliset komennot jakavat prototyypin, joten
// VOIT Itse lisätä toimintoja merkkijonoon, esimerkiksi.
String.prototype.firstCharacter = toiminto () {
    palata this.charAt (0);
}
"ABC" .firstCharacter (); // = ""

//: Tätä seikkaa on USEIN käytetään "polyfillingissä", JOKA toteuttaa uudempia JavaScriptin
// Ominaisuuksia, JavaScript vanhemmissa osajoukoissa, jotta niita voidaan
// Käyttää vanhemmissa ympäristöissä, kuten vanhentuneissa selaimissa.

// Esimerkiksi mainitsemme, Että Object.create Ei ole Vielä saatavilla kaikissa
// Toteutuksia, mutta voimme silti käyttää Sita TÄLLÄ Polyfillä:
jos (Object.create === määrittelemätön) {// EI Korvaa SITA, Jos se on olemassa
Object.create = toiminto (proto) {
// Tehdä väliaikaisen Rakentajan KANSSA oikea prototyyppi
        var Rakentaja = function () {};
Constructor.prototype = proto;
// Käytetään SITA jotta luodaa Uusi, asianmukainen-prototyyppi objekti
        palata uusi Rakentaja ();
}
}
`` `

## Kirjallisuutta

[Mozilla Developer Network] [1] tarjoaa erinomaisen dokumentaation
JavaScriptistä siten Kuin Stia käytetään selaimissa. Plus, se on wiki, Eli KUN Opit enemmän
VOIT auttaa muita jakamalla Oman tietosi ..

MDN: n [uudelleen käyttöön JavaScript] [2] kattaa Paljon käsitteitä yksityiskohtaisemmin. This opas täysin tietoisesti kattaa turhaan
JavaScript kielen Itse; Jos haluat tietää lisää siitä, Miten käyttää
JavaScripta web-sivuille, aloita täältä [Document Object Model] [3].

[Lue JavaScriptin Esimerkki ja haasteet] [4] muunnelma TÄSTÄ
VIITE sisäänrakennetuihin haasteisiin.

[Javascript Garden] [5] perusteellinen opas kaikista EPA-intuitiivinen osista
kielessä.

[JavaScript: Lopulliset opas] [6] Klassinen opas ja hakuteos.

[Eloquent Javascript] [8] Marijn Haverbekeltä päälle Erinomainen JS kirja / eBook, where liitteenä terminaali.

[Javascript: Oikea tapa] [9] opas JOKA on tarkoitus ottaa käyttöön uutena kehittäjänä JavaScriptissä ja lisätä kokeneiden kehittäjien LISÄTIETOJA Senin parhaista käytännöistä.


Suorien kontribuutioiden ohella Taman Artikkelin, jotkut sisältöä on mukautettu
Louie Dinhn Python tutoriaalista TÄLLÄ sivustolla, ja [JS Tutorial] [7]
Mozilla Developer Network.


[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[3]: https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core
[4]: http://www.learneroo.com/modules/64/nodes/350
[5]: http://bonsaiden.github.io/JavaScript-Garden/
[6]: http://www.amazon.com/gp/product/0596805527/
[7]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
[8]: http://eloquentjavascript.net/
[9]: http://jstherightway.org/