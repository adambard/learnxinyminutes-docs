---
language: sass
filename: learnsass-cz.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
translators:
  - ["Michal Martinek", "https://github.com/MichalMartinek"]
lang: cs-cz
---

Sass je rozšíření jazyka CSS, který přidává nové vlastnosti jako proměnné, zanořování, mixiny a další.
Sass (a další preprocesory, jako  [Less](http://lesscss.org/)) pomáhají vývojářům psát udržovatelný a neopakující (DRY) kód.

Sass nabízí dvě možnosti syntaxe. SCSS, které je stejná jako CSS, akorát obsahuje nové vlastnosti Sassu. Nebo Sass, který používá odsazení místo složených závorek a středníků.
Tento tutoriál bude používat syntaxi CSS.


Pokud jste již obeznámeni s CSS3, budete schopni používat Sass relativně rychle. Nezprostředkovává nějaké úplně nové stylové možnosti, spíše nátroje, jak psát Vás CSS kód více efektivně, udržitelně a jednoduše.

```scss


//Jednořádkové komentáře jsou ze Sassu při kompilaci vymazány

/*Víceřádkové komentáře jsou naopak zachovány */



/*Proměnné
==============================*/



/* Můžete uložit CSS hodnotu (jako třeba barvu) do proměnné.
Použijte symbol '$' k jejímu vytvoření. */

$hlavni-barva: #A3A4FF;
$sekundarni-barva: #51527F;
$body-font: 'Roboto', sans-serif;

/* Můžete používat proměnné napříč vaším souborem.
Teď, když chcete změnit barvu, stačí ji změnit pouze jednou.*/

body {
	background-color: $hlavni-barva;
	color: $sekundarni-barva;
	font-family: $body-font;
}

/* Toto se zkompiluje do: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Toto je o hodně více praktické, než měnit každý výskyt barvy.  */



/*Mixiny
==============================*/



/* Pokud zjistíte, že píšete kód pro více než jeden element, můžete jej uložit do mixinu.

Použijte '@mixin' direktivu, plus jméno vašeho mixinu.*/

@mixin na-stred {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Mixin vložíte pomocí '@include' a jména mixinu */

div {
	@include na-stred;
	background-color: $hlavni-barva;
}

/*Což se zkompiluje do: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}


/* Můžete využít mixiny i třeba pro takovéto ušetření práce: */

@mixin velikost($sirka, $vyska) {
	width: $sirka;
	height: $vyska;
}

/*Stačí vložit argumenty: */

.obdelnik {
	@include velikost(100px, 60px);
}

.ctverec {
	@include velikost(40px, 40px);
}

/* Toto se zkompiluje do: */
.obdelnik {
  width: 100px;
  height: 60px;
}

.ctverec {
  width: 40px;
  height: 40px;
}



/*Funkce
==============================*/   



/* Sass obsahuje funkce, které vám pomůžou splnit různé úkoly. */

/* Funkce se spouštějí pomocí jejich jména, které následuje seznam argumentů uzavřený v kulatých závorkách. */
body {
  width: round(10.25px);    
}

.footer {
  background-color: fade_out(#000000, 0.25)
}

/* Se zkompiluje do: */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}   

/* Můžete také definovat vlastní funkce. Funkce jsou velmi podobné mixinům.
   Když se snažíte vybrat mezi funkcí a mixinem, mějte na paměti, že mixiny
   jsou lepší pro generování CSS kódu, zatímco funkce jsou lepší pro logiku.
   Příklady ze sekce Matematické operátory jsou skvělí kandidáti na
   znovupoužitelné funkce. */

/* Tato funkce vrací poměr k velikosti rodiče v procentech.
@function vypocitat-pomer($velikost, $velikost-rodice) {
  @return $velikost / $velikost-rodice * 100%;
}

$hlavni obsah: vypocitat-pomer(600px, 960px);

.hlavni-obsah {
  width: $hlavni-obsah;
}

.sloupec {
  width: vypocitat-pomer(300px, 960px);
}

/* Zkompiluje do: */

.hlavni-obsah {
  width: 62.5%;
}

.sloupec {
  width: 31.25%;
}



/*Dědění
==============================*/



/*Dědění je způsob jak používat vlastnosti pro jeden selektor ve druhém. */

.oznameni {
	@include velikost(5em, 5em);
	border: 5px solid $sekundarni-barva;
}

.oznameni-uspech {
	@extend .oznameni;
	border-color: #22df56;
}

/* Zkompiluje do: */
.oznameni, .oznameni-uspech {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
}

.oznameni-uspech {
  border-color: #22df56;
}


/* Dědění CSS výrazů je preferováno před vytvořením mixinu kvůli způsobu,
   jakým způsobem Sass dává dohromady třídy, které sdílejí stejný kód.
   Kdyby to bylo udělané pomocí mixinu, tak výška, šířka, rámeček by byl v
   každém výrazu, který by volal mixin. I když tohle neovlivní vaše workflow,
   přidá to kód navíc do souborů. */


/*Zanořování
==============================*/



/*Sass vám umožňuje zanořovat selektory do selektorů */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
	}
}

/* '&' nahradí rodičovský element. */
/* Můžete také zanořovat pseudo třídy. */
/* Pamatujte, že moc velké zanoření do hloubky snižuje čitelnost.
   Doporučuje se používat maximálně trojité zanoření.
   Na příklad: */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: red;

		&:hover {
		  background-color: blue;
		}

		a {
		  color: white;
		}
	}
}

/* Zkompiluje do: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



/*Částečné soubory a importy
==============================*/   



/* Sass umožňuje vytvářet částečné soubory. Tyto soubory pomahájí udržovat váš
   kód modulární. Tyto soubory by měli začínat vždy '_', např. _reset.css.
   Částečné soubory se nepřevádí do CSS. */

/* Toto je kód, který si uložíme do souboru _reset.css */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Sass obsahuje @import, které může být použit pro import částečných souborů.
   Toto se liší od klasického CSS @import, který dělá HTTP požadavek na stáhnutí
   souboru. Sass vezme importovaný soubor a vloží ho do kompilovaného kódu. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* Zkompiluje do: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}   

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/*Zástupné selektory
==============================*/  



/* Zástupné selektory jsou užitečné, když vytváříte CSS výraz, ze kterého
   chcete později dědit. Když chcete vytvořit výraz, ze kterého je možné pouze
   dědit pomocí @extend, vytvořte zástupný selektor s CSS výrazem. Ten začíná
   symbolem '%' místo '.' nebo '#'. Tyto výrazy se neobjeví ve výsledném CSS */

%okno-obsahu {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.okno-zpravy {
  @extend %okno-obsahu;
  background-color: #0000ff;
}

/* Zkompiluje do: */

.okno-zpravy {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.okno-zpravy {
  background-color: #0000ff;
}



/*Matematické operace
==============================*/   



/* Sass obsahuje následující operátory: +, -, *, /, and %. Tyto operátory
   můžou být velmi užitečné pro počítání hodnot přímo ve vašem souboru Sass.
   Níže je příklad, jak udělat jednoduchý dvousloupcový layout. */

$oblast-obsahu: 960px;
$hlavni-obsah: 600px;
$vedlejsi-sloupec: 300px;

$obsah-velikost: $hlavni-obsah / $oblast-obsahu * 100%;
$vedlejsi-sloupec-velikost: $vedlejsi-sloupec / $oblast-obsahu * 100%;
$zbytek-velikost: 100% - ($main-size + $vedlejsi-sloupec-size);

body {
  width: 100%;
}

.hlavni-obsah {
  width: $obsah-velikost;
}

.vedlejsi-sloupec {
  width: $vedlejsi-sloupec-velikost;
}

.zbytek {
  width: $zbytek-velikost;
}

/* Zkompiluje do: */

body {
  width: 100%;
}

.hlavni-obsah {
  width: 62.5%;
}

.vedlejsi-sloupec {
  width: 31.25%;
}

.gutter {
  width: 6.25%;
}


```



## SASS nebo Sass?
Divili jste se někdy, jestli je Sass zkratka nebo ne? Pravděpodobně ne, ale řeknu vám to stejně. Jméno tohoto jazyka je slovo, "Sass", a ne zkratka.
Protože to lidé konstatně píší jako "SASS", nazval ho autor jazyka jako "Syntactically Awesome StyleSheets" (Syntaktický úžasně styly).


## Procvičování Sassu
Pokud si chcete hrát se Sassem ve vašem prohlížeči, navštivte [SassMeister](http://sassmeister.com/).
Můžete používát oba dva způsoby zápisu, stačí si vybrat v nastavení SCSS nebo SASS.


## Kompatibilita

Sass může být použit v jakémkoliv projektu, jakmile máte program, pomocí kterého ho zkompilujete do CSS. Pokud si chcete ověřit, že CSS, které Sass produkuje je kompatibilní s prohlížeči:

[QuirksMode CSS](http://www.quirksmode.org/css/) a [CanIUse](http://caniuse.com) jsou skvělé stránky pro kontrolu kompatibility.


## Kam dál?
* [Oficiální dokumentace](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) obsahuje tutoriál a řadu skvělých článků
