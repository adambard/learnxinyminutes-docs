---
category: tool
tool: amd
contributors:
    - ["Frederik Ring", "https://github.com/m90"]
translators:
    - ["Reinoud Kruithof", "https://github.com/reinoudk"]
filename: learnamd-nl.js
lang: nl-nl
---

## Aan de slag met AMD

De **Asynchronous Module Definition** API specificeert een mechanisme om JavaScript
 modules the definiëren zodat de module en dependencies (afhankelijkheden) asynchroon
 geladen kunnen worden. Dit is vooral erg geschikt voor de browseromgeving, waar het
 synchroon laden van modules zorgt voor problemen qua prestatie, gebruiksvriendelijkheid,
 debugging en cross-domain toegangsproblemen.

### Basis concept

```javascript
// De basis AMD API bestaat uit niks meer dan twee methodes: `define` en `require`
// and gaat vooral over de definitie en gebruik van modules:
// `define(id?, dependencies?, factory)` definieert een module
// `require(dependencies, callback)` importeert een set van dependencies en
// gebruikt ze in de gegeven callback

// Laten we starten met het gebruiken van define om een nieuwe module (met naam)
// te creëeren, welke geen dependencies heeft. Dit doen we door een naam
// en een zogeheten factory functie door te geven aan define:
define('awesomeAMD', function(){
  var isAMDAwesome = function(){
    return true;
  };
  // De return waarde van een module's factory functie is
  // wat andere modules of require calls ontvangen wanneer
  // ze onze `awesomeAMD` module requiren.
  // De geëxporteerde waarde kan van alles zijn: (constructor) functies,
  // objecten, primitives, zelfs undefined (hoewel dat niet veel nut heeft).
  return isAMDAwesome;
});


// We gaan nu een andere module defineren die afhankelijk is van onze
// `awesomeAMD` module. Merk hierbij op dat er nu een extra functieargument
// is die de dependencies van onze module defineert:
define('schreewlelijk', ['awesomeAMD'], function(awesomeAMD){
  // dependencies worden naar de factory's functieargumenten
  // gestuurd in de volgorde waarin ze gespecificeert zijn
  var vertelIedereen = function(){
    if (awesomeAMD()){
      alert('Dit is zOoOo cool!');
    } else {
      alert('Vrij saai, niet?');
    }
  };
  return vertelIedereen;
});

// Nu we weten hoe we define moeten gebruiken, kunnen we require gebruiken
// om ons programma mee te starten. De vorm van `require` is
// `(arrayVanDependencies, callback)`.
require(['schreeuwlelijk'], function(schreewlelijk){
  schreeuwlelijk();
});

// Om deze tutorial code uit te laten voeren, gaan we hier een vrij basic
// (niet-asynchrone) versie van AMD implementeren:
function define(naam, deps, factory){
  // merk op hoe modules zonder dependencies worden afgehandeld
  define[naam] = require(factory ? deps : [], factory || deps);
}

function require(deps, callback){
  var args = [];
  // we halen eerst alle dependecies op die nodig zijn
  // om require aan te roepen
  for (var i = 0; i < deps.length; i++){
    args[i] = define[deps[i]];
  }
  // voldoe aan alle dependencies van de callback
  return callback.apply(null, args);
}
// je kan deze code hier in actie zien (Engels): http://jsfiddle.net/qap949pd/
```

### require.js in de echte wereld

In contrast met het voorbeeld uit de introductie, implementeert `require.js`
 (de meest populaire AMD library) de **A** in **AMD**. Dit maakt het mogelijk
 om je modules en hun dependencies asynchroon in the laden via XHR:

```javascript
/* file: app/main.js */
require(['modules/someClass'], function(SomeClass){
  // de callback word uitgesteld tot de dependency geladen is
  var things = new SomeClass();
});
console.log('Dus, hier wachten we!'); // dit wordt als eerste uitgevoerd
```

De afspraak is dat je over het algemeen één module in één bestand opslaat.
`require.js` kan module-namen achterhalen gebaseerd op de bestandslocatie,
dus je hoeft je module geen naam te geven. Je kan simpelweg aan ze referen
 door hun locatie te gebruiken.
In het voorbeeld nemen we aan dat `someClass` aanwezig is in de `modules` map,
 relatief ten opzichte van de `baseUrl` uit je configuratie.

* app/
  * main.js
  * modules/
    * someClass.js
    * someHelpers.js
    * ...
  * daos/
    * things.js
    * ...

Dit betekent dat we `someClass` kunnen defineren zonder een module-id te specificeren:

```javascript
/* file: app/modules/someClass.js */
define(['daos/things', 'modules/someHelpers'], function(thingsDao, helpers){
  // definitie van de module gebeurt, natuurlijk, ook asynchroon
  function SomeClass(){
    this.method = function(){/**/};
    // ...
  }
  return SomeClass;
});
```

Gebruik `requirejs.config(configObj)` om het gedrag van de standaard mapping
 aan te passen in je `main.js`:

```javascript
/* file: main.js */
requirejs.config({
  baseUrl : 'app',
  paths : {
    // je kan ook modules uit andere locatie inladen
    jquery : '//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min',
    coolLibUitBower : '../bower_components/cool-lib/coollib'
  }
});
require(['jquery', 'coolLibUitBower', 'modules/someHelpers'], function($, coolLib, helpers){
  // een `main` bestand moet require minstens eenmaal aanroepen,
  // anders zal er geen code uitgevoerd worden
  coolLib.doFancyDingenMet(helpers.transform($('#foo')));
});
```

Op `require.js` gebaseerde apps hebben vaak een enkel beginpunt (`main.js`)
 welke toegevoegd wordt aan de `require.js` script tag als een data-attribuut.
Deze zal automisch geladen en uitgevoerd worden als de pagina laadt:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Honder script tags? Nooi meer!</title>
</head>
<body>
  <script src="require.js" data-main="app/main"></script>
</body>
</html>
```

### Een heel project optimaliseren met r.js

Veel mensen geven er de voorkeur aan om AMD te gebruiken tijdens de
  ontwikkelfase om code op een gezonde manier te organiseren maar
  willen nog steeds een enkel scriptbestand gebruiken in productie in
  plaats van honderderen XHR verzoeken uit te voeren als de pagina laadt.

`require.js` wordt geleverd met een script genaamd `r.js` (die je waarschijnlijk
uitvoert in node.js, hoewel Rhino ook ondersteund wordt) welke de
dependency book van je project analyseert en een enkel bestand bouwt met daarin
al je module (juist genaamd), geminificeerd en klaar voor productie.

Instaleren met `npm`:

```shell
$ npm install requirejs -g
```

Nu kun je het een configuratiebestand voeden:

```shell
$ r.js -o app.build.js
```

Voor ons bovenstaande voorbeeld zou de configuratie er zo uit kunnen zien:

```javascript
/* file : app.build.js */
({
  name : 'main', // naam van het beginpunt
  out : 'main-built.js', // naam van het bestand waar de output naar geschreven wordt
  baseUrl : 'app',
  paths : {
    // `empty:` verteld r.js dat dee nog steeds geladen moet worden van de CDN,
    // gebruik makend van de locatie gespecificeert in `main.js`
    jquery : 'empty:',
    coolLibUitBower : '../bower_components/cool-lib/coollib'
  }
})
```

Verwissel simpelweg `data-main` om het gebouwde bestand te gebruiken in productie:

```html
<script src="require.js" data-main="app/main-built"></script>
```

Een erg gedetaileerd [overzicht van bouwopties](https://github.com/jrburke/r.js/blob/master/build/example.build.js) is
beschikbar in de GitHub repo (Engels).

Hieronder vind je nog meer informatie over AMD (Engels).

### Onderwerpen die niet aan bod zijn gekomen

* [Loader plugins / transforms](http://requirejs.org/docs/plugins.html)
* [CommonJS style loading and exporting](http://requirejs.org/docs/commonjs.html)
* [Advanced configuration](http://requirejs.org/docs/api.html#config)
* [Shim configuration (loading non-AMD modules)](http://requirejs.org/docs/api.html#config-shim)
* [CSS loading and optimizing with require.js](http://requirejs.org/docs/optimization.html#onecss)
* [Using almond.js for builds](https://github.com/jrburke/almond)

### Verder lezen:

* [Official Spec](https://github.com/amdjs/amdjs-api/wiki/AMD)
* [Why AMD?](http://requirejs.org/docs/whyamd.html)
* [Universal Module Definition](https://github.com/umdjs/umd)

### Implementaties:

* [require.js](http://requirejs.org)
* [dojo toolkit](http://dojotoolkit.org/documentation/tutorials/1.9/modules/)
* [cujo.js](http://cujojs.com/)
* [curl.js](https://github.com/cujojs/curl)
* [lsjs](https://github.com/zazl/lsjs)
* [mmd](https://github.com/alexlawrence/mmd)
