---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
translators:
    - ["Michal Martinek", "https://github.com/MichalMartinek"]
lang: cs-cz
filename: learncss-cz.css
---

V ranných dobách webu se nevyskytovaly žádné vizuální elementy, pouze čistý text, ale s vývojem webových browserů se staly stránky plné grafických prvků běžné.

A právě proto vzniklo CSS, aby oddělilo obsah (HTML) od vzhledu webové stránky.

Pomocí CSS můžete označit různé elementy na HTML stránce a přiřadit jim různé vzhledové vlastnosti.

Tento návod byl napsán pro CSS 2, avšak CSS 3 se stalo velmi oblíbené a v dnešní době už běžné.

**POZNÁMKA** Protože CSS produkuje vizuální výsledky, je nutné k jeho naučení všechno zkoušet třeba na [dabbletu](http://dabblet.com/).
Tento článek se zaměřuje hlavně na syntaxi a poskytue také pár obecných tipů.

```css
/* komentáře jsou ohraničeny lomítkem s hvězdičkou, přesně jako tyto dva
   řádky, v CSS není nic jako jednořádkový komentář, pouze tenhle zápis   */

/* ################
   ## SELEKTORY
   ################ */

/* Selektor se používá pro vybrání elementu na stránce:
selektor { vlastnost: hodnota; /* více vlastností...  }*/

/*
Toto je náš element:
<div trida='trida1 trida2' id='nejakeID' attr='hodnota' otherAttr='cs-cz co neco' />
*/

/* Můžeme vybrat tento element třeba podle jeho třídy */
.trida1 { }

/* nebo obou tříd! */
.trida1.trida2 { }

/* nebo jeho jména */
div { }

/* nebo jeho id */
#nejakeID { }

/* nebo podle toho, že má atribut! */
[attr] { font-size:smaller; }

/* nebo že argument nabývá specifické hodnoty*/
[attr='hodnota'] { font-size:smaller; }

/* začíná nějakou hodnotou (CSS 3) */
[attr^='ho'] { font-size:smaller; }

/* nebo končí něčím (CSS 3) */
[attr$='ta'] { font-size:smaller; }

/* nebo obsahuje nějakou hodnotu, která je v atributu oddělená mezerami */
[otherAttr~='co'] { }
[otherAttr~='neco'] { }

/* nebo obsahuje hodnotu oddělenou pomlčkou - "-" (U+002D) */
[otherAttr|='cs'] { font-size:smaller; }


/* Můžeme spojit různé selektory, abychom získali specifičtější selektor.
   Pozor, nedávejte mezi ně mezery! */
div.nejaka-trida[attr$='ta'] { }

/* Můžeme vybrat element, který je potomek jineho */
div.vnejsi-element > .jmeno-tridy { }

/* nebo zanořen ještě hlouběji. Potomci jsou přímo pod vnější třídou, pouze 1
   úroveň pod rodičem. Tento selektor bude fungovat na jakékoliv úrovni pod
   rodičem */
div.rodic .jmeno-tridy { }

/* Varování: stejný selektor bez mezery má úplně jiný význam
   Vzpomínáte si jaký? */
div.rodic.jmeno-tridy { }

/* Možná budete chtít vybrat element, který leží přímo vedle */
.jsem-primo-pred + .timto-elementem { }

/* nebo kdekoliv na stejné úrovni stromu */
.jsem-kdekoliv-pred ~ .timto-elementem { }

/* Existují selektory nazvané pseudo třídy, kterými můžeme vybrat elementy,
   když jsou v určitém stavu */

/* na příklad, když kurzor najede na element */
selektor:hover { }

/* nebo již navštívený odkaz */
selektor:visited { }

/* nebo nebyl navštíven */
selektor:link { }

/* nebo když je vybrán, např kliknutím do inputu*/
selektor:focus { }

/* element, ktery je prvni potomek rodiče */
selektor:first-child {}

/* element, který je poslední potomek rodiče */
selektor:last-child {}

/* Stejně jako pseudo třídy, umožňují pseudo elementy stylizovat určité
   části dokumentu */

/* odpovídá virtuálnímu prvnímu potomku */
selektor::before {}

/* odpovídá virtuálnímu poslednímu potomku */
selektor::after {}

/* Na vhodném místě, může být použitá hvězdička jako žolík, který vybere každý element */
* { } /* všechny elementy */
.rodic * { } /* všechny vnořené elementy */
.rodic > * { } /* všichni potomci */

/* ####################
   ## VLASTNOSTI
   #################### */

selektor {

    /* Jednotky délky můžou být relativní nebo absolutní */

    /* Relativní jednotky */
    width: 50%;       /* počet procent šířky rodičovského elementu */
    font-size: 2em;   /* násobek puvodní velikosti fontu elementu */
    font-size: 2rem;  /* nebo kořenového elementu */
    font-size: 2vw;   /* násobek 1% šířky zařízení (viewport) (CSS 3) */
    font-size: 2vh;   /* nebo jeho výšky */
    font-size: 2vmin; /* násobek 1% výšky nebo šířky, dle toho, co je menší */
    font-size: 2vmax; /* nebo větší */

    /* Absolutní jednotky */
    width: 200px;     /* pixely */
    font-size: 20pt;  /* body */
    width: 5cm;       /* centimetry */
    min-width: 50mm;  /* milimetry */
    max-width: 5in;   /* palce */

    /* Barvy */
    color: #F6E;                 /* krátký hexadecimální formát */
    color: #FF66EE;              /* dlouhý hexadecimální formát */
    color: tomato;               /* pojmenovaná barva */
    color: rgb(255, 255, 255);   /* hodnoty rgb */
    color: rgb(10%, 20%, 50%);   /* procenta rgb */
    color: rgba(255, 0, 0, 0.3); /* hodnoty rgba (CSS 3) Poznámka: 0 < a < 1 */
    color: transparent;          /* ekvivalentní jako nastavení alfy 0 */
    color: hsl(0, 100%, 50%);    /* procenta hsl (CSS 3) */
    color: hsla(0, 100%, 50%, 0.3); /* procenta hsl s alfou */

    /* Obrázky jako pozadí elementu */
    background-image: url(/cesta/k/obrazku.jpg); /* uvozovky jsou dobrovolné */

    /* Fonty */
    font-family: Arial;
    /* když název fontu obsahuje mezeru, tak musí být v uvozovkách */
    font-family: "Courier New";
    /* když se první nenaleze, použije se další atd. */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## Použití

Uložte CSS soubor s příponou `.css`.

```xml
<!-- Musíte vložit css soubor do hlavičky vaší stránky. Toto je
     doporučená metoda. Viz http://stackoverflow.com/questions/8284365 -->
<link rel='stylesheet' type='text/css' href='cesta/k/stylu.css' />

<!-- Také lze vložit CSS přímo do HTML. -->
<style>
   a { color: purple; }
</style>

<!-- Nebo přímo nastavit vlasnost elementu -->
<div style="border: 1px solid red;">
</div>
```

## Priorita nebo kaskáda

Element může být vybrán více selektory a jeho vlastnosti můžou být nastaveny více než jednou. V těchto případech, má jedno zadání vlastnosti prioritu před druhým. Obecně platí, že více specifické selektory mají přednost před těmi méně specifickými.

Tento proces se nazývá kaskáda, proto i název kaskádové styly(Cascading Style Sheets).

Máme následující CSS

```css
/* A */
p.trida1[attr='hodnota']

/* B */
p.trida1 { }

/* C */
p.trida2 { }

/* D */
p { }

/* E */
p { vlastnost: hodnota !important; }
```

a tento element
```xml
<p style='/*F*/ vlastnost:hodnota;' trida='trida1 trida2' attr='hodnota' />
```
Priorita stylu je následující. Pamatujte, priorita pro každou **vlastnost**, ne pro celý blok.

* `E` má nejvyšší prioritu kvůli slůvku `!important`. Je doporučováno se úplně vyhnout jeho použití.
* `F` je další, kvůli stylu zadanému přimo do elementu
* `A` je další, protože je více specifické, než cokoliv dalšího. Má 3 selektory: jméno elementu `p`, jeho třídu `trida1`, atribut `attr='hodnota'`.
* `C` je další, i když je stejně specifický jako `B`, protože je uveden až po něm.
* `B` je další
* `D` je poslední

## Kompatibilita

Většina z možností v CSS 2 (a spousta v CSS 3) je dostupná napříč všemi browsery a zařízeními. Ale pořád je dobrá praxe, zkontrolovat dostupnost, před užitím nové vlastnosti/fičury.

## Zdroje

* Přehled dostupnosti [CanIUse](http://caniuse.com).
* CSS hřiště [Dabblet](http://dabblet.com/).
* [Mozilla Developer Network - CSS dokumentace](https://developer.mozilla.org/en-US/docs/Web/CSS)
* [Codrops](http://tympanus.net/codrops/css_reference/)

## Další čtení

* [Pochopení priority v CSS: specifičnost, děditelnost a kaskáda](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Vybírání elementů pomocí atributů](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - překrývání obsahu](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) a [LESS](http://lesscss.org/) pro CSS pre-processing
* [CSS-Triky](https://css-tricks.com)
