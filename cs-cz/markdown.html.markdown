---
language: markdown
lang: cs-cz
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Michal Martinek", "https://github.com/MichalMartinek"]
filename: markdown-cz.md
lang: cs-cz
---

Markdown byl vytvořen Johnem Gruberem v roce 2004. Je zamýšlen jako lehce čitelná
a psatelná syntaxe, která je jednoduše převeditelná do HTML (a dnes i do mnoha
dalších formátů)

```markdown
<!-- Markdown je nadstavba nad HTML, takže jakýkoliv kód HTML je validní
Markdown, to znamená, že můžeme používat HTML elementy, třeba jako komentář, a
nebudou ovlivněny parserem Markdownu. Avšak, pokud vytvoříte HTML element v
Markdownu, tak nemůžete používat syntaxi Markdownu uvnitř tohoto elementu. -->

<!-- Markdown se také mírně liší v jednotlivých interpretacích parseru. Tento
návod vás bude upozorňovat, které vlastnosti jsou obecné a které specifické pro
konkrétní parser. -->

<!-- Nadpisy -->
<!-- Můžete vytvořit HTML elementy <h1> až <h6> jednoduše tak, že text předsadíte
počtem křížků (#), podle toho jaké úrovně to má být nadpis -->
# Toto je <h1>
## Toto je <h2>
### Toto je <h3>
#### Toto je <h4>
##### Toto je <h5>
###### Toto je <h6>

<!-- Markdown obsahuje taky dvě další cesty, jak udělat h1 a h2 -->
Toto je h1
==========

Toto je h2
----------

<!-- Jednoduché stylování textu -->
<!-- Pomocí markdownu můžete text jednoduše označit jako kurzívu či tučný -->

*Tento text je kurzívou;*
_Stejně jako tento._

**Tento text je tučně**
__Stejně jako tento.__

***Tento text je obojí***
**_Jako tento!_**
*__A tento!__*

<!-- Ve verzi Markdownu od GitHubu, máme k dispozici taky prošktrnutí: -->

~~Tento text je prošktrnutý.~~

<!-- Odstavce jsou jedna nebo více řádek textu, oddělených jednou nebo více prázdnými řádky. -->

Toto je odstavec. Píši odstavec, není to zábava?

Teď jsem v odstavci 2.
Jsem pořád v odstavci 2!


Toto je odstavec 3.

<!-- Chtěli jste někdy vložit znak <br /> tag? Můžete napsat na konec odstavce
dvě nebo více mezer a potom začít nový odstavec. -->

Tento odstavec končí dvěma mezerami.  

Nad tímto odstavcem je  <br />!

<!-- Blokové citace se dělají jednoduše pomocí znaku >. -->

> Toto je bloková citace. Můžete dokonce
> manuálně rozdělit řádky, a před každý vložit >, nebo nechat vaše řádky jakkoliv dlouhé, ať se zarovnají sami.
> Nedělá to rozdíl, dokud začínáte vždy znakem >.

> Můžu použít více než jednu
>> odsazení?
> Jak je to úhledné, že?

<!-- Seznamy -->
<!-- Nečíslovaný seznam můžete jednoduše udělat pomocí hvězdiček, plusů, nebo
 pomlček -->

* Položka
* Položka
* Jinná položka

nebo

+ Položka
+ Položka
+ Další položka

nebo

- Položka
- Položka
- Další položka

<!-- Číslovaný seznam se dělají pomocí čísla a . -->

1. Položka jedna
2. Položka dvě
3. Položka tři

<!-- Nemusíte dokonce psát čísla správně a markdown je zobrazi správně,
     ale nemusí to být vždy dobrý nápad -->

1. Položka jedna
1. Položka dvě
1. Položka tři
<!-- (Toto zobrazí to samě, jako příklad nadtím.) -->

<!-- Můžete také tvořit podseznamy -->

1. Položka jedna
2. Položka dvě
3. Položka tři
    * Podpoložka
    * Podpoložka
4. Položka čtyři

<!-- Existují i zašktávací seznamy. Toto vytvoří HTML checkboxy. -->

Boxy níže bez 'x' jsou nezašktrnuté checkboxy.
- [ ] První úkol
- [ ] Druhý úkol
Tento box bude zašktrnutý
- [x] Tento úkol byl dokončen

<!-- Bloky ködu -->
<!-- Můžete označit kód bloku (který používá <code> element) odsazením pomocí 4
     mezer, nebo tabu -->

    Toto je kód
    Stejně jako toto

<!-- Můžete dokonce přidat další 4 mezery nebo tab pro další odsazení -->

    moje_pole.each do |i|
        puts i
    end

<!-- Kód na řádku může být označen pomocí zpětných apostrofů ` -->

Jan nevědel, jak se dělá `go_to()` funkce!

<!-- V Markdownu od GitHubu , můžete použít speciální syntaxi pro kód -->

\`\`\`ruby <!-- vyjma zpětných lomítek, jenom ```ruby ! -->
def neco
    puts "Ahoj světe!"
end
\`\`\` <!-- zde taky, žádné zpětná lomítka, pouze ``` -->

<!-- Text výše nepotřebuje odsazení a navíc GitHub použije zvýraznění označeného
 jazyka. -->

<!-- Horizontální čára (<hr />) -->
<!-- Horizontální čára se jednoduše přidá pomocí 3 nebo více hvězdiček nebo pomlček
s nebo bez mezer. -->

***
---
- - -
****************

<!-- Odkazy -->
<!-- Jedna z nejlepších věcí na Markdownu je, jak jednoduše se dělají odkazy.
Dejte text, který chcete zobrazit, do [] následovaný url v závorkách () a je to. -->

[Klikni na mě!](http://test.com/)

<!-- Můžete také přidat jméno linku pomocí uvozovek -->

[Klikni na mě!](http://test.com/ "Odkaz na Test.com")

<!-- Relativní cesty fungují taky -->

[Jdi na hudbu](/hudba/).

<!-- Markdown taktéž podporuje reference odkazů. -->

[Klikni na tento odkaz][link1] pro více informací!
[Taky zkontrolujte tento odkaz][neco], když chcete.

[link1]: http://test.com/ "Cool!"
[neco]: http://neco.czz/ "Dobře!"

<!-- Titulek může být v apostrofech nebo závorkách, nebo vyjmutý úplně. Reference
 může být kdekoliv ve vašem dokumentu a identifikátor může být jakýkoliv, dokud
 je unikátní.-->

<!-- Také existuje "implicitní pojmenování", které použije text jako id -->

[Toto][] je odkaz..

[toto]: http://totojelink.cz/

<!-- Ale toto není zrovna běžné užívané. -->

<!-- Obrázky -->
<!-- Obrázky se dělají stejně jako odkazy, ale s vykřičníkem na začátku -->

![Toto je atribut alt pro obrázek](http://imgur.com/myimage.jpg "Nepovinný titulek")

<!-- Reference fungují, jak bychom čekali-->

![Toto je atribut alt][mujobrazek]

[mujobrazek]: relativni/cesta/obrazek.jpg "a toto by byl titulek"

<!-- Ostatní -->
<!-- Automatické odkazy -->

<http://stranka.cz/> je stejná jako
[http://stranka.cz/](http://stranka.cz/)

<!-- Automatické odkazy pro emaily-->

<jmeno@prijmeni.cz>

<!-- Escapování znaků -->

Chci napsat *tento text obklopený hvězdičkami*, ale nechci aby to bylo kurzívou, tak udělám: \*tento text obklopený hvězdičkami\*.

<!-- Klávesové zkratky -->
<!-- V Markdownu od GitHubu, můžete použít tag <kbd> k reprezentování klaves na počítači -->

Váš počítač přestal pracovat? Zkuste
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>

<!-- Tabulky -->
<!-- Tabulky jsou povolené pouze v Markdownu od GitHubu a jsou trochu podivně,
     ale když je opravdu chcete: -->

| Sloupec1     | Sloupec2 | Sloupec3      |
| :----------- | :------: | ------------: |
| Vlevo zarovn.| Na střed | Vpravo zarovn.|
| blah         | blah     | blah          |

<!-- nebo, to jde i taky: -->

Sloupec 1 | Sloupec2 | Sloupec3
:-- | :-: | --:
Ohh toto je tak ošklivé | radši to | nedělejte

<!-- Konec -->

```

Pro více informací, prozkoumejte oficiální článek o syntaxi od Johna Grubera
 [zde](http://daringfireball.net/projects/markdown/syntax) a skvělý tahák od Adama Pritcharda [zde](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
