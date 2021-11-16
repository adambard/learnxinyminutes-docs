---
language: markdown
lang: cs-cz
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Michal Martinek", "https://github.com/MichalMartinek"]
    - ["Tomáš Hartman", "https://github.com/tomas-hartman"]
filename: markdown-cz.md
lang: cs-cz
---

Markdown byl vytvořen Johnem Gruberem v roce 2004 jako značkovací jazyk, který
by šlo snadno číst a psát a který by bylo možné jednoduše převést do HTML
(a dnes i do mnoha dalších formátů).

Implementace markdownu se v různých parserech (syntaktických analyzátorech,
které markdown dále zpracovávají) mírně odlišuje. V této příručce se budeme
snažit upřesnit, kdy se jedná o obecnou vlastnost markdownu a kdy se jedná o
specifickou vlastnost daného parseru.

- [HTML Elementy](#html-elementy)
- [Nadpisy](#nadpisy)
- [Jednoduché stylování textu](#jednoduché-stylování-textu)
- [Odstavce](#odstavce)
  - [Blokové citace](#blokové-citace)
- [Seznamy](#seznamy)
- [Bloky kódu](#bloky-kódu)
- [Vodorovná čára (`<hr />`)](#vodorovná-čára-hr-)
- [Odkazy](#odkazy)
  - [Obsahy](#obsahy)
- [Obrázky](#obrázky)
- [Ostatní](#ostatní)
  - [Automatické odkazy](#automatické-odkazy)
  - [Automatické odkazy z emailů](#automatické-odkazy-z-emailů)
  - [Escapování znaků](#escapování-znaků)
  - [Klávesové zkratky](#klávesové-zkratky)
  - [Tabulky](#tabulky)
- [Markdownlint](#markdownlint)

## HTML Elementy

Markdown je nadstavba HTML. To znamená, že každý HTML kód je zároveň validním
kódem v Markdownu.

```md
<!-- To znamená, že v Markdownu můžeme používat HTML elementy jako například 
komentáře, které nebudou ovlivněny parserem Markdownu. Na druhou stranu to také 
znamená, že pokud ve svém Markdown kódu vytvoříte HTML element, už v rámci 
tohoto elementu nelze použít Markdown. 

Markdown využívá i tato stránka, a tak kdyby tento text nebyl obalen v bloku 
kódu (viz níže), jako komentář by vůbec nebyl vidět. -->
```

## Nadpisy

HTML elementy `<h1>` až `<h6>` vytvoříte jednoduše tak, že nadpisu předsadíte
takový počet křížků (#), jaký odpovídá úrovni nadpisu.

```md
# Toto je <h1>
## Toto je <h2>
### Toto je <h3>
#### Toto je <h4>
##### Toto je <h5>
###### Toto je <h6>
```

Markdown obsahuje ještě dva další způsoby, jak vytvořit h1 a h2:

```md
Toto je h1
==========

Toto je h2
----------
```

## Jednoduché stylování textu

Pomocí markdownu můžete text jednoduše označit jako kurzívu či tučný.

```md
*Tento text je kurzívou;*
_Stejně jako tento._

**Tento text je tučně**
__Stejně jako tento.__

***Tento text je obojí***
**_Tak jako tento!_**
*__Nebo tento!__*
```

Ve verzi Markdownu od GitHubu, máme k dispozici taky přeškrtnutí:

```md
~~Tento text je přeškrtlý.~~
```

## Odstavce

Odstavce tvoří jeden nebo více řádků textu, oddělených jedním nebo více
prázdnými řádky.

```md
Toto je odstavec. Zde jsem napsal odstavec, a je to bezva!

Teď jsem v odstavci 2.
Pořád jsem v odstavci 2!

A tady už je odstavec 3.
```

Pokud byste chtěli vložit HTML element `<br />`, můžete na konec odstavce napsat
dvě nebo více mezer a potom začít nový odstavec.

```md
Tento odstavec končí dvěma mezerami.  

Nad tímto odstavcem je <br />!
```

### Blokové citace

Blokové citace se dělají jednoduše pomocí znaku >.

```md
> Toto je bloková citace. Můžete dokonce
> manuálně rozdělit řádky, a před každý vložit >, nebo nechat vaše řádky 
> jakkoli dlouhé, ať se zarovnají sami.
> Je to jedno, pokud vždy začinají symbolem `>`.

> Použít můžu i více než jednu úroveň
>> odsazení?
> Co vy na to?
```

## Seznamy

Nečíslovaný seznam můžete jednoduše udělat pomocí hvězdiček, plusů, nebo pomlček:

```md
* Položka
* Položka
* Jiná položka

nebo

+ Položka
+ Položka
+ Další položka

nebo

- Položka
- Položka
- Další položka
```

Číslované seznamy se dělají pomocí číslice a `.`.

```md
1. Položka jedna
2. Položka dvě
3. Položka tři

<!-- Čísla ani nemusíte psát popořadě. Markdown je umí zobrazit správně, jenom 
je třeba vždy překontrolovat, že číslování funguje správně. -->

1. Položka jedna
1. Položka dvě
1. Položka tři

<!-- (Toto zobrazí to samě, jako příklad nadtím.) -->
```

Můžete také tvořit podseznamy:

```md
1. Položka jedna
2. Položka dvě
3. Položka tři
    - Podpoložka
    - Podpoložka
4. Položka čtyři
```

Vytvořit lze i zaškrtávací seznamy. Toto vytvoří seznam s HTML checkboxy. (Boxy
níže bez 'x' jsou nezašktrnuté checkboxy.)

```md
- [ ] První úkol, který je třeba dokoncič
- [ ] Druhý úkol na dodělání
Tento box bude zašktrnutý
- [x] Tento úkol byl dokončen
```

## Bloky kódu

Bloky kódu můžete označit tak, že řádek odsadíte čtyřmi mezerami nebo pomocí
tabu. Pro interpretaci kódu parser používá `<code>` element.

```md
    Toto je kód
    Stejně jako toto
```

Pro ještě hlubší odsazení můžete přidat další 4 mezery nebo další tab:

```md
    moje_pole.each do |i|
        puts i
    end
```

Jednořádkový kód můžete zabalit do dvou zpětných apostrofů (`) podobně, jako
kdybyste text normálně stylovali:

```md
Honza neměl tušení, co dělá funkce `go_to()`!
```

V Markdownu od GitHubu, můžete použít speciální syntaxi pro kód:

<pre>
<code class="highlight">&#x60;&#x60;&#x60;ruby
def neco
    puts "Ahoj světe!"
end
&#x60;&#x60;&#x60;
</code>
</pre>

Text výše nepotřebuje čtyřmezerové odsazení a parser navíc použije zvýraznění
syntaxe pro zvolený jazyk.

## Vodorovná čára (`<hr />`)

Vodorovnou oddělovací čáru lze snadno přidat pomocí 3 nebo více hvězdiček (nebo
pomlček), a to buť s mezerami mezi jednotlivými znaky nebo bez nich.

```md
***
---
- - -
****************
```

## Odkazy

```md
<!-- Jedna z nejlepších vlastností Markdownu je, jak snadno lze s jeho pomocí 
vytvářet odkazy. Text odkazu, který chcete zobrazit, vložte do [], a hned za něj 
v kulatých závorkách () připojte url adresu. -->

[Klikni na mě!](http://test.com/)


<!-- V uvozovkách za url můžete přidat název linku -->

[Klikni na mě!](http://test.com/ "Odkaz na Test.com")


<!-- Relativní cesty fungují také -->

[Jdi na hudbu](/hudba/).


<!-- Markdown taktéž podporuje referenční odkazy. -->

[Klikni na tento odkaz][link1] pro více informací!
[Taky zkontrolujte tento odkaz][neco], jestli teda chcete.

[link1]: http://test.com/ "Cool!"
[neco]: http://neco.czz/ "Dobře!"


<!-- Titulek v tomto případě může být v jednoduchých uvozovkách, závorkách, nebo 
zcela vynechaný. Reference může být kdekoliv ve vašem dokumentu a identifikátory 
mohou být jakékoli, pokud jsou unikátní.

V markdownu existuje rovněž "implicitní pojmenování", které použije text odkazu 
jako své id -->

[Toto][] je odkaz..

[toto]: http://totojelink.cz/

<!-- Ale tento způsob se obvykle nepoužítá. -->
```

### Obsahy

Kombinace seznamů, odkazů a nadpisů využívají také některé parsery pro
generování obsahu Markdown souborů. Jako identifikátory slouží jména nadpisů
psané malými písmeny, které jsou uvozené křížkem (`#`). Víceslovné nadpisy
ývají propojeny pomlčkou (`-`), která někdy nahrazuje i speciální znaky (jiné
speciální znaky mohou být vynechány).

```md
- [Nadpis](#nadpis)
- [Víceslovný text](#víceslovný-text)
- [Odstavce](#odstavce)
  - [Podkapitola <h3 />](#podkapitola-h3-)
```

V případě obsahů se v každém případě jedná o nadstavbu, která nemusí vždy
fungovat stoprocentně.

## Obrázky

```md
<!-- Obrázky se vytváří stejně jako odkazy, ale s vykřičníkem na začátku -->

![Toto je atribut alt pro obrázek](http://imgur.com/myimage.jpg "Nepovinný titulek")

<!-- Reference fungují tak, jak bychom čekali-->

![Toto je atribut alt][mujobrazek]

[mujobrazek]: relativni/cesta/obrazek.jpg "a toto by byl titulek"
```

## Ostatní

### Automatické odkazy

```md
<http://stranka.cz/> je stejné jako
[http://stranka.cz/](http://stranka.cz/)
```

### Automatické odkazy z emailů

```md
<jmeno@prijmeni.cz>
```

###  Escapování znaků

```md
Chci napsat *tento text obklopený hvězdičkami*, ale nechci aby to bylo kurzívou, 
tak udělám: \*tento text bude obklopený hvězdičkami\*.
```

### Klávesové zkratky

```md
<!-- V Markdownu od GitHubu, můžete použít tag <kbd> k reprezentování klaves na 
počítači -->

Váš počítač přestal pracovat? Zkuste
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```

### Tabulky

Tabulky lze využít pouze v Markdownu od GitHubu a jejich syntax je trošku
zvláštní. Pokud ale trváte na to, že je chcete použít, tady to máte:

```md
| Sloupec1     | Sloupec2 | Sloupec3      |
| :----------- | :------: | ------------: |
| Vlevo zarovn.| Na střed | Vpravo zarovn.|
| blah         | blah     | blah          |

<!-- nebo to jde taky udělat takto: -->

Sloupec 1 | Sloupec2 | Sloupec3
:-- | :-: | --:
Že se to nedá číst? | Tak takhle to | radši nedělejte.
```

## Markdownlint

Pro usnadnění práce s Markdownem a s cílem sjednotit styl psaní jeho kódu vznikl
nástroj `Markdownlint`. Tento nástroj je dostupný i jako plugin pro některé
editory kódu (IDE) a lze jej použít jako nástroj pro vytváření a ověřování
validity a čitelnosti Markdownu kódu.

---

> _Pozn. překladatele:_ Tento text vznikl jako překlad původního článku, který
> vznikl v roce 2013, který byl po obsahové stránce naposledy editován v roce
> 2015 a kombinace původního českého překladu z roku 2015. Některé informace v
> tomto článku, zejména ty, týkající se specifických vlastnostní parserů
> markdownu tak již dnes mohou být zasrtaralé.
>
> Za účelem aktualizace tohoto článku jsem přidal kapitoly o
> [generování obsahů](#obsahy), které mj. využívá i tento článek a o
> [Markdownlintu](#markdownlint).

---

Pro více informací doporučujeme oficiální článek o syntaxi od Johna Grubera
 [zde](http://daringfireball.net/projects/markdown/syntax) a skvělý tahák od
 Adama Pritcharda [zde](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
