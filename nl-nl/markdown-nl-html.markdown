---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
filename: learnmarkdown-nl.md
translators:
  - ["Niels van Velzen", "https://nielsvanvelzen.me"]
lang: nl-nl
---

Markdown is gemaakt door John Gruber in 2004. De bedoeling was om een simpel te
lezen en schrijven syntax te creëren wat makkelijk om te zetten is naar
HTML (en tegenwoordig ook vele andere formaten).

```markdown
<!-- Markdown is een superset van HTML, ofterwijl: een HTML bestand is
geldige Markdown code. Daardoor kunnen we dus HTML elementen in Markdown gebruiken.
Let wel op dat je geen markdown syntax in een HTML element kan gebruiken. -->

<!-- Markdown verschilt ook in implementatie tussen verschillende parsers. In dit
artikel wordt aangegeven of iets overal werkt of alleen bij specifieke implementaties -->

<!-- Koppen -->
<!-- Je kan HTML koppen (<h1 - 6>) makkelijk maken met markdown door
een aantal hekjes te gebruiken (#) -->
# Dit is een <h1>
## Dit is een <h2>
### Dit is een <h3>
#### Dit is een <h4>
##### Dit is een <h5>
###### Dit is een <h6>

<!-- Voor h1 en h2 zijn er ook nog alternatieve methodes -->
Dit is een h1
=============

Dit is een h2
-------------

<!-- Simpele text stijlen -->
<!-- Tekst kan makkelijk vet of shuin gedrukt worden -->

*Deze tekst staat schuin.*
_Net als deze tekst._

**Deze tekst is dikgedrukt.**
__Net als deze tekst.__

***En deze tekst is dik en schuin.***
**_Net als dit!_**
*__En dit!__*

<!-- In Github's markdown stijl is er ook een doorhaal mogelijkheid -->

~~Deze tekst is doorgehaald.~~

<!-- Paragraphs are a one or multiple adjacent lines of text separated by one or
multiple blank lines. -->
<!-- Paragrafen zijn een of meerdere aan elkaar lopende lijnen gescheiden door
een of meerdere lege lijnen -->

Dit is een paragraaf. Ik typ in een paragraaf, is dat niet leuk?

Nu ben ik in paragraaf 2.
Nog steeds in paragraaf 2!

Dit is paragraaf drie!

<!-- Wil je een nieuwe regel starten zonder de <br /> tag van HTML te gebruiken?
Je kunt een paragraaf eindigen met twee of meer spaties en dan een nieuwe paragraaf
beginnen -->

Ik eindig met twee spaties (selecteer mij om het te zien).  

Er is een nieuwe regel boven mij!

<!-- Citaten zijn makkelijk en worden gemaakt met het >. -->

> Dit is een citaat. Je kan
> handmatig je lijnen laten vormen of je kan je lijnen lang laten worden en vanzelf op nieuwe regels verder laten gaan.
> Het maakt niet uit zolang je maar begint met een `>`.

> Je kunt ook meer dan 1 level
>> uitlijning gebruiken.
> Hoe handig is dat?

<!-- Lijsten -->
<!-- Ongesorteerde lijsten kanje maken met sterretjes, plussen of koppeltekens -->

* Item
* Item
* Nog een item

of

+ Item
+ Item
+ Nog een item

of

- Item
- Item
- Nog een item

<!-- Geordende lijsten maak je met een nummer gevolgt door een punt -->

1. Item een
2. Item twee
3. Item drie

<!-- Je hoeft niet een correct getal te gebruiken en markdown zal nogsteeds werken,
ondanks dat het werkt is het niet aangeraden -->

1. Item een
1. Item twee
1. Item drie

<!-- (Dit is getzelfde als het voorbeeld hierboven) -->

<!-- Je kan ook lijsten in lijsten gebruiken -->

1. Item een
2. Item twee
3. Item drie
    * Sub-item een
    * Sub-item twee
4. Item vier

<!-- Er zijn ook taken lijsten. Hier worden HTML checkboxes gebruikt -->

Checkboxes hieronder zonder de 'x' zijn leeg.
- [ ] Eerste taak.
- [ ] Tweede taak
Checkboxes met een 'x' zijn klaar.
- [x] Deze taak is klaar.

<!-- Code blokken -->
<!-- Je kunt een code blok maken door te inspringen met vier spaties fo een tab --> 

    Dit is code
    net als dit

<!-- Je kan ook extra tabs (of spaties) gebruiken voor het inspringen in je code -->

    mijn_array.each do |item|
        puts item
    end

<!-- Inline code kan worden gemaakt met het backtick karakter  ` -->

John wist niet eens wat de `go_to()` functie deed!

<!-- In Github's markdown stijl is er een speciale syntax voor code -->

\`\`\`ruby <!-- alleen moet je de backslashes weghalen als je dit doet, gewoon ```ruby dus! -->
def foobar
    puts "Hallo wereld!"
end
\`\`\` <!-- hier ook, geen backslashes ``` -->

<!-- Voor de bovenstaande syntax hoef je niet te inspringen. Daarnaast kan Github "syntax highlighting" geven. -->

<!-- Horizontale regel (<hr />) -->
<!-- Horizontale regel zijn makkelijk toegevoegd met drie of meer sterretjes of
koppeltekens, met of zonder spaties. -->

***
---
- - -
****************

<!-- Links -->
<!-- Een van de beste dingen van markdown is het maken van links.
De tekst om te weergeven plaats je tussen harde brackets [] gevolgd door de link tussen haakjes. -->

[Klik op mij!](http://test.com/)

<!-- Je kunt ook een titel aan de link defineren met quotes -->

[Klik op mij!](http://test.com/ "Link naar Test.com")

<!-- Relative paden werken ook. -->

[Ga naar de muziek](/music/).

<!-- Markdown support ook ankers -->

[Klik op deze link][link1] vor meer informatie erover!
[Bekijk deze ook eens][foobar] als je wilt.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "In orde!"

<!-- De titel kan ook in enkele quotes, tussen haakjes of gewoon zonder iets.
De refferentie kan overal in het document geplaats worden en zo lang als je wilt.
Zolang het maar uniek blijft -->

<!-- Er is ook "impliciete naamgeving" wat de tekst en de link hetzelfde maakt -->

[Dit][] is een link.

[dit]: http://ditiseenlink.nl/

<!-- Dat is wel weinig gebruikt. -->

<!-- Plaatjes -->
<!-- Plaatjes werken nar als links maar dan met een uitroepteken -->

![Dit is de alt attribuut van het plaatje](http://imgur.com/myimage.jpg "Een optionele titel")

<!-- En refferentie stijl werkt zoals verwacht -->

![Dit is de alt attribuut.][mijnplaatje]

[mijnplaatje]: relative/urls/cool/image.jpg "Hier is de titel"

<!-- Diverse -->
<!-- Auto-links -->

<http://testwebsite.com/> is hetzelfde als
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-links voor emails -->

<foo@bar.com>

<!-- Escaping -->

Ik wil *deze tekst met sterretjes typen* maar ik wil het niet schuin, dus ik doe dit: \*deze tekst met sterretjes typen\*.

<!-- Toetsenbord toetsen -->
<!-- In Github's markdown stijl kan je de <kbd> tag gebruiken -->

Computer gecrashed? Gebruik eens 
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>

<!-- Tabellen -->
<!-- Tabellen werken alleen in Github's stijl: -->

| kolom1           | kolom2      |kolom3             |
| :--------------- | :---------: | ----------------: |
| Links-uitgelijnd | Gecentreerd | Rechts-uitgelijnd |
| blah             | blah        | blah              |

<!-- of, voor dezelfde resultaten -->

Kolom 1 | Kolom 2 | Kolom 3
:-- | :-: | --:
Dit is zo lelijk | stop | er mee

<!-- Einde! -->

```

Voor meer informatie, bekijk Josn Gruber's officiele post over de syntax [hier](http://daringfireball.net/projects/markdown/syntax). Of bekijk Adam Pritchard's grote cheatsheet [hier](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) (beide Engels).
