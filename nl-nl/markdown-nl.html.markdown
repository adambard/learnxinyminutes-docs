---
language: markdown
filename: markdown-nl.md
contributors:
  - ["Dan Turkel", "http://danturkel.com/"]
translators:
  - ["Jeroen Deviaene", "https://www.github.com/jerodev"]
lang: nl-nl
---

Markdown is gecreëerd door John Gruber in 2004. Het is bedoeld om met een gemakkelijke te lezen en 
schrijven syntax te zijn die gemakkelijk omgevormd kan worden naar HTML (en op heden verschillende
andere formaten)

```markdown
<!-- Markdown erft over van HTML, dus ieder HTML bestand is een geldig Markdown
bestand. Dit betekend ook dat html elementen gebruikt kunnen worden in Markdown
zoals het commentaar element. Echter, als je een html element maakt in een Markdown 
document kan je geen markdown gebruiken voor de waarde van dat element. -->

<!-- Markdown varieert ook van implementatie tot implementatie en per parser. Deze
tutorial zal de universele functies van Markdown -->

<!-- Headers -->
<!-- Je kan de HTML elementen <h1> tot <h6> gemakkelijk maken door voor de titel
een aantal hashes (#) te plaatsen gelijk aan het level van de header.
# Dit is een <h1>
## Dit is een <h2>
### Dit is een <h3>
#### Dit is een <h4>
##### Dit is een <h5>
###### Dit is een <h6>

<!-- Markdown heeft ook een alternatieve manier om h1 en h2 te maken -->
Dit is een h1
=============

Dit is een h2
-------------

<!-- Simpele tekst stijlen -->
<!-- Tekst kan heel gemakelijk gestyled worden cursief of bold met markdown -->

*Deze tekst is cursief*
_Deze tekst ook_

**Deze tekst is vet gedrukt**
__En deze tekst ook!__

***Deze tekst is zowel bold als schuin gedrukt***
**_Deze ook!_**
*__En zelfs deze!__*

<!-- In de github versie van markdown, die gebruikt wordt om markdown te renderen
op Github, is er ook doorstrepen -->

~~Deze tekst wordt doorstreept.~~

<!-- Paragrafen worden onderscheiden door een of meerdere lege lijnen -->

Dit is een paragraaf. 

Dit is paragraaf 2.
Dit is nog steeds paragraaf 2!


Hallo, ik ben paragraaf 3.

<!-- Citaten zijn gemakkelijk te maken met het '>' karakter. -->

> Dit is een citaat. Je kan alle lijnen manueel starten met een '>'.
> Of je kan de lijn heel heel, heel, lang laten worden zodat de parser deze automatisch zal afbreken en op een nieuwe lijn plaatsen.
> Het maakt niet uit, zolang je start met een '>'.

> Je kan ook in niveaus werken
>> Niveau 2
> Hoe leuk is dat?

<!-- Lijsten -->
<!-- Niet geordende lijsten kunnen gemaakt worden met sterretjes, plussen of streepjes -->

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
- Een laatste item

<!-- Geordende lijsten kunnen gemaakt wroden met cijfers -->

1. Item een
2. Item twee
3. Item drie

<!-- Het getal moet zelfs niet overeen komen met het item in de lijst, markdown zal
automatisch de nummers aanpassen -->

1. Item een
1. Item twe
1. Item drie
<!-- (De output is gelijk aan de vorige lijst) -->

<!-- Je kan ook gebruik maken van sub lijsten -->

1. Item een
2. Item twee
3. Item drie
    * Sub-item
    * Sub-item
4. Item vier

<!-- Er zijn zelfs todo lijsten. Dit genereert HTML checkboxen. -->

Boxen zonder een 'x' zijn niet aangevinkt
- [ ] Eerste to-do item.
- [ ] Tweede to-do item
Dit item zal aangevinkt zijn in de gerenderde html.
- [x] Deze taak is uitgevoerd

<!-- Code blokken -->
<!-- Een code block kan aangeduid worden door vier spaties of een tab -->

    Dit is code
    En dit ook

<!-- Extra tabs kunnen gebruikt worden om tabs in de code aan te geven -->

    my_array.each do |item|
        puts item
    end

<!-- Inline code kan aangeduid worden met ` -->

John wist zelfs niet dat de `go_to()` functie bestond!

<!-- In Github Markdown kan je een speciale syntax gebruiken die aangeeft welke
taal gebruikt wordt in het code blok. -->

\`\`\`ruby <!-- Wis de backslashes om dit te doen, juist ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- Hier ook, geen backslashes, juist ``` -->

<!-- Voor bovenstaande tekst moet geen tab gebruikt worden. Plus, Github zal syntax
highlighting gebruiken voor deze specifieke taal. Hier, Ruby.

<!-- Horizontale lijn (<hr />) -->
<!-- Horizontale lijnen kunnen gemakkelijk toegevoegd worden door drie of meer
sterretjes, of streepjes te plaatsen -->

***
---
- - -
****************

<!-- Links -->
<!-- Een van de beste dingen in Markdown is hoe simpel het is om links te maken.
plaats de tekst om weer te geven tussen [ en ] gevolgd door de link tussen ( en ) -->

[Klik mij!](http://test.com/)

<!-- Een titel kan ook toegevoegd worden aan de link met aanhalingstekens -->

[Klik mij!](http://test.com/ "Titel voor de link")

<!-- Relative paden werken ook -->

[Naar de muziek](/music/).

<!-- Links kunnen ook gelegd worden met referenties -->

[Klik deze link][link1] voor meer info!
[Beijk ook dit][foobar] als je echt wil.

[link1]: http://test.com/ "Cool!"
[foobar]: http://foobar.biz/ "Tof!"


<!-- Afbeeldingen -->
<!-- Afbeeldingen worden toegevoegd op exact de zelfde manier als links maar met een 
uitroepteken aan het begin van de lijn. -->

![Dit is de alt waarde van een afbeelding](http://imgur.com/myimage.jpg "Optionele titel")

<!-- Referenties werkt ook zals bij links -->

![Dit is de alt waarde][myimage]

[myimage]: relative/urls/cool/image.jpg "als een titel nodig is, staat deze hier"

<!-- Varia -->
<!-- Auto-links -->

<http://testwebsite.com/> is gelijk aan
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Auto-links for emails -->

<foo@bar.com>

<!-- Karakters escapen -->

Als je sterretjes wil gebruiken in je tekst zoals *dit* dan zal dit schuingedrukt weergegeven
worden.
Dit kan je oplossen met backslashes: \*dit\* staat tussen sterretjes

<!-- Toetsenbord toetsen -->
<!-- In Github Markdown, kan je <kbd> gebruiken om toetsenbord toetsen weer te geven -->

Loopt je computer vast? Probeer volgende toetsen combinatie:
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>

<!-- Tabellen -->
<!-- Tabellen zijn momenteel enkel beschikbaar in Github Markdown en zijn redelijk omslachtig.
Maar als je er echt wilt toevoegen: -->

| Col1             | Col2        | Col3              |
| :--------------- | :---------: | ----------------: |
| Links uitgelijnt | Gecentreerd | Rechts uitgelijnt |
| blah             | blah        | blah              |

<!-- of, Voor het zelfde resultaat -->

Col 1 | Col2 | Col3
:-- | :-: | --:
Zeer | Lelijke | Code!

<!-- The end! -->

```

Voor meer info, bekijk de officiële post van John Gruber [hier](http://daringfireball.net/projects/markdown/syntax) en de handige cheatsheet van Adam Pritchard [hier](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
