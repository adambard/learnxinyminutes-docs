---
language: markdown
filename: markdown-fi.md
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Timo Virkkunen", "https://github.com/ComSecNinja"]
lang: fi-fi
---

John Gruber loi Markdownin vuona 2004. Sen tarkoitus on olla helposti luettava ja kirjoitettava syntaksi joka muuntuu helposti HTML:ksi (ja nyt myös moneksi muuksi formaatiksi).

```md
<!-- Jokainen HTML-tiedosto on pätevää Markdownia. Tämä tarkoittaa että voimme
käyttää HTML-elementtejä Markdownissa, kuten kommentteja, ilman että markdown
-jäsennin vaikuttaa niihin. Tästä johtuen et voi kuitenkaan käyttää markdownia
HTML-elementtien sisällä jos luot sellaisen markdown-tiedostoon. -->

<!-- Markdownin toteutus vaihtelee jäsentimestä toiseen. Tämä opas yrittää
selventää mitkä ominaisuudet ovat yleisiä ja mitkä ovat eritysesti tiettyjen
jäsentimien ominaisuuksia. -->

<!-- Otsikot -->
<!-- Voit luoda HTML-elementtejä <h1> - <h6> helposti aloittamalla rivin
haluamallasi määrällä ristikkomerkkejä (#).  -->
# Tämä on <h1>
## Tämä on <h2>
### Tämä on <h3>
#### Tämä on <h4>
##### Tämä on <h5>
###### Tämä on <h6>

<!-- Markdownissa on myös vaihtoehtoisia tapoja ilmaista h1 ja h2. -->
Tämä on h1
=============

Tämä on h2
-------------

<!-- Yksinkertaiset tekstimuotoilut -->
<!-- Tekstin voi helposti muotoilla kursiiviksi tai lihavoiduksi. -->

*Tämä teksti on kursivoitua.*
_Kuten on myös tämä teksti._

**Tämä teksti on lihavoitua.**
__Kuten on tämäkin teksti.__

***Tämä teksti on molempia.***
**_Kuten tämäkin!_**
*__Kuten tämäkin!__*

<!-- GitHub-tyylisessä Markdownissa, jota käytetään tiedostojen esittämiseksi
GitHubissa, meillä on käytössämme myös yliviivaus: -->

~~Tämä teksti on yliviivattua.~~

<!-- Kappaleet ovat yhdellä tai useammalla peräkkäisellä tekstirivillä jotka
erotellaan yhdellä tai useammalla tyhjällä rivillä -->

Tämä on kappala. Kirjoittelen kappaleeseen, eikö tämä olekin hauskaa?

Nyt olen kappaleessa 2.
Olen edelleen toisessa kappaleessa!


Olen kolmannessa kappaleessa!

<!-- Jos haluat lisätä <br /> HTML-elementin, päätä kappale kahdella tai
useammalla välilyönnillä ja aloita sitten uusi kappale -->

Päätän tämän kahteen välilyöntiin (maalaa minut nähdäksesi ne).  

There's a <br /> above me!

<!-- Lainaukset ovat helppoja ja ne tehdään >-merkillä -->

> Tämä on lainaus. Voit joko
> manuaalisesti rivittää tekstisi ja laittaa >-merkin jokaisen rivin eteen tai antaa jäsentimen rivittää pitkät tekstirivit.
> Sillä ei ole merkitystä kunhan rivit alkavat >-merkillä.

> Voit myös käyttää useampaa
>> sisennystasoa
> Kuinka hienoa se on?

<!-- Listat -->
<!-- Järjestämättömät listat tehdään asteriskilla, plussalla tai viivalla -->

* Kohta
* Kohta
* Kolmas kohta

tai

+ Kohta
+ Kohta
+ Kolmas kohta

tai

- Kohta
- Kohta
- Kolmas kohta

<!-- Järjestetyt listat tehdään järjestysluvuilla. -->

1. Kohta yksi
2. Kohta kaksi
3. Kohta kolme

<!-- Sinun ei tarvitse edes merkitä kohtia oikein ja silti markdown näyttää
oikean järjestyksen, mutta se ei välttämättä ole hyvä idea. -->

1. Kohta yksi
1. Kohta kaksi
1. Kohta kolme
<!-- (Tämä korjaantuu samanlaiseksi kuin yllä oleva esimerkki) -->

<!-- Voit myös käyttää alalistoja. -->

1. Kohta yksi
2. Kohta kaksi
3. Kohta kolme
    * Alakohta
    * Alakohta
4. Kohta neljä

<!-- Myös tehtävälistoja on olemassa. Tämä tekee HTML-valintaruutuja. -->

Alla olevat ruudut ilman x-merkkiä ovat merkitsemättömiä HTML-valintaruutuja.
- [ ] Ensimmäinen suoritettava tehtävä.
- [ ] Toinen tehtävä joka täytyy tehdä
Tämä alla oleva ruutu on merkitty HTML-valintaruutu.
- [x] Tämä tehtävä on suoritettu

<!-- Koodiosiot -->
<!-- Voit merkitä koodiosion (jaka käyttää <code> -elementtiä) sisentämällä
rivin neljällä välilyönnillä tai tabulaattorilla. -->

    Tämä on koodia
    Kuten tämäkin

<!-- Voit myös sisentää koodia samalla tavalla. -->

    my_array.each do |item|
        puts item
    end

<!-- Muun tekstin seassa oleva koodi merkitään kahden `-merkin väliin -->

John ei tiennyt edes mitä `go_to()` -funktio teki!

<!-- GitHubin Markdownissa voit käyttää erityissyntaksia koodille. -->

\`\`\`ruby <!-- paitsi että poista nuo kenoviivat, vain ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- tästä myös, ei kenoviivoja, vain ``` -->

<!-- Yllä oleva teksti ei vaadi sisennystä. Lisäksi GitHub käyttää ``` jälkeen
mainitsemasi kielen syntaksin korostusta  -->

<!-- Vaakaviiva (<hr />) -->
<!-- Vaakaviivojen lisääminen käy näppärästi kolmella tai useammalla
asteriskilla taikka viivalla, välilyönneillä tai ilman -->

***
---
- - -
****************

<!-- Linkit -->
<!-- yksi markdownin parhaita ominaisuuksia on yksinkertaiset hyperlinkit. Laita
näytettävä teksti hakasulkuihin [] ja URL-osoite perään sulkeissa (). -->

[Klikkaa tästä!](http://example.com/)

<!-- Voit myös lisätä linkin otsikon heittomerkeissä osoitteen perään. -->

[Klikkaa tästä!](http://example.com/ "Linkki Example.com:iin")

<!-- Suhteelliset polut toimivat myös. -->

[Musiikkia](/musiikki/).

<!-- Markdown tukee myös viittaustyylisiä linkkejä. -->

[Klikkaa tätä linkkiä][link1] saadaksesi lisätietoja!
[Katso myös tämä linkki][foobar] jos haluat.

[link1]: http://example.com/ "Siistii!"
[foobar]: http://foobar.biz/ "Selkis!"

<!-- Otsikko voi olla myös ykittäisissä heittomerkeissä tai sulkeissa, tai
ohitettu kokonaan. Viittaukset voivat olla missä tahansa kohdassa dokumenttia ja
viittausten ID:t voivat olla mitä tahansa kunhan ne ovat uniikkeja. -->

<!-- Voit myös käyttää linkin tekstiä ID:nä näin: -->

[This][] is a link.

[this]: http://tämäonlinkki.com/

<!-- Mutta tämä tapa ei ole yleinen. -->

<!-- Kuvat -->
<!-- Kuvat tehdään samalla tavalla kuin linkitkin, mutta huutomerkki edessä! -->

![Kuvan alt-attribuutti](http://imgur.com/munkuva.jpg "Vaihtoehtoinen otsikko")

<!-- Ja viittaukset toimivat odotetusti. -->

![Tämä on se alt-attribuutti][munkuva]

[munkuva]: suhteellinen/polku/siitii/kuva.jpg "otsikko tähän tarvittaessa"

<!-- Sekalaista -->
<!-- Automaattiset linkit -->

<http://testwebsite.com/> on sama kuin
[http://testwebsite.com/](http://testwebsite.com/)

<!-- Automaattiset sähköpostilinkit -->

<foo@bar.com>

<!-- Varattujen merkkien käyttö -->

haluan kirjoittaa *tämän tekstin jonka ympärillä on asteriskit* mutta en halua
sen kursivoituvan, joten teen näin: \*tämän tekstin ympärillä on asteriskit\*.

<!-- Näppäimistön näppäimet -->
<!-- GitHubin Markdownissa, voit käyttää <kbd> -tagia esittämään näppäimiä -->

Tietokoneesi kaatui? Kokeile painaa
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>

<!-- Taulukot -->
<!-- Taulukot ovat saatavilla vain GitHubin markdownissa ja ne ovat melko
vaivalloisia käyttää, mutta jos todella haluat: -->

| Kolumni1     | Kolumni2 | Kolumni3      |
| :----------- | :------: | ------------: |
| Vasemmalle   | Keskelle | Oikealle      |
| blaa         | blaa     | blaa          |

<!-- vaihtoehtoisesti, sama tulos -->

Kolumni 1 | Kolumni 2 | Kolumni 3
:-- | :-: | --:
Hyi tämä on ruma | saa se | loppumaan

<!-- Loppu! -->

```

Lisää tietoa löydät John Gruberin [virallisesta julkaisusta](http://daringfireball.net/projects/markdown/syntax)
ja Adam Pritchardin loistavasta [lunttilapusta](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
