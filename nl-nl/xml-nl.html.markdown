---
language: xml
filename: learnxml-nl.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
  - ["Rachel Stiyer", "https://github.com/rstiyer"]
  translators:
    - ["Reinoud Kruithof", "https://github.com/reinoudk"]
lang: nl-nl
---

XML is een opmaaktaal ontworpen voor het opslaan en versturen van data.

In tegenstelling tot HTML, specificeert XML niet hoe data getoond of geformateerd moet 
worden, het bevat alleen de data.

* XML Syntax

```xml
<!-- Opmerkingen in XML zien er zo uit -->

<?xml version="1.0" encoding="UTF-8"?>
<boekwinkel>
    <boek categorie="KOKEN">
        <titel taal="nl">Alledaags Italiaans</titel>
        <auteur>Giada De Laurentiis</auteur>
        <jaar>2005</jaar>
        <prijs>30.00</prijs>
    </boek>
    <boek categorie="KINDEREN">
        <titel taal="nl">Harry Potter</titel>
        <auteur>J K. Rowling</auteur>
        <jaar>2005</jaar>
        <prijs>29.99</prijs>
    </boek>
    <boek categorie="WEB">
        <titel taal="nl">XML Leren</titel>
        <auteur>Erik T. Ray</auteur>
        <jaar>2003</jaar>
        <prijs>39.95</prijs>
    </boek>
</boekwinkel>

<!-- Bovenstaande is een typisch XML-bestand.
  Het begint met een declaratie, informerend met (optionele) metadata.

  XML maakt gebruikt van een boomstructuur. Hierboven is de root node (wortel) 'boekwinkel', 
  welke child nodes (kinderen) heeft, allen 'boeken'. Deze child nodes hebben meerdere 
  nodes (of kinderen), en zo voorts...

  Nodes worden gecreëerd met behulp van open-/sluiten-tags, en kinderen zijn gewoon
   nodes tussen de open- en sluiten-tags.-->

<!-- XML bevat twee soorten data:
  1 - Attributen -> Dit is metadata over een node.
      Meestal gebruikt de XML-parser deze informatie om de data juist op te slaan.
      Attributen kenmerkenen zich door het format naam="waarde" in de open-tag.
  2 - Elementen -> Pure data.
      Dit is wat de parser op zal halen uit het XML-bestand.
      Elementen verschijnen tussen de open- en sluiten-tags. -->

<!-- Hieronder een element met twee attributen -->
<bestand type="gif" id="4293">computer.gif</bestand>


```

* Juist-Geformateerd Document x Validatie

Een XML-document is juist-geformateerd als deze correct is qua syntax.
Echter, het is mogelijk om meer voorwaarden te injecteren in het document,
door gebruik te maken van document-definities, zoals DTD en XML Schema.

Een XML-document welke zich aan de document-definitie houdt wordt valide genoemd,
met betrekking tot dat document.

Met dit gereedschap kun je de XML-data controleren buiten je applicatie logica om.

```xml

<!-- Hieronder kun je een versimpelde versie van het boekenwinkel-document zien,
  met de toevoeging van DTD-definities.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Boekwinkel.dtd">
<boekwinkel>
    <boek categorie="COOKING">
        <titel>Everyday Italian</titel>
        <prijs>30.00</prijs>
    </boek>
</boekwinkel>

<!-- Deze DTD zou iets kunnen zijn zoals:-->

<!DOCTYPE note
[
<!ELEMENT boekwinkel (boek+)>
<!ELEMENT boek (titel,prijs)>
<!ATTLIST boek categorie CDATA "Literatuur">
<!ELEMENT titel (#PCDATA)>
<!ELEMENT prijs (#PCDATA)>
]>


<!-- De DTD start met een declaratie.
  Vervolgens wordt de root node gedeclareerd, met 1 of meerdere child nodes 'boek' 
  als vereiste. Elk 'boek' moet precies één 'titel' en 'prijs' bevatten en een
  attribuut 'categorie', met "Literatuur" als standaardwaarde.
  De 'titel' en 'prijs' nodes bevatten geparste character data.-->

<!-- De DTD zou gedeclareerd kunnen worden binnen het XML-bestand zelf.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT boekwinkel (boek+)>
<!ELEMENT boek (titel,prijs)>
<!ATTLIST boek categorie CDATA "Literatuur">
<!ELEMENT titel (#PCDATA)>
<!ELEMENT prijs (#PCDATA)>
]>

<boekwinkel>
    <boek categorie="Koken">
        <titel>Alledaags Italiaans</titel>
        <prijs>30.00</prijs>
    </boek>
</boekwinkel>
```
