---
language: xml
filename: learnxml-nl.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
  - ["Frank van Gemeren", "https://github.com/frvge"]
lang: nl-nl
---

XML is een markuptaal die ontwikkeld is om data in te bewaren en data mee te 
verzenden.

Anders dan HTML specificeert XML niet hoe data getoond of geformatteerd moet worden. 
Het bevat de data slechts.

* XML Syntax

```xml
<!-- Dit is commentaar in XML -->

<?xml version="1.0" encoding="UTF-8"?>
<boekenwinkel>
  <boek categorie="KOKEN">
    <title taal="nl">Alledaags Italiaans</titel>
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
    <titel taal="en">Learning XML</titel>
    <auteur>Erik T. Ray</auteur>
    <jaar>2003</jaar>
    <prijs>39.95</prijs>
  </boek>
</boekenwinkel>

<!-- Hierboven staat een standaard XML bestand.
  Het begint met een declaratie die optionele metadata bevat.

  XML werkt met een boomstructuur. De stamknoop hierboven is 'boekenwinkel'. 
  Deze heeft drie kinderen die allemaal 'boek' zijn. Deze knopen hebben op 
  hun beurt weer kinderen, enzovoort...

  Knopen hebben open- en sluittags. Kinderen zijn knopen die zich tussen de 
  open- en sluittags van hun ouders bevinden. -->

<!-- XML bevat two soorten data:
  1 - Attributen -> Dit is metadata van een knoop.
      Deze informatie wordt meestal door de XML parser gebruikt om de data op
      de juiste manier op te slaan. Je herkent het door de syntax in de vorm 
      van naam="waarde" in de open tag.
  2 - Elementen -> Dit is de pure data
      Deze gegevens worden door de parser uit het XML bestand gehaald.
      Elementen staan tussen de open- en sluittags. -->


<!-- Hieronder staat een element met twee attributen -->
<bestand type="gif" id="4293">computer.gif</bestand>


```

* Grammaticaal correcte documenten x Validatie

Een XML document is "grammaticaal correct" of "well-formatted" als de 
syntax correct is. Het is ook mogelijk om meer structuur in het document 
aan te brengen met document definities zoals DTD en XML Schema.

Een XML document dat aan een document definitie voldoet wordt "valide" volgens 
die document definitie genoemd.

Met deze gereedschappen kan je de XML data buiten je applicatie logica 
controleren.

```xml

<!-- Hieronder staat een versimpelde versie voor een boekenwinkel document,
  met een toevoeging van een DTD definitie. -->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "boekenwinkel.dtd">
<boekenwinkel>
  <boek categorie="KOKEN">
    <titel>Alledaags Italiaans</titel>
    <prijs>30.00</prijs>
  </boek>
</boekenwinkel>

<!-- De DTD kan er als volgt uitzien:-->

<!DOCTYPE note
[
<!ELEMENT boekenwinkel (boek+)>
<!ELEMENT boek (titel,prijs)>
<!ATTLIST boek categorie CDATA "Literatuur">
<!ELEMENT titel (#PCDATA)>
<!ELEMENT prijs (#PCDATA)>
]>


<!-- De DTD begint met een declaratie.
  Hierna volgt de declaratie van de stamknoop, die 1 of meer 'boek' kinderen 
  moet bevatten. 
  Elk 'boek' moet precies 1 'titel' en 'prijs' element bevatten en een attribuut 
  'categorie' hebben waarvan 'Literatuur' de standaard waarde is.
  De 'titel' en 'prijs' knopen bevatten parsed character data.-->

<!-- De DTD kan ook in het XML bestand zelf gedeclareerd worden.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT boekenwinkel (boek+)>
<!ELEMENT boek (titel,prijs)>
<!ATTLIST boek categorie CDATA "Literatuur">
<!ELEMENT titel (#PCDATA)>
<!ELEMENT prijs (#PCDATA)>
]>

<boekenwinkel>
  <boek categorie="KOKEN">
    <titel>Alledaags Italiaans</titel>
    <prijs>30.00</prijs>
  </boek>
</boekenwinkel>
```
