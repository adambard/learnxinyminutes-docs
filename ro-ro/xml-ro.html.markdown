---
language: xml
filename: learnxml-ro.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
    - ["Serban Constantin", "https://github.com/fuzzmz"]
lang: ro-ro
---

XML este un limbaj de markup ce are ca scop stocarea si transportul de date.

Spre deosebire de HTML, XML nu specifica cum sa fie afisata sau formatata
informatia, ci doar o transporta.

* Sintaxa XML

```xml
<!-- Comentariile in XML arata asa -->

<?xml version="1.0" encoding="UTF-8"?>
<librarie>
  <carte categorie="GATIT">
    <titlu limba="ro">Mancaruri italiene</titlu>
    <autor>Giada De Laurentiis</autor>
    <an>2005</an>
    <pret>30.00</pret>
  </carte>
  <carte categorie="COPII">
    <titlu limba="ro">Harry Potter</titlu>
    <autor>J K. Rowling</autor>
    <an>2005</an>
    <pret>29.99</pret>
  </carte>
  <carte categorie="WEB">
    <titlu limba="ro">Invata XML</titlu>
    <autor>Erik T. Ray</autor>
    <an>2003</an>
    <pret>39.95</pret>
  </carte>
</librarie>

<!-- Deasupra este un fisier XML obisnuit.
  Incepe cu o declaratie ce adauga niste metadata (optional).
  
  XML foloseste o structura arborescenta. Deasupra, nodul de baza este
  'librarie', care are trei noduri copil, toate 'carti'. Acele noduri au la
  randul lor noduri copii si asa mai departe...

  Nodurile sunt create folosind taguri deschise/inchise, iar copii sunt doar
  noduri intre tagurile de deschis si inchis.-->  


<!-- XML transporta doua tipuri de date:
  1 - Atribute -> Metadata despre un nod.
      In general, parserul XML foloseste aceasta informatie sa stocheze
      proprietatile datelor.
      Este caracterizat de aparitia in paranteze in cadrul tagului deschis
  2 - Elemente -> Date pure.
      Asta este ceea ce parserul va extrage din documentul XML.
      Elementele apar intre tagurile deschis si inchis, fara paranteze. -->
      
  
<!-- Dedesubt, un element cu doua atribute -->
<file type="gif" id="4293">computer.gif</file>


```

* Document bine formatat x Validare

Un document XML este bine formatat daca este corect sintactic.
Cu toate astea este posibil sa injectam mai multe constrangeri in document
folosind definitii precum DTD si XML Schema.

Un document XML ce foloseste o definitie de document este numit valid in
contextul documentului. 

Cu acest tool poti verifica datele XML in afara codului aplicatiei.

```xml

<!-- Dedesubt este o versiune simplificata a documentului librarie, 
  cu aditia definitiei DTD.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Librarie.dtd">
<librarie>
  <carte categorie="GATIT">
    <titlu >Everyday Italian</titlu>
    <pret>30.00</pret>
  </carte>
</librarie>

<!-- DTD-ul poate fi ceva similar cu:-->

<!DOCTYPE note
[
<!ELEMENT librarie (carte+)>
<!ELEMENT carte (titlu,pret)>
<!ATTLIST carte categorie CDATA "Literatura">
<!ELEMENT titlu (#PCDATA)>
<!ELEMENT pret (#PCDATA)>
]>


<!-- DTD-ul incepe cu o declaratie.
  Dupa, nodul de baza este declarat, cerand unul sau mai multe noduri copii
  de tipul 'carte'.
  Fiecare 'carte' trebuie sa contina exact un 'titlu' si 'pret' si un atribut
  numit 'categorie', cu "Literatura" ca valoare implicita.
  Nodurile 'titlu' si 'pret' contin parsed character data.-->

<!-- DTD-ul poate fi declara si in interiorul fisierului XML.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT librarie (carte+)>
<!ELEMENT carte (titlu,pret)>
<!ATTLIST carte categorie CDATA "Literatura">
<!ELEMENT titlu (#PCDATA)>
<!ELEMENT pret (#PCDATA)>
]>

<librarie>
  <carte categorie="GATIT">
    <titlu >Everyday Italian</titlu>
    <pret>30.00</pret>
  </carte>
</librarie>
```
