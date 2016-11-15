---
language: xml
filename: learnxml-pl.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:  
  - ["Tomasz Janiszewski", "https://github.com/janisz"]  
lang: pl-pl
---

XML (_Extensible Markup Language_) to rozszerzalny język znaczników, stworzony
do przechowywania i transportu danych.

W przeciwieństwie do HTML, XML nie specyfikuje w jaki sposób wyświetlić dane, a
tylko je przechowuje.

* Składnia XML

```xml
<!-- Komentarze w XML wyglądają jak ten -->

<?xml version="1.0" encoding="UTF-8"?>
<ksiegarnia>
  <ksiazka kategoria="GOTOWANIE">
    <tytul lang="pl">Codzienny Włoski</tytul>
    <autor>Giada De Laurentiis</autor>
    <rok>2005</rok>
    <cena>30.00</cena>
  </ksiazka>
  <ksiazka kategoria="DZIECI">
    <tytul lang="pl">Harry Potter</tytul>
    <autor>J K. Rowling</autor>
    <rok>2005</rok>
    <cena>29.99</cena>
  </ksiazka>
  <ksiazka kategoria="WEB">
    <tytul lang="pl">Nauka XML</tytul>
    <autor>Erik T. Ray</autor>
    <rok>2003</rok>
    <cena>39.95</cena>
  </ksiazka>
</ksiegarnia>

<!-- Powyżej jest typowy plik XML.
  Zaczyna się od deklaracji zawierającej metadane (opcjonalne).  

  XML używa drzewiastej struktury. Powyżej, głównym wierzchołkiem jest
  'ksiegarnia' , która zawiera trzy (3) węzły potomne, wszystkie 'ksiazki',
  które zawierają swoje węzły potomne, i tak dalej...

  Węzły są tworzone używające otwierających/zamykających znaczników.
  Węzły potomne znajdują się pomiędzy otwierającym i zamykającym znacznikiem.
-->

<!-- XML przechowuje dwa typy danych
  1 - Atrybuty -> metadane o węźle
      Zazwyczaj parser XML używa tych informacji do przechowywania danych we
      właściwy sposób. Atrybuty nadawane są poprzez wpisanie ich w otwierajacym
      znaczniku.
  2 - Elementy -> to są czyste dane.
      Dane, które parser otrzymuje z pliku XML.
      Elementy są deklarowane pomiędzy otwierajacym i zamykającym znacznikiem,
      bez nawiasów. -->

<!-- Poniższy element ma dwa atrybuty -->
<plik type="gif" id="4293">komputer.gif</plik>


```

* Dobrze sformatowany dokument i walidacja

Dokument XML jest dobrze sformatowany gdy jest syntaktycznie poprawny.
Jednakże możliwe jest wstrzykiwanie większej liczby ograniczeń w dokumencie,
używając definicji takich jak DTD i XML Schema.

Dokument XML, który jest zgodny ze swoją definicją jest poprawny.


Korzystając z tych narzędzi możesz sprawdzić dane zawarte w dokumencie poza
logiką aplikacji.

```xml


<!-- Poniżej jest uproszczona wersja dokumentu księgarni,
  z dodatkową definicją DTD.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE notatka SYSTEM "Ksiegarnia.dtd">
<ksiegarnia>
  <ksiazka kategoria="GOTOWANIE">
    <tytul >Everyday Italian</tytul>
    <cena>30.00</cena>
  </ksiazka>
</ksiegarnia>

<!-- DTD może wyglądać następująco:-->

<!DOCTYPE notatka
[
<!ELEMENT ksiegarnia (ksiazka+)>
<!ELEMENT ksiazka (tytul,cena)>
<!ATTLIST ksiazka kategoria CDATA "Literatura">
<!ELEMENT tytul (#PCDATA)>
<!ELEMENT cena (#PCDATA)>
]>


<!-- DTD zaczyna się od deklaracji
  Zaczynając od góry, główny węzeł jest zadeklarowany jako wymagający jednego
  lub więcej węzłów potomnych typu 'ksiżka'.
  Każda 'ksiażka' powinna zawierać dokładnie jeden 'tytuł' i 'cene' oraz atrybut
  'kategoria' z 'literaturą' jako wartość domyślna.
  'tytuł' i 'cena' to pola typu parsowalnych zmiennyc znakowych, co oznacza że
  użyte znaczniki zostaną zinterpretowane &lt; zamienione <. -->

<!-- DTD moze być deklarowane wewnątrz pliku XML. -->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE notatka
[
<!ELEMENT ksiegarnia (ksiazka+)>
<!ELEMENT ksiazka (tytul,cena)>
<!ATTLIST ksiazka kategoria CDATA "Literatura">
<!ELEMENT tytul (#PCDATA)>
<!ELEMENT cena (#PCDATA)>
]>

<ksiegarnia>
  <ksiazka kategoria="GOTOWANIE">
    <tytul >Everyday Italian</tytul>
    <cena>30.00</cena>
  </ksiazka>
</ksiegarnia>
```
