---
language: xml
filename: learnxml.xml
contributors:
  - ["João Farias", "https://github.com/JoaoGFarias"]
translators:
    - ["Michał Woźniczak", "https://github.com/mwozniczak"]
lang: pl-pl
---

XML to język znacznikowy przeznaczony do przechowywania i transportu danych.

W przeciwieństwie do HTML, XML nie określa, jak mają być wyświetlane dane - on je jedynie zawiera.

* Składnia XML

```xml
<!-- Komentarze w XML wyglądają tak -->

<?xml version="1.0" encoding="UTF-8"?>
<ksiegarnia>
  <ksiazka kategoria="GOTOWANIE">
    <tytul jezyk="pl">Kuchnia Włoska na Codzień</tytul>
    <autor>Giada De Laurentiis</autor>
    <rok>2005</rok>
    <cena>30.00</cena>
  </ksiazka>
  <ksiazka kategoria="DZIECIECA">
    <tytul jezyk="pl">Harry Potter</tytul>
    <autor>J K. Rowling</autor>
    <rok>2005</rok>
    <cena>29.99</cena>
  </ksiazka>
  <ksiazka kategoria="WEB">
    <tytul jezyk="pl">XML od Podstaw</tytul>
    <autor>Erik T. Ray</autor>
    <rok>2003</rok>
    <cena>39.95</cena>
  </ksiazka>
</ksiegarnia>

<!-- Powyżej przedstawiono typowy plik XML.
  Rozpoczyna się (opcjonalną) deklaracją zawierającą metadane.

  XML używa struktury drzewiastej. W powyższym przykładzie węzłem głównym jest 'ksiegarnia', posiadająca trzy węzły podrzędne, wszystkie typu 'ksiazka'. Węzły te mogą mieć swoje podrzędne węzły, które z kolei... i tak dalej.

  Węzły tworzone są przy pomocy znaczników otwarcia/zamknięcia - węzły podrzędne to po prostu takie, które znajdują się między tymi znacznikami. -->


<!-- Każdy znacznik XML może zawierać dwa rodzaje danych:
  1 - Atrybuty -> Metadane na temat danego węzła.
      Parsery XML używają zazwyczaj tych informacje, by móc poprawnie zapisać zawarte dane. Zapisywane są w postaci nazwa="wartość" wewnątrz znacznika otwarcia.
  2 - Elementy -> Faktyczne dane.
      To jest to, co parser pozyska z pliku XML.
      Elementy znajdują się pomiędzy znacznikami otwarcia i zamknięcia. -->


<!-- Poniżej element z dwoma atrybutami -->
<plik typ="gif" id="4293">komputer.gif</plik>


```

* Dobrze sformatowany Dokument, a Walidacja

Dokument XML można określić dobrze sformatowanym, jeśli jego składnia jest poprawna.
Możliwe jest jednak dodanie warunków jakie muszą spełniać dane przy pomocy definicji dokumentu takich, jak DTD czy schemat XML.

Dokument XML spełniający taką definicję nazwać można zwalidowanym.

Dzięki temu, można sprawdzić poprawność danych XML poza logiką aplikacji.

```xml

<!-- Poniżej uproszczona wersja dokumentu księgarni, z dodaną definicją DTD.-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE note SYSTEM "Bookstore.dtd">
<ksiegarnia>
  <ksiazka kategoria="GOTOWANIE">
    <tytul>Kuchnia Włoska na Codzień</tytul>
    <cena>30.00</cena>
  </ksiazka>
</ksiegarnia>

<!-- DTD dla tego dokumentu może wyglądać na przykład tak: -->

<!DOCTYPE note
[
<!ELEMENT ksiegarnia (ksiazka+)>
<!ELEMENT ksiazka (title,price)>
<!ATTLIST ksiazka kategoria CDATA "Literatura">
<!ELEMENT tytul (#PCDATA)>
<!ELEMENT cena (#PCDATA)>
]>


<!-- DTD rozpoczyna się deklaracją.
  Następnie, deklarowany jest węzeł główny, który musi zawierać 1 lub więcej węzłów 'ksiazka'.
  Każda 'ksiazka' powinna zawierać dokładnie jeden 'tytul' i 'cena' oraz atrybut 'kategoria', którego domyślną wartością jest "Literatura".
  'tytul' i 'cena' zawierają przeparsowane dane znakowe.-->

<!-- DTD można też zdeklarować wewnątrz samego pliku XML.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE note
[
<!ELEMENT ksiegarnia (ksiazka+)>
<!ELEMENT ksiazka (tytul,cena)>
<!ATTLIST ksiazka kategoria CDATA "Literatura">
<!ELEMENT tytul (#PCDATA)>
<!ELEMENT cena (#PCDATA)>
]>

<ksiegarnia>
  <ksiazka kategoria="GOTOWANIE">
    <tytul>Kuchnia Włoska na Codzień</tytul>
    <cena>30.00</cena>
  </ksiazka>
</ksiegarnia>
```
