---
language: json
filename: learnjson.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
translators:
    - ["Michał Woźniczak", "https://github.com/mwozniczak"]
lang: pl-pl
---

JSON jest wyjątkowo prostym formatem wymiany danych, w związku z czym będzie to prawdopodobnie najkrótsze "Naucz się X w Y Minut" świata.

W swojej najczystszej postaci, JSON nie zawiera komentarzy, chociaż większość parserów poprawnie zinterpretuje te oznaczone stylem C (`//`, `/* */`). Niektóre parsery dopuszczają też końcowy przecinek (tj. przecinek po ostatnim elemencie tablicy, czy po ostatniej właściwości obiektu), ale zaleca się tego unikać dla zwiększenia kompatybilności.

Kod zamieszczony poniżej będzie jednak stuprocentowo poprawnym JSONem. Na nasze szczęście, kod ten mówi sam za siebie.

Typy danych obsługiwane przez JSON to: liczby, ciągi znaków, wartości logiczne (`true` i `false`), tablice, obiekty i `null`.
Przeglądarki obsługujące JSON to: Firefox(Mozilla) 3.5, Internet Explorer 8, Chrome, Opera 10, Safari 4.
Rozszerzenie plików JSON to ".json". Typ MIME to "application/json".
Wady JSONa to niemożność definiowania typów i brak jakiegokolwiek DTD.

```json
{
  "klucz": "wartość",

  "klucze": "należy zawsze umieszczać w podwójnych cudzysłowach",
  "liczby": 0,
  "ciągi znaków": "Witøj, świëcie. Unicode jest dozwolony, podobnie jak \"znaki modyfikacji\".",
  "wartości logiczne są?": true,
  "zupełnie nic": null,

  "duże liczby": 1.2e+100,

  "obiekty": {
    "komentarz": "Większość struktury wywodzi się z obiektów.",

    "tablice": [0, 1, 2, 3, "W tablicach można umieścić wszystko.", 5],

    "inny obiekt": {
      "komentarz": "Można je zagnieżdżać jedne w drugich. Bardzo przydatne."
    }
  },

  "bzdury": [
    {
      "źródła potasu": ["banany"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "inne style": {
    "komentarz": "oczaj to!"
  , "umiejscowienie przecinka": "nie ma znaczenia - byleby znajdował się przed następną parą klucz-wartość"
  , "inny komentarz": "nieźle"
  },

  "szybko poszło": "I to by było na tyle. Wiesz już wszystko, co JSON ma do zaoferowania."
}
```
