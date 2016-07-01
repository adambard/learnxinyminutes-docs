---
language: json
filename: learnjson-pl.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
  - ["Michael Neth", "https://github.com/infernocloud"]
translators:
  - ["Michał Mitrosz", "https://github.com/Voltinus"]
lang: pl-pl
---

JSON to bardzo prosty format wymiany danych. Jak jest napisane na [json.org](http://json.org), jest łatwy do pisania i czytania dla ludzi i do parsowania i generowania dla maszyn.

Kod JSON musi zawierać któreś z poniższych:
* Zbiór par nazwa/wartość (`{ }`). W różnych językach jest to obiekt, rekord, struktura, słownik, tablica mieszająca, lista z kluczami, lub tablica asocjacyjna.
* Uporządkowana lista wartości (`[ ]`). W różnych językach jest to tablica, wektor, lista, lub sekwencja.
 tablica/lista/sekwencja (`[ ]`) lub słownik/obiekt/tablica asocjacyjna (`{ }`).

JSON w swojej czystej postaci nie ma komentarzy, ale większość parserów akceptuje komentarze w stylu C (`//`, `/* */`). Niektóre parsery pozwalają także na końcowy przecinek (np. przecinek po ostatnim elemencie w tablicy lub po ostatiej własności obiektu), ale powinien on być omijany dla lepszej kompatybilności.

Dla celów tego poradnika wszystko będzie 100% kodem JSON. Na szczęście, to samo mówi za siebie.

Wspierane typy danych:

* Łańcuchy znaków: `"witaj"`, `"\"Cytat.\""`, `"\u0abe"`, `"Nowa linia.\n"`
* Liczby: `23`, `0.11`, `12e10`, `3.141e-10`, `1.23e+4`
* Obiekty: `{ "klucz": "wartość" }`
* Tablice: `["Wartości"]`
* Inne: `true`, `false`, `null`

```json
{
  "klucz": "wartość",

  "klucze": "muszą być zawsze zamknięte w podwójnych cudzysłowach",
  "liczby": 0,
  "łańcuchy": "Hellø, wørld. Wszystkie znaki unicode są dozwolone, razem z \"sekwencjami escape\".",
  "wartości logiczne?": true,
  "nic": null,

  "duża liczba": 1.2e+100,

  "obiekty": {
    "komentarz": "Większość twojej struktury będzie zbudowana z obiektów.",

    "tablica": [0, 1, 2, 3, "Tablice mogą mieć wewnątrz cokolwiek", 5],

    "inny obiekt": {
      "komentarz": "Elementy mogą się w sobie zawierać, bardzo użyteczne"
    }
  },

  "głupota": [
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

  "styl alternatywny": {
    "komentarz": "sprawdź to!"
  , "pozycja przecinka": "nie ma znaczenia, o ile jest przed następnym kluczem, jest poprawnie"
  , "następny komentarz": "jak ładnie"
  },



  "znaki białe": "nie mają znaczenia",



  "to było krótkie": "I gotowe. Wiesz już wszystko o formacie JSON."
}
```

## Dalsza lektura

* [JSON.org](http://json.org) Cały JSON pięknie wytłumaczony na podstawie grafik przypominających schematy blokowe.
