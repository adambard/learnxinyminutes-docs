---
language: bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Jakub Młokosiewicz", "https://github.com/hckr"]
lang: pl-pl
---

Brainfuck (pisane małymi literami, za wyjątkiem początku zdania) jest bardzo 
minimalistycznym, kompletnym w sensie Turinga, językiem programowania.
Zawiera zaledwie 8 poleceń.

Możesz przetesotwać brainfucka w swojej przeglądarce, korzystając z narzędzia 
[brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```
Wszystkie znaki oprócz "><+-.,[]" (wyłączając znaki zapytania) są ignorowane.

Pamięć w brainfucku jest reprezentowana przez tablicę 30.000 komórek
zainicjalizowanych zerami, ze wskaźnikiem pokazującym na aktualną komórkę.

Oto osiem poleceń brainfucka:
+ : inkrementuje (zwiększa o jeden) wartość aktualnie wskazywanej komórki
- : dekrementuje (zmniejsza o jeden) wartość aktualnie wskazywanej komórki
> : przesuwa wskaźnik na następną komórkę (w prawo)
< : przesuwa wskaźnik na poprzednią komórkę (w lewo)
. : wyświetla wartość bieżącej komórki (w formie znaku ASCII, np. 65 = 'A')
, : wczytuje (jeden) znak z wejścia do bieżącej komórki
    (konkretnie jego numer z tabeli ASCII)
[ : jeśli wartość w bieżącej komórce jest rózna zero, przechodzi do
    odpowiadającego ]; w przeciwnym wypdaku przechodzi do następnej instrukcji
] : Jeśli wartość w bieżącej komórce jest rózna od zera, przechodzi do
    następnej instrukcji; w przeciwnym wypdaku przechodzi do odpowiadającego [

[ i ] oznaczają pętlę while. Oczywiście każda pętla rozpoczęta [
musi być zakończona ].

Zobaczmy kilka prostych programów w brainfucku.


++++++ [ > ++++++++++ < - ] > +++++ .

Ten program wypisuje literę 'A'. Najpierw zwiększa wartość komórki #1 do 6.
Komórka #1 będzie wykorzystana w pętli. Następnie program wchodzi w pętlę ([)
i przechodzi do komórki #2. Pętla wykonuje się sześć razy (komórka #1 jest
dekrementowana sześć razy, nim osiągnie wartość zero, kiedy to program
przechodzi do odpowiadającego ] i wykonuje kolejne instrukcje).

W tym momencie wskaźnik pokazuje na komórkę #1, mającą wartość 0, podczas gdy
komórka #2 ma wartość 60. Przesuwamy wskaźnik na komórkę #2, inkrementujemy ją
pięć razy, uzyskując wartość 65. Następnie wyświetlamy wartość komórki #2.
65 to 'A' w tabeli ASCII, więc właśnie ten znak jest wypisany na konsolę.


, [ > + < - ] > .

Ten program wczytuje znak z wejścia i umieszcza jego kod ASCII w komórce #1.
Następnie zaczyna się pętla, w której znajdują się następujące instrukcje: 
przesunięcie wskaźnika na komórkę #2, inkrementacja wartości komóri #2,
powrót do komórki #1 i dekrementacja wartości komórki #1. Instrukcje pętli
wykonują się aż wartość komórki #1 osiągnie zero, a komórka #2 osiągnie
poprednią wartość komórki #1. Ponieważ na końcu pętli wskaźnik pokazuje na
komórkę #1, po pętli następuje instrukcja przejścia do komórki #2 i wysłanie
jej wartości (w formie znaku ASCII) na wyjście.

Zauważ, że odstępy służą wyłącznie poprawie czytelności.
Równie dobrze można powyższy program zapisać tak:

,[>+<-]>.


Spróbuj odgadnąć, co robi poniższy program:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Ten program pobiera z wejścia dwie liczby i je mnoży.

Po wczytaniu dwóch wejść (do komórek #1 i #2) następuje pętla zewnętrzna,
warunkowana wartością komórki #1. Następnie program przechodzi do komórki #2
i rozpoczyna pętlę wewnętrzną z warunkiem zakończenia w komórce #2,
inkrementującą komórkę #3. Tu jednak pojawia się problem: w chwili zakończenia
wewnętrznej pętli komórka #2 ma wartość zero. W takim razie wewętrzna pętla
nie wywoła się następny raz. Aby rozwiązać ten problem, inkrementujemy także
wartość komórki #4, a następnie kopiujemy jej wartość do komórki #2.
Ostatecznie wynik działania znajduje się w komórce #3.
```

I to właśnie jest brainfuck. Nie taki trudny, co? W ramach rozrywki możesz
napisać własne programy w brainfucku. Możesz też napisać interpreter brainfucka
w innym języku. Implementacja interpretera to dość proste zadanie. Jeśli
jesteś masochistą, spróbuj napisać interpreter brainfucka w... brainfucku.
