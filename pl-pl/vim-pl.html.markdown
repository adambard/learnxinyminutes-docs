---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
    - ["Adam Bobowski", "https://github.com/Bobowski"]
lang: pl-pl
filename: LearnVim-pl.txt
---


[Vim](www.vim.org)
(Vi IMproved) jest klonem popularnego edytora vi dla systemów Unix. 
Zaprojektowany z myślą o prędkości edycji i zwiększeniu produktywności jest 
wszechobecny na systemach UNIXopodobnych. Posiada wiele skrótów klawiszowych 
do szybkiej nawigacji do wybranych miejsc w plikach oraz szybkiej edycji 
danych fragmentów.

## Podstawy nawigacji w Vim

```
    vim <nazwapliku> # Otwórz <nazwapliku> w vim
    :q               # Zamknij vim
    :w               # Zapisz aktualny plik
    :wq              # Zapisz i wyjdź z vim
    :q!              # Wyjdź bez zapisywania
                     # ! *wymusza* wykonanie :q, dlatego nie wykonuje zapisu
    :x               # Zapisz i wyjdź, krótszy odpowiednik :wq

    u                # Cofnij operację
    CTRL+R           # Powtórz operację

    h                # Przesuń kursor w lewo
    j                # Przesuń kursor w dół
    k                # Przesuń kursor w górę
    l                # Przesuń kursor w prawo

    # Poruszanie w obrębie jednej lini

    0                # Skocz do początku linii
    $                # Skocz do końca linii
    ^                # Skocz do pierwszego niebiałego znaku

    # Wyszukiwanie w tekście

    /slowo           # Zaznacza wszystkie wystąpienia słowa za kursorem
    ?slowo           # Zaznacza wszystkie wystąpienia słowa przed kursorem
    n                # Przemieszcza kursor do następnego wystąpienia słowa
    N                # Przemieszcza kursor do poprzedniego wystąpenia słowa

    :%s/foo/bar/g    # Zamień 'foo' na 'bar' w każdej linii tekstu
    :s/foo/bar/g     # Zamień 'foo' na 'bar' w aktualnej linii

    # Skoki do znaków

    f<znak>          # Skocz do przodu i zatrzymaj się na <znak>
    t<znak>          # Skocz do przodu i zatrzymaj się przed <znak> 

    # Na przykład,    
    f<               # Skocz do przodu i zatrzymaj się na <
    t<               # Skocz do przodu i zatrzymaj się przed <
    
    # Moving by word

    w                # Przesuń kursor do przodu o jedno słowo
    b                # Przesuń kursor do tyłu o jedno słowo
    e                # Przesuń kursor do końca aktualnego słowa

    # Inne znaki do przemieszczania się

    gg               # Skocz na początek pliku
    G                # Skocz na koniec pliku
    :NUM             # Skocz do linii o numerze NUM
    H                # Skocz na górę ekranu
    M                # Skocz na środek ekranu
    L                # Skocz na dół ekranu
```

## Tryby:

Vim oparty jest na koncepcji **trybów**.

Command Mode - (Tryb komend) vim zaczyna w tym trybie, używany do nawigacji i wpisywania komend
Insert Mode  - (Tryb wprowadzania) używany do wprowadzania zmian w pliku
Visual Mode  - (Tryb wizualny) używany do zaznaczania tekstu i wykonywania komend na nim
Ex Mode      - (Tryb Ex) 

```
    i                # Przechodzi to trybu wprowadzania, przed pozycją kursora
    a                # Przechodzi do trybu wprowadzania, za pozycją kursora
    v                # Przechodzi do trybu wizualnego
    :                # Przechodzi do trybu ex
    <esc>            # Wychodzi z dowolnego aktywnego trybu do trybu komend

    # Kopiowanie i wklejanie tekstu

    y                # Skopiuj zaznaczony tekst
    yy               # Skopiuj aktualną linię
    d                # Usuń zaznaczony tekst
    dd               # Usuń aktualną linię
    p                # Wklej skopiowany tekst za kursorem
    P                # Wklej skopiowany tekst przed kursorem
    x                # Usuń znak pod kursorem
```

## 'Gramatyka' vim'a

Vim można traktować jako zbiór komend w formacie 'Akcja-Modyfikator-Obiekt', gdzie:

Akcja       - jedna z dostępnych akcji
Modyfikator - w jaki sposób wykonywana jest akcja
Obiekt      - obiekt na którym wykonywana jest akcja

Kilka ważnych przykładów Akcji, Modyfikatorów i Obiektów:

```
    # 'Akcje'
 
    d                # Usuń
    c                # Zmień
    y                # Skopiuj
    v                # Zaznacz

    # 'Modyfikatory'

    i                # W środku
    a                # Dookoła
    NUM              # Liczba
    f                # Szuka czegoś i zatrzymuje się na tym
    t                # Szuka czegoś i zatrzymuje się przed tym
    /                # Znajduje napis od kursora naprzód
    ?                # Znajduje napis przed kursorem

    # 'Obiekty'

    w                # Słowo
    s                # Zdanie
    p                # Paragraf
    b                # Blok
    
    # Przykładowe 'zdania'

    d2w              # Usuń 2 słowa
    cis              # Zmień w zdaniu
    yip              # Skopiuj paragraf w którym jest kursor
    ct<              # Zamień na <
    d$               # Usuń tekst do końca linii
```

## Pewne skróty i triki

        <!--TODO: Dodać więcej!-->
```
    >                # Zrób wcięcie zaznaczonego bloku
    <                # Usuń wcięcie zaznaczonego bloku
    :earlier 15m     # Przywróć dokument do stanu z przed 15 minut 
    :later 15m       # Odwróć efekt poprzedniej komendy
    ddp              # Zamień kolejnością kolejne linie, dd potem p
    .                # Powtórz poprzednią komendę
```

## Makra

Makra są właściwie nagrywanymi akcjami. Gdy zaczynasz nagrywać makro, nagrywa ono
**każdą** akcję i komendę jaką wykonasz do momentu przerwania nagrywania.
Wywołanie makra wykonuje dokładnie te same operacje w tej samej kolejności.

```
    qa               # Zacznij nagrywać makro 'a'
    q                # Przerwij nagrywanie
    @a               # Odtwórz makro 'a'
```

### Konfiguracja ~/.vimrc

Plik .vimrc może być użyty do skonfigurowania Vim'a przy jego starcie

Poniżej zamieszczono przykładowy plik ~/.vimrc:

```
" Przykładowy ~/.vimrc
" 2016.10 

" Wymagane aby korzystać z opcji iMproved
set nocompatible

" Na podstawie typu pliku włącza inteligentne wcięcia i inne.
filetype indent plugin on

" Włącz podkreślanie składni
syntax on

" Lepsze uzupełnianie składni komend
set wildmenu

" Wyszukiwanie będzie ignorować wielkość liter poza przypadkami gdy użyjemy wielkich liter
set ignorecase
set smartcase

" Po otwarciu pliku gdzie nie jest zdefiniowane zachowanie wcięć
" zostanie zachowane wcięcie takie samo jak w aktualnej linii
set autoindent

" Wyświetlaj numer lini
set number

" Opcje wcięć, zmień w zależności od osobistych upodobań

" Szerokość TAB w spacjach
set tabstop=4

" Liczba spacji w TAB podczas edycji
set softtabstop=4

" Liczba spacji gdy wykonywane są operacje wcięcia (>> i <<)
set shiftwidth=4

" Zamieniaj tabulatory na spacje
set expandtab

" Aktywuj inteligentne tabulatory i spacje do wcięć i wyrównań
set smarttab
```

### Odniesienia [ENG]

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)
