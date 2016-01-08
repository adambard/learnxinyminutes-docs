---
name: perl
category: language
language: perl
filename: learnperl.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
    - ["Michał Kupczyński", "http://github.com/ukoms"]
lang: pl-pl
---

Perl 5 jest wysoce użytecznym, bogatym w wiele opcji językiem programowania
z ponad 25 latami nieustannego rozwoju.

Perl 5 używany jest na ponad 100 różnych platformach (od przenośnych do w
pełni stacjonarnych) i nadaje się zarówno do szybkiego prototypowania jak
i projektów deweloperskich prowadzonych na szeroką skalę.

```perl

# Pojedyncza linia komentarza zaczyna się od znaku hasha (płotka) "#".

#### Typy zmiennych w Perlu

# Zmienna zaczyna się od symbolu dolara "$".
# Prawidłowa nazwa zmiennej zaczyna się od litery lub podkreślnika "_",
# po których następuje dowolna ilość liter, cyfr i podkreślników.

### W Perlu występują trzy główne typy zmiennych: skalary, tablice i hasze.

## Skalary
#  Skalar przechowuje pojedynczą wartość:
my $zwierze = "wielbłąd";
my $odpowiedź = 42;

# Wartości skalarne mogą być ciągami znaków, liczbami całkowitymi lub
# zmiennoprzecinkowymi, zaś Perl automatycznie dokonuje konwersji pomiędzy nimi,
# w zależności od wykonywanego kodu/kontekstu.

## Tablice
#  Tablica przechowuje listę wartości:
my @zwierzęta = ("wielbłąd", "alpaka", "sowa");
my @liczby    = (23, 42, 69);
my @mieszanka = ("wielbłąd", 42, 1.23);

## Hasze
#  Hasz przechowuje zestawy par klucz-wartość:
my %kolor_owocu = ('jabłko', 'czerwony', 'banan', 'żółty');

# Możesz używać białych znaków (spacje, tabulatory) i operatora strzałki "=>"
# by czytelniej sformatować zapis hasza:
my %kolor_owocu = (
    jabłko  => 'czerwony',
    banan => 'żółty',
);

# Skalary, tablice i hasze są bardziej wyczerpująco udokumentowane w dokumencie
# [perldoc perldata](http://perldoc.perl.org/perldata.html).

# Bardziej złożone typy danych mogą być stworzone poprzez używanie referencji,
# które pozwalają Ci zbudować listy i hasze wewnątrz list i haszy.

#### Warunki logiczne i pętle

# W Perlu występują typowe warunki i pętle.
if ($var) {
    ...
} elsif ($var eq 'bar') {
    ...
} else {
    ...
}

unless (warunek) {
    ...
}
# Powyższy zapis jest równoznaczny zapisowi "if (!warunek)"

# Perlowy skrócony zapis warunków:
print "Siema!" if $rozochocony;
print "Nie mamy bananów" unless $banany;

# Pętla while
while (warunek) {
    ...
}

# Pętle for oraz foreach
for ($i = 0; $i <= $max; $i++) {
    ...
}

foreach (@tablica) {
    print "Tym elementem jest $_\n";
}

# lub

foreach my $iterator (@tablica) {
    print "Iterowanym elementem jest $iterator\n";
}

#### Wyrażenia regularne

# Perlowe wyrażenia regularne są tematem tak rozległym, jak wymagającym.
# Istnieje ogromna ilość dokumentacji w artykułach takich jak
# [perlrequick](http://perldoc.perl.org/perlrequick.html),
# [perlretut](http://perldoc.perl.org/perlretut.html) i inne.
# W dużym skrócie, podstawy perlowych wyrażeń regularnych są następujące:

# Proste dopasowanie:
if (/foo/)       { ... }  # prawda jeżeli $_ zawiera "foo"
if ($a =~ /foo/) { ... }  # prawda jeżeli $a zawiera "foo"

# Prosta zamiana:
# Zamienia "foo" na "bar" w zmiennej $a
$a =~ s/foo/bar/;
# Zamienia WSZYSTKIE WYSTĄPIENIA "foo" na "bar" w zmiennej $a
$a =~ s/foo/bar/g;

#### Pliki i I/O

# Możesz otworzyć plik do odczytu lub zapisu używając funkcji "open ()".
open (my $odczyt,    "<",  "odczyt.txt") or die "Błąd otwierania input.txt: $!";
open (my $zapis,     ">",  "zapis.txt")  or die "Błąd otwierania output.txt: $!";
open (my $dopisanie, ">>", "my.log")     or die "Błąd otwierania my.log: $!";

# Pliki możesz odczytywać z otworzonego handlera używając operatora "<>"
# (operator diamentowy). W kontekście skalarnym (przypisanie wyniku do skalara)
# operator ten zczytuje pojedynczą linię pliku, w kontekście listowym
# (przypisanie wyniku do tablicy) zczytuje całą zawartość pliku, przypisując
# każdą linię jako kolejny element listy:
my $linia = <$in>;
my @linie = <$in>;

#### Perlowe funkcje (procedury)

# Pisanie funkcji (procedur) jest proste:
sub logger {
    my $wiadomosc_do_loga = shift;
    open (my HANDLER, ">>", "my.log") or die "Błąd otwierania my.log: $!";
    print HANDLER $wiadomosc_do_loga;
}

# Teraz można używać napisanej funkcji, tak jak każdej innej wbudowanej
# funkcji perlowej:
logger ("Mamy funkcję perlową");

```

#### Używanie modułów perlowych

Moduły perlowe dostarczają szeroki wachlarz możliwości, byś nie musiał
wynajdywać koła na nowo. Moduły te można pobrać z [CPAN](http://www.cpan.org).
Sam Perl zawiera w swoich dystrybucjach kilka najpopularniejszych modułów
z repozytorium [CPAN](http://www.cpan.org).

Najczęściej zadawane pytania [perlfaq](http://perldoc.perl.org/perlfaq.html)
- zawierają pytania i odpowiedzi dotyczące wielu typowo realizowanych zadań.
Często znajdziesz tam również sugestie dotyczące użycia najlepszego modułu
z repozytorium CPAN do zrealizowania konkretnego zadania.


#### Do doczytania

 - [perl-tutorial](http://perl-tutorial.org/)
 - [Naucz się Perla na www.perl.com](http://www.perl.org/learn.html)
 - [perldoc](http://perldoc.perl.org/)
 - wbudowane w Perla: `perldoc perlintro`
