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

Perl 5 jest wysoce użytecznym, bogatym w wiele opcji językiem programowania z ponad 25 latami nieustannego rozwoju.

Perl 5 używany jest na ponad 100 różnych platformach (od przenośnych do w pełni stacjonarnych) i nadaje się zarówno do szybkiego prototypowania
jak i projektów deweloperskich prowadzonych na szeroką skalę.

```perl
# Pojedyncza linia komentarza zaczyna się od znaku hasha (płotka) "#".

#### Typy zmiennych w Perlu

#  Zmienna zaczyna się od symbolu dolara "$".
#  Prawidłowa nazwa zmiennej zaczyna się od litery lub podkreślnika "_", po których następuje dowolna ilość liter, cyfr i podkreślników.

### W Perlu występują trzy główne typy zmiennych: skalary, tablice i hasze.

## Skalary
#  Skalar przechowuje pojedynczą wartość:

my $zwierze = "wielbłąd";
my $odpowiedź = 42;

# Wartości skalarne mogą być ciągami znaków, liczbami całkowitymi lub zmiennoprzecinkowymi, a Perl automatycznie dokonuje konwersji pomiędzy nimi,
# w zalezności od wykonywanego kodu.

## Tablice
#  Tablica przechowuje listę wartości:

my @zwierzęta = ("wielbłąd", "alpaka", "sowa");
my @liczby    = (23, 42, 69);
my @mieszanka = ("wielbłąd", 42, 1.23);



## Hasze
#  Hasz przechowuje zestawy par klucz-wartość:

my %kolor_owocu = ('jabłko', 'czerwony', 'banan', 'żółty');

#  Możesz używać białych znaków (spacje, tabulatory) i operatora strzałki "=>" by czytelniej sformatować zapis hasza:

my %kolor_owocu = (
    jabłko  => 'czerwony',
    banan => 'żółty',
);

# Skalary, tablice i hasze są bardziej wyczerpująco udokumentowane w dokumencie perldata. (perldoc perldata).

# Bardziej złożone typy danych mogą być stworzone poprzez używanie referencji, które pozwalają Ci zbudować listy i hasze wewnątrz list i haszy.

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

# Perlowy skrócony zapis warunków

print "Siema!" if $rozochocony;
print "Nie mamy bananów" unless $banany;

# while
while (warunek) {
    ...
}

# for and foreach
for ($i = 0; $i <= $max; $i++) {
    ...
}

foreach (@tablica) {
    print "Tym elementem jest $_\n";
}

#### Wyrażenia regularne

# Perlowe wyrażenia regularne są tematem tak rozległym, jak wymagającym. Istnieje ogromna ilość dokumentacji w artykułach takich jak perlrequick, perlretut i inne.
# W dużym skrócie, podstawy perlowych wyrażeń regularnych są następujące:

# Proste dopasowanie:

if (/foo/)       { ... }  # true if $_ zawiera "foo"
if ($a =~ /foo/) { ... }  # true if $a zawiera "foo"

# Prosta zamiana:

$a =~ s/foo/bar/;         # zamienia "foo" na "bar" w zmiennej $a
$a =~ s/foo/bar/g;        # zamienia WSZYSTKIE WYSTĄPIENIA "foo" na "bar" w zmiennej $a


#### Pliki i I/O

# Możesz otworzyć plik dla odczytu lub zapisu używając funkcji "open ()".

open (my $odczyt,    "<",  "odczyt.txt") or die "Nie można otworzyć input.txt: $!";
open (my $zapis,     ">",  "zapis.txt")  or die "Nie można otworzyć output.txt: $!";
open (my $dopisanie, ">>", "my.log")     or die "Nie można otworzyć my.log: $!";

# Pliki możesz odczytywać z otworzonego handlera do pliku używając operatora "<>" (operator diamentowy). W kontekście skalarnym (przypisanie wyniku do skalara)
# operator ten zczytuje pojedynczą linię pliku, w kontekście listowym (przypisanie wyniku do tablicy) zczytuje całą zawartość pliku, przypisując każdą linię
# jako kolejny element listy:

my $linia = <$in>;
my @linie = <$in>;

#### Perlowe funkcje (procedury)

# Pisanie funkcji (procedur) jest proste:

sub logger {
    my $wiadomosc_do_loga = shift;
    open my $plik_log, ">>", "my.log" or die "Nie można otworzyć my.log: $!";
    print $plik_log $wiadomosc_do_loga;
}

# Teraz można używać napisanej funkcji tak jak każdej innej wbudowanej funkcji perlowej:

logger ("Mamy funkcję perlową");


```

#### Używanie modułów perlowych

Perl modules provide a range of features to help you avoid reinventing the wheel, and can be downloaded from CPAN ( http://www.cpan.org/ ).  A number of popular modules are included with the Perl distribution itself.
Moduły perlowe dostarczają szeroki wachlarz możliwości, by pomóc Ci w uniknięciu wynajdywania koła na nowo. Moduły te możesz pobrać z CPANa (http://www.cpan.org).
Sam Perl zawiera w swoich dystrybucjach kilka najpopularniejszych modułów z repozytorium CPANa.

Najczęściej zadawane pytania - perlfaq - zawierają pytania i odpowiedzi dotyczące wielu typowo realizowanych zadań. Często znajdziesz tam również sugestie
dotyczące użycia najlepszego modułu z repozytorium CPAN do zrealizowania konkretnego zadania.


#### Do doczytania

    - [perl-tutorial](http://perl-tutorial.org/)
    - [Naucz się Perla na www.perl.com](http://www.perl.org/learn.html)
    - [perldoc](http://perldoc.perl.org/)
    - wbudowane w Perla: `perldoc perlintro`
