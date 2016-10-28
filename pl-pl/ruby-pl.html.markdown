---
language: ruby
filename: learnruby-pl.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
  - ["Marcos Brizeno", "http://www.about.me/marcosbrizeno"]
  - ["Ariel Krakowski", "http://www.learneroo.com"]
  - ["Dzianis Dashkevich", "https://github.com/dskecse"]
  - ["Levi Bostian", "https://github.com/levibostian"]
  - ["Rahil Momin", "https://github.com/iamrahil"]
  - ["Gabriel Halley", "https://github.com/ghalley"]
  - ["Persa Zula", "http://persazula.com"]
translators:
    - ["Marcin Klocek", "https://github.com/mklocek"]
lang: pl-pl
---

```ruby
# To jest komentarz

=begin 
To jest wielolinijkowy komentarz
Nikt ich nie używa
Ty też nie powinieneś
=end

# Przede wszystkim: Wszystko jest obiektem.

# Liczby są obiektami

3.class #=> Fixnum

3.to_s #=> "3"


# Trochę podstawowej arytmetyki
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
5 % 3 #=> 2
5 ^ 6 #=> 3

# Arytmetyka jest zastąpeniem składni
# metod wywoływanych na obiektach
1.+(3) #=> 4
10.* 5 #=> 50

# Wartości specjalne są obiektami
nil # To na prawdę jest niczym
true # prawda
false # fałsz

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Równość
1 == 1 #=> true
2 == 1 #=> false

# Nierówność
1 != 1 #=> false
2 != 1 #=> true

# jedyną 'fałszywą' wartością poza false, jest nil 

!nil   #=> true
!false #=> true
!0     #=> false

# Więcej porównań
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Operatory logiczne
true && false #=> false
true || false #=> true
!true #=> false

# Istnieją alternatywne wersje operatorów logicznych ze znacznie mniejszym
# pierwszeństwem. Używane są by kontrolować wyrażenia w łańcuchach wyrażeń
# aż jedno z nich wróci true lub false.

# `zrob_cos_innego` wywołaj tylko wtedy gdy `zrob_cos` zakończy się sukcesem.
zrob_cos_innego() and zrob_cos()
# `log_error` wywołaj tylko wtedy gdy `zrob_cos` nie zakończy się sukcesem.
zrob_cos() or log_error()


# Stringi są obiektami

'Jestem stringiem.'.class #=> String
"Ja również jestem stringiem.".class #=> String

wypelnienie = 'użyć interpolacji stringa'
"Potrafię #{wypelnienie} używając podwójnych cudzysłowów."
#=> "Potrafię użyć interpolacji stringa używając podwójnych cudzysłowów."

# Staraj się zapisywać stringi za pomocą apostrof, zamiast cudzysłowów tam, gdzie to możliwe
# Cudzysłowy wykonują dodatkowe wewnętrzne operacje


# Łączenie stringów, ale nie liczb
'hej ' + 'świecie'  #=> "hej świecie"
'hej ' + 3 #=> TypeError: can't convert Fixnum into String
'hej ' + 3.to_s #=> "hej 3"

# Łączenie stringów i operatorów
'hej ' * 3 #=> "hej hej hej "

# Dodawanie do stringa
'hej' << ' świecie' #=> "hej świecie"

# wydrukowanie wartości wraz z nową linią na końcu
puts "Drukuję!"
#=> Drukuję!
#=> nil

# wydrukowanie wartości bez nowej linii na końcu 
print "Drukuję!"
#=> Drukuję! => nill

# Zmienne
x = 25 #=> 25
x #=> 25

# Zauważ, że przypisanie zwraca przypisywaną wartość
# To znaczy, że możesz wykonać wielokrotne przypisanie:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Zwyczajowo, używaj notacji nazwa_zmiennej dla nazw zmiennych
nazwa_zmiennej = true

# Używaj opisowych nazw zmiennych
sciezka_do_projektu = '/dobra/nazwa/'
sciezka = '/zla/nazwa/'

# Symbole (są obiektami)
# Symbole są niezmiennymi, wielokrotnie używanymi stałymi reprezentowanymi wewnętrznie jako 
# liczby całkowite. Często używane są zamiast stringów w celu wydajniejszego przekazywania danych

:oczekujacy.class #=> Symbol

status = :oczekujacy

status == :oczekujacy #=> true

status == 'oczekujacy' #=> false

status == :zatwierdzony #=> false

# Tablice

# To jest tablica
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Tablice mogą zwierać różne typy danych

[1, 'hej', false] #=> [1, "hej", false]

# Tablice mogę być indeksowane
# Od początku
tablica[0] #=> 1
tablica.first #=> 1
tablica[12] #=> nil

# Podobnie jak przy arytmetyce, dostęp poprzez [zmienna]
# jest tylko czytelniejszą składnią
# dla wywoływania metody [] na obiekcie
tablica.[] 0 #=> 1
tablica.[] 12 #=> nil

# Od końca
tablica[-1] #=> 5
tablica.last #=> 5

# Z początkowym indeksem i długością
tablica[2, 3] #=> [3, 4, 5]

# Odwrotność tablicy
a=[1,2,3]
a.reverse! #=> [3,2,1]

# Lub zakres
array[1..3] #=> [2, 3, 4]

# Dodawanie do tablicy w taki sposób
tablica << 6 #=> [1, 2, 3, 4, 5, 6]
# Lub taki
tablica.push(6) #=> [1, 2, 3, 4, 5, 6]

# Sprawdzanie, czy tablica zawiera element
tablica.include?(1) #=> true

# Hasze są Ruby'owymi podstawowymi słownikami z parami klucz/wartość.
# Hasze są zapisywane za pomocą nawiasów klamrowych
hasz = { 'kolor' => 'zielony', 'numer' => 5 }

hasz.keys #=> ['kolor', 'numer']

# Można szybko sprawdzić zawartość hasza za pomocą kluczy:
hasz['kolor'] #=> 'zielony'
hasz['numer'] #=> 5

# Sprawdzenie wartośći dla nieistniejącego klucza zwraca nil:
hasz['nic tutaj nie ma'] #=> nil

# Od wersji 1.9, Ruby posiada specjalną składnię, gdy używamy symboli jako kluczy:

nowy_hasz = { stan: 3, akcja: true }

nowy_hasz.keys #=> [:stan, :akcja]

# Sprawdzenie istnienia kluczy i wartości w haszu
new_hash.has_key?(:defcon) #=> true
new_hash.has_value?(3) #=> true

# Wskazówka: Zarówno tablice, jak i hasze, są policzalne
# Współdzielą wiele metod takich jak each, map, count, i inne

# Instrukcje warunkowe

if true
  'wyrażenie if'
elsif false
  'wyrażenie if, opcjonalne'
else
  'wyrażenie else, również opcjonalne'
end

for licznik in 1..5
  puts "powtórzenie #{licznik}"
end
#=> powtórzenie 1
#=> powtórzenie 2
#=> powtórzenie 3
#=> powtórzenie 4
#=> powtórzenie 5

# JEDNAKŻE, Nikt nie używa pętli for.
# Zamiast tego, powinno się używać metody "each" i podawać jej blok.
# Blok jest kawałkiem kodu, który możesz podać metodzie podobnej do "each".
# Jest analogiczny do wyrażeń lambda, funkcji anonimowych lub zamknięć w innych
# językach programowania.
#
# Metoda "each" danego zakresu, wykonuje blok dla każdego elementu w zakresie.
# Do bloku zostaje przekazany licznik jako parametr.
# Wykonanie metody "each" z przekazaniem bloku wygląda następująco:

(1..5).each do |licznik|
  puts "powtórzenie #{licznik}"
end
#=> powtórzenie 1
#=> powtórzenie 2
#=> powtórzenie 3
#=> powtórzenie 4
#=> powtórzenie 5

# Możesz również otoczyć blok nawiasami klamrowymi:
(1..5).each { |licznik| puts "powtórzenie #{licznik}" }

# Zawartość struktur danych również może być powtarzana używając each.
tablica.each do |element|
  puts "#{element} jest częścią tablicy"
end
hasz.each do |klucz, wartosc|
  puts "#{klucz} jest #{wartosc}"
end

# Jeśli nadal potrzebujesz indeksum, możesz użyć "each_with_index" i zdefiniować 
# zmienną odpowiadającą indeksowi
tablica.each_with_index do |element, indeks|
  puts "#{element} jest numerem #{indeks} w tablicy"
end

licznik = 1
while licznik <= 5 do
  puts "powtórzenie #{licznik}"
  licznik += 1
end
#=> powtórzenie 1
#=> powtórzenie 2
#=> powtórzenie 3
#=> powtórzenie 4
#=> powtórzenie 5

# W Ruby istnieje dużo pomocnych funkcji wykonujących pętle,
# na przykład "map", "reduce", "inject" i wiele innych. Map,
# w każdym wywołaniu, pobiera tablicę, na której wykonuję pętlę,
# wykonuje kod zapisany za pomocą bloku i zwraca całkowicie nową tablicę.
tablica = [1,2,3,4,5]
podwojone = tablica.map do |element|
  element * 2
end
puts podwojona
#=> [2,4,6,8,10]
puts tablica
#=> [1,2,3,4,5]

ocena = 2

case ocena
when 1
  puts 'Dobra robota, masz wolne'
when 2
  puts 'Następnym razem będziesz miał więcej szczęścia'
when 3
  puts 'Możesz to zrobić lepiej'
when 4
  puts 'Przebrnąłeś'
when 5
  puts 'Oblałeś!'
else
  puts 'Inny system oceniania?'
end
#=> "Następnym razem będziesz miał więcej szczęścia"

# case może również użwać zakresów
ocena = 82
case ocena
when 90..100
  puts 'Hurra!'
when 80...90
  puts 'Dobra robota'
else
  puts 'Oblałeś!'
end
#=> "Dobra robota"

# obsługa błędów:
begin
  # kod, który może wywołać wyjątek
  raise NoMemoryError, 'Zabrakło pamięci.'
rescue NoMemoryError => zmienna_wyjatku
  puts 'Został wywołany NoMemoryError', zmienna_wyjatku
rescue RuntimeError => inna_zmienna_wyjatku
  puts 'Teraz został wywołany RuntimeError'
else
  puts 'To zostanie uruchomione, jeśli nie wystąpi żaden wyjątek'
ensure
  puts 'Ten kod wykona się zawsze'
end

# Funkcje

def podwojenie(x)
  x * 2
end

# Funkcje (i wszystkie bloki) zawsze zwracają wartość ostatniego wyrażenia
podwojenie(2) #=> 4

# Okrągłe nawiady są opcjonalne, gdy wynik jest jednoznaczny
podwojenie 3 #=> 6

podwojenie podwojenie 3 #=> 12

def suma(x, y)
  x + y
end

# Argumenty metod są oddzielone przecinkami
suma 3, 4 #=> 7

suma suma(3, 4), 5 #=> 12

# yield
# Wszystkie metody mają ukryty, opcjonalny parametr bloku,
# który może być wykonany używając słowa kluczowego 'yield'

def otoczenie
  puts '{'
  yield
  puts '}'
end

otoczenie { puts 'hej świecie' }

# {
# hej świecie
# }


# Możesz przekazać blok do funkcji
# "&" oznacza referencję to przekazanego bloku
def goscie(&blok)
  blok.call 'jakis_argument'
end

# Możesz przekazać listę argumentów, które będę przekonwertowane na tablicę
# Do tego służy operator ("*")
def goscie(*tablica)
  tablica.each { |gosc| puts gosc }
end

# Definiowanie klas używając słowa kluczowego class
class Czlowiek

  # Zmienna klasowa. Jest współdzielona przez wszystkie instancje tej klasy.
  @@gatunek = 'H. sapiens'

  # Podstawowe inicjalizowanie
  def initialize(imie, wiek = 0)
    # Przypisanie argumentu do zmiennej danej instancji o nazwie "imie"
    @imie = imie
    # Jeśli nie podano wieku, zostanie użyta domyślna wartość z listy argumentów.
    @wiek = wiek
  end

  # Podstawowa metoda przypisująca wartość
  def imie=(imie)
    @imie = imie
  end

  # Podstawowa metoda pobierająca wartość
  def imie
    @imie
  end

  # Powyższa funkcjonalność może być zastąpiona używając metody attr_accessor w taki sposób
  attr_accessor :imie

  # Metody przypisujące/pobierające mogą być stworzone indywidualnie
  attr_reader :imie
  attr_writer :imie

  # Metody klasowe używają self aby odróżnić się od metody instancji.
  # To może być wywołane na klasie, nie na instancji.
  def self.powiedz(wiadomosc)
    puts wiadomosc
  end

  def gatunek
    @@gatunek
  end
end


# Tworzenie instancji klasy
jim = Czlowiek.new('Jim Halpert')

dwight = Czlowiek.new('Dwight K. Schrute')

# Wywołajmy parę metod
jim.gatunek #=> "H. sapiens"
jim.imie #=> "Jim Halpert"
jim.imie = "Jim Halpert II" #=> "Jim Halpert II"
jim.imie #=> "Jim Halpert II"
dwight.gatunek #=> "H. sapiens"
dwight.imie #=> "Dwight K. Schrute"

# Wywołanie metody klasowej
Czlowiek.powiedz('Cześć') #=> "Cześć"

# Zasięg zmiennej jest definiowany poprzez jej nazwę.
# Zmienne, które zaczynają się na $ mają zasięg globalny
$zmienna = "Jestem zmienną globalną"
defined? $zmienna #=> "global-variable"

# Zmienne zczynające się na @ mają zasięg danej instancji
@zmienna = "Jestem zmienną instancji"
defined? @zmienna #=> "instance-variable"

# Zmienne, które zaczynają się na @@ mają zasięg danej klasy
@@zmienna = "Jestem zmienną klasową"
defined? @@zmienna #=> "class variable"

# Zmienne, które zaczynają się na dużą literę, są stałymi
Zmienna = "Jestem stałą"
defined? Zmienna #=> "constant"

# Klasa jest również obiektem w ruby. Może więc mieć zmienne instancji.
# Zmienna klasowa może być współdzielona między klasą i jej potomstwem.

# podstawowa klasa
class Czlowiek
  @@cokolwiek = 0

  def self.cokolwiek
    @@cokolwiek
  end

  def self.cokolwiek=(wartosc)
    @@cokolwiek = wartosc
  end
end

# klasa pochodna
class Pracownik < Czlowiek
end

Czlowiek.cokolwiek # 0
Pracownik.cokolwiek # 0

Czlowiek.cokolwiek = 2 # 2
Pracownik.cokolwiek # 2

# Zmienna instancji danej klasy nie jest współdzielona przez jej potomstwo.

class Czlowiek
  @cos = 0

  def self.cos
    @cos
  end

  def self.cos=(wartosc)
    @cos = wartosc
  end
end

class Doktor < Czlowiek
end

Czlowiek.cos # 0
Doktor.cos # nil

module PrzykladowyModul
  def cokolwiek
    'cokolwiek'
  end
end

# Włączanie modułów łączy ich metody z metodami instancji klasy
# Rozszerzanie modułów łączy ich metody z metodami klasy

class Osoba
  include PrzykladowyModul
end

class Ksiazka
  extend PrzykladowyModul
end

Osoba.cokolwiek         # => NoMethodError: undefined method `cokolwiek' for Osoba:Class
Osoba.new.cokolwiek     # => 'cokolwiek'
Ksiazka.cokolwiek       # => 'cokolwiek'
Ksiazka.new.cokolwiek   # => NoMethodError: undefined method `cokolwiek'

# Gdy włączamy lub rozszerzamy muduły, wykonywane są tzw. wywołania zwrotne

module PrzykladowyModul
  def self.included(baza)
    baza.extend(MotodyKlasowe)
    baza.send(:include, MetodyInstancji)
  end

  module MotodyKlasowe
    def cos
      'cos'
    end
  end

  module MetodyInstancji
    def xyz
      'xyz'
    end
  end
end

class Cokolwiek
  include PrzykladowyModul
end

Cokolwiek.cos     # => 'cos'
Cokolwiek.xyz     # => NoMethodError: undefined method `xyz'
Cokolwiek.new.cos # => NoMethodError: undefined method `cos'
Cokolwiek.new.xyz # => 'qux'
```

## Dodatkowe źródła
### Polskie

- [Dokumentacja](https://www.ruby-lang.org/pl/documentation/quickstart/)

### Angielskie

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - A variant of this reference with in-browser challenges.
- [Official Documentation](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - An older [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) is available online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - A community-driven Ruby coding style guide.
