---
language: ruby
filename: learnruby.rb
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
translators:
  - ["Juraj Kostolanský", "http://www.kostolansky.sk"]
lang: sk-sk
---

```ruby
# Toto je komentár

=begin
Toto je viacriadkový komentár
Nikto ho nepoužíva
Ani ty by si nemal
=end

# V prvom rade: Všetko je objekt.

# Čísla sú objekty

3.class #=> Fixnum

3.to_s #=> "3"


# Základná aritmetika
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32

# Aritmetika je iba syntaktickým cukrom
# pre volanie metódy nad objektom
1.+(3) #=> 4
10.* 5 #=> 50

# Špeciálne hodnoty sú objektami
nil # nič
true # pravda
false # lož

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Rovnosť
1 == 1 #=> true
2 == 1 #=> false

# Nerovnosť
1 != 1 #=> false
2 != 1 #=> true

# Okrem samotného false, nil je jedinou ďalšou 'nepravdivou' hodnotou

!nil   #=> true
!false #=> true
!0     #=> false

# Ďalšie porovnania
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Logické operácie
true && false #=> false
true || false #=> true
!true #=> false

# Existujú aj alternatívne verzie logických operátorov s nižšou prioritou.
# Tie sa využívajú ako konštrukcie pre riadenie toku na reťazenie výrazov
# kým jeden z nich nevráti true alebo false.

# `sprav_nieco_ine` sa vykoná ak bude `sprav_nieco` úspešné (vráti true)
sprav_nieco() and sprav_nieco_ine()
# `zaznamenaj_chybu` sa vykoná ak `sprav_nieco` neuspeje (vráti false)
sprav_nieco() or zaznamenaj_chybu()


# Reťazce sú objekty

'Ja som reťazec'.class #=> String
"Ja som tiež reťazec".class #=> String

retazec = 'použiť interpoláciu reťazca'
"Môžem #{retazec} pri použití dvojitých úvodzoviek"
#=> "Môžem použiť interpoláciu reťazca pri použití dvojitých úvodzoviek"

# Preferuj jednoduché úvodzovky pred dvojitými, ak je to možné
# Dvojité úvodzovky totiž vyžadujú ďalšie výpočty

# Kombinuj reťazce, ale nie s číslami
'ahoj ' + 'svet'  #=> "ahoj svet"
'ahoj ' + 3 #=> TypeError: can't convert Fixnum into String
'ahoj ' + 3.to_s #=> "ahoj 3"

# Výpis na štandardný výstup
puts "Píšem!"


# Premenné
x = 25 #=> 25
x #=> 25

# Všimni si, že priradenie vracia priradenú hodnotu
# To umožňuje viacnásobné priradenie:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Podľa konvencie sa pre mená premenných využíva snake_case
snake_case = true

# Používaj správne (opisné) mená premenných
cesta_ku_korenovemu_adresaru = '/dobre/meno/'
cesta = '/zle/meno/'


# Symboly (sú objektami)
# Symboly sú nemenné znovupoužiteľné konštanty, ktoré sú interne
# reprezentované ako číslo. Často sa používajú namiesto reťazcov
# pre efektívnu reprezentáciu špecifickej hodnoty.

:cakajuci.class #=> Symbol

status = :cakajuci

status == :cakajuci #=> true

status == 'cakajuci' #=> false

status == :schvaleny #=> false


# Polia

# Toto je pole
pole = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Polia môžu obsahovať rôzne typy objektov

[1, 'ahoj', false] #=> [1, "ahoj", false]

# Polia môžu byť indexované
# Od začiatku
pole[0] #=> 1
pole[12] #=> nil

# Podobne ako pri aritmetike, prístup prostredníctvom [var]
# je iba syntaktickým cukrom pre volanie metódy [] nad objektom
pole.[] 0 #=> 1
pole.[] 12 #=> nil

# Od konca
pole[-1] #=> 5

# Pomocou počiatočného indexu a dĺžky
pole[2, 3] #=> [3, 4, 5]

# Alebo rozsahom
pole[1..3] #=> [2, 3, 4]

# Pridanie prvku do pola
pole << 6 #=> [1, 2, 3, 4, 5, 6]
# Alebo takto
pole.push(6) #=> [1, 2, 3, 4, 5, 6]

# Skontroluje, či už je objekt v poli
pole.include?(1) #=> true


# Asociatívne polia (hash) sú slovníkmi s dvojicami kľúč-hodnota.
# Asociatívne polia sú označované kučeravými zátvorkami:
asoc_pole = { 'farba' => 'zelena', 'cislo' => 5 }

asoc_pole.keys #=> ['farba', 'cislo']

# V asociatívnych poliach sa rýchlo vyhľadáva pomocou kľúča
asoc_pole['farba'] #=> 'zelena'
asoc_pole['cislo'] #=> 5

# Asking a hash for a key that doesn't exist returns nil:
asoc_pole['nic tu nie je'] #=> nil

# Od verzie Ruby 1.9 existuje špeciálna syntax,
# pri ktorej sa využíva symbol ako kľúč

nove_asoc_pole = { defcon: 3, akcia: true }
nove_asoc_pole.keys #=> [:defcon, :akcia]

# Skontroluje existenciu kľúča a hodnoty v asociatívnom poli
nove_asoc_pole.has_key?(:defcon) #=> true
nove_asoc_pole.has_value?(3) #=> true

# Tip: Polia aj asociatívne polia sú vypočítateľné (Enumerable)
# Zdieľajú veľa užitočných metód ako each, map, count a ďalšie


# Štruktúry riadenia

if true
  'if podmienka'
elsif false
  'else if, voliteľná vetva'
else
  'else, tiež voliteľná vetva'
end

for pocitadlo in 1..5
  puts "iterácia #{pocitadlo}"
end
#=> iterácia 1
#=> iterácia 2
#=> iterácia 3
#=> iterácia 4
#=> iterácia 5

# NIKTO však nepoužíva for cyklus
# Aj ty by si mal preferovať metódu "each" a podať jej blok
# Blok kus kódu, ktorý môžeš podať metódam ako "each"
# Je podobný lambdám alebo anonymným funkciám v iných jazykoch
#
# Metóda "each" pre rozsah spustí blokpre každý element tohto rozsahu
# Blok získava počítadlo ako parameter
# Volanie metódy "each" s blokomvyzerá takto:

(1..5).each do |pocitadlo|
  puts "iterácia #{pocitadlo}"
end
#=> iterácia 1
#=> iterácia 2
#=> iterácia 3
#=> iterácia 4
#=> iterácia 5

# Blok môže byť uzavretý aj v kučeravých záítvorkách:
(1..5).each { |pocitadlo| puts "iterácia #{pocitadlo}" }

# Obsah dátových štruktúr môže byť tiež prechádzaný pomocou metódy "each"
pole.each do |prvok|
  puts "#{prvok} je súčasťou pola"
end
asoc_pole.each do |kluc, hodnota|
  puts "#{kluc} je #{hodnota}"
end

pocitadlo = 1
while pocitadlo <= 5 do
  puts "iterácia #{pocitadlo}"
  pocitadlo += 1
end
#=> iterácia 1
#=> iterácia 2
#=> iterácia 3
#=> iterácia 4
#=> iterácia 5

znamka = 'B'

case znamka
when 'A'
  puts 'Len tak ďalej, chlapče'
when 'B'
  puts 'Viac šťastia nabudúce'
when 'C'
  puts 'Zvládneš to aj lepšie'
when 'D'
  puts 'S odratými ušami'
when 'F'
  puts 'Zlyhal si!'
else
  puts 'Iný systém známkovania, čo?'
end
#=> "Viac šťastia nabudúce"

# prípady (cases) môžu tiež využívať rozsahy
znamka = 82
case znamka
when 90..100
  puts 'Hurá!'
when 80...90
  puts 'Dobrá práca'
else
  puts 'Zlyhal si!'
end
#=> "Dobrá práca"

# Zaobchádzanie s výnimkami
begin
  # kód, ktorý môže vyhodiť výnimku
  raise NoMemoryError, 'Došla ti pamäť.'
rescue NoMemoryError => premenna_vynimky
  puts 'Nastala vynimka NoMemoryError', premenna_vynimky
rescue RuntimeError => ina_premenna_vynimky
  puts 'Nastala vynimka RuntimeError'
else
  puts 'Toto sa spustí, ak nenastala žiadna výnimka'
ensure
  puts 'Táto časť kódu sa spustí vždy'
end

# Funkcie

def zdvojnasob(x)
  x * 2
end

# Funkcie (a bloky) implicitne vracajú hodnotu posledného výrazu
zdvojnasob(2) #=> 4

# Zátvorky sú voliteľné ak je výsledok jednoznačný
zdvojnasob 3 #=> 6

zdvojnasob zdvojnasob 3 #=> 12

def suma(x, y)
  x + y
end

# Argumenty metódy sa oddeľujú čiarkami
suma 3, 4 #=> 7

suma suma(3, 4), 5 #=> 12

# yield
# Všetky metódy majú implicitný, voliteľný parameter bloku
# môže byť zavolaný ako kľúčové slovo 'yield'

def obal
  puts '{'
  yield
  puts '}'
end

obal { puts 'ahoj svet' }

# {
# ahoj svet
# }


# Funkcii môžeš odovzdať blok
# "&" označuje referenciu na tento blok
def hostia(&blok)
  blok.call 'nejake argumenty'
end

# Tiež môžeš odovzdať zoznam argumentov, ktoré sa prevedú na pole
# Na to sa využíva operátor "*"
def hostia(*pole)
  pole.each { |host| puts host }
end


# Trieda sa definuje kľúčovým slovom class
class Clovek

  # Premenná triedy. Je zdieľaná všetkými inštanciami tejto triedy.
  @@druh = 'H. sapiens'

  # Jednoduchý inicializátor
  def initialize(meno, vek = 0)
    # Priradí argument k premennej inštancie "meno"
    @meno = meno
    # Ak nie je uvedený vek, použije sa špecifikovaná predvolená hodnota
    @vek = vek
  end

  # Jednoduchá metóda pre nastavenie hodnoty premennej
  def meno=(meno)
    @meno = meno
  end

  # Jednoduchá metóda pre získanie hodnoty premennej
  def meno
    @meno
  end

  # Vyššie uvedená funkcionalita môže byť zapúzdrená použitím
  # metódy attr_accessor
  attr_accessor :meno

  # Metódy pre nastavenie a získanie hodnoty premennej môžu byť vytvorené
  # aj individuálne
  attr_reader :meno
  attr_writer :meno

  # Metóda triedy používa kľúčové slovo self pre odlíšenie
  # od metód inštancií. Môže byť volaná iba nad triedou, nie inštanciou.
  def self.povedz(sprava)
    puts sprava
  end

  def druh
    @@druh
  end
end


# Vytvorenie inštancie triedy
jim = Clovek.new('Jim Halpert')

dwight = Clovek.new('Dwight K. Schrute')

# Skúsme zavolať zopár metód
jim.druh #=> "H. sapiens"
jim.meno #=> "Jim Halpert"
jim.meno = "Jim Halpert II" #=> "Jim Halpert II"
jim.meno #=> "Jim Halpert II"
dwight.druh #=> "H. sapiens"
dwight.meno #=> "Dwight K. Schrute"

# Volanie metódy triedy
Clovek.povedz('Ahoj') #=> "Ahoj"

# Rozsah platnosti premennej je definovaná spôsobom, akým ju nazveme.
# Premenné začínajúce znakom $ majú globálnu platnosť.
$premenna = "Ja som globálna premenná"
defined? $premenna #=> "global-variable"

# Premenné začínajúce znakom @ majú platnosť v rámci inštancie
@premenna = "Ja som premenná inštancie"
defined? @premenna #=> "instance-variable"

# Premenné začínajúce znakmi @@ majú platnosť v rámci triedy
@@premenna= "Ja som premenná triedy"
defined? @@premenna #=> "class variable"

# Premenné začínajúce veľkým písmenom sú konštanty
Premenna = "Ja som konštanta"
defined? Premenna #=> "constant"

# Trieda je tiež objektom v ruby, takže aj ona môže mať premenné inštancie.
# Premenná triedy je zdieľaná triedou a jej nasledovníkmi.

# Základná trieda
class Clovek
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(hodnota)
    @@foo = hodnota
  end
end

# Odvodená trieda
class Pracovnik < Clovek
end

Clovek.foo # 0
Pracovnik.foo # 0

Clovek.foo = 2 # 2
Pracovnik.foo # 2

# Premenné inštancie triedy nie sú zdieľané jej nasledovníkmi.

class Clovek
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(hodnota)
    @bar = hodnota
  end
end

class Doktor < Clovek
end

Clovek.bar # 0
Doktor.bar # nil

module PrikladModulu
  def foo
    'foo'
  end
end

# Vloženie modulu (include) previaže jeho metódy s inštanciou triedy.
# Rozšírenie modulu (extend) previaže jeho metódy so samotnou triedou.

class Osoba
  include PrikladModulu
end

class Kniha
  extend PrikladModulu
end

Osoba.foo     # => NoMethodError: undefined method `foo' for Osoba:Class
Osoba.new.foo # => 'foo'
Kniha.foo       # => 'foo'
Kniha.new.foo   # => NoMethodError: undefined method `foo'

# Spätné volania sú vykonané pri vložení alebo rozšírení modulu

module PrikladKoncernu
  def self.included(zaklad)
    zaklad.extend(MetodyTriedy)
    zaklad.send(:include, MetodyInstancie)
  end

  module MetodyTriedy
    def bar
      'bar'
    end
  end

  module MetodyInstancie
    def qux
      'qux'
    end
  end
end

class Nieco
  include PrikladKoncernu
end

Nieco.bar     # => 'bar'
Nieco.qux     # => NoMethodError: undefined method `qux'
Nieco.new.bar # => NoMethodError: undefined method `bar'
Nieco.new.qux # => 'qux'
```

## Ďalšie zdroje

- [Nauč sa ruby v príkladoch s úlohami](http://www.learneroo.com/modules/61/nodes/338) - Variácia tejto referencie s úlohami v prehliadači.
- [Oficiálna dokumentácia](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby z iných jazykov](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - Staršia [bezplatná edícia](http://ruby-doc.com/docs/ProgrammingRuby/) je dostupná online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - Komunitná príručka odporúčaných štýlov programovania v Ruby.
