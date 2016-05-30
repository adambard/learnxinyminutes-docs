---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]
translators:
    - ["Cristian Achille", "http://github.com/blackdev1l/"]
lang: it-it
---

Generalmente chi usa ruby ha l'esigenza di avere differenti versioni di Ruby
installate, gestire le proprie gemme, e le loro dipendenze.

## Manager Ruby

Alcune piattaforme hanno Ruby pre-installato o disponibile come pacchetto.
Molti sviluppatori Ruby non usano questi pacchetti, o se lo fanno, li usano solo
per installare dei manager Ruby, i quali permettono di installare e gestire più
versioni di Ruby in base al progetto su cui si lavora.

Di seguito i più famosi manager Ruby:

* [RVM](https://rvm.io/) - Installa e permette di utilizzare diverse versioni di
  Ruby. RVM Ha anche il concetto di gemsets i quali isolano completamente l'ambiente di sviluppo del progetto.
* [ruby-build](https://github.com/sstephenson/ruby-build) - Installa solamente
  multiple versioni di ruby. Usa questo se vuoi maggior controllo sull'installazione di Ruby.
* [rbenv](https://github.com/sstephenson/rbenv) -
  Permette solo la scelta di quale versione Ruby utilizzare. Usato insieme a ruby-build.
  Utilizza questo per un maggior controllo su quale versione di Ruby utilizzare.
* [chruby](https://github.com/postmodern/chruby) -
  Permette solo la scelta di quale Ruby utilizzare, simile a rbenv.

## Ruby Versions

Ruby fu creato da Yukihiro "Matz" Matsumoto, il [BDFL](https://en.wikipedia.org/wiki/Benevolent_Dictator_for_Life),
(acronimo inglese che sta per "Benevolo dittatore a vita") , seppur
ultimamente non è più del tutto vera; l'implementazione di Ruby
è detta  MRI (Matz' Reference Implementation), e dunque quando si legge di una
versione Ruby, essa si riferisce sempre al rilascio di una MRI

Le tre maggiori versioni di Ruby in uso sono:

* 2.0.0 - Rilasciata nel febbraio 2013. La maggior parte delle librerie e
  framework supportano la 2.0.0
* 1.9.3 - Rilasciata nel ottobre 2011. QUesta è la versione che molti
  svluppatori usano, il supporto è
  [concluso](https://www.ruby-lang.org/en/news/2015/02/23/support-for-ruby-1-9-3-has-ended/)
* 1.8.7 - Il supporto per Ruby 1.8.7 è
  [concluso](http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

I cambiamenti tra la 1.8.7 a la 1.9.x sono maggiori di quelli tra la 1.9.3 a la
2.0.0. Per esempio, nella 1.9 vengono introdotti encodings e bytecode VM.
Esistono ancora dei progetti basati sulla 1.8.7, ma stanno diventando una
minoranza man mano che la community si trasferisce alle versioni 1.92 e
1.9.3

## Ruby Implementations

L'ecosistema Ruby gode di molte implementazioni differenti di Ruby, ognuna con
particolari punti di forza, da chiarire che ogni implementazione è scritta con
un linguaggio diverso, ma esse *sono tutte Ruby*. Ogni implementazione ha
feature extra ma tutte esse possono eseguire file ruby. Per esempio, JRuby è
scritto in Java, ma non devi conoscere java per usarlo.

Implementazioni mature e compatibili:

* [MRI](https://github.com/ruby/ruby) - Scritto in C, Questa è l'implementazione
  standard di Ruby, per definizione è 100% compatibile (con se stessa). Tutte le
  altre implemetazioni mantengono la compatibilità con MRI
  (vedere [RubySpec](#rubyspec) sotto).
* [JRuby](http://jruby.org/) - Scritto in Java e Ruby, Questa implementazione è
  molto veloce e robusta, la forza di JRuby consiste nell'interoperabilità
  tra JVM/Java, permettendo l'utilizzo di struemnti Java già esistenti, progetti
  e linguaggi
* [Rubinius](http://rubini.us/) - Scritto principalmente in Ruby con un
  c++ bytecode VM, molto matura e veloce, permette alcune feature riguardo VM.

Mediamente mature e compatibili:

* [Maglev](http://maglev.github.io/) - Sviluppata sui Gemstone, è una Smalltalk
VM, Smalltalk è degli strumenti molto utili, e questo progetto cerca di portare
questi strumenti nello sviluppo Ruby.
* [RubyMotion](http://www.rubymotion.com/) - Porta ruby nello sviluppo iOS.

Poco mature e compatibili:

* [Topaz](http://topazruby.com/) - Scritto in RPython (usando PyPy come
  toolchain) Topaz è un progetto ancora giovane e non compatibile, ha le
  possibilità di diventare una implementazione Ruby molto performante
* [IronRuby](http://ironruby.net/) - Scritto in C# e prendendo di mira la
  piattaforma .NET, lo sviluppo sembra fermo da quando Microsoft ha rimosso il
  suo supporto.

Le implementazioni Ruby possono avere una propria versione, ma hanno sempre come
target una specifica versione di MRI. Molte implementazioni hanno l'abilità di
selezionare una versione specifica di MRI.

##RubySpec

La maggior parte delle implementazioni Ruby dipendono pesantemente su
[RubySpec](http://rubyspec.org/). Ruby non ha una specifica ufficiale, quindi la
community ha scritto una specifica eseguibile in Ruby per testare la compatibilità
con MRI.

## RubyGems

[RubyGems](http://rubygems.org/) è un package manager gestito dalla communtiy
per Ruby. Rubygems viene installato con Ruby, quindi non c'è bisogno di
scaricarlo separatamente.

I pacchetti Ruby sono chiamate "gemme", e possono essere hostate dalla community
su RubyGems.org . Ogni gemma contiene il codice sorgente e del metadata, tra cui
la versione, le dipendenze, autor* e licenz*.

## Bundler

[Bundler](http://bundler.io/) è un risolvitore di dipendenze, Esso usa il Gemfile
di un progetto per cercare le dipendenze, dopo di che ottiene le dipendenze delle
dipendenze ricorsivamente, Questo procedimento viene eseguito finchè tutte le
dipendenze sono state risolte e scaricate, o si fermerà se un conflitto verrà
trovato.

Bundler genererà un error se troverà dipendenze in conflitto, Per esempio,
se la gemma A richiede la versione 3 o maggiore della gemma Z, ma la gemma B
richiede la versione 2, Bundler ti notificherà del conflitto. Questo diventa
di aiuto nel momento in cui si hanno molte gemme nel progetto, il che porta a
un grande grafo di dipendenza da risolvere.

# Testing

Il testing è un pezzo fondamentale della cultura Ruby, Ruby viene installato con
il proprio testing framework chiamato minitest (O TestUnit per ruby 1.8.x).
Esistono molte librerie con obiettivi differenti

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html) - Testing frameowrk rilasciato insieme a Ruby 1.8.x
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest/rdoc/MiniTest.html) - Testing frameowrk rilasciato insieme a Ruby 1.9/2.0
* [RSpec](http://rspec.info/) - Un testing framework che si concentra nella chiarezza
* [Cucumber](http://cukes.info/) - Un BDD testing framework che estrapola testo formattato in Gherkin

## Sii cordiale

La community Ruby è orgogliosa di essere una communtiy aperta, accogliente e
variegata. Matz stesso è estremamente amichevole, e la generosità degli sviluppatori
Ruby è fantastica.
