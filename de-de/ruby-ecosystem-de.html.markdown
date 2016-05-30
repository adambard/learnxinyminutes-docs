---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]
translators:
  - ["Christian Albrecht", "https://github.com/coastalchief"]
filename: ruby-ecosystem-de.txt
lang: de-de
---

Hier gibt es einen Überblick über die gängigsten Tools zur Verwaltung  
von verschiedenen Ruby Versionen, Gems und Dependencies.  

## Ruby Managers

Einige Betriebssysteme haben bereits eine Ruby Version vorinstalliert  
oder bieten sie als Package zum Download an. Die meisten Rubyisten  
benutzen diese aber eher nicht und wenn, dann um damit einen Ruby  
Manager zu installieren. Damit kann man komfortabel zwischen den  
verschiedenen Versionen hin und herspringen.  

Dies sind die beliebtesten:

* [RVM](https://rvm.io/) - Installiert und wechselt zwischen rubies
  RVM kennt verschiedene Ruby Versionen und hat das Konzept der gemsets,  
  um gem Abhängigkeiten pro Projekt zu managen.  
* [ruby-build](https://github.com/sstephenson/ruby-build)  
  Installiert nur rubies, kann diese aber sehr gut verwalten
* [rbenv](https://github.com/sstephenson/rbenv) - Wechselt Ruby Versionen.
  Wird zusammen mit ruby-build benutzt. Hiermit kann man kontrollieren,  
  wie rubies laden.
* [chruby](https://github.com/postmodern/chruby) - Wechselt Ruby Versionen.
  Ähnlich rbenv.

## Ruby Versionen

Ruby wurde von Yukihiro "Matz" Matsumoto vor gut 20 Jahren veröffentlicht.  
Matz ist nach wie vor in die Entwicklung involviert. Daher kommt auch der  
Name der Referenzimplementierung: MRI (Matz' Reference Implementation).  

Die aktuellste Version ist **2.2.3** und wurde im August 2015 veröffentlicht!  

Hier eine kurze Versionshistorie:

* 2.0.0 - Release im Februar 2013  -- Release zum 20. Geburtstag der Sprache
  [Rubies are forever](http://www.heise.de/developer/artikel/Ruby-2-0-als-Geschenk-zum-20-Geburtstag-1808109.html)
* 1.9.3 - Release im Oktober 2011  
  [End of Life](https://www.ruby-lang.org/en/news/2015/02/23/support-for-ruby-1-9-3-has-ended/)
* 1.8.7 - Release im Juni 2006  
  [End of Life](http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

Die Veränderung zwischen 1.8.7 und 1.9.x war sehr groß und eine Migration  
nicht so einfach möglich. Der Versionssprung auf 2.0.0 war verglichen dazu  
weit weniger dramatisch.  
Beispielsweise hat 1.9. Encodings und eine Bytecode VM eingeführt.  
Es gibt immer noch Projekte die auf der stabilen Version 1.8.7 laufen,  
aber diese sind mittlerweile in der Minderheit. Die meisten Projekte  
laufen auf 1.9.x oder auf 2.x.

## Ruby Implementierungen

Das Ruby Ecosystem beinhaltet viele verschiedene Implementierungen von Ruby,  
jedes mit seinen eigenen Vorteilen und verschiedenen Graden von  
Kompatibilität. Auch wenn alle diese Implementierungen in verschiedenen  
Sprachen geschrieben sind, sind sie doch **alle Ruby**.  
Jede Implementierung bietet neben ihren speziellen Features immer auch  
die Möglichkeit normale ruby Dateien auszuführen.

Am ausgereiftesten und stabilsten:

* [MRI](https://github.com/ruby/ruby) - Geschrieben in C, das ist die Referenz Implementierung.
  Sie ist 100% kompatibel (mit sich selbst ;-). Alle anderen rubies 
  bleiben kompatibel mit MRI (siehe [RubySpec](#rubyspec) weiter unten).
* [JRuby](http://jruby.org/) - Geschrieben in Java and Ruby, Robust und ziemlich schnell.
  Der größte Vorteil von JRuby ist die Interoperabilität mit JVM/Java und damit die  
  Benutzung von Ruby im Java Ecosystem.
* [Rubinius](http://rubini.us/) - Geschrieben in Ruby mit C++ bytecode VM. 
  Auch sehr ausgereift und schnell. 

Mittel ausgereift / kompatibel:

* [Maglev](http://maglev.github.io/) - Baut auf Gemstone, ein Smalltalk VM.
  Dieses Projekt versucht das großartige Smalltalk Tooling in die Ruby Welt  
  zu bringen.
* [RubyMotion](http://www.rubymotion.com/) - Ruby in der iOS Entwicklung.

Weniger ausgereift/kompatibel:

* [Topaz](http://topazruby.com/) - Geschrieben in RPython (via PyPy)
  Topaz ist noch ziemlich jung und versucht die schnellste Implementierung  
  zu werden.
* [IronRuby](http://ironruby.net/) - Geschrieben in C# für die .NET Plaftform  
  Das letzte Release von IronRuby ist mittlerweile 5 Jahre her. 

Die Ruby Implementierungen haben ihre eigenen Versionsnummern, sind aber  
trotzdem immer zu einer MRI Version kompatibel.  
Viele können sogar zwischen verschiedenen Modi wechseln (1.8 mode -> 1.9 mode)

## RubySpec

Die meisten Ruby Implementierungen vertrauen der [RubySpec](http://rubyspec.org/).  
sehr stark. Da Ruby keine offizielle Spezifikation hat, hat die  
Community ausführbare Specs (in Ruby) geschrieben, um so die Kompatibilität  
zur MRI testen zu können.

## RubyGems

[RubyGems](http://rubygems.org/) ist der Community Paket Manager von Ruby.  
RubyGems kommt mit Ruby zusammen, so dass kein extra Tool nötig ist.  
  
Ruby Pakete werden "gems" genannt und könnten auf RubyGems.org  
veröffentlicht werden. Jedes Gem enthält den Source Code und Meta Daten,  
wie die Versionsnummer, weitere Abhängigkeiten, Autoren und Lizenzen.

## Bundler

[Bundler](http://bundler.io/) ist ein Tool um Abhängigkeiten zwischen  
Gems aufzulösen und zu managen. Dazu werden diese in einem gemfile  
zusammengefasst und Bundler kümmert sich darum die Abhängigkeiten  
untereinander rekursiv aufzulösen. Entweder es klappt und alle gems  
konnten runtergeladen werden, oder es wird abgebrochen und  
der Konflikt gemeldet.  
Zum Beispiel:  
Wenn Gem A die Version 3 oder höher von Gem Z braucht, aber Gem B  
von Gem Z die Version 2, dann ist das ein Konflikt.  

# Testing

Test-Driven Development ist ein essentieller Teil der Ruby Kultur.  
Ruby bringt sein eigenes Unit-Test framework mit, minitest. Darüberhinaus  
gibt es noch viele weitere Testframeworks mit unterschiedlichsten Zielen:

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html) - Eingebaut in Ruby 1.8
  "Unit-style" Testframework
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest/rdoc/MiniTest.html) - Eingebaut in Ruby 1.9/2.0
  "Unit-style" Testframework
* [RSpec](http://rspec.info/) - Ein Testframework welches auf verständliche Testdefinition setzt
* [Cucumber](http://cukes.info/) - Ein BDD Testframework welches Gherkin tests parsen kann

## Be Nice
Die Ruby Community ist stolz darauf eine offene, vielfältige und einladende  
Community zu sein. Es gibt viele aktive Ruby User Gruppen und diverse  
Ruby Konferenzen. Matz selbst ist so oft es geht dabei.

* [Euruko](http://www.euruko2015.org)
* [User Groups](https://www.ruby-lang.org/de/community/user-groups/)

