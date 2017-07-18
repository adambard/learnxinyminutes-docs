---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]
translators:
    - ["Xuan-thi Nguyen", "http://github.com/mellenguyen"]
    - ["Sylvain Abélard", "http://github.com/abelards"]
lang: fr-fr

---

Les gens utilisant Ruby adoptent généralement un gestionnaire pour installer
différentes versions de Ruby, gérer leurs paquets (ou gems), et gérer les
dépendances des gems.

## Ruby Managers

Quelques plateformes possèdent Ruby pré-installé ou disponible en tant que
paquet. La plupart des rubyistes ne les utilisent pas, ou si c'est le cas, ne
les utilisent que pour faire démarrer un autre installateur ou implémentation de
Ruby. Les rubyistes tendent plutôt à installer un gestionnaire en Ruby pour installer
et changer entre les différentes et nombreuses versions de Ruby et les
environnements de leurs projets Ruby.

Les gestionnaires d'environnement Ruby les plus populaires sont :

* [RVM](https://rvm.io/) - Installe et navigue entre les rubies. RVM possède
  églement le concept des gemsets pour isoler les environnements de projets
  complètement.
* [ruby-build](https://github.com/sstephenson/ruby-build) - Installe seulement
  les rubies. Utilisez-le pour un contrôle plus fin des installations des
  rubies.
* [rbenv](https://github.com/sstephenson/rbenv) - Navigue seulement entre les
  rubies. Utilisé avec ruby-build. Utilisez-le pour un contrôle plus fin des
  chargements des rubies.
* [chruby](https://github.com/postmodern/chruby) - Navigue seulement entre les
  rubies. Similaire à rbenv. Neutre sur comment les rubies sont installés.

## Versions de Ruby

Ruby a été créé par Yukihiro "Matz" Matsumoto, qui reste quelque peu un
[BDFL](https://fr.wikipedia.org/wiki/Benevolent_Dictator_for_Life), bien que
cela soit récemment en changement. Jusqu'à la standardisation du langage en
2011, l'implémentation de référence de Ruby était appelé MRI (Matz' Reference
Implementation).

Les trois versions majeures de Ruby actuellement utilisées sont :

* 2.0.0 - Sortie en février 2013. La plupart des bibliothèques et frameworks
  gèrent la versions 2.0.0.
* 1.9.3 - Sortie en octobre 2011. Il s'agit de la version que la majorité des
  rubyists utilisent actuellement. [Fin de vie](https://www.ruby-lang.org/en/news/2015/02/23/support-for-ruby-1-9-3-has-ended/)
* 1.8.7 - Sortie en juin 2006. [Fin de vie](http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

Les changements entre 1.8.7 et 1.9.x sont bien plus grands qu'entre 1.9.3
et 2.0.0. Par exemple, les versions 1.9 ont introduit le support des
encodages et d'une VM bytecode ([YARV](https://fr.wikipedia.org/wiki/YARV)).
Il y a toujours des projets sur 1.8.7, mais ils deviennent minoritaires, étant
donné que la majorité de la communauté a migré vers au moins 1.9.2 ou 1.9.3.

## Implémentations Ruby

L'écosystème Ruby comprend de nombreuses implémentations de Ruby, chacune avec
des points forts uniques et différents degrés de compatibilité. Les différentes
implémentations sont écrites dans différents languages.
Chaque implémentation a des "hooks" et des fonctionnalités spécifiques, elles
exécutent cependant très bien des fichiers Ruby classiques.
Par exemple, JRuby est écrit en Java, mais vous n'avez pas besoin de connaître
le Java pour l'utiliser.

Très mature/compatible:

* [MRI](https://github.com/ruby/ruby) - Ecrite en C, c'est l'implémentation de
  référence de Ruby. Elle est par définition 100% compatible (avec elle-même).
  Tous les autres rubies maintiennent la compatibilité avec MRI
  (voir [RubySpec](#rubyspec) à la suite).
* [JRuby](http://jruby.org/) - Écrite en Java et Ruby, cette robuste
  implémentation est assez rapide.
  La force de JRuby réside surtout sur l'interopérabilité JVM/Java, faisant
  levier sur des outils JVM, des projets et des langages existants.
* [Rubinius](http://rubini.us/) - Ecrite principalement en Ruby avec une VM
  bytecode en C++. Egalement mature et rapide. Etant donné qu'elle est
  implémentée en Ruby, elle couvre beaucoup de fonctionnalités de la
  VM dans Ruby.

Mpyennement mature/compatible:

* [Maglev](http://maglev.github.io/) - Basée sur Gemstone, une VM Smalltalk.
  Smalltalk possède quelques outils impressionnants, et ce projet tente
  de les apporter dans le développement Ruby.
* [RubyMotion](http://www.rubymotion.com/) - Ruby pour développement iOS et Android.
* [Opal](http://opalrb.org/) - Compile le Ruby en Javascript

Les implémentations de Ruby peuvent avoir leurs propres numéros de versions,
mais elles ciblent toujours une versions spéficique de MRI pour la
compatibilité.
Beaucoup d'implémentations ont la capacité d'entrer dans différents modes
(par exemple, la version 1.8 ou 1.9) afin de spécifier quelle version de MRI
cibler.

Une liste non exhaustive d'implémentations peut être trouvée [ici (EN)](https://github.com/cogitator/ruby-implementations/wiki/List-of-Ruby-implementations).

## RubySpec

La plupart des implémentations Ruby s'appuient fortement sur [RubySpec](http://rubyspec.org/).
Ruby n'a pas de spécification officielle, c'est pourquoi la commaunité a écrit
des spécifications exécutables en Ruby pour tester la compatibilité de leur
implémentation avec MRI.

## RubyGems

[RubyGems](http://rubygems.org/) est un gestionnaire de paquets communautaire
pour Ruby.
RubyGems est livré avec Ruby, il n'y a donc pas besoin de le télécharger
séparément.

Les paquets Ruby sont appelés des "gems", et peuvent être hébergés par la
communauté à RubyGems.org. Chaque gem contient son code source et quelques
métadatas, includant des choses comme la version, les dépendances,
l(es) auteur(s) et la/les licence(s).

## Bundler

[Bundler](http://bundler.io/) est un outil de résolution de dépendances des gems. Il
utilise le Gemfile d'un projet pour en trouver les dépendances, et récupère
ensuite les dépendances de ces dépendances récursivement. Il déroule cet
algorithme jusqu'à ce que toutes les dépendances soient résolues et
téléchargées, ou s'arrête si un conflit est trouvé.

Bundler lèvera une erreur s'il trouve des conflits de dépendances. Par exemple,
si la gem A recquiert la version 3 ou plus de gem Z, mais que gem B recquiert
seulement la version 2 de la même gem Z, Bundler vous notifiera ce conflit. Cela devient
extrêmement utile, étant donné que beaucoup de gems font référence à d'autres
gems (qui se réfèrent à d'autres gems et ainsi de suite), ce qui peut former un large graphe de
dépendance à résoudre.

# Les tests

Tester fait partie intégrante de la culture Ruby. Ruby fournit son propre
framework de tests unitaires appelé minitest (ou TestUnit pour Ruby
version 1.8.x). Il existe beaucoup de librairies de tests avec des buts
différents.

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html) - Framework de tests intégré de Ruby version 1.8 style "Unit"
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest/rdoc/MiniTest.html) - Framework de tests intégré de Ruby version 1.9/2.0
* [RSpec](http://rspec.info/) - Un framework de tests qui se focalise sur l'expressivité
* [Cucumber](http://cukes.info/) - Un framework de tests BDD ([behaviour-driven development](https://fr.wikipedia.org/wiki/Behavior_driven_development)) qui parse les tests formatés de Gherkin.

## Soyez gentil

La communauté Ruby est fière d'être une communauté ouverte, riche et
accueillante. Matz lui-même est extrêmement sociable, et la générosité des
rubyistes est généralement remarquable.
