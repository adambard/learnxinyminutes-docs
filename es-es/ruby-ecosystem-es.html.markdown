---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]
translators:
    - ["Ale Mohamad", "http://twitter.com/alemohamad"]
lang: es-es
---

Las personas que usan Ruby en general tienen una tendencia a instalar diferentes
versiones de Ruby, administrar sus paquetes (o gemas), y gestionar las
dependencias de sus gemas.

## Gestores de Ruby

Algunas plataformas ya tienen Ruby pre-instalado o disponible como un paquete
propio. Muchos rubystas no utilizan estas versiones, o si lo hacen, solo lo
utilizan para preparar otra instalación o implementación de Ruby. En lugar de
eso, los rubystas tienden a instalar un gestor de Ruby para poder instalar
diferentes versiones y poder cambiar dependiendo del entorno de cada proyecto.

Los siguientes son gestores populares de entorno de Ruby:

* [RVM](https://rvm.io/) - Instala y cambia versiones de Ruby. Además RVM tiene
  el concepto de gemsets para aislar complemtante entornos de proyectos.
* [ruby-build](https://github.com/sstephenson/ruby-build) - Solo instala
  versiones de Ruby. Se utiliza para tener un control más fino sobre las
  versiones instaladas de Ruby.
* [rbenv](https://github.com/sstephenson/rbenv) - Solo se utiliza para cambiar
  la versión de Ruby. Se utiliza junto con ruby-build. Se utiliza para tener un
  control más fino sobre cómo se carga Ruby en el sistema.
* [chruby](https://github.com/postmodern/chruby) - Solo se utiliza para cambiar
  la versión de Ruby. En espíritu es similar a rbenv. No le es tan importante
  como son instaladas las versiones de Ruby.

## Versiones de Ruby

Ruby fue creado por Yukihiro "Matz" Matsumoto, quien se mantiene como una
especie de [BDFL](https://en.wikipedia.org/wiki/Benevolent_Dictator_for_Life),
aunque recientemente está cambiando. Como resultado, la implementación de
referencia de Ruby es llamada MRI (Matz' Reference Implementation), y cuando se
habla de una versión de Ruby, se está haciendo referencia a la versión inicial
de MRI.

Las tres versiones mayores en uso de Ruby son:

* 2.0.0 - Lanzada en Febrero de 2013. La mayoría de las librerías importantes y
  frameworks soportan 2.0.0.
* 1.9.3 - Lanzada en Octubre de 2011. Es la versión que actualmente usan más
  rubystas. Además fue
  [retirada](https://www.ruby-lang.org/en/news/2015/02/23/support-for-ruby-1-9-3-has-ended/)
* 1.8.7 - Ruby 1.8.7 fue
  [retirada](http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

El cambio de 1.8.7 a 1.9.x es un cambio mucho mayor que de 1.9.3 a 2.0.0. Por
ejemplo, la serie 1.9 presentó codificaciones (encodings) y un bytecode VM.
Todavía hay proyectos que utilizan 1.8.7, pero se están convirtiendo en una
pequeña minoría, debido a que la mayor parte de la comunidad está utilizando
como mínimo 1.9.2 o 1.9.3.

## Implementaciones de Ruby

El ecosistema de Ruby disfruta de muchas diferentes implementaciones de Ruby,
cada una con fortalezas únicas y estados de compatibilidad. Para ser claros, las
diferentes implementaciones están escritas en diferentes lenguajes, pero *todas
son Ruby*. Cada implementación tiene hooks especiales y características extra,
pero todas interpretan archivos Ruby de forma normal. Por ejemplo, JRuby está
escrito en Java, pero no necesitás saber de Java para poder utilizarla.

Muy maduras/compatibles:

* [MRI](https://github.com/ruby/ruby) - Escrita en C, es la implementación de
  referencia de Ruby. Por definición es 100% compatible (consigo misma). Las
  otras implementaciones de Ruby mantienen compatibilidad con MRI (ver
  [RubySpec](#rubyspec) más abajo).
* [JRuby](http://jruby.org/) - Escrita en Java y Ruby, esta implementación es
  robusta y bastante veloz. Más importante, la fortaleza de JRuby reside en la
  interoperabilidad con JVM/Java, pudiendo utilizar herramientas, proyectos y
  lenguajes ya existentes en JVM.
* [Rubinius](http://rubini.us/) - Escrita principalmente en Ruby junto con un
  bytecode VM de C++. Además es bastante madura y veloz. Debido a que está
  implementada de forma directa en Ruby, expone varias funcionalidades de VM en
  rubyland.

Medianamente maduras/compatibles:

* [Maglev](http://maglev.github.io/) - Construida sobre Gemstone, una VM de
  Smalltalk. Smalltalk tiene herramientas que son impresionantes, y este
  proyecto intenta llevar eso dentro del desarrollo con Ruby.
* [RubyMotion](http://www.rubymotion.com/) - Lleva Ruby al desarrollo en iOS.

No tan maduras/compatibles:

* [Topaz](http://topazruby.com/) - Escrito en RPython (usando el intérprete
  PyPy), Topaz es bastante joven y no tan compatible. Se muestra prometedor para
  ser una implementación de Ruby de alta performance.
* [IronRuby](http://ironruby.net/) - Escrita en C#, apuntando a la plataforma
  .NET. El trabajo en IronRuby parece haberse detenido desde que Microsoft
  retiró su soporte.

Las implementaciones de Ruby pueden tener su propio número de versión de
release, pero siempre apuntan a una versión específica de MRI para poder tener
compatibilidad. Muchas implementaciones tienen la habilidad de trabajar en
diferentes modos (por ejemplo, modo 1.8 o 1.9) para especificar a qué versión de
MRI están apuntando.

## RubySpec

Muchas implementaciones de Ruby dependen en gran medida de
[RubySpec](http://rubyspec.org/). Ruby no tiene una especificación oficial, por
lo que la comunidad ha escrito especificaciones ejecutables en Ruby para poder
testear la compatibilidad de sus implementaciones con MRI.

## RubyGems

[RubyGems](http://rubygems.org/) es un manejador de paquetes/comunidad de Ruby.
RubyGems viene incluido con Ruby, por lo que no hay necesidad de instalarlo por
separado.

Los paquetes de Ruby son llamados "gemas" ("gems"), y pueden ser alojados por la
comunidad en RubyGems.org. Cada gema contiene su código fuente y algo de
metadata, incluyendo cosas como la versión, dependencias, autor(es), y
licencia(s).

## Bundler

[Bundler](http://bundler.io/) es una herramienta para resolución de dependencias
de gemas. Utiliza un archivo llamado Gemfile en cada proyecto para poder
organizar sus dependencias, y luego poder agregar dichas dependencias y sus
dependencias de forma recursiva. Hace esta acción hasta que resuelve y descarga
todas las dependencias, o se detiene si es que un conflicto aparece.

Bundler eleva un error si encuentra dependencias conflictivas. Por ejemplo, si
la gema A requiere la versión 3 o mayor de la gema Z, pero la gema B requiere la
versión 2, Bundler va a notificarte sobre dicho conflicto. Esto es
extremadamente útil ya que varias gemas hacen referencia a otras gemas (de las
cuales puede referenciar a otras gemas), lo cual puede formar un gran grafo de
dependencias para resolver.

# Testing

Testing es una parte grande dentro de la cultura de Ruby. Ruby incluye su propio
framework de testing llamado minitest (o TestUnit para la versión 1.8.x de
Ruby). Hay varias librerías de testing con diferentes objetivos.

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html) - Framework de testing de Ruby 1.8
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest/rdoc/MiniTest.html) - Framework de testing de Ruby 1.9/2.0
* [RSpec](http://rspec.info/) - Un framework de testing que se focaliza en expresividad
* [Cucumber](http://cukes.info/) - Un framework de testing que utiliza BDD, que parsea tests con formato Gherkin

## Se Amable

La comunidad de Ruby se enorgullece de ser una comunidad abierta, diversa y
acogedora. Matz mismo es extremadamente amigable, y en general la generosidad de
los rubystas es increíble.
