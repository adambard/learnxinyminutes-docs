---
category: tool
tool: ruby ecosystem
contributors:
    - ["Jon Smock", "http://github.com/jonsmock"]
    - ["Rafal Chmiel", "http://github.com/rafalchmiel"]
translators:
    - ["Claudson Martins", "http://github.com/claudsonm"]
lang: pt-br

---

Pessoas utilizando Ruby geralmente têm uma forma de instalar diferentes versões
do Ruby, gerenciar seus pacotes (ou gemas) e as dependências das gemas.

## Gerenciadores Ruby

Algumas plataformas possuem o Ruby pré-instalado ou disponível como um pacote.
A maioria dos "rubistas" não os usam, e se usam, é apenas para inicializar outro
instalador ou implementação do Ruby. Ao invés disso, rubistas tendêm a instalar
um gerenciador para instalar e alternar entre diversas versões do Ruby e seus
ambientes de projeto.

Abaixo estão os gerenciadores Ruby mais populares:

* [RVM](https://rvm.io/) - Instala e alterna entre os rubies. RVM também possui
  o conceito de gemsets para isolar os ambientes dos projetos completamente.
* [ruby-build](https://github.com/sstephenson/ruby-build) - Apenas instala os
  rubies. Use este para um melhor controle sobre a instalação de seus rubies.
* [rbenv](https://github.com/sstephenson/rbenv) - Apenas alterna entre os rubies.
  Usado com o ruby-build. Use este para um controle mais preciso sobre a forma
  como os rubies são carregados.
* [chruby](https://github.com/postmodern/chruby) - Apenas alterna entre os rubies.
  A concepção é bastante similar ao rbenv. Sem grandes opções sobre como os
  rubies são instalados.

## Versões do Ruby

O Ruby foi criado por Yukihiro "Matz" Matsumoto, que continua a ser uma espécie
de [BDFL](https://en.wikipedia.org/wiki/Benevolent_Dictator_for_Life), embora
isso esteja mudando recentemente. Como resultado, a implementação de referência
do Ruby é chamada de MRI (Matz' Reference Implementation), e quando você ver uma
versão do Ruby, ela está se referindo a versão de lançamento do MRI.

As três principais versões do Ruby em uso são:

* 2.0.0 - Lançada em Fevereiro de 2013. Maioria das principais bibliotecas e 
  suporte a frameworks 2.0.0.
* 1.9.3 - Lançada em Outubro de 2011. Está é a versão mais utilizada pelos rubistas
  atualmente. Também [aposentada](https://www.ruby-lang.org/en/news/2015/02/23/support-for-ruby-1-9-3-has-ended/).
* 1.8.7 - O Ruby 1.8.7 foi
  [aposentado](http://www.ruby-lang.org/en/news/2013/06/30/we-retire-1-8-7/).

A diferença entre a versão 1.8.7 para 1.9.x é muito maior do que a da 1.9.3 para
a 2.0.0. Por exemplo, a série 1.9 introduziu encodes e uma VM bytecode. Ainda
existem projetos na versão 1.8.7, mas eles estão tornando-se uma pequena minoria
pois a maioria da comunidade migrou para a versão, pelo menos, 1.9.2 ou 1.9.3.

## Implementações Ruby

O ecossistema Ruby conta com várias diferentes implementações do Ruby, cada uma
com pontos fortes e estados de compatibilidade. Para ser claro, as diferentes
implementações são escritas em diferentes linguagens, mas *todas elas são Ruby*.
Cada implementação possui hooks especiais e recursos extra, mas todas elas
executam arquivos normais do Ruby tranquilamente. Por exemplo, JRuby é escrita
em Java, mas você não precisa saber Java para utilizá-la.

Muito maduras/compatíveis:

* [MRI](https://github.com/ruby/ruby) - Escrita em C, esta é a implementação de
  referência do Ruby. Por definição, é 100% compatível (consigo mesma). Todos os
  outros rubies mantêm compatibilidade com a MRI (veja [RubySpec](#rubyspec) abaixo).
* [JRuby](http://jruby.org/) - Escrita em Java e Ruby, esta implementação
  robusta é um tanto rápida. Mais importante ainda, o ponto forte do JRuby é a
  interoperabilidade com JVM/Java, aproveitando ferramentas JVM, projetos, e
  linguagens existentes.
* [Rubinius](http://rubini.us/) - Escrita principalmente no próprio Ruby, com
  uma VM bytecode em C++. Também madura e rápida. Por causa de sua implementação
  em Ruby, ela expõe muitos recursos da VM na rubyland.

Medianamente maduras/compatíveis:

* [Maglev](http://maglev.github.io/) - Construída em cima da Gemstone, uma
  máquina virtual Smalltalk. O Smalltalk possui algumas ferramentas impressionantes,
  e este projeto tenta trazer isso para o desenvolvimento Ruby.
* [RubyMotion](http://www.rubymotion.com/) - Traz o Ruby para o desenvolvimento iOS.

Pouco maduras/compatíveis:

* [Topaz](http://topazruby.com/) - Escrita em RPython (usando o conjunto de
  ferramentas PyPy), Topaz é bastante jovem e ainda não compatível. Parece ser
  promissora como uma implementação Ruby de alta performance.
* [IronRuby](http://ironruby.net/) - Escrita em C# visando a plataforma .NET,
  o trabalho no IronRuby parece ter parado desde que a Microsoft retirou seu apoio.

Implementações Ruby podem ter seus próprios números de lançamento, mas elas
sempre focam em uma versão específica da MRI para compatibilidade. Diversas
implementações têm a capacidade de entrar em diferentes modos (1.8 ou 1.9, por
exemplo) para especificar qual versão da MRI focar.

## RubySpec

A maioria das implementações Ruby dependem fortemente da [RubySpec](http://rubyspec.org/).
Ruby não tem uma especificação oficial, então a comunidade tem escrito
especificações executáveis em Ruby para testar a compatibilidade de suas
implementações com a MRI.

## RubyGems

[RubyGems](http://rubygems.org/) é um gerenciador de pacotes para Ruby mantido
pela comunidade. RubyGems vem com o Ruby, portanto não é preciso baixar separadamente.

Os pacotes do Ruby são chamados de "gemas", e elas podem ser hospedadas pela 
comunidade em RubyGems.org. Cada gema contém seu código-fonte e alguns metadados,
incluindo coisas como versão, dependências, autor(es) e licença(s).

## Bundler

[Bundler](http://bundler.io/) é um gerenciador de dependências para as gemas.
Ele usa a Gemfile de um projeto para encontrar dependências, e então busca as
dependências dessas dependências de forma recursiva. Ele faz isso até que todas 
as dependências sejam resolvidas e baixadas, ou para se encontrar um conflito.

O Bundler gerará um erro se encontrar um conflito entre dependências. Por exemplo, 
se a gema A requer versão 3 ou maior que a gema Z, mas a gema B requer a versão 
2, o Bundler irá notificá-lo que há um conflito. Isso se torna extremamente útil
quando diversas gemas começam a referenciar outras gemas (que referem-se a outras
gemas), o que pode formar uma grande cascata de dependências a serem resolvidas.

# Testes

Testes são uma grande parte da cultura do Ruby. O Ruby vem com o seu próprio 
framework de teste de unidade chamado minitest (ou TestUnit para Ruby versão 1.8.x). 
Existem diversas bibliotecas de teste com diferentes objetivos.

* [TestUnit](http://ruby-doc.org/stdlib-1.8.7/libdoc/test/unit/rdoc/Test/Unit.html) - 
  Framework de testes "Unit-style" para o Ruby 1.8 (built-in)
* [minitest](http://ruby-doc.org/stdlib-2.0.0/libdoc/minitest/rdoc/MiniTest.html) - 
  Framework de testes para o Ruby 1.9/2.0 (built-in)
* [RSpec](http://rspec.info/) - Um framework de testes que foca na expressividade
* [Cucumber](http://cukes.info/) - Um framework de testes BDD que analisa testes Gherkin formatados

## Seja Legal

A comunidade Ruby orgulha-se de ser uma comunidade aberta, diversa, e receptiva.
O próprio Matz é extremamente amigável, e a generosidade dos rubistas em geral
é incrível.
