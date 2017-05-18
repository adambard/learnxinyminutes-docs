---
language: ruby
lang: pt-br
filename: learnruby-pt.rb
contributors:
  - ["Bruno Henrique - Garu", "http://garulab.com"]
  - ["Jean Matheus Souto", "http://jeanmatheussouto.github.io"]
translators:
  - ["Katyanna Moura", "https://twitter.com/amelie_kn"]
  - ["Alan Peterson Carvalho Silva", "https://twitter.com/DemonKart"]
---

```ruby
# Isso é um comentário

=begin
Isso é um comentário multilinha
Ninguém os usa
Você não deve usar também
=end

# Primeiro e principal: Tudo é um objeto.

# Números são objetos

3.class #=> Fixnum

3.to_s #=> "3"


# Um pouco de aritmética básica

1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Aritmética é apenas açúcar sintático
# para chamar um método de um objeto
1.+(3) #=> 4
10.* 5 #=> 50

# Valores especiais são objetos
nil # Nada para ver aqui
true # verdadeiro
false # falso

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Igualdade
1 == 1 #=> true
2 == 1 #=> false

# Desigualdade
1 != 1 #=> false
2 != 1 #=> true
!true  #=> false
!false #=> true

# além de 'false', 'nil' é o único outro valor falso

!nil   #=> true
!false #=> true
!0     #=> false

# Mais comparações
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Strings são objects

'Eu sou uma string'.class #=> String
"Eu também sou uma string".class #=> String

placeholder = "usar interpolação de string"
"Eu posso #{placeholder} quando estiver usando aspas duplas"
#=> "Eu posso usar insterpolação de string quando estiver usando aspas duplas"

# imprime para output (saída)
puts "Estou imprimindo"

# Variáveis
x = 25 #=> 25
x #=> 25

# Note que uma atribuição retorna o valor atribuido
# Isso significa que você pode fazer múltiplas atribuições:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Por convenção, use snake_case para nomes de variáveis
snake_case = true

# Use nomes de variáveis descritivos
caminho_para_a_raiz_do_projeto = '/bom/nome/'
caminho = '/nome/ruim/'

# Símbolos (são objetos)
# Símbolos são imutáveis, são constantes reutilizáveis representados
# internamente por um valor inteiro. Eles são frequentemente usados no
# lugar de strings para transmitir com eficiência os valores específicos
# e significativos

:pendente.class #=> Symbol

status = :pendente

status == :pendente #=> true

status == 'pendente' #=> false

status == :aprovado #=> false

# Arrays

# Isso é um array
[1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Arrays podem conter diferentes tipos de itens

array = [1, "Oi", false] #=> => [1, "Oi", false]

# Arrays podem ser indexados
# a partir do começo
array[0] #=> 1
array[12] #=> nil

# Como aritmética, o acesso via [var]
# é apenas açúcar sintático
# para chamar o método [] de um objeto
array.[] 0 #=> 1
array.[] 12 #=> nil

# a partir do final
array[-1] #=> 5

# Com um índice de começo e fim
array[2, 4] #=> [3, 4, 5]

# Ou com um intervalo de valores
array[1..3] #=> [2, 3, 4]

# Adicionar a um array como este
array << 6 #=> [1, 2, 3, 4, 5, 6]

# Hashes são o principal dicionário de Ruby com pares de chaves(keys)/valor(value).
# Hashes são simbolizados com chaves "{}"
hash = {'cor' => 'verde', 'numero' => 5}

hash.keys #=> ['cor', 'numero']

# Hashes podem ser rapidamente pesquisados pela chave (key)
hash['cor'] #=> 'verde'
hash['numero'] #=> 5

# Procurar em um hash por uma chave que não existe retorna nil:
hash['nada aqui'] #=> nil

# Interar sobre hashes com o método #each:

hash.each do |k, v|
  puts "#{k} é #{v}"
end

# Desde o Ruby 1.9, temos uma sintaxe especial quando usamos símbolos como chaves (keys)

novo_hash = {defcon: 3, acao: true}

novo_hash.keys #=> [:defcon, :acao]

# Dica: Tanto Arrays quanto Hashes são Enumerable.
# Eles compartilham um monte de métodos úteis como each, map, count e mais

# Estruturas de controle

if true
  "Se verdadeiro"
elsif false
 "else if, opcional"
else
 "else, também é opcional"
end

for contador in 1..5
  puts "interação #{contador}"
end
#=> contador 1
#=> contador 2
#=> contador 3
#=> contador 4
#=> contador 5

# PORÉM
# Ninguém usa para loops
# Use "each" em vez, dessa forma:

(1..5).each do |contador|
  puts "interação #{contador}"
end
#=> contador 1
#=> contador 2
#=> contador 3
#=> contador 4
#=> contador 5

contador = 1
while contador <= 5 do
  puts "interação #{contador}"
  contador += 1
end
#=> contador 1
#=> contador 2
#=> contador 3
#=> contador 4
#=> contador 5

grau = 'B'

case grau
when 'A'
  puts "Um longo caminho a percorrer, pequeno gafanhoto"
when 'B'
  puts "Melhor sorte da próxima vez"
when 'C'
  puts "Você pode fazer melhor"
when 'D'
  puts "Scraping through"
when 'F'
  puts "Você falhou"
else
  puts "Alternative grading system, eh?"
end

# Funções

def dobrar(x)
  x * 2
end

# Funções (e todos os blocos) retornam implicitamente o valor da última linha
dobrar(2) #=> 4

# Parênteses são opicionais onde o resultado é claro
dobrar 3 #=> 6

dobrar dobrar 3 #=> 12

def somar(x,y)
  x + y
end

# Argumentos de métodos são separados por uma vírgula
somar 3, 4 #=> 7

somar(3,4), 5 #=> 12

# yield
# Todos os métodos possuem implicitamente um paramêtro opcional que é um bloco
# ele pode ser chamado com a palavra chave 'yield'

def ao_redor
  puts "{"
  yield
  puts "}"
end

ao_redor { puts 'Olá mundo' }

# {
# Olá mundo
# }


# Define uma classe com a palavra chave 'class'

class Humano

  # Uma variável de classe. Ela é compartilhada por todas as instâncias dessa classe
  @@especies = "H. sapiens"

  # Inicialização básica (contructor)
  def initialize(nome, idade=0)
    # Atribui o argumento para a variável de instância "nome" do objeto
    @nome = nome
    # Se a idade não for passada, nós definimos um valor padrão na lista de argumentos
    @idade = idade
  end

  # Método básico para atribuir valor
  def nome=(nome)
    @nome = nome
  end

  # Método básico de resgatar valor
  def nome
    @nome
  end

  # Um método de classe usa a palavra chave self para se diferenciar dos métodos de instância.
  # Ele só pode ser chamado na classe, não na instancia
  def self.diz(msg)
    puts "#{msg}"
  end

  def especies
    @@especies
  end

end


# Instanciando uma classe
jim = Humano.new("Jim Halpert")

dwight = Humano.new("Dwight K. Schrute")

# Vamos chamar um par de métodos
jim.especies #=> "H. sapiens"

jim.nome #=> "Jim Halpert"

jim.nome = "Jim Halpert II" #=> "Jim Halpert II"

jim.nome #=> "Jim Halpert II"

dwight.especies #=> "H. sapiens"

dwight.nome #=> "Dwight K. Schrute"

# Chamar o método de classe
Humano.diz("Oi") #=> "Oi"

# Uma classe também é objeto em Ruby. Então uma classe pode possuir variável de instância
# Variáveis de classe são compartilhadas entre a classe e todos os seus descendentes.


# Classe base
class Humano
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(value)
    @@foo = value
  end
end

#  classe filha
class Trabalhador < Humano
end

Humano.foo # 0
Trabalhador.foo # 0

Humano.foo = 2 # 2
Trabalhador.foo # 2

# Uma variável de instância não é compartilhada por suas classes descendentes.

class Humano
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(value)
    @bar = value
  end
end

class Doutor < Humano
end

Humano.bar # 0
Doutor.bar # nil

---

module ModuloDeExemplo
  def foo
    'foo'
  end
end

# Incluir (include) módulos conecta seus métodos às instâncias da classe 
# Herdar (extend) módulos conecta seus métodos à classe em si

class Pessoa
  include ExemploDeModulo
end

class Livro
  extend ExemploDeModulo
end

Pessoa.foo     # => NoMethodError: undefined method `foo' for Pessoa:Class
Pessoa.new.foo # => 'foo'
Livro.foo       # => 'foo'
Livro.new.foo   # => NoMethodError: undefined method `foo'

# Callbacks são executados ao incluir e herdar um módulo

module ExemploDeConceito
  def self.included(base)
    base.extend(MetodosDeClasse)
    base.send(:include, MetodosDeInstancia)
  end

  module MetodosDeClasse
    def bar
      'bar'
    end
  end

  module MetodosDeInstancia
    def qux
      'qux'
    end
  end
end

class Algo
  include ExemploDeConceito
end

Algo.bar     # => 'bar'
Algo.qux     # => NoMethodError: undefined method `qux'
Algo.new.bar # => NoMethodError: undefined method `bar'
Algo.new.qux # => 'qux'
```

## Recursos adicionais

- [Aprenda Ruby com desafios](http://www.learneroo.com/modules/61/nodes/338) - Uma coleção de desafios para testar a linguagem.
- [Documentação oficial](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby a partir de outras linguagens](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/)- Um mais antigo [free edition](http://ruby-doc.com/docs/ProgrammingRuby/) e tambem uma versão online disponível.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - Uma versão colaborativa de um *style-guide*
