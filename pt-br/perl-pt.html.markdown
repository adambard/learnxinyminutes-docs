---
name: perl
category: language
language: perl
filename: learnperl-pt.pl
contributors:
    - ["Korjavin Ivan", "http://github.com/korjavin"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
---

Perl 5 é, uma linguagem de programação altamente capaz, rica em recursos, com mais de 25 anos de desenvolvimento.

Perl 5 roda em mais de 100 plataformas, de portáteis a mainframes e é adequada tanto para prototipagem rápida, quanto em projetos de desenvolvimento em grande escala.

```perl
# Comentários de uma linha começam com um sinal de número.

#### Tipos de variáveis em Perl

# Variáveis iniciam com um sigilo, que é um símbolo que mostra o tipo.
# Um nome de variável válido começa com uma letra ou sublinhado,
# seguido por qualquer número de letras, números ou sublinhados.

### Perl has three main variable types: $scalar, @array, e %hash.

## Scalars
# Um scalar representa um valor único:
my $animal = "camelo";
my $resposta = 42;

# Valores scalar podem ser strings, inteiros ou números ponto-flutuantes e
# Perl vai automaticamente converter entre eles quando for preciso.

## Arrays
# Um array representa uma lista de valores:
my @animais = ("camelo", "vaca", "boi");
my @números = (23, 42, 69);
my @misturado   = ("camelo", 42, 1.23);

## Hashes
# Um hash representa um conjunto de pares chave/valor:

my %fruta_cor = ("maçã", "vermelho", "banana", "amarelo");

# Você pode usar o espaço em branco e o operador "=>" para colocá-los de
# maneira mais agradável:

my %fruta_cor = (
  maçã  => "vermelho",
  banana => "amarelo",
);

# Scalars, arrays and hashes são documentados mais profundamentes em perldata.
# (perldoc perldata).

# Mais tipos de dados complexos podem ser construídas utilizando referências,
# o que permite que você crie listas e hashes dentro de listas e hashes.

#### Condicionais e construtores de iteração

# Perl possui a maioria das construções condicionais e de iteração habituais.

if ($var) {
  ...
} elsif ($var eq 'bar') {
  ...
} else {
  ...
}

unless (condição) {
  ...
}
# Isto é fornecido como uma versão mais legível de "if (!condition)"

# A forma Perlish pós-condição
print "Yow!" if $zippy;
print "Nós não temos nenhuma banana" unless $bananas;

#  while
while (condição) {
  ...
}

# for
for (my $i = 0; $i < $max; $i++) {
  print "valor é $i";
}

for (my $i = 0; $i < @elements; $i++) {
  print "Elemento atual é " . $elements[$i];
}

for my $element (@elements) {
  print $element;
}

# implícito

for (@elements) {
  print;
}

#### Expressões regulares

# O suporte a expressões regulares do Perl é ao mesmo tempo amplo e profundo,
# e é objeto de longa documentação em perlrequick, perlretut, e em outros
# lugares. No entanto, em suma:

# Casamento simples
if (/foo/)       { ... }  # verdade se $_ contém "foo"
if ($a =~ /foo/) { ... }  # verdade se $a contém "foo"

# Substituição simples

$a =~ s/foo/bar/;         # substitui foo com bar em $a
$a =~ s/foo/bar/g;        # substitui TODAS AS INSTÂNCIAS de foo com bar em $a

#### Arquivos e I/O

# Você pode abrir um arquivo para entrada ou saída usando a função "open()".

open(my $in,  "<",  "input.txt")  ou desistir "Não pode abrir input.txt: $!";
open(my $out, ">",  "output.txt") ou desistir "Não pode abrir output.txt: $!";
open(my $log, ">>", "my.log")     ou desistir "Não pode abrir my.log: $!";

# Você pode ler de um arquivo aberto usando o operador "<>". No contexto
# scalar, ele lê uma única linha do arquivo, e em contexto de lista lê o
# arquivo inteiro, atribuindo cada linha a um elemento da lista:

my $linha  = <$in>;
my @linhas = <$in>;

#### Escrevendo subrotinas

# Escrever subrotinas é fácil:

sub logger {
  my $mensagem = shift;

  open my $arquivo, ">>", "my.log" or die "Não poderia abrir my.log: $!";

  print $arquivo $ensagem;
}

# Agora nós podemos usar a subrotina como qualquer outra função construída:

logger("Nós temos uma subrotina de log!");
```

#### Usando módulos Perl

Módulos Perl provê uma lista de recursos para lhe ajudar a evitar redesenhar
a roda, e tudo isso pode ser baixado do CPAN (http://www.cpan.org/). Um número
de módulos populares podem ser incluídos com a própria distribuição do Perl.

perlfaq contém questões e respostas relacionadas a muitas tarefas comuns, e frequentemente provê sugestões para um bom números de módulos CPAN.

#### Leitura Adicional

 - [perl-tutorial](http://perl-tutorial.org/)
 - [Learn at www.perl.com](http://www.perl.org/learn.html)
 - [perldoc](http://perldoc.perl.org/)
 - and perl built-in : `perldoc perlintro`
