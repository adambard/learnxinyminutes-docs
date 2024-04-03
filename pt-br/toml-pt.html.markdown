---
language: toml
filename: learntoml-pt.toml
contributors:
  - ["Alois de Gouvello", "https://github.com/aloisdg"]
translators:
  - ["Adaías Magdiel", "https://adaiasmagdiel.com/"]
lang: pt-br
---

TOML significa Tom's Obvious, Minimal Language. É uma linguagem de serialização de dados projetada para ser um formato de arquivo de configuração mínimo que é fácil de ler devido à semântica óbvia.

É uma alternativa ao YAML e JSON. Tem como objetivo ser mais amigável para humanos do que JSON e mais simples que YAML. TOML é projetado para mapear de forma inequívoca para uma tabela de hash e deve ser fácil de converter em estruturas de dados em uma ampla variedade de linguagens.

Cuidado, a especificação do TOML ainda passa por muitas mudanças. Até que seja marcado como 1.0, você deve assumir que é instável e agir de acordo. Este documento segue o TOML v0.4.0.

```toml
# Comentários em TOML são feitos desta forma.

###################
# TIPOS ESCALARES #
###################

# Nosso objeto raiz (que continuará por todo o documento) será um mapa,
# que é equivalente a um dicionário, hash ou objeto em outras linguagens.

# A chave, o sinal de igual e o valor precisam estar na mesma linha
# (embora alguns valores possam ser quebrados em várias linhas).
chave = "valor"
string = "Olá"
number = 42
float = 3.14
boolean = true
dateTime = 2002-07-16T20:32:00-03:00
scientificNotation = 1e+12
"chaves podem estar entre aspas" = true # Tanto " quanto ' são aceitáveis
"chaves podem conter" = "letras, números, underscores e hífens"

# Uma chave não pode ser vazia, mas uma chave vazia entre aspas é permitido
"" = "blank" # VÁLIDO mas não é recomendado
'' = 'blank' # VÁLIDO mas não é recomendado

##########
# String #
##########

# Todas as strings precisam ter apenas caracteres UTF-8 válidos.
# Podemos escapar caracteres e alguns deles têm uma sequência de escape compacta.
# Por exemplo: \t adiciona uma tabulação. Leia a spec para conhecer todos.
basicString = "São cercadas por aspas. \"Sou digno de citação\". Nome\tJosé."

multiLineString = """
são cercadas por três aspas
em cada lado e permitem novas linhas."""

literalString = 'são cercadas por aspas simples. Escape de caracteres não é permitido.'

multiLineString = '''
são cercadas por três aspas simples em cada lado
e permitem novas linhas. Escape de caracteres também não é permitido.
A primeira quebra de linha é removida em strings brutas
   Todo outro espaço em branco
   é preservado. #! foi preservado?
'''

# Para dados binários é recomendado que você use Base64, outra codificação ASCII ou UTF8.
# A manipulação dessa codificação será específico da aplicação.

############
# Inteiros #
############

## Inteiros podem começar com um +, um -, ou nada.
## Zeros à frente não são permitidos.
## Formatos em hexadecimal, octal e binário são permitidos.
## Não são permitidos valores que não podem ser expressados como uma série de dígitos.
int1 = +42
int2 = 0
int3 = -21
int4 = 0xcafebabe
int5 = 0o755
int6 = 0b11011100
integerRange = 64

## Você pode usar underscores para melhorar a legibilidade.
## Cada underscore precisa estar entre, pelo menos, um dígito.
int7 = 5_349_221
int8 = 1_2_3_4_5     # VÁLIDO, mas não é recomendado

#########
# Float #
#########

# Floats são inteiros seguidos por uma fração e/ou de um expoente.
flt1 = 3.1415
flt2 = -5e6
flt3 = 6.626E-34

#############
# Booleanos #
#############

bool1 = true
bool2 = false
booleanosPrecisamEstarEmMinusculo = true

############
# Datetime #
############

date1 = 1979-05-27T07:32:00Z # Tempo UTC, seguindo especificação RFC 3339/ISO 8601
date2 = 1979-05-26T15:32:00+08:00 # com um deslocamento segundo a RFC 3339/ISO 8601
date3 = 1979-05-27T07:32:00 # sem deslocamento
date4 = 1979-05-27 # sem as horas e sem deslocamento

####################
# TIPOS DE COLEÇÃO #
####################

#########
# Array #
#########

array1 = [ 1, 2, 3 ]
array2 = [ "Vírgulas", "são", "delimitadores" ]
array3 = [ "Não misture", "tipos", "diferentes" ]
array4 = [ [ 1.2, 2.4 ], ["todas as", 'strings', """são do mesmo""", '''tipo'''] ]
array5 = [
  "Espaços em branco", "são", "ignorados"
]

##########
# Tabela #
##########

# Tabelas (ou tabelas de hash, ou dicionários) é uma coleção de pares chave/valor.
# Eles aparecem entre colchetes em uma linha separada.
# Tabelas vazias são permitidas e simplesmente não possuem chave/valor associado.
[tabela]

# Abaixo disso, e até a próxima tabela ou final do arquivo, estão as chaves/valores dessa tabela.
# Os pares de chave/valor dentro das tabelas não têm garantia de estar em nenhuma ordem específica.
[table-1]
chave1 = "algum texto"
chave2 = 123

[table-2]
chave1 = "outro texto"
chave2 = 456

# Pontos são proibidos em chaves simples porque são usados para indicar tabelas aninhadas.
# As regras de nomenclatura para cada parte separada por ponto são as mesmas que para chaves.
[dog."tater.man"]
type = "pug"

# Na terra do JSON, você teria a seguinte estrutura:
# { "dog": { "tater.man": { "type": "pug" } } }

# Espaços em branco em torno das partes separadas por pontos são ignorados, de qualquer forma,
# é sempre recomendado não utilizar espaços em branco desnecessários.
[a.b.c]            # isso é o recomendado
[ d.e.f ]          # mesmo que [d.e.f]
[ j . "ʞ" . 'l' ]  # mesmo que [j."ʞ".'l']

# Você não precisa especificar todas as super-tabelas se não quiser. TOML sabe
# como lidar com isso para você.
# [x] você
# [x.y] não precisa
# [x.y.z] disso
[x.y.z.w] # para isso funcionar

# Mesmo que uma super-tabela não tenha sido definida diretamente e não tenha definido uma
# chave específica, ainda é possível escrever nela.
[a.b]
c = 1

[a]
d = 2

# Irá gerar o seguinte JSON:
# { "a": {"b": {"c": 1}, "d": 2 } }

# Você não pode definir uma chave ou tabela mais de uma vez. É inválido fazer isso.

# NÃO FAÇA ISSO
[a]
b = 1

[a]
c = 2

# NEM MESMO ISSO
[a]
b = 1

[a.b]
c = 2

# O nome de todas as tabelas não pode ser vazio.
[]     # INVÁLIDO
[a.]   # INVÁLIDO
[a..b] # INVÁLIDO
[.b]   # INVÁLIDO
[.]    # INVÁLIDO

####################
# Tabelas em linha #
####################

tabelasEmLinha = { sãoFechadasCom = "{ e }", precisamEstarEmUmaLinha = true }
ponto = { x = 1, y = 2 }

####################
# Array de Tabelas #
####################

# Um array de tabelas pode ser expresso usando um nome de tabela entre colchetes duplos.
# Cada tabela com o mesmo nome entre colchetes duplos será um item no array.
# As tabelas são inseridas na ordem em que são encontradas.

[[produtos]]
nome = "array de tabelas"
sku = 738594937
tabelasVaziasSaoPermitidas = true

[[produtos]]

[[produtos]]
nome = "Unhas"
sku = 284758393
color = "cinza"
```
