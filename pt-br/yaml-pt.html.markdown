---
language: yaml
contributors:
  - ["Leigh Brenecki", "https://github.com/adambrenecki"]
  - [Suhas SG, 'https://github.com/jargnar']
translators:
  - ["Rodrigo Russo", "https://github.com/rodrigozrusso"]
filename: learnyaml-pt.yaml
lang: pt-br
---

YAML é uma linguagem de serialização de dados projetado para ser diretamente gravável e
legível por seres humanos.

É um superconjunto de JSON, com a adição de identação e quebras de linhas sintaticamente significativas, como Python. Ao contrário de Python, entretanto, YAML não permite o caracter literal tab para identação.

```yaml
---  # início do documento

# Comentários em YAML são como este.

###################
# TIPOS ESCALARES #
###################

# Nosso objeto raiz (que continua por todo o documento) será um mapa,
# o que equivale a um dicionário, hash ou objeto em outras linguagens.
chave: valor
outra_chave: Outro valor vai aqui.
u_valor_numerico: 100
notacao_cientifica: 1e+12
boleano: true
valor_nulo: null
chave com espaco: valor
# Observe que strings não precisam de aspas. Porém, elas podem ter.
porem: 'Uma string, entre aspas.'
'Chaves podem estar entre aspas tambem.': "É útil se você quiser colocar um ':' na sua chave."
aspas simples: 'possuem ''um'' padrão de escape'
aspas duplas: "possuem vários: \", \0, \t, \u263A, \x0d\x0a == \r\n, e mais."
# Caracteres UTF-8/16/32 precisam ser codificados
Superscript dois: \u00B2

# Seqüências de várias linhas podem ser escritas como um 'bloco literal' (utilizando |),
# ou em um 'bloco compacto' (utilizando '>').
bloco_literal: |
  Todo esse bloco de texto será o valor da chave 'bloco_literal',
  preservando a quebra de com linhas.

  O literal continua até 'des-indentar', e a primeira identação é 
  removida.

    Quaisquer linhas que são 'mais identadas' mantém o resto de suas identações - 
    estas linhas serão identadas com 4 espaços.
estilo_compacto: >
  Todo esse bloco de texto será o valor de 'estilo_compacto', mas esta
  vez, todas as novas linhas serão substituídas com espaço simples.

  Linhas em branco, como acima, são convertidas em um carater de nova linha.

    Linhas 'mais-indentadas' mantém suas novas linhas também -
    este texto irá aparecer em duas linhas.

####################
# TIPOS DE COLEÇÃO #
####################

# Texto aninhado é conseguido através de identação.
um_mapa_aninhado:
  chave: valor
  outra_chave: Outro valor
  outro_mapa_aninhado:
    ola: ola

# Mapas não tem que ter chaves com string.
0.25: uma chave com valor flutuante

# As chaves podem ser complexas, como sequência de várias linhas
# Utilizando ? seguido por espaço para indicar o começo de uma chave complexa.
? |
  Esta é uma chave
  que tem várias linhas
: e este é o seu valor

# YAML também permite o mapeamento entre sequências com a sintaxe chave complexa
# Alguns analisadores de linguagem de programação podem reclamar
# Um exemplo
? - Manchester United
  - Real Madrid
: [2001-01-01, 2002-02-02]

# Sequências (equivalente a listas ou arrays) semelhante a isso:
uma_sequencia:
  - Item 1
  - Item 2
  - 0.5 # sequências podem conter tipos diferentes.
  - Item 4
  - chave: valor
    outra_chave: outro_valor
  -
    - Esta é uma sequência
    - dentro de outra sequência
  - - - Indicadores de sequência aninhadas
      - podem ser recolhidas

# Como YAML é um super conjunto de JSON, você também pode escrever mapas JSON de estilo e
# sequências:
mapa_json: {"chave": "valor"}
json_seq: [3, 2, 1, "decolar"]
e aspas são opcionais: {chave: [3, 2, 1, decolar]}

###########################
# RECURSOS EXTRAS DO YAML #
###########################

# YAML também tem um recurso útil chamado "âncoras", que permitem que você facilmente duplique
# conteúdo em seu documento. Ambas estas chaves terão o mesmo valor:
conteudo_ancora: &nome_ancora Essa string irá aparecer como o valor de duas chaves.
outra_ancora: *nome_ancora

# Âncoras podem ser usadas para dubplicar/herdar propriedades
base: &base
  name: Todos possuem o mesmo nome

# O regexp << é chamado Mesclar o Tipo Chave Independente-de-Idioma. É usado para
# indicar que todas as chaves de um ou mais mapas específicos devam ser inseridos
# no mapa atual.

foo:
  <<: *base
  idade: 10

bar:
  <<: *base
  idade: 20

# foo e bar terão o mesmo nome: Todos possuem o mesmo nome

# YAML também tem tags, que você pode usar para declarar explicitamente os tipos.
string_explicita: !!str 0.5
# Alguns analisadores implementam tags específicas de linguagem, como este para Python de
# Tipo de número complexo.
numero_complexo_em_python: !!python/complex 1+2j

# Podemos utilizar chaves YAML complexas com tags específicas de linguagem
? !!python/tuple [5, 7]
: Fifty Seven
# Seria {(5, 7): 'Fifty Seven'} em Python

####################
# YAML TIPOS EXTRA #
####################

# Strings e números não são os únicos que escalares YAML pode entender.
# Data e 'data e hora' literais no formato ISO também são analisados.
datetime: 2001-12-15T02:59:43.1Z
datetime_com_espaços: 2001-12-14 21:59:43.10 -5
date: 2002-12-14

# A tag !!binary indica que a string é na verdade um base64-encoded (codificado)
# representação de um blob binário.
gif_file: !!binary |
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML também tem um tipo de conjunto, o que se parece com isso:
conjunto:
  ? item1
  ? item2
  ? item3
ou: {item1, item2, item3}

# Como Python, são apenas conjuntos de mapas com valores nulos; o acima é equivalente a:
conjunto2:
  item1: null
  item2: null
  item3: null

...  # fim do documento
```

### Mais Recursos

+ [Site Oficial do YAML](https://yaml.org/)
+ [Validador YAML Online](http://www.yamllint.com/)
