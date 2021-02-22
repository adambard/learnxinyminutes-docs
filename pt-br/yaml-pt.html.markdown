---
language: yaml
contributors:
  - ["Leigh Brenecki", "https://github.com/adambrenecki"]
translators:
  - ["Rodrigo Russo", "https://github.com/rodrigozrusso"]
filename: learnyaml-pt.yaml
lang: pt-br
---

YAML é uma linguagem de serialização de dados projetado para ser diretamente gravável e
legível por seres humanos.

É um superconjunto de JSON, com a adição de identação e quebras de linhas sintaticamente significativas, como Python. Ao contrário de Python, entretanto, YAML não permite o caracter literal tab para identação.

```yaml
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
porem: "Uma string, entre aspas."
"Chaves podem estar entre aspas tambem.": "É útil se você quiser colocar um ':' na sua chave."

# Seqüências de várias linhas podem ser escritas como um 'bloco literal' (utilizando |),
# ou em um 'bloco compacto' (utilizando '>').
bloco_literal: |
    Todo esse bloco de texto será o valor da chave 'bloco_literal',
    preservando a quebra de com linhas.

    O literal continua até de-dented, e a primeira identação é 
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

# As chaves podem ser também objetos multi linhas, utilizando ? para indicar o começo de uma chave.
? |
    Esta é uma chave
    que tem várias linhas
: e este é o seu valor

# também permite tipos de coleção de chaves, mas muitas linguagens de programação
# vão reclamar.

# Sequências (equivalente a listas ou arrays) semelhante a isso:
uma_sequencia:
    - Item 1
    - Item 2
    - 0.5 # sequencias podem conter tipos diferentes.
    - Item 4
    - chave: valor
      outra_chave: outro_valor
    -
        - Esta é uma sequencia
        - dentro de outra sequencia

# Como YAML é um super conjunto de JSON, você também pode escrever mapas JSON de estilo e
# sequencias:
mapa_json: {"chave": "valor"}
json_seq: [3, 2, 1, "decolar"]

##########################
# RECURSOS EXTRA DO YAML #
##########################

# YAML também tem um recurso útil chamado "âncoras", que permitem que você facilmente duplique
# conteúdo em seu documento. Ambas estas chaves terão o mesmo valor:
conteudo_ancora: & nome_ancora Essa string irá aparecer como o valor de duas chaves.
outra_ancora: * nome_ancora

# YAML também tem tags, que você pode usar para declarar explicitamente os tipos.
string_explicita: !! str 0,5
# Alguns analisadores implementam tags específicas de linguagem, como este para Python de
# Tipo de número complexo.
numero_complexo_em_python: !! python / complex 1 + 2j

####################
# YAML TIPOS EXTRA #
####################

# Strings e números não são os únicos que escalares YAML pode entender.
# Data e 'data e hora' literais no formato ISO também são analisados.
datetime: 2001-12-15T02: 59: 43.1Z
datetime_com_espacos 2001/12/14: 21: 59: 43.10 -5
Data: 2002/12/14

# A tag !!binary indica que a string é na verdade um base64-encoded (codificado)
# representação de um blob binário.
gif_file: !!binary |
    R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
    OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
    +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
    AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=

# YAML também tem um tipo de conjunto, o que se parece com isso:
set:
    ? item1
    ? item2
    ? item3

# Como Python, são apenas conjuntos de mapas com valors nulos; o acima é equivalente a:
set2:
    item1: nulo
    item2: nulo
    item3: nulo
```
