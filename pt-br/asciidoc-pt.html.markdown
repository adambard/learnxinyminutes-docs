---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
translators:
    - ["David Lima", "https://github.com/davelima"]
lang: pt-br
filename: asciidoc-pt.md
---

AsciiDoc é uma linguagem de marcação similar ao Markdown e pode ser
usada para qualquer coisa, de livros até blogs. Criada em 2002 por
Stuart Rackham, a linguagem é simples mas facilita muito a customização.

Cabeçalho do documento

Cabeçalhos são opcionais e não podem conter linhas em branco.
Devem estar separados do conteúdo com pelo menos uma linha em branco.

Apenas Título

```
= Título do documento

Primeira sentência do documento.
```

Título e Autor

```
= Título do Documento
Nome Sobrenome <nome.sobrenome@learnxinyminutes.com>

Início do documento.
```

Múltiplos Autores

```
= Título do Documento
John Doe <john@go.com>; Jane Doe<jane@yo.com>; Black Beard <beardy@pirate.com>

Início do documento com múltiplos autores.
```

Linhas de revisão (requer uma linha de autor)

```
= Documento V1
Potato Man <chip@crunchy.com>
v1.0, 2016-01-13

Este artigo sobre batatas será divertido.
```

Parágrafos

```
Você não precisa fazer nada especial para criar um parágrafo.

Adicione uma linha em branco entre os parágrafos para separá-los.

Para criar uma linha em branco adicione um +
e você terá uma quebra de linha!
```

Formatando texto

```
_underscore é itálico_
*asterisco é negrito*
*_você pode combinar efeitos_*
`use crase para fonte monoespaçada`
`*fonte monoespaçada em negrito*`
```

Título de seções

```
= Nível 0 (Use apenas no cabeçalho do documento)

== Nível 1 <h2>

=== Nível 2 <h3>

==== Nível 3 <h4>

===== Nível 4 <h5>

====== Nível 5 <h6>

======= Nível 6  <h7>

```

Listas

Para criar uma lista com marcadores use asteriscos.

```
* foo
* bar
* baz
```

Para criar uma lista númerada use pontos.

```
. item 1
. item 2
. item 3
```

Você pode criar listas dentro de listas adicionando
asteriscos ou pontos extras em até 5 níveis.

```
* foo 1
** foo 2
*** foo 3
**** foo 4
***** foo 5

. foo 1
.. foo 2
... foo 3
.... foo 4
..... foo 5
```
