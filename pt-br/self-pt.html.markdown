---
language: self
contributors:
    - ["Russell Allen", "http://github.com/russellallen"]
translators:
    - ["Ricardo de Almeida Gonzaga", "http://github.com/ricardotk"]
lang: pt-br
filename: learnself.self
---

Self é um protótipo rápido baseado em linguagem orientada a objeto a qual roda em sua própria JIT vm. A maior parte do desenvolvimento é feito através de interações com objetos ativos, através de um ambiente visual de desenvolvimento chamado *morphic* com navegador e depurador integrados.

Tudo em Self é um objeto. Toda computação é feita através do envio de mensagens para objetos. Objetos em Self podem ser compreendidos como conjuntos de slots de chave-valor.

# Construindo objetos

O analisador interno de Self pode construir objetos, incluindo objetos método.

```
"Isto é um comentario"

"A string:"
'Isto é uma string com caracteres \'escapados\'.\n'

"Um inteiro de 30 bits"
23

"Um float de 30 bits"
3.2

"-20"
-14r16

"Um objeto o qual entende apenas uma menssagem, 'x' que retorna 20"
(|
  x = 20.
|)

"Um objeto o qual tambem entende 'x', que atribui o slot x"
(|
  x <- 20.
|)

"Um objeto o qual entende o método 'doubleX' que 
dobra o valor de x e então retorna o objeto"
(|
  x <- 20.
  doubleX = (x: x * 2. self)
|)

"Um objeto o qual entende todas as mensagens 
que 'traits point' entende". O analisador  
procura 'traits point' enviando as mensagens 
'traits' e 'point' para um objeto conhecido
chamado de 'lobby'. Ele procura pelo objeto
'true' também enviando a mensagem 'true'
para o 'lobby'."" 
(|     parent* = traits point.
       x = 7.
       y <- 5.
       isNice = true.
|)
```

# Enviando mensagens para objetos

Mensagens podem ser unárias, binárias ou palavras-chave. Precedência é nesta ordem. Diferentemente de Smalltalk, a precedência de mensagens binárias precisam ser especificadas, e todas as palavras-chave após a primeira devem começar com letra maiúscula. Mensagens são separadas de seu destinatário através de espaço em branco.

```
"mensagem unária, envia 'printLine' para o objeto '23' 
o qual escreve a string '23' para stdout e retorna o objeto recebido (ie 23)"
23 printLine

"envia a mensagem '+' com '7' para '23', em seguida a mensagem '*' com '8' para o resultado"
(23 + 7) * 8 

"envia 'power:' para '2' com '8' retorna 256"
2 power: 8 

"envia 'keyOf:IfAbsent:' para 'hello' com argumentos 'e' e '-1'. 
Retorna 1, o índice de 'e' em 'hello'."
'hello' keyOf: 'e' IfAbsent: -1 
```

# Blocos

Self define controle de fluxo assim como Smalltalk e Ruby por meio de blocos. Blocos são computações atrasadas da forma:

```
[|:x. localVar| x doSomething with: localVar]
```

Exemplos do uso de um bloco:

```
"retorna 'HELLO'"
'hello' copyMutable mapBy: [|:c| c capitalize] 

"retorna 'Nah'"
'hello' size > 5 ifTrue: ['Yay'] False: ['Nah'] 

"retorna 'HaLLO'"
'hello' copyMutable mapBy: [|:c| 
   c = 'e' ifTrue: [c capitalize]
            False: ['a']]
```

Múltiplas expressões são separadas por ponto final. ^ retorna imediatamente.

```
"retorna An 'E'! How icky!"
'hello' copyMutable mapBy: [|:c. tmp <- ''| 
   tmp: c capitalize.
   tmp = 'E' ifTrue: [^ 'An \'E\'! How icky!'].
   c capitalize
   ]
```

Blocos são realizados (completados) ao enviá-los a mensagem 'value' e herdando (imcumbir a) seus contextos:
```
"retorna 0"
[|x|
    x: 15.
    "Repetidamente envia 'value' para o primeiro bloco enquanto o resultado do
     envio de 'value' para o segundo bloco é o objeto 'true'"
    [x > 0] whileTrue: [x: x - 1]. 
    x
] value
```

# Métodos

Métodos são como blocos, porém eles não estão dentro de um contexto, ao invés disto são armazenados como valores de slots. Diferentemente de Smalltalk, métodos por padrão retornam o seu valor final e não 'self'.

```
"Aqui está um objeto com um slot atribuível 'x' e um método 'reduceXTo: y'.
Enviar a mensagem 'reduceXTo: 10' para este objeto colocará 
o objeto '10' no slot 'x' e retornará o objeto original"
(| 
    x <- 50.
    reduceXTo: y = (
        [x > y] whileTrue: [x: x - 1]. 
        self)
|)
.
```

# Protótipos

Não existem classes em Self. A maneira de obter um objeto é encontrar um protótipo e copia-lo.

```
| d |
d: dictionary copy.
d at: 'hello' Put: 23 + 8.
d at: 'goodbye' Put: 'No!.
"Imprime No!"
( d at: 'goodbye' IfAbsent: 'Yes! ) printLine.
"Imprime 31"
( d at: 'hello' IfAbsent: -1 ) printLine.
```

# Informações adicionais

O [Manual do Self](http://handbook.selflanguage.org) tem muito mais informações, e nada melhor do que por a mão na massa com Self através de sua [homepage](http://www.selflanguage.org).
