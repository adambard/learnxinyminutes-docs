---
language: bf
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Suzane Sant Ana", "http://github.com/suuuzi"]
    - ["Rodrigo Muniz", "http://github.com/muniz95"]
lang: pt-br
---

Brainfuck (em letras minúsculas, exceto no início de frases) é uma linguagem de
programação Turing-completa extremamente simples com apenas 8 comandos.

```
Qualquer caractere exceto "><+-.,[]" (sem contar as aspas) é ignorado.

Brainfuck é representado por um vetor com 30 000 células inicializadas em zero
e um ponteiro de dados que aponta para a célula atual.

Existem 8 comandos:
+ : Incrementa o valor da célula atual em 1.
- : Decrementa o valor da célula atual em 1.
> : Move o ponteiro de dados para a célula seguinte (célula à direita).
< : Move o ponteiro de dados para a célula anterior (célula à esquerda).
. : Imprime o valor ASCII da célula atual. (ex. 65 = 'A').
, : Lê um único caractere para a célula atual.
[ : Se o valor da célula atual for zero, salta para o ] correspondente.
    Caso contrário, passa para a instrução seguinte.
] : Se o valor da célula atual for zero, passa para a instrução seguinte.
    Caso contrário, volta para a instrução relativa ao [ correspondente.

[ e ] formam um ciclo while. Obviamente, devem ser equilibrados.

Vamos ver alguns exemplos básicos em brainfuck:

++++++ [ > ++++++++++ < - ] > +++++ .

Este programa imprime a letra 'A'. Primeiro incrementa a célula #1 para 6.
A célula #1 será usada num ciclo. Depois é iniciado o ciclo ([) e move-se
o ponteiro de dados para a célula #2. O valor da célula #2 é incrementado 10
vezes, move-se o ponteiro de dados de volta para a célula #1, e decrementa-se
a célula #1. Este ciclo acontece 6 vezes (são necessários 6 decrementos para
a célula #1 chegar a 0, momento em que se salta para o ] correspondente,
continuando com a instrução seguinte).

Nesta altura estamos na célula #1, cujo valor é 0, enquanto a célula #2
tem o valor 60. Movemos o ponteiro de dados para a célula #2, incrementa-se 5
vezes para um valor final de 65, e então é impresso o valor da célula #2. O valor
65 corresponde ao caractere 'A' em ASCII, então 'A' é impresso no terminal.

, [ > + < - ] > .

Este programa lê um caractere e copia o seu valor para a célula #1. Um ciclo é
iniciado. Movemos o ponteiro de dados para a célula #2, incrementamos o valor na
célula #2, movemos o ponteiro de dados de volta para a célula #1 e finalmente
decrementamos o valor na célula #1. Isto continua até o valor na célula #1 ser
igual a 0 e a célula #2 ter o antigo valor da célula #1. Como o ponteiro de
dados está apontando para a célula #1 no fim do ciclo, movemos o ponteiro para a
célula #2 e imprimimos o valor em ASCII.

Os espaços servem apenas para tornar o programa mais legível. Podemos escrever
o mesmo programa da seguinte maneira:

,[>+<-]>.

Tente descobrir o que este programa faz:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Este programa lê dois números e os multiplica.

Basicamente o programa pede dois caracteres ao usuário. Depois é iniciado um
ciclo exterior controlado pelo valor da célula #1. Movemos o ponteiro de dados
para a célula #2 e inicia-se o ciclo interior controlado pelo valor da célula
#2, incrementando o valor da célula #3. Porém existe um problema, no final do
ciclo interior: a célula #2 tem o valor 0. Para resolver este problema o valor da
célula #4 é também incrementado e copiado para a célula #2.
```

E isto é brainfuck. Simples, não? Por divertimento você pode escrever os
seus próprios programas em brainfuck, ou então escrever um interpretador de
brainfuck em outra linguagem. O interpretador é relativamente fácil de se
implementar, mas caso você seja masoquista, tente escrever um interpretador de
brainfuck… em brainfuck.
