---
language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Joao Marques", "http://github.com/mrshankly"]
lang: pt-pt
---

Brainfuck (não capitalizado excepto no início de uma frase) é uma linguagem de
programação Turing-completa extremamente simples com apenas 8 comandos.

```
Qualquer caractere excepto "><+-.,[]" (não contar com as aspas) é ignorado.

Brainfuck é representado por um vector com 30 000 células inicializadas a zero
e um ponteiro de dados que aponta para a célula actual.

Existem 8 comandos:
+ : Incrementa o valor da célula actual em 1.
- : Decrementa o valor da célula actual em 1.
> : Move o ponteiro de dados para a célula seguinte (célula à direita).
< : Move o ponteiro de dados para a célula anterior (célula à esquerda).
. : Imprime o valor ASCII da célula actual. (ex. 65 = 'A').
, : Lê um único caractere para a célula actual.
[ : Se o valor da célula actual for zero, salta para o ] correspondente.
    Caso contrário, passa para a instrução seguinte.
] : Se o valor da célula actual for zero, passa para a instrução seguinte.
    Caso contrário, volta para a instrução relativa ao [ correspondente.

[ e ] formam um ciclo while. Obviamente, devem ser equilibrados.

Vejamos alguns programas básicos de brainfuck.

++++++ [ > ++++++++++ < - ] > +++++ .

Este programa imprime a letra 'A'. Primeiro incrementa a célula #1 para 6.
A célula #1 será usada num ciclo. Depois é iniciado o ciclo ([) e move-se
o ponteiro de dados para a célula #2. Incrementa-se o valor da célula #2 10
vezes, move-se o ponteiro de dados de volta para a célula #1, e decrementa-se
a célula #1. Este ciclo acontece 6 vezes (são necessários 6 decrementos para
a célula #1 chegar a 0, momento em que se salta para o ] correspondente,
continuando com a instrução seguinte).

Nesta altura encontramo-nos na célula #1, cujo valor é 0, enquanto a célula #2
tem o valor 60. Movemos o ponteiro de dados para a célula #2, incrementa-se 5
vezes para um valor final de 65, é então impresso o valor da célula #2. Ao valor
65 corresponde o caractere 'A' em ASCII, 'A' é então impresso para o terminal.

, [ > + < - ] > .

Este programa lê um caractere e copia o seu valor para a célula #1. Um ciclo é
iniciado. Movemos o ponteiro de dados para a célula #2, incrementamos o valor na
célula #2, movemos o ponteiro de dados de volta para a célula #1, finalmente
decrementamos o valor na célula #1. Isto continua até o valor na célula #1 ser
igual a 0 e a célula #2 ter o antigo valor da célula #1. Como o ponteiro de
dados está a apontar para a célula #1 no fim do ciclo, movemos o ponteiro para a
célula #2 e imprimimos o valor em ASCII.

Os espaços servem apenas para tornar o programa mais legível. Podemos escrever
o mesmo programa da seguinte maneira:

,[>+<-]>.

Tenta descobrir o que este programa faz:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Este programa lê dois números e multiplica-os.

Basicamente o programa pede dois caracteres ao utilizador. Depois é iniciado um
ciclo exterior controlado pelo valor da célula #1. Movemos o ponteiro de dados
para a célula #2 e inicia-se o ciclo interior controlado pelo valor da célula
#2, incrementando o valor da célula #3. Contudo, existe um problema, no final do
ciclo interior a célula #2 tem o valor 0. Para resolver este problema o valor da
célula #4 é também incrementado e copiado para a célula #2.
```

Fica então explicado brainfuck. Simples, não? Por divertimento podes escrever os
teus próprios programas em brainfuck, ou então escrever um interpretador de
brainfuck noutra linguagem. O interpretador é relativamente fácil de se
implementar, mas se fores masoquista, tenta escrever um interpretador de
brainfuck… em brainfuck.
