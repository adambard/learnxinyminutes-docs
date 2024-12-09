---
language: bc
contributors:
    - ["Btup"]
translators:
    - ["David Lima", "https://github.com/davelima"]
lang: pt-br
filename: learnbc-pt.bc
---
```bc
/*Este é um comentário
multi-linhas*/
# Este é um comentário de uma única linha! (em bc GNU).

    /*1. Variáveis e estruturas de controle*/
num = 45 /*Todas as variáveis apenas salvam dados do tipo double, e
    você não pode salvar strings diretamente em constantes.*/
num 45; /*Você pode adicionar ponto-e-vírgula após cada linha.
    Isto é opcional*/
/*Blocos são denotados usando os operadores {} (similar ao C):*/
while(num < 50) {
    num += 1 /*equivalente a num=num+1.
    a = a op b é equivalente a a op= b.*/
}
/*E existem os operadores ++ (incrementar) e -- (decrementar).*/
/* Existem 3 tipos especiais de variáveis:
scale: define a escala de números double.
ibase: define a base de da entrada.
obase: define a base da saída.
*/
/*Cláusulas If:*/
hora = read() /*Lê a entrada de um número*/

if(hora < 12) { /*Os operadores são idênticos ao C.*/
    print "Bom dia\n" /*"print" imprime strings ou variáveis
    separados por vírgula (,).*/
} else if(hora == 12) {
    print "Olá\n"
    /*Para escapar strings, inicie a string com \.
    Para deixar o escape de strings mais claros,
    aqui está uma lista simplificada das strings escapadas
    que funcionarão com bc:
    \b: backspace
    \c: carriage return (enter)
    \n: newline (nova linha)
    \t: tab
    \\: backslash (barra inertida)*/
} else {
    /*Variáveis são globais por padrão.*/
    istoEGlobal = 5
    /*Para tornar uma variável local, use a palavra-chave "auto" em uma função.*/
}

/*Todas as variáveis por padrão tem o valor 0*/
num = variavelEmBranco /*num é igual a 0.*/

/*Assim como no C, "0" é considerado "false"*/
if(!num) {print "false\n"}

/*Diferente do C, bc não tem o operador ?:. Por exemplo,
este bloco de código causaria um erro:
a = (num) ? 1 : 0
Entretanto, você pode simular da seguinte forma:
a = (num) && (1) || (0) /*&& é "E", || é "OU"*/
*/

/*Loops For*/
num = 0
for(i = 1; i <= 100; i++) {/*Similar ao loop For do C.*/
    num += i
}

    /*2.Funções e arrays*/
define fac(n) { /*para definir uma função, use "define".*/
    if(n == 1 || n == 0) {
        return 1 /*retorna um valor*/
    }
    return n * fac(n - 1) /*recursão é permitido*/
}

/*Closures e funções anônimas não são permitidas*/

num = fac(4) /*24*/

/*Exemplo de variáveis locais:*/
define x(n) {
    auto x
    x = 1
    return n + x
}
x(3) /*4*/
print x /*A variável "x" não será acessível de fora da função*/
/*Arrays são equivalentes aos arrays em C.*/
for(i = 0; i <= 3; i++) {
    a[i] = 1
}
/*Para acessar um array, faça assim:*/
print a[0], " ", a[1], " ", a[2], " ", a[3], "\n"
quit /*Adicione essa linha no final do código
para garantir que o programa encerre. Essa linha é opcional.*/
```

Aproveite bem essa simples calculadora! (Ou essa linguagem de programação, para ser exato.)

Este programa inteiro foi escrito em GNU bc. Para rodá-lo, use ```bc learnbc-pt.bc```
