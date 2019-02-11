---
language: Pascal
filename: learnpascal.pas
contributors:
    - ["Raul Almeida", "http://github.com/almeidaraul"]
---


>Pascal is an imperative and procedural programming language, which Niklaus Wirth designed in 1968–69 and published in 1970, as a small, efficient language intended to encourage good programming practices using structured programming and data structuring. It is named in honor of the French mathematician, philosopher and physicist Blaise Pascal. 
source : [wikipedia](https://en.wikipedia.org/wiki/Pascal_(programming_language))



Para compilar e executar um programa em Pascal, você pode usar o freepascal, um compilador gratuito. [Faça o download aqui](https://www.freepascal.org/)

```pascal
//Corpo de um programa em Pascal
//isso é um comentário
{
    isso também é um comentário,
    mas com várias linhas
}

//cabeçalho do programa
program learn_pascal; //<-- não esqueça o ponto e vírgula. O nome do programa deve começar com uma letra.

const
    {
        aqui você declara valores constantes
    }
type
    {
        aqui você declara tipos não nativos
    }
var
    {
        aqui você declara variáveis
    }

//programa de fato
begin
    {
        aqui ficam todas as instruções que não pertencem nem a funções nem a procedimentos
    }
end. //o "end" no final do programa requer um ponto final.
```

```pascal
//formas de declarar uma variável
var a:integer;
var b:integer;

var 
    a : integer;
    b : integer;

var a,b : integer;
```

```pascal
program Learn_More;
//agora, mais sobre tipos de dados e operações

const
    PI = 3.141592654;
    GNU = 'GNU''s Not Unix';
        // a convenção é usar caixa alta para constantes
        // constantes têm valores fixos que só podem ser alterados antes da compilação
        // constantes podem ser de qualquer tipo nativo (integer, real, boolean, char, string)

type
    ch_array : array [0..255] of char;
        // isso declara um novo tipo de variável, ch_array, que tem 256 caracteres
    md_array : array of array of integer;
        // vetores de vetores são vetores multidimensionais
        // pode-se definir vetores de tamanho 0 (daí, usa-se alocação dinâmica)
        // vetores do tipo md_array são de duas dimensões

//declarando variáveis
var
    int, c, d  : integer;
           // três variáveis que guardam valores inteiros
           // inteiros têm 16 bits (vão de -32768 a 32767)
    r    : real;
           // uma variável que guarda um valor real
           // reais vão de 3.4E-38 a 3.4E38
    bool : boolean;
           // uma variável que guarda um valor booleano (verdadeiro ou falso)
    ch   : char;
           // uma variável que guarda um caractere
           // caracteres têm 8 bits 
    str  : string;
           // esse tipo de variável não é padrão, mas é incluído na maioria dos compiladores
           // string é um vetor de caracteres com tamanho padrão de 255 elementos
    s    : string[50];
           // você pode especificar o tamanho de uma string desta maneira para otimizar o uso de memória
    my_str: ch_array;
           // aqui, uma variável de um tipo personalizado declarado acima
    my_2d : md_array;
           // vetores de alocação dinâmica de memória precisam receber um tamanho antes de serem usados

    // tipos adicionais de inteiros
    b    : byte;     // faixa [0..255]
    shi  : shortint; // faixa [-128..127]
    smi  : smallint; // faixa [-32,768..32,767] (inteiro padrão)
    w    : word;     // faixa [0..65,535]
    li   : longint;  // faixa [-2,147,483,648..2,147,483,647]
    lw   : longword; // faixa [0..4,294,967,295]
    c    : cardinal; // longword
    i64  : int64;    // faixa [-9223372036854775808..9223372036854775807]
    qw   : qword;    // faixa [0..18,446,744,073,709,551,615]

    // tipos adicionais de reais
    rr   : real;     // faixa depende da plataforma (8 bits, 16 bits, etc)
    rs   : single;   // faixa [1.5E-45..3.4E38]
    rd   : double;   // faixa [5.0E-324 .. 1.7E308]
    re   : extended; // faixa [1.9E-4932..1.1E4932]
    rc   : comp;     // faixa [-2E64+1 .. 2E63-1]

Begin
    int := 1;// usa-se := para atribuir valores a variáveis
    r   := 3.14; 
    ch  := 'a';
    str := 'apple';
    bool := true;
    //pascal não é case-sensitive (não diferencia maiúsculas de minúsculas)
	//uma opção de organização é usar maiúsculas para termos da linguagem (BEGIN, END, INTEGER, etc) e constantes
    //aritmética
    int := 1 + 1; // int deixa de ser 1 e passa a ser 2
    int := int + 1; // int = 2 + 1 = 3;
    int := 4 div 2; //int = 2 (DIV é uma divisão inteira, ou seja, o resto é "jogado fora")
    int := 3 div 2; //int = 1
    int := 1 div 2; //int = 0

    bool := true or false; // bool = true
    bool := false and true; // bool = false
    bool := true xor true; // bool = false

    r := 3 / 2; // usa-se a "/" para divisões entre inteiros
    r := int; // um real pode receber um valor inteiro (mas não o contrário)

    c := str[1]; //acessando elementos de um vetor: vetor[índice do elemento]
    str := 'hello' + 'world'; //concatenção de strings

    my_str[0] := 'a'; // só se pode atribuir valores a vetores elemento por elemento (não o vetor inteiro de uma vez)

    setlength(my_2d,10,10); // inicialização de um vetor com alocação dinâmica de memória; my_2d vira um 10x10 
    for c := 0 to 9 do // vetores começam em 0 e terminam em tamanho-1 (exceto se, na declaração do tipo, forem especificados valores diferentes)
        for d := 0 to 9 do // a variável usada em um laço FOR deve ter sido declarada no cabeçalho do programa
        my_2d[c,d] := c * d; // acessam-se elementos de vetores multidimensionais com [dimensão1, dimensão2, dimensão3...]

End.
```

```pascal
program Functional_Programming;

Var
    i, dummy : integer;

{ OBS: diferença entre procedimento e função
	função: realiza operações e retorna valor
	procedimento: só realiza operações
} 
//declarando e descrevendo uma função
function fatorial_recursiva(const a: integer) : integer;
{ calcula a! recursivamente }

// pode-se declarar variáveis locais para funções e procedimentos
// exemplo:
// Var
//    local_a : integer;

Begin
    If a >= 1 Then
    // o valor de retorno é atribuído como se a função fosse uma variável
        fatorial_recursiva := a * fatorial_recursiva(a-1)
    Else
        fatorial_recursiva := 1;
End; // o END de funções e procedimentos recebe ponto e vírgula

//declarando e descrevendo um procedimento 
procedure salvainteiro(var i : integer; dummy : integer);
{ recebe entrada do usuário e salva na variável i
  passagem de valor:
	por referência - "VAR i: integer"; implica que alterações na variável i dentro da função são guardadas para o escopo de todo o programa
	por valor - "dummy: integer"; implica que o valor do argumento é copiado e alterações na variável dummy não são guardadas
}

Begin
    write('Insira um inteiro: '); //escreve sem quebrar linha
    readln(i); //lê i com quebra de linha
    dummy := 4; // dummy não terá seu valor alterado fora do procedimento. 
End;

Begin // programa principal
    dummy := 3;
    salvainteiro(i, dummy);

	// escrevendo i! 
    writeln(i, '! = ', factorial_recursion(i)); // escreve e quebra linha; valores numéricos são automaticamente convertidos para texto na escrita

    // escrevendo o valor de dummy
    writeln('dummy = ', dummy); // sempre vai escrever 3, já que o valor de dummy não é alterado por salvainteiro
End.

```

