---
language: Pascal
filename: learnpascal-pt.pas
contributors:
    - ["Ganesha Danu", "https://github.com/blinfoldking"]
    - ["Keith Miyake", "https//github.com/kaymmm"]
    - ["Raul Almeida", "https://github.com/almeidaraul"]
translators:
    - ["Raul Almeida", "https://github.com/almeidaraul"]
lang: pt-br
---


>Pascal é uma linguagem de programação estruturada, que recebeu este nome em homenagem ao matemático e físico Blaise Pascal. Foi criada em 1970 pelo suíço Niklaus Wirth,tendo em mente encorajar o uso de código estruturado. 
fonte : [wikipédia](https://pt.wikipedia.org/wiki/Pascal_(linguagem_de_programa%C3%A7%C3%A3o))



Para compilar e executar um programa em Pascal, você pode usar o freepascal, 
um compilador gratuito. [Faça o download aqui](https://www.freepascal.org/)

```pascal
//Corpo de um programa em Pascal
//isso é um comentário
{
    isso também é um comentário,
    mas com várias linhas
}

//cabeçalho do programa
PROGRAM aprendendo_pascal; //<-- não esqueça o ponto e vírgula. O nome do 
programa deve começar com uma letra.

CONST
    {
        aqui você declara valores constantes
    }
TYPE
    {
        aqui você declara tipos não nativos
    }
VAR
    {
        aqui você declara variáveis
    }

//programa de fato
BEGIN
    80
        aqui ficam todas as instruções que não pertencem nem a funções 
		nem a procedimentos 
		
		blocos de código começam com BEGIN e terminam com END 
		(como as chaves em C)
    }
END. //o "end" no final do programa requer um ponto final.
```

```pascal
//formas de declarar uma variável
VAR a:INTEGER;
VAR b:INTEGER;

VAR 
    a : INTEGER;
    b : INTEGER;

VAR a,b : INTEGER;
```

```pascal
PROGRAM Learn_More;
//agora, mais sobre tipos de dados e operações

CONST
    PI = 3.141592654;
    GNU = 'GNU''s Not Unix';
        { a convenção é usar caixa alta para constantes
          
		  constantes têm valores fixos que só podem ser alterados 
		  antes da compilação
          
		  constantes podem ser de qualquer tipo nativo (integer, real, boolean, 
		  char, string)

TYPE
    vet_char : array [0..255] of CHAR;
        // isso declara um novo tipo de variável, vet_char, com 256 caracteres
    vet_multd : array of array of INTEGER;
        // vetores de vetores são vetores multidimensionais
        // use vetores de tamanho 0 (daí, usa-se alocação dinâmica)
        // vetores do tipo vet_multd são de duas dimensões

//declarando variáveis
VAR
    int, c, d  : INTEGER;
           // três variáveis que guardam valores inteiros
           // inteiros têm 16 bits (vão de -32768 a 32767)
    r    : REAL;
           // uma variável que guarda um valor real
           // reais vão de 3.4E-38 a 3.4E38
    bool : BOOLEAN;
           // uma variável que guarda um valor booleano (verdadeiro ou falso)
    ch   : CHAR;
           // uma variável que guarda um caractere
           // caracteres têm 8 bits 
    str  : STRING;
           // STRING não é um tipo padrão, mas vem na maioria dos compiladores
           // string é um vetor de caracteres com tamanho padrão de 255 elementos
    s    : STRING[50];
           // especificar o tamanho de uma string assim otimiza o uso de memória
    my_str: vet_char;
           // aqui, uma variável de um tipo personalizado declarado acima
    my_2d : vet_multd;
           // defina o tamanho de vetores dinâmicos antes de usá-los

// outros tipos de dados (pascal é uma linguagem fortemente tipada)

    // tipos adicionais de inteiros
    b    : BYTE;     // faixa [0..255]
    shi  : SHORTINT; // faixa [-128..127]
    smi  : SMALLINT; // faixa [-32,768..32,767] (inteiro padrão)
    w    : WORD;     // faixa [0..65,535]
    li   : LONGINT;  // faixa [-2,147,483,648..2,147,483,647]
    lw   : LONGWORD; // faixa [0..4,294,967,295]
    c    : CARDINAL; // longword
    i64  : INT64;    // faixa [-9223372036854775808..9223372036854775807]
    qw   : QWORD;    // faixa [0..18,446,744,073,709,551,615]

    // tipos adicionais de reais
    rr   : REAL;     // faixa depende da plataforma (8 bits, 16 bits, etc)
    rs   : SINGLE;   // faixa [1.5E-45..3.4E38]
    rd   : DOUBLE;   // faixa [5.0E-324 .. 1.7E308]
    re   : EXTENDED; // faixa [1.9E-4932..1.1E4932]
    rc   : COMP;     // faixa [-2E64+1 .. 2E63-1]

BEGIN
    int := 1;// usa-se := para atribuir valores a variáveis
    r   := 3.14; 
    ch  := 'a';
    str := 'apple';
    bool := true;
    //pascal não é case-sensitive (não diferencia maiúsculas de minúsculas)
	// uma opção de organização é usar maiúsculas para termos da linguagem 
	// (BEGIN, END, INTEGER, etc) e constantes
    // aritmética
    int := 1 + 1; // int deixa de ser 1 e passa a ser 2
    int := int + 1; // int = 2 + 1 = 3;
    int := 4 DIV 2; //int = 2 (DIV é uma divisão inteira - ignora o resto) 
    int := 3 DIV 2; //int = 1
    int := 1 DIV 2; //int = 0

    bool := true OR false; // bool = true
    bool := false AND true; // bool = false
    bool := true XOR true; // bool = false

    r := 3 / 2; // usa-se a "/" para divisões entre inteiros
    r := int; // um real pode receber um valor inteiro (mas não o contrário)

    c := str[1]; //acessando elementos de um vetor: vetor[índice do elemento]
    str := 'hello' + 'world'; //concatenação de strings

    my_str[0] := 'a'; { só se pode atribuir valores a vetores elemento 
						por elemento (não o vetor inteiro de uma vez) }

    // LAÇOS
	WHILE (ch != 'a') DO
		BEGIN
			writeln('ch é diferente de a');
			IF (ch = 'c') THEN
				writeln('ch é igual a c');
		END;
	
	SETLENGTH(my_2d,10,10); 
	// inicialização de um vetor com alocação dinâmica; my_2d vira um 10x10 
    FOR c := 0 to 9 DO 
		{ vetores começam em 0 e terminam em tamanho-1
		  (exceto se, declarando o tipo, forem especificados valores diferentes) }
        FOR d := 0 to 9 DO 
			// a variável usada em um laço FOR deve ter sido declarada no cabeçalho
        my_2d[c,d] := c * d; 
			// acessam-se elementos de vetores multidimensionais com [d1, d2, d3...]
	

END.
```

```pascal
PROGRAM programacao_com_funcoes;

VAR
    i, inutil : INTEGER;

{ OBS: diferença entre procedimento e função
	função: realiza operações e retorna valor
	procedimento: só realiza operações
} 
//declarando e descrevendo uma função
FUNCTION fatorial_recursiva(CONST a: INTEGER) : INTEGER;
{ calcula a! recursivamente }

// pode-se declarar variáveis locais para funções e procedimentos
// exemplo:
// VAR
//    local_a : INTEGER;

BEGIN
	{ O bloco ELSE só funciona se não houver ponto e vírgula no bloco do IF 
	  exemplo: 
	  	IF a THEN
			writeln('a'); 
		ELSE 
			writeln('b'); 
		Isso não permitiria que o programa compilasse }

    IF a >= 1 THEN
    // o valor de retorno é atribuído como se a função fosse uma variável
        fatorial_recursiva := a * fatorial_recursiva(a-1)
    ELSE
        fatorial_recursiva := 1;
END; // o END de funções e procedimentos recebe ponto e vírgula

//declarando e descrevendo um procedimento 
PROCEDURE salvainteiro(VAR i : INTEGER; inutil : INTEGER);
{ recebe entrada do usuário e salva na variável i
  passagem de valor:

	POR REFERÊNCIA - "VAR i: integer"; implica que alterações na variável i
	dentro da função são guardadas para o escopo de todo o programa

	POR VALOR - "inutil: integer"; implica que o valor do argumento é copiado 
	e alterações na variável inutil não são guardadas
}

BEGIN
    WRITE('Insira um inteiro: '); //escreve sem quebrar linha
    READLN(i); //lê i com quebra de linha
    inutil := 4; // inutil não terá seu valor alterado fora do procedimento. 
END;

BEGIN // programa principal
    inutil := 3;
    salvainteiro(i, inutil);

	// escrevendo i! 
    WRITELN(i, '! = ', factorial_recursion(i)); // escreve e quebra linha
	// valores numéricos são automaticamente convertidos para texto na escrita

    // escrever valor de inutil (sempre 3, já que salvainteiro não a altera)
    WRITELN('inutil = ', inutil); 
END.

```

