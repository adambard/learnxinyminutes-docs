---
language: c
filename: learnc.c
contributors:
    - ["Adam Bard", "http://adambard.com/"]
    - ["Árpád Goretity", "http://twitter.com/H2CO3_iOS"]
translators:
    - ["João Farias", "https://github.com/JoaoGFarias"]
    - ["Elton Viana", "https://github.com/eltonvs"]
    - ["Cássio Böck", "https://github.com/cassiobsilva"]
lang: pt-br
filename: c-pt.el
---

Ah, C. Ainda é **a** linguagem de computação de alta performance.

C é a linguagem de mais baixo nível que a maioria dos programadores
utilizarão, e isso dá a ela uma grande velocidade bruta. Apenas fique
atento se este manual de gerenciamento de memória e C vai te levar
tão longe quanto precisa.

```c
// Comentários de uma linha iniciam-se com // - apenas disponível a partir do C99

/*
Comentários de múltiplas linhas se parecem com este.
Funcionam no C89 também.
*/

// Constantes: #define <palavra-chave>
#define DAY_IN_YEAR 365

//enumerações também são modos de definir constantes.
enum day {DOM = 1, SEG, TER, QUA, QUI, SEX, SAB};
// SEG recebe 2 automaticamente, TER recebe 3, etc.

// Cabeçalhos são inclusos com #include
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// (Nomes dos arquivos entre <colchetes> são cabeçalhos para bibliotecas padrão de C.)
// Para cabeçalhos próprios, use aspas ao invés de colchetes:
#include "minha_biblioteca.h"

// Declare assinaturas das funções no início do arquivo .h ou no topo
// do seu arquivo .c.
void funcao_1(char c);
int funcao_2(void);

// Deve-se declarar um 'protótipo de função' antes do main() quando as ocorrências
// dessas funções estão após sua função main()
int soma_dois_ints(int x1, int x2); // protótipo de função

// O ponto de entrada do teu programa é uma função
// chamada main, com tipo de retorno inteiro
int main() {
	// Usa-se printf para escrever na tela,
	// para "saída formatada"
	// %d é um inteiro, \n é uma nova linha
    printf("%d\n", 0); // => Imprime 0
	// Todos as declarações devem acabar com
	// ponto e vírgula

    ///////////////////////////////////////
    // Tipos
    ///////////////////////////////////////

    // ints normalmente tem 4 bytes
    int x_int = 0;

    // shorts normalmente tem 2 bytes
    short x_short = 0;

    // chars sempre tem um byte
    char x_char = 0;
    char y_char = 'y'; // Literais de caracter são cercados por '

    // longs tem entre 4 e 8 bytes; longs long tem garantia
    // de ter pelo menos 64 bits
    long x_long = 0;
    long long x_long_long = 0;

    // floats são normalmente números de ponto flutuante
	// com 32 bits
    float x_float = 0.0;

    // doubles são normalmente números de ponto flutuante
	// com 64 bits
    double x_double = 0.0;

    // Tipos inteiros podem ser sem sinal.
    unsigned short ux_short;
    unsigned int ux_int;
    unsigned long long ux_long_long;

	// caracteres dentro de aspas simples são inteiros
	// no conjunto de caracteres da máquina.
    '0' // => 48 na tabela ASCII.
    'A' // => 65 na tabela ASCII.

	// sizeof(T) devolve o tamanho de uma variável do tipo T em bytes
	// sizeof(obj) devolve o tamanho de uma expressão (variável, literal, etc.).
    printf("%zu\n", sizeof(int)); // => 4 (na maioria das máquinas com palavras de 4 bytes)

	// Se o argumento do operador `sizeof` é uma expressão, então seus argumentos
	// não são avaliados (exceto em VLAs (veja abaixo)).
	// O valor devolve, neste caso, é uma constante de tempo de compilação.
    int a = 1;
	// size_t é um inteiro sem sinal com pelo menos 2 bytes que representa
	// o tamanho de um objeto.
    size_t size = sizeof(a++); // a++ não é avaliada.
    printf("sizeof(a++) = %zu where a = %d\n", size, a);
    // imprime "sizeof(a++) = 4 onde a = 1" (quando em uma arquitetura de 32 bits)

	// Arrays precisam ser inicializados com um tamanho concreto
    char meu_char_array[20]; // Este array ocupa 1 * 20 = 20 bytes
    int meu_int_array[20]; // Este array ocupa 4 * 20 = 80 bytes
                           // (assumindo palavras de 4 bytes)

	// Você pode inicializar um array com 0 desta forma:
    char meu_array[20] = {0};

	// Indexar um array é semelhante a outras linguagens
	// Melhor dizendo, outras linguagens são semelhantes a C
    meu_array[0]; // => 0

	// Array são mutáveis; são apenas memória!
    meu_array[1] = 2;
    printf("%d\n", meu_array[1]); // => 2

	// No C99 (e como uma features opcional em C11), arrays de tamanho variável
	// VLA (do inglês), podem ser declarados também. O tamanho destes arrays
	// não precisam ser uma constante de tempo de compilação:
    printf("Entre o tamanho do array: "); // Pergunta ao usuário pelo tamanho
    char buf[0x100];
    fgets(buf, sizeof buf, stdin);

    // strtoul transforma a string em um inteiro sem sinal
    size_t size = strtoul(buf, NULL, 10);
    int var_length_array[size]; // declara o VLA
    printf("sizeof array = %zu\n", sizeof var_length_array);

    // Uma possível saída para esse programa seria:
    // > Entre o tamanho do array: 10
    // > sizeof array = 40

	// String são apenas arrays de caracteres terminados por um
	// byte nulo (0x00), representado em string pelo caracter especial '\0'.
	// (Não precisamos incluir o byte nulo em literais de string; o compilador
	// o insere ao final do array para nós.)
    char uma_string[20] = "Isto é uma string";
	// Observe que 'é' não está na tabela ASCII
	// A string vai ser salva, mas a saída vai ser estranha
	// Porém, comentários podem conter acentos
    printf("%s\n", uma_string); // %s formata a string

    printf("%d\n", uma_string[17]); // => 0
    // i.e., byte #18 é 0 (assim como o 19°, 20°, 21°...)

	// Se temos caracteres entre aspas simples, temos um caracter literal.
	// Seu tipo é `int`, *não* `char` (por razões históricas).
    int cha = 'a'; // ok
    char chb = 'a'; // ok também (conversão implícita de int para char)

	// Arrays multi-dimensionais:
    int multi_array[2][5] = {
        {1, 2, 3, 4, 5},
        {6, 7, 8, 9, 0}
    };
    // Acesso a elementos:
    int array_int = multi_array[0][2]; // => 3

    ///////////////////////////////////////
    // Operadores
    ///////////////////////////////////////

    // Atalho para multiplas declarações:
    int i1 = 1, i2 = 2;
    float f1 = 1.0, f2 = 2.0;

    int a, b, c;
    a = b = c = 0;

    // Aritimética é óbvia
    i1 + i2; // => 3
    i2 - i1; // => 1
    i2 * i1; // => 2
    i1 / i2; // => 0 (0.5, porém, é truncado para 0)

    f1 / f2; // => 0.5, mais ou menos epsilon
    // Números e cálculos de ponto flutuante não são exatos

    // Modulo também existe
    11 % 3; // => 2

	// Operadores de comparação provavelmente são familiares,
	// porém não há tipo booleano em C. Usa-se ints no lugar.
	// (Ou _Bool or bool em C99.)
	// 0 é falso e qualquer outra coisa é verdadeiro
	// (Os operadores de comparação devolvem 0 ou 1.)
    // Comparison operators are probably familiar, but
    3 == 2; // => 0 (falso)
    3 != 2; // => 1 (verdadeiro)
    3 > 2; // => 1
    3 < 2; // => 0
    2 <= 2; // => 1
    2 >= 2; // => 1

	// C não é Python - comparações não se encadeiam.
    int a = 1;
    // Errado:
    int entre_0_e_2 = 0 < a < 2;
    // Correto:
    int entre_0_e_2 = 0 < a && a < 2;

    // Lógica funciona sobre ints
    !3; // => 0 (Não lógico)
    !0; // => 1
    1 && 1; // => 1 (E lógico)
    0 && 1; // => 0
    0 || 1; // => 1 (Ou lógico)
    0 || 0; // => 0

    //Expressão condicional ternária ( ? : )
    int a = 5;
    int b = 10;
    int z;
    z = (a > b) ? a : b; // => 10 "se a > b retorne a, senão retorne b."

    //Operadores de incremento e decremento:
    char *s = "iLoveC";
    int j = 0;
    s[j++]; // => "i". Retorna o j-ésimo item de s E DEPOIS incrementa o valor de j.
    j = 0;
    s[++j]; // => "L". Incrementa o valor de j. E DEPOIS retorna o j-ésimo item de s.
    // o mesmo com j-- e --j

    // Operadores bit a bit!
    ~0x0F; // => 0xF0 (negação bit a bit, "complemento de 1")
    0x0F & 0xF0; // => 0x00 (bit a bit E)
    0x0F | 0xF0; // => 0xFF (bit a bit OU)
    0x04 ^ 0x0F; // => 0x0B (bit a bit OU EXCLUSIVO)
    0x01 << 1; // => 0x02 (bit a bit shift para esquerda (por 1))
    0x02 >> 1; // => 0x01 (bit a bit shift para direita (por 1))

	// Cuidado quando fizer shift em inteiro com sinal - o seguinte é indefinido:
	// - Fazer shift sobre um bit de sinal de um inteiro com sinal (int a = 1 << 32)
	// - Fazer shift a esquerda sobre um número negativo (int a = -1 << 2)
	// - Fazer shift maior que a largura do tipo de LHS:
	//   int a = 1 << 32; // Indefinido se int é de tamanho 32 bits

    ///////////////////////////////////////
    // Estruturas de Controle
    ///////////////////////////////////////

    if (0) {
      printf("Nunca rodará\n");
    } else if (0) {
      printf("Também nunca rodará\n");
    } else {
      printf("Eu serei impresso\n");
    }

    // Loops while existem
    int ii = 0;
    while (ii < 10) { //QUALQUER valor diferente de 0 é verdadeiro
        printf("%d, ", ii++); // ii++ incrementa o valor de ii APÓS usá-lo
    } // => imprime "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    int kk = 0;
    do {
        printf("%d, ", kk);
    } while (++kk < 10); // ++kk incrementa o valor de kk ANTES de usá-lo
    // => imprime "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    // Loops for também
    int jj;
    for (jj=0; jj < 10; jj++) {
        printf("%d, ", jj);
    } // => imprime "0, 1, 2, 3, 4, 5, 6, 7, 8, 9, "

    printf("\n");

    // *****NOTAS*****:
	// Loops e Funções PRECISAM ter um corpo. Se nenhum corpo é necessário:
    int i;
    for (i = 0; i <= 5; i++) {
        ; // Use ponto e vírgula para agir como um corpo (declaração nula)
    }
    // Ou
    for (i = 0; i <= 5; i++);

	// Criando branchs com escolhas múltiplas: switch()
    switch (alguma_expressao_integral) {
    case 0: // labels precisam ser expressões integrais **constantes**
        faca_algo();
        break; // Sem break, o controle continua após a label
    case 1:
        faca_outra_coisa();
        break;
    default:
        // Se `alguma_expressao_integral` não coincidir com nenhuma label
        fputs("erro!\n", stderr);
        exit(-1);
        break;
    }


    ///////////////////////////////////////
    // Cast de tipos
    ///////////////////////////////////////

	// Todo valor em C tem um tipo, mas você pode fazer um cast de um valor em outro tipo
	// se você quiser (com algumas restrições).

    int x_hex = 0x01; // Você pode colocar valores hexadecimais em variáveis

	// Cast entre tipos tentará preservar seus valores numéricos
    printf("%d\n", x_hex); // => Imprime 1
    printf("%d\n", (short) x_hex); // => Imprime 1
    printf("%d\n", (char) x_hex); // => Imprime 1

	// Tipos irão ter overflow sem aviso
    printf("%d\n", (unsigned char) 257); // => 1 (Max char = 255 se char tem 8 bits)

	// Para determinar o valor máximo de um `char`, de um `signed char` e de
	// um `unisigned char`, respectivamente, use as macros CHAR_MAX, SCHAR_MAX
	// e UCHAR_MAX de <limits.h>

	// Tipos inteiros podem sofrer cast para pontos-flutuantes e vice-versa.
    printf("%f\n", (float)100); // %f formata um float
    printf("%lf\n", (double)100); // %lf formata um double
    printf("%d\n", (char)100.0);

    ///////////////////////////////////////
    // Ponteiros
    ///////////////////////////////////////

	// Um ponteiro é uma variável declarada para armazenar um endereço de memória.
	// Sua declaração irá também dizer o tipo de dados para o qual ela aponta. Você
	// Pode usar o endereço de memória de suas variáveis, então, brincar com eles.

    int x = 0;
    printf("%p\n", (void *)&x); // Use & para usar o endereço de uma variável
    // (%p formata um objeto ponteiro do tipo void *)
    // => Imprime algum endereço de memória;

	// Ponteiros começam com * na sua declaração
    int *px, nao_eh_um_ponteiro; // px é um ponteiro para um int
    px = &x; // armazena o endereço de x em px
    printf("%p\n", (void *)px); // => Imprime algum endereço de memória
    printf("%zu, %zu\n", sizeof(px), sizeof(nao_eh_um_ponteiro));
    // => Imprime "8, 4" em um sistema típico de 64 bits

	// Para pegar um valor no endereço apontado por um ponteiro,
	// coloque * na frente para de-referenciá-lo.
	// Nota: sim, é confuso usar '*' _tanto_ para declaração de ponteiro
	// como para de-referenciá-lo.
    printf("%d\n", *px); // => Imprime 0, o valor de x

	// Você também pode mudar o valor que o ponteiro está apontando.
	// Temos que cercar a de-referência entre parênteses, pois
	// ++ tem uma precedência maior que *.
    (*px)++; // Incrementa o valor que px está apontando por 1
    printf("%d\n", *px); // => Imprime 1
    printf("%d\n", x); // => Imprime 1

	// Arrays são uma boa maneira de alocar um bloco contínuo de memória
    int x_array[20]; // Declara um array de tamanho 20 (não pode-se mudar o tamanho
    int xx;
    for (xx = 0; xx < 20; xx++) {
        x_array[xx] = 20 - xx;
    } //Inicializa x_array com 20, 19, 18,... 2, 1

	// Declara um ponteiro do tipo int e inicialize ele para apontar para x_array
    int* x_ptr = x_array;
	// x_ptr agora aponta para o primeiro elemento do array (o inteiro 20).
	// Isto funciona porque arrays são apenas ponteiros para seus primeiros elementos.
	// Por exemplo, quando um array é passado para uma função ou é atribuído a um
	// ponteiro, ele transforma-se (convertido implicitamente) em um ponteiro.
	// Exceções: quando o array é o argumento de um operador `&` (endereço-de):
    // Exceptions: when the array is the argument of the `&` (address-of) operator:
    int arr[10];
    int (*ptr_to_arr)[10] = &arr; // &arr não é do tipo `int *`!
                                  // É do tipo "ponteiro para array" (de `int`s).
    // ou quando o array é uma string literal usada para inicializar um array de char:
    char arr[] = "foobarbazquirk";
	// ou quando é um argumento dos operadores `sizeof` ou `alignof`:
    int arr[10];
    int *ptr = arr; // equivalente a int *ptr = &arr[0];
    printf("%zu, %zu\n", sizeof arr, sizeof ptr); // provavelmente imprime "40, 4" ou "40, 8"

	// Ponteiros podem ser incrementados ou decrementados baseado no seu tipo
	// (isto é chamado aritmética de ponteiros
    printf("%d\n", *(x_ptr + 1)); // => Imprime 19
    printf("%d\n", x_array[1]); // => Imprime 19

	// Você também pode alocar dinamicamente blocos de memória com a função
	// da biblioteca padrão malloc, a qual recebe um argumento do tipo size_t
	// representando o número de bytes a ser alocado (geralmente da heap, apesar de
	// isto poder não ser verdadeiro em, e.g., sistemas embarcados - o C padrão diz
	// nada sobre isso).
    int *my_ptr = malloc(sizeof(*my_ptr) * 20);
    for (xx = 0; xx < 20; xx++) {
        *(my_ptr + xx) = 20 - xx; // my_ptr[xx] = 20-xx
    } //Inicializa a memória com 20, 19, 18, 17... 2, 1 (como ints)

	// Dereferenciar memória que você não alocou cria
	// "resultados imprevisíveis" - o programa é dito ter um "comportamento indefinido"
    printf("%d\n", *(my_ptr + 21)); // => Imprime quem-sabe-o-que? Talvez até quebre o programa.

	// Quando se termina de usar um bloco de memória alocado, você pode liberá-lo,
	// ou ninguém mais será capaz de usá-lo até o fim da execução
	// (Isto chama-se "memory leak"):
    free(my_ptr);

	// Strings são arrays de char, mas elas geralmente são representadas
	// como um ponteiro para char (com o apontador para o primeiro elemento do array).
	// É boa prática usar `const char *' quando de-referenciando uma literal string,
	// dado que elas não deverão ser modificadas (i.e. "foo"[0] = 'a' é ILEGAL.)
    const char *my_str = "Esta é a minha literal string";
    printf("%c\n", *my_str); // => 'T'

	// Este não é o caso se a string for um array
	// (potencialmente inicializado com um literal string)
	// que reside em uma memória de escrita, como em:
    char foo[] = "foo";
    foo[0] = 'a'; // Isto é legal, foo agora contém "aoo"

    funcao_1();
} // fim da função main

///////////////////////////////////////
// Funções
///////////////////////////////////////

//Sintaxe de declaração de funções:
// <tipo de retorno> <nome da função>(<argumentos>)

int soma_dois_int(int x1, int x2)
{
    return x1 + x2; // Use return para retornar um valor
}

/*
Funções são chamadas por valor. Quando uma função é chamada, os argumentos passados
para a função são cópias dos argumento originais (a não ser arrays). Qualquer coisa
que você faz nos argumentos de uma função não alteram o valor do argumento original
onde a função foi chamada.

Use ponteiros se você precisa alterar os valores dos argumentos originais

Exemplo: reversão de string in-place
*/

// Uma função void não retorna valor algum
void str_reverse(char *str_in)
{
    char tmp;
    int ii = 0;
    size_t len = strlen(str_in); // `strlen()` é parte da biblioteca padrão C
    for (ii = 0; ii < len / 2; ii++) {
        tmp = str_in[ii];
        str_in[ii] = str_in[len - ii - 1]; // iiº char do final
        str_in[len - ii - 1] = tmp;
    }
}

/*
char c[] = "Isto é um teste.";
str_reverse(c);
printf("%s\n", c); // => ".etset mu é otsI"
*/

// Se estiver referenciando variáveis externas à função, use a palavra-chave extern.
int i = 0;
void testFunc() {
    extern int i; //i aqui agora está usando a variável externa
}

// Faça variáveis externas privadas para o código-fonte com static:
static int i = 0; // Outros arquivos usando testFunc() não podem acessar a variável i
void testFunc() {
    extern int i;
}
//**Você pode declarar funções como static para torná-las privadas**


///////////////////////////////////////
// Tipos definidos pelo usuário e structs
///////////////////////////////////////

// Typedefs podem ser usadas para criar apelidos para tipos
typedef int meu_tipo;
meu_tipo var_meu_tipo = 0;

// Structs são apenas coleções de dados, os membros são alocados sequencialmente,
// na ordem que são escritos:
struct retangulo {
    int altura;
    int largura;
};

// Geralmente não é verdade que
// sizeof(struct retangulo) == sizeof(int) + sizeof(int)
// devido ao potencial de preenchimento entre os membros da estrutura
// (isto é por razões de alinhamento). [1]

void funcao_1()
{
    struct retangulo meu_retan;

    // Acesse os membros da estrutura com .
    meu_retan.altura = 10;
    meu_retan.largura = 20;

	// Você pode declarar ponteiros para structs
    struct retangulo *meu_retan_ptr = &meu_retan;

	// Use de-referenciamento para setar os membros da
	// struct apontada...
    (*meu_retan_ptr).altura = 30;

	// ... ou ainda melhor: prefira usar o atalho -> para melhorar legibilidade
    meu_retan_ptr->largura = 10; // O mesmo que (*meu_retan_ptr).largura = 10;
}

//Você pode aplicar um typedef para uma struct por conveniência
typedef struct retangulo retan;

int area(retan r)
{
    return r.largura * r.altura;
}

// Se você tiver structus grande, você pode passá-las "por ponteiro"
// para evitar cópia de toda a struct:
int area(const retan *r)
{
    return r->largura * r->altura;
}

///////////////////////////////////////
// Ponteiros para funções
///////////////////////////////////////
/*
Em tempo de execução, funções são localizadas em endereços de memória
conhecidos. Ponteiros para funções são como qualquer outro ponteiro
(apenas guardam endereços de memória), mas podem ser usados para invocar funções
diretamente e passá-las para por toda parte.
Entretanto, a sintaxe de definição por ser um pouco confusa.

Exemplo: use str_reverso através de um ponteiro
*/
void str_reverso_através_ponteiro(char *str_entrada) {
    // Define uma variável de ponteiro para função, nomeada f.
    void (*f)(char *); //Assinatura deve ser exatamente igual à função alvo.
    f = &str_reverso; //Atribue o endereço da função em si (determinado em tempo de execução.
    // f = str_reverso; Também funciona - função tornam-se ponteiros, assim como arrays
    (*f)(str_entrada); // Chamando a função através do ponteiro
    // f(str_entrada); // Esta é uma sintaxe alternativa, mas equivalente.
}

/*
Desde que as assinaturas das funções sejam compatíveis, você pode atribuir qualquer
função ao mesmo ponteiro. Ponteiros para funções são geralmente um typedef por
simplicidade e legibilidade, como segue:
*/

typedef void (*minha_função_type)(char *);

// Declarando o ponteiro:
// ...
// minha_função_type f;

//Caracteres especiais:
'\a' // Alerta (sino)
'\n' // Nova linha
'\t' // Tab (justifica texto a esquerda)
'\v' // Tab vertical
'\f' // Nova linha (formfeed)
'\r' // Retorno de carroça
'\b' // Backspace
'\0' // Caracter nulo. Geralmente colocado ao final de string em C.
     //   oi\n\0. \0 é usado por convenção para marcar o fim da string.
'\\' // Barra invertida
'\?' // Interrogação
'\'' // Aspas simples
'\"' // Aspas duplas
'\xhh' // Número hexadecimal. Exemplo: '\xb' = tab vertical
'\ooo' // Número octal. Exemplo: '\013' = tab vertical

// formatando impressão:
"%d"    // inteiro
"%3d"   // inteiro com pelo menos 3 dígitos (justifica texto a direita)
"%s"    // string
"%f"    // ponto-flutuante
"%ld"   // long
"%3.2f" // ponto-flutuante com pelo menos 3 dígitos a esquerda e 2 a direita
"%7.4s" // (também pode-se fazer com strings)
"%c"    // char
"%p"    // ponteiro
"%x"    // hexadecimal
"%o"    // octal
"%%"    // imprime %

///////////////////////////////////////
// Ordem de avaliação
///////////////////////////////////////

//-----------------------------------------------------------//
//        Operadores                       | Associatividade //
//-----------------------------------------------------------//
// () [] -> .                        | esquerda para direita //
// ! ~ ++ -- + = *(type)sizeof       | direita para esqureda //
// * / %                             | esquerda para direita //
// + -                               | esquerda para direita //
// << >>                             | esquerda para direita //
// < <= > >=                         | esquerda para direita //
// == !=                             | esquerda para direita //
// &                                 | esquerda para direita //
// ^                                 | esquerda para direita //
// |                                 | esquerda para direita //
// &&                                | esquerda para direita //
// ||                                | esquerda para direita //
// ?:                                | direita para esqureda //
// = += -= *= /= %= &= ^= |= <<= >>= | direita para esqureda //
// ,                                 | esquerda para direita //
//-----------------------------------------------------------//

```

## Leitura adicional

É recomendado ter uma cópia de [K&R, aka "The C Programming Language"](https://en.wikipedia.org/wiki/The_C_Programming_Language).
Este é *o* livro sobre C, escrito pelos criadores da linguage. Mas cuidado - ele é antigo e contém alguns erros (bem,
ideias que não são consideradas boas hoje) ou práticas mudadas.

Outra boa referência é [Learn C the hard way](http://c.learncodethehardway.org/book/).

Se você tem uma pergunta, leia [compl.lang.c Frequently Asked Questions](http://c-faq.com).

É importante usar espaços e indentação adequadamente e ser consistente com seu estilo de código em geral.
Código legível é melhor que código 'esperto' e rápido. Para adotar um estilo de código bom e são, veja
[Linux kernel coding stlye](https://www.kernel.org/doc/Documentation/CodingStyle).

Além disso, Google é teu amigo.
[1] http://stackoverflow.com/questions/119123/why-isnt-sizeof-for-a-struct-equal-to-the-sum-of-sizeof-of-each-member
