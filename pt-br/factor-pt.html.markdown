---
language: factor
contributors:
    - ["hyphz", "http://github.com/hyphz/"]
filename: learnfactor-br.factor

lang: pt-br
---

Factor é uma linguagem moderna baseada em pilha, baseado em Forth, criada por Slava Pestov.

Código neste arquivo pode ser digitado em Fator, mas não importado diretamente porque o cabeçalho de vocabulário e importação faria o início completamente confuso.

```factor
! Este é um comentário

! Como Forth, toda a programação é feita manipulando a pilha.
! A indicação de um valor literal o coloca na pilha.
5 2 3 56 76 23 65    ! Nenhuma saída, mas a pilha é impressa no modo interativo

! Esses números são adicionados à pilha, da esquerda para a direita.
! .s imprime a pilha de forma não destrutiva.
.s     ! 5 2 3 56 76 23 65

! A aritmética funciona manipulando dados na pilha.
5 4 +    ! Sem saída

! `.` mostra o resultado superior da pilha e o imprime.
.    ! 9

! Mais exemplos de aritmética:
6 7 * .        ! 42
1360 23 - .    ! 1337
12 12 / .      ! 1
13 2 mod .     ! 1

99 neg .       ! -99
-99 abs .      ! 99
52 23 max .    ! 52
52 23 min .    ! 23

! Várias palavras são fornecidas para manipular a pilha, coletivamente conhecidas como palavras embaralhadas.

3 dup -          ! duplica o primeiro item (1st agora igual a 2nd): 3 - 3
2 5 swap /       ! troca o primeiro com o segundo elemento:         5 / 2
4 0 drop 2 /     ! remove o primeiro item (não imprima na tela):    4 / 2
1 2 3 nip .s     ! remove o segundo item (semelhante a drop):       1 3
1 2 clear .s     ! acaba com toda a pilha
1 2 3 4 over .s  ! duplica o segundo item para o topo:   1 2 3 4 3
1 2 3 4 2 pick .s ! duplica o terceiro item para o topo: 1 2 3 4 2 3

! Criando Palavras
! O `:` conjuntos de palavras do Factor no modo de compilação até que ela veja a palavra `;`.
: square ( n -- n ) dup * ;    ! Sem saída
5 square .                     ! 25

! Podemos ver o que as palavra fazem também.
! \ suprime a avaliação de uma palavra e coloca seu identificador na pilha.
\ square see    ! : square ( n -- n ) dup * ;

! Após o nome da palavra para criar, a declaração entre parênteses dá o efeito da pilha.
! Podemos usar os nomes que quisermos dentro da declaração:
: weirdsquare ( camel -- llama ) dup * ;

! Contanto que sua contagem corresponda ao efeito da pilha da palavra:
: doubledup ( a -- b ) dup dup ; ! Error: Stack effect declaration is wrong
: doubledup ( a -- a a a ) dup dup ; ! Ok
: weirddoubledup ( i -- am a fish ) dup dup ; ! Além disso Ok

! Onde Factor difere do Forth é no uso de citações.
! Uma citação é um bloco de código que é colocado na pilha como um valor.
! [ inicia o modo de citação; ] termina.
[ 2 + ]       ! A citação que adiciona 2 é deixada na pilha
4 swap call . ! 6

! E assim, palavras de ordem mais alta. TONS de palavras de ordem superior.
2 3 [ 2 + ] dip .s      ! Retira valor do topo da pilha, execute citação, empurre de volta: 4 3
3 4 [ + ] keep .s       ! Copie o valor do topo da pilha, execute a citação, envie a cópia: 7 4
1 [ 2 + ] [ 3 + ] bi .s ! Executar cada citação no valor do topo, empurrar os dois resultados: 3 4
4 3 1 [ + ] [ + ] bi .s ! As citações em um bi podem extrair valores mais profundos da pilha: 4 5 ( 1+3 1+4 )
1 2 [ 2 + ] bi@ .s      ! Executar a citação no primeiro e segundo valores
2 [ + ] curry           ! Injeta o valor fornecido no início da citação: [ 2 + ] é deixado na pilha

! Condicionais
! Qualquer valor é verdadeiro, exceto o valor interno f.
! m valor interno não existe, mas seu uso não é essencial.
! Condicionais são palavras de maior ordem, como com os combinadores acima.

5 [ "Five is true" . ] when                     ! Cinco é verdadeiro
0 [ "Zero is true" . ] when                     ! Zero é verdadeiro
f [ "F is true" . ] when                        ! Sem saída
f [ "F is false" . ] unless                     ! F é falso
2 [ "Two is true" . ] [ "Two is false" . ] if   ! Two é verdadeiro

! Por padrão, as condicionais consomem o valor em teste, mas variantes com asterisco
! deixe sozinho se é verdadeiro:

5 [ . ] when*      ! 5
f [ . ] when*      ! Nenhuma saída, pilha vazia, f é consumida porque é falsa


! Laços
! Você adivinhou .. estas são palavras de ordem mais elevada também.

5 [ . ] each-integer               ! 0 1 2 3 4
4 3 2 1 0 5 [ + . ] each-integer   ! 0 2 4 6 8
5 [ "Hello" . ] times              ! Hello Hello Hello Hello Hello

! Here's a list:
{ 2 4 6 8 }                        ! Goes on the stack as one item

! Aqui está uma lista:
{ 2 4 6 8 } [ 1 + . ] each          ! Exibe 3 5 7 9
{ 2 4 6 8 } [ 1 + ] map             ! Sai { 3 5 7 9 } na pilha

! Reduzir laços ou criar listas:
{ 1 2 3 4 5 } [ 2 mod 0 = ] filter  ! Mantém apenas membros da lista para os quais a citação é verdadeira: { 2 4 }
{ 2 4 6 8 } 0 [ + ] reduce .        ! Como "fold" em linguagens funcionais: exibe 20 (0+2+4+6+8)
{ 2 4 6 8 } 0 [ + ] accumulate . .  ! Como reduzir, mas mantém os valores intermediários em uma lista: exibe { 0 2 6 12 } então 20
1 5 [ 2 * dup ] replicate .         ! Repete a citação 5 vezes e coleta os resultados em uma lista: { 2 4 8 16 32 }
1 [ dup 100 < ] [ 2 * dup ] produce ! Repete a segunda citação até que a primeira retorne como falsa e colete os resultados: { 2 4 8 16 32 64 128 }

! Se tudo mais falhar, uma finalidade geral, enquanto repete:
1 [ dup 10 < ] [ "Hello" . 1 + ] while  ! Exibe "Hello" 10 vezes
                                        ! Sim, é difícil de ler
                                        ! Isso é o que todos esses loops variantes são para

! Variáveis
! Normalmente, espera-se que os programas Factor mantenham todos os dados na pilha.
! Usar variáveis ​​nomeadas torna a refatoração mais difícil (e é chamada de Factor por um motivo)
! Variáveis ​​globais, se você precisar:

SYMBOL: name            ! Cria o nome como uma palavra identificadora
"Bob" name set-global   ! Sem saída
name get-global .       ! "Bob"

! Variáveis ​​locais nomeadas são consideradas uma extensão, mas estão disponíveis
! Em uma citação ..
[| m n                  ! A citação captura os dois principais valores da pilha em m e n
 | m n + ]              ! Leia-os

! Ou em uma palavra..
:: lword ( -- )           ! Note os dois pontos duplos para invocar a extensão da variável lexica
   2 :> c                 ! Declara a variável imutável c para manter 2
   c . ;                  ! Imprima isso

! Em uma palavra declarada dessa maneira, o lado de entrada da declaração de pilha
! torna-se significativo e fornece os valores das variáveis ​​em que os valores da pilha são capturados
:: double ( a -- result ) a 2 * ;

! Variáveis ​​são declaradas mutáveis ​​ao terminar seu nome com um ponto de exclamação
:: mword2 ( a! -- x y )   ! Capture o topo da pilha na variável mutável a
   a                      ! Empurrar a
   a 2 * a!               ! Multiplique por 2 e armazene o resultado em a
   a ;                    ! Empurre novo valor de a
5 mword2                  ! Pilha: 5 10

! Listas e Sequências
! Vimos acima como empurrar uma lista para a pilha

0 { 1 2 3 4 } nth         ! Acessar um membro específico de uma lista: 1
10 { 1 2 3 4 } nth        ! Error: índice de sequência fora dos limites
1 { 1 2 3 4 } ?nth        ! O mesmo que nth se o índice estiver dentro dos limites: 2
10 { 1 2 3 4 } ?nth       ! Nenhum erro se estiver fora dos limites: f

{ "at" "the" "beginning" } "Append" prefix    ! { "Append" "at" "the" "beginning" }
{ "Append" "at" "the" } "end" suffix          ! { "Append" "at" "the" "end" }
"in" 1 { "Insert" "the" "middle" } insert-nth ! { "Insert" "in" "the" "middle" }
"Concat" "enate" append                       ! "Concatenate" - strings are sequences too
"Concatenate" "Reverse " prepend              ! "Reverse Concatenate"
{ "Concatenate " "seq " "of " "seqs" } concat ! "Concatenate seq of seqs"
{ "Connect" "subseqs" "with" "separators" } " " join  ! "Connect subseqs with separators"

! E se você quiser obter meta, as citações são seqüências e podem ser desmontadas..
0 [ 2 + ] nth                              ! 2
1 [ 2 + ] nth                              ! +
[ 2 + ] \ - suffix                         ! Quotation [ 2 + - ]


```

##Pronto para mais?

* [Documentação do Factor](http://docs.factorcode.org/content/article-help.home.html)
