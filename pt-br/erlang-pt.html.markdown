---
language: erlang
filename: learnerlang-pt.erl
contributors:
    - ["Giovanni Cappellotto", "http://www.focustheweb.com/"]
translators:
    - ["Guilherme Heuser Prestes", "http://twitter.com/gprestes"]
lang: pt-br
---

```erlang
% Símbolo de porcento começa comentários de uma linha.

%% Dois caracteres de porcento devem ser usados para comentar funções.

%%% Três caracteres de porcento devem ser usados para comentar módulos.

% Nós usamos três tipos de pontuação em Erlang.
% Vírgulas (`,`) separam argumentos em chamadas de função, construtores de
% dados, e padrões.
% Pontos finais (`.`) separam totalmente funções e expressões no prompt.
% Ponto e vírgulas (`;`) separam cláusulas. Nós encontramos cláusulas em
% vários contextos: definições de função e em expressões com `case`, `if`,
% `try..catch` e `receive`.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Variáveis e casamento de padrões.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Num = 42.  % Todos nomes de variáveis devem começar com uma letra maiúscula.

% Erlang tem atribuição única de variáveis, se você tentar atribuir um valor
% diferente à variável `Num`, você receberá um erro.
Num = 43. % ** exception error: no match of right hand side value 43

% Na maioria das linguagens, `=` denota um comando de atribuição. Em Erlang, no
% entanto, `=` denota uma operação de casamento de padrão. `Lhs = Rhs` realmente
% significa isso: avalia o lado direito (Rhs), e então casa o resultado com o
% padrão no lado esquerdo (Lhs).
Num = 7 * 6.

% Número de ponto flutuante.
Pi = 3.14159.

% Átomos são usados para representar diferentes valores constantes não
% numéricos. Átomos começam com letras minúsculas seguidas por uma sequência de
% caracteres alfanuméricos ou sinais de subtraço (`_`) ou arroba (`@`).
Hello = hello.
OtherNode = example@node.

% Átomos com valores alfanuméricos podem ser escritos colocando aspas por fora
% dos átomos.
AtomWithSpace = 'some atom with space'.

% Tuplas são similares a structs em C.
Point = {point, 10, 45}.

% Se nós queremos extrair alguns valores de uma tupla, nós usamos o operador `=`.
{point, X, Y} = Point.  % X = 10, Y = 45

% Nós podemos usar `_` para ocupar o lugar de uma variável que não estamos interessados.
% O símbolo `_` é chamado de variável anônima. Ao contrário de variáveis regulares,
% diversas ocorrências de _ no mesmo padrão não precisam se amarrar ao mesmo valor.
Person = {person, {name, {first, joe}, {last, armstrong}}, {footsize, 42}}.
{_, {_, {_, Who}, _}, _} = Person.  % Who = joe

% Nós criamos uma lista colocando valores separados por vírgula entre colchetes.
% Cada elemento de uma lista pode ser de qualquer tipo.
% O primeiro elemento de uma lista é a cabeça da lista. Se removermos a cabeça
% da lista, o que sobra é chamado de cauda da lista.
ThingsToBuy = [{apples, 10}, {pears, 6}, {milk, 3}].

% Se `T` é uma lista, então `[H|T]` também é uma lista, com cabeça `H` e cauda `T`.
% A barra vertical (`|`) separa a cabeça de uma lista de sua cauda.
% `[]` é uma lista vazia.
% Podemos extrair elementos de uma lista com uma operação de casamento de
% padrão. Se temos uma lista não-vazia `L`, então a expressão `[X|Y] = L`, onde
% `X` e `Y` são variáveis desamarradas, irá extrair a cabeça de uma lista para
% `X` e a cauda da lista para `Y`.
[FirstThing|OtherThingsToBuy] = ThingsToBuy.
% FirstThing = {apples, 10}
% OtherThingsToBuy = {pears, 6}, {milk, 3}

% Não existe o tipo string em Erlang. Strings são somente listas de inteiros.
% Strings são representadas dentro de aspas duplas (`"`).
Name = "Hello".
[72, 101, 108, 108, 111] = "Hello".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. Programação sequencial.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Módulos são a unidade básica de código em Erlang. Todas funções que
% escrevemos são armazenadas em módulos. Módulos são armazenados em arquivos
% com extensão `.erl`.
% Módulos devem ser compilados antes que o código possa ser rodado. Um módulo
% compilado tem a extensão `.beam`.
-module(geometry).
-export([area/1]). % lista de funções exportadas de um módulo.

% A função `area` consiste de duas cláusulas. As cláusulas são separadas por um
% ponto e vírgula, e a cláusula final é terminada por um ponto final.
% Cada cláusula tem uma cabeça em um corpo; a cabeça consiste de um nome de
% função seguido por um padrão (entre parêntesis), e o corpo consiste de uma
% sequência de expressões, que são avaliadas se o padrão na cabeça é um par bem
% sucedido dos argumentos da chamada. Os padrões são casados na ordem que
% aparecem na definição da função.
area({rectangle, Width, Ht}) -> Width * Ht;
area({circle, R})            -> 3.14159 * R * R.

% Compila o código no arquivo geometry.erl.
c(geometry).  % {ok,geometry}

% Nós precisamos incluir o nome do módulo junto com o nome da função de maneira
% a identificar exatamente qual função queremos chamar.
geometry:area({rectangle, 10, 5}).  % 50
geometry:area({circle, 1.4}).  % 6.15752

% Em Erlang, duas funções com o mesmo nome e diferentes aridades (números de
% argumentos) no mesmo módulo representam funções totalmente diferentes.
-module(lib_misc).
-export([sum/1]). % exporta a função `sum` de aridade 1 aceitando um argumento: lista de inteiros.
sum(L) -> sum(L, 0).
sum([], N)    -> N;
sum([H|T], N) -> sum(T, H+N).

% Funs são funções "anônimas". Elas são chamadas desta maneira por que elas não
% têm nome. No entanto podem ser atribuídas a variáveis.
Double = fun(X) -> 2*X end. % `Double` aponta para uma função anônima com referência: #Fun<erl_eval.6.17052888>
Double(2).  % 4

% Funções aceitam funs como seus argumentos e podem retornar funs.
Mult = fun(Times) -> ( fun(X) -> X * Times end ) end.
Triple = Mult(3).
Triple(5).  % 15

% Compreensão de lista são expressões que criam listas sem precisar usar funs,
% maps, ou filtros.
% A notação `[F(X) || X <- L]` significa "a lista de `F(X)` onde `X` é tomada
% da lista `L`."
L = [1,2,3,4,5].
[2*X || X <- L].  % [2,4,6,8,10]
% Uma compreensão de lista pode ter geradores e filtros que selecionam
% subconjuntos dos valores gerados.
EvenNumbers = [N || N <- [1, 2, 3, 4], N rem 2 == 0]. % [2, 4]

% Sentinelas são contruções que podemos usar para incrementar o poder de
% casamento de padrão. Usando sentinelas, podemos executar testes simples e
% comparações nas variáveis em um padrão.
% Você pode usar sentinelas nas cabeças das definições de função onde eles são
% introduzidos pela palavra-chave `when`, ou você pode usá-los em qualquer
% lugar na linguagem onde uma expressão é permitida.
max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

% Um sentinela é uma série de expressões sentinelas, separadas por
% vírgulas (`,`).
% O sentinela `GuardExpr1, GuardExpr2, ..., GuardExprN` é verdadeiro se todas
% expressões sentinelas `GuardExpr1, GuardExpr2, ...` forem verdadeiras.
is_cat(A) when is_atom(A), A =:= cat -> true;
is_cat(A) -> false.
is_dog(A) when is_atom(A), A =:= dog -> true;
is_dog(A) -> false.

% Uma `sequência sentinela` é um sentinela ou uma série de sentinelas separados
% por ponto e vírgula (`;`). A sequência sentinela `G1; G2; ...; Gn` é
% verdadeira se pelo menos um dos sentinelas `G1, G2, ...` for verdadeiro.
is_pet(A) when is_dog(A); is_cat(A) -> true;
is_pet(A) -> false.

% Registros provêem um método para associar um nome com um elemento particular
% em uma tupla.
% Definições de registro podem ser incluídas em arquivos fonte Erlang ou em
% arquivos com extensão `.hrl`, que então são incluídos em arquivos fonte Erlang.
-record(todo, {
  status = reminder,  % Default value
  who = joe,
  text
}).

% Nós temos que ler definições de registro no prompt antes que possamos definir
% um registro. Nós usamos a função de prompt `rr` (abreviação de read records)
% para fazer isso.
rr("records.hrl").  % [todo]

% Criando e atualizando registros:
X = #todo{}.
% #todo{status = reminder, who = joe, text = undefined}
X1 = #todo{status = urgent, text = "Fix errata in book"}.
% #todo{status = urgent, who = joe, text = "Fix errata in book"}
X2 = X1#todo{status = done}.
% #todo{status = done,who = joe,text = "Fix errata in book"}

% Expressões `case`.
% A função `filter` retorna uma lista de todos elementos `X` em uma lista `L`
% para qual `P(X)` é verdadeiro.
filter(P, [H|T]) ->
  case P(H) of
    true -> [H|filter(P, T)];
    false -> filter(P, T)
  end;
filter(P, []) -> [].
filter(fun(X) -> X rem 2 == 0 end, [1, 2, 3, 4]). % [2, 4]

% Expressões `if`.
max(X, Y) ->
  if
    X > Y -> X;
    X < Y -> Y;
    true -> nil;
  end.

% Aviso: pelo menos um dos sentinelas na expressão `if` deve retornar
% verdadeiro; Caso contrário, uma exceção será levantada.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. Exceções.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Exceções são levantadas pelo sistema quando erros internos são encontrados ou
% explicitamente em código pela chamada `throw(Exception)`, `exit(Exception)`
% ou `erlang:error(Exception)`.
generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> erlang:error(a).

% Erlang tem dois métodos para capturar uma exceção. Uma é encapsular a chamada
% para a função que levanta uma exceção dentro de uma expressão `try...catch`.
catcher(N) ->
  try generate_exception(N) of
    Val -> {N, normal, Val}
  catch
    throw:X -> {N, caught, thrown, X};
    exit:X -> {N, caught, exited, X};
    error:X -> {N, caught, error, X}
  end.

% O outro é encapsular a chamada em uma expressão `catch`. Quando você captura
% uma exceção, é convertida em uma tupla que descreve o erro.
catcher(N) -> catch generate_exception(N).

```

## Referências

* ["Learn You Some Erlang for great good!"](http://learnyousomeerlang.com/)
* ["Programming Erlang: Software for a Concurrent World" by Joe Armstrong](http://pragprog.com/book/jaerlang2/programming-erlang)
* [Erlang/OTP Reference Documentation](http://www.erlang.org/doc/)
* [Erlang - Programming Rules and Conventions](http://www.erlang.se/doc/programming_rules.shtml)

