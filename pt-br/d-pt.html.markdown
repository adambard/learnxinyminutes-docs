---
language: D
filename: learnd-pt.d
contributors:
    - ["Nick Papanastasiou", "www.nickpapanastasiou.github.io"]
translators:
    - ["Julio Vanzelli", "https://github.com/JulioVanzelli"]
lang: pt-br
---

```d
// Você sabe o que está por vir...
module hello;

import std.stdio;

// args é opcional
void main(string[] args) {
    writeln("Hello, World!");
}
```

Se você é como eu e passa muito tempo na Internet, é provável que tenha ouvido
sobre [D](http://dlang.org/). A linguagem de programação D é moderna, de uso geral,
linguagem multiparadigma com suporte para tudo, desde recursos de baixo nível até
abstrações expressivas de alto nível.

D é desenvolvido ativamente por um grande grupo de pessoas super-inteligentes e é liderado por
[Walter Bright](https://en.wikipedia.org/wiki/Walter_Bright) e
[Andrei Alexandrescu](https://en.wikipedia.org/wiki/Andrei_Alexandrescu).
Com tudo isso fora do caminho, vamos dar uma olhada em alguns exemplos!

```d
import std.stdio;

void main() {

    // Condicionais e loops funcionam como esperado.
    for(int i = 0; i < 10000; i++) {
        writeln(i);
    }

    // 'auto' pode ser usado para inferir tipos.
    auto n = 1;

    // literais numéricos podem usar '_' como um separador de dígitos para maior clareza.
    while(n < 10_000) {
        n += n;
    }

    do {
        n -= (n / 2);
    } while(n > 0);

    // Por e enquanto são bons, mas em D-land preferimos loops 'foreach'.
    // O '..' cria um intervalo contínuo, incluindo o primeiro valor
    // mas excluindo o último.
    foreach(n; 1..1_000_000) {
        if(n % 2 == 0)
            writeln(n);
    }

    // Há também 'foreach_reverse' quando você deseja fazer um loop para trás.
    foreach_reverse(n; 1..int.max) {
        if(n % 2 == 1) {
            writeln(n);
        } else {
            writeln("No!");
        }
    }
}
```

Podemos definir novos tipos com `struct`, `class`, `union` e `enum`. Estruturas e uniões
são passados para funções por valor(ou seja, copiados) e as classes são passadas por referência. Além disso,
podemos usar modelos para parametrizar tudo isso em tipos e valores!

```d
// Aqui, 'T' é um parâmetro de tipo. Pense '<T>' em C++/C#/Java.
struct LinkedList(T) {
    T data = null;

    // Usar '!' para instanciar um tipo parametrizado. Mais uma vez, pense '<T>'.
    LinkedList!(T)* next;
}

class BinTree(T) {
    T data = null;

    // Se houver apenas um parâmetro de modelo, podemos omitir os parênteses.
    BinTree!T left;
    BinTree!T right;
}

enum Day {
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
}

// Use o alias para criar abreviações para tipos.
alias IntList = LinkedList!int;
alias NumTree = BinTree!double;

// Também podemos criar modelos de funções!
T max(T)(T a, T b) {
    if(a < b)
        return b;

    return a;
}

// Use a palavra-chave ref para garantir a passagem por referência. Ou seja, mesmo que 'a'
// e 'b' sejam tipos de valor, eles sempre serão passados por referência a 'swap ()'.
void swap(T)(ref T a, ref T b) {
    auto temp = a;

    a = b;
    b = temp;
}

// Com os modelos, também podemos parametrizar valores, não apenas tipos.
class Matrix(uint m, uint n, T = int) {
    T[m] rows;
    T[n] columns;
}

auto mat = new Matrix!(3, 3); // O tipo 'T' foi padronizado como 'int'.
```

Falando em aulas, vamos falar sobre propriedades por um segundo. Uma propriedade
é aproximadamente uma função que pode agir como um valor I, para que possamos
ter a sintaxe das estruturas POD (`structure.x = 7`) com a semântica de
métodos getter e setter (`object.setX (7)`)!

```d
// Considere uma classe parametrizada nos tipos 'T' e 'U'.
class MyClass(T, U) {
    T _data;
    U _other;
}

// E os métodos "getter" e "setter", assim:
class MyClass(T, U) {
    T _data;
    U _other;

    // Os construtores sempre são chamados de 'this'.
    this(T t, U u) {
        // This will call the setter methods below.
        data = t;
        other = u;
    }

    // getters
    @property T data() {
        return _data;
    }

    @property U other() {
        return _other;
    }

    // setters
    @property void data(T t) {
        _data = t;
    }

    @property void other(U u) {
        _other = u;
    }
}

// E nós os usamos desta maneira:
void main() {
    auto mc = new MyClass!(int, string)(7, "seven");

    // Importe o módulo 'stdio' da biblioteca padrão para gravar no
    // console (as importações podem ser locais para um escopo).
    import std.stdio;

    // Ligue para os getters para buscar os valores.
    writefln("Earlier: data = %d, str = %s", mc.data, mc.other);

    // Ligue para os setters para atribuir novos valores.
    mc.data = 8;
    mc.other = "eight";

    // Ligue para os getters novamente para buscar os novos valores.
    writefln("Later: data = %d, str = %s", mc.data, mc.other);
}
```

Com propriedades, podemos adicionar qualquer quantidade de lógica para
nossos métodos getter e setter, e mantenha a sintaxe limpa de
acessando membros diretamente!

Outras guloseimas orientadas a objetos à nossa disposição,
incluem interfaces, classes abstratas,
e métodos de substituição. D faz herança como Java:
Estenda uma classe, implemente quantas interfaces você desejar.

Vimos as instalações OOP de D, mas vamos mudar de marcha. D oferece
programação funcional com funções de primeira classe, `pura`
funções e dados imutáveis. Além disso, todos os seus favoritos
algoritmos funcionais (mapear, filtrar, reduzir e amigos) podem ser
encontrado no maravilhoso módulo `std.algorithm`!

```d
import std.algorithm : map, filter, reduce;
import std.range : iota; // cria uma gama exclusiva de final

void main() {
    // Queremos imprimir a soma de uma lista de quadrados de ints pares
    // de 1 a 100. Fácil!

    // Basta passar expressões lambda como parâmetros de modelo!
    // Você pode passar qualquer função que desejar, mas as lambdas são convenientes aqui.
    auto num = iota(1, 101).filter!(x => x % 2 == 0)
                           .map!(y => y ^^ 2)
                           .reduce!((a, b) => a + b);

    writeln(num);
}
```

Observe como conseguimos construir um bom pipeline haskelliano para calcular num?
Isso se deve a uma inovação em D, conhecida como Uniform Function Call Syntax (UFCS).
Com o UFCS, podemos optar por escrever uma chamada de função como método
ou chamada de função grátis! Walter escreveu um bom artigo sobre isso
[aqui](http://www.drdobbs.com/cpp/uniform-function-call-syntax/232700394).
Em resumo, você pode chamar funções cujo primeiro parâmetro
é de algum tipo A em qualquer expressão do tipo A como método.

Eu gosto de paralelismo. Alguém mais gosta de paralelismo? Com certeza. Vamos fazer um pouco!

```d
// Digamos que queremos preencher uma matriz grande com a raiz quadrada de todos
// os números inteiros consecutivos começando de 1 (até o tamanho da matriz), e queremos
// fazer isso simultaneamente, aproveitando o número de núcleos que temos
// disponível.

import std.stdio;
import std.parallelism : parallel;
import std.math : sqrt;

void main() {
    // Crie sua grande variedade
    auto arr = new double[1_000_000];

    // Use um índice, acesse todos os elementos da matriz por referência (porque vamos
    // mudar cada elemento) e apenas chame paralelo na matriz!
    foreach(i, ref elem; parallel(arr)) {
        elem = sqrt(i + 1.0);
    }
}
```
