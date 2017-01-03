---
language: c++
filename: learncpp.cpp
contributors:
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Matt Kline", "https://github.com/mrkline"]
translators:
    - ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
---

C++ é uma linguagem de programação de sistemas que,
[de acordo com seu inventor Bjarne Stroustrup](http://channel9.msdn.com/Events/Lang-NEXT/Lang-NEXT-2014/Keynote),
foi concebida para

- ser um "C melhor"
- suportar abstração de dados
- suportar programação orientada a objetos
- suportar programação genérica

Embora sua sintaxe pode ser mais difícil ou complexa do que as linguagens mais
recentes, C++ é amplamente utilizado porque compila para instruções nativas que
podem ser executadas diretamente pelo processador e oferece um controlo rígido sobre hardware (como C), enquanto oferece recursos de alto nível, como os
genéricos, exceções e classes. Esta combinação de velocidade e funcionalidade
faz C++ uma das linguagens de programação mais utilizadas.

```c++
//////////////////
// Comparação com C
//////////////////

// C ++ é quase um super conjunto de C e compartilha sua sintaxe básica para
// declarações de variáveis, tipos primitivos, e funções. No entanto, C++ varia
// em algumas das seguintes maneiras:

// A função main() em C++ deve retornar um int, embora void main() é aceita
// pela maioria dos compiladores (gcc, bumbum, etc.)
// Este valor serve como o status de saída do programa.
// Veja http://en.wikipedia.org/wiki/Exit_status para mais informações.

int main(int argc, char** argv)
{
    // Argumentos de linha de comando são passados em pelo argc e argv da mesma
    // forma que eles estão em C.
    // argc indica o número de argumentos,
    // e argv é um array de strings, feito C (char*) representado os argumentos
    // O primeiro argumento é o nome pelo qual o programa foi chamado.
    // argc e argv pode ser omitido se você não se importa com argumentos,
    // dando a assinatura da função de int main()

    // Uma saída de status de 0 indica sucesso.
    return 0;
}

// Em C++, caracteres literais são um byte.
sizeof('c') == 1

// Em C, caracteres literais são do mesmo tamanho que ints.
sizeof('c') == sizeof(10)

// C++ tem prototipagem estrita
void func(); // função que não aceita argumentos

// Em C
void func(); // função que pode aceitar qualquer número de argumentos

// Use nullptr em vez de NULL em C++
int* ip = nullptr;

// Cabeçalhos padrão C estão disponíveis em C++,
// mas são prefixados com "c" e não têm sufixo .h

#include <cstdio>

int main()
{
    printf("Hello, world!\n");
    return 0;
}

///////////////////////
// Sobrecarga de função
///////////////////////

// C++ suporta sobrecarga de função
// desde que cada função tenha parâmetros diferentes.

void print(char const* myString)
{
    printf("String %s\n", myString);
}

void print(int myInt)
{
    printf("My int is %d", myInt);
}

int main()
{
    print("Hello"); // Funciona para void print(const char*)
    print(15); // Funciona para void print(int)
}

/////////////////////////////
// Parâmetros padrão de função
/////////////////////////////

// Você pode fornecer argumentos padrões para uma função se eles não são
// fornecidos pelo chamador.

void doSomethingWithInts(int a = 1, int b = 4)
{
    // Faça alguma coisa com os ints aqui
}

int main()
{
    doSomethingWithInts();      // a = 1,  b = 4
    doSomethingWithInts(20);    // a = 20, b = 4
    doSomethingWithInts(20, 5); // a = 20, b = 5
}

// Argumentos padrões devem estar no final da lista de argumentos.

void invalidDeclaration(int a = 1, int b) // Erro!
{
}


/////////////
// Namespaces (nome de espaços)
/////////////

// Namespaces fornecem escopos distintos para variável, função e outras
// declarações. Namespaces podem estar aninhados.

namespace First {
    namespace Nested {
        void foo()
        {
            printf("This is First::Nested::foo\n");
        }
    } // Fim do namespace aninhado
} // Fim do namespace First

namespace Second {
    void foo()
    {
        printf("This is Second::foo\n")
    }
}

void foo()
{
    printf("This is global foo\n");
}

int main()
{
    // Assuma que tudo é do namespace "Second" a menos que especificado de
    // outra forma.
    using namespace Second;

    foo(); // imprime "This is Second::foo"
    First::Nested::foo(); // imprime "This is First::Nested::foo"
    ::foo(); // imprime "This is global foo"
}

///////////////
// Entrada/Saída
///////////////

// C ++ usa a entrada e saída de fluxos (streams)
// cin, cout, and cerr representa stdin, stdout, and stderr.
// << É o operador de inserção e >> é o operador de extração.

#include <iostream> // Inclusão para o I/O streams

using namespace std; // Streams estão no namespace std (biblioteca padrão)

int main()
{
   int myInt;

   // Imprime na saída padrão (ou terminal/tela)
   cout << "Enter your favorite number:\n";
   // Pega a entrada
   cin >> myInt;

   // cout também pode ser formatado
   cout << "Your favorite number is " << myInt << "\n";
   // imprime "Your favorite number is <myInt>"

    cerr << "Usado para mensagens de erro";
}

//////////
// Strings
//////////

// Strings em C++ são objetos e têm muitas funções de membro
#include <string>

using namespace std; // Strings também estão no namespace std (bib. padrão)

string myString = "Hello";
string myOtherString = " World";

// + é usado para concatenação.
cout << myString + myOtherString; // "Hello World"

cout << myString + " You"; // "Hello You"

// Em C++, strings são mutáveis e têm valores semânticos.
myString.append(" Dog");
cout << myString; // "Hello Dog"


/////////////
// Referência
/////////////

// Além de indicadores como os de C, C++ têm _referências_. Esses são tipos de
// ponteiro que não pode ser reatribuída uma vez definidos e não pode ser nulo.
// Eles também têm a mesma sintaxe que a própria variável: Não * é necessário
// para _dereferencing_ e & (endereço de) não é usado para atribuição.

using namespace std;

string foo = "I am foo";
string bar = "I am bar";


string& fooRef = foo; // Isso cria uma referência para foo.
fooRef += ". Hi!"; // Modifica foo através da referência
cout << fooRef; // Imprime "I am foo. Hi!"

// Não realocar "fooRef". Este é o mesmo que "foo = bar", e foo == "I am bar"
// depois desta linha.

fooRef = bar;

const string& barRef = bar; // Cria uma referência const para bar.
// Como C, valores const (e ponteiros e referências) não podem ser modificado.
barRef += ". Hi!"; // Erro, referência const não pode ser modificada.

//////////////////////////////////////////
// Classes e programação orientada a objeto
//////////////////////////////////////////

// Primeiro exemplo de classes
#include <iostream>

// Declara a classe.
// As classes são geralmente declarado no cabeçalho arquivos (.h ou .hpp).
class Dog {
    // Variáveis de membro e funções são privadas por padrão.
    std::string name;
    int weight;

// Todos os membros a seguir este são públicos até que "private:" ou
// "protected:" é encontrado.
public:

    // Construtor padrão
    Dog();

    // Declarações de função Membro (implementações a seguir)
    // Note que usamos std :: string aqui em vez de colocar
    // using namespace std;
    // acima.
    // Nunca coloque uma declaração "using namespace" em um cabeçalho.
    void setName(const std::string& dogsName);

    void setWeight(int dogsWeight);

    // Funções que não modificam o estado do objecto devem ser marcadas como
    // const. Isso permite que você chamá-los se for dada uma referência const
    // para o objeto. Além disso, observe as funções devem ser explicitamente
    // declarados como _virtual_, a fim de ser substituídas em classes
    // derivadas. As funções não são virtuais por padrão por razões de
    // performance.

    virtual void print() const;

    // As funções também podem ser definidas no interior do corpo da classe.
    // Funções definidas como tal são automaticamente embutidas.
    void bark() const { std::cout << name << " barks!\n" }

    // Junto com os construtores, C++ fornece destruidores.
    // Estes são chamados quando um objeto é excluído ou fica fora do escopo.
    // Isto permite paradigmas poderosos, como RAII
    // (veja abaixo)
    // Destruidores devem ser virtual para permitir que as classes de ser
    // derivada desta.
    virtual ~Dog();

}; // Um ponto e vírgula deve seguir a definição de classe.

// Funções membro da classe geralmente são implementados em arquivos .cpp.
void Dog::Dog()
{
    std::cout << "A dog has been constructed\n";
}

// Objetos (como strings) devem ser passados por referência
// se você pretende modificá-los, ou com const caso contrário.
void Dog::setName(const std::string& dogsName)
{
    name = dogsName;
}

void Dog::setWeight(int dogsWeight)
{
    weight = dogsWeight;
}

// Observe que "virtual" só é necessária na declaração, não a definição.
void Dog::print() const
{
    std::cout << "Dog is " << name << " and weighs " << weight << "kg\n";
}

void Dog::~Dog()
{
    std::cout << "Goodbye " << name << "\n";
}

int main() {
    Dog myDog; // imprime "A dog has been constructed"
    myDog.setName("Barkley");
    myDog.setWeight(10);
    myDog.printDog(); // imprime "Dog is Barkley and weighs 10 kg"
    return 0;
} // imprime "Goodbye Barkley"

// herança:

// Essa classe herda tudo público e protegido da classe Dog
class OwnedDog : public Dog {

    void setOwner(const std::string& dogsOwner)

    // Substituir o comportamento da função de impressão de todas OwnedDogs.
    // Ver http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Subtyping
    // Para uma introdução mais geral, se você não estiver familiarizado com o
    // polimorfismo subtipo. A palavra-chave override é opcional, mas torna-se
    // na verdade você está substituindo o método em uma classe base.
    void print() const override;

private:
    std::string owner;
};

// Enquanto isso, no arquivo .cpp correspondente:

void OwnedDog::setOwner(const std::string& dogsOwner)
{
    owner = dogsOwner;
}

void OwnedDog::print() const
{
    Dog::print(); // Chame a função de impressão na classe Dog base de
    std::cout << "Dog is owned by " << owner << "\n";
    // Prints "Dog is <name> and weights <weight>"
    //        "Dog is owned by <owner>"
}

//////////////////////////////////////////
// Inicialização e Sobrecarga de Operadores
//////////////////////////////////////////

// Em C ++, você pode sobrecarregar o comportamento dos operadores, tais como
// +, -, *, /, etc. Isto é feito através da definição de uma função que é
// chamado sempre que o operador é usado.

#include <iostream>
using namespace std;

class Point {
public:
    // Variáveis membro pode ser dado valores padrão desta maneira.
    double x = 0;
    double y = 0;

    // Define um construtor padrão que não faz nada
    // mas inicializar o Point para o valor padrão (0, 0)
    Point() { };

    // A sintaxe a seguir é conhecido como uma lista de inicialização
    // e é a maneira correta de inicializar os valores de membro de classe
    Point (double a, double b) :
        x(a),
        y(b)
    { /* Não fazer nada, exceto inicializar os valores */ }

    // Sobrecarrega o operador +.
    Point operator+(const Point& rhs) const;

    // Sobrecarregar o operador +=.
    Point& operator+=(const Point& rhs);

    // Ele também faria sentido para adicionar os operadores - e -=,
    // mas vamos pular para sermos breves.
};

Point Point::operator+(const Point& rhs) const
{
    // Criar um novo ponto que é a soma de um e rhs.
    return Point(x + rhs.x, y + rhs.y);
}

Point& Point::operator+=(const Point& rhs)
{
    x += rhs.x;
    y += rhs.y;
    return *this;
}

int main () {
    Point up (0,1);
    Point right (1,0);
    // Isto chama que o operador ponto +
    // Ressalte-se a chamadas (função)+ com direito como seu parâmetro...
    Point result = up + right;
    // Imprime "Result is upright (1,1)"
    cout << "Result is upright (" << result.x << ',' << result.y << ")\n";
    return 0;
}

/////////////////////////
// Tratamento de Exceções
/////////////////////////

// A biblioteca padrão fornece alguns tipos de exceção
// (see http://en.cppreference.com/w/cpp/error/exception)
// mas qualquer tipo pode ser jogado como uma exceção
#include <exception>

// Todas as exceções lançadas dentro do bloco try pode ser capturado por
// manipuladores de captura subseqüentes
try {
    // Não aloca exceções no heap usando _new_.
    throw std::exception("A problem occurred");
}
// Capturar exceções por referência const se eles são objetos
catch (const std::exception& ex)
{
  std::cout << ex.what();
// Captura qualquer exceção não capturada pelos blocos _catch_ anteriores
} catch (...)
{
    std::cout << "Exceção desconhecida encontrada";
    throw; // Re-lança a exceção
}

///////
// RAII
///////

// RAII significa alocação de recursos é de inicialização.
// Muitas vezes, é considerado o paradigma mais poderoso em C++, e é o
// conceito simples que um construtor para um objeto adquire recursos daquele
// objeto e o destruidor liberá-los.

// Para entender como isso é útil,
// Considere uma função que usa um identificador de arquivo C:
void doSomethingWithAFile(const char* filename)
{
    // Para começar, assuma que nada pode falhar.

    FILE* fh = fopen(filename, "r"); // Abra o arquivo em modo de leitura.

    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

    fclose(fh); // Feche o arquivo.
}

// Infelizmente, as coisas são levemente complicadas para tratamento de erros.
// Suponha que fopen pode falhar, e que doSomethingWithTheFile e
// doSomethingElseWithIt retornam códigos de erro se eles falharem. (As
// exceções são a forma preferida de lidar com o fracasso, mas alguns
// programadores, especialmente aqueles com um conhecimento em C, discordam
// sobre a utilidade de exceções). Agora temos que verificar cada chamada para
// o fracasso e fechar o identificador de arquivo se ocorreu um problema.

bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Abra o arquivo em modo de leitura
    if (fh == nullptr) // O ponteiro retornado é nulo em caso de falha.
        return false; // Relate o fracasso para o chamador.

    // Suponha cada função retorne false, se falhar
    if (!doSomethingWithTheFile(fh)) {
        fclose(fh); // Feche o identificador de arquivo para que ele não vaze.
        return false; // Propague o erro.
    }
    if (!doSomethingElseWithIt(fh)) {
        fclose(fh); // Feche o identificador de arquivo para que ele não vaze.
        return false; // Propague o erro.
    }

    fclose(fh); // Feche o identificador de arquivo para que ele não vaze.
    return true; // Indica sucesso
}

// Programadores C frequentemente limpam isso um pouco usando Goto:
bool doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r");
    if (fh == nullptr)
        return false;

    if (!doSomethingWithTheFile(fh))
        goto failure;

    if (!doSomethingElseWithIt(fh))
        goto failure;

    fclose(fh); // Close the file
    return true; // Indica sucesso

failure:
    fclose(fh);
    return false; // Propague o erro.
}

// Se as funções indicam erros usando exceções,
// as coisas são um pouco mais limpo, mas ainda abaixo do ideal.
void doSomethingWithAFile(const char* filename)
{
    FILE* fh = fopen(filename, "r"); // Abra o arquivo em modo de leitura.
    if (fh == nullptr)
        throw std::exception("Não pode abrir o arquivo.");

    try {
        doSomethingWithTheFile(fh);
        doSomethingElseWithIt(fh);
    }
    catch (...) {
        fclose(fh); // Certifique-se de fechar o arquivo se ocorrer um erro.
        throw; // Em seguida, re-lance a exceção.
    }

    fclose(fh); // Feche o arquivo
    // Tudo ocorreu com sucesso!
}

// Compare isso com o uso de C++ classe fluxo de arquivo (fstream) fstream usa
// seu destruidor para fechar o arquivo. Lembre-se de cima que destruidores são
// automaticamente chamado sempre que um objeto cai fora do âmbito.
void doSomethingWithAFile(const std::string& filename)
{
    // ifstream é curto para o fluxo de arquivo de entrada
    std::ifstream fh(filename); // Abra o arquivo

    // faça alguma coisa com o arquivo
    doSomethingWithTheFile(fh);
    doSomethingElseWithIt(fh);

} // O arquivo é automaticamente fechado aqui pelo destructor

// Isto tem _grandes_ vantagens:
// 1. Não importa o que aconteça,
//    o recurso (neste caso, o identificador de ficheiro) irá ser limpo.
//    Depois de escrever o destruidor corretamente,
//    É _impossível_ esquecer de fechar e vazar o recurso
// 2. Nota-se que o código é muito mais limpo.
//    As alças destructor fecham o arquivo por trás das cenas
//    sem que você precise se preocupar com isso.
// 3. O código é seguro de exceção.
//    Uma exceção pode ser jogado em qualquer lugar na função e a limpeza
//    irá ainda ocorrer.

// Todos códigos C++ usam RAII extensivamente para todos os recursos.
// Outros exemplos incluem
// - Memória usa unique_ptr e shared_ptr
// - Contentores - a lista da biblioteca ligada padrão,
//   vetor (i.e. array de autodimensionamento), mapas hash, e assim por diante
//   tudo é automaticamente destruído quando eles saem de escopo
// - Mutex usa lock_guard e unique_lock


/////////////////////
// Templates
/////////////////////

// Templates em C++ são utilizados para programação genérica, ou seja,
// utilizar um tipo de dado genérico onde possa suportar qualquer entrada.
// Por exemplo, invés de criar uma função que apenas some inteiros, você
// poderá fazer uma função que soma double, float e inteiros em uma única
// definição para reutilizar código.

// Definimos um função que utiliza um "typename"
template<class T>
T soma(T a, T b) {
  return A + B;
}

// E agora para executá-la
int i=5, j=6, k;
double f=2.0, g=0.5, h;
k=sum<int>(i,j);
h=sum<double>(f,g);

// Deste modo, não precisamos fazer overload nas funções! (:
```
Leitura Adicional:

Uma referência atualizada da linguagem pode ser encontrada em
<http://cppreference.com/w/cpp>

Uma fonte adicional pode ser encontrada em <http://cplusplus.com>
