---
name: Go
category: language
language: Go
filename: learngo-pt.go
lang: pt-br
contributors:
    - ["Sonia Keys", "https://github.com/soniakeys"]
    - ["Jose Donizetti", "https://github.com/josedonizetti"]
translators:
    - ["Nuno Antunes", "https://github.com/ntns"]
---

A linguagem Go foi criada a partir da necessidade de ver trabalho feito. Não 
é a última moda em ciências da computação, mas é a mais recente e mais rápida
forma de resolver os problemas do mundo real.

Tem conceitos familiares de linguagens imperativas com tipagem estática. É 
rápida para compilar e rápida para executar, acrescentando mecanismos de concorrência
fáceis de entender para tirar partido dos CPUs multi-core de hoje em dia, e tem
recursos para ajudar com a programação em larga escala.

Go vem com uma biblioteca padrão exaustiva e uma comunidade entusiasta.

```go
// Comentário de uma linha
/* Comentário de
   várias linhas */

// A cláusula package aparece no início de cada arquivo.
// Main é um nome especial declarando um executável ao invés de uma biblioteca.
package main

// A cláusula Import declara os pacotes referenciados neste arquivo.
import (
    "fmt"      // Um pacote da biblioteca padrão da linguagem Go
    "net/http" // Sim, um servidor web!
    "strconv"  // Conversão de Strings
)

// Definição de uma função. Main é especial. É o ponto de entrada para o
// programa executável. Goste-se ou não, a linguagem Go usa chaves.
func main() {
    // A função Println envia uma linha para stdout.
    // É necessário qualificá-la com o nome do pacote, fmt.
    fmt.Println("Olá Mundo!")

    // Chama outra função dentro deste pacote.
    beyondHello()
}

// As funções declaram os seus parâmetros dentro de parênteses. Se a função
// não receber quaisquer parâmetros, é obrigatório usar parênteses vazios.
func beyondHello() {
    var x int // Declaração de variável. Tem de ser declarada antes de usar.
    x = 3     // Atribuição de variável.
    // Declarações "curtas" usam := para inferir o tipo, declarar e atribuir.
    y := 4
    sum, prod := learnMultiple(x, y)        // a função retorna dois valores
    fmt.Println("soma:", sum, "produto:", prod) 
    learnTypes()                            // continuar a aprender!
}

// As funções podem receber parâmetros e retornar (vários!) valores.
func learnMultiple(x, y int) (sum, prod int) {
    return x + y, x * y // retorna dois valores
}

// Alguns tipos e literais básicos.
func learnTypes() {
    // Declarações "curtas" geralmente servem para o que pretendemos.
    s := "Aprender Go!" // tipo string 

    s2 := `Uma string em "bruto" 
pode incluir quebras de linha.` // mesmo tipo string

    // literal não-ASCII. A linguagem Go utiliza de raiz a codificação UTF-8.
    g := 'Σ' // tipo rune, um alias para int32, que contém um código unicode

    f := 3.14195 // float64, número de vírgula flutuante de 64bit (IEEE-754)
    c := 3 + 4i  // complex128, representado internamente com dois float64s

    // Declaração de variáveis, com inicialização.
    var u uint = 7 // inteiro sem sinal, tamanho depende da implementação do Go
    var pi float32 = 22. / 7

    // Sintaxe de conversão de tipo, com declaração "curta".
    n := byte('\n') // byte é um alias para uint8

    // Os arrays têm tamanho fixo e definido antes da compilação. 
    var a4 [4]int           // um array de 4 ints, inicializado com ZEROS
    a3 := [...]int{3, 1, 5} // um array de 3 ints, inicializado como mostrado

    // As slices têm tamanho dinâmico. Os arrays e as slices têm cada um as
    // suas vantagens mas o uso de slices é muito mais comum.
    s3 := []int{4, 5, 9}   // compare com a3. sem reticências aqui
    s4 := make([]int, 4)   // aloca uma slice de 4 ints, inicializada com ZEROS
    var d2 [][]float64     // declaração apenas, nada é alocado
    bs := []byte("uma slice") // sintaxe de conversão de tipos

    p, q := learnMemory() // learnMemory retorna dois apontadores para int.
    fmt.Println(*p, *q)   // * segue um apontador. isto imprime dois ints.

    // Os maps são um tipo de matriz associativa, semelhante aos tipos hash
    // ou dictionary que encontramos noutras linguagens.
    m := map[string]int{"três": 3, "quatro": 4}
    m["um"] = 1

    // As variáveis não usadas são um erro em Go.
    // O traço inferior permite "usar" uma variável, mas descarta o seu valor.
    _, _, _, _, _, _, _, _, _ = s2, g, f, u, pi, n, a3, s4, bs
    // Enviar para o stdout conta como utilização de uma variável.
    fmt.Println(s, c, a4, s3, d2, m)

    learnFlowControl() 
}

// A linguagem Go é totalmente garbage collected. Tem apontadores mas não 
// permite que os apontadores sejam manipulados com aritmética. Pode-se cometer
// um erro com um apontador nulo, mas não por incrementar um apontador.
func learnMemory() (p, q *int) {
    // A função retorna os valores p e q, que são do tipo apontador para int.
    p = new(int) // a função new aloca memória, neste caso para um int.
    // O int alocado é inicializado com o valor 0, p deixa de ser nil.
    s := make([]int, 20) // alocar 20 ints como um único bloco de memória
    s[3] = 7             // atribui o valor 7 a um deles
    r := -2              // declarar outra variável local
    return &s[3], &r     // & obtém o endereço de uma variável.
}

func expensiveComputation() int {
    return 1e6
}

func learnFlowControl() {
    // As instruções if exigem o uso de chavetas, e não requerem parênteses.
    if true {
        fmt.Println("eu avisei-te")
    }
    // A formatação do código-fonte é "estandardizada" através do comando
    // da linha de comandos "go fmt."
    if false {
        // reclamar
    } else {
        // exultar
    }
    // Preferir o uso de switch em vez de ifs em cadeia.
    x := 1
    switch x {
    case 0:
    case 1:
        // os cases não fazem "fall through"
    case 2:
        // esta linha só é executada se e só se x=2
    }
    // Tal como a instrução if, a instrução for não usa parênteses.
    for x := 0; x < 3; x++ { // x++ é uma instrução, nunca uma expressão
        fmt.Println("iteração", x)
    }
    // note que, x == 1 aqui.

    // A instrução for é a única para ciclos, mas assume várias formas.
    for { // ciclo infinito
        break    // brincadeirinha
        continue // nunca executado
    }
    // O uso de := numa instrução if permite criar uma variável local,
    // que existirá apenas dentro do bloco if.
    if y := expensiveComputation(); y > x {
        x = y
    }
    // As funções podem ser closures.
    xBig := func() bool {
        return x > 100 // referencia x, declarado acima da instrução switch.
    }
    fmt.Println("xBig:", xBig()) // true (1e6 é o último valor de x)
    x /= 1e5                     // agora temos x == 10 
    fmt.Println("xBig:", xBig()) // false

    // Quando for mesmo necessário, pode usar o velho goto.
    goto love
love:

    learnInterfaces() // Mais coisas interessantes chegando!
}

// Define Stringer como uma interface consistindo de um método, String.
type Stringer interface {
    String() string
}

// Define pair como uma struct com dois campos ints chamados x e y.
type pair struct {
    x, y int
}

// Define um método para o tipo pair. O tipo pair implementa agora a 
// interface Stringer.
func (p pair) String() string { // p é chamado de "receptor"
    // Sprintf é outra função pública no pacote fmt.
    // Uso de pontos para referenciar os campos de p.
    return fmt.Sprintf("(%d, %d)", p.x, p.y)
}

func learnInterfaces() {
    // Uma struct pode ser inicializada com os valores dos seus campos dentro
    // de chavetas, seguindo a mesma ordem com que os campos foram definidos.
    p := pair{3, 4}
    fmt.Println(p.String()) // chama o método String de p, que tem tipo pair.
    var i Stringer          // declara i do tipo interface Stringer.
    i = p                   // válido, porque pair implementa Stringer
    // Chama o método String de i, que tem tipo Stringer. Mesmo que acima.
    fmt.Println(i.String())

    // As funções no pacote fmt chamam o método String para pedir a um objecto
    // uma representação textual de si mesmo.
    fmt.Println(p) // mesmo que acima. Println chama o método String.
    fmt.Println(i) // mesmo que acima.

    learnErrorHandling()
}

func learnErrorHandling() {
    // ", ok" forma idiomática usada para saber se algo funcionou ou não.
    m := map[int]string{3: "três", 4: "quatro"}
    if x, ok := m[1]; !ok { // ok vai ser false porque 1 não está no map m.
        fmt.Println("ninguem lá")
    } else {
        fmt.Print(x) // x seria o valor, se 1 estivesse no map.
    }
    // Um valor de erro comunica mais informação sobre o problema.
    if _, err := strconv.Atoi("non-int"); err != nil { // _ descarta o valor
        // imprime "strconv.ParseInt: parsing "non-int": invalid syntax"
        fmt.Println(err)
    }
    // Vamos revisitar as interfaces um pouco mais tarde. Entretanto,
    learnConcurrency()
}

// c é um channel, um objecto para comunicação concurrency-safe.
func inc(i int, c chan int) {
    c <- i + 1 // <- é operador "enviar" quando um channel aparece à esquerda.
}

// Vamos usar a função inc para incrementar números de forma concorrente.
func learnConcurrency() {
    // A mesma função make usada anteriormente para alocar uma slice.
    // Make aloca e inicializa slices, maps, e channels.
    c := make(chan int)
    // Inicia três goroutines concorrentes. Os números serão incrementados de
    // forma concorrente, talvez em paralelo se a máquina for capaz e estiver
    // configurada correctamente. As três goroutines enviam para o mesmo canal.
    go inc(0, c) // go é a instrução para iniciar uma goroutine.
    go inc(10, c)
    go inc(-805, c)
    // Lê três resultados do channel c e imprime os seus valores.
    // Não se pode dizer em que ordem os resultados vão chegar!
    fmt.Println(<-c, <-c, <-c) // channel na direita, <- é operador "receptor".

    cs := make(chan string)       // outro channel, este lida com strings.
    cc := make(chan chan string)  // channel que lida com channels de strings.
    go func() { c <- 84 }()       // inicia uma goroutine para enviar um valor
    go func() { cs <- "palavroso" }() // outra vez, para o channel cs desta vez
    // A instrução select tem uma sintaxe semelhante à instrução switch mas
    // cada caso envolve uma operação com channels. Esta instrução seleciona, 
    // de forma aleatória, um caso que esteja pronto para comunicar.
    select {
    case i := <-c: // o valor recebido pode ser atribuído a uma variável
        fmt.Printf("é um %T", i)
    case <-cs: // ou o valor recebido pode ser descartado
        fmt.Println("é uma string")
    case <-cc: // channel vazio, não se encontra pronto para comunicar.
        fmt.Println("não aconteceu")
    }
    // Neste ponto um valor foi recebido de um dos channels c ou cs. Uma das
    // duas goroutines iniciadas acima completou, a outra continua bloqueada.

    learnWebProgramming() // Go faz. Você quer faze-lo também.
}

// Basta apenas uma função do pacote http para iniciar um servidor web.
func learnWebProgramming() {
    // O primeiro parâmetro de ListenAndServe é o endereço TCP onde escutar.
    // O segundo parâmetro é uma interface, especificamente http.Handler.
    err := http.ListenAndServe(":8080", pair{})
    fmt.Println(err) // não ignorar erros
}

// Tornar pair um http.Handler ao implementar o seu único método, ServeHTTP.
func (p pair) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    // Servir dados com um método de http.ResponseWriter
    w.Write([]byte("Aprendeu Go em Y minutos!"))
}
```

## Leitura Recomendada

A principal fonte de informação é o [web site oficial Go](http://golang.org/).
Lá é possível seguir o tutorial, experimentar de forma iterativa, e ler muito.

A própria especificação da linguagem é altamente recomendada. É fácil de ler e
incrivelmente curta (em relação ao que é habitual hoje em dia).

Na lista de leitura para os aprendizes de Go deve constar o [código fonte da 
biblioteca padrão](http://golang.org/src/pkg/). Exaustivamente documentado, é
a melhor demonstração de código fácil de ler e de perceber, do estilo Go, e da
sua escrita idiomática. Ou então clique no nome de uma função na [documentação]
(http://golang.org/pkg/) e veja o código fonte aparecer!

Outra ótima fonte para aprender Go é o [Go by example](https://gobyexample.com/).
Apesar de ser em inglês, é possível recodificar os exemplos para aprender sobre
a linguagem.
