---
nome: Gleam
contributors:
    - ["Antonio Ognio", "https://github.com/aognio/"]
filename: learngleam.gleam
---

Gleam é uma nova linguagem para a máquina virtual BEAM de Erlang que depende do
poder de um sistema de tipos robusto, a expressividade da programação funcional,
e o tempo de execução Erlang altamente simultâneo e tolerante a falhas usando
sintaxe moderna inspirada em linguagens como OCaml, Rust e Elixir.

Sendo um desenvolvimento bastante moderno, o Gleam vem com um compilador, uma ferramenta de construção,
um formatador de código, várias integrações de editores e um gerenciador de pacotes.

Fazendo parte do ecossistema BEAM mais amplo, os programas criados com Gleam
também podem aproveitar milhares de pacotes publicados escritos em Erlang ou Elixir.

O design da linguagem é muito conciso, por isso não apresenta valores nulos,
sem exceções, mensagens de erro claras e um sistema de tipos prático.

JavaScript também é suportado como alvo de compilação, então você pode executar o Gleam
código no navegador ou qualquer outro tempo de execução habilitado para JS. Ao usar esse recurso,
As definições TypeScript são criadas para que você possa interagir com seu código Gleam
com confiança, mesmo de fora.

Para executar este código, primeiro crie um projeto Gleam:

```sh
gleam new my_project
cd my_project
```

Substitua o código gerado por este exemplo e execute:

```sh
gleam run
```


```gleam
//// Este comentário com quatro barras é de nível de módulo.
//// Este tipo de comentários são usados para descrever todo o módulo.

import gleam/bool
import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/dict

// O nome de um tipo sempre começa com letra maiúscula, contrastando com variáveis
// e funções, que começam com letra minúscula.

// Quando a palavra-chave pub é usada, o alias do tipo é público e pode ser consultado
// por outros módulos.

pub type IdUsuario =
  Int

pub fn main() {
  io.println("Ola de learnxinyminutes.com!")
  // io.println("Esta declaração foi comentada por um comentário de duas barras.!")

  // Módulos são as unidades nas quais todo o código Gleam é organizado.
  //Em um módulo você encontrará várias definições de tipos, funções, etc.
  // que parecem pertencer um ao outro.
  // Por exemplo, o módulo gleam/io contém uma variedade de funções para
  // impressão, como println.

  // Todo código Gleam está em algum módulo ou outro, cujo nome vem do nome
  // do arquivo em que está.
  // Por exemplo, gleam/io está em um arquivo chamado io.gleam em um diretório
  // chamado gleam.

  // Gleam possui um sistema robusto de tipos estáticos que ajuda você enquanto você escreve e edita
  // código, detectando erros e mostrando onde fazer alterações.
  // io.println(10)
  // Se você descomentar a linha anterior, receberá um erro de tempo de compilação relatado
  // já que a função io.println só funciona com strings, não com ints.

  // A compilação irá gerar um erro parecido com este:
  // error: Type mismatch
  //  ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:21:14
  //  │
  // 21 │   io.println(10)
  //  │              ^^
  //
  // Expected type:
  //
  //     String
  //
  // Found type:
  //
  //     Int

  // Trabalhando com números

  // Ao executar na máquina virtual Erlang, os ints não têm máximo e mínimo
  // tamanho.
  // Ao executar em tempos de execução JavaScript, os ints são representados usando JavaScript's
  // Números de ponto flutuante de 64 bits.

  // Podemos usar `echo` para depurar a impressão de um valor.

  // Aritmetica de inteiros
  echo 1 + 1
  echo 5 - 1
  echo 5 / 2
  echo 3 * 3
  echo 5 % 2

  // Int comparisons using variables to prevent compiler static analysis warnings
  let dois = 2
    let um = 1
    let mesmo_um = 1
    let mesmo_dois = 2
  let _ = echo dois > um
  let _ = echo dois < um
  let _ = echo dois >= um
  let _ = echo dois <= um

  // A igualdade funciona para qualquer tipo e é verificada estruturalmente, o que significa que dois
  // os valores são iguais se tiverem a mesma estrutura e não se estiverem no mesmo nível.
  // o mesmo local de memória.
  let _ = echo um == mesmo_um
  // True
  let _ = echo dois != mesmo_dois
  // False

  //Funções int da biblioteca padrão
  echo int.min(142, 137)
  // 137
  echo int.clamp(-80, min: 0, max: 100) // clamp(valor, min, max) especifica o valor que deve cair em determinado intervalo
  // 0
  let _ = echo int.base_parse("10", 2) // analisa a string binária como um número inteiro de base 2. 
  // Ok(2) : 10 in binary equals to 2 in integer

  // Literais Int binários, octais e hexadecimais
  echo 0b00001111
  echo 0o17
  echo 0xF

  // Use sublinhados para melhorar a legibilidade de números inteiros
  echo 1_000_000

  // Os operadores numéricos do Gleam não estão sobrecarregados, portanto existem
  // operadores para trabalhar com carros alegóricos.

  // Aritmetica de floats
  echo 1.0 +. 1.5
  echo 5.0 -. 1.5
  echo 5.0 /. 2.5
  echo 3.0 *. 3.5

  // Float comparisons using variables to prevent compiler static analysis warnings
  let dois_ponto_dois = 2.2
  let um_ponto_tres = 1.3
  let _ = echo dois_ponto_dois >. um_ponto_tres
  let _ = echo dois_ponto_dois <. um_ponto_tres
  let _ = echo dois_ponto_dois >=. um_ponto_tres
  let _ = echo dois_ponto_dois <=. um_ponto_tres

  // Os floats são representados como números de ponto flutuante de 64 bits no Erlang
  // e tempos de execução JavaScript.
  // O comportamento do ponto flutuante é nativo de seus respectivos tempos de execução, portanto
  // seu comportamento exato será ligeiramente diferente nos dois tempos de execução.

  // No tempo de execução do JavaScript, excedendo o máximo (ou mínimo)
  // valor representável para um valor de ponto flutuante resultará em Infinito
  // (ou -Infinito). Se você tentar dividir dois infinitos, obterá NaN
  // como resultado.

  // Ao executar no BEAM, qualquer estouro gerará um erro. Então não há
  // Valor flutuante NaN ou Infinity no tempo de execução Erlang.

  // A divisão por zero não é um erro e é definida como zero
  echo 3.14 /. 0.0
  // 0.0

  // Funções flutuantes da biblioteca padrão
  echo float.max(2.0, 9.5)
  // 9.5
  echo float.ceiling(5.4)
  // 6.0

  // Sublinhados para carros flutuantes também são suportados
  echo 10_000.01

  // Trabalhando com strings
  echo "⭐ Gleam ⭐ - 별"
  echo "this
    is
    a
    multi
    line
    string"

  echo "\u{1F600}"
  // Produz um smiley 😀

  // Aspas duplas podem ser escapadas
  io.println("\"X\" marca o local")

  // Concatenacao de strings
  echo "One " <> "Two"

  // Funcoes de string
  echo string.reverse("1 2 3 4 5")
  echo "abc" <> "def"

  io.println(string.reverse("!desrever tog gnirts sihT"))
  // Saídas "Esta string foi invertida!"

  // Várias sequências de escape são suportadas:

  // \" - aspas duplas
  // \\ - barra invertida
  // \f - feed de formulário
  // \n - nova linha
  // \r - retorno de carro
  // \t - aba

  // Operadores booleanos
  // O || e os operadores && funcionam por curto-circuito

  echo True && False
  // False

  echo True && True
  // True

  echo False || False
  // False

  echo False || True
  // True

  // Funcoes booleanas
  echo bool.to_string(True)
  // "True"

  // Atribuições
  let x = "Original valor"
  echo x

  // Atribuir `y` ao valor de `x`
  let y = x
  echo y

  // Atribuir `x` a um novo valor
  let x = "New valor"
  echo x

  // O `y` ainda se refere ao valor original
  echo y

  // No Gleam, os nomes de variáveis e funções são escritos em snake_case.
    let resposta_do_universo = 42
  echo resposta_do_universo

    let e_todo_o_resto = resposta_do_universo
  // Agora, usar uma variável produz um aviso

  // warning: Unused variable
  //     ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:199:7
  //     │
  // 199 │     let e_todo_o_resto = resposta_do_universo
  //     │       ^^^^^^^^^^^^^^ This variable is never used
  // Hint: You can ignore it with an underscore: `_e_todo_o_resto`.

  // Digite anotações

    let _nome: String = "Gleam"

    let _e_legal: Bool = True

  let _version: Int = 1
  // Úteis para fins de documentação, mas não alteram a forma como o compilador
  // verifica o código além de garantir que a anotação corresponda ao tipo;
  // caso contrário, você receberá um erro.

  // let _has_wrong_type_annotation: Int = True

  //  error: Type mismatch
  //      ┌─ /home/contributor/learnxinmyminutes/src/learnxinmyminutes.gleam:219:41
  //      │
  //  219 │   let _has_wrong_type_annotation: Int = True
  //      │                                         ^^^^
  //
  //  Expected type:
  //
  //      Int
  //
  //  Found type:
  //
  //      Bool

  // Aliases de tipo
    let um: IdUsuario = 1
  // Consulte o início do arquivo para a definição do tipo UserId

    let dois: Int = 2

  // Aliases servem apenas para criar código mais legível e mais preciso
  //documentação.
  // Nos bastidores, eles ainda são valores do mesmo tipo, então as operações
  // ainda trabalho
  echo um + dois
  // 3

  // Blocos: escopo e valor
  let raio = {
    let valor = 100.0
    valor
  }
  // echo valor // <- Isto não será compilado porque "valor" está fora do escopo

  let area = 3.14159 *. raio *. raio
  echo area

  // Use blocos para agrupar operações em vez de parênteses
  let n1 = { 3 + 2 } * 5
  let n2 = 3 + { 2 * 5 }
  echo n1 != n2
  // True

  // Listas

  // Sobrinhos do Tio Patinhas
    let sobrinhos = ["Huguinho", "Zezinho", "Luizinho"]
  echo sobrinhos
  // ["Huguinho", "Zezinho", "Luizinho"]

  // Anexar imutavelmente para que a lista original não seja alterada
  echo ["Donald", ..sobrinhos]
  // ["Donald", "Huguinho", "Zezinho", "Luizinho"]

  // Algumas funções de biblioteca padrão para listas

  list.each(sobrinhos, io.println)
  // Huguinho
  // Zezinho
  // Luizinho

  echo list.drop(sobrinhos, 2)
  // ["Luizinho"]

  mais_exemplos()
  mais_exemplos_funcoes()
  exemplos_tipos_genericos()
  demo_pipelines()
  rotulos_em_chamadas()
  mostrar_controle_de_fluxo()
  mais_sobre_recursao()
  mais_sobre_correspondencia_de_padroes()
  mostrar_tipos()
  mais_sobre_tipos()
  mais_sobre_callbacks()
  mostrar_externos()
  mostrar_constantes()
  mostrar_dicts()
  mostrar_arrays_de_bits()
  mostrar_tipos_opacos()
  mostrar_panic()
}

// A palavra-chave fn é usada para definir novas funções.
fn multiplicar(a: Int, b: Int) -> Int {
  // Sem retorno explícito
  // A última expressão é retornada
  a * b
}

// As funções dupla e multiplicada são definidas sem a palavra-chave pub.
// Isso as torna funções privadas, elas só podem ser usadas dentro deste módulo.
// Se outro módulo tentasse usá-los, isso resultaria em um erro do compilador.
fn dobrar(a: Int) -> Int {
  multiplicar(a, 2)
}

// Somente funções públicas são exportadas e podem ser chamadas de fora do módulo.

// As anotações de tipo são opcionais para argumentos de função e valores de retorno
// mas são consideradas boas práticas para maior clareza e para encorajar
// design intencional e pensativo.

pub fn e_bissexto(ano: Int) -> Bool {
  { ano % 4 == 0 } && { { ano % 100 != 0 } || { ano % 400 == 0 } }
}

fn mais_exemplos() {
  //Debug também retorna um valor, então sua saída é o valor de retorno de
  // esta função
  echo dobrar(10)
  // 20
  echo e_bissexto(2000)
  // True
}

// Gleam oferece suporte a funções de ordem superior:
// Eles podem ser atribuídos a variáveis, passados como argumentos para outras funções
// ou até mesmo ser retornados como valores de blocos ou outras funções
fn chamar_funcao_com_inteiro(func: fn(Int) -> Int, valor: Int) -> Int {
  func(valor)
}

fn mais_exemplos_funcoes() -> Int {
  echo chamar_funcao_com_inteiro(dobrar, 2)
  // 4

  let quadrado = fn(x: Int) -> Int { x * x }
  echo quadrado(3)
  // 9

  // Chamar uma função anônima imediatamente após defini-la
  echo fn(x: Int) { x + 1 }(1)

  // Exemplo de fechamento
  let criar_somadora = fn(n: Int) -> fn(Int) -> Int {
    fn(argumento: Int) -> Int { argumento + n }
  }

  let somadora_de_cinco = criar_somadora(5)
  echo somadora_de_cinco(10)
  // 15

  // Funções anônimas podem ser usadas de forma intercambiável com funções nomeadas.
  echo chamar_funcao_com_inteiro(fn(x: Int) -> Int { x + 100 }, 900)
  // 1000

  // Vamos criar um decorador de função
  let duas_vezes = fn(funcao_envolvida: fn(Int) -> Int) -> fn(Int) -> Int {
    fn(argumento: Int) -> Int { funcao_envolvida(funcao_envolvida(argumento)) }
  }
    let quadruplo = duas_vezes(dobrar)
    echo quadruplo(1)

    let quadruplo_2 = fn(a: Int) -> Int { multiplicar(4, a) }
  echo quadruplo_2(2)
  // 8

  // Uma captura de função é uma sintaxe abreviada para criar funções anônimas
  // que recebe um argumento e imediatamente chama outra função com esse
  // argumento
    let quadruplo_3 = multiplicar(4, _)
  echo quadruplo_3(4)
  // 16
}

// Funções genéricas são suportadas usando variáveis de tipo.
fn dobro_generico(func: fn(valor) -> valor, argumento: valor) -> valor {
  func(func(argumento))
}

// Em dobro_generico o valor era a variável de tipo.
// Em decorador_dobro_generico the_type é a variável de tipo.
// Como em qualquer outra variável você escolhe o nome.
fn decorador_dobro_generico(
  func: fn(the_type) -> the_type,
) -> fn(the_type) -> the_type {
  fn(argumento: the_type) -> the_type { func(func(argumento)) }
}

fn exemplos_tipos_genericos() {
  let dobrar_inteiros = fn(a: Int) -> Int { a * 2 }
  let dobrar_floats = fn(a: Float) -> Float { a *. 2.0 }
  echo dobro_generico(dobrar_inteiros, 3)
  echo dobro_generico(dobrar_floats, 3.0)

  let quadruplo_inteiros = decorador_dobro_generico(dobrar_inteiros)
  let quadruplo_floats = decorador_dobro_generico(dobrar_floats)
  echo quadruplo_inteiros(1)
  // 4
  echo quadruplo_floats(1.0)
  // 4.0
}

// O operador pipe do Gleam |> pega o resultado da expressão à sua esquerda
// e passa-o como argumento para a função à sua direita.
fn demo_pipelines() {
  // Sejamos honestos: você quer usar o Gleam apenas para esse operador legal, certo?
  ["ola", "mundo"]
  |> list.intersperse(" ")
  |> list.append(["!"])
  |> string.concat
  |> string.capitalise
  |> echo

  // Muito mais limpo do que isso, certo?
  echo 
    string.capitalise(
      string.concat(
        list.append(list.intersperse(["ola", "mundo"], " "), ["!"]),
      ),
    )

  // Solução para o primeiro problema do Projeto Euler:
  // URL: https://projecteuler.net/problem=1
  // Descricao: encontre a soma de todos os multiplos de 3 e 5 abaixo de 1000.
  // Usando int.range de gleam/int para iterar de forma recursiva.
  int.range(from: 1, to: 999, with: 0, run: fn(acum, n) {
    case n % 3 == 0 || n % 5 == 0 {
      True -> acum + n
      False -> acum
    }
  })
  |> int.to_string
  |> fn(soma_como_texto: String) {
    "Solucao para o problema #1 do Project Euler: " <> soma_como_texto
  }
  |> echo
  // Solucao para o problema #1 do Project Euler: 233168
}

// Rótulos podem ser adicionados antes de cada argumento
fn chamar_funcao_com_inteiro_com_rotulos(
  func funcao_passada: fn(Int) -> Int,
  valor n: Int,
) -> Int {
  funcao_passada(n)
}

// O rótulo e o argumento podem ter o mesmo nome
fn somar_um(numero numero: Int) -> Int {
  numero + 1
}

fn somar_dois_inteiros(primeiro n: Int, segundo m: Int) -> Int {
  n + m
}

fn rotulos_em_chamadas() -> Int {
  //Como estamos rotulando os argumentos, podemos mudar a ordem
  // se quisermos
  echo chamar_funcao_com_inteiro_com_rotulos(valor: 8, func: dobrar)
  echo somar_um(numero: 1)
  // 2
  echo string.contains(does: "tema", contain: "te")
  // True
  // Argumentos não rotulados devem ir primeiro
  echo somar_dois_inteiros(2, segundo: 2)
  // 4
}

fn mostrar_controle_de_fluxo() {
  // Caso de uso se você quiser usar correspondência de padrões para
  // selecione qual código executar.
  // A Gleam garantirá que todos os valores possíveis sejam cobertos
  // realizando verificações de exaustividade.
  // Caso contrário, você receberá erros de compilação.
  let filhotes = ["Bear", "Frisco", "Ranger"]
  let contagem = list.length(of: filhotes)
  {
    "Temos "
    <> int.to_string(contagem)
    <> " "
    <> // O sublinhado corresponde a qualquer outro valor
    case contagem {
      1 -> "filhote"
      _ -> "filhotes"
    }
  }
  |> echo

  // O Gleam permite que padrões em expressões case também atribuam variáveis.
  {
    "Quantidade de filhotes: "
    <> case list.length(filhotes) {
      0 -> "Nenhum."
      1 -> "Apenas um."
      outra_contagem -> "Tantos quantos " <> int.to_string(outra_contagem) <> " filhotes."
    }
  }
  |> echo

  // Considere que as linguagens BEAM são funcionais em design e o Gleam não é exceção
  // portanto, não há construções if, for ou while disponíveis.

  // Use correspondência de padrões para condicionais
  let resposta = 42
  case resposta == 42 {
    True -> {
      echo "Esta e a resposta do universo."
    }
    False -> {
      echo "Esta e a resposta de outra coisa."
    }
  }

  // Use recursão em vez de loop
  de_um_a_dez(1)
}

// Função recursiva
fn de_um_a_dez(n: Int) {
  echo n
  case n {
    10 -> Nil
    _ -> de_um_a_dez(n + 1)
  }
}

// Para evitar o esgotamento da memória devido à criação excessiva
// empilhar frames ao chamar funções recursivamente, Gleam suporta
// "otimização da chamada final", o que significa que o compilador pode reutilizar
// o quadro de pilha para a função atual se uma chamada de função for
// a última coisa que a função faz.

pub fn fibonacci(x: Int) -> Int {
  // A função pública chama a função recursiva de cauda privada
  laco_fibonacci(x, 1)
}

fn laco_fibonacci(x: Int, acumulador: Int) -> Int {
  case x {
    1 -> acumulador

    // A última coisa que esta função faz é chamar a si mesma
    // Na lição anterior, a última coisa que fizemos foi multiplicar dois inteiros
    _ -> laco_fibonacci(x - 1, acumulador + x)
  }
}

// Gleam suporta correspondência de padrões do primeiro elemento e do restante
//de uma lista com o padrão [x, ..y] dentro de uma expressão case.
fn inverter_lista(a_lista: List(valor)) -> List(valor) {
  case a_lista {
    [cabeca, ..cauda] -> list.flatten([inverter_lista(cauda), [cabeca]])
    [] -> []
  }
}

fn mais_sobre_recursao() {
  echo fibonacci(10)
  // 55
  echo inverter_lista([1, 2, 3])
}

fn mais_sobre_correspondencia_de_padroes() {
  // Ao combinar padrões em strings, o operador <> corresponde a strings
  // com um prefixo específico e atribui o lembrete a uma variável
    let lucia = "Ola, Lucia"
  let _ = echo case lucia {
    "Ola, " <> nome -> "Saudacoes para " <> nome
    _ -> "Talvez nao haja saudacoes"
  }

  // Padrões alternativos são suportados para que a mesma cláusula seja usada
  // para vários valores
  let mes = 2
  let ano = 2024
  let numero_de_dias = case mes {
    2 ->
      case e_bissexto(ano) {
        False -> 28
        True -> 29
      }
    4 | 6 | 9 | 11 -> 30
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
    _ -> 0
  }
  echo "Numero de dias: " <> int.to_string(numero_de_dias)
  // 29

  // Protetores na correspondência de padrões:
  // Ao usar a palavra-chave if, uma expressão deve ser avaliada como True
  // para que o padrão corresponda.
  let lista_comeca_com = fn(a_lista: List(valor), o_valor: valor) -> Bool {
    case a_lista {
      [cabeca, ..] if cabeca == o_valor -> True
      _ -> False
    }
  }
  echo lista_comeca_com([10, 20, 30], 10)
  // True
}

pub type Genero {
  Masculino
  Feminino
  Outro
}

// Registros:
// - Variantes de suporte
// - Cada variante é semelhante a uma estrutura com campos
pub type Forma {
  Retangulo(base: Float, altura: Float)
  Triangulo(base: Float, altura: Float)
}

// Registros com uma variante se assemelham a estruturas
pub type Ponto {
  Ponto(x: Float, y: Float)
}

fn mostrar_tipos() {
  // Tuplas:
  // - Pode misturar elementos de diferentes tipos
  // - Seu tipo está implícito, por ex. #{1, "Olá"} é do tipo #{Int, String}
  // - Seus elementos podem ser acessados por índices numéricos
  let tupla_01 = #(1, "Ferris", "crustaceo", True)
  let tupla_02 = #(1, "Lucia", "estrela_do_mar", True)
  echo tupla_01
  echo tupla_01.0
  // 1
  echo tupla_02.1
  // Lúcia
  let #(_, nome, especie, _) = tupla_01
  echo nome <> " o " <> especie

  // Correspondência de padrões com tuplas, incluindo atribuição de variáveis
  let _ = imprimir_estado_mascote(tupla_02)

  // Usando um tipo personalizado com correspondência de padrões
    let genero = Outro
  let _ = echo genero_para_string(genero)

  // Usando registros
  let retangulo_1 = Retangulo(base: 10.0, altura: 20.0)
  echo retangulo_1.altura
  // 10.3

  let ponto_1 = Ponto(x: 3.2, y: 4.3)
  echo ponto_1

  // Atualizando um registro
  let ponto_2 = Ponto(..ponto_1, y: 5.7)
  echo ponto_2

  // No Gleam, os valores não são anuláveis.
  // Nil é o único valor desse tipo.
  let alguma_variavel = Nil
    let resultado = io.println("Ola!")
  echo alguma_variavel == resultado
  // True
}

fn imprimir_estado_mascote(mascote: #(Int, String, String, Bool)) {
  case mascote {
    #(_, nome, _, True) -> echo nome <> " e um mascote."
    #(_, nome, _, False) -> echo nome <> " nao e um mascote."
  }
}

fn genero_para_string(genero: Genero) -> String {
  case genero {
    Masculino -> "Masculino"
    Feminino -> "Feminino"
    Outro -> "Indeterminado"
  }
}

pub type Mineral {
  Ouro
  Prata
  Cobre
}

// Tipos personalizados genéricos com tipos contidos como parâmetros
pub type Pureza(tipo_interno) {
  Puro(tipo_interno)
  Impuro(tipo_interno)
}

pub type Bebida {
  Agua
  Suco
}

// Tipos personalizados existentes dos módulos gleam/option e gleam/result
// facilitar o trabalho com valores anuláveis e o tratamento de possíveis erros
pub type Pessoa {
  Pessoa(nome: String, apelido: Option(String))
}

pub type ErroDado {
  ValorDadoForaDoIntervalo
}

fn valor_dado_verificado(valor: Int) -> Result(Int, ErroDado) {
  case valor {
    1 | 2 | 3 | 4 | 5 | 6 -> Ok(valor)
    _ -> Error(ValorDadoForaDoIntervalo)
  }
}

fn dobrar_valor_dado(valor: Int) -> Result(Int, ErroDado) {
  case valor {
    1 | 2 | 3 -> Ok(valor * 2)
    _ -> Error(ValorDadoForaDoIntervalo)
  }
}

fn mais_sobre_tipos() {
    let amostra_mineral_01: Pureza(Mineral) = Puro(Ouro)
    let amostra_mineral_02 = Impuro(Prata)
  echo amostra_mineral_01
  echo amostra_mineral_02

  // Um copo pode estar vazio ou não
    let copo_01: Option(Bebida) = Some(Agua)
    let copo_02 = None
  echo copo_01
  echo copo_02

  // Uma pessoa pode ter um apelido ou não
    let pessoa_01 = Pessoa(nome: "Joao", apelido: Some("O Estripador"))
    let pessoa_02 = Pessoa(nome: "Martim", apelido: None)
  echo pessoa_01
  echo pessoa_02

  // Trabalhando com funções que retornam valores do tipo Result
    let dado_01 = 5
  case valor_dado_verificado(dado_01) {
    Ok(valor_verificado) ->
      echo "O valor de " <> int.to_string(valor_verificado) <> " esta correto."
    Error(ValorDadoForaDoIntervalo) ->
      echo "O valor do dado esta fora do intervalo"
  }

  // Vamos tentar dobrar o valor se o valor resultante ainda for
  // um número em qualquer um dos lados do dado.
  // Caso contrário, vamos colocar o valor máximo.
  2
  |> valor_dado_verificado
  |> result.try(dobrar_valor_dado)
  |> result.unwrap(or: 6)
  |> echo
}

pub fn lancar_dado_como_resultado() {
  Ok(int.random(6) + 1)
}

pub fn somar_valores_dados(a: Int, b: Int) {
  Ok(a + b)
}

// Apostando em funções de primeira classe e correspondência de padrões
// pode facilmente levar a toneladas de recuo
fn lancar_dois_dados_sem_use() {
    result.try(lancar_dado_como_resultado(), fn(primeiro_dado) {
        result.try(lancar_dado_como_resultado(), fn(segundo_dado) {
            somar_valores_dados(primeiro_dado, segundo_dado)
    })
  })
}

// A expressão use ainda nos permite escrever código que usa retornos de chamada
// mas limpa o recuo excessivo:
// - Uma chamada para uma função de ordem superior vai para o lado direito do operador <-
// - Os nomes dos argumentos para a função de retorno de chamada ficam no lado esquerdo de
//   o operador <-
// - Todo o código restante no bloco {} envolvente se torna o corpo do
//   função de retorno de chamada.
fn lancar_dois_dados_com_use() {
    use primeiro_dado <- result.try(lancar_dado_como_resultado())
    use segundo_dado <- result.try(lancar_dado_como_resultado())
        somar_valores_dados(primeiro_dado, segundo_dado)
}

fn mais_sobre_callbacks() {
  let _ = echo lancar_dois_dados_sem_use()
  let _ = echo lancar_dois_dados_com_use()
  Nil
}

// Desde a v1.14.0, a anotação @external também pode ser usada em tipos externos para
// aponte-os para os tipos Erlang ou TypeScript.
pub type DataHora

// Funções externas devem anotar um tipo de retorno.
// Esta função é definida apenas no alvo Erlang; compilando para JavaScript
// exigiria um corpo substituto ou uma anotação @external(javascript, ...).
@external(erlang, "calendar", "local_time")
pub fn agora() -> DataHora {
  todo
}

fn mostrar_externos() {
  let _ = echo agora()
  // #(#(2024, 4, 6), #(14, 4, 16))
  Nil
}

// Constantes em nível de módulo (suportadas desde v1.0.0, com lista anexada desde v1.16.0)
pub const numeros_primos = [2, 3, 5]
pub const mais_primos = [7, 11, ..numeros_primos]

fn mostrar_constantes() {
  let _ = echo numeros_primos
  let _ = echo mais_primos

  //Desde a v1.17.0, você também pode usar `todo` dentro de declarações constantes:
  // pub const next_constant = todo
  Nil
}

fn mostrar_dicts() {
  // Dicionários são coleções de valores-chave (stdlib v0.33.0)
  let pontuacoes = dict.new()
    |> dict.insert("Aline", 10)
    |> dict.insert("Beto", 15)

    let _ = echo dict.get(pontuacoes, "Aline")
  // Ok(10)
    let _ = echo dict.get(pontuacoes, "Carlos")
  // Error(Nil)
  Nil
}

fn mostrar_arrays_de_bits() {
  // Matrizes de bits representam sequências de dados binários (stdlib v0.32.0)
  let dados_em_binario = <<0x41, 0x42, 0x43>> // "ABC" em ASCII
  let _ = echo dados_em_binario

  // No Gleam, a correspondência de padrões com `let` deve ser exaustiva (deve cobrir todos
  // valores possíveis). Se um padrão não corresponder em tempo de execução, devemos usar
  // `let assert` em vez de `let`. Se o match falhar, o programa entra em pânico:
  let assert <<primeiro_byte, resto:bytes>> = dados_em_binario
  let _ = echo primeiro_byte // 65
  let _ = echo resto // <<66, 67>>
  Nil
}

// Tipos opacos são tipos cujos construtores são privados do módulo de definição,
// habilitando o encapsulamento e ocultando detalhes de implementação.
pub opaque type DadoCriptografado {
  DadoCriptografado(valor: String)
}

pub fn criptografar(dado: String) -> DadoCriptografado {
  DadoCriptografado(dado <> "_criptografado")
}

fn mostrar_tipos_opacos() {
    let segredo = criptografar("minha_senha")
  // Podemos passar o segredo, mas não podemos combinar padrões em DadoCriptografado
  // ou acesse seu campo .valor de fora deste módulo.
  let _ = echo segredo
  Nil
}

fn mostrar_panic() {
  // Podemos abortar deliberadamente a execução usando a palavra-chave panic
  // para fazer nosso programa travar imediatamente
    let tres = 3
  let dois = 2
    case tres == dois {
    True -> panic as "O operador de igualdade esta quebrado!"
    False -> "O operador de igualdade funciona para inteiros"
  }
  // Chamar uma função que usa a palavra-chave todo também trava
  // lição de casa()
}

pub fn tarefa() {
  todo as "Esta funcao ainda nao foi implementada"
}
```

## Leitura adicional

* [Site oficial do Gleam](https://gleam.run/)
* [Tour de idioma](https://tour.gleam.run/) - Inclui editor de código ao vivo
* [Documentação oficial](https://gleam.run/documentation/)
* [Lista incrível do Gleam](https://github.com/gleam-lang/awesome-gleam)
* [Faixa de exercícios para Gleam](https://exercism.org/tracks/gleam)

Os documentos oficiais possuem cheatsheets para pessoas familiarizadas com:

* [Elixir](https://gleam.run/cheatsheets/gleam-for-elixir-users)
* [Elm](https://gleam.run/cheatsheets/gleam-for-elm-users)
* [Erlang](https://gleam.run/cheatsheets/gleam-for-erlang-users)
* [PHP](https://gleam.run/cheatsheets/gleam-for-php-users)
* [Python](https://gleam.run/cheatsheets/gleam-for-python-users)
* [Rust](https://gleam.run/cheatsheets/gleam-for-rust-users)
