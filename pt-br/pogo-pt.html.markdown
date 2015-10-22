---
language: pogoscript
contributors:
    - ["Tim Macfarlane", "http://github.com/refractalize"]
    - ["Cássio Böck", "https://github.com/cassiobsilva"]
filename: learnPogo-pt-br.pogo
lang: pt-br
---

Pogoscript é uma linguagem de programação que possui tipos primitivos concorrents
e que compila para linguagem Javascript padrão.

``` javascript
// definindo uma variável
water temperature = 24

// reatribuindo o valor de uma variável após a sua definição
water temperature := 26

// funções permitem que seus parâmetros sejam colocados em qualquer lugar
temperature at (a) altitude = 32 - a / 100

// funções longas são apenas indentadas
temperature at (a) altitude :=
    if (a < 0)
        water temperature
    else
        32 - a / 100

// declarando uma função
current temperature = temperature at 3200 altitude

// essa função cria um novo objeto com métodos
position (x, y) = {
    x = x
    y = y

    distance from position (p) =
        dx = self.x - p.x
        dy = self.y - p.y
        Math.sqrt (dx * dx + dy * dy)
}

// `self` é similiar ao `this` do Javascript, com exceção de que `self` não
// é redefinido em cada nova função

// declaração de métodos
position (7, 2).distance from position (position (5, 1))

// assim como no Javascript, objetos também são hashes
position.'x' == position.x == position.('x')

// arrays
positions = [
    position (1, 1)
    position (1, 2)
    position (1, 3)
]

// indexando um array
positions.0.y

n = 2
positions.(n).y

// strings
poem = 'Tail turned to red sunset on a juniper crown a lone magpie cawks.
        Mad at Oryoki in the shrine-room -- Thistles blossomed late afternoon.
        Put on my shirt and took it off in the sun walking the path to lunch.
        A dandelion seed floats above the marsh grass with the mosquitos.
        At 4 A.M. the two middleaged men sleeping together holding hands.
        In the half-light of dawn a few birds warble under the Pleiades.
        Sky reddens behind fir trees, larks twitter, sparrows cheep cheep cheep
        cheep cheep.'

// texto de Allen Ginsburg

// interpolação
outlook = 'amazing!'
console.log "the weather tomorrow is going to be #(outlook)"

// expressões regulares
r/(\d+)m/i
r/(\d+) degrees/mg

// operadores
true @and true
false @or true
@not false
2 < 4
2 >= 2
2 > 1

// os operadores padrão do Javascript também são suportados

// definindo seu próprio operador
(p1) plus (p2) =
    position (p1.x + p2.x, p1.y + p2.y)

// `plus` pode ser usado com um operador
position (1, 1) @plus position (0, 2)
// ou como uma função
(position (1, 1)) plus (position (0, 2))

// retorno explícito
(x) times (y) = return (x * y)

// new
now = @new Date ()

// funções podem receber argumentos opcionais
spark (position, color: 'black', velocity: {x = 0, y = 0}) = {
    color = color
    position = position
    velocity = velocity
}

red = spark (position 1 1, color: 'red')
fast black = spark (position 1 1, velocity: {x = 10, y = 0})

// funções também podem ser utilizadas para realizar o "unsplat" de argumentos
log (messages, ...) =
    console.log (messages, ...)

// blocos são funções passadas para outras funções.
// Este bloco recebe dois parâmetros, `spark` e `c`,
// o corpo do bloco é o código indentado após a declaração da função

render each @(spark) into canvas context @(c)
    ctx.begin path ()
    ctx.stroke style = spark.color
    ctx.arc (
        spark.position.x + canvas.width / 2
        spark.position.y
        3
        0
        Math.PI * 2
    )
    ctx.stroke ()

// chamadas assíncronas

// O Javascript, tanto no navegador quanto no servidor (através do Node.js)
// realiza um uso massivo de funções assíncronas de E/S (entrada/saída) com
// chamadas de retorno (callbacks). A E/S assíncrona é ótima para a performance e
// torna a utilização da concorrência simples, porém pode rapidamente se tornar
// algo complicado.
// O Pogoscript possui algumas coisas que tornam o uso de E/S assíncrono muito
// mais fácil

// O Node.js inclui o móduolo `fs` para acessar o sistema de arquivos.
// Vamos listar o conteúdo de um diretório

fs = require 'fs'
directory listing = fs.readdir! '.'

// `fs.readdir()` é uma função assíncrona, então nos a chamamos usando o
// operador `!`. O operador `!` permite que você chame funções assíncronas
// com a mesma sintaxe e a mesma semântica do que as demais funções síncronas.
// O Pogoscript reescreve a função para que todo código inserido após o
//  operador seja inserido em uma função de callback para o `fs.readdir()`.

// obtendo erros ao utilizar funções assíncronas

try
    another directory listing = fs.readdir! 'a-missing-dir'
catch (ex)
    console.log (ex)

// na verdade, se você não usar o `try catch`, o erro será passado para o
// `try catch` mais externo do evento, assim como é feito em exceções síncronas

// todo o controle de estrutura também funciona com chamadas assíncronas
// aqui um exemplo de `if else`
config =
    if (fs.stat! 'config.json'.is file ())
        JSON.parse (fs.read file! 'config.json' 'utf-8')
    else
        {
            color: 'red'
        }

// para executar duas chamadas assíncronas de forma concorrente, use o
// operador `?`.
// O operador `?` retorna um *future*, que pode ser executado para
// aguardar o resultado, novamente utilizando o operador `!`

// nós não esperamos nenhuma dessas chamadas serem concluídas
a = fs.stat? 'a.txt'
b = fs.stat? 'b.txt'

// agora nos aguardamos o término das chamadas e escrevemos os resultados
console.log "size of a.txt is #(a!.size)"
console.log "size of b.txt is #(b!.size)"

// no Pogoscript, futures são semelhantes a Promises
```
E encerramos por aqui.

Baixe o [Node.js](http://nodejs.org/) e execute `npm install pogo`.

Há bastante documentação em [http://pogoscript.org/](http://pogoscript.org/), incluindo um material para [consulta rápida](http://pogoscript.org/cheatsheet.html), um [guia](http://pogoscript.org/guide/), e como o [Pogoscript é traduzido para o Javascript](http://featurist.github.io/pogo-examples/). Entre em contato através do [grupo do Google](http://groups.google.com/group/pogoscript) se você possui dúvidas!
