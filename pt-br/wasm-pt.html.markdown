---
language: WebAssembly
filename: learn-wasm.wast
contributors:
    - ["Dean Shaff", "http://dean-shaff.github.io"]
translators:
    - ["Bárbara Luz", "http://github.com/barbluz"]
---

```
;; learn-wasm.wast

(module
   ;; No WebAssembly, tudo está incluído em um módulo. Além disso, tudo
   ;; pode ser expresso como uma expressão s. Alternativamente, existe a
   ;; sintaxe "stack machine", mas isso não é compatível com Binaryen
   ;; sintaxe de representação intermediária (IR).

   ;; O formato Binaryen IR é *principalmente* compatível com o formato de texto WebAssembly.
   ;; Existem algumas pequenas diferenças:
   ;; local_set -> local.set
   ;; local_get -> local.get

    ;; Temos que colocar o código em funções

  ;; Tipos de dados
(func $data_types
    ;; WebAssembly has only four types:
    ;; i32 - 32 bit integer
    ;; i64 - 64 bit integer (não suportado em JavaScript)
    ;; f32 - 32 bit floating point
    ;; f64 - 64 bit floating point

     ;; Podemos declarar variáveis locais com a palavra-chave "local"
     ;; Temos que declarar todas as variáveis antes de começarmos a fazer qualquer coisa
     ;; dentro da função

    (local $int_32 i32)
    (local $int_64 i64)
    (local $float_32 f32)
    (local $float_64 f64)

     ;; Esses valores permanecem não inicializados.
     ;; Para defini-los com um valor, podemos usar <type>.const:

    (local.set $int_32 (i32.const 16))
    (local.set $int_64 (i64.const 128))
    (local.set $float_32 (f32.const 3.14))
    (local.set $float_64 (f64.const 1.28))
  )

  ;; Operações básicas
  (func $basic_operations

    ;; No WebAssembly, tudo é uma s-expression, incluindo
    ;; fazendo contas ou obtendo o valor de alguma variável

    (local $add_result i32)
    (local $mult_result f64)

    (local.set $add_result (i32.add (i32.const 2) (i32.const 4)))
    ;; o valor de add_result agora é 6!

    ;; Temos que usar o tipo de dados correto para cada operação:
    ;; (local.set $mult_result (f32.mul (f32.const 2.0) (f32.const 4.0))) 
    ;; ERRADO! mult_result é f64!
     (local.set $mult_result (f64.mul (f64.const 2.0) (f64.const 4.0)))

    ;; O WebAssembly tem algumas operações internas, como matemática básica e deslocamento de bits.
    ;; Notavelmente, ele não possui funções trigonométricas embutidas.
    ;; Para ter acesso a essas funções, temos que
    ;; - implementá-los nós mesmos (não recomendado)
    ;; - importá-los de outro lugar (mais tarde)
  )

   ;; Funções
   ;; Especificamos argumentos com a palavra-chave `param` e especificamos valores de retorno
   ;; com a palavra-chave `result`
   ;; O valor atual na pilha é o valor de retorno de uma função

   ;; Podemos chamar outras funções que definimos com a palavra-chave `call`

  (func $get_16 (result i32)
    (i32.const 16)
  )

  (func $add (param $param0 i32) (param $param1 i32) (result i32)
    (i32.add
      (local.get $param0)
      (local.get $param1)
    )
  )

  (func $double_16 (result i32)
    (i32.mul
      (i32.const 2)
      (call $get_16))
  )

  ;; Até agora, não conseguimos imprimir nada, nem temos
  ;; acesso a funções matemáticas de nível superior (funções pow, exp ou trig).
  ;; Além disso, não conseguimos usar nenhuma das funções WASM em Javascript!
  ;; A maneira como colocamos essas funções no WebAssembly
  ;; parece diferente se estivermos em um ambiente Node.js ou de navegador.

   ;; Se estivermos em Node.js, temos que fazer duas etapas. Primeiro temos que converter o
   ;; Representação de texto WASM em webassembly real. Se estivermos usando Binyaren,
   ;; podemos fazer isso com um comando como o seguinte:
   
  ;; wasm-as learn-wasm.wast -o learn-wasm.wasm

  ;; Podemos aplicar otimizações Binaryen a esse arquivo com um comando como o
  ;; seguinte:

  ;; wasm-opt learn-wasm.wasm -o learn-wasm.opt.wasm -O3 --rse

  ;; Com nosso WebAssembly compilado, agora podemos carregá-lo no Node.js:
  ;; const fs = require('fs')
  ;; const instantiate = async function (inFilePath, _importObject) {
  ;;  var importObject = {
  ;;     console: {
  ;;       log: (x) => console.log(x),
  ;;     },
  ;;     math: {
  ;;       cos: (x) => Math.cos(x),
  ;;     }
  ;;   }
  ;;  importObject = Object.assign(importObject, _importObject)
  ;;
  ;;  var buffer = fs.readFileSync(inFilePath)
  ;;  var module = await WebAssembly.compile(buffer)
  ;;  var instance = await WebAssembly.instantiate(module, importObject)
  ;;  return instance.exports
  ;; }
  ;;
  ;; const main = function () {
  ;;   var wasmExports = await instantiate('learn-wasm.wasm')
  ;;   wasmExports.print_args(1, 0)
  ;; }

  ;; O trecho a seguir obtém as funções do importObject que definimos
  ;; no JavaScript instanciar a função assíncrona e, em seguida, exportar uma função
  ;; "print_args" que podemos chamar do Node.js

  (import "console" "log" (func $print_i32 (param i32)))
  (import "math" "cos" (func $cos (param f64) (result f64)))

  (func $print_args (param $arg0 i32) (param $arg1 i32)
    (call $print_i32 (local.get $arg0))
    (call $print_i32 (local.get $arg1))
  )
  (export "print_args" (func $print_args))

  ;; Carregando dados da memória do WebAssembly.
  ;; Digamos que queremos aplicar a função cosseno a um array Javascript.
  ;; Precisamos ser capazes de acessar a matriz alocada e iterar por ela.
  ;; Este exemplo modificará a matriz de entrada no local.
  ;; f64.load e f64.store esperam a localização de um número na memória *em bytes*.
  ;; Se quisermos acessar o 3º elemento de um array, temos que passar algo
  ;; como (i32.mul (i32.const 8) (i32.const 2)) para a função f64.store.
  ;; In JavaScript, we would call `apply_cos64` as follows
  ;; (using the instantiate function from earlier):
  ;;
  ;; const main = function () {
  ;;   var wasm = await instantiate('learn-wasm.wasm')
  ;;   var n = 100
  ;;   const memory = new Float64Array(wasm.memory.buffer, 0, n)
  ;;   for (var i=0; i<n; i++) {
  ;;     memory[i] = i;
  ;;   }
  ;;   wasm.apply_cos64(n)
  ;; }
  ;;
  ;; Esta função não funcionará se alocarmos um Float32Array no lado do JavaScript.

  (memory (export "memory") 100)

 (func $apply_cos64 (param $array_length i32)
     ;; declarar o contador de loops
     (local $idx i32)
     ;; declare o contador que nos permitirá acessar a memória
     (local $idx_bytes i32)
     ;; constante que expressa o número de bytes em um número f64.
     (local $ bytes_per_double i32)

     ;; declarar uma variável para armazenar o valor carregado da memória
     (local $temp_f64 f64)

     (local.set $idx (i32.const 0))
     (local.set $idx_bytes (i32.const 0)) ;; não é totalmente necessário
     (local.set $bytes_per_double (i32.const 8))
     
    (block
      (loop
      ;; isso define idx_bytes para deslocamento de bytes do valor em que estamos interessados.
      (local.set $idx_bytes (i32.mul (local.get $idx) (local.get $bytes_per_double)))

        ;; obtenha o valor do array da memória:
        (local.set $temp_f64 (f64.load (local.get $idx_bytes)))

        ;; agora aplique a função cosseno:
        (local.set $temp_64 (call $cos (local.get $temp_64)))

        ;; agora armazene o resultado no mesmo local na memória:
        (f64.store
          (local.get $idx_bytes)
          (local.get $temp_64))

        ;; fazer tudo em um passo em vez disso
        (f64.store
          (local.get $idx_bytes)
          (call $cos
            (f64.load
              (local.get $idx_bytes))))

        ;; incrementar o contador de loops
        (local.set $idx (i32.add (local.get $idx) (i32.const 1)))

        ;; parar o loop se o contador de loop for igual ao comprimento do array
        (br_if 1 (i32.eq (local.get $idx) (local.get $array_length)))
        (br 0)
      )
    )
  )
  (export "apply_cos64" (func $apply_cos64))

  ;; Wasm é uma linguagem baseada em pilha, mas para retornar valores mais complicados
  ;; do que um int/float, uma pilha de memória separada deve ser gerenciada manualmente. Um
  ;; abordagem é usar um global mutável para armazenar o stack_ptr. Nós damos
  ;; nós mesmos 1MiB de memstack e crescemos para baixo.
  ;;
  ;; Abaixo está uma demonstração de como este código C **pode** ser escrito à mão
  ;;   typedef struct {
  ;;       int a;
  ;;       int b;
  ;;   } sum_struct_t;
  ;;
  ;;   sum_struct_t sum_struct_create(int a, int b) {
  ;;     return (sum_struct_t){a, b};
  ;;   }
  ;;
  ;;   int sum_local() {
  ;;     sum_struct_t s = sum_struct_create(40, 2);
  ;;     return s.a + s.b;
  ;;   }

  ;; Ao contrário de C, devemos gerenciar nossa própria pilha de memória. Reservamos 1MiB
  (global $memstack_ptr (mut i32) (i32.const 65536))

  ;; Estruturas só podem ser retornadas por referência
  (func $sum_struct_create 
        (param $sum_struct_ptr i32) 
        (param $var$a i32) 
        (param $var$b i32)
    ;; c// sum_struct_ptr->a = a;
    (i32.store
      (get_local $sum_struct_ptr)
      (get_local $var$a)
    )

    ;; c// sum_struct_ptr->b = b;
    (i32.store offset=4
      (get_local $sum_struct_ptr)
      (get_local $var$b)
    )
  )

  (func $sum_local (result i32)
    (local $var$sum_struct$a i32)
    (local $var$sum_struct$b i32)
    (local $local_memstack_ptr i32)

    ;; reserve memstack space
    (i32.sub
      (get_global $memstack_ptr)
      (i32.const 8)
    )
    tee_local $local_memstack_ptr ;; tee guarda e retorna o valor dado
    set_global $memstack_ptr

    ;; chamar a função, armazenando o resultado no memstack
    (call $sum_struct_create
      ((;$sum_struct_ptr=;) get_local $local_memstack_ptr)
      ((;$var$a=;) i32.const 40)
      ((;$var$b=;) i32.const 2)
    )

    ;; recuperar valores da estrutura
    (set_local $var$sum_struct$a
      (i32.load offset=0 (get_local $local_memstack_ptr))
    )
    (set_local $var$sum_struct$b
      (i32.load offset=4 (get_local $local_memstack_ptr))
    )

    ;; desmarcar espaço de memstack
    (set_global $memstack_ptr
        (i32.add
          (get_local $local_memstack_ptr)
          (i32.const 8)
        )
    )

    (i32.add
      (get_local $var$sum_struct$a)
      (get_local $var$sum_struct$b)
    )
  )
  (export "sum_local" (func $sum_local))
)

```
