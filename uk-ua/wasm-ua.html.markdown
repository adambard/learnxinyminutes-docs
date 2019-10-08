---
language: WebAssembly
lang: uk-ua
filename: learnwasm-ua.wast
contributors:
    - ["Dean Shaff", "http://dean-shaff.github.io"]
translators:
    - ["Oleh Hromiak", "https://github.com/ogroleg"]
---

```
;; learnwasm-ua.wast

(module
  ;; У WebAssembly весь код знаходиться в модулях. Будь-яка операція
  ;; може бути записана за допомогою s-виразу. Також існує синтаксис "стек машини",
  ;; втім, він не сумісний з проміжним бінарним представленням коду.

  ;; Формат бінарного проміжного представлення майже повністю сумісний 
  ;; з текстовим форматом WebAssembly.
  ;; Деякі відмінності:
  ;; local_set -> local.set
  ;; local_get -> local.get

  ;; Код розміщується у функціях

  ;; Типи даних
  (func $data_types
    ;; WebAssembly має чотири типи даних:
    ;; i32 - ціле число, 32 біти
    ;; i64 - ціле число, 64 біти (не підтримується у JavaScript)
    ;; f32 - число з плаваючою комою, 32 біти
    ;; f64 - число з плаваючою комою, 64 біти

    ;; Створити локальну змінну можна за допомогою ключового слова "local".
    ;; Змінні потрібно оголошувати на початку функції.

    (local $int_32 i32)
    (local $int_64 i64)
    (local $float_32 f32)
    (local $float_64 f64)

    ;; Змінні, оголошені вище, ще не ініціалізовані, себто, не мають значення.
    ;; Давайте присвоїмо їм значення за допомогою <тип даних>.const:

    (local.set $int_32 (i32.const 16))
    (local.set $int_32 (i64.const 128))
    (local.set $float_32 (f32.const 3.14))
    (local.set $float_64 (f64.const 1.28))
  )

  ;; Базові операції
  (func $basic_operations

    ;; Нагадаємо, у WebAssembly будь-що є s-виразом, включно
    ;; з математичними виразами або зчитуванням значень змінних

    (local $add_result i32)
    (local $mult_result f64)

    (local.set $add_result (i32.add (i32.const 2) (i32.const 4)))
    ;; тепер add_result дорівнює 6!

    ;; Для кожної операції потрібно використовувати правильний тип:
    ;; (local.set $mult_result (f32.mul (f32.const 2.0) (f32.const 4.0))) ;; Ніт! mult_result має тип f64!
    (local.set $mult_result (f64.mul (f64.const 2.0) (f64.const 4.0))) ;; Ніт! mult_result має тип f64!

    ;; У WebAssembly є вбудовані функції накшталт математики та побітових операцій.
    ;; Варто зазначити, що тут відсутні вбудовані тригонометричні функції.
    ;; Тож нам потрібно:
    ;; - написати їх самостійно (не найкраща ідея)
    ;; - звідкись їх імпортувати (як саме - побачимо згодом)
  )

  ;; Функції
  ;; Параметри вказуються ключовим словом `param`, значення, що повертається - `result`
  ;; Поточне значення стеку і є значенням функції, що повертається

  ;; Ми можемо викликати інші функції за допомогою `call`

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

  ;; Досі ми не могли що-небудь вивести на консоль і не мали доступу
  ;; до високорівневої математики (степеневі функції, обрахунок експоненти або тригонометрія).
  ;; Більше того, ми навіть не могли викликати WASM функції у Javascript!
  ;; Виклик цих функцій у WebAssembly залежить від того,
  ;; де ми знаходимось - чи це Node.js, чи середовище браузера.

  ;; Якщо ми у Node.js, то потрібно виконати два кроки. По-перше, ми маємо сконвертувати
  ;; текстове представлення WASM у справжній код webassembly.
  ;; Наприклад, ось так (Binaryen):

  ;; wasm-as learn-wasm.wast -o learn-wasm.wasm

  ;; Давай також застосуємо оптимізації:

  ;; wasm-opt learn-wasm.wasm -o learn-wasm.opt.wasm -O3 --rse

  ;; Тепер наш скомпільований WebAssembly можна завантажити у Node.js:
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

  ;; Цей код зчитує функції з importObject
  ;; (вказано у асинхронній JavaScript функції instantiate), а потім експортує функцію
  ;; "print_args", яку ми викликаємо у Node.js

  (import "console" "log" (func $print_i32 (param i32)))
  (import "math" "cos" (func $cos (param f64) (result f64)))

  (func $print_args (param $arg0 i32) (param $arg1 i32)
    (call $print_i32 (local.get $arg0))
    (call $print_i32 (local.get $arg1))
  )
  (export "print_args" (func $print_args))

  ;; Завантаження даних з пам'яті WebAssembly.
  ;; Наприклад, ми хочемо порахувати cos для елементів Javascript масиву.
  ;; Нам потрібно отримати доступ до масиву і можливість ітерувати по ньому.
  ;; У прикладі нижче ми змінимо існуючий масив.
  ;; f64.load і f64.store приймають адресу числа у пам'яті *у байтах*.
  ;; Для того, щоб отримати доступ до 3-го елементу масиву, ми маємо передати щось
  ;; накшталт (i32.mul (i32.const 8) (i32.const 2)) у функцію f64.store.

  ;; У JavaScript ми викличемо `apply_cos64` таким чином
  ;; (використаємо функцію instantiate з попереднього прикладу):
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
  ;; Ця функція не буде працювати, якщо ми виділимо пам'ять для (створимо) Float32Array у JavaScript.

  (memory (export "memory") 100)

  (func $apply_cos64 (param $array_length i32)
    ;; визначаємо змінну циклу або лічильник
    (local $idx i32)
    ;; визначаємо змінну для доступу до пам'яті
    (local $idx_bytes i32)
    ;; константа - кількість байтів у числі типу f64.
    (local $bytes_per_double i32)

    ;; визначаємо змінну, яка зберігатиме значення з пам'яті
    (local $temp_f64 f64)

    (local.set $idx (i32.const 0))
    (local.set $idx_bytes (i32.const 0)) ;; не обов'язково
    (local.set $bytes_per_double (i32.const 8))

    (block
      (loop
        ;; записуємо у idx_bytes необхідне зміщення в пам'яті - для поточного числа.
        (local.set $idx_bytes (i32.mul (local.get $idx) (local.get $bytes_per_double)))

        ;; отримуємо число з пам'яті (за зміщенням):
        (local.set $temp_f64 (f64.load (local.get $idx_bytes)))

        ;; рахуємо cos:
        (local.set $temp_64 (call $cos (local.get $temp_64)))

        ;; тепер зберігаємо результат обчислень у пам'ять:
        (f64.store
          (local.get $idx_bytes)
          (local.get $temp_64))

        ;; або робимо все за один крок (альтернативний код)
        (f64.store
          (local.get $idx_bytes)
          (call $cos
            (f64.load
              (local.get $idx_bytes))))

        ;; збільшуємо лічильник на одиницю (інкремент)
        (local.set $idx (i32.add (local.get $idx) (i32.const 1)))

        ;; якщо лічильник дорівнює довжині масиву, то завершуємо цикл
        (br_if 1 (i32.eq (local.get $idx) (local.get $array_length)))
        (br 0)
      )
    )
  )
  (export "apply_cos64" (func $apply_cos64))
)

```
