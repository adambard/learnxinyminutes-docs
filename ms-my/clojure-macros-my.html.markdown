---
language: "clojure macros"
filename: learnclojuremacros-ms.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Burhanuddin Baharuddin", "https://github.com/burhanloey"]
lang: ms-my
---

Sama seperti Lisp yang lain, sifat Clojure yang mempunyai [homoiconicity](https://en.wikipedia.org/wiki/Homoiconic)
membolehkan anda untuk menggunakan sepenuhnya language ini untuk menulis code yang boleh generate code sendiri yang
dipanggil "macro". Macro memberi cara yang sangat menarik untuk mengubahsuai language mengikut kehendak anda.

Jaga-jaga. Penggunaan macro boleh dikatakan tidak elok jika digunakan secara berlebihan jika function sahaja sudah mencukupi.
Gunakan macro hanya apabila anda mahu lebih kawalan terhadap sesuatu form.

Biasakan diri dengan Clojure terlebih dahulu. Pastikan anda memahami semuanya di 
[Clojure in Y Minutes](/docs/ms-my/clojure-my/).

```clojure
;; Define macro menggunakan defmacro. Macro anda akan output list yang boleh
;; dijalankan sebagai code clojure.
;;
;; Macro ini adalah sama seperti (reverse "Hello World")
(defmacro my-first-macro []
  (list reverse "Hello World"))

;; Lihat hasil macro tersebut menggunakan macroexpand atau macroexpand-1.
;;
;; Pastikan panggilan kepada macro tersebut mempunyai tanda petikan
(macroexpand '(my-first-macro))
;; -> (#<core$reverse clojure.core$reverse@xxxxxxxx> "Hello World")

;; Anda boleh menggunakan eval terus kepada macroexpand untuk mendapatkan hasil:
(eval (macroexpand '(my-first-macro)))
; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; Tetapi anda sepatutnya menggunakan cara yang lebih ringkas, sama seperti panggilan kepada function:
(my-first-macro)  ; -> (\d \l \o \r \W \space \o \l \l \e \H)

;; Anda boleh memudahkan cara untuk membuat macro dengan mengguna tanda petikan
;; untuk membuat list untuk macro:
(defmacro my-first-quoted-macro []
  '(reverse "Hello World"))

(macroexpand '(my-first-quoted-macro))
;; -> (reverse "Hello World")
;; Perhatikan yang reverse bukan lagi function tetapi adalah simbol.

;; Macro boleh mengambil argument.
(defmacro inc2 [arg]
  (list + 2 arg))

(inc2 2) ; -> 4

;; Tetapi jika anda membuat cara yang sama menggunakan tanda petikan, anda akan mendapat error sebab
;; argument tersebut juga akan mempunyai tanda petikan. Untuk mengatasi masalah ini, Clojure memberi
;; cara untuk meletak tanda petikan untuk macro: `. Di dalam `, anda boleh menggunakan ~ untuk mendapatkan scope luaran
(defmacro inc2-quoted [arg]
  `(+ 2 ~arg))

(inc2-quoted 2)

;; Anda boleh menggunakan destructuring untuk argument seperti biasa. Gunakan ~@ untuk mengembangkan variable
(defmacro unless [arg & body]
  `(if (not ~arg)
     (do ~@body))) ; Jangan lupa do!

(macroexpand '(unless true (reverse "Hello World")))
;; ->
;; (if (clojure.core/not true) (do (reverse "Hello World")))

;; (unless) mengembalikan body jika argument yang pertama adalah false.
;; Jika tidak, (unless) akan memulangkan nil

(unless true "Hello") ; -> nil
(unless false "Hello") ; -> "Hello"

;; Jika tidak berhati-hati, macro boleh memeningkan anda dengan mencampuradukkan nama variable
(defmacro define-x []
  '(do
     (def x 2)
     (list x)))

(def x 4)
(define-x) ; -> (2)
(list x) ; -> (2)

;; Untuk mengelakkan masalah ini, gunakan gensym untuk mendapatkan identifier yang berbeza
(gensym 'x) ; -> x1281 (atau yang sama waktu dengannya)

(defmacro define-x-safely []
  (let [sym (gensym 'x)]
    `(do
       (def ~sym 2)
       (list ~sym))))

(def x 4)
(define-x-safely) ; -> (2)
(list x) ; -> (4)

;; Anda boleh menggunakan # di dalam ` untuk menghasilkan gensym untuk setiap simbol secara automatik
(defmacro define-x-hygienically []
  `(do
     (def x# 2)
     (list x#)))

(def x 4)
(define-x-hygienically) ; -> (2)
(list x) ; -> (4)

;; Kebiasaannya helper function digunakan untuk membuat macro. Jom buat beberapa function untuk
;; membuatkan program boleh memahami inline arithmetic. Saja suka-suka.
(declare inline-2-helper)
(defn clean-arg [arg]
  (if (seq? arg)
    (inline-2-helper arg)
    arg))

(defn apply-arg
  "Diberi argument [x (+ y)], pulangkan (+ x y)"
  [val [op arg]]
  (list op val (clean-arg arg)))

(defn inline-2-helper
  [[arg1 & ops-and-args]]
  (let [ops (partition 2 ops-and-args)]
    (reduce apply-arg (clean-arg arg1) ops)))

;; Kita boleh test terlebih dahulu tanpa membuat macro
(inline-2-helper '(a + (b - 2) - (c * 5))) ; -> (- (+ a (- b 2)) (* c 5))

; Tetapi, kita perlu membuat macro jika kita mahu jalankan code tersebut
(defmacro inline-2 [form]
  (inline-2-helper form))

(macroexpand '(inline-2 (1 + (3 / 2) - (1 / 2) + 1)))
; -> (+ (- (+ 1 (/ 3 2)) (/ 1 2)) 1)

(inline-2 (1 + (3 / 2) - (1 / 2) + 1))
; -> 3 (sepatutnya, 3N, sebab nombor tersebut ditukarkan kepada pecahan rasional menggunakan /)
```

### Bacaaan Lanjut

[Writing Macros daripada](http://www.braveclojure.com/writing-macros/)

[Dokumen rasmi](http://clojure.org/macros)

[Bila perlu guna macro?](https://lispcast.com/when-to-use-a-macro/)
