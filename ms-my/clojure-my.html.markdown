---
language: clojure
filename: learnclojure-ms.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
translators:
    - ["Burhanuddin Baharuddin", "https://github.com/burhanloey"]
lang: ms-my
---

Clojure ialah salah satu bahasa pengaturcaraan dalam keluarga Lisp yang dibangunkan untuk Java Virtual Machine. Ia lebih
menekankan kepada konsep [functional programming](https://en.wikipedia.org/wiki/Functional_programming) jika dibandingkan
dengan Common Lisp, tetapi juga menyediakan kemudahan [STM](https://en.wikipedia.org/wiki/Software_transactional_memory) 
untuk mengendalikan *state* apabila diperlukan.

Gabungan tersebut membolehkan Clojure untuk mengendalikan beberapa proses serentak (*concurrency*) dengan mudah,
dan kebiasaannya secara automatik.

(Anda perlukan Clojure versi 1.2 ke atas)


```clojure
; Komen bermula dengan koma bertitik (semicolon).

; Clojure ditulis dalam bentuk yang seragam, iaitu
; senarai perkataan di dalam kurungan (parentheses), dipisahkan dengan ruang kosong (whitespace).
;
; Pembaca Clojure akan menganggap bahawa perkataan pertama dalam senarai tersebut
; sebagai `function` atau `macro` untuk digunakan, dan yang selebihnya sebagai arguments.

; Panggilan pertama di dalam fail Clojure mestilah bermula dengan ns, untuk menentukan `namespace`
(ns learnclojure)

; Contoh-contoh asas yang lain:

; str akan mewujudkan sebuah string daripada beberapa `argument`
(str "Hello" " " "World") ; => "Hello World"

; Operasi matematik pun mudah
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; Tanda = boleh digunakan untuk membuat perbandingan yang sama
(= 1 1) ; => true
(= 2 1) ; => false

; Gunakan not untuk mengubah lojik
(not true) ; => false

; Bentuk `nested` berfungsi seperti yang dijangkakan
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Type (Jenis Data)
;;;;;;;;;;;;;

; Clojure menggunakan jenis `object` dari Java untuk `boolean`, `string` dan nombor.
; Gunakan `class` untuk memeriksa jenis sesebuah data.
(class 1) ; Secara default jenis data untuk `Integer` ialah java.lang.Long
(class 1.); Jenis data untuk Float pula ialah java.lang.Double
(class ""); `String` sentiasa berada dalam tanda petikan (quotation mark), dan merupakan java.lang.String
(class false) ; `Boolean` ialah java.lang.Boolean
(class nil); Nilai "null" dipanggil nil

; Jika mahu membuat senarai data secara harfiah, gunakan ' untuk elakkan senarai tersebut
; daripada terus berfungsi
'(+ 1 2) ; => (+ 1 2)
; (singkatan untuk (quote (+ 1 2)))

; Senarai data secara harfiah boleh berfungsi menggunakan eval
(eval '(+ 1 2)) ; => 3

; Collection & Sequence (Koleksi & Urutan)
;;;;;;;;;;;;;;;;;;;

; `List` ialah struktur data `linked-list`, manakala `Vector` pula berasaskan `array`.
; `Vector` dan `List` juga merupakan class dari Java!
(class [1 2 3]); => clojure.lang.PersistentVector
(class '(1 2 3)); => clojure.lang.PersistentList

; Sesebuah list boleh ditulis seperti (1 2 3), tetapi kita perlu meletakkan '
; untuk mengelakkannya daripada berfungsi.
; Juga, (list 1 2 3) adalah sama dengan '(1 2 3)

; "Collections" hanyalah kumpulan data
; Kedua-dua list dan vector ialah collection:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; "Sequences" (seq) ialah kriteria untuk sesebuah senarai data.
; Hanya list yang dikira sebagai seq.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Sesebuah seq hanya perlukan satu kemasukan data untuk diakses.
; Jadi, seq yang boleh jadi `lazy` (malas) -- boleh menjadi tidak terkira (infinite):
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (tiada penghujung)
(take 4 (range)) ;  (0 1 2 3)

; Gunakan cons untuk menambah sesuatu di awal sesebuah list atau vector
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; Conj akan menambah sesuatu ke dalam collection dengan paling berkesan.
; Untuk list, data tersebut dimasukkan di permulaan. Untuk vector, dimasukkan di pengakhiran.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; Gunakan concat untuk menggabungkan list atau vector
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; Gunakan filter dan map untuk berinteraksi dengan data di dalam collection
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Gunakan reduce untuk dikecilkan (kepada satu nilai)
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; Reduce boleh diberi nilai permulaan
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Function
;;;;;;;;;;;;;;;;;;;;;

; Gunakan fn untuk membuat `function`. Sesebuah function pasti memulangkan semula
; hasil daripada barisan yang terakhir.
(fn [] "Hello World") ; => fn

; (Anda perlukan satu lagi kurungan supaya function tersebut dikira)
((fn [] "Hello World")) ; => "Hello World"

; Anda boleh membuat var menggunakan def
(def x 1)
x ; => 1

; Tetapkan sebuah function ke dalam var
(def hello-world (fn [] "Hello World"))
(hello-world) ; => "Hello World"

; Proses di atas boleh diringkaskan menggunakan defn
(defn hello-world [] "Hello World")

; Tanda [] merupakan senarai argument untuk function tersebut.
(defn hello [name]
  (str "Hello " name))
(hello "Steve") ; => "Hello Steve"

; Cara ini juga boleh digunakan untuk membuat function dengan lebih ringkas:
(def hello2 #(str "Hello " %1))
(hello2 "Fanny") ; => "Hello Fanny"

; Anda juga boleh membuat satu function yang mempunyai beberapa bilangan argument
(defn hello3
  ([] "Hello World")
  ([name] (str "Hello " name)))
(hello3 "Jake") ; => "Hello Jake"
(hello3) ; => "Hello World"

; Function boleh diberi argument ekstra dalam bentuk seq
(defn count-args [& args]
  (str "You passed " (count args) " args: " args))
(count-args 1 2 3) ; => "You passed 3 args: (1 2 3)"

; Anda boleh letakkan sekali argument biasa dan argument ekstra
(defn hello-count [name & args]
  (str "Hello " name ", you passed " (count args) " extra args"))
(hello-count "Finn" 1 2 3)
; => "Hello Finn, you passed 3 extra args"


; Map
;;;;;;;;;;

; Hash map dan array map menggunakan `interface` yang sama. Hash map lebih laju untuk diakses
; tetapi tidak mengekalkan urutan.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Arraymap akan bertukar menjadi hashmap secara automatik untuk kebanyakan operasi
; apabila mereka menjadi semakin besar, jadi anda tidak perlu bimbang.

; Map boleh menggunakan apa-apa sahaja jenis data sebagai key, tetapi kebiasaannya keyword adalah yang terbaik
; Keyword adalah sama seperti string cuma lebih efisyen
(class :a) ; => clojure.lang.Keyword

(def stringmap {"a" 1, "b" 2, "c" 3})
stringmap  ; => {"a" 1, "b" 2, "c" 3}

(def keymap {:a 1, :b 2, :c 3})
keymap ; => {:a 1, :c 3, :b 2}

; Oh, sebelum terlupa, tanda koma di atas hanya dianggap seperti whitespace, tak buat apa-apa.
; Dapatkan nilai daripada map dengan menggunakannya seperti function
(stringmap "a") ; => 1
(keymap :a) ; => 1

; Keyword juga boleh digunakan untuk mendapatkan nilai daripada map tersebut!
(:b keymap) ; => 2

; Jangan cuba teknik di atas menggunakan string, tak jadi.
;("a" stringmap)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Apabila key yang digunakan tidak wujud, map akan memberi nil
(stringmap "d") ; => nil

; Gunakan assoc untuk menambah key yang baru ke dalam hash-map
(def newkeymap (assoc keymap :d 4))
newkeymap ; => {:a 1, :b 2, :c 3, :d 4}

; Tetapi ingat, data dalam clojure adalah `immutable` (tidak berubah)!
keymap ; => {:a 1, :b 2, :c 3}

; Gunakan dissoc untuk membuang key
(dissoc keymap :a :b) ; => {:c 3}

; Set
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; Tambah data menggunakan conj
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; Buang data menggunakan disj
(disj #{1 2 3} 1) ; => #{2 3}

; Periksa kewujudan data dengan menggunakan set tersebut sebagai function:
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; Ada pelbagai lagi function untuk set di namespace clojure.sets.

; Form yang berguna
;;;;;;;;;;;;;;;;;

; Lojik dalam clojure hanyalah sebuah macro, dan kelihatan seperti
; yang lain
(if false "a" "b") ; => "b"
(if false "a") ; => nil

; Gunakan let untuk membuat binding sementara
(let [a 1 b 2]
  (> a b)) ; => false

; Kumpulkan beberapa statement sekali menggunakan do
(do
  (print "Hello")
  "World") ; => "World" (prints "Hello")

; Function sebenarnya ada do secara tersirat
(defn print-and-say-hello [name]
  (print "Saying hello to " name)
  (str "Hello " name))
(print-and-say-hello "Jeff") ;=> "Hello Jeff" (prints "Saying hello to Jeff")

; Let pun sama
(let [name "Urkel"]
  (print "Saying hello to " name)
  (str "Hello " name)) ; => "Hello Urkel" (prints "Saying hello to Urkel")


; Gunakan `threading macro` (-> dan ->>) untuk menulis penggubahan data
; dengan lebih jelas.

; Macro "thread-first" (->) memasukkan hasil perkiraan ke setiap form
; yang selepasnya, sebagai argument pertama (item yang kedua)
(->  
   {:a 1 :b 2} 
   (assoc :c 3) ;=> (assoc {:a 1 :b 2} :c 3)
   (dissoc :b)) ;=> (dissoc (assoc {:a 1 :b 2} :c 3) :b)

; Code di atas boleh ditulis seperti ini:
; (dissoc (assoc {:a 1 :b 2} :c 3) :b)
; dan hasilnya ialah {:a 1 :c 3}

; Yang dua anak panah pula membuat benda yang sama, tetapi memasukkan hasil perkiraan 
; setiap baris ke pengakhiran form selepasnya. Cara ini berguna untuk operasi 
; yang melibatkan collection:
(->>
   (range 10)
   (map inc)     ;=> (map inc (range 10)
   (filter odd?) ;=> (filter odd? (map inc (range 10))
   (into []))    ;=> (into [] (filter odd? (map inc (range 10)))
                 ; Result: [1 3 5 7 9]

; Jika anda mahu lebih fleksibel untuk meletakkan hasil perkiraan,
; anda boleh menggunakan macro `as->`. Dengan menggunakan macro tersebut,
; anda boleh menentukan nama untuk output dan menggunakannya semula
; ke dalam operasi berangkai:

(as-> [1 2 3] input
  (map inc input);=> You can use last transform's output at the last position
  (nth input 2) ;=>  and at the second position, in the same expression
  (conj [4 5 6] input [8 9 10])) ;=> or in the middle !



; Module
;;;;;;;;;;;;;;;

; Gunakan "use" untuk mendapatkan semua function daripada sesebuah module
(use 'clojure.set)

; Sekarang kita boleh menggunakan operasi untuk set
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Anda juga boleh memilih sebahagian daripada function untuk diimport
(use '[clojure.set :only [intersection]])

; Gunakan require untuk mengimport sesebuah module
(require 'clojure.string)

; Gunakan / untuk menggunakan function daripada module
; Di sini, nama module tersebut ialah clojure.string dan function-nya ialah blank?
(clojure.string/blank? "") ; => true

; Anda juga boleh memberi nama yang lebih ringkas untuk module semasa import
(require '[clojure.string :as str])
(str/replace "This is a test." #"[a-o]" str/upper-case) ; => "THIs Is A tEst."
; (#"" ialah ungkapan untuk regular expression, regex)

; Anda boleh menggunakan require (dan use, tetapi elakkan) daripada namespace menggunakan :require.
; Anda tidak perlu menulis semula nama module dengan cara ini.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))

; Java
;;;;;;;;;;;;;;;;;

; Java mengandungi banyak standard library yang kita boleh manfaatkan, jadi
; anda patut tahu bagaimana untuk menggunakannya.

; Gunakan import untuk load module java
(import java.util.Date)

; Anda juga boleh import menggunakan ns.
(ns test
  (:import java.util.Date
           java.util.Calendar))

; Gunakan nama class berserta "." di hujungnya untuk membuat object baru
(Date.) ; <object date>

; Gunakan . untuk menggunakan method. Atau gunakan shortcut seperti ".method"
(. (Date.) getTime) ; <sebuah timestamp>
(.getTime (Date.)) ; sama sahaja.

; Gunakan / untuk menggunakan static method
(System/currentTimeMillis) ; <sebuah timestamp> (System sentiasa wujud dalam Java)

; Gunakan doto untuk menjadikan proses yang melibatkan class mutable (boleh berubah) lebih mudah
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0)
  .getTime) ; => Sebuah Date. yang ditetapkan kepada 2000-01-01 00:00:00

; STM
;;;;;;;;;;;;;;;;;

; Software Transactional Memory ialah mekanisme dalam Clojure untuk mengendalikan
; state yang kekal berterusan. Ada beberapa kaedah dalam Clojure yang menggunakan teknik tersebut.

; Atom adalah yang paling mudah. Letakkannya sewaktu meletakkan nilai permulaan.
(def my-atom (atom {}))

; Kemas kini sebuah atom menggunakan swap!.
; swap! mengambil satu function dan menggunakannya menggunakan nilai asal atom
; sebagai argument pertama, dan argument selebihnya sebagai argument kedua
(swap! my-atom assoc :a 1) ; Tetapkan my-atom kepada hasil perkiraan (assoc {} :a 1)
(swap! my-atom assoc :b 2) ; Tetapkan my-atom kepada hasil perkiraan (assoc {:a 1} :b 2)

; Gunakan '@' untuk mendapatkan nilai daripada atom
my-atom  ;=> Atom<#...> (memberi object atom itu sendiri)
@my-atom ; => {:a 1 :b 2}

; Ini adalah contoh untuk mengira menggunakan atom
(def counter (atom 0))
(defn inc-counter []
  (swap! counter inc))

(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)
(inc-counter)

@counter ; => 5

; Kaedah lain yang menggunakan STM ialah ref dan agent.
; Ref: http://clojure.org/refs
; Agent: http://clojure.org/agents
```

### Bacaan Lanjut

Ini masih belum lengkap, tetapi harap-harap cukup untuk membuatkan anda lebih bersedia.

Clojure.org mempunyai banyak artikel:
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org mempunyai dokumentasi berserta contoh untuk menggunakan kebanyakan function teras:
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure ialah cara yang baik untuk mengasah skill Clojure dan functional programming:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org (yup, serius) juga mengandungi beberapa artikel sebagai permulaan:
[http://clojure-doc.org/](http://clojure-doc.org/)
