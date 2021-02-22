---

language: "Common Lisp"
filename: commonlisp-ms.lisp
contributors:
  - ["Paul Nathan", "https://github.com/pnathan"]
  - ["Rommel Martinez", "https://ebzzry.io"]
translators:
  - ["Burhanuddin Baharuddin", "https://github.com/burhanloey"]
lang: ms-my
---

Common Lisp ialah programming language yang general-purpose (boleh digunakan untuk semua benda) dan multi-paradigm (konsep yang pelbagai) sesuai untuk pelbagai kegunaan di dalam
industri aplikasi. Common Lisp biasa digelar sebagai programmable programming language (programming language yang boleh di-program-kan).

Sumber bacaan yang klasik ialah [Practical Common Lisp](http://www.gigamonkeys.com/book/). Sumber bacaan yang lain dan
yang terbaru ialah [Land of Lisp](http://landoflisp.com/). Buku baru mengenai best practices (amalan terbaik),
[Common Lisp Recipes](http://weitz.de/cl-recipes/), baru sahaja diterbitkan.



```common-lisp

;;;-----------------------------------------------------------------------------
;;; 0. Syntax
;;;-----------------------------------------------------------------------------

;;; General form (Bentuk umum)

;;; Ada dua asas dalam syntax CL: ATOM dan S-EXPRESSION.
;;; Kebiasaannya, gabungan S-expression dipanggil sebagai `forms`.

10            ; atom; bermaksud seperti yang ditulis iaitu nombor 10
:thing        ; juga atom; bermaksud simbol :thing
t             ; juga atom, bermaksud true (ya/betul/benar)
(+ 1 2 3 4)   ; s-expression
'(4 :foo t)   ; juga s-expression


;;; Comment (Komen)

;;; Comment satu baris bermula dengan semicolon; gunakan empat untuk comment
;;; mengenai file, tiga untuk seksyen penghuraian, dua untuk yang dalam definition,
;;; dan satu untuk satu baris. Sebagai contoh,

;;;; life.lisp

;;; Foo bar baz, disebabkan quu quux. Sangat optimum untuk krakaboom dan umph.
;;; Diperlukan oleh function LINULUKO. Ini merepek sahaja kebaboom.

(defun meaning (life)
  "Memulangkan hasil pengiraan makna KEHIDUPAN"
  (let ((meh "abc"))
    ;; Jalankan krakaboom
    (loop :for x :across meh
       :collect x)))                    ; Simpan hasil ke x, kemudian pulangkan

;;; Komen berbentuk blok, sebaliknya, membenarkan komen untuk bentuk bebas. Komen
;;; tersebut berada di antara #| dan |#

#| Ini adalah komen berbentuk blok di mana
   tulisan boleh ditulis dalam beberapa baris dan
    #|
       juga boleh dalam bentuk nested (berlapis-lapis)!
    |#
|#


;;; Environment (benda-benda yang diperlukan untuk program menggunakan Common Lisp)

;;; Common Lisp ada banyak jenis; kebanyakannya mengikut standard. SBCL
;;; ialah titik permulaan yang baik. Quicklisp boleh digunakan untuk install 
;;; library third party.

;;; CL kebiasaannya digunakan dengan text editor dan Real Eval Print
;;; Loop (REPL) yang dilancarkan dengan serentak. REPL membolehkan kita menjelajah
;;; program secara interaktif semasa program tersebut sedang berjalan secara "live".


;;;-----------------------------------------------------------------------------
;;; 1. Datatype primitif dan operator
;;;-----------------------------------------------------------------------------

;;; Simbol

'foo ; => FOO  Perhatikan simbol menjadi huruf besar secara automatik.

;;; INTERN menjadikan string sebagai simbol secara manual.

(intern "AAAA")        ; => AAAA
(intern "aaa")         ; => |aaa|

;;; Nombor

9999999999999999999999 ; integer
#b111                  ; binary => 7
#o111                  ; octal => 73
#x111                  ; hexadecimal => 273
3.14159s0              ; single
3.14159d0              ; double
1/2                    ; ratio
#C(1 2)                ; complex number

;;; Function ditulis sebagai (f x y z ...) di mana f ialah function dan
;;; x, y, z, ... adalah argument.

(+ 1 2)                ; => 3

;;; Jika anda ingin membuat data sebagai data bukannya function, gunakan QUOTE
;;; untuk mengelakkan data tersebut daripada dikira oleh program

(quote (+ 1 2))        ; => (+ 1 2)
(quote a)              ; => A

;;; Singkatan untuk QUOTE ialah ' (tanda petikan)

'(+ 1 2)               ; => (+ 1 2)
'a                     ; => A

;;; Operasi arithmetic asas

(+ 1 1)                ; => 2
(- 8 1)                ; => 7
(* 10 2)               ; => 20
(expt 2 3)             ; => 8
(mod 5 2)              ; => 1
(/ 35 5)               ; => 7
(/ 1 3)                ; => 1/3
(+ #C(1 2) #C(6 -4))   ; => #C(7 -2)

;;; Boolean

t                      ; true; semua nilai yang bukan NIL ialah true
nil                    ; false; termasuklah list yang kosong: ()
(not nil)              ; => T
(and 0 t)              ; => T
(or 0 nil)             ; => 0

;;; Character

#\A                    ; => #\A
#\λ                    ; => #\GREEK_SMALL_LETTER_LAMDA
#\u03BB                ; => #\GREEK_SMALL_LETTER_LAMDA

;;; String ialah array character yang tidak berubah panjang

"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; backslash ialah escape character

;;; String boleh digabungkan

(concatenate 'string "Hello, " "world!") ; => "Hello, world!"

;;; String boleh diperlakukan seperti urutan character

(elt "Apple" 0) ; => #\A

;;; FORMAT digunakan untuk output mengikut format, daripada penggubahan string
;;; yang simple sehinggalah loop dan conditional. Argument pertama untuk FORMAT
;;; menentukan ke mana string akan pergi. Jika NIL, FORMAT
;;; akan pulangkan string sebagai data string; jika T, FORMAT akan output
;;; ke standard output, biasanya di screen, kemudian pulangkan NIL.

(format nil "~A, ~A!" "Hello" "world")   ; => "Hello, world!"
(format t "~A, ~A!" "Hello" "world")     ; => NIL


;;;-----------------------------------------------------------------------------
;;; 2. Variable
;;;-----------------------------------------------------------------------------

;;; Anda boleh membuat variable global (dynamically scoped) menggunakan DEFVAR dan
;;; DEFPARAMETER. Nama variable boleh guna mana-mana character kecuali: ()",'`;#|\

;;; Beza antara DEFVAR dengan DEFPARAMETER ialah DEFVAR tidak akan ubah nilai
;;; variable jika dijalankan semula. Manakala DEFPARAMETER, akan mengubah nilai
;;; jika dijalankan semula.

;;; Kebiasaannya, variable global diletakkan earmuff (asterisk) pada nama.

(defparameter *some-var* 5)
*some-var* ; => 5

;;; Anda juga boleh menggunakan character unicode.
(defparameter *AΛB* nil)

;;; Variable yang tidak wujud boleh diakses tetapi akan menyebabkan undefined
;;; behavior. Jangan buat.

;;; Anda boleh membuat local binding menggunakan LET. Dalam snippet berikut, `me`
;;; terikat dengan "dance with you" hanya dalam (let ...). LET mesti akan pulangkan
;;; nilai `form` yang paling terakhir.

(let ((me "dance with you")) me) ; => "dance with you"


;;;-----------------------------------------------------------------------------;
;;; 3. Struct dan collection
;;;-----------------------------------------------------------------------------;


;;; Struct

(defstruct dog name breed age)
(defparameter *rover*
    (make-dog :name "rover"
              :breed "collie"
              :age 5))
*rover*            ; => #S(DOG :NAME "rover" :BREED "collie" :AGE 5)
(dog-p *rover*)    ; => T
(dog-name *rover*) ; => "rover"

;;; DOG-P, MAKE-DOG, dan DOG-NAME semuanya dibuat oleh DEFSTRUCT secara automatik


;;; Pair

;;; CONS membuat pair. CAR dan CDR pulangkan head (kepala) dan tail (ekor) CONS-pair.

(cons 'SUBJECT 'VERB)         ; => '(SUBJECT . VERB)
(car (cons 'SUBJECT 'VERB))   ; => SUBJECT
(cdr (cons 'SUBJECT 'VERB))   ; => VERB


;;; List

;;; List ialah data structure linked-list, dihasilkan daripada pair CONS dan
;;; berakhir dengan NIL (atau '()) menandakan akhirnya list tersebut

(cons 1 (cons 2 (cons 3 nil)))     ; => '(1 2 3)

;;; LIST ialah constructor untuk memudahkan penghasilan list

(list 1 2 3)                       ; => '(1 2 3)

;;; Apabila argument pertama untuk CONS ialah atom dan argument kedua ialah
;;; list, CONS akan pulangkan CONS-pair baru dengan argument pertama sebagai
;;; item pertama dan argument kedua sebagai CONS-pair yang lain

(cons 4 '(1 2 3))                  ; => '(4 1 2 3)

;;; Gunakan APPEND untuk menggabungkan list

(append '(1 2) '(3 4))             ; => '(1 2 3 4)

;;; Atau CONCATENATE

(concatenate 'list '(1 2) '(3 4))  ; => '(1 2 3 4)

;;; List ialah type utama, jadi ada pelbagai function untuk mengendalikan
;;; list, contohnya:

(mapcar #'1+ '(1 2 3))             ; => '(2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => '(11 22 33)
(remove-if-not #'evenp '(1 2 3 4)) ; => '(2 4)
(every #'evenp '(1 2 3 4))         ; => NIL
(some #'oddp '(1 2 3 4))           ; => T
(butlast '(subject verb object))   ; => (SUBJECT VERB)


;;; Vector

;;; Vector ialah array yang tidak berubah panjang

#(1 2 3) ; => #(1 2 3)

;;; Gunakan CONCATENATE untuk menggabungkan vector

(concatenate 'vector #(1 2 3) #(4 5 6)) ; => #(1 2 3 4 5 6)


;;; Array

;;; Vector dan string adalah sejenis array.

;;; 2D array

(make-array (list 2 2))         ; => #2A((0 0) (0 0))
(make-array '(2 2))             ; => #2A((0 0) (0 0))
(make-array (list 2 2 2))       ; => #3A(((0 0) (0 0)) ((0 0) (0 0)))

;;; Perhatian: nilai awal MAKE-ARRAY adalah bergantung kepada jenis Common Lisp.
;;; Untuk meletakkan nilai awal secara manual:

(make-array '(2) :initial-element 'unset)  ; => #(UNSET UNSET)

;;; Untuk mengakses element di kedudukan 1, 1, 1:

(aref (make-array (list 2 2 2)) 1 1 1)     ;  => 0


;;; Adjustable vector (vector yang boleh berubah)

;;; Adjustable vector mempunyai rupa yang sama dengan
;;; vector yang tidak berubah panjang.

(defparameter *adjvec* (make-array '(3) :initial-contents '(1 2 3)
                                   :adjustable t :fill-pointer t))
*adjvec* ; => #(1 2 3)

;;; Tambah element baru

(vector-push-extend 4 *adjvec*)   ; => 3
*adjvec*                          ; => #(1 2 3 4)


;;; Set hanyalah list:

(set-difference '(1 2 3 4) '(4 5 6 7))   ; => (3 2 1)
(intersection '(1 2 3 4) '(4 5 6 7))     ; => 4
(union '(1 2 3 4) '(4 5 6 7))            ; => (3 2 1 4 5 6 7)
(adjoin 4 '(1 2 3 4))                    ; => (1 2 3 4)

;;; Tetapi, anda perlukan data structure yang lebih baik untuk digunakan dengan
;;; data set yang sangat banyak

;;; Kamus dibuat menggunakan hash table.

;;; Bina hash table

(defparameter *m* (make-hash-table))

;;; Tetapkan nilai

(setf (gethash 'a *m*) 1)

;;; Baca nilai

(gethash 'a *m*) ; => 1, T

;;; CL boleh memulangkan beberapa nilai (multiple value).

(values 1 2) ; => 1, 2

;;; dan boleh digunakan dengan MULTIPLE-VALUE-BIND untuk bind setiap nilai

(multiple-value-bind (x y)
    (values 1 2)
  (list y x))

; => '(2 1)

;;; GETHASH antara contoh function yang memulangkan multiple value. Value
;;; pertama ialah nilai untuk key dalam hash table; jika key tidak
;;; jumpa GETHASH akan pulangkan NIL.

;;; Value kedua menentukan sama ada key tersebut betul-betul wujud dalam hash
;;; table. Jika key tidak jumpa dalam table value tersebut ialah NIL. Cara ini
;;; membolehkan kita untuk periksa sama ada value untuk key ialah NIL.

;;; Dapatkan value yang tidak wujud akan pulangkan nil

(gethash 'd *m*) ;=> NIL, NIL

;;; Anda boleh menentukan value default untuk key yang tidak wujud

(gethash 'd *m* :not-found) ; => :NOT-FOUND

;;; Jom lihat penggunaan multiple return value di dalam code.

(multiple-value-bind (a b)
    (gethash 'd *m*)
  (list a b))
; => (NIL NIL)

(multiple-value-bind (a b)
    (gethash 'a *m*)
  (list a b))
; => (1 T)


;;;-----------------------------------------------------------------------------
;;; 3. Function
;;;-----------------------------------------------------------------------------

;;; Gunakan LAMBDA untuk membuat anonymous function. Function sentiasa memulangkan
;;; value untuk expression terakhir.

(lambda () "Hello World") ; => #<FUNCTION (LAMBDA ()) {1004E7818B}>

;;; Gunakan FUNCALL untuk memanggil anonymous function

(funcall (lambda () "Hello World"))   ; => "Hello World"
(funcall #'+ 1 2 3)                   ; => 6

;;; Panggilan kepada FUNCALL juga boleh terjadi apabila lambda tersebut ialah CAR
;;; (yang pertama) untuk list (yang tidak mempunyai tanda petikan)

((lambda () "Hello World"))           ; => "Hello World"
((lambda (val) val) "Hello World")    ; => "Hello World"

;;; FUNCALL digunakan apabila argument sudah diketahui. Jika tidak, gunakan APPLY

(apply #'+ '(1 2 3))   ; => 6
(apply (lambda () "Hello World") nil) ; => "Hello World"

;;; Untuk menamakan sebuah function, guna DEFUN

(defun hello-world () "Hello World")
(hello-world) ; => "Hello World"

;;; Simbol () di atas bermaksud list kepada argument

(defun hello (name) (format nil "Hello, ~A" name))
(hello "Steve") ; => "Hello, Steve"

;;; Function boleh ada argument optional (tidak wajib); argument tersebut bernilai
;;; NIL secara default

(defun hello (name &optional from)
  (if from
      (format t "Hello, ~A, from ~A" name from)
      (format t "Hello, ~A" name)))

(hello "Jim" "Alpacas")       ; => Hello, Jim, from Alpacas

;;; Nilai default boleh ditetapkan untuk argument tersebut

(defun hello (name &optional (from "The world"))
   (format nil "Hello, ~A, from ~A" name from))

(hello "Steve")               ; => Hello, Steve, from The world
(hello "Steve" "the alpacas") ; => Hello, Steve, from the alpacas

;;; Function juga mempunyai keyword argument untuk membolehkan argument diletakkan
;;; tidak mengikut kedudukan

(defun generalized-greeter (name &key (from "the world") (honorific "Mx"))
  (format t "Hello, ~A ~A, from ~A" honorific name from))

(generalized-greeter "Jim")
; => Hello, Mx Jim, from the world

(generalized-greeter "Jim" :from "the alpacas you met last summer" :honorific "Mr")
; => Hello, Mr Jim, from the alpacas you met last summer


;;;-----------------------------------------------------------------------------
;;; 4. Kesamaan
;;;-----------------------------------------------------------------------------

;;; CL mempunyai sistem kesaksamaan yang canggih. Antaranya adalah seperti berikut.

;;; Untuk nombor, guna `='
(= 3 3.0)               ; => T
(= 2 1)                 ; => NIL

;;; Untuk identiti object (lebih kurang) guna EQL
(eql 3 3)               ; => T
(eql 3 3.0)             ; => NIL
(eql (list 3) (list 3)) ; => NIL

;;; untuk list, string, dan bit-vector, guna EQUAL
(equal (list 'a 'b) (list 'a 'b)) ; => T
(equal (list 'a 'b) (list 'b 'a)) ; => NIL


;;;-----------------------------------------------------------------------------
;;; 5. Control Flow
;;;-----------------------------------------------------------------------------

;;; Conditional (syarat)

(if t                ; test expression
    "this is true"   ; then expression
    "this is false") ; else expression
; => "this is true"

;;; Dalam conditional, semua value yang bukan NIL ialah true

(member 'Groucho '(Harpo Groucho Zeppo)) ; => '(GROUCHO ZEPPO)
(if (member 'Groucho '(Harpo Groucho Zeppo))
    'yep
    'nope)
; => 'YEP

;;; Guna COND untuk meletakkan beberapa test
(cond ((> 2 2) (error "wrong!"))
      ((< 2 2) (error "wrong again!"))
      (t 'ok)) ; => 'OK

;;; TYPECASE adalah seperti switch tetapi untuk data type value tersebut
(typecase 1
  (string :string)
  (integer :int))
; => :int


;;; Loop

;;; Recursion

(defun fact (n)
  (if (< n 2)
      1
    (* n (fact(- n 1)))))

(fact 5) ; => 120

;;; Iteration

(defun fact (n)
  (loop :for result = 1 :then (* result i)
     :for i :from 2 :to n
     :finally (return result)))

(fact 5) ; => 120

(loop :for x :across "abc" :collect x)
; => (#\a #\b #\c #\d)

(dolist (i '(1 2 3 4))
  (format t "~A" i))
; => 1234


;;;-----------------------------------------------------------------------------
;;; 6. Mutation
;;;-----------------------------------------------------------------------------

;;; Guna SETF untuk meletakkan nilai baru untuk variable yang sedia ada. Ini sama
;;; seperti contoh hash table di atas.

(let ((variable 10))
    (setf variable 2))
; => 2

;;; Sebaik-baiknya kurangkan penggunaan destructive function dan elakkan
;;; mutation jika boleh.


;;;-----------------------------------------------------------------------------
;;; 7. Class dan object
;;;-----------------------------------------------------------------------------

;;; Takde dah class untuk haiwan. Jom buat Human-Powered Mechanical
;;; Conveyances (Kenderaan Mekanikal Berkuasa Manusia).

(defclass human-powered-conveyance ()
  ((velocity
    :accessor velocity
    :initarg :velocity)
   (average-efficiency
    :accessor average-efficiency
   :initarg :average-efficiency))
  (:documentation "A human powered conveyance"))

;;; Argument untuk DEFCLASS, mengikut susunan ialah:
;;; 1. nama class
;;; 2. list untuk superclass
;;; 3. list untuk slot
;;; 4. specifier optional (tidak wajib)

;;; Apabile list untuk superclass tidak ditetapkan, list yang kosong bermaksud
;;; class standard-object. Ini *boleh* ditukar, kalau anda tahu apa yang anda buat.
;;; Baca Art of the Metaobject Protocol untuk maklumat lebih lanjut.

(defclass bicycle (human-powered-conveyance)
  ((wheel-size
    :accessor wheel-size
    :initarg :wheel-size
    :documentation "Diameter of the wheel.")
   (height
    :accessor height
    :initarg :height)))

(defclass recumbent (bicycle)
  ((chain-type
    :accessor chain-type
    :initarg :chain-type)))

(defclass unicycle (human-powered-conveyance) nil)

(defclass canoe (human-powered-conveyance)
  ((number-of-rowers
    :accessor number-of-rowers
    :initarg :number-of-rowers)))

;;; Panggilan DESCRIBE kepada class HUMAN-POWERED-CONVEYANCE di REPL akan memberi:

(describe 'human-powered-conveyance)

; COMMON-LISP-USER::HUMAN-POWERED-CONVEYANCE
;  [symbol]
;
; HUMAN-POWERED-CONVEYANCE names the standard-class #<STANDARD-CLASS
;                                                    HUMAN-POWERED-CONVEYANCE>:
;  Documentation:
;    A human powered conveyance
;  Direct superclasses: STANDARD-OBJECT
;  Direct subclasses: UNICYCLE, BICYCLE, CANOE
;  Not yet finalized.
;  Direct slots:
;    VELOCITY
;      Readers: VELOCITY
;      Writers: (SETF VELOCITY)
;    AVERAGE-EFFICIENCY
;      Readers: AVERAGE-EFFICIENCY
;      Writers: (SETF AVERAGE-EFFICIENCY)

;;; Perhatikan apa yang berlaku. CL memang direka sebagai sistem interaktif.

;;; Untuk membuat method, jom kira berapa panjang lilitan untuk
;;; roda basikal menggunakan formula: C = d * pi

(defmethod circumference ((object bicycle))
  (* pi (wheel-size object)))

;;; Nilai PI memang sudah ada dalam CL

;;; Katakanlah kita ingin ambil tahu efficiency value (nilai keberkesanan)
;;; rower (pendayung) di dalam canoe (perahu) adalah berbentuk logarithmic. Ini
;;; boleh ditetapkan di dalam constructor/initializer.

;;; Untuk initialize instance selepas CL sudah siap construct:

(defmethod initialize-instance :after ((object canoe) &rest args)
  (setf (average-efficiency object)  (log (1+ (number-of-rowers object)))))

;;; Kemudian untuk construct sesebuah instance dan periksa purata efficiency...

(average-efficiency (make-instance 'canoe :number-of-rowers 15))
; => 2.7725887


;;;-----------------------------------------------------------------------------
;;; 8. Macro
;;;-----------------------------------------------------------------------------

;;; Macro membolehkan anda untuk menambah syntax language. CL tidak ada
;;; WHILE loop, tetapi, kita boleh mencipta syntax ter. Jika kita buat menggunakan
;;; naluri, kita akan dapat:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
    (let ((block-name (gensym)) (done (gensym)))
        `(tagbody
           ,block-name
           (unless ,condition
               (go ,done))
           (progn
           ,@body)
           (go ,block-name)
           ,done)))

;;; Jom lihat versi yang lebih high-level:

(defmacro while (condition &body body)
    "While `condition` is true, `body` is executed.
`condition` is tested prior to each execution of `body`"
  `(loop while ,condition
         do
         (progn
            ,@body)))

;;; Namun, dengan compiler yang modern, cara ini tidak diperlukan; form LOOP
;;; compile sama sahaja dan juga mudah dibaca.

;;; Perhatikan ``` digunakan, sama juga `,` dan `@`. ``` ialah operator jenis quote
;;; yang dipanggil quasiquote; operator tersebut membolehkan penggunaan `,` .
;;; `,` membolehkan variable "di-unquote-kan". @ mengembangkan list.

;;; GENSYM membuat simbol unik yang pasti tidak wujud di tempat-tempat yang
;;; lain. Ini kerana macro dikembangkan semasa compile dan
;;; nama variable di dalam macro boleh bertembung dengan nama variable yang
;;; digunakan dalam code yang biasa.

;;; Baca Practical Common Lisp dan On Lisp untuk maklumat lebih lanjut mengenai macro.
```


## Bacaan lanjut

- [Practical Common Lisp](http://www.gigamonkeys.com/book/)
- [Common Lisp: A Gentle Introduction to Symbolic Computation](https://www.cs.cmu.edu/~dst/LispBook/book.pdf)


## Maklumat tambahan

- [CLiki](http://www.cliki.net/)
- [common-lisp.net](https://common-lisp.net/)
- [Awesome Common Lisp](https://github.com/CodyReichert/awesome-cl)
- [Lisp Lang](http://lisp-lang.org/)


## Kredit

Terima kasih banyak diucapkan kepada ahli Scheme yang membuat permulaan yang sangat
bagus dan mudah untuk diguna pakai untuk Common Lisp.

- [Paul Khuong](https://github.com/pkhuong) untuk review yang bagus.
