---
language: elisp
contributors:
    - ["Bastien Guerry", "https://bzg.fr"]
    - ["Saurabh Sandav", "http://github.com/SaurabhSandav"]
translators:
    - ["Burhanuddin Baharuddin", "https://github.com/burhanloey"]
lang: ms-my
filename: learn-emacs-lisp-ms.el
---

```scheme
;; Ini adalah pengenalan kepada Emacs Lisp dalam masa 15 minit (v0.2d)
;;
;; Mula-mula pastikan anda sudah membaca artikel daripada Peter Norvig ini:
;; http://norvig.com/21-days.html
;;
;; Kemudian install GNU Emacs 24.3:
;;
;; Debian: apt-get install emacs (atau lihat arahan untuk distro anda)
;; OSX: http://emacsformacosx.com/emacs-builds/Emacs-24.3-universal-10.6.8.dmg
;; Windows: http://ftp.gnu.org/gnu/windows/emacs/emacs-24.3-bin-i386.zip
;;
;; Maklumat lanjut boleh didapati di:
;; http://www.gnu.org/software/emacs/#Obtaining

;; Amaran penting:
;;
;; Tutorial ini tidak akan merosakkan komputer anda melainkan jika anda berasa
;; terlalu marah sehingga anda menghempap komputer anda ke lantai. Kalau begitu,
;; saya dengan ini tidak akan bertanggungjawab terhadap apa-apa. Berseronoklah ya!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buka Emacs.
;;
;; Tekan `q' untuk tutup mesej selamat datang.
;;
;; Sekarang lihat garis kelabu di bahagian bawah window:
;;
;; "*scratch*" ialah nama ruangan untuk anda edit.
;; Ruangan ini disebut sebagai "buffer".
;;
;; Buffer scratch ialah buffer yang default setiap kali Emacs dibuka.
;; Anda bukannya edit file: anda edit buffer yang kemudiannya
;; boleh save ke file.
;;
;; "Lisp interaction (interaksi)" merujuk kepada command yang wujud di sini.
;;
;; Emacs mempunyai beberapa command yang sedia ada dalam setiap buffer,
;; dan sesetengah command yang lain boleh didapati jika sesetengah mode
;; diaktifkan.  Di sini kita menggunakan `lisp-interaction-mode', yang
;; mempunyai command untuk menjalankan dan mengendalikan code Elisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Semicolon akan menjadikan comment sepanjang baris tersebut.
;;
;; Program Elisp mengandungi symbolic expressions ("sexps"):
(+ 2 2)

;; Symbolic expression di atas dibaca begini "Tambah 2 pada 2".

;; Sexps dilitupi oleh parentheses, dan boleh dalam bentuk nested (parentheses
;; dalam parentheses):
(+ 2 (+ 1 1))

;; Symbolic expression mengandungi atom atau symbolic expression
;; yang lain.  Untuk contoh di atas, 1 dan 2 ialah atom,
;; (+ 2 (+ 1 1)) dan (+ 1 1) ialah symbolic expression.

;; Dengan menggunakan `lisp-interaction-mode', anda boleh evaluate
;; (mendapatkan hasil pengiraan) sexps. Letak cursor selepas parenthesis penutup
;; kemudian tekan control dan j ("C-j").

(+ 3 (+ 1 2))
;;           ^ cursor di sini
;; `C-j' => 6

;; `C-j' memasukkan jawapan pengiraan ke dalam buffer.

;; `C-xC-e' memaparkan jawapan yang sama di bahagian bawah Emacs,
;; yang dipanggil "minibuffer".  Secara umumnya kita akan menggunakan `C-xC-e',
;; sebab kita tidak mahu memenuhi buffer dengan teks yang tidak penting.

;; `setq' menyimpan value ke dalam variable:
(setq my-name "Bastien")
;; `C-xC-e' => "Bastien" (terpapar di mini-buffer)

;; `insert' akan memasukkan "Hello!" di tempat di mana cursor berada:
(insert "Hello!")
;; `C-xC-e' => "Hello!"

;; Di atas, kita menggunakan `insert' dengan satu argument "Hello!", tetapi
;; kita boleh meletakkan beberapa argument -- di sini kita letak dua:

(insert "Hello" " world!")
;; `C-xC-e' => "Hello world!"

;; Anda boleh menggunakan variable selain string:
(insert "Hello, I am " my-name)
;; `C-xC-e' => "Hello, I am Bastien"

;; Anda boleh menggabungkan sexps untuk membuat function:
(defun hello () (insert "Hello, I am " my-name))
;; `C-xC-e' => hello

;; Anda boleh evaluate function:
(hello)
;; `C-xC-e' => Hello, I am Bastien

;; Parentheses kosong di dalam function bermaksud function tersebut tidak
;; terima argument.  Sekarang kita tukar function untuk menerima satu argument.
;; Di sini, argument tersebut dinamakan "name":

(defun hello (name) (insert "Hello " name))
;; `C-xC-e' => hello

;; Sekarang panggil function tersebut dengan string "you" sebagai value
;; untuk argument:
(hello "you")
;; `C-xC-e' => "Hello you"

;; Yay!

;; Tarik nafas.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sekarang tukar ke buffer baru dengan nama "*test*" di window yang lain:

(switch-to-buffer-other-window "*test*")
;; `C-xC-e'
;; => [The screen has two windows and cursor is in the *test* buffer]

;; Gerakkan mouse ke window atas dan klik kiri untuk pergi balik ke buffer scratch.
;; Cara lain adalah dengan menggunakan `C-xo' (i.e. tekan control-x kemudian
;; tekan o) untuk pergi ke window yang lain.

;; Anda boleh menggabungkan beberapa sexps menggunakan `progn':
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))
;; `C-xC-e'
;; => [The screen has two windows and cursor is in the *test* buffer]

;; Mulai dari sekarang saya tidak akan beritahu anda untuk tekan `C-xC-e' lagi:
;; buat untuk setiap sexp yang akan datang.

;; Pergi balik ke buffer *scratch* menggunakan mouse atau `C-xo'.

;; Seelok-eloknya padam buffer tersebut:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))

;; Atau pergi balik ke window lain:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))

;; Anda boleh menetapkan value dengan local variable menggunakan `let':
(let ((local-name "you"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; Tidak perlu menggunakan `progn', sebab `let' juga menggabungkan
;; beberapa sexps.

;; Jom format string:
(format "Hello %s!\n" "visitor")

;; %s ialah tempat untuk meletakkan string, digantikan dengan "visitor".
;; \n ialah character untuk membuat baris baru.

;; Jom tukar function kita menggunakan format:
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; Jom buat function lain menggunakan `let':
(defun greeting (name)
  (let ((your-name "Bastien"))
    (insert (format "Hello %s!\n\nI am %s."
                    name       ; argument untuk function
                    your-name  ; variable "Bastien" daripada let
                    ))))

;; Kemudian evaluate:
(greeting "you")

;; Sesetengah function adalah interaktif:
(read-from-minibuffer "Enter your name: ")

;; Function tersebut akan memulangkan kembali apa yang anda masukkan ke prompt.

;; Jom jadikan function `greeting' untuk prompt nama anda:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (insert (format "Hello!\n\nI am %s and you are %s."
                    from-name ; argument untuk function
                    your-name ; variable daripada let, yang dimasukkan dari prompt
                    ))))

(greeting "Bastien")

;; Jom siapkan function dengan memaparkan result di window yang lain:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello %s!\n\nI am %s." your-name from-name))
    (other-window 1)))

;; Test function tersebut:
(greeting "Bastien")

;; Tarik nafas.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jom simpan senarai nama:
;; Jika anda ingin membuat list(senarai) data, guna ' untuk elak
;; daripada list tersebut evaluate.
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; Dapatkan elemen pertama daripada list menggunakan `car':
(car list-of-names)

;; Dapatkan semua elemen kecuali yang pertama menggunakan `cdr':
(cdr list-of-names)

;; Tambah elemen di awal list menggunakan `push':
(push "Stephanie" list-of-names)

;; NOTA: `car' dan `cdr' tidak ubah suai list, tetapi `push' ya.
;; Perbezaan ini penting: sesetengah function tiada side-effects (kesan sampingan)
;; (seperti `car') dan yang lain ada side-effect (seperti `push').

;; Jom panggil `hello' untuk setiap elemen dalam `list-of-names':
(mapcar 'hello list-of-names)

;; Tukar `greeting' supaya ucapkan hello kepada semua orang dalam `list-of-names':
(defun greeting ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (mapcar 'hello list-of-names)
    (other-window 1))

(greeting)

;; Ingat lagi function `hello' di atas? Function tersebut mengambil satu
;; argument, iaitu nama.  `mapcar' memanggil `hello', kemudian menggunakan setiap
;; nama dalam `list-of-names' sebagai argument untuk function `hello'.

;; Sekarang kita susun sedikit untuk apa yang terpapar di buffer:

(defun replace-hello-by-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (search-forward "Hello")
      (replace-match "Bonjour"))
    (other-window 1))

;; (goto-char (point-min)) akan pergi ke permulaan buffer.
;; (search-forward "Hello") akan mencari string "Hello".
;; (while x y) evaluate sexp(s) y selagi x masih pulangkan sesuatu.
;; Jika x pulangkan `nil', kita akan keluar daripada while loop.

(replace-hello-by-bonjour)

;; Anda akan dapat melihat semua "Hello" dalam buffer *test*
;; ditukarkan dengan "Bonjour".

;; Anda juga akan dapat error: "Search failed: Hello".
;;
;; Bagi mengelakkan error tersebut, anda perlu beritahu `search-forward' sama ada
;; perlu berhenti mencari pada suatu ketika, dan sama ada perlu diam jika
;; tidak jumpa apa yang dicari:

;; (search-forward "Hello" nil 't) selesai masalah:

;; Argument `nil' cakap: carian tidak mengikut kedudukan.
;; Argument `'t'  cakap: diam saja jika tidak jumpa apa yang dicari.

;; Kita guna sexp ini di function berikut, barulah tidak keluar error:

(defun hello-to-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    ;; Ucap hello pada nama-nama dalam `list-of-names'
    (mapcar 'hello list-of-names)
    (goto-char (point-min))
    ;; Ganti "Hello" dengan "Bonjour"
    (while (search-forward "Hello" nil 't)
      (replace-match "Bonjour"))
    (other-window 1))

(hello-to-bonjour)

;; Jom jadikan nama-nama tersebut bold:

(defun boldify-names ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (re-search-forward "Bonjour \\(.+\\)!" nil 't)
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold)))
    (other-window 1))

;; Function ini memperkenalkan `re-search-forward': anda mencari menggunakan
;; pattern iaitu "regular expression", bukannya mencari string "Bonjour".

;; Regular expression tersebut ialah "Bonjour \\(.+\\)!" dan dibaca begini:
;; string "Bonjour ", dan
;; kumpulan                | ini ialah \\( ... \\)
;;   mana-mana character   | ini ialah .
;;   yang boleh berulang   | ini ialah +
;; dan string "!".

;; Dah sedia?  Test function tersebut!

(boldify-names)

;; `add-text-properties' tambah... ciri-ciri teks, seperti face.

;; OK, kita sudah selesai.  Selamat ber-hacking!

;; Jika anda ingin tahu lebih mengenai variable atau function:
;;
;; C-h v a-variable RET
;; C-h f a-function RET
;;
;; Jika anda ingin membaca manual Emacs Lisp menggunakan Emacs:
;;
;; C-h i m elisp RET
;;
;; Jika ingin membaca pengenalan kepada Emacs Lisp secara online:
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html
```
