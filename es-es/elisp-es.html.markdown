---
language: elisp
contributors:
    - ["Bastien Guerry", "http://bzg.fr"]
translators:
    - ["Guillermo Vayá", "http://willyfrog.es"]
lang: es-es
filename: learn-emacs-lisp-es.el
---

```scheme
;; Introduccion a Emacs Lisp en 15 minutos (v0.2d)
;;
;; Autor: Bastien / @bzg2 / http://bzg.fr
;; Traducción: Guillermo Vayá / @Driadan / http://willyfrog.es
;;
;; Antes de nada, lee este texto de Peter Norvig:
;; Traducido: http://loro.sourceforge.net/notes/21-dias.html
;; Original: http://norvig.com/21-days.html
;;
;; Ahora instala GNU Emacs 24.3:
;;
;; Debian: apt-get install emacs 
;; (o sigue las instrucciones de tu distribución preferida)
;; OSX: http://emacsformacosx.com/emacs-builds/Emacs-24.3-universal-10.6.8.dmg
;; Windows: http://ftp.gnu.org/gnu/windows/emacs/emacs-24.3-bin-i386.zip
;;
;; Puedes encontrar información general sobre Emacs en:
;; http://www.gnu.org/software/emacs/#Obtaining

;; Aviso importante:
;;
;; Seguir este tutorial no provocará daños en tu ordenador a menos que
;; te enfades tanto que que acabes tirándolo al suelo. En tal caso 
;; declino cualquier responsabilidad. ¡A divertirse!


;; "N. del. T.": Algunos términos comunes de la informática se han dejado 
;; sin traducir ya que es mucho más probable que el lector los conozca en 
;; su forma en inglés, siendo la versión en español de muy raro uso.
;; Además "sexps" se ha decidido traducir por sexpresión.
;; Por último, añadir que no se han traducido los ejemplos de código ya que no 
;; es necesario entender qué dice el string para comprender el funcionamiento
;; y podría llevar a error.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Inicia Emacs.
;;
;; Pulsa la tecla `q' para pasar el mensaje de bienvenida.
;;
;; Mira a la línea gris en la parte inferior de la ventana:
;;
;; "*scratch*" es el nombre del espacio editable donde estás.
;; A este espacio editable se le llama "buffer".
;;
;; Scratch es el buffer por defecto cuando abres Emacs.
;; En Emacs nunca editas ficheros, sino que editas buffers que 
;; posteriormente pueden grabarse a un fichero.
;; can save to a file.
;; 
;; "Lisp interaction" indica el conjunto de ordenes disponibles.
;; 
;; Emacs dispone de un set de comandos disponibles en cualquier buffer
;; ("built-ins") y aparte varios conjuntos de ordenes disponibles 
;; según el modo específico que esté activo. En nuestro caso 
;; estamos usando `lisp-interaction-mode', el cual incluye las
;; ordenes necesarias para evaluar y navegar código Elisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Un punto y coma comienza un comentario. Pueden ponerse en cualquier 
;; posicion de la linea.
;;
;; Los programas en Elisp se componen de expresiones simbólicas
;; tambien llamadas "sexps":
(+ 2 2)

;; Esta expresión simbólica se lee tal que "Suma 2 y 2"

;; Las sexpresiones se rodean por paréntesis, y pueden anidarse:
(+ 2 (+ 1 1))

;; Una expresion simbólica está formada bien por átomos o bien por otras 
;; expresiones simbólicas. En el ejemplo de arriba, 1 y 2 son átomos, 
;; mientras que (+ 2 (+ 1 1)) y (+ 1 1) son expresiones simbólicas.

;; Gracias a `lisp-interaction-mode' puedes evaluar las sexpresiones.
;; Coloca el cursor justo despues del paréntesis de cierre y 
;; mantén pulsada la tecla Control y la j (para abreviar usaremos "C-j").

(+ 3 (+ 1 2))
;;           ^ pon aquí el cursor
;; `C-j' => 6

;; `C-j' añade el resultado de la evaluación al buffer.

;; `C-xC-e' muestra el mismo resultado pero en la linea inferior
;; la cual se llama "minibuffer".  Este será el metodo que usaremos 
;; normalmente para no llenar el buffer con texto inútil.

;; `setq' guarda un valor en una variable:
(setq my-name "Bastien")
;; `C-xC-e' => "Bastien" (aparece en el mini-buffer)

;; `insert' añade "Hello!" en el punto donde esté tu cursor:
(insert "Hello!")
;; `C-xC-e' => "Hello!"

;; Aunque hemos usado `insert' con solo un parámetro "Hello!", se
;; pueden pasar más. Por ejemplo, en esta otra sexpresión usamos dos:

(insert "Hello" " world!")
;; `C-xC-e' => "Hello world!"

;; Se pueden usar variables en lugar de strings:
(insert "Hello, I am " my-name)
;; `C-xC-e' => "Hello, I am Bastien"

;; Puedes combinar sexpresiones en funciones:
(defun hello () (insert "Hello, I am " my-name))
;; `C-xC-e' => hello

;; Evaluemos la funcion:
(hello)
;; `C-xC-e' => Hello, I am Bastien

;; Los parentesis vacios en la definicion de una funcion indican
;; que no acepta parámetros. En cualquier caso, usar `my-name' siempre
;; es aburrido, asi que vamos a hacer que la función accepte un parámetro
;; (en este caso el parametro se llama "name"):
(defun hello (name) (insert "Hello " name))
;; `C-xC-e' => hello

;; Ahora vamos a llamar a la funcion con el string "you" como valor para 
;; el único parámetro que posee.
(hello "you")
;; `C-xC-e' => "Hello you"

;; ¡Genial!

;; Descansa un poco y respira.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ahora cambiaremos al nuevo buffer, llamado "*test*", en una nueva ventana.

(switch-to-buffer-other-window "*test*")
;; `C-xC-e'
;; => [La pantalla ahora tiene dos ventanas y el cursor está en el buffer *test*]

;; Mueve el ratón sobre la ventana superior y pulsa el boton izdo. para volver. 
;; Otra forma es usando `C-xo' (pulsa simultaneamente control y x y luego la o)
;; para ir a la otra ventana.

;; Se pueden combinar varias sexpresiones mediante `progn':
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))
;; `C-xC-e'
;; => [De las dos ventanas de la pantalla, el cursor está en la marcada como *test*]

;; A partir de ahora, si no te importa, dejaremos de decir que pulses `C-xC-e': 
;; tendrás que hacerlo para ejecutar cada sexpresión que siga.

;; También tendrás que volver al buffer *scratch* bien con el ratón o con `C-xo'.

;; En ocasiones será util limpiar el buffer:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))

;; O volver a la ventana anterior:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))

;; Puedes enlazar un valor a una variable local con `let':
(let ((local-name "you"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; En este caso, no hace falta añadir `progn' ya que `let' permite combinar 
;; varias sexpresiones.

;; Vamos a darle formato a un string:
(format "Hello %s!\n" "visitor")

;; Cada %s indica la posicion donde irá un string, el cual será reemplazado 
;; por "visitor". "\n" es el caracter de nueva línea.

;; Mejoremos nuestra funcion usando `format':
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; Creemos una nueva funcion que utililce `let':
(defun greeting (name)
  (let ((your-name "Bastien"))
    (insert (format "Hello %s!\n\nI am %s."
                    name       ; the argument of the function
                    your-name  ; the let-bound variable "Bastien"
                    ))))

;; Y ahora la evaluamos:
(greeting "you")

;; Algunas funciones son interactivas:
(read-from-minibuffer "Enter your name: ")

;; Al evaluar esta función, ésta devuelve lo que hayas introducido.

;; Ahora hagamos nuestra función `greeting' preguntar por tu nombre:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (insert (format "Hello!\n\nI am %s and you are %s."
                    from-name ; the argument of the function
                    your-name ; the let-bound var, entered at prompt
                    ))))

(greeting "Bastien")

;; Y ahora la completamos mostrando el resultado en la otra ventana:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello %s!\n\nI am %s." your-name from-name))
    (other-window 1)))

;; Probémosla:
(greeting "Bastien")

;; Descansa un poco y respira.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Creemos una lista de nombres:
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; Para coger el primer elemento de la lista usaremos `car':
(car list-of-names)

;; Para coger todos menos el primer elemento de la lista 
;; usaremos `cdr':
(cdr list-of-names)

;; Para añadir un elemento al comienzo de la lista utilizamos `push':
(push "Stephanie" list-of-names)

;; OJO: `car' y `cdr' no modifican la lista, mientras que `push' sí.
;; ¡Es una diferencia importante! Algunas funciones no tienen efectos 
;; colaterales (como `car') mientras que otras sí (como `push').
;; "N. del T.": estos efectos colaterales se les llama `side-effects' en 
;; las distintas variantes de lisp.

;; Llamemos a `hello' con cada elemento de `list-of-names':
(mapcar 'hello list-of-names)

;; Retocamos `greeting' para que salude a todos los que estén en `list-of-names':
(defun greeting ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (mapcar 'hello list-of-names)
    (other-window 1))

(greeting)

;; ¿Te acuerdas de la función `hello' definida un poco más arriba?
;; Recibía un parámetro: `name'. Así que `mapcar' llama a `hello' con cada 
;; elemento de `list-of-names' como parámetro de `hello'.

;; Ahora ordenaremos un poco lo que tenemos en el buffer:

(defun replace-hello-by-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (search-forward "Hello")
      (replace-match "Bonjour"))
    (other-window 1))

;; (goto-char (point-min)) mueve el cursor al principio del buffer.
;; (search-forward "Hello") busca un string "Hello".
;; (while x y) evalua la/s sexpresion/es y mientras que x devuelva
;; alguna cosa.
;; En el momento que x devuelva `nil' (es decir nada), sale del 
;; bucle `while'.

(replace-hello-by-bonjour)

;; Observamos que todas las veces que teníamos la palabra "Hello" en el buffer *test*
;; han sido reemplazadas por "Bonjour".

;; Y además, hemos obtenido un error: "Search failed: Hello".
;;
;; Para evitar este error, hay que decirle a `search-forward' si debería dejar de 
;; buscar en el buffer en algún momento y si debería fallar sin quejarse cuando 
;; no encuentra nada.

;; (search-forward "Hello" nil t) justo hace eso:

;; El argumento `nil' significa que la busqueda no está ligada a ninguna posición.
;; Y el argumento `t' le pide que no diga nada si no encuentra el string.

;; Usaremos esta sexpresión en la función siguiente, la cual ya 
;; no muestra ningún error:

(defun hello-to-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    ;; Say hello to names in `list-of-names'
    (mapcar 'hello list-of-names)
    (goto-char (point-min))
    ;; Replace "Hello" by "Bonjour"
    (while (search-forward "Hello" nil t)
      (replace-match "Bonjour"))
    (other-window 1))

(hello-to-bonjour)

;; Añadamos algo de color a los nombres:

(defun boldify-names ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (re-search-forward "Bonjour \\(.+\\)!" nil t)
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold)))
    (other-window 1))

;; Esta función nos presenta `re-search-forward': en vez de 
;; buscar el string "Bonjour" exacto, se busca por un patrón
;; usando una "expresión regular" (lo cual se muestra abreviado 
;; en el prefijo "re-" del inglés "Regular Expression").

;; La expresión regular a utilizar es "Bonjour \\(.+\\)!" y se traduce como:
;; el string "Bonjour ", seguido de 
;; un grupo de           | representado por \\( ... \\)
;;   cualquier caracter  | representado por .
;;   al menos una vez    | representado por +
;; y el string "!".

;; ¿Preparado?  ¡Probemoslo!

(boldify-names)

;; `add-text-properties' añade propiedades al texto, como una fuente.

;; ¡Hale! ¡Ya lo tenemos! ¡Feliz hacking!

;; Si quieres saber más sobre una función o una variable:
;;
;; C-h v la-variable RET
;; C-h f la-funcion RET
;;
;; Si quieres leer el manual de Emacs Lisp desde dentro de Emacs:
;;
;; C-h i m elisp RET
;;
;; Para leer una introducción en linea de Emacs Lisp:
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html

;; Me gustaría agradecer a las siguientes personas su feedback y sugerencias:
;; - Wes Hardaker
;; - notbob
;; - Kevin Montuori
;; - Arne Babenhauserheide
;; - Alan Schmitt
;; - LinXitoW
;; - Aaron Meurer
```
