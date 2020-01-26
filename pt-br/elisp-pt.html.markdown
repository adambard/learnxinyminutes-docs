---
language: elisp
contributors:
    - ["Bastien Guerry", "http://bzg.fr"]
translators:
    - ["Lucas Tadeu Teixeira", "http://ltt.me"]
lang: pt-br
filename: learn-emacs-lisp-pt.el
---

```scheme
;; Introdução ao Emacs Lisp em 15 minutos (v0.2d)
;;
;; Autor: Bastien / @bzg2 / http://bzg.fr
;;
;; Antes de começar, leia este texto escrito Peter Norvig:
;; http://norvig.com/21-days.html
;;
;; Agora instale GNU Emacs 24.3:
;;
;; Debian: apt-get install emacs (ou veja as instruções da sua distribuição)
;; OSX: http://emacsformacosx.com/emacs-builds/Emacs-24.3-universal-10.6.8.dmg
;; Windows: http://ftp.gnu.org/gnu/windows/emacs/emacs-24.3-bin-i386.zip
;;
;; Informações mais gerais podem ser encontradas em:
;; http://www.gnu.org/software/emacs/#Obtaining

;; Aviso importante:
;;
;; Realizar este tutorial não danificará seu computador, a menos
;; que você fique tão irritado a ponto de jogá-lo no chão. Neste caso,
;; me abstenho de qualquer responsabilidade. Divirta-se!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Abra o Emacs.
;;
;; Aperte a tecla `q' para ocultar a mensagem de boas vindas.
;;
;; Agora olhe para a linha cinza na parte inferior da janela:
;;
;; "*scratch*" é o nome do espaço de edição em que você se encontra.
;; Este espaço de edição é chamado "buffer".
;;
;; O buffer de rascunho (i.e., "scratch") é o buffer padrão quando
;; o Emacs é aberto. Você nunca está editando arquivos: você está
;; editando buffers que você pode salvar em um arquivo.
;;
;; "Lisp interaction" refere-se a um conjunto de comandos disponíveis aqui.
;;
;; O Emacs possui um conjunto de comandos embutidos (disponíveis em
;; qualquer buffer) e vários subconjuntos de comandos disponíveis
;; quando você ativa um modo específico. Aqui nós utilizamos
;; `lisp-interaction-mode', que possui comandos para interpretar e navegar
;; em código Elisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pontos e vírgulas iniciam comentários em qualquer parte de uma linha.
;;
;; Programas codificados em Elisp são compostos por expressões simbólicas
;; (conhecidas também por "sexps"):
(+ 2 2)

;; Esta expressão simbólica significa "Some 2 e 2".

;; "Sexps" são envoltas em parêntese, possivelmente aninhados:
(+ 2 (+ 1 1))

;; Uma expressão simbólica contém átomos ou outras expressões
;; simbólicas. Nos exemplos acima, 1 e 2 são átomos;
;; (+ 2 (+ 1 1)) e (+ 1 1) são expressões simbólicas.

;; No modo `lisp-interaction-mode' você pode interpretar "sexps".
;; Posicione o cursor logo após o parêntese de fechamento e,
;; então, segure apertado Ctrl e aperte a tecla j ("C-j", em resumo).

(+ 3 (+ 1 2))
;;           ^ posicione o cursor aqui
;; `C-j' => 6

;; `C-j' insere o resultado da interpretação da expressão no buffer.

;; `C-xC-e' exibe o mesmo resultado na linha inferior do Emacs,
;; chamada de "mini-buffer".  Nós geralmente utilizaremos `C-xC-e',
;; já que não queremos poluir o buffer com texto desnecessário.

;; `setq' armazena um valor em uma variável:
(setq my-name "Bastien")
;; `C-xC-e' => "Bastien" (texto exibido no mini-buffer)

;; `insert' insere "Hello!" na posição em que se encontra seu cursor:
(insert "Hello!")
;; `C-xC-e' => "Hello!"

;; Nós executamos `insert' com apenas um argumento ("Hello!"), mas
;; mais argumentos podem ser passados --  aqui utilizamos dois:

(insert "Hello" " world!")
;; `C-xC-e' => "Hello world!"

;; Você pode utilizar variávies no lugar de strings:
(insert "Hello, I am " my-name)
;; `C-xC-e' => "Hello, I am Bastien"

;; Você pode combinar "sexps" em funções:
(defun hello () (insert "Hello, I am " my-name))
;; `C-xC-e' => hello

;; Você pode interpretar chamadas de funções:
(hello)
;; `C-xC-e' => Hello, I am Bastien

;; Os parênteses vazios na definição da função significam que ela
;; não aceita argumentos. Mas sempre utilizar `my-name' é um tédio!
;; Vamos dizer à função para aceitar um argumento (o argumento é
;; chamado "name"):

(defun hello (name) (insert "Hello " name))
;; `C-xC-e' => hello

;; Agora vamos executar a função com a string "you" como o valor
;; para seu único parâmetro:
(hello "you")
;; `C-xC-e' => "Hello you"

;; Aí sim!

;; Respire um pouco.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Agora mude para um novo buffer chamado "*test*":

(switch-to-buffer-other-window "*test*")
;; `C-xC-e'
;; => [a tela exibirá duas janelas e o cursor estará no buffer *test*]

;; Posicione o mouse sobre a janela superior e clique com o botão
;; esquerdo para voltar. Ou você pode utilizar `C-xo' (i.e. segure
;; ctrl-x e aperte o) para voltar para a outra janela, de forma interativa.

;; Você pode combinar várias "sexps" com `progn':
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))
;; `C-xC-e'
;; => [A tela exibirá duas janelas e o cursor estará no buffer *test*]

;; Agora, se você não se importar, pararei de pedir que você aperte
;; `C-xC-e': faça isso para cada "sexp" que escrevermos.

;; Sempre volte para o buffer *scratch* com o mouse ou `C-xo'.

;; Frequentemente, é útil apagar o conteúdo do buffer:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))

;; Ou voltar para a outra janela:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))

;; Você pode armazenar um valor em uma variável local utilizando `let':
(let ((local-name "you"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; Neste caso, não é necessário utilizar `progn' já que `let' combina
;; várias "sexps".

;; Vamos formatar uma string:
(format "Hello %s!\n" "visitor")

;; %s é um espaço reservado para uma string, substituído por "visitor".
;; \n é um caractere de nova linha.

;; Vamos refinar nossa função utilizando `format':
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; Vamos criar outra função que utilize `let':
(defun greeting (name)
  (let ((your-name "Bastien"))
    (insert (format "Hello %s!\n\nI am %s."
                    name       ; the argument of the function
                    your-name  ; the let-bound variable "Bastien"
                    ))))

;; E executá-la:
(greeting "you")

;; Algumas funções são interativas:
(read-from-minibuffer "Enter your name: ")

;; Ao ser interpretada, esta função retorna o que você digitou no prompt.

;; Vamos fazer nossa função `greeting' pedir pelo seu nome:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (insert (format "Hello!\n\nI am %s and you are %s."
                    from-name ; the argument of the function
                    your-name ; the let-bound var, entered at prompt
                    ))))

(greeting "Bastien")

;; Vamos finalizá-la fazendo-a exibir os resultados em outra janela:
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello %s!\n\nI am %s." your-name from-name))
    (other-window 1)))

;; Agora teste-a:
(greeting "Bastien")

;; Respire um pouco.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vamos armazenar uma lista de nomes:
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; Pegue o primeiro elemento desta lista utilizando `car':
(car list-of-names)

;; Pegue uma lista de todos os elementos, exceto o primeiro, utilizando
;; `cdr':
(cdr list-of-names)

;; Adicione um elemento ao início da lista com `push':
(push "Stephanie" list-of-names)

;; NOTA: `car' e `cdr' não modificam a lista, `push' sim.
;; Esta é uma diferença importante: algumas funções não têm qualquer
;; efeito colateral (como `car'), enquanto outras sim (como `push').

;; Vamos executar `hello' para cada elemento em `list-of-names':
(mapcar 'hello list-of-names)

;; Refine `greeting' para saudar todos os nomes em `list-of-names':
(defun greeting ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (mapcar 'hello list-of-names)
    (other-window 1))

(greeting)

;; Você se lembra da função `hello' que nós definimos lá em cima? Ela
;; recebe um argumento, um nome. `mapcar' executa `hello', sucessivamente,
;; utilizando cada elemento de `list-of-names' como argumento para `hello'.

;; Agora vamos arrumar, um pouco, o que nós temos escrito no buffer:

(defun replace-hello-by-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (search-forward "Hello")
      (replace-match "Bonjour"))
    (other-window 1))

;; (goto-char (point-min)) vai para o início do buffer.
;; (search-forward "Hello") busca pela string "Hello".
;; (while x y) interpreta a(s) sexp(s) y enquanto x retornar algo.
;; Se x retornar `nil' (nada), nós saímos do laço.

(replace-hello-by-bonjour)

;; Você deveria ver todas as ocorrências de "Hello" no buffer *test*
;; substituídas por "Bonjour".

;; Você deveria, também, receber um erro: "Search failed: Hello".
;;
;; Para evitar este erro, você precisa dizer ao `search-forward' se ele
;; deveria parar de buscar em algum ponto no buffer, e se ele deveria
;; falhar de forma silenciosa quando nada fosse encontrado:

;; (search-forward "Hello" nil t) dá conta do recado:

;; O argumento `nil' diz: a busca não está limitada a uma posição.
;; O argumento `t' diz: falhe silenciosamente quando nada for encontrado.

;; Nós utilizamos esta "sexp" na função abaixo, que não gera um erro:

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

;; Vamos colorir os nomes:

(defun boldify-names ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (re-search-forward "Bonjour \\(.+\\)!" nil t)
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold)))
    (other-window 1))

;; Esta função introduz `re-search-forward': ao invés de buscar
;; pela string "Bonjour", você busca por um padrão utilizando uma
;; "expressão regular" (abreviada pelo prefixo "re-").

;; A expressão regular é "Bonjour \\(.+\\)!" e lê-se:
;; a string "Bonjour ", e
;; um grupo de               | que é o \\( ... \\)
;;   quaisquer caracteres    | que é o .
;;   possivelmente repetidos | que é o +
;; e a string "!".

;; Preparado? Teste!

(boldify-names)

;; `add-text-properties' adiciona... propriedades de texto, como uma fonte.

;; OK, terminamos por aqui. Feliz Hacking!

;; Se você quiser saber mais sobre uma variável ou função:
;;
;; C-h v uma-variável RET
;; C-h f uma-função RET
;;
;; Para ler o manual de Emacs Lisp que vem com o Emacs:
;;
;; C-h i m elisp RET
;;
;; Para ler uma introdução online ao Emacs Lisp:
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html

;; Agradecimentos a estas pessoas por seu feedback e sugestões:
;; - Wes Hardaker
;; - notbob
;; - Kevin Montuori
;; - Arne Babenhauserheide
;; - Alan Schmitt
;; - LinXitoW
;; - Aaron Meurer
```
