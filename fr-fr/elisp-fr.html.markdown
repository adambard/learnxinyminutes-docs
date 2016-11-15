---
language: elisp
contributors:
    - ["Bastien Guerry", "https://bzg.fr"]
    - ["Saurabh Sandav", "http://github.com/SaurabhSandav"]
translators:
    - ["Bastien Guerry", "https://bzg.fr"]
filename: learn-emacs-lisp-fr.el
lang: fr-fr
---

```scheme
;; Ceci est une introduction à Emacs Lisp en 15 minutes (v0.2d)
;;
;; Auteur : Bastien / @bzg2 / https://bzg.fr
;;
;; Prenez d'abord le temps de lire ce texte en anglais de Peter Norvig :
;; http://norvig.com/21-days.html
;;
;; Ensuite installez GNU Emacs 24.3 (ou une version ultérieure) :
;;
;; Debian : apt-get install emacs (voir les instructions pour votre distribution)
;; MacOSX : http://emacsformacosx.com/emacs-builds/Emacs-24.3-universal-10.6.8.dmg
;; Windows : http://ftp.gnu.org/gnu/windows/emacs/emacs-24.3-bin-i386.zip
;;
;; Vous trouverez plus d'informations sur l'installation :
;; http://www.gnu.org/software/emacs/#Obtaining

;; Avertissement important :
;;
;; Suivre ce tutoriel ne risque pas d'endommager votre ordinateur,
;; sauf si vous vous énervez au point de le jeter par terre. En tout
;; cas, je décline toute responsabilité en cas de problème.
;; Amusez-vous bien !

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lancez Emacs.
;;
;; Tapez la touche "q" pour enlever le message d'accueil.
;;
;; Maintenant regardez la ligne grise au pied de la fenêtre :
;;
;; "*scratch*" est le nom de l'espace d'édition dans lequel vous vous
;; trouvez. Cet espace d'édition est appelé un "buffer".
;;
;; Le buffer scratch est le buffer par défaut quand on ouvre Emacs.
;; Vous n'éditez jamais de fichier directement : vous éditez des
;; buffers que vous pouvez sauvegarder dans des fichiers.
;;
;; "Lisp interaction" désigne le jeu de commandes disponible ici.
;;
;; Emacs a un jeu de commandes par défaut pour chaque buffer, et
;; plusieurs autres jeux de commandes disponibles quand vous activez
;; un mode particulier.  Ici nous utilisons `lisp-interaction-mode',
;; qui propose des commandes pour évaluer et naviguer dans du code
;; Elisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Le point-virgule commence un commentaire partout sur une ligne.
;;
;; Les programmes Elisp sont composés d'expressions symboliques aussi
;; appelées "sexps" :
(+ 2 2)

;; Cette expression symbolique se lit "Ajouter 2 à 2".

;; Les sexps sont placées entre parenthèses, possiblement sur
;; plusieurs niveaux :
(+ 2 (+ 1 1))

;; Une expression symbolique contient des atomes ou d'autres
;; expressions symboliques. Dans les exemples ci-dessus, 1 et 2 sont
;; des atomes et (+ 2 (+ 1 1)) et (+ 1 1) des expressions symboliques.

;; Dans le mode `lisp-interaction-mode' vous pouvez évaluer les sexps.
;; Placez le curseur juste après la parenthèse fermante, tenez la
;; touche "Control" enfoncée et appuyez sur la touche "j" (soit le
;; raccourci "C-j").

(+ 3 (+ 1 2))
;;           ^ curseur ici
;; `C-j' => 6

;; `C-j' insère le résultat de l'évaluation dans le buffer.

;; `C-x C-e' affiche le même résultat dans la ligne tout en bas
;; d'Emacs, appelée le "minibuffer". On utilise en général `C-x C-e',
;; pour ne pas encombrer le buffer avec du texte inutile.

;; `setq' assigne une valeur à une variable :
(setq my-name "Bastien")
;; `C-x C-e' => "Bastien" (affiché dans le minibuffer)

;; `insert' va insérer "Hello!" là où se trouve le curseur :
(insert "Hello!")
;; `C-x C-e' => "Hello!"

;; Nous utilisons `insert' avec un seul argument "Hello!", mais
;; nous pouvons passer plus d'arguments - ici nous en passons deux :

(insert "Hello" " world!")
;; `C-x C-e' => "Hello world!"

;; Vous pouvez utiliser des variables au lieu de chaînes de caractères :
(insert "Hello, I am " my-name)
;; `C-x C-e' => "Hello, I am Bastien"

;; Vous pouvez combiner les sexps en fonctions :
(defun hello () (insert "Hello, I am " my-name))
;; `C-x C-e' => hello

;; Vous pouvez évaluer les fonctions :
(hello)
;; `C-x C-e' => Hello, I am Bastien

;; Les parenthèses vides dans la définition de la fonction signifient
;; qu'elle ne prend pas d'argument. Mais toujours utiliser `my-name'
;; est ennuyant, demandons à la fonction d'accepter un argument (ici
;; l'argument est appelé "name") :

(defun hello (name) (insert "Hello " name))
;; `C-x C-e' => hello

;; Maintenant appelons la fonction avec la chaîne de caractères "you"
;; comme valeur de son unique argument :
(hello "you")
;; `C-x C-e' => "Hello you"

;; Youpi!

;; Faites une pause.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maintenant ouvrez un nouveau buffer appelé "*test*" dans une
;; nouvelle fenêtre :

(switch-to-buffer-other-window "*test*")
;; `C-x C-e'
;; => [l'écran a deux fenêtres et le curseur est dans le buffer *test*]

;; Placez la souris sur la fenêtre du haut et cliquez-gauche pour
;; retourner dans cette fenêtre. Ou bien utilisez `C-x o' (i.e. tenez
;; control-x appuyé et appuyez sur o) pour aller dans l'autre fenêtre
;; interactivement.

;; Vous pouvez combiner plusieurs sexps avec `progn' :
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))
;; `C-x C-e'
;; => [L'écran a deux fenêtres et le curseur est dans le buffer *test*]

;; Maintenant si ça ne vous dérange pas, je vais arrêter de vous
;; demander de faire `C-x C-e' : faites-le pour chaque sexp qui suit.

;; Retournez toujours dans le buffer *scratch* avec la souris ou `C-x o'.

;; Il est souvent utile d'effacer le contenu du buffer :
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))

;; Ou d'aller à l'autre fenêtre :
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))

;; Vous pouvez associer une valeur à une variable locale avec `let' :
(let ((local-name "you"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; Dans ce cas pas besoin d'utiliser `progn' puisque `let' combine
;; aussi plusieurs sexps.

;; Mettons en forme une chaîne de caractères :
(format "Hello %s!\n" "visitor")

;; %s désigne l'emplacement de la chaîne, remplacé par "visitor".
;; \n est le caractère de saut de ligne.

;; Améliorons notre fonction en utilisant "format" :
(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; Créons une autre fonction qui utilise `let' :
(defun greeting (name)
  (let ((your-name "Bastien"))
    (insert (format "Hello %s!\n\nI am %s."
                    name       ; l'argument de la fonction
                    your-name  ; la variable "let-bindée" "Bastien"
                    ))))

;; Et évaluons-la :
(greeting "you")

;; Certaines fonctions sont interactives :
(read-from-minibuffer "Enter your name: ")

;; Évaluer cette fonction va renvoyer ce que vous avez saisi dans le
;; minibuffer.

;; Faisons que notre fonction `greeting' vous demande votre nom :
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (insert (format "Hello!\n\nI am %s and you are %s."
                    from-name ; l'argument de la fonction
                    your-name ; la variable "let-bindée", entrée dans le minibuffer
                    ))))

(greeting "Bastien")

;; Complétons la fonction pour qu'elle affiche le résultat dans
;; l'autre fenêtre :
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name: ")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello %s!\n\nI am %s." your-name from-name))
    (other-window 1)))

;; Maintenant testons :
(greeting "Bastien")

;; Faites une pause.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Stockons une liste de noms :
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; Récupérez le premier élément de la liste avec `car' :
(car list-of-names)

;; Récupérez tous les élements sauf le premier avec `cdr' :
(cdr list-of-names)

;; Ajoutez un élément au début avec `push' :
(push "Stephanie" list-of-names)

;; Note : `car' et `cdr' ne modifient pas la liste, mais `push' oui.
;; C'est une différence importante : certaines fonctions n'ont pas
;; d'effets de bord (comme `car') et d'autres oui (comme `push').

;; Évaluons `hello' pour tous les éléments dans `list-of-names' :
(mapcar 'hello list-of-names)

;; Améliorons `greeting' pour dire hello aux noms de `list-of-names' :
(defun greeting ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (mapcar 'hello list-of-names)
    (other-window 1))

(greeting)

;; Vous vous souvenez de la fonction `hello' définie ci-dessus ?  Elle
;; prend seulement un argument, un nom.  `mapcar' appelle `hello' en
;; utilisant successivement chaque élément de `list-of-names' comme
;; argument de `hello'.

;; Maintenant arrangeons un peu ce qui est affiché dans le buffer :

(defun replace-hello-by-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (search-forward "Hello")
      (replace-match "Bonjour"))
    (other-window 1))

;; (goto-char (point-min)) va au début du buffer.
;; (search-forward "Hello") cherche la chaîne "Hello".
;; (while x y) évalue la sexp(s) y tant que x renvoie quelque chose.
;; Si x renvoie `nil' (rien), nous sortons de la boucle.

(replace-hello-by-bonjour)

;; Vous devriez voir toutes les occurrences de "Hello" dans le buffer
;; *test* remplacées par "Bonjour".

;; Vous devriez aussi avoir une erreur : "Search failed: Hello".
;;
;; Pour éviter cette erreur, il faut dire à `search-forward' si la
;; recherche doit s'arrêter à un certain point du buffer, et si elle
;; doit s'arrêter silencieusement si aucune chaîne n'est trouvée.

;; (search-forward "Hello" nil t) fait ça :

;; L'argument `nil' indique que la recherche n'est pas limitée à une
;; position.  L'argument `t' indique de s'arrêter silencieusement si
;; rien n'est trouvé.

;; Nous utilisons cette sexp dans la fonction ci-dessous, qui ne
;; renvoie pas d'erreur :

(defun hello-to-bonjour ()
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    ;; Dit hello aux noms de `list-of-names'
    (mapcar 'hello list-of-names)
    (goto-char (point-min))
    ;; Remplace "Hello" par "Bonjour"
    (while (search-forward "Hello" nil t)
      (replace-match "Bonjour"))
    (other-window 1))

(hello-to-bonjour)

;; Mettons les noms en gras :

(defun boldify-names ()
    (switch-to-buffer-other-window "*test*")
    (goto-char (point-min))
    (while (re-search-forward "Bonjour \\(.+\\)!" nil t)
      (add-text-properties (match-beginning 1)
                           (match-end 1)
                           (list 'face 'bold)))
    (other-window 1))

;; Cette fonction introduit `re-search-forward' : au lieu de chercher
;; la chaîne "Bonjour", nous cherchons un "pattern" en utilisant une
;; "expression régulière" (le préfixe "re-" signifie "regular
;; expression").

;; L'expression régulière est "Bonjour \\(.+\\)!" et se lit :
;; la chaîne "Bonjour ", et
;; un groupe de                | c'est la syntaxe \\( ... \\)
;;   n'importe quel caractère  | c'est le .
;;   une ou plusieurs fois     | c'est le +
;; et la chaîne "!".

;; Prêt ?  Testons !

(boldify-names)

;; `add-text-properties' ajoute des propriétés textuelles telle que
;; des "faces" (une "face" définit la fonte, la couleur, la taille et
;; d'autres propriétés du texte.)

;; Et voilà, c'est fini.  Happy hacking!

;; Si vous voulez en savoir plus sur une variable ou une fonction :
;;
;; C-h v une-variable RET
;; C-h f une-fonction RET
;;
;; Pour lire le manuel Emacs Lisp avec Emacs :
;;
;; C-h i m elisp RET
;;
;; Pour lire en ligne une introduction à Emacs Lisp :
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html

;; Merci à ces personnes pour leurs retours et suggetions :
;; - Wes Hardaker
;; - notbob
;; - Kevin Montuori
;; - Arne Babenhauserheide
;; - Alan Schmitt
;; - LinXitoW
;; - Aaron Meurer
```
