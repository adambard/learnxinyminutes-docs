---
category: tool
tool: emacs
filename: emacs.txt
contributors:
    - ["Joseph Riad", "https://github.com/Joseph-Riad"]
translators:
    - ["André de Santa Gabriel", "https://github.com/andredesanta"]
lang: pt-br
---

O Emacs começou sua vida como [gnu.org/software/emacs/emacs-paper.html](https://www.gnu.org/software/emacs/emacs-paper.html) e cresceu
ao longo dos anos em um ecossistema completo. Muitas tarefas, geralmente
relegado a um conjunto diversificado de ferramentas pode ser realizado de dentro
Emacs em uma interface consistente e familiar. Exemplos incluem
gerenciamento de diretório, visualização de documentos PDF, edição de arquivos via SSH, gerenciamento de
repos git. Em suma, o Emacs é seu para fazer
o que você quiser: o espectro de usuários varia daqueles que o usam para
editar arquivos de texto para puristas extremos que o usam para substituir virtualmente seu
sistema operacional.

O Emacs é extensível através de um dialeto especializado do Lisp conhecido como Emacs
Lisp (Elisp), que possui muitas macros voltadas para a edição de texto e
gerenciamento de buffers de texto. Qualquer tecla (combinação) usada no Emacs está vinculada
para uma função Emacs Lisp e pode ser remapeado para qualquer outra função,
incluindo aqueles que você escreve
você mesmo.

# Conceitos básicos de Emacs

Aqui, discuto alguns conceitos e terminologia básicos do Emacs que podem ser
confusos para os recém-chegados (especialmente para as pessoas acostumadas à terminologia do Vim):

  - O texto que o Emacs está editando é conhecido como **buffer**
  - Um buffer não corresponde necessariamente a um arquivo real no disco. Pode ser apenas texto na memória.
  - Quando um buffer corresponde a um arquivo no disco, dizemos que o buffer está **visitando** esse arquivo.
  - O Emacs normalmente possui muitos buffers abertos ao mesmo tempo.
  - A exibição do Emacs pode ser dividida em diferentes **windows**.
  - Uma janela do sistema operacional para o Emacs é chamada de **frame**. Assim, quando o manual do Emacs fala sobre a abertura de um novo frame, esse essencialmente significa abrir uma nova janela do SO contendo uma (outra) instância do Emacs.
  - Os conceitos convencionalmente conhecidos como recortar e colar são referido como **killing** e **yanking**, respectivamente no Emacs.
  - A posição atual do cursor é chamada de **point** no Emacs. Tecnicamente, **point** é definido como a posição imediatamente antes do caractere onde o cursor está atualmente.
  - Finalmente, cada buffer pode ter vários **modes** associados: o **major mode** e possivelmente vários **minor modes**.
  - O **major mode** define o principal comportamento do Emacs no buffer atualmente selecionado. Isso pode ser pensado como o tipo de arquivo. Por exemplo, se você estiver editando um arquivo Python, os principais modes é (por padrão) `python-mode`, que faz com que o Emacs destaque a sintaxe Python e idente automaticamente seus blocos de código conforme exigido sintaticamente pelo seu código Python.
  - **Minor modes** definem mudanças sutis no comportamento e várias alterações menores Os modos podem estar ativos ao mesmo tempo no mesmo buffer. Um exemplo menor modo é o modo flyspell, que destaca automaticamente os erros de ortografia no seu buffer.

# Recursos adicionais

  - [The GNU Emacs Manual](https://www.gnu.org/software/emacs/manual/emacs.html)
  - [Emacs Stack Exchange](https://emacs.stackexchange.com/)
  - [Emacs Wiki](https://www.emacswiki.org/emacs/EmacsWiki)
