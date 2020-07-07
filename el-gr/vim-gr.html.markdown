---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
filename: LearnVim.txt
lang: el-gr
---


[Vim](http://www.vim.org)
To (Vi IMproved) είναι ένας κλώνος του δημοφιλούς vi editor για Unix.
Είναι ένας text editor σχεδιασμένος για ταχύτητα και αυξημένη παραγωγικότητα,
και υπάρχει σχεδόν σε όλα τα Unix-based συστήματα. Έχει διάφορα keybindings
(συντομεύσεις πλήκτρων) για να πλοηγούμαστε γρήγορα σε συγκεκριμένα σημεία ενός αρχείου,
καθώς και για γρήγορη επεξεργασία.

## Τα βασικά της πλοήγησης στον Vim

```
    vim <filename>   # Άνοιξε το <filename> στον vim
    :help <topic>    # Άνοιξε το built-in βοήθημα για το <topic> αν υπάρχει
    :q               # Βγες από τον vim
    :w               # Αποθήκευσε το τρέχον αρχείο
    :wq              # Αποθήκευσε το τρέχον αρχείο και βγες από τον vim
    ZZ               # Αποθήκευσε το τρέχον αρχείο και βγες από τον vim
    :q!              # Βγες χωρίς αποθήκευση
                     # ! *αναγκάζει* το :q να εκτελεστεί, γι αυτό βγαίνει χωρίς saving
    :x               # Ίδιο με το wq αλλά πιο σύντομο

    u                # Undo
    CTRL+R           # Redo

    h                # Μετακινήσου κατά ένα χαρακτήρα αριστερά
    j                # Μετακινήσου μια γραμμή κάτω
    k                # Μετακινήσου μια γραμμή πάνω
    l                # Μετακινήσου μια γραμμή δεξιά

    Ctrl+B 	         # Πήγαινε μία οθόνη πίσω
    Ctrl+F 	         # Πήγαινε μία οθόνη μπροστά
    Ctrl+U           # Πήγαινε μισή οθόνη πίσω
    Ctrl+D 	         # Πήγαινε μισή οθόνη μπροστά

    # Μετακινήσεις στην ίδια γραμμή

    0                # Πήγαινε στην αρχή της γραμμής
    $                # Πήγαινε στο τέλος της γραμμής
    ^                # Πήγαινε στον πρώτο μη κενό χαρακτήρα της γραμμής

    # Αναζήτηση στο κείμενο

    /word            # Υπογραμμίζει όλες τις εμφανίσεις της λέξης μετά τον cursor
    ?word            # Υπογραμμίζει όλες τις εμφανίσεις της λέξης πριν τον cursor
    n                # Μετακινεί τον cursor στην επόμενη εμφάνιση της λέξης
    N                # Μετακινεί τον cursor στην προηγούμενη εμφάνιση της λέξης

    :%s/foo/bar/g    # άλλαξε το 'foo' σε 'bar' σε κάθε γραμμή του αρχείου
    :s/foo/bar/g     # άλλαξε το 'foo' σε 'bar' στην τρέχουσα γραμμή

    # Άλματα σε χαρακτήρες

    f<character>     # Άλμα μπροστά και προσγείωση στο επόμενο <character>
    t<character>     # Άλμα μπροστά και προσγείωση αμέσως πριν το προηγούμενο <character>

    # Για παράδειγμα,
    f<               # Άλμα μπροστά και προσγείωση σε <
    t<               # Άλμα μπροστά και προσγείωση αμέσως πριν <

    # Μετακινήσεις κατά λέξεις

    w                # Πήγαινε μια λέξη μπροστά
    b                # Πήγαινε μια λέξη πίσω
    e                # Πήγαινε στο τέλος της λέξης στην οποία είσαι

    # Άλλοι χαρακτήρες για να τριγυρνάμε

    gg               # Πήγαινε στην αρχή του αρχείου
    G                # Πήγαινε στο τέλος του αρχείου
    :NUM             # Πήγαινε στη γραμμή με αριθμό NUM (οποιοσδήποτε αριθμός)
    H                # Πήγαινε στην κορυφή της σελίδας
    M                # Πήγαινε στην μέση της σελίδας
    L                # Πήγαινε στο κάτω άκρο της σελίδας
```

## Help docs:
Το Vim έχει built-in help documentation που μπορείς να δεις με `:help <topic>`.
Για παράδειγμα το `:help navigation` θα σου εμφανίσει documentation σχετικό με
το πως να πλοηγείσαι στο αρχείο!

To `:help` μπορεί να χρησιμοποιηθεί και χωρίς option. Αυτό θα εμφανίσει το default
help dialog που σκοπεύει να κάνει το vim πιο προσιτό σε αρχάριους!

## Modes:

O Vim στηρίζεται στο concept των **modes**.

- Command Mode -  ο vim εκκινεί σε αυτό mode, χρησιμοποιείται για πλοήγηση και εντολές
- Insert Mode  - χρησιμοποιείται για να κάνουμε αλλαγές στα αρχεία
- Visual Mode  - χρησιμοποιείται για να υπογραμμίζουμε κείμενα και να κάνουμε διάφορα σε αυτά
- Ex Mode      - χρησιμοποιείται για να πάμε στο κάτω μέρος με το ':' που δίνουμε εντολές

```
    i                # Βάζει το vim σε insert mode, πριν τη θέση cursor
    a                # Βάζει το vim σε insert mode, μετά τη θέση cursor
    v                # βάζει τον vim σε visual mode
    :                # Βάζει τον vim σε ex mode
    <esc>            # φεύγει από όποιο mode είμαστε και πάει σε command mode

    # Αντιγραφή-Επικόληση κειμένου

    y                # Yank (κάνε copy) ό,τι είναι επιλεγμένο
    yy               # Yank την γραμμή στην οποία είσαι
    d                # διάγραψε ό,τι είναι επιλεγμένο
    dd               # Διάγραψε τη γραμμή στην οποία είσαι
    p                # Κάνε Paste το αντεγραμένο κείμενο μετά την θέση του cursor
    P                # Κάνε Paste το αντεγραμένο κείμενο πριν την θέση του cursor
    x                # Διάγραψε τον χαρακτήρα που είναι κάτω από τον cursor
```

## Η 'γραμματική' του Vim

Μπορείς να σκεφτείς τον Vim ως ένα σύνολο εντολών
σε μορφή 'Verb-Modifier-Noun', όπου

- Verb     - η ενέργεια που θες να κάνεις
- Modifier - πώς κάνεις την ενέργεια
- Noun     - το αντικείμενο που δέχεται την ενέργεια

Μερικά παραδείγματα ''Ρημάτων', 'Modifiers' και 'Ουσιαστικών':

```
    # 'Ρήματα'

    d                # Διάγραψε
    c                # Άλλαξε
    y                # Yank (αντίγραψε)
    v                # Επίλεξε οπτικά

    # 'Modifiers'

    i                # Μέσα
    a                # Γύρω
    NUM              # Αριθμός (NUM = οποιοσδήποτε αριθμός)
    f                # Ψάξε κάτι και πήγαινε εκεί που βρίσκεται
    t                # Ψάξε κάτι και πήγαινε πριν από εκεί που βρίσκεται
    /                # Βρες κάποιο string μετά από τον cursor
    ?                # Βρες κάποιο string πριν τον cursor

    # 'Ουσιαστικά'

    w                # Λέξη
    s                # Πρόταση
    p                # Παράγραφος
    b                # Block

    # Δείγματα 'προτάσεων' ή εντολών

    d2w              # Διάγραψε 2 λέξεις
    cis              # Άλλαξε μέσα στην πρώταση
    yip              # Αντίγραψε την παράγραφο στην οποία βρίσκεσαι
    ct<              # Άλλαξε σε <
                     # Άλλαξε το κείμενο από το οποίο είσαι πριν το επόμενο bracketChange the text from where you are to the next open bracket
    d$               # Διάγραψε μέχρι το τέλος της γραμμής
```

## Μερικά shortcuts και κόλπα

        <!--TODO: Βάλτε κι άλλα!-->
```
    >                # Στοίχισε προς τα δεξιά την επιλογή σου κατά ένα block
    <                # Στοίχισε προς τα αριστερά την επιλογή σου κατά ένα block
    :earlier 15m     # Κάνε το αρχείο όπως ήταν πριν 15 λεπτά
    :later 15m       # Ακύρωση για την παραπάνω εντολή
    ddp              # Αντάλλαξε τις θέσεις διαδοχικών γραμμών
    .                # Επανάλαβε την προηγούμενη ενέργεια
    :w !sudo tee %   # Σώσε το τρέχον αρχείο ως root
    :set syntax=c    # Κάνε syntax highlighting για τη γλώσσα c
    :sort            # Ταξινόμησε όλες τις γραμμές
    :sort!           # Ταξινόμησε ανάποδα όλες τις γραμμές (αύξουσα σειρά)
    :sort u          # Ταξινόμησε όλες τις γραμμές και διάγραψε τις διπλές γραμμές
    ~                # Άλλαξε τα κεφαλαία σε μικρά στο επιλεγμένο κείμενο
    u                # Το επιλεγμένο κείμενο να γίνει πεζά γράμματα
    U                # Το επιλεγμένο κείμενο να γίνει κεφαλαία γράμματα

    # Fold text
    zf               # Διπλώνει (συμπιέζει τις γραμμές σε μία) το επιλεγμένο κείμενο
    zo               # Ξεδιπλώνει το επιλεγμένο fold
    zc               # Κλείνει το επιλεγμένο fold
    zR               # Ανοίγει όλα τα folds
    zM               # Κλείνει όλα τα folds
```

## Macros

Τα macros βασικά είναι καταγραφή ενεργειών.
Όταν ξεικάς να καταγράφεις ένα macro καταγράφονται **όλες** οι ενέργεις και οι
εντολές που χρησιμοποιείς, μέχρι να σταματήσεις την καταγραφή. Όταν καλείς ένα macro,
εκτελείται πάλι η ίδια σειρά από ενέργειες και εντολές στο επιλεγμένο κείμενο.

```
    qa               # Ξεκίνα να καταγράφεις ένα macro που θα ονομαστεί 'a'
    q                # Σταμάτα την καταγραφή
    @a               # Τρέξε το macro
```

### Configuring ~/.vimrc

Το αρχείο .vimrc μπορεί να χρησιμοποιηθεί για να κάνεις configure το Vim στο startup.

Εδώ βλέπουμε δείγμα ενός ~/.vimrc file:

```
" Example ~/.vimrc
" 2015.10

" Required for vim to be iMproved
set nocompatible

" Determines filetype from name to allow intelligent auto-indenting, etc.
filetype indent plugin on

" Enable syntax highlighting
syntax on

" Better command-line completion
set wildmenu

" Use case insensitive search except when using capital letters
set ignorecase
set smartcase

" When opening a new line and no file-specific indenting is enabled,
" keep same indent as the line you're currently on
set autoindent

" Display line numbers on the left
set number

" Indentation options, change according to personal preference

" Number of visual spaces per TAB
set tabstop=4

" Number of spaces in TAB when editing
set softtabstop=4

" Number of spaces indented when reindent operations (>> and <<) are used
set shiftwidth=4

" Convert TABs to spaces
set expandtab

" Enable intelligent tabbing and spacing for indentation and alignment
set smarttab
```

### Αναφορές

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (Stack Overflow thread)](http://stackoverflow.com/questions/726894/what-are-the-dark-corners-of-vim-your-mom-never-told-you-about)

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)
