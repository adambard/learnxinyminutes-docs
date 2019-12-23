---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
    - ["Andrew Taylor", "http://github.com/andrewjt71"]
translators:
    - ["Terka Slanináková", "http://github.com/TerkaSlan"]
lang: sk-sk
filename: LearnGit-sk.txt
---

Git je distribuovaný systém riadenia revízií a správy zdrojového kódu.

Funguje robením "snímkov" tvojho projektu, s ktorými ďalej pracuje na revíziach a správe zdrojových kódov.

## Koncept Revízií

### Čo je riadenie revízií?

Riadenie revízií je systém, ktorý postupom času zaznamenáva zmeny súboru (súborov).

### Centralizované Revízie VS Distribuované revízie

* Centralizované riadenie revízií sa zameriava na synchronizáciu, sledovanie a zálohovanie súborov.
* Distribuované riadenie revízií sa zameriava na zdieľanie zmien. Kaťdá zmena má jedinečný identifikátor (id).
* Distribuované systémy nemajú definovanú štruktúru. S gitom môžeš mať centralizovaný systém v subversion (SVN) štýle.

[Ďalšie informácie](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Prečo Používať Git? 

* Môžeš pracovať offline.
* Spolupráca s ostatnými je jednoduchá!
* Vetvenie je jednoduché!
* Zlučovanie je jednoduché!
* Git je rýchly.
* Git je flexibilný.

## Architektúra Gitu


### Repozitár

Skupina súborov, adresárov, minulých záznamov, commitov (konkrétnych revízií) a odkazy na aktuálu vetvu (HEADs). Predstav si ho ako údajovú štruktúru, kde ti každý "prvok" zdrojového kódu poskytne (okrem iného) prístup k minulým revíziam.

Git repozitár sa skladá z .git adresára a pracovného stromu

### .git Adresár (časť repozitára)

.git  adresár obsahuje všetky konfigurácie, logy, vetvy, odkaz na aktuálnu vetvu (HEAD) a ostatné.
[Detailný zoznam.](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Pracovný Strom (Working Tree - časť repozitára)

Toto sú adresáre a súbory v tvojom repozitári. Tiež sa tomu hovorí pracovný adresár.

### Index (časť .git adresára)

Index je také odpočívadlo Gitu. Je to v podstate vrstva, ktorá oddeľuje pracovný strom od Git repozitára. Toto dáva vývojárom viac možností nad tým, čo do repozitára naozaj pošlú.

### Commit

Commit je "snímka" zmien, či manipulácií s tvojím Pracovným Stromom. Ak si napríklad pridal 5 súborov a odstránil 2 ďalšie, tieto zmeny budú zachytené v commite. Ten môže (ale nemusí) byť zverejnený (pushed) do iných repozitárov.

### Vetva (Branch)

Vetva je ukazateľ na posledný vykonaný commit. Po ďalších commitnutiach sa ukazateľ bude automaticky posúvať na ten najnovší.

### Tag

Tag je označenie špecifického bodu v minulosti. Typicky sa používa na značenie vydaných verzií (v1.0, atď).

### HEAD a head (časť .git adresára)

HEAD je ukazateľ na aktuálnu vetvu. Repozitár má len 1 *aktívny* HEAD.
head je ukazateľ, ktorý môže ukazovať na akýkoľvek commit. Repozitár môže mať niekoľko headov.

### Štádia Gitu
* Modified - Súbor bol zmenený, no nebol ešte commitnutý do Git Databázy.
* Staged - Zmenený súbor, ktorý pôjde do najbližšieho commit snímku.
* Committed - Súbory boli commitnuté do Git Databázy.

### Koncepčné zdroje

* [Git Pre Informatikov](http://eagain.net/articles/git-for-computer-scientists/)
* [Git Pre Designerov](http://hoth.entp.com/output/git_for_designers.html)


## Príkazy


### init

Vytvorí prázdny Git repozitár. Jeho nastavenia, uložené informácie a mnoho iného sú uložené v adresári (zložke) s názvom ".git".

```bash
$ git init
```

### config

Konfiguruj nastavenia. Či už pre repozitár, samotný systém, alebo globálne konfigurácie (súbor pre globálny config je `~/.gitconfig`).


```bash
# Zobraz a Nastav Základné Konfiguračné Premenné (Globálne)
$ git config --global user.email "MôjEmail@Zoho.com"
$ git config --global user.name  "Moje Meno	"
```

[Prečítaj si viac o git configu.](http://git-scm.com/docs/git-config)

### pomoc

Máš tiež prístup k extrémne detailnej dokumentácií pre každý príkaz (po anglicky). Hodí sa, ak potrebuješ pripomenúť semantiku.

```bash
# Rýchlo zobraz všetky dostupné príkazy
$ git help

# Zobraz všetky dostupné príkazy
$ git help -a

# Zobraz konkrétnu pomoc - použivateľský manuál
# git help <príkaz_tu>
$ git help add
$ git help commit
$ git help init
# alebo git <príkaz_tu> --help
$ git add --help
$ git commit --help
$ git init --help
```

### ignoruj súbory

Zámerne prestaneš sledovať súbor(y) a zložky. Typicky sa používa pre súkromné a dočasné súbory, ktoré by boli inak zdieľané v repozitári.
```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```


### status

Na zobrazenie rozdielov medzi indexovými súbormi (tvoj pracovný repozitár) a aktuálnym HEAD commitom.


```bash
# Zobrazí vetvu, nesledované súbory, zmeny a ostatné rozdiely
$ git status

# Zistí iné vychytávky o git statuse
$ git help status
```

### add

Pripraví súbory na commit pridaním do tzv. staging indexu. Ak ich nepridáš pomocou `git add` do staging indexu, nebudú zahrnuté v commitoch!

```bash
# pridá súbor z tvojho pracovného adresára
$ git add HelloWorld.java

# pridá súbor z iného adresára
$ git add /cesta/k/súboru/HelloWorld.c

# Môžeš použiť regulárne výrazy!
$ git add ./*.java
```
Tento príkaz len pridáva súbory do staging indexu, necommituje ich do repozitára.

### branch

Spravuj svoje vetvy. Môžeš ich pomocou tohto príkazu zobraziť, meniť, vytvoriť, či zmazať.

```bash
# zobraz existujúce vetvy a vzdialené repozitáre
$ git branch -a

# vytvor novú vetvu
$ git branch myNewBranch

# vymaž vetvu
$ git branch -d myBranch

# premenuj vetvu
# git branch -m <starémeno> <novémeno>
$ git branch -m mojaStaraVetva mojaNovaVetva

# zmeň opis vetvy
$ git branch myBranchName --edit-description
```

### tag

Spravuj svoje tagy

```bash
# Zobraz tagy
$ git tag
# Vytvor tag so správou
# -m špecifikuje správu, ktorá bude s tagom uložená.
# Ak nešpeficikuješ správu pri tagu so správou,
# Git spustí tvoj editor, aby si ju napísal.
$ git tag -a v2.0 -m 'moja verzia 2.0'
# Ukáž informácie o tagu
# Zobrazí zadané informácie, dátum tagnutia commitu
# a správu pred zobrazením informácií o commite.
$ git show v2.0
# Zverejní (pushne) jediný tag do vzdialeného repozitára
$ git push origin v2.0
# Zverejní viacero tagov do vzdialeného repozitára
$ git push origin --tags
```

### checkout

Aktualizuje všetky súbory v pracovnom strome, aby odpovedali verzií v indexe, alebo v inom strome.

```bash
# Aktualizuj strom, aby odpovedal (predvolene) 
# hlavnej vetve repozitáru (master branch)
$ git checkout
# Aktualizuj strom, aby odpovedal konrkétnej vetve
$ git checkout menoVetvy
# Vytvor novú vetvu & prepni sa na ňu
# ekvivalentný príkaz: "git branch <meno>; git checkout <meno>"
$ git checkout -b nováVetva
```

### clone

"Naklonuje", alebo vytvorí kópiu existujúceho repozitára do nového adresára. Tiež pridá špeciálne ďiaľkovo-monitorujúce vetvy (remote-tracking branches), ktoré ti umožnia zverejňovať do vzdialených vetiev.

```bash
# Naklonuj learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
# povrchné klonovanie - rýchlejšie, uloží iba najnovšiu snímku
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git
# naklonuj iba konkrétnu vetvu
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Uloží aktuálny obsah indexu v novom "commite". Ten obsahuje vytvorené zmeny a s nimi súvisiace správy vytvorené použivateľom.

```bash
# commitni so správou
$ git commit -m "Pridal som multiplyNumbers() funkciu do HelloWorld.c"

# automaticky pridaj zmenené a vymazané súbory do staging indexu,  potom ich commitni.
$ git commit -a -m "Zmenil som foo.php a vymazal bar.php"

# zmeň posledný commit (toto nahradí predchádzajúci commit novým)
$ git commit --amend -m "Správna správa"
```

### diff

Ukáže rozdiel medzi súborom v pracovnom repozitári, indexe a commitoch.

```bash
# Ukáž rozdiel medzi pracovným repozitárom a indexom.
$ git diff

# Ukáž rozdiely medzi indexom a najnovším commitom.
$ git diff --cached

# Ukáž rozdiely medzi pracovným adresárom a najnovším commitom.
$ git diff HEAD
```

### grep

Umožní ti rýchlo prehľadávať repozitár.

Možná konfigurácia:

```bash
# Nastav, aby sa vo výsledkoch vyhľadávania zobrazovalo číslo riadku
$ git config --global grep.lineNumber true

# Urob výsledky vyhľadávania čitateľnejšie, vrátane zoskupovania
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Vďaka Travisovi Jefferymu za túto sekciu
# Hľadaj "názovPremennej" vo všetkých java súboroch
$ git grep 'názovPremennej' -- '*.java'

# Hľadaj riadok, ktorý obsahuje "názovPoľa" a "add" alebo "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Google je tvoj kamarát; pre viac príkladov skoč na
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Zobral commity do repozitára.

```bash
# Zobraz všetky commity
$ git log

# Zobraz iba správy a referencie commitov
$ git log --oneline

# Zobraz zlúčené (merged) commity
$ git log --merges

# Zobraz všetky commity vo forme ASCII grafu
$ git log --graph
```

### merge

"Zlúč" zmeny externých commitov do aktuálnej vetvy.

```bash
# Zlúč vybranú vetvu do aktuálnej.
$ git merge názovVetvy

# Vždy vytvor zlučovací commit
$ git merge --no-ff názovVetvy
```

### mv

Premenuj, alebo presuň súbor

```bash
# Premenuj súbor
$ git mv HelloWorld.c HelloNewWorld.c

# Presuň súbor
$ git mv HelloWorld.c ./nová/cesta/HelloWorld.c

# "Nasilu" premenuj, alebo presuň
# "existujúciSúbor" už v adresári existuje, bude prepísaný
$ git mv -f môjSúbor existujúciSúbor
```

### pull

Uloží obsah repozitára a zlúči ho s inou vetvou.

```bash
# Aktualizuje tvoj lokálny repozitár zlúčením nových zmien
# zo vzdialených "origin" a "master" vetiev.
# git pull <alias-vzdialeného-repo> <vetva>
$ git pull origin master

# Predvolene, git pull aktualizuje tvoju aktuálnu vetvu
# zlúčením nových zmien zo vzdialenej vetvy 
$ git pull

# Zlúč zmeny zo vzdialenej vetvy a presuň vetvu do nového základného commitu (rebase)
# vetva commitne na tvoj lokálny repozitár, ekvivalentný príkaz: "git pull <alias-vzdialeného-repo> <vrstva>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Zverejní a zlúči zmeny z lokálnej do vzdialenej vetvy.

```bash
# Zverejni a zlúč zmeny z lokálneho repozitára do
# vzdialených vetiev s názvom "origin" a "master".
# git push <vzdialené> <vetvy>
$ git push origin master

# Predvolene git zverejní a zlúči zmeny z
# aktuálnej vetvy do vzdialenej vetvy s ňou spojenej
$ git push

# Na spojenie lokálnej vetvy so vzdialenou pridaj -u:
$ git push -u origin master
# Kedykoľvek budeš chcieť zverejniť z rovnakej lokálnej vetvy, použi príkaz:
$ git push
```

### stash

Umožní ti opustiť chaotický stav pracovného adresára a uloží ho na zásobník nedokončených zmien, ku ktorým sa môžeš kedykoľvek vrátiť.

Povedzme, že si urobil nejaké zmeny vo svojom git repozitári, ale teraz potrebuješ pullnúť zo vzdialenej repo. Keďže máš necommitnuté zmeny, príkaz `git pull` nebude fungovať. Namiesto toho môžeš použiť `git stash` a uložiť svoje nedokončené zmeny na zásobník!

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Teraz môžeš uložiť vzdialenú vetvu!

```bash
$ git pull
```

Over, či je všetko v poriadku

```bash
$ git status
# On branch master
nothing to commit, working directory clean
```

Môžeš si pozrieť, čo za chaos je na zásobníku cez `git stash list`.
Nedokončené zmeny sú uložené ako Last-In-First-Out (Prvý dnu, posledný von) štruktúra, navrchu sa objavia najnovšie zmeny.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

Keď so zmenami budeš chcieť pracovať, odstráň ich zo stacku.

```bash
$ git stash pop
# On branch master
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#
#      modified:   index.html
#      modified:   lib/simplegit.rb
#
```

`git stash apply` urobí presne to isté

Hotovo, môžeš pokračovať v práci!

[Čítaj viac.](http://git-scm.com/book/en/v1/Git-Tools-Stashing)

### rebase (pozor)

Zober všetky zmeny commitnuté do vetvy a aplikuj ich na inú vetvu.
*Tento príkaz nerob na verejných repozitároch*.

```bash
# Aplikuj commity z experimentálnej vetvy na master
# git rebase <základnáVetva> <ináVetva>
$ git rebase master experimentBranch
```

[Čítaj viac.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (pozor)

Resetni HEAD (ukazateľ na aktuálnu vetvu) do konrkétneho stavu. To ti umožní vziať späť zlúčenia, zverejnenia, commity, pridania atď. Je to užitočný, no nebezpečný príkaz, pokiaľ nevieš, čo robíš.

```bash
# Resetni index (vrstvu medzi pracovným stromom a Git repozitárom), aby odpovedal najnovšiemu commitu (adresár ostane nezmenený)
$ git reset

# Resetni index, aby odpovedal najnovšiemu commitu (adresár sa prepíše)
$ git reset --hard

# Presunie vrchol aktuálnuej vetvy do konkrétneho commitu (adresár ostane nezmenený)
# všetky zmeny sú zachované v adresári.
$ git reset 31f2bb1

# Presunie vrchol aktuálnuej vetvy naopak do konkrétneho commitu
# a zosúladí ju s pracovným adresárom (vymaže nekomitnuté zmeny).
$ git reset --hard 31f2bb1
```
### revert

Vezme naspäť ("od-urobí") commit. Nezamieňaj s resetom, ktorý obnoví stav
projektu do predchádzajúceho bodu v čase. Revert pridá nový commit, inverzný tomu, ktorý chceš vymazať, tým ho od-urobí.

```bash
# Vezmi späť konkrétny commit
$ git revert <commit>
```

### rm

Opak od git add, rm odstráni súbory z aktuálneho pracovného stromu.

```bash
# odstráň HelloWorld.c
$ git rm HelloWorld.c

# Odstráň súbor z vnoreného adresára
$ git rm /pather/to/the/file/HelloWorld.c
```

## Ďalšie informácie

* [tryGit - Zábavný interaktívny spôsob, ako sa naučiť Git.](http://try.github.io/levels/1/challenges/1)

* [Udemy Git Tutoriál: Kompletný návod](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [Git Immersion - Návod, ktorý Ťa prevedie základmi Gitu](http://gitimmersion.com/)

* [git-scm - Video Tutoriály](http://git-scm.com/videos)

* [git-scm - Dokumentácia](http://git-scm.com/docs)

* [Atlassian Git - Tutoriály & Postupy](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [Git - jednoducho](http://rogerdudler.github.io/git-guide/index.html)

* [Pro Git](http://www.git-scm.com/book/en/v2)

* [Úvod do Gitu a GitHubu pre začiatočníkov (Tutoriál)](http://product.hubspot.com/blog/git-and-github-tutorial-for-beginners)
