---
category: tool
lang: tr-tr
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
    - ["Andrew Taylor", "http://github.com/andrewjt71"]
    - ["Jason Stathopulos", "http://github.com/SpiritBreaker226"]
    - ["Milo Gilad", "http://github.com/Myl0g"]
    - ["Adem Budak", "https://github.com/p1v0t"]
    
filename: LearnGit-tr.txt
---

Git dağınık versiyon kontrol ve kaynak kod yönetim sistemidir.

Bunu projenin bir seri anlık durumunu kaydederek yapar ve bu anlık durumları 
kullanarak versiyon ve kaynak kodu yönetmeni sağlar.

## Versiyonlama Konseptleri

### Versiyon kontrol nedir?

Versiyon kontrol, zaman içerisinde dosya(lar)daki değişikliği kaydeden sistemdir.

### Merkezi Versiyonlama vs. Dağınık Versiyonlama

* Merkezi versiyon kontrolü dosyaların eşitlenmesine, takibine ve yedeklenmesine odaklanır. 
* Dağınık versiyon kontrolü değişimin paylaşılmasına odaklanır. Her değişiminin benzersiz bir adı vardır.
* Dağınık sistemlerin belirlenmiş bir yapısı yoktur. Git ile kolayca SVN'deki gibi merkezi bir sistem elde edebilirsin. 

[Daha fazla bilgi](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Neden Git?

* Çevrimdışı çalışabilir
* Diğerleriyle beraber çalışmak kolaydır!
* Dallanma kolaydır!
* Dallanma hızlıdır!
* Git hızlıdır
* Git esnektir

## Git Mimarisi

### Repository

Bir grup dosya, dizin, geriye dönük kayıt, commit, head. Bunları kaynak kodun veri
yapısı gibi düşünebilirsin, herbir kaynak kod "elemanı" seni kendi revizyon geçmişine 
eriştirir. 

Bir git repo'su .git dizini ve çalışma ağacından oluşur. 

### .git Dizini (repository bileşeni)

.git dizini bütün konfigrasyon, log, dallanma, HEAD ve daha fazlasını tutar.
[detaylı liste](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Çalışma Ağacı (repository bileşeni)

Temelde repo'daki dizinlerin ve dosyalarındır. Sıkça çalışma ağacın olarak anılır.

### Index (.git dizininin birleşeni)

Index git'in evreleme alanıdır (staging area). Temelde çalışma ağacını Git repo'sundan 
ayıran bir katmandır. Bu geliştiricilere neyin Git repo'suna gönderileceği hakkında daha 
fazla güç verir. 

### Commit

Bir git commit'i Çalışma Ağacındaki bir takım değişiklerdir. Mesela 5 tane dosya
eklemişsindir ve diğer 2 tanesini silmişindir, bu değişikler commit'te (anlık kayıtta) 
tutulacaktır. Bu commit daha sonra diğer repo'lara bastırılabilir (pushed) ve bastırılmaz!

### Branch

Bir branch esasen yaptığın son commit'e göstericidir(pointer). Commit'lemeye devam ettiğinde,
bu gösterici otomatik olarak son commit'e güncellenir.

### Tag

Bir tag, tarihteki belirli bir noktanın işaretidir. İnsanlar bunu genelde 
sürüm notları için kullanır (v1.0 vs.)

### HEAD ve head (.git dizininin birleşenleri)

HEAD mevcut branch'a bir göstericidir. Bir repository yalnızca 1 *aktif*
HEAD'e sahiptir.
head, commit'e bir göstericidir. Bir repository herhangi bir sayıda head'e sahip olabilir.

### Git'in Stage'leri
* Modified - Dosyada değişikler yapıldı ama henüz Git Veritabanına commit yapılmadı.
* Staged - Modified edilmiş bir dosyayı, sonraki commit'e gitmek üzere işaretler.
* Committed - Dosyalar Git Veritabanına commit'lendi.  

### Kavramsal Kaynaklar 

* [Bilgisayar Bilimciler için Git](http://eagain.net/articles/git-for-computer-scientists/)
* [Tasarımcılar için Git](http://hoth.entp.com/output/git_for_designers.html)

## Komutlar

### init

Boş bir Git repository'si oluştur. Git repository'sinin ayarları, depolanmış
bilgileri ve daha fazlası ".git" adlı dizinde (bir klasör) tutulur.

```bash
$ git init
```

### config

Ayarları yapılandırmak için. Repository, sistemin kendisi veya global yapılandırmalar
için olarabilir. (global yapılandırma dosyası `~/.gitconfig`).

```bash
# Print & Set Some Basic Config Variables (Global)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[git config hakkında daha fazla bilgi için.](http://git-scm.com/docs/git-config)

### help

Her bir komutun detaylı kılavuzuna hızlı bir erişim için. Ya da sadece bazı şeylerin
anlamı için hızlı bir hatırlatıcı için.

```bash
# Quickly check available commands
$ git help

# Check all available commands
$ git help -a

# Command specific help - user manual
# git help <command_here>
$ git help add
$ git help commit
$ git help init
# or git <command_here> --help
$ git add --help
$ git commit --help
$ git init --help
```

### dosyaları ignore etme 

git'in bazı dosya(ları) ve klasör(leri) kasıtlı olarak takip etmemesi için. Genel
olarak,repository'de ne de olsa paylaşılacak, private ve temp dosyaları için.

```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```

### status

index dosyası(temelde çalıştığın repo) ve mevcut HEAD commit arasındaki farkı göstermek için.

```bash
# Will display the branch, untracked files, changes and other differences
$ git status

# To learn other "tid bits" about git status
$ git help status
```

### add

Dosyaları staging area'ya eklemek için. Eğer yeni dosyaları staging area'ya `git add` 
yapmazsanız, commit'lere eklenmez!

```bash
# add a file in your current working directory
$ git add HelloWorld.java

# add a file in a nested dir
$ git add /path/to/file/HelloWorld.c

# Regular Expression support!
$ git add ./*.java

# You can also add everything in your working directory to the staging area.
$ git add -A
```
Bu yalnızca dosyayı staging area'a/index'e ekler, çalışılan dizine/repo'ya commit etmez.

### branch

Branch'ları yönetir. Bu komutu kullanarak, branch'ları görebilir, düzenleyebilir, oluşturabilir, silebilirsin.

```bash
# list existing branches & remotes
$ git branch -a

# create a new branch
$ git branch myNewBranch

# delete a branch
$ git branch -d myBranch

# rename a branch
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# edit a branch's description
$ git branch myBranchName --edit-description
```

### tag

tag'leri yönetir

```bash
# List tags
$ git tag

# Create a annotated tag
# The -m specifies a tagging message, which is stored with the tag.
# If you don’t specify a message for an annotated tag,
# Git launches your editor so you can type it in.
$ git tag -a v2.0 -m 'my version 2.0'

# Show info about tag
# That shows the tagger information, the date the commit was tagged,
# and the annotation message before showing the commit information.
$ git show v2.0

# Push a single tag to remote
$ git push origin v2.0

# Push a lot of tags to remote
$ git push origin --tags
```

### checkout

index'in versiyonun eşlemek için çalışma ağacındaki,veya belirtilen ağactaki, tüm dosyaları günceller.

```bash
# Checkout a repo - defaults to master branch
$ git checkout

# Checkout a specified branch
$ git checkout branchName

# Create a new branch & switch to it
# equivalent to "git branch <name>; git checkout <name>"

$ git checkout -b newBranch
```

### clone

Varolan bir repository'i yeni bir dizine clone'lar veya kopyalar. 
Ayrıca clone'lanmış repodaki her bir branch için, uzak branch'a bastırmana izin veren,
uzak takip branch'ları ekler.

```bash
# Clone learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git

# shallow clone - faster cloning that pulls only latest snapshot
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git

# clone only a specific branch
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

index'in mevcut içeriğini yeni bir "commit"te saklar. Bu commit, kullanıcının oluşturduğu
bir mesajı ve yapılan değişiklikleri saklar. 

```bash
# commit with a message
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"

# signed commit with a message (user.signingkey must have been set
# with your GPG key e.g. git config --global user.signingkey 5173AAD5)
$ git commit -S -m "signed commit message"

# automatically stage modified or deleted files, except new files, and then commit
$ git commit -a -m "Modified foo.php and removed bar.php"

# change last commit (this deletes previous commit with a fresh commit)
$ git commit --amend -m "Correct message"
```

### diff

Shows differences between a file in the working directory, index and commits.
Bir dosyanın, çalışma ağacı, index ve commit'ler arasındaki farklarını göster.

```bash
# Show difference between your working dir and the index
$ git diff

# Show differences between the index and the most recent commit.
$ git diff --cached

# Show differences between your working dir and the most recent commit
$ git diff HEAD
```

### grep

Bir repository'de hızlıca arama yapmana izin verir.

İsteğe Bağlı Yapılandırmalar:

```bash
# Thanks to Travis Jeffery for these
# Set line numbers to be shown in grep search results
$ git config --global grep.lineNumber true

# Make search results more readable, including grouping
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Search for "variableName" in all java files
$ git grep 'variableName' -- '*.java'

# Search for a line that contains "arrayListName" and, "add" or "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Daha fazla örnek için
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Repository'deki commitleri gösterir.

```bash
# Show all commits
$ git log

# Show only commit message & ref
$ git log --oneline

# Show merge commits only
$ git log --merges

# Show all commits represented by an ASCII graph
$ git log --graph
```

### merge

Dış commit'lerdeki değişiklikleri mevcut branch'a "merge" et (birleştir).

```bash
# Merge the specified branch into the current.
$ git merge branchName

# Always generate a merge commit when merging
$ git merge --no-ff branchName
```

### mv

Bir dosyayı yeniden taşı veya yeniden adlandır

```bash
# Renaming a file
$ git mv HelloWorld.c HelloNewWorld.c

# Moving a file
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Force rename or move
# "existingFile" already exists in the directory, will be overwritten
$ git mv -f myFile existingFile
```

### pull

Bir repository'den çeker ve diğer branch'a merge eder.

```bash
# Update your local repo, by merging in new changes
# from the remote "origin" and "master" branch.
# git pull <remote> <branch>
$ git pull origin master

# By default, git pull will update your current branch
# by merging in new changes from its remote-tracking branch 
$ git pull

# Merge in changes from remote branch and rebase
# branch commits onto your local repo, like: "git fetch <remote> <branch>, git 
# rebase <remote>/<branch>"
$ git pull origin master --rebase
```

### push

Bir branch'taki değişikleri, uzak branch'a bastır ve birleştir.

```bash
# Push and merge changes from a local repo to a
# remote named "origin" and "master" branch.
# git push <remote> <branch>
$ git push origin master

# By default, git push will push and merge changes from
# the current branch to its remote-tracking branch 
$ git push

# To link up current local branch with a remote branch, add -u flag:
$ git push -u origin master
# Now, anytime you want to push from that same local branch, use shortcut:
$ git push
```

### stash

Stash'leme çalışma dizinindeki kirli durumu alır ve bitmemiş değişiklikler 
yığınına kaydeder. Bu değişikleri istediğin zaman tekrar uygulayabilirsin.

Mesela git repo'nda bazı işler yaptın ama remote'dan pull yapmak istiyorsun.
Bazı dosyalarında kirli (commit'lenmemiş) değişiklikler olduğundan `git pull`
yapamazsın. Onun yerine önce `git stash` ile değişikliklerini yığına kaydet!

(stash, sözlük anlamı: bir şeyi, özel bir yere güvenli biçimde saklamak)

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Şimdi pull yapabilirsin!

```bash
git pull
```
`...changes apply...`

Herşeyin tamam olduğunu kontrol et

```bash
$ git status
# On branch master
nothing to commit, working directory clean
```
Şu ana kadar neleri stash'lediğini `git stash list` kullanarak görebilirsin.
Stash'lenen şeyler Son-Giren-İlk-Çıkar şeklinde tutulduğundan en son değişim
en üste olacaktır.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```
Şimdi de kirli değişiklileri yığından çıkarıp uygulayalım.

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

`git stash apply` da aynı şeyi yapar

Şimdi kendi işine dönmeye hazırsın!

[Ek Okuma.](http://git-scm.com/book/en/v1/Git-Tools-Stashing)

### rebase (dikkat)

Branch'ta commit'lenen tüm değişimleri al ve onları başka bir branch'ta tekrar oynat
*Public repo'ya push edilmiş commit'leri rebase etme*

```bash
# Rebase experimentBranch onto master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Ek Okuma.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (dikkat)

Reset the current HEAD to the specified state. This allows you to undo merges,
pulls, commits, adds, and more. It's a great command but also dangerous if you 
don't know what you are doing.

HEAD'i belirtilen duruma resetle. Bu merge'leri, pull'ları, commit'leri, add'leri
ve daha fazlasını geriye almanı sağlar. Muhteşem bir komuttur ama aynı zamanda, ne
yaptığını bilmiyorsan, tehlikelidir.

```bash
# Reset the staging area, to match the latest commit (leaves dir unchanged)
$ git reset

# Reset the staging area, to match the latest commit, and overwrite working dir
$ git reset --hard

# Moves the current branch tip to the specified commit (leaves dir unchanged)
# all changes still exist in the directory.
$ git reset 31f2bb1

# Moves the current branch tip backward to the specified commit
# and makes the working dir match (deletes uncommitted changes and all commits
# after the specified commit).
$ git reset --hard 31f2bb1
```

### reflog (dikkat)

Reflog, verilen zaman içinde,default olarak 90 gündür, yaptığın git komutlarını listeler.

Bu sana beklemediğin şekilde yanlış giden komutları geriye çevirme şansı verir. 
(mesela, eğer bir rebase uygulamanı kırdıysa)

Şu şekilde yapıbilirsin:

1. `git reflog` rebase için tüm git komutlarını listele

```
38b323f HEAD@{0}: rebase -i (finish): returning to refs/heads/feature/add_git_reflog
38b323f HEAD@{1}: rebase -i (pick): Clarify inc/dec operators
4fff859 HEAD@{2}: rebase -i (pick): Update java.html.markdown
34ed963 HEAD@{3}: rebase -i (pick): [yaml/en] Add more resources (#1666)
ed8ddf2 HEAD@{4}: rebase -i (pick): pythonstatcomp spanish translation (#1748)
2e6c386 HEAD@{5}: rebase -i (start): checkout 02fb96d
```
2. Nereye reset'leyeceğini seç, şu durumda `2e6c386` veya `HEAD@{5}`
3. 'git reset --hard HEAD@{5}' bu repo'nu seçilen head'e eşitler 
4. Rebase'e yeniden başlayabilir veya onu yalnız bırakabilirsin.

[Ek Okuma.](https://git-scm.com/docs/git-reflog)

### revert

Revert commit'leri geri almada kullanılır. Projenin durumunu önceki bir noktaya 
alan reset ile karıştırılmamalıdır. Revert, belirtilen commit'in tersine yeni bir
commit ekleyecektir.

```bash
# Revert a specified commit
$ git revert <commit>
```

### rm

git add'in tersine, git rm çalışma ağacından dosyaları kaldırır.

```bash
# remove HelloWorld.c
$ git rm HelloWorld.c

# Remove a file from a nested dir
$ git rm /pather/to/the/file/HelloWorld.c
```

## Daha Fazla Bilgi

* [tryGit - Git'i öğrenmek için eğlenceli interaktif bir yol](http://try.github.io/levels/1/challenges/1)

* [Git Dallanmayı Öğren -  Git'i web üzerinde öğrenmek için en görsel ve interaktif yol](http://learngitbranching.js.org/)

* [Udemy Git Tutorial: Kapsayıcı bir kılavuz](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [Git Immersion - Git'in temelinden başlayan bir tur](http://gitimmersion.com/)

* [git-scm - Video Tutorial](http://git-scm.com/videos)

* [git-scm - Dökümantasyon](http://git-scm.com/docs)

* [Atlassian Git - Tutorial & Workflow](https://www.atlassian.com/git/)

* [SalesForce Kopya Kağıdı](http://res.cloudinary.com/hy4kyit2a/image/upload/SF_git_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [Git - Basit bir kılavuz](http://rogerdudler.github.io/git-guide/index.html)

* [Pro Git](http://www.git-scm.com/book/en/v2)

* [Yeni başlayanlar için Git ve Github](http://product.hubspot.com/blog/git-and-github-tutorial-for-beginners)
