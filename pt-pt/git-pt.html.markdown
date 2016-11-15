---
category: tool
tool: git
lang: pt-pt
filename: LearnGit-pt.txt
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
  - ["Rafael Jegundo", "http://rafaeljegundo.github.io/"]
---

Git é um sistema distribuido de gestão para código fonte e controlo de versões.

Funciona através de uma série de registos de estado do projecto e usa esse
registo para permitir funcionalidades de versionamento e gestão de código
fonte.

## Conceitos de versionamento

### O que é controlo de versões

Controlo de versões (*source control*) é um processo de registo de alterações
a um ficheiro ou conjunto de ficheiros ao longo do tempo.

### Controlo de versões:  Centralizado VS Distribuido

* Controlo de versões centralizado foca-se na sincronização, registo e *backup*
de ficheiros.
* Controlo de versões distribuido foca-se em partilhar alterações. Cada
alteração é associada a um *id* único.
* Sistemas distribuidos não têm estrutura definida. É possivel ter um sistema
centralizado ao estilo SVN usando git.

[Informação adicional (EN)](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Porquê usar git?

* Permite trabalhar offline.
* Colaborar com outros é fácil!
* Criar *branches* é fácil!
* Fazer *merge* é fácil!
* Git é rápido.
* Git é flexivel.

## Git - Arquitectura


### Repositório

Um conjunto de ficheiros, directórios, registos históricos, *commits* e
referências. Pode ser imaginado como uma estrutura de dados de código fonte
com a particularidade de cada elemento do código fonte permitir acesso ao
histórico das suas alterações, entre outras coisas.

Um repositório git é constituido pelo directório .git e a *working tree*

### Directório .git (componente do repositório)

O repositório .git contém todas as configurações, *logs*, *branches*,
referências e outros.

[Lista detalhada (EN)](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### *Working Tree* (componente do repositório)

Isto é basicamente os directórios e ficheiros do repositório. É frequentemente
referido como o directório do projecto.

### *Index* (componente do directório .git)

O *Index* é a camada de interface no git. Consistente num elemento que separa
o directório do projecto do repositório git. Isto permite aos programadores um
maior controlo sobre o que é registado no repositório git.

### *Commit*

Um *commit** de git é um registo de um cojunto de alterações ou manipulações nos ficheiros do projecto.
Por exemplo, ao adicionar cinco ficheiros e remover outros 2, estas alterações
serão gravadas num *commit* (ou registo). Este *commit* pode então ser enviado
para outros repositórios ou não!

### *Branch*

Um *branch* é essencialmente uma referência que aponta para o último *commit*
efetuado. À medida que são feitos novos commits, esta referência é atualizada
automaticamente e passa a apontar para o commit mais recente.

### *HEAD* e *head* (componentes do directório .git)

*HEAD* é a referência que aponta para o *branch* em uso. Um repositório só tem
uma *HEAD* activa.
*head* é uma referência que aponta para qualquer *commit*. Um repositório pode
ter um número indefinido de *heads*

### Recursos conceptuais (EN)

* [Git para Cientistas de Computação](http://eagain.net/articles/git-for-computer-scientists/)
* [Git para Designers](http://hoth.entp.com/output/git_for_designers.html)

## Comandos

### *init*

Cria um repositório Git vazio. As definições, informação guardada e outros do
repositório git são guardados num directório (pasta) denominado ".git".

```bash
$ git init
```

### *config*

Permite configurar as definições, sejam as definições do repositório, sistema
ou configurações globais.

```bash
# Imprime e define algumas variáveis de configuração básicas (global)
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Aprenda mais sobre git config. (EN)](http://git-scm.com/docs/git-config)

### help

Para aceder rapidamente a um guia extremamente detalhada sobre cada comando.
Ou para dar apenas uma lembraça rápida de alguma semântica.

```bash
# Ver rapidamente os comandos disponiveis
$ git help

# Ver todos os comandos disponiveis
$ git help -a

# Requerer *help* sobre um comando especifico - manual de utilizador
# git help <command_here>
$ git help add
$ git help commit
$ git help init
```

### status

Apresenta as diferenças entre o ficheiro *index* (no fundo a versão corrente
do repositório) e o *commit* da *HEAD* atual.


```bash
# Apresenta o *branch*, ficheiros não monitorizados, alterações e outras
# difereças
$ git status

# Para aprender mais detalhes sobre git *status*
$ git help status
```

### add

Adiciona ficheiros ao repositório corrente. Se os ficheiros novos não forem
adicionados através de `git add` ao repositório, então eles não serão
incluidos nos commits!

```bash
# adiciona um ficheiro no directório do projecto atual
$ git add HelloWorld.java

# adiciona um ficheiro num sub-directório
$ git add /path/to/file/HelloWorld.c

# permite usar expressões regulares!
$ git add ./*.java
```

### branch

Gere os *branches*. É possível ver, editar, criar e apagar branches com este
comando.

```bash
# listar *branches* existentes e remotos
$ git branch -a

# criar um novo *branch*
$ git branch myNewBranch

# apagar um *branch*
$ git branch -d myBranch

# alterar o nome de um *branch*
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# editar a descrição de um *branch*
$ git branch myBranchName --edit-description
```

### checkout

Atualiza todos os ficheiros no directório do projecto de forma a ficarem iguais
à versão do index ou do *branch* especificado.

```bash
# Checkout de um repositório - por predefinição para o branch master
$ git checkout
# Checkout de um branch especifico
$ git checkout branchName
# Cria um novo branch e faz checkout para ele.
# Equivalente a: "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Clona ou copia um repositório existente para um novo directório. Também
adiciona *branches* de monitorização remota para cada *branch* no repositório
clonado o que permite enviar alterações para um *branch* remoto.

```bash
# Clona learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
```

### commit

Guarda o conteudo atual do index num novo *commit*. Este *commit* contém
as alterações feitas e a mensagem criada pelo utilizador.

```bash
# commit com uma mensagem
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"
```

### diff

Apresenta as diferenças entre um ficheiro no repositório do projecto, *index*
e *commits*

```bash
# Apresenta a diferença entre o directório atual e o index
$ git diff

# Apresenta a diferença entre o index e os commits mais recentes
$ git diff --cached

# Apresenta a diferença entre o directório atual e o commit mais recente
$ git diff HEAD
```

### grep

Permite procurar facilmente num repositório

Configurações opcionais:

```bash
# Obrigado a Travis Jeffery por estas
# Define a apresentação de números de linha nos resultados do grep
$ git config --global grep.lineNumber true

# Torna os resultados da pesquisa mais fáceis de ler, agrupando-os
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Pesquisa por "variableName" em todos os ficheiros de java
$ git grep 'variableName' -- '*.java'

# Pesquisa por uma linha que contém "arrayListName" e "add" ou "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Google é teu amigo; para mais exemplos:
[Git Grep Ninja (EN)](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Apresenta commits do repositório.

```bash
# Apresenta todos os commits
$ git log

# Apresenta X commits
$ git log -n 10

# Apresenta apenas commits de merge
$ git log --merges
```

### merge

"Merge" (junta) as alterações de commits externos com o *branch* atual.

```bash
# Junta o branch especificado com o atual
$ git merge branchName

# Para gerar sempre um commit ao juntar os branches
$ git merge --no-ff branchName
```

### mv

Alterar o nome ou mover um ficheiro.

```bash
# Alterar o nome de um ficheiro
$ git mv HelloWorld.c HelloNewWorld.c

# Mover um ficheiro
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Forçar a alteração de nome ou mudança local
# "existingFile" já existe no directório, será sobre-escrito.
$ git mv -f myFile existingFile
```

### pull

Puxa alterações de um repositório e junta-as com outro branch

```bash
# Atualiza o repositório local, juntando as novas alterações
# do repositório remoto 'origin' e branch 'master'
# git pull <remote> <branch>
# git pull => aplica a predefinição => git pull origin master
$ git pull origin master

# Juntar alterações do branch remote e fazer rebase commits do branch
# no repositório local, como: "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Enviar e juntar alterações de um branch para o seu branch correspondente
num repositório remoto.

```bash
# Envia e junta as alterações de um repositório local
# para um remoto denominado "origin" no branch "master".
# git push <remote> <branch>
# git push => aplica a predefinição => git push origin master
$ git push origin master
```

### rebase (cautela!)

Pega em todas as alterações que foram registadas num branch e volta a
aplicá-las em outro branch.
*Não deve ser feito rebase de commits que foram enviados para um repositório
público*

```bash
# Faz Rebase de experimentBranch para master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Additional Reading (EN).](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (cautela!)

Restabelece a HEAD atual ao estado definido. Isto permite reverter *merges*,
*pulls*, *commits*, *adds* e outros. É um comando muito poderoso mas também
perigoso se não há certeza quanto ao que se está a fazer.

```bash
# Restabelece a camada intermediária de registo para o último
# commit (o directório fica sem alterações)
$ git reset

# Restabelece a camada intermediária de registo para o último commit, e
# sobre-escreve o projecto atual
$ git reset --hard

# Move a head do branch atual para o commit especificado, sem alterar o projecto.
# todas as alterações ainda existem no projecto
$ git reset 31f2bb1

# Inverte a head do branch atual para o commit especificado
# fazendo com que este esteja em sintonia com o directório do projecto
# Remove alterações não registadas e todos os commits após o commit especificado
$ git reset --hard 31f2bb1
```

### rm

O oposto de git add, git rm remove ficheiros do branch atual.

```bash
# remove HelloWorld.c
$ git rm HelloWorld.c

# Remove um ficheiro de um sub-directório
$ git rm /pather/to/the/file/HelloWorld.c
```

## Informação complementar (EN)

* [tryGit - A fun interactive way to learn Git.](http://try.github.io/levels/1/challenges/1)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)
