---
category: tool
tool: git
lang: pt-br
filename: LearnGit-br.txt
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
translators:
  - ["Suzane Sant Ana", "http://github.com/suuuzi"]
  - ["Bruno Volcov", "http://github.com/volcov"]
  - ["Marcel Ribeiro-Dantas", "http://github.com/mribeirodantas"]
---

Git é um sistema distribuido de gestão para código fonte e controle de versões.

Funciona através de uma série de registos de estado do projeto e usa esse
registo para permitir funcionalidades de versionamento e gestão de código
fonte.

## Conceitos de versionamento

### O que é controle de versão

Controle de versão (*source control*) é um processo de registo de alterações
a um arquivo ou conjunto de arquivos ao longo do tempo.

### Controle de versão:  Centralizado VS Distribuído

* Controle de versão centralizado foca na sincronização, registo e *backup*
de arquivos.
* Controle de versão distribuído foca em compartilhar alterações. Cada
alteração é associada a um *id* único.
* Sistemas distribuídos não têm estrutura definida. É possivel ter um sistema
centralizado ao estilo SVN usando git.

[Informação adicional (EN)](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Por que usar git?

* Permite trabalhar offline.
* Colaborar com outros é fácil!
* Criar *branches* é fácil!
* Fazer *merge* é fácil!
* Git é rápido.
* Git é flexivel.

## Git - Arquitetura


### Repositório

Um conjunto de arquivos, diretórios, registos históricos, *commits* e
referências. Pode ser descrito como uma estrutura de dados de código fonte
com a particularidade de cada elemento do código fonte permitir acesso ao
histórico das suas alterações, entre outras coisas.

Um repositório git é constituído pelo diretório .git e a *working tree*

### Diretório .git (componente do repositório)

O repositório .git contém todas as configurações, *logs*, *branches*,
referências e outros.

[Lista detalhada (EN)](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### *Working Tree* (componente do repositório)

A *Working Tree* é basicamente a listagem dos diretórios e arquivos do
repositório. É chamada também de diretório do projeto.

### *Index* (componente do diretório .git)

O *Index* é a camada da interface no git. É o elemento que separa
o diretório do projeto do repositório git. Isto permite aos programadores um
maior controle sobre o que é registado no repositório git.

### *Commit*

Um *commit** de git é um registo de um conjunto de alterações ou manipulações
nos arquivos do projeto. Por exemplo, ao adicionar cinco arquivos e remover
outros 2, estas alterações serão gravadas em um *commit* (ou registro). Este
*commit* pode então ser enviado para outros repositórios, caso você queira.

### *Branch*

Uma *branch* é essencialmente uma referência que aponta para o último *commit*
efetuado. Na medida que são feitos novos commits, esta referência é atualizada
automaticamente e passa a apontar para o commit mais recente.

### *Tag*

Uma tag é uma marcação em um ponto específico da história. Geralmente as
pessoas usam esta funcionalidade para marcar pontos de release (v2.0, e por aí
vai)

### *HEAD* e *head* (componentes do diretório .git)

*HEAD* é a referência que aponta para a *branch* em uso. Um repositório só tem
uma *HEAD* activa. *head* é uma referência que aponta para qualquer *commit*. Um
repositório pode ter um número indefinido de *heads*.

### Estados no Git

* Modificado (Modified): Ocorreram mudanças em ao menos um arquivo mas essas
mudanças ainda não foram registradas na base de dados do Git
* Preparado (Staged): Marca o arquivo como preparado para ser adicionado ao
próximo commit
* Consolidado (Committed): As mudanças foram registradas na base de dados do
Git

### Recursos conceituais (EN)

* [Git para Cientistas de Computação](http://eagain.net/articles/git-for-computer-scientists/)
* [Git para Designers](http://hoth.entp.com/output/git_for_designers.html)

## Comandos

### *init*

Cria um repositório Git vazio. As configurações do repositório, e outras
informações são guardadas em uma pasta dentro do repositório com o nome ".git".

```bash
$ git init
```

### *config*

Permite configurar o git, seja com respeito a definições deste repositório,
do sistema ou configurações globais para todos os repositórios.

```bash
# Define e imprime algumas variáveis de configuração básicas (global)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"

$ git config --global user.email
$ git config --global user.name
```

[Aprenda mais sobre git config. (EN)](http://git-scm.com/docs/git-config)

### help

Para visualizar rapidamente o detalhamento de cada comando ou apenas lembrar da
semântica.

```bash
# Ver rapidamente os comandos disponiveis
$ git help

# Ver todos os comandos disponíveis
$ git help -a

# Usar o *help* para um comando específico
# git help <comando_aqui>
$ git help add
$ git help commit
$ git help init
# ou git <comando_aqui> --help
$ git add --help
$ git commit --help
$ git init --help
```

### Ignorando arquivos

É possível intencionalmente ignorar arquivos e pastas. Costuma-se utilizar o
arquivo .gitignore para fazer o git ignorar a existência de arquivos e pastas
geralmente utilizados para arquivos privados ou temporários que, de outro modo,
seriam compartilhados com todos que tem acesso ao repositório.

```bash
$ echo "temp/" >> .gitignore
$ echo "chave_privada" >> .gitignore
```

### status

Apresenta as diferenças entre o arquivo *index* (a versão corrente do
repositório) e o *commit* da *HEAD* atual.


```bash
# Apresenta a *branch*, arquivos não monitorados, alterações e outras
# diferenças
$ git status

# Para aprender mais detalhes sobre git *status*
$ git help status
```

### add

Adiciona arquivos ao repositório corrente. Se os arquivos novos não forem
adicionados através de `git add` ao repositório, então eles não serão
incluidos nos commits!

```bash
# adiciona um arquivo no diretório do projeto atual
$ git add OlaMundo.java

# adiciona um arquivo em um sub-diretório
$ git add /caminho/para/arquivo/OlaMundo.java

# permite usar expressões regulares!
$ git add ./*.java

# Você também pode adicionar tudo no seu diretório de trabalho como alterações
prontas para o próximo commit.
$ git add -A
```

Esse comando apenas adiciona os arquivos no estado de preparados para o próximo
commit, mas não realiza o commit de fato.

### branch

Gerencia as *branches*. É possível ver, editar, criar e apagar branches com este
comando.

```bash
# listar *branches* existentes e remotos
$ git branch -a

# criar uma nova *branch*
$ git branch myNewBranch

# apagar uma *branch*
$ git branch -d myBranch

# alterar o nome de uma *branch*
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# editar a descrição de uma *branch*
$ git branch myBranchName --edit-description
```

### Tag

Gerencia as *tags*

```bash
# Listar tags
$ git tag

# Criar uma tag anotada.
# O parâmetro -m define uma mensagem, que é armazenada com a tag.
# Se você não especificar uma mensagem para uma tag anotada,
# o Git vai rodar seu editor de texto para você digitar alguma coisa.
$ git tag -a v2.0 -m 'minha versão 2.0'

# Mostrar informações sobre a tag
# O comando mostra a informação da pessoa que criou a tag,
# a data de quando o commit foi taggeado,
# e a mensagem antes de mostrar a informação do commit.
$ git show v2.0

# Enviar uma tag para o repositório remoto
$ git push origin v2.0

# Enviar várias tags para o repositório remoto
$ git push origin --tags
```

### checkout

Atualiza todos os arquivos no diretório do projeto para que fiquem iguais
à versão do index ou da *branch* especificado.

```bash
# Checkout de um repositório - por padrão para a branch master
$ git checkout

# Checkout de uma branch especifica
$ git checkout branchName

# Cria uma nova branch e faz checkout para ela.
# Equivalente a: "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Clona ou copia um repositório existente para um novo diretório. Também
adiciona *branches* no repositório clonado para cada *branch* no repositório
remoto o que permite enviar alterações para uma *branch* no repositório remoto.

```bash
# Clona learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git

# Clone superficial - É mais rápido, mas puxa apenas o último commit
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git

# Clona apenas uma branch em específica
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

### commit

Guarda o conteúdo atual do index em um novo *commit*. Este *commit* contém
as alterações feitas e a mensagem criada pelo usuário.

```bash
# Realiza um commit com uma mensagem
$ git commit -m "Adicione a função multipliqueNumeros() em OlaMundo.c"

# Assine um commit com sua chave GPG. Antes disso, você precisa ter
# configurado a opção user.signkey do git com o comando:
# git config --global user.signinkey ID_CHAVE_AQUI
$ git commit -S -m "mensagem do commit aqui"

# Automaticamente adicione como preparados arquivos modificados ou apagados e
# então realize um commit:
$ git commit -a -m "Modified foo.php and removed bar.php"

# Altere o último commit (Esse comando cria um novo commit com o conteúdo do
# commit anterior mais suas novas alterações e sobrescreve o último commit)
$ git commit --amend -m "Correct message"
```

### diff

Apresenta as diferenças entre um arquivo no repositório do projeto, *index*
e *commits*

```bash
# Apresenta a diferença entre o diretório atual e o index
$ git diff

# Apresenta a diferença entre o index e os commits mais recentes
$ git diff --cached

# Apresenta a diferença entre o diretório atual e o commit mais recente
$ git diff HEAD
```

### grep

Permite procurar facilmente em um repositório.

Configurações opcionais:

```bash
# Define a apresentação de números de linha nos resultados do grep
$ git config --global grep.lineNumber true

# Agrupa os resultados da pesquisa para facilitar a leitura
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Pesquisa por "variableName" em todos os arquivos java
$ git grep 'variableName' -- '*.java'

# Pesquisa por uma linha que contém "arrayListName" e "add" ou "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

O Google é seu amigo; para mais exemplos:
[Git Grep Ninja (EN)](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Apresenta commits do repositório.

```bash
# Apresenta todos os commits
$ git log

# Apresenta apenas mensagem do commit e referência
$ git log --oneline

# Apresenta apenas commits de merge
$ git log --merges

# Apresenta todos os commits representados por um gráfico em ASCII
$ git log --graph
```

### merge

"Merge" junta as alterações de commits externos com a *branch* atual.

```bash
# Junta a branch especificada com o atual
$ git merge branchName

# Para gerar sempre um commit ao juntar as branches
$ git merge --no-ff branchName
```

### mv

Altera o nome ou move um arquivo.

```bash
# Alterar o nome de um arquivo
$ git mv OlaMundo.c OlaNovoMundo.c

# Mover um arquivo
$ git mv OlaMundo.c ./novo/caminho/OlaMundo.c

# Forçar a alteração de nome ou mudança de local
# Se o arquivo já existir no diretório, será sobrescrito.
$ git mv -f myFile existingFile
```

### pull

Puxa alterações de um repositório e as junta com outra branch

```bash
# Atualiza o repositório local, juntando as novas alterações
# do repositório remoto 'origin' e branch 'master'
# git pull <remote> <branch>
$ git pull origin master

# Por padrão, o git irá realizar o pull na branch atual fazendo um merge
# com as alterações novas na branch remota
$ git pull

# Juntar alterações da branch remote e fazer rebase commits da branch
# no repositório local, como: "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Enviar e juntar alterações de uma branch para a sua branch correspondente
em um repositório remoto.

```bash
# Envia e junta as alterações de um repositório local
# para um remoto denominado "origin" na branch "master".
# git push <remote> <branch>
$ git push origin master

# Por padrão, o git push irá enviar e realizar merge das mudanças da sua branch
# local com a branch remota
$ git push

# Para associar a branch local com uma branch específica remota, adicione a
# flag -u
$ git push -u origin master

# Agora, sempre que você realizar um push daquela branch local, use o atalho:
$ git push
```

### stash

O objetivo desse comando do git é pegar o estado "sujo" do seu diretório de
trabalho, que não está pronto (staged), e salvá-lo em um outro lugar para que
você possa trabalhar no seu repositório do zero, mas sem perder as mudanças que
fez. Em qualquer outro momento que você quiser, você pode trazer de volta as
alterações que você tirou dali com o comando stash.

Digamos que você tem feito algumas alterações no seu repositório, mas agora
você quer realizar um pull do repositório remoto. Como você tem alterações não
commitadas no seu diretório (ele está "sujo"), você não irá conseguir realizar
o `git pull` com sucesso. Mas você pode executar o `git stash` para salvar
essas alterações e conseguir realizar o pull. Depois, você traz de volta.

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

Agora você irá conseguir realizar o pull!

```bash
git pull
```

`...changes apply...`

Verifique se está tudo OK com o `git status`

```bash
$ git status
# On branch master
nothing to commit, working directory clean
```

É possível verificar as alterações que você guardou com cada uso do comando
`git stash` com o `git stash list`. Como os usos desse comando são guardados
como em uma pilha, os usos mais recentes estarão no topo.

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

Agora vamos trazer o que havíamos salvado com o `git stash` de volta para o
diretório de trabalho.

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

`git stash apply` faz a mesma coisa

Agora podemos voltar a trabalhar no que havíamos deixado de lado!

[Additional Reading.](http://git-scm.com/book/en/v1/Git-Tools-Stashing)

### rebase (cautela!)

Pega em todas as alterações que foram registadas em uma branch e volta a
aplicá-las em outra branch.
*Não deve ser feito rebase de commits que foram enviados para um repositório
público*

```bash
# Faz Rebase de experimentBranch para master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Leitura adicional (EN).](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (cuidado!)

Restabelece a HEAD atual ao estado definido. Isto permite reverter *merges*,
*pulls*, *commits*, *adds* e outros. É um comando muito poderoso mas também
perigoso quando não há certeza do que se está fazendo.

```bash
# Restabelece a camada intermediária de registo para o último
# commit (o diretório fica sem alterações)
$ git reset

# Restabelece a camada intermediária de registo para o último commit, e
# sobrescreve o projeto atual
$ git reset --hard

# Move a head da branch atual para o commit especificado, sem alterar o projeto.
# todas as alterações ainda existem no projeto
$ git reset 31f2bb1

# Inverte a head da branch atual para o commit especificado
# fazendo com que este esteja em sintonia com o diretório do projeto
# Remove alterações não registradas e todos os commits após o commit
# especificado
$ git reset --hard 31f2bb1
```

### reflog (cuidado!)

O reflog irá listar a maior parte dos comandos que você realizou no git em um
determinado intervalo de tempo. O intervalo padrão é de 90 dias.

Isso te dá a oportunidade de reverter qualquer comando que você realizou no git
e que tenha tido consequências indesejadas (por exemplo, se um rebase quebrou
sua aplicação).

Você pode fazer assim:

1. `git reflog` para listar todos os comandos utilizados no git para o rebase

```
38b323f HEAD@{0}: rebase -i (finish): returning to refs/heads/feature/add_git_reflog
38b323f HEAD@{1}: rebase -i (pick): Clarify inc/dec operators
4fff859 HEAD@{2}: rebase -i (pick): Update java.html.markdown
34ed963 HEAD@{3}: rebase -i (pick): [yaml/en] Add more resources (#1666)
ed8ddf2 HEAD@{4}: rebase -i (pick): pythonstatcomp spanish translation (#1748)
2e6c386 HEAD@{5}: rebase -i (start): checkout 02fb96d
```

2. Selecione para onde você quer resetar. No nosso exemplo, seria o commit
`2e6c386`, ou `HEAD@{5}`
3. 'git reset --hard HEAD@{5}' esse comando irá resetar o seu repositório para
aquele ponto (5 commits atrás).
4. Agora você pode recomeçar o rebase ou apenas deixar como está.

[Leitura complementar](https://git-scm.com/docs/git-reflog)

### revert

O comando revert pode ser utilizado para desfazer um commit. Não deve ser
confundido com o comando reset que restabelece o estado do repositório para um
momento anterior. O revert irá criar um novo commit com alterações inversas a
de um outro commit especificado, portanto revertendo aquelas alterações.

```bash
# Revertendo um commit específico
$ git revert <commit>
```

### rm

O oposto de git add, git rm remove arquivos da branch atual.

```bash
# remove OlaMundo.c
$ git rm OlaMundo.c

# Remove um arquivo de um sub-diretório
$ git rm /caminho/para/o/arquivo/OlaMundo.c
```

## Leitura complementar

* [Configurar o Git (GitHub Docs)](https://docs.github.com/pt/get-started/quickstart/set-up-git)

* [Learn Git Branching - the most visual and interactive way to learn Git on the web](http://learngitbranching.js.org/)

* [Udemy Git Tutorial: A Comprehensive Guide](https://blog.udemy.com/git-tutorial-a-comprehensive-guide/)

* [Git Immersion - A Guided tour that walks through the fundamentals of git](http://gitimmersion.com/)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [Git - the simple guide](http://rogerdudler.github.io/git-guide/index.html)

* [Pro Git (em Português)](https://www.git-scm.com/book/pt-br/v2)

* [An introduction to Git and GitHub for Beginners (Tutorial)](http://product.hubspot.com/blog/git-and-github-tutorial-for-beginners)

* [The New Boston tutorial to Git covering basic commands and workflow](https://www.youtube.com/playlist?list=PL6gx4Cwl9DGAKWClAD_iKpNC0bGHxGhcx)
