---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
   	- ["Miguel Araújo", "https://github.com/miguelarauj1o"]
lang: pt-br
filename: learngit-pt.txt
---

Git é um sistema de controle de versão distribuído e de gerenciamento de código-fonte.

Ele faz isso através de uma série de momentos instantâneos de seu projeto, e ele funciona 
com esses momentos para lhe fornecer a funcionalidade para a versão e 
gerenciar o seu código-fonte.

## Versionando Conceitos

### O que é controle de versão?

O controle de versão é um sistema que registra alterações em um arquivo ou conjunto 
de arquivos, ao longo do tempo.

### Versionamento Centralizado VS Versionamento Distribuído

* Controle de versão centralizado concentra-se na sincronização, controle e backup de arquivos.
* Controle de versão distribuído concentra-se na partilha de mudanças. Toda mudança tem um ID único.
* Sistemas Distribuídos não têm estrutura definida. Você poderia facilmente ter um estilo SVN, 
sistema centralizado, com git.

[Informação Adicional](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Porque usar o Git?

* Possibilidade de trabalhar offline
* Colaborar com os outros é fácil!
* Ramificação é fácil
* Mesclagem é fácil
* Git é rápido
* Git é flexível.

## Arquitetura Git

### Repositório

Um conjunto de arquivos, diretórios, registros históricos, cometes, e cabeças. Imagine-o 
como uma estrutura de dados de código-fonte, com o atributo que cada "elemento" do 
código-fonte dá-lhe acesso ao seu histórico de revisão, entre outras coisas.

Um repositório git é composto do diretório git. e árvore de trabalho.

### Diretório .git (componente do repositório)

O diretório git. contém todas as configurações, registros, galhos, cabeça(HEAD) e muito mais.
[Lista Detalhada](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Árvore de trabalho (componente do repositório)

Esta é, basicamente, os diretórios e arquivos no seu repositório. Ele é muitas vezes referida 
como seu diretório de trabalho.

### Índice (componente do diretório .git)

O Índice é a área de teste no git. É basicamente uma camada que separa a sua árvore de trabalho 
a partir do repositório Git. Isso dá aos desenvolvedores mais poder sobre o que é enviado para o
repositório Git.

### Comete (commit)

A commit git é um instantâneo de um conjunto de alterações ou manipulações a sua árvore de trabalho. 
Por exemplo, se você adicionou 5 imagens, e removeu outros dois, estas mudanças serão contidas 
em um commit (ou instantâneo). Esta confirmação pode ser empurrado para outros repositórios, ou não!

### Ramo (branch)

Um ramo é, essencialmente, um ponteiro que aponta para o último commit que você fez. Como 
você se comprometer, este ponteiro irá atualizar automaticamente e apontar para o último commit.

### Cabeça (HEAD) e cabeça (head) (componente do diretório .git)

HEAD é um ponteiro que aponta para o ramo atual. Um repositório tem apenas 1 * ativo * HEAD. 
head é um ponteiro que aponta para qualquer commit. Um repositório pode ter qualquer número de commits.

### Recursos Conceituais

* [Git para Cientistas da Computação](http://eagain.net/articles/git-for-computer-scientists/)
* [Git para Designers](http://hoth.entp.com/output/git_for_designers.html)

## Comandos

### init

Criar um repositório Git vazio. As configurações do repositório Git, informações armazenadas, 
e mais são armazenados em um diretório (pasta) com o nome ". git".

```bash
$ git init
```

### config

Para configurar as definições. Quer seja para o repositório, o próprio sistema, ou 
configurações globais.

```bash
# Impressão e definir algumas variáveis ​​de configuração básica (global)
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Saiba mais sobre o git config.](http://git-scm.com/docs/git-config)

### help

Para lhe dar um acesso rápido a um guia extremamente detalhada de cada comando. ou 
apenas dar-lhe um rápido lembrete de algumas semânticas.

```bash
# Rapidamente verificar os comandos disponíveis
$ git help

# Confira todos os comandos disponíveis
$ git help -a

# Ajuda específica de comando - manual do usuário
# git help <command_here>
$ git help add
$ git help commit
$ git help init
```

### status

Para mostrar as diferenças entre o arquivo de índice (basicamente o trabalho de 
copiar/repo) e a HEAD commit corrente.

```bash
# Irá exibir o ramo, os arquivos não monitorados, as alterações e outras diferenças
$ git status

# Para saber outras "tid bits" sobre git status
$ git help status
```

### add

Para adicionar arquivos para a atual árvore/directory/repo trabalho. Se você não 
der `git add` nos novos arquivos para o trabalhando árvore/diretório, eles não serão 
incluídos em commits!

```bash
# Adicionar um arquivo no seu diretório de trabalho atual
$ git add HelloWorld.java

# Adicionar um arquivo em um diretório aninhado
$ git add /path/to/file/HelloWorld.c

# Suporte a expressões regulares!
$ git add ./*.java
```

### branch

Gerenciar seus ramos. Você pode visualizar, editar, criar, apagar ramos usando este comando.

```bash
# Lista ramos e controles remotos existentes
$ git branch -a

# Criar um novo ramo
$ git branch myNewBranch

# Apagar um ramo
$ git branch -d myBranch

# Renomear um ramo
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# Editar a descrição de um ramo
$ git branch myBranchName --edit-description
```

### checkout

Atualiza todos os arquivos na árvore de trabalho para corresponder à versão no 
índice, ou árvore especificada.

```bash
# Finalizar um repo - padrão de ramo mestre
$ git checkout
# Checa um ramo especificado
$ git checkout branchName
# Criar um novo ramo e mudar para ela, como: "<nome> git branch; git checkout <nome>"
$ git checkout -b newBranch
```

### clone

Clones, ou cópias, de um repositório existente para um novo diretório. Ele também adiciona 
filiais remotas de rastreamento para cada ramo no repo clonado, que permite que você empurre 
a um ramo remoto.

```bash
# Clone learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
```

### commit

Armazena o conteúdo atual do índice em um novo "commit". Este commit contém 
as alterações feitas e uma mensagem criada pelo utilizador.

```bash
# commit com uma mensagem
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"
```

### diff

Mostra as diferenças entre um arquivo no diretório, o índice de trabalho e commits.

```bash
# Mostrar diferença entre o seu diretório de trabalho e o índice.
$ git diff

# Mostrar diferenças entre o índice e o commit mais recente.
$ git diff --cached

# Mostrar diferenças entre o seu diretório de trabalho e o commit mais recente.
$ git diff HEAD
```

### grep

Permite procurar rapidamente um repositório.

Configurações opcionais:

```bash
# Obrigado ao Travis Jeffery por isto
# Configure os números de linha a serem mostrados nos resultados de busca grep
$ git config --global grep.lineNumber true

# Fazer resultados de pesquisa mais legível, incluindo agrupamento
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Procure por "variableName" em todos os arquivos java
$ git grep 'variableName' -- '*.java'

# Procure por uma linha que contém "arrayListName" e "adicionar" ou "remover"
$ git grep -e 'arrayListName' --and \( -e add -e remove \) 
```

Google é seu amigo; para mais exemplos
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Mostrar commits para o repositório.

```bash
# Mostrar todos os commits
$ git log

# Mostrar um número X de commits
$ git log -n 10

# Mostrar somente commits mesclados
$ git log --merges
```

### merge

"Merge" em mudanças de commits externos no branch atual.

```bash
# Mesclar o ramo especificado para o atual.
$ git merge branchName

# Gera sempre uma mesclagem commit ao mesclar
$ git merge --no-ff branchName
```

### mv

Renomear ou mover um arquivo

```bash
# Renomear um arquivo
$ git mv HelloWorld.c HelloNewWorld.c

# Mover um arquivo
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Força renomear ou mover
# "ExistingFile" já existe no diretório, será substituído
$ git mv -f myFile existingFile
```

### pull

Puxa de um repositório e se funde com outro ramo.

```bash
# Atualize seu repo local, através da fusão de novas mudanças
# A partir da "origem" remoto e ramo "master (mestre)".
# git pull <remote> <branch>
# git pull => implícito por padrão => git pull origin master
$ git pull origin master

# Mesclar em mudanças de ramo remoto e rebase
# Ramo commita em seu repo local, como: "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

Empurre e mesclar as alterações de uma ramificação para uma remota e ramo.

```bash
# Pressione e mesclar as alterações de um repo local para um 
# Chamado remoto "origem" e ramo de "mestre".
# git push <remote> <branch>
# git push => implícito por padrão => git push origin master
$ git push origin master

# Para ligar atual filial local com uma filial remota, bandeira add-u:
$ git push -u origin master
# Agora, a qualquer hora que você quer empurrar a partir desse mesmo ramo local, uso de atalho:
$ git push 
```

### rebase (CAUTELA) 

Tire todas as alterações que foram commitadas em um ramo, e reproduzi-las em outro ramo. 
* Não rebase commits que você tenha empurrado a um repo público *.

```bash
# Rebase experimentBranch para mestre
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Leitura Adicional.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (CAUTELA)

Repor o atual HEAD de estado especificado. Isto permite-lhe desfazer fusões (merge), 
puxa (push), commits, acrescenta (add), e muito mais. É um grande comando, mas também 
perigoso se não saber o que se está fazendo.

```bash
# Repor a área de teste, para coincidir com o último commit (deixa diretório inalterado)
$ git reset

# Repor a área de teste, para coincidir com o último commit, e substituir diretório trabalhado
$ git reset --hard

# Move a ponta ramo atual para o especificado commit (deixa diretório inalterado)
# Todas as alterações ainda existem no diretório.
$ git reset 31f2bb1

# Move a ponta ramo atual para trás, para o commit especificado
# E faz o jogo dir trabalho (exclui mudanças não commitadas e todos os commits
# Após o commit especificado).
$ git reset --hard 31f2bb1
```

### rm

O oposto do git add, git rm remove arquivos da atual árvore de trabalho.

```bash
# remove HelloWorld.c
$ git rm HelloWorld.c

# Remove um arquivo de um diretório aninhado
$ git rm /pather/to/the/file/HelloWorld.c
```

# # Mais informações

* [tryGit - A fun interactive way to learn Git.](http://try.github.io/levels/1/challenges/1)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [Git - guia prático](http://rogerdudler.github.io/git-guide/index.pt_BR.html)