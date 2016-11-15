---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
translators:
    - ["Chenbo Li", "http://binarythink.net"]
lang: zh-cn
---

Git是一个分布式版本控制及源代码管理工具 

Git可以为你的项目保存若干快照，以此来对整个项目进行版本管理

## 版本

### 什么是版本控制

版本控制系统就是根据时间来记录一个或多个文件的更改情况的系统。

### 集中式版本控制 VS 分布式版本控制

* 集中式版本控制的主要功能为同步，跟踪以及备份文件
* 分布式版本控制则更注重共享更改。每一次更改都有唯一的标识
* 分布式系统没有预定的结构。你也可以用git很轻松的实现SVN风格的集中式系统控制

[更多信息](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### 为什么要使用Git

* 可以离线工作
* 和他人协同工作变得简单
* 分支很轻松
* 合并很容易
* Git系统速度快，也很灵活

## Git 架构


### 版本库

一系列文件，目录，历史记录，提交记录和头指针。
可以把它视作每个源代码文件都带有历史记录属性数据结构

一个Git版本库包括一个 .git 目录和其工作目录

### .git 目录(版本库的一部分)

.git 目录包含所有的配置、日志、分支信息、头指针等
[详细列表](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### 工作目录 (版本库的一部分)

版本库中的目录和文件，可以看做就是你工作时的目录

### 索引(.git 目录)

索引就是git中的 staging 区. 可以算作是把你的工作目录与Git版本库分割开的一层
这使得开发者能够更灵活的决定要将要在版本库中添加什么内容

### 提交

一个 git 提交就是一组更改或者对工作目录操作的快照
比如你添加了5个文件，删除了2个文件，那么这些变化就会被写入一个提交比如你添加了5个文件，删除了2个文件，那么这些变化就会被写入一个提交中
而这个提交之后也可以被决定是否推送到另一个版本库中

### 分支

分支其实就是一个指向你最后一次的提交的指针
当你提交时，这个指针就会自动指向最新的提交

### 头指针 与 头(.git 文件夹的作用)

头指针是一个指向当前分支的指针，一个版本库只有一个当前活动的头指针
而头则可以指向版本库中任意一个提交，每个版本库也可以有多个头

### 其他形象化解释

* [给计算机科学家的解释](http://eagain.net/articles/git-for-computer-scientists/)
* [给设计师的解释](http://hoth.entp.com/output/git_for_designers.html)


## 命令


### 初始化

创建一个新的git版本库。这个版本库的配置、存储等信息会被保存到.git文件夹中

```bash
$ git init
```

### 配置

更改设置。可以是版本库的设置，也可以是系统的或全局的


```bash
# 输出、设置基本的全局变量
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[关于git的更多设置](http://git-scm.com/docs/git-config)

### 帮助

git内置了对命令非常详细的解释，可以供我们快速查阅

```bash
# 查找可用命令
$ git help

# 查找所有可用命令
$ git help -a

# 在文档当中查找特定的命令
# git help <命令>
$ git help add
$ git help commit
$ git help init
```

### 状态

显示索引文件（也就是当前工作空间）和当前的头指针指向的提交的不同


```bash
# 显示分支，为跟踪文件，更改和其他不同
$ git status

# 查看其他的git status的用法
$ git help status
```

### 添加

添加文件到当前工作空间中。如果你不使用 `git add` 将文件添加进去，
那么这些文件也不会添加到之后的提交之中

```bash
# 添加一个文件
$ git add HelloWorld.java

# 添加一个子目录中的文件
$ git add /path/to/file/HelloWorld.c

# 支持正则表达式
$ git add ./*.java
```

### 分支

管理分支，可以通过下列命令对分支进行增删改查

```bash
# 查看所有的分支和远程分支
$ git branch -a

# 创建一个新的分支
$ git branch myNewBranch

# 删除一个分支
$ git branch -d myBranch

# 重命名分支
# git branch -m <旧名称> <新名称>
$ git branch -m myBranchName myNewBranchName

# 编辑分支的介绍
$ git branch myBranchName --edit-description
```

### 检出

将当前工作空间更新到索引所标识的或者某一特定的工作空间

```bash
# 检出一个版本库，默认将更新到master分支
$ git checkout
# 检出到一个特定的分支
$ git checkout branchName
# 新建一个分支，并且切换过去，相当于"git branch <名字>; git checkout <名字>"
$ git checkout -b newBranch
```

### clone

这个命令就是将一个版本库拷贝到另一个目录中，同时也将
分支都拷贝到新的版本库中。这样就可以在新的版本库中提交到远程分支

```bash
# clone learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
```

### commit

将当前索引的更改保存为一个新的提交，这个提交包括用户做出的更改与信息

```bash
# 提交时附带提交信息
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"
```

### diff

显示当前工作空间和提交的不同

```bash
# 显示工作目录和索引的不同
$ git diff

# 显示索引和最近一次提交的不同
$ git diff --cached

# 显示工作目录和最近一次提交的不同
$ git diff HEAD
```

### grep

可以在版本库中快速查找

可选配置：

```bash
# 感谢Travis Jeffery提供的以下用法：
# 在搜索结果中显示行号
$ git config --global grep.lineNumber true

# 是搜索结果可读性更好
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# 在所有的java中查找variableName
$ git grep 'variableName' -- '*.java'

# 搜索包含 "arrayListName" 和, "add" 或 "remove" 的所有行
$ git grep -e 'arrayListName' --and \( -e add -e remove \) 
```

更多的例子可以查看：
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

显示这个版本库的所有提交

```bash
# 显示所有提交
$ git log

# 显示某几条提交信息
$ git log -n 10

# 仅显示合并提交
$ git log --merges
```

### merge

合并就是将外部的提交合并到自己的分支中

```bash
# 将其他分支合并到当前分支
$ git merge branchName

# 在合并时创建一个新的合并后的提交
$ git merge --no-ff branchName
```

### mv

重命名或移动一个文件

```bash
# 重命名
$ git mv HelloWorld.c HelloNewWorld.c

# 移动
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# 强制重命名或移动
# 这个文件已经存在，将要覆盖掉
$ git mv -f myFile existingFile
```

### pull

从远端版本库合并到当前分支

```bash
# 从远端origin的master分支更新版本库
# git pull <远端> <分支>
$ git pull origin master
```

### push

把远端的版本库更新

```bash
# 把本地的分支更新到远端origin的master分支上
# git push <远端> <分支>
# git push 相当于 git push origin master
$ git push origin master
```

### rebase (谨慎使用) 

将一个分支上所有的提交历史都应用到另一个分支上
*不要在一个已经公开的远端分支上使用rebase*.

```bash
# 将experimentBranch应用到master上面
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[更多阅读](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (谨慎使用)

将当前的头指针复位到一个特定的状态。这样可以使你撤销merge、pull、commits、add等
这是个很强大的命令，但是在使用时一定要清楚其所产生的后果

```bash
# 使 staging 区域恢复到上次提交时的状态，不改变现在的工作目录
$ git reset

# 使 staging 区域恢复到上次提交时的状态，覆盖现在的工作目录
$ git reset --hard

# 将当前分支恢复到某次提交，不改变现在的工作目录
# 在工作目录中所有的改变仍然存在
$ git reset 31f2bb1

# 将当前分支恢复到某次提交，覆盖现在的工作目录
# 并且删除所有未提交的改变和指定提交之后的所有提交
$ git reset --hard 31f2bb1
```

### rm

和add相反，从工作空间中去掉某个文件

```bash
# 移除 HelloWorld.c
$ git rm HelloWorld.c

# 移除子目录中的文件
$ git rm /pather/to/the/file/HelloWorld.c
```

## 更多阅读

* [tryGit - 学习Git的有趣方式](http://try.github.io/levels/1/challenges/1)

* [git-scm - 视频教程](http://git-scm.com/videos)

* [git-scm - 文档](http://git-scm.com/docs)

* [Atlassian Git - 教程与工作流程](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)
