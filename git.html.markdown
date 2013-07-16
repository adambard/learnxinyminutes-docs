---

language: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
filename: LearnGit.txt

---

Git is a distributed version control and source code management system. 

Git does this through a series of snapshopts of your project, and it works 
with those snapshots to provide you with functionality to version and 
manage your source code.

In layman's terms, it's a way of managing, and keeping a detailed historical record,
of your source code.

```

///////////////////////////////////////
// Concepts
///////////////////////////////////////

	///////////////////////////////////////
	// What is version control?
	///////////////////////////////////////
	
	Version control is a system that records changes to a file, or set of files, over time.

	///////////////////////////////////////
	// Centralized Versioning VS Distributed Versioning
	///////////////////////////////////////
	
	[Detailed Information & Images.](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

	///////////////////////////////////////
	// Why Use Git?
	///////////////////////////////////////
	
	* Can work offline.
	* Collaborating with others is easy!
	* Branching is easy!
	* Merging is easy!
	* Git is fast.
	* Git is flexible.

	///////////////////////////////////////
	// Repository
	///////////////////////////////////////

	A set of files, directories, historical records, commits, and heads. Imagine it as a source 		code datastructure, with the attribute that each source code "element" gives you access to 		its revision history, among other things.

	A git repository is comprised of the .git directory & working tree.

	///////////////////////////////////////
	// .git Directory (component of repository)
	///////////////////////////////////////

	The .git directory contains all the configurations, logs, branches, HEAD, and more.
	[Detailed List.](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)
	
	///////////////////////////////////////
	// Working Tree (component of repository)
	///////////////////////////////////////

	This is basically the directories and files in your repository. It is often referred to
	as your working directory.

	///////////////////////////////////////
	// Index (component of .git dir)
	///////////////////////////////////////
	
	The Index is the staging area in git. It's basically layer that separates your working tree
	from the Git repository. This gives developers more power over what gets sent to the Git
	repository.

	///////////////////////////////////////
	// Commit
	///////////////////////////////////////
	
	A git commit is a snapshot of a set of changes, or manipulations to your Working Tree.
	For example, if you added 5 files, and removed 2 others, these changes will be contained
	in a commit (or snapshot). This commit, can then be pushed to other repositorys, or not!

	///////////////////////////////////////
	// Branch
	///////////////////////////////////////

	A branch is essentially a pointer, that points to the last commit you made. As you commit
	this pointer will automatically update and point to the latest commit.

	///////////////////////////////////////
	// HEAD and head (component of .git dir)
	///////////////////////////////////////

	HEAD, is a pointer, that points to the current branch. A repository only has 1 *active* HEAD.
	head, is a pointer, that points to any commit. A repository can have any number of heads.
	
	///////////////////////////////////////
	// Conceptual Resources
	///////////////////////////////////////
	
	[Git For Computer Scientists](http://eagain.net/articles/git-for-computer-scientists/)
	[Git For Designers](http://hoth.entp.com/output/git_for_designers.html)


///////////////////////////////////////
// Commands
///////////////////////////////////////

	///////////////////////////////////////
	// init
	///////////////////////////////////////

	Purpose: 
	To create an empty Git repository. The Git repository's settings, stored information, 
	and more is stored in a directory, or folder named, ".git".

	Examples:
	$ git init

	///////////////////////////////////////
	// config
	///////////////////////////////////////

	Purpose:
	To configure settings. Whether it be for the repository, the system itself, or global
	configurations.

	Examples:

	// Print & Set Some Basic Config Variables (Global)
	$ git config --global user.email
	$ git config --global user.name

	$ git config --global user.email "MyEmail@Zoho.com"
	$ git config --global user.name "My Name"

	[Learn More About git config.](http://git-scm.com/docs/git-config)
	
	///////////////////////////////////////
	// help
	///////////////////////////////////////

	Purpose:
	To give you quick access to an extremeled detailed guide of each command. Or to
	just give you a quick reminder of some semantics.

	Examples:
	// Quickly check available commands
	$ git help

	// Check all available commands
	$ git help -a

	// Command specific help - user manual
	// git help <command_here>
	$ git help add
	$ git help commit
	$ git help init

	///////////////////////////////////////
	// status
	///////////////////////////////////////

	Purpose:
	To show differences between the index file (basically your working copy/repo) and the current
	HEAD commit.

	Examples:
	// Will display the branch, untracked files, changes and other differences
	$ git status

	// To learn other "tid bits" about git status
	$ git help status

	///////////////////////////////////////
	// add
	///////////////////////////////////////

	Purpose:
	To add files to the current working tree/directory/repo. If you do not git add files to the
	working tree/directory they will not be included in commits!

	Exmaples:
	// add a file in your current working directory
	$ git add HelloWorld.java

	// add a file in a nested dir
	$ git add /path/to/file/HelloWorld.c
	
	// Regular Expression support!
	$ git add ./*.java

	///////////////////////////////////////
	// branch
	///////////////////////////////////////

	Purpose:
	Manage your branches. You can view, edit, create, delete branches using this command.
	
	Examples:
	// list existing branches & remotes
	$ git branch -a

	// create a new branch
	$ git branch myNewBranch
	
	// delete a branch
	$ git branch -d myBranch

	// rename a branch
	// git branch -m <oldname> <newname>
	$ git branch -m myBranchName myNewBranchName

	// edit a branch's description
	$ git branch myBranchName --edit-description

	///////////////////////////////////////
	// checkout             
	///////////////////////////////////////

	Purpose:
	Updates all files in the working tree to match the version in the index, or specified tree.

	Examples:
	// Checkout a repo - defaults to master branch
	$ git checkout
	// Checkout a specified branch
	$ git checkout -b branchName

	///////////////////////////////////////
	// clone
	///////////////////////////////////////

	Purpose:
	Clones, or copys, an existing repository into a new directory. It almost adds
	remote-tracking branches for each branch in the cloned repo. (which allows you to push
	to a remote branch)

	Examples:
	// Clone learnxinyminutes-docs
	$ git clone https://github.com/adambard/learnxinyminutes-docs.git
	
	///////////////////////////////////////
	// commit
	///////////////////////////////////////

	Purpose:
	Stores the current contents of the index in a new "commit". This commit contains
	the changes made and a message created by the user.

	Examples:
	// commit with a message
	$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"

	///////////////////////////////////////
	// grep
	///////////////////////////////////////

	Purpose:
	Allows you to quickly search a repository.	

	Optional Configurations:
	// Thanks to Travis Jeffery for these
	// Set line numbers to be shown in grep search results
	$ git config --global grep.lineNumber true

	// Make search results more readable, including grouping
	$ git config --global alias.g "grep --break --heading --line-number"

	Examples:
	// Search for "variableName" in all java files
	$ git grep 'variableName' -- '*.java'

	// Search for a line that contains "arrayListName" and, "add" or "remove"
	$ git grep -e 'arrayListName' --and \( -e add -e remove \) 

	Google is your friend for more examples
	[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

	///////////////////////////////////////
	// log
	///////////////////////////////////////

	Purpose:
	Display commits to the repository.

	Examples:
	// Show all commits
	$ git log

	// Show X number of commits
	$ git log -n 10

	// Show merge commits only
	$ git log --merges

	///////////////////////////////////////
	// merge
	///////////////////////////////////////

	Purpose:
	"Merge" in changes, from external commits, into the current branch.

	Examples:
	// Merge the specified branch into the current.
	$ git merge branchName
	
	// Always generate a merge commit when merging
	$ git merge --no-ff branchName

	///////////////////////////////////////
	// mv
	///////////////////////////////////////

	Purpose:
	Rename or move a file	
	
	Examples:
	// Renaming a file
	$ git mv HelloWorld.c HelloNewWorld.c

	// Moving a file
	$ git mv HelloWorld.c ./new/path/HelloWorld.c

	// Force rename or move
	// "existingFile" already exists in the directory, will be overwritten
	$ git mv -f myFile existingFile

	///////////////////////////////////////
	// pull
	///////////////////////////////////////

	Purpose:
	Pulls from a repository and merges it with another branch.

	Examples:
	// Update your local repo, by merging in new changes
	// from the remote "origin" and "master" branch.
	// git pull <remote> <branch>
	$ git pull origin master

	///////////////////////////////////////
	// push
	///////////////////////////////////////

	Purpose:
	Push, and merge changes from a branch to a remote & branch.

	Examples:
	// Push, and merge changes from a local repo to a 
	// remote named "origin" and "master" branch.
	// git push <remote> <branch>
	// git push => implicitly defaults to => git push origin master
	$ git push origin master

	///////////////////////////////////////
	// rebase (caution) 
	///////////////////////////////////////

	Purpose:
	Take all changes that were committed on one branch, and replay them onto another branch.
	*Do not rebase commits that you have pushed to a public repo*

	Examples:
	// Rebase experimentBranch onto master
	// git rebase <basebranch> <topicbranch>
	$ git rebase master oldTest

	[Additional Reading.](http://git-scm.com/book/en/Git-Branching-Rebasing)
	
	///////////////////////////////////////
	// reset (caution)
	///////////////////////////////////////

	Purpose:
	Reset the current HEAD to the specified state. This allows you to undo merges,
	pulls, commits, adds, and more. It's a great command but also dangerous if you don't
	know what you are doing.	

	Examples:
	// Reset the staging area, to match the latest commit (leaves dir unchanged)
	$ git reset

	// Reset the staging area, to match the latest commit, and overwrite working dir
	$ git reset --hard

	// Moves the current branch tip to the specified commit (leaves dir unchanged)
	// all changes still exist in the directory.
	$ git reset 31f2bb1

	// Moves the current branch tip backward to the specified commit
	// and makes the working dir match (deletes uncommited changes and all commits
	// after the specified commit).
	$ git reset --hard 31f2bb1

	///////////////////////////////////////
	// rm
	///////////////////////////////////////
	
	Purpose:
	The opposite of git add, git rm removes files from the current working tree.

	Example:
	// remove HelloWorld.c
	$ git rm HelloWorld.c

	// Remove a file from a nested dir
	$ git rm /pather/to/the/file/HelloWorld.c

```

## Further Information

* [tryGit - A fun interactive way to learn Git.](http://try.github.io/levels/1/challenges/1)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)
