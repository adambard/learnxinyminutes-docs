---
category: tool
tool: Mercurial
contributors:
  - ["Will L. Fife", "http://github.com/sarlalian"]
filename: LearnMercurial.txt
---

Mercurial is a free, distributed source control management tool. It offers
you the power to efficiently handle projects of any size while using an
intuitive interface. It is easy to use and hard to break, making it ideal for
anyone working with versioned files.

## Versioning Concepts

### What is version control?

Version control is a system that keeps track fo changes to a set of file(s)
and/or directorie(s) over time.

### Why use Mercurial?

* Distributed Architecture - Traditionally version control systems such as CVS
and Subversion are a client server architecture with a central server to
store the revision history of a project. Mercurial however is a truly
distributed architecture, giving each developer a full local copy of the
entire development history. It works independently of a central server.
* Fast - Traditionally version control systems such as CVS and Subversion are a
client server architecture with a central server to store the revision history
of a project. Mercurial however is a truly distributed architecture, giving
each developer a full local copy of the entire development history. It works
independently of a central server.
* Platform Independent - Mercurial was written to be highly platform
independent. Much of Mercurial is written in Python, with small performance
critical parts written in portable C. Binary releases are available for all
major platforms.
* Extensible - The functionality of Mercurial can be increased with extensions,
either by activating the official ones which are shipped with Mercurial or
downloading some [from the wiki](https://www.mercurial-scm.org/wiki/UsingExtensions) or by [writing your own](https://www.mercurial-scm.org/wiki/WritingExtensions). Extensions are written in
Python and can change the workings of the basic commands, add new commands and
access all the core functions of Mercurial.
* Easy to use - The Mercurial command set is consistent with what subversion
users would expect, so they are likely to feel right at home. Most dangerous
actions are part of extensions that need to be enabled to be used.
* Open Source - Mercurial is free software licensed under the terms of the [GNU
General Public License Version 2](http://www.gnu.org/licenses/gpl-2.0.txt) or
any later version.

## Terminology

| Term | Definition |
| ------------- | ---------------------------------- |
| Repository | A repository is a collection of revisions |
| hgrc | A configuration file which stores the defaults for a repository. |
| revision | A committed changeset: has a REV number |
| changeset | Set of changes saved as diffs |
| diff | Changes between file(s) |
| tag | A named named revision |
| parent(s) | Immediate ancestor(s) of a revision |
| branch | A child of a revision |
| head | A head is a changeset with no child changesets |
| merge | The process of merging two HEADS |
| tip | The latest revision in any branch |
| patch | All of the diffs between two revisions |
| bundle | Patch with permis­sions and rename support |

## Commands

### init

Create a new repository in the given directory, the settings and stored
information are in a directory named `.hg`.

```bash
$ hg init
```

### help

Will give you access to a very detailed description of each command.

```bash 
# Quickly check what commands are available
$ hg help

# Get help on a specific command
# hg help <command>
$ hg help add
$ hg help commit
$ hg help init
```

### status

Show the differences between what is on disk and what is committed to the current
branch or tag.

```bash
# Will display the status of files
$ hg status

# Get help on the status subcommand
$ hg help status
```

### add

Will add the specified files to the repository on the next commit.

```bash
# Add a file in the current directory
$ hg add filename.rb

# Add a file in a sub directory
$ hg add foo/bar/filename.rb

# Add files by pattern
$ hg add *.rb
```

### branch

Set or show the current branch name.

*Branch names are permanent and global. Use 'hg bookmark' to create a
light-weight bookmark instead. See 'hg help glossary' for more information
about named branches and bookmarks.*

```bash
# With no argument it shows the current branch name
$ hg branch

# With a name argument it will change the current branch.
$ hg branch new_branch
marked working directory as branch new_branch
(branches are permanent and global, did you want a bookmark?)
```

### tag

Add one or more tags for the current or given revision.

Tags are used to name particular revisions of the repository and are very
useful to compare different revisions, to go back to significant earlier
versions or to mark branch points as releases, etc. Changing an existing tag
is normally disallowed; use -f/--force to override.

```bash
# List tags
$ hg tags
tip                                2:efc8222cd1fb
v1.0                               0:37e9b57123b3

# Create a new tag on the current revision
$ hg tag v1.1

# Create a tag on a specific revision
$ hg tag -r efc8222cd1fb v1.1.1
```

### clone

Create a copy of an existing repository in a new directory.

If no destination directory name is specified, it defaults to the basename of
the source.

```bash
# Clone a remote repo to a local directory
$ hg clone https://some-mercurial-server.example.com/reponame

# Clone a local repo to a remote server
$ hg clone . ssh://username@some-mercurial-server.example.com/newrepo

# Clone a local repo to a local repo
$ hg clone . /tmp/some_backup_dir
```

### commit / ci

Commit changes to the given files into the repository. 

```bash
# Commit with a message
$ hg commit -m 'This is a commit message'

# Commit all added / removed files in the current tree
$ hg commit -A 'Adding and removing all existing files in the tree'

# amend the parent of the working directory with a new commit that contains the
# changes in the parent in addition to those currently reported by 'hg status',
$ hg commit --amend -m "Correct message"
```

### diff

Show differences between revisions for the specified files using the unified
diff format.

```bash
# Show the diff between the current directory and a previous revision
$ hg diff -r 10

# Show the diff between two previous revisions
$ hg diff -r 30 -r 20
```

### grep

Search revision history for a pattern in specified files.

```bash
# Search files for a specific phrase
$ hg grep "TODO:"
```

### log / history

Show revision history of entire repository or files. If no revision range is
specified, the default is "tip:0" unless --follow is set, in which case the
working directory parent is used as the starting revision.

```bash
# Show the history of the entire repository
$ hg log

# Show the history of a single file
$ hg log myfile.rb

# Show the revision changes as an ASCII art DAG with the most recent changeset
# at the top.
$ hg log -G
```

### merge

Merge another revision into working directory.

```bash
# Merge changesets to local repository
$ hg merge

# Merge from a named branch or revision into the current local branch
$ hg merge branchname_or_revision

# After successful merge, commit the changes
hg commit
```

### move / mv / rename

Rename files; equivalent of copy + remove. Mark dest as copies of sources;
mark sources for deletion. If dest is a directory, copies are put in that
directory. If dest is a file, there can only be one source.

```bash
# Rename a single file
$ hg mv foo.txt bar.txt

# Rename a directory
$ hg mv some_directory new_directory
```

### pull

Pull changes from a remote repository to a local one.

```bash
# List remote paths
$ hg paths
remote1 = http://path/to/remote1
remote2 = http://path/to/remote2

# Pull from remote 1
$ hg pull remote1

# Pull from remote 2
$ hg pull remote2
```

### push

Push changesets from the local repository to the specified destination.

```bash
# List remote paths
$ hg paths
remote1 = http://path/to/remote1
remote2 = http://path/to/remote2

# Pull from remote 1
$ hg push remote1

# Pull from remote 2
$ hg push remote2
```

### rebase

Move changeset (and descendants) to a different branch.

Rebase uses repeated merging to graft changesets from one part of history
(the source) onto another (the destination). This can be useful for
linearizing *local* changes relative to a master development tree.

* Draft the commits back to the source revision.
* -s is the source, ie. what you are rebasing.
* -d is the destination, which is where you are sending it.

```bash
# Put the commits into draft status
# This will draft all subsequent commits on the relevant branch
$ hg phase --draft --force -r 1206

# Rebase from from revision 102 over revision 208
$ hg rebase -s 102 -d 208
```

### revert

Restore files to their checkout state. With no revision specified, revert the
specified files or directories to the contents they had in the parent of the
working directory. This restores the contents of files to an unmodified state
and unschedules adds, removes, copies, and renames. If the working directory
has two parents, you must explicitly specify a revision.

```bash
# Reset a specific file to its checked out state
$ hg revert oops_i_did_it_again.txt

# Revert a specific file to its checked out state without leaving a .orig file
# around
$ hg revert -C oops_i_did_it_again.txt

# Revert all changes
$ hg revert -a
```

### rm / remove

Remove the specified files on the next commit.

```bash
# Remove a specific file
$ hg remove go_away.txt

# Remove a group of files by pattern
$ hg remove *.txt
```

## Further information

* [Learning Mercurial in Workflows](https://www.mercurial-scm.org/guide)
* [Mercurial Quick Start](https://www.mercurial-scm.org/wiki/QuickStart)
* [Mercurial: The Definitive Guide by Bryan O'Sullivan](http://hgbook.red-bean.com/)
