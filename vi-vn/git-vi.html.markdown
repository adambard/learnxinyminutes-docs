---
category: tool
tool: git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Vinh Nguyen", "https://twitter.com/vinhnx"]
filename: LearnGit-vi.txt
lang: vi-vn
---

Git là một hệ quản lý mã nguồn và phiên bản phân tán (distributed version control and source code management system).

Nó làm được điều này là do một loạt các snapshot từ đề án của bạn, và nó hoạt động
với các snapshot đó để cung cấp cho bạn với chức năng đến phiên bản và
quản lý mã nguồn của bạn.

## Khái Niệm Versioning

### Version Control là gì?

Version Control là một hệ thống ghi lại những thay đổi ở một tập tin, hay một nhóm các tập tin, theo thời gian.

### So sánh giữa Centralized Versioning và Distributed Versioning

* Quản lý phiên bản tập trung (Centralized Versioning) tập trung vào việc đồng bộ hóa, theo dõi, và lưu trữ tập tin.
* Quản lý phiên bản phân tán (Distributed Versioning) tập trung vào việc chia sẻ các thay đổi. Mỗi sự thay đổi có một mã định dạng (id) duy nhất.
* Các hệ phân tán không có cấu trúc định sẵn. Bạn có thể thay đổi một kiểu SVN, hệ phân tán, với git.

[Thông tin thêm](http://git-scm.com/book/en/Getting-Started-About-Version-Control)

### Tại Sao Dùng Git?

* Có thể hoạt động offline.
* Cộng tác với nhau rất dễ dàng!
* Phân nhánh dễ dàng!
* Trộn (Merging)
* Git nhanh.
* Git linh hoạt.

## Kiến Trúc Git


### Repository

Một nhóm các tập tin, thư mục, các ghi chép trong quá khứ, commit, và heads. Tưởng tượng nó như là một cấu trúc dữ liệu mã nguồn,
với thuộc tính mà một "nhân tố" mã nguồn cho bạn quyền truy cập đến lịch sử sửa đổi, và một số thứ khác.

Một git repository bao gồm thư mục .git & tree đang làm việc.

### Thư mục .git (thành phần của một repository)

Thư mục .git chứa tất cả các cấu hình, log, nhánh, HEAD, và hơn nữa.
[Danh Sách Chi Tiết.](http://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html)

### Tree Đang Làm (thành phần của một repository)

Đây cơ bản là các thư mục và tập tin trong repository của bạn. Nó thường được tham chiếu
thư mục đang làm việc của bạn

### Chỉ mục (thành phần của một thư mục .git)

Chỉ mục của là một staging area trong git. Nó đơn giản là một lớp riêng biệt với tree đang làm việc của bạn
từ Git repository. Điều này cho nhà phát triền nhiều lựa chọn hơn trong việc xem xét những gì được gửi đến Git
repository.

### Commit

Một git commit là một snapshot của một nhóm các thay đổi, hoặc các thao tác Working Tree của bạn.
Ví dụ, nếu bạn thêm 5 tập tin, và xóa 2 tập tin khác, những thay đổi này sẽ được chứa trong
một commit (hoặc snapshot). Commit này có thể được đẩy đến các repo khác, hoặc không!

### Nhánh

Nhánh thực chất là một con trỏ đến commit mới nhất mà bạn vừa thực hiện. Khi bạn commit,
con trỏ này sẽ cập nhật tự động và trỏ đến commit mới nhất.

### HEAD và head (thành phần của thư mục .git)

HEAD là một con trỏ đến branch hiện tại. Một repo chỉ có một HEAD *đang hoạt động*.
head là một con trỏ đến bất kỳ commit nào. Một repo có thể có nhiều head.

### Các Tài Nguyên Mang Tính Khái Niệm

* [Git For Computer Scientists](http://eagain.net/articles/git-for-computer-scientists/)
* [Git For Designers](http://hoth.entp.com/output/git_for_designers.html)


## Các Lệnh


### init

Tạo một repo Git rỗng. Các cài đặt, thông tin lưu trữ... của Git
được lưu ở một thư mục tên là ".git".

```bash
$ git init
```

### config

Để chỉnh tùy chọn. Bất kể là cho repo, hay cho hệ thống, hay điều chỉnh
toàn cục (global)



```bash
# In Ra & Và Gán Một Số Biến Tùy Chỉnh Cơ Bản (Toàn cục - Global)
$ git config --global user.email
$ git config --global user.name

$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"
```

[Tìm hiểu thêm về git config.](http://git-scm.com/docs/git-config)

### help

Để cho bạn lối truy cập nhanh đến một chỉ dẫn cực kỳ chi tiết của từng lệnh. Hoặc chỉ để
nhắc bạn một số cú pháp.

```bash
# Xem nhanh các lệnh có sẵn
$ git help

# Xem tất các các lệnh
$ git help -a

# Lệnh help riêng biệt - tài liệu người dùng
# git help <command_here>
$ git help add
$ git help commit
$ git help init
```

### status

Để hiển thị sự khác nhau giữa tập tin index (cơ bản là repo đang làm việc) và HEAD commit
hiện tại.


```bash
# Sẽ hiển thị nhánh, các tập tin chưa track (chưa commit), các thay đổi và những khác biệt khác
$ git status

# Để xem các "tid bits" về git status
$ git help status
```

### add

Để thêm các tập vào tree/thư mục/repo hiện tại. Nếu bạn không `git add` các tập tin mới đến
tree/thư mục hiện tại, chúng sẽ không được kèm theo trong các commit!

```bash
# thêm một file vào thư mục hiện tại
$ git add HelloWorld.java

# thêm một file vào một thư mục khác
$ git add /path/to/file/HelloWorld.c

# Hỗ trợ Regular Expression!
$ git add ./*.java
```

### branch

Quản lý nhánh (branch). Bạn có thể xem, sửa, tạo, xóa các nhánh bằng cách dùng lệnh này.

```bash
# liệt kê các branch đang có và ở remote
$ git branch -a

# tạo branch mới
$ git branch myNewBranch

# xóa một branch
$ git branch -d myBranch

# đặt tên lại một branch
# git branch -m <oldname> <newname>
$ git branch -m myBranchName myNewBranchName

# chỉnh sửa diễn giải của một branch
$ git branch myBranchName --edit-description
```

### checkout

Cập nhật tất cả các file trong tree hiện tại để cho trùng khớp với phiên bản của index, hoặc tree cụ thể.

```bash
# Checkout (chuyển) một repo - mặc định là nhánh master
$ git checkout
# Checkout một nhánh cụ thể
$ git checkout branchName
# Tạo một nhánh mới và chuyển đến nó, tương tự: "git branch <name>; git checkout <name>"
$ git checkout -b newBranch
```

### clone

Nhân bản, hoặc sao chép, một repo hiện có thành một thư mục mới. Nó cũng thêm
các branch có remote-tracking cho mỗi branch trong một repo được nhân bản, mà
cho phép bạn push đến một remote branch.

```bash
# Nhân bản learnxinyminutes-docs
$ git clone https://github.com/adambard/learnxinyminutes-docs.git
```

### commit

Lưu trữ nội dung hiện tại của index trong một "commit" mới. Điều này cho phép tạo ra thay đổi và một ghi chú tạo ra bởi người dùng.

```bash
# commit với một ghi chú
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"
```

### diff

Hiển thị sự khác biệt giữa một file trong thư mục hiện tại, index và commits.

```bash
# Hiển thị sự khác biệt giữa thư mục hiện tại và index
$ git diff

# Hiển thị khác biệt giữa index và commit mới nhất.
$ git diff --cached

# Hiển thị khác biệt giữa thư mục đang làm việc và commit mới nhất
$ git diff HEAD
```

### grep

Cho phép bạn tìm kiếm nhanh một repo.

Các tinh chỉnh tùy chọn:

```bash
# Cảm ơn Travis Jeffery vì những lệnh này
# Đặt số của dòng được hiển thị trong kết quả tìm kiếm grep
$ git config --global grep.lineNumber true

# Làm cho kết quả tìm kiếm dễ đọc hơn, bao gồm cả gom nhóm
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# Tìm "variableName" trong tất cả các file Java
$ git grep 'variableName' -- '*.java'

# Tìm một dòng mà có chứa "arrayListName" và, "add" hoặc "remove"
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

Google để xem thêm các ví dụ
[Git Grep Ninja](http://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja)

### log

Hiển thị các commit đến repo.

```bash
# Hiện tất cả các commit
$ git log

# Hiện X commit
$ git log -n 10

# Chỉ hiện các commit đã merge merge commits
$ git log --merges
```

### merge

"Trộn" các thay đổi từ commit bên ngoài vào trong nhánh hiện tại.

```bash
# Merge branch cụ thể vào branch hiện tại.
$ git merge branchName

# Luôn khởi tạo một merge commit khi trộn (merge)
$ git merge --no-ff branchName
```

### mv

Đặt lại tên hoặc di chuyển một file

```bash
# Đặt lại tên một file
$ git mv HelloWorld.c HelloNewWorld.c

# Di chuyển một file
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# Buộc đặt lại tên hoặc di chuyển
# "existingFile" đã tồn tại trong thự mục, sẽ bị ghi đè
$ git mv -f myFile existingFile
```

### pull

Pull về từ một repo và merge nó vào branch khác.

```bash
# Cập nhật repo local của bạn, bằng cách merge các thay đổi mới
# từ remote "origin" và nhánh "master".
# git pull <remote> <branch>
# git pull => hoàn toàn mặc định như => git pull origin master
$ git pull origin master

# Merge các thay đổi từ remote branch và rebase
# các commit trong branch lên trên local repo, như sau: "git pull <remote> <branch>, git rebase <branch>"
$ git pull origin master --rebase
```

### push

push và merge các thay đổi từ một branch đến một remote & branch.

```bash
# Push và merge các thay đổi từ một repo local đến một
# remote có tên là "origin" và nhánh "master".
# git push <remote> <branch>
# git push => mặc định ẩn đến => git push origin master
$ git push origin master

# Để liên kết đến một branch local với một branch remote, thêm vào cờ -u:
$ git push -u origin master
# Từ lúc này, bất cứ khi nào bạn muốn push từ cùng một nhánh local đó, sử dụng lối tắt:
$ git push 
```

### rebase (thận trọng)

Lấy tất cả các thay đổi mà đã được commit trên một nhánh, và replay (?) chúng trên một nhánh khác.
*Không rebase các commit mà bạn đã push đến một repo công khai*.

```bash
# Rebase experimentBranch lên master
# git rebase <basebranch> <topicbranch>
$ git rebase master experimentBranch
```

[Đọc Thêm.](http://git-scm.com/book/en/Git-Branching-Rebasing)

### reset (thận trọng)

Thiết lập lạo HEAD hiện tại đến một trạng thái cụ thể. Điều này cho phép bạn làm lại các merges,
pulls, commits, thêm, and hơn nữa. Nó là một lệnh hay nhưng cũng nguy hiểm nếu bạn không
biết mình đang làm gì.

```bash
# Thiết lập lại staging area, để trùng với commit mới nhất (để thư mục không thay đổi)
$ git reset

#  Thiết lập lại staging area, để trùng với commit mới nhất, và ghi đè lên thư mục hiện tại
$ git reset --hard

# Di chuyển nhánh hiện tại đến một commit cụ thể (để thư mục không thay đổi)
# tất cả thay đổi vẫn duy trì trong thư mục.
$ git reset 31f2bb1

# Di chuyển nhánh hiện tại lùi về một commit cụ thể
# và làm cho thư mục hiện tại trùng (xóa các thay đổi chưa được commit và tất cả các commit
# sau một commit cụ thể).
$ git reset --hard 31f2bb1
```

### rm

Ngược lại với git add, git rm xóa file từ tree đang làm việc.

```bash
# xóa HelloWorld.c
$ git rm HelloWorld.c

# Xóa file từ thư mục khác
$ git rm /pather/to/the/file/HelloWorld.c
```

## Thông tin thêm

* [tryGit - A fun interactive way to learn Git.](http://try.github.io/levels/1/challenges/1)

* [git-scm - Video Tutorials](http://git-scm.com/videos)

* [git-scm - Documentation](http://git-scm.com/docs)

* [Atlassian Git - Tutorials & Workflows](https://www.atlassian.com/git/)

* [SalesForce Cheat Sheet](https://na1.salesforce.com/help/doc/en/salesforce_git_developer_cheatsheet.pdf)

* [GitGuys](http://www.gitguys.com/)

* [Git - the simple guide](http://rogerdudler.github.io/git-guide/index.html)


