---
category: tool
name: Git
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Leo Rudberg" , "http://github.com/LOZORD"]
    - ["Betsy Lorton" , "http://github.com/schbetsy"]
    - ["Bruno Volcov", "http://github.com/volcov"]
    - ["Andrew Taylor", "http://github.com/andrewjt71"]
    - ["Jason Stathopulos", "http://github.com/SpiritBreaker226"]
    - ["Milo Gilad", "http://github.com/Myl0g"]
filename: LearnGit.txt
translators:
    - ["Learn X in Y Minutes (ar)", "https://github.com/adambard/learnxinyminutes-docs"]
---

<p dir="rtl">
جِت (Git) نظام موزّع لإدارة الإصدارات ومصدر الشيفرة.
</p>

<p dir="rtl">
يعمل عبر سلسلة لقطات (snapshots) لمشروعك، ويستخدم هذه اللقطات ليمنحك إصدارات الشيفرة وإدارتها.
</p>

<h2 dir="rtl">مفاهيم الإصدارة</h2>

<h3 dir="rtl">ما هي إدارة الإصدارات؟</h3>

<p dir="rtl">
نظام يسجّل تغييرات الملفات عبر الزمن.
</p>

<h3 dir="rtl">مركزي مقابل موزّع</h3>

<ul dir="rtl">
<li>المركزي يركّز على المزامنة والتتبع والنسخ الاحتياطي للملفات.</li>
<li>الموزّع يركّز على مشاركة التغييرات؛ كل تغيير له معرّف فريد.</li>
<li>الأنظمة الموزّعة لا تفرض شكلاً واحداً؛ يمكن تشغيل جِت بأسلوب يشبه SVN المركزي.</li>
</ul>

<p dir="rtl"><a href="https://git-scm.com/book/en/v2/Getting-Started-About-Version-Control">معلومات إضافية</a></p>

<h3 dir="rtl">لماذا جِت؟</h3>

<ul dir="rtl">
<li>العمل دون اتصال.</li>
<li>التعاون مع الآخرين سهل.</li>
<li>إنشاء الفروع سهل وسريع!</li>
<li>الدمج سهل.</li>
<li>جِت سريع ومرن.</li>
</ul>

<h2 dir="rtl">بنية جِت</h2>

<h3 dir="rtl">المستودع (Repository)</h3>

<p dir="rtl">
مجموعة ملفات ومجلدات وسجلّ تاريخ وعمليات commit ورؤوس (heads). تخيّلها هيكل بيانات للشيفرة حيث كل «عنصر» يتيح الوصول إلى تاريخ مراجعاته وغير ذلك.
مستودع جِت يتكوّن من مجلد .git وشجرة العمل (working tree).
</p>

<h3 dir="rtl">مجلد .git</h3>

<p dir="rtl">
يحتوي الإعدادات والسجلات والفروع وHEAD وغيرها.
<a href="https://gitready.com/advanced/2009/03/23/whats-inside-your-git-directory.html">قائمة تفصيلية</a>
</p>

<h3 dir="rtl">شجرة العمل</h3>

<p dir="rtl">
الملفات والمجلدات في مستودعك؛ غالباً تُسمّى دليل العمل.
</p>

<h3 dir="rtl">الفهرس (Index)</h3>

<p dir="rtl">
منطقة الإعداد (staging): طبقة تفصل شجرة العمل عن مستودع جِت، فتعطيك تحكّماً بما يُرسَل إلى المستودع.
</p>

<h3 dir="rtl">الالتزام (Commit)</h3>

<p dir="rtl">
لقطة لتغييرات أو تعديلات على شجرة العمل. مثلاً إن أضفت خمسة ملفات وحذفت اثنين، فكل ذلك في commit واحد يمكن دفعه لمستودعات أخرى أو لا.
</p>

<h3 dir="rtl">الفرع (Branch)</h3>

<p dir="rtl">
مؤشّر إلى آخر commit. مع كل commit جديد يتحدّث المؤشّر تلقائياً.
</p>

<h3 dir="rtl">الوسم (Tag)</h3>

<p dir="rtl">
علامة على نقطة في التاريخ؛ غالباً لنقاط الإصدار (مثل v1.0).
</p>

<h3 dir="rtl">HEAD و head</h3>

<p dir="rtl">
HEAD يشير إلى الفرع الحالي؛ يوجد HEAD نشط واحد فقط.
head (بأحرف صغيرة) يمكن أن يشير إلى أي commit؛ يمكن أن يكون هناك عدة heads.
</p>

<h3 dir="rtl">حالات الملفات</h3>

<ul dir="rtl">
<li><strong>معدّل (Modified)</strong> — تغيّر الملف ولم يُلتَمَع بعد إلى قاعدة بيانات جِت.</li>
<li><strong>مُعدّ للالتزام (Staged)</strong> — مُعلَّم للدخول في لقطة الالتزام التالية.</li>
<li><strong>مُلتَمَع (Committed)</strong> — الملفات محفوظة في قاعدة بيانات جِت.</li>
</ul>

<h2 dir="rtl">الأوامر</h2>

<h3 dir="rtl">init</h3>

<p dir="rtl">
إنشاء مستودع جِت فارغ. الإعدادات والبيانات في مجلد باسم «.git».
</p>

```bash
$ git init
```

<h3 dir="rtl">config</h3>

<p dir="rtl">
ضبط الإعدادات للمستودع أو النظام أو العام (ملف الإعداد العام عادة <code>~/.gitconfig</code>).
</p>

```bash
# ضبط وعرض إعدادات أساسية (عالمية)
$ git config --global user.email "MyEmail@Zoho.com"
$ git config --global user.name "My Name"

$ git config --global user.email
$ git config --global user.name
```

<p dir="rtl"><a href="https://git-scm.com/docs/git-config">المزيد عن git config</a></p>

<h3 dir="rtl">help</h3>

<p dir="rtl">
دليل مفصّل لكل أمر، أو تذكير سريع بالصياغة.
</p>

```bash
# عرض مساعدة سريعة
$ git help

# كل الأوامر
$ git help -a

# دليل أمر معيّن
# git help <الأمر>
$ git help add
$ git help commit
$ git help init
# أو git <الأمر> --help
$ git add --help
$ git commit --help
$ git init --help
```

<h3 dir="rtl">ignore files</h3>

<p dir="rtl">
استبعاد ملفات ومجلدات عن التتبع عمداً؛ مفيد للملفات الخاصة والمؤقتة.
</p>

```bash
$ echo "temp/" >> .gitignore
$ echo "private_key" >> .gitignore
```

<h3 dir="rtl">status</h3>

<p dir="rtl">
عرض الفروق بين الفهرس وآخر commit على HEAD.
</p>

```bash
# يعرض الفرع، غير المتتبّع، الفروقات
$ git status

# تفاصيل إضافية عن status
$ git help status
```

<h3 dir="rtl">add</h3>

<p dir="rtl">
إضافة ملفات لمنطقة الإعداد. بدون <code>git add</code> لن تُدرَج الملفات الجديدة في الالتزامات!
</p>

```bash
# إضافة ملف من دليل العمل الحالي
$ git add HelloWorld.java

# ملف في مجلد متداخل
$ git add /path/to/file/HelloWorld.c

# أنماط glob
$ git add ./*.java

# كل التغييرات في دليل العمل إلى منطقة الإعداد
$ git add -A
```

<p dir="rtl">
يضيف إلى منطقة الإعداد فقط ولا يُنشئ commitاً في المستودع بعد.
</p>

<h3 dir="rtl">branch</h3>

<p dir="rtl">
إدارة الفروع: عرض، إنشاء، حذف، إعادة تسمية.
</p>

```bash
# الفروع والبعيدة
$ git branch -a

# فرع جديد
$ git branch myNewBranch

# حذف فرع
$ git branch -d myBranch

# إعادة تسمية
# git branch -m <قديم> <جديد>
$ git branch -m myBranchName myNewBranchName

# وصف الفرع
$ git branch myBranchName --edit-description
```

<h3 dir="rtl">tag</h3>

<p dir="rtl">
إدارة الوسوم.
</p>

```bash
# قائمة الوسوم
$ git tag

# وسم مشروح
# -m رسالة تُخزَّن مع الوسم
# بلا -m يفتح المحرر
$ git tag -a v2.0 -m 'my version 2.0'

# تفاصيل الوسم (الوسام، التاريخ، الرسالة، ثم الالتزام)
$ git show v2.0

# دفع وسم واحد
$ git push origin v2.0

# دفع كل الوسوم
$ git push origin --tags
```

<h3 dir="rtl">checkout</h3>

<p dir="rtl">
تحديث ملفات شجرة العمل لتطابق النسخة في الفهرس أو الشجرة المحددة.
</p>

```bash
# checkout — افتراضياً فرع master
$ git checkout

# التبديل لفرع
$ git checkout branchName

# فرع جديد والانتقال إليه
# يعادل: git branch <اسم>; git checkout <اسم>

$ git checkout -b newBranch
```

<h3 dir="rtl">clone</h3>

<p dir="rtl">
نسخ مستودع موجود إلى مجلد جديد، مع فروع تتبّع عن بُعد لكل فرع في المستودع المنسوخ.
</p>

```bash
# استنساخ المستودع
$ git clone https://github.com/adambard/learnxinyminutes-docs.git

# استنساخ ضحل — أسرع، آخر لقطة فقط
$ git clone --depth 1 https://github.com/adambard/learnxinyminutes-docs.git

# فرع محدد فقط
$ git clone -b master-cn https://github.com/adambard/learnxinyminutes-docs.git --single-branch
```

<h3 dir="rtl">commit</h3>

<p dir="rtl">
حفظ محتويات الفهرس في commit جديد مع رسالة منك.
</p>

```bash
# التزام مع رسالة
$ git commit -m "Added multiplyNumbers() function to HelloWorld.c"

# التزام موقّع (يُضبط user.signingkey مع مفتاح GPG)
$ git commit -S -m "signed commit message"

# إضافة تلقائية للمعدّل والمحذوف (لا ملفات جديدة) ثم التزام
$ git commit -a -m "Modified foo.php and removed bar.php"

# تعديل آخر التزام (يستبدله التزاماً جديداً)
$ git commit --amend -m "Correct message"
```

<h3 dir="rtl">diff</h3>

<p dir="rtl">
عرض الفروق بين الملف في دليل العمل والفهرس والالتزامات.
</p>

```bash
# فرق دليل العمل والفهرس
$ git diff

# الفهرس مقابل آخر التزام
$ git diff --cached

# دليل العمل مقابل آخر التزام
$ git diff HEAD
```

<h3 dir="rtl">grep</h3>

<p dir="rtl">
بحث سريع في المستودع.
</p>

<p dir="rtl">إعدادات اختيارية:</p>

```bash
# (أمثلة من Travis Jeffery)
# أرقام أسطر في نتائج grep
$ git config --global grep.lineNumber true

# نتائج أوضح مع تجميع
$ git config --global alias.g "grep --break --heading --line-number"
```

```bash
# البحث عن variableName في ملفات java
$ git grep 'variableName' -- '*.java'

# سطر فيه arrayListName و add أو remove
$ git grep -e 'arrayListName' --and \( -e add -e remove \)
```

<p dir="rtl">لمزيد من الأمثلة:
<a href="https://travisjeffery.com/b/2012/02/search-a-git-repo-like-a-ninja">Git Grep Ninja</a>
</p>

<h3 dir="rtl">log</h3>

<p dir="rtl">
عرض سجلّ الالتزامات.
</p>

```bash
# كل الالتزامات
$ git log

# سطر واحد لكل التزام
$ git log --oneline

# دمجات فقط
$ git log --merges

# رسم بياني ASCII
$ git log --graph
```

<h3 dir="rtl">merge</h3>

<p dir="rtl">
دمج تغييرات من التزامات خارجية في الفرع الحالي.
</p>

```bash
# دمج الفرع المحدد في الحالي
$ git merge branchName

# دمج مع التزام دمج دائماً
$ git merge --no-ff branchName
```

<h3 dir="rtl">mv</h3>

<p dir="rtl">
إعادة تسمية أو نقل ملف.
</p>

```bash
# إعادة تسمية
$ git mv HelloWorld.c HelloNewWorld.c

# نقل
$ git mv HelloWorld.c ./new/path/HelloWorld.c

# إجبار — يستبدل existingFile إن وُجد
$ git mv -f myFile existingFile
```

<h3 dir="rtl">pull</h3>

<p dir="rtl">
جلب من مستودع بعيد ودمج مع فرع.
</p>

```bash
# جلب ودمج من origin و master
# git pull <بعيد> <فرع>
$ git pull origin master

# الافتراضي: دمج الفرع الحالي مع متتبّعه البعيد
$ git pull

# جلب مع إعادة تأسيس التزاماتك فوق البعيد
$ git pull origin master --rebase
```

<h3 dir="rtl">push</h3>

<p dir="rtl">
دفع التغييرات من فرع محلي إلى بعيد ودمجها.
</p>

```bash
# دفع من محلي إلى origin و master
# git push <بعيد> <فرع>
$ git push origin master

# الافتراضي: دفع الفرع الحالي إلى متتبّعه
$ git push

# -u يربط الفرع المحلي بالبعيد
$ git push -u origin master
# بعدها يكفي git push من نفس الفرع
$ git push
```

<h3 dir="rtl">stash</h3>

<p dir="rtl">
يحفظ حالة دليل العمل «القذرة» (تغييرات غير ملتزَمة) في مكدس يمكن إعادة تطبيقه لاحقاً.
مثال: تعمل محلياً وتريد <code>git pull</code> لكن لديك تغييرات غير ملتزَمة؛ استخدم <code>git stash</code> لحفظها مؤقتاً.
</p>

```bash
$ git stash
Saved working directory and index state \
  "WIP on master: 049d078 added the index file"
  HEAD is now at 049d078 added the index file
  (To restore them type "git stash apply")
```

<p dir="rtl">ثم يمكنك التنفيذ:</p>

```bash
git pull
```

<p dir="rtl"><code>...changes apply...</code></p>

<p dir="rtl">تحقق أن كل شيء على ما يرام:</p>

```bash
# مثال: لا شيء للالتزام، دليل العمل نظيف (النص التالي كما يطبعه git)
$ git status
On branch master
nothing to commit, working directory clean
```

<p dir="rtl">
<code>git stash list</code> يعرض ما خزّنته؛ الأحدث في الأعلى (LIFO).
</p>

```bash
$ git stash list
stash@{0}: WIP on master: 049d078 added the index file
stash@{1}: WIP on master: c264051 Revert "added file_size"
stash@{2}: WIP on master: 21d80a5 added number to log
```

<p dir="rtl">
استرجع التغييرات بـ <code>git stash pop</code> لإزالتها من المكدس.
</p>

```bash
$ git stash pop
# غالباً يذكر git أنك على فرع master مع ملفات معدّلة غير في الفهرس
# ويقترح git add لتحديث ما سيُلتَمَع — index.html و lib/simplegit.rb في المثال الأصلي
```

<p dir="rtl"><code>git stash apply</code> يعيد التطبيق دون إزالة العنصر من القائمة.</p>

<p dir="rtl"><a href="https://git-scm.com/book/en/v2/Git-Tools-Stashing-and-Cleaning">قراءة إضافية عن stash</a></p>

<h3 dir="rtl">rebase (بحذر)</h3>

<p dir="rtl">
إعادة تشغيل التزامات فرع على فرع آخر.
<strong>لا تعيد تأسيس (rebase) التزامات دفعتها علناً.</strong>
</p>

```bash
# إعادة تأسيس experimentBranch فوق master
# git rebase <فرع-الأساس> <فرع-الموضوع>
$ git rebase master experimentBranch
```

<p dir="rtl"><a href="https://git-scm.com/book/en/v2/Git-Branching-Rebasing">قراءة إضافية</a></p>

<h3 dir="rtl">reset (بحذر)</h3>

<p dir="rtl">
إرجاع HEAD لحالة محددة؛ يلغي دمجاً أو pull أو commit أو add. مفيد لكن خطير إن لم تفهم العواقب.
</p>

```bash
# إعادة الفهرس ليطابق آخر التزام؛ دليل العمل دون تغيير
$ git reset

# فهرس + دليل عمل يطابقان آخر التزام
$ git reset --hard

# نقل طرف الفرع لالتزام محدد؛ الملفات تبقى كما هي في الدليل
$ git reset 31f2bb1

# نقل الطرف للخلف مع مطابقة دليل العمل — يحذف غير الملتزَم وبعد ذلك الالتزامات
$ git reset --hard 31f2bb1
```

<h3 dir="rtl">reflog (بحذر)</h3>

<p dir="rtl">
يعرض معظم أوامر جِت التي نفّذتها لفترة (افتراضياً 90 يوماً)، فيمكنك عكس خطأ (مثل rebase أفسد المشروع).
</p>

<ol dir="rtl">
<li>نفّذ <code>git reflog</code></li>
<li>اختر نقطة الإرجاع، مثلاً <code>2e6c386</code> أو <code>HEAD@{5}</code></li>
<li><code>git reset --hard HEAD@{5}</code></li>
<li>أعد rebase أو اترك الوضع كما هو.</li>
</ol>

```
38b323f HEAD@{0}: rebase -i (finish): returning to refs/heads/feature/add_git_reflog
38b323f HEAD@{1}: rebase -i (pick): Clarify inc/dec operators
4fff859 HEAD@{2}: rebase -i (pick): Update java.html.markdown
34ed963 HEAD@{3}: rebase -i (pick): [yaml/en] Add more resources (#1666)
ed8ddf2 HEAD@{4}: rebase -i (pick): pythonstatcomp spanish translation (#1748)
2e6c386 HEAD@{5}: rebase -i (start): checkout 02fb96d
```

<p dir="rtl"><a href="https://git-scm.com/docs/git-reflog">توثيق reflog</a></p>

<h3 dir="rtl">revert</h3>

<p dir="rtl">
يلغى أثر commit بإنشاء commit جديد معكوس؛ يختلف عن reset الذي يعيد حالة المشروع لنقطة سابقة.
</p>

```bash
# عكس التزام محدد بالتزام جديد
$ git revert <commit>
```

<h3 dir="rtl">rm</h3>

<p dir="rtl">
عكس <code>git add</code>: يزيل الملفات من شجرة العمل الحالية.
</p>

```bash
# حذف HelloWorld.c من التتبع والدليل
$ git rm HelloWorld.c

# ملف في مسار متداخل
$ git rm /pather/to/the/file/HelloWorld.c
```

<h3 dir="rtl">blame</h3>

<p dir="rtl">
يستعرض تاريخ أجزاء من الشيفرة ومن عدّل كل سطر أخيراً.
</p>

```bash
# من عدّل كل سطر (مثال)
$ git blame google_python_style.vim
b88c6a1b (Google Python team  2019-12-30 13:45:23 -0800 12) " See the License for the specific language governing permissions and
b88c6a1b (Google Python team  2019-12-30 13:45:23 -0800 13) " limitations under the License.
b88c6a1b (Google Python team  2019-12-30 13:45:23 -0800 14) 
222e6da8 (mshields@google.com 2010-11-29 20:32:06 +0000 15) " Indent Python in the Google way.
222e6da8 (mshields@google.com 2010-11-29 20:32:06 +0000 16) 
222e6da8 (mshields@google.com 2010-11-29 20:32:06 +0000 17) setlocal indentexpr=GetGooglePythonIndent(v:lnum)
```

<h2 dir="rtl">مزيد من المعلومات</h2>

<ul dir="rtl">
<li><a href="https://learngitbranching.js.org/">Learn Git Branching</a></li>
<li><a href="https://blog.udemy.com/git-tutorial-a-comprehensive-guide/">Udemy Git Tutorial</a></li>
<li><a href="https://gitimmersion.com/">Git Immersion</a></li>
<li><a href="https://git-scm.com/videos">git-scm — فيديوهات</a></li>
<li><a href="https://git-scm.com/docs">git-scm — التوثيق</a></li>
<li><a href="https://www.atlassian.com/git/">Atlassian Git</a></li>
<li><a href="https://res.cloudinary.com/hy4kyit2a/image/upload/SF_git_cheatsheet.pdf">SalesForce Cheat Sheet</a></li>
<li><a href="https://rogerdudler.github.io/git-guide/index.html">git — دليل بسيط</a></li>
<li><a href="https://git-scm.com/book/en/v2">Pro Git</a></li>
<li><a href="https://product.hubspot.com/blog/git-and-github-tutorial-for-beginners">Git وGitHub للمبتدئين</a></li>
<li><a href="https://www.youtube.com/playlist?list=PL6gx4Cwl9DGAKWClAD_iKpNC0bGHxGhcx">The New Boston — جِت</a></li>
<li><a href="https://eagain.net/articles/git-for-computer-scientists/">Git For Computer Scientists</a></li>
</ul>
