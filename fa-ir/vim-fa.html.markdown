---
category: tool
tool: vim
contributors:
    - ["RadhikaG", "https://github.com/RadhikaG"]
translators:
    - ["Moein Halvaei", "https://github.com/mo1ein"]
lang: fa-ir
filename: LearnVim-fa.txt
---

<p dir="rtl">
[Vim](http://www.vim.org) (Vi رشدیافته) یک کلون از ادیتور مشهور vi است برای unix. ادیتورمتنی‌ست که برای سرعت و افزایش بهره‌وری طراحی شده‌ و در همه جا به ویژه در سیستم های unix-based دیده می‌شود. شورتکات کلید های بی‌شماری برای سرعت جهت‌یابی به نقاط ویژه‌ي فایل ها و تغییر سریع، دارد.
<br />
`vimtutor` یک برنامه‌ي عالیست که به شما چگونگی استفاده از `vim` را آموزش می‌دهد.
این به همراه پکیج های vim هنگام نصب کردن، نصب می‌شود. شما باید بتوانید با ران کردن `vimtutor` در کامندلاین از آموزش ها استفاده کنید. این همه‌ی ویژگی های عمده‌ی `vim` را به شما راهنمایی می‌کند.
</p>
<h3>
	<p dir="rtl">
مکان‌یابی اولیه vim
	</p>
</h3>


<p dir="rtl">
باز کردن `<filename>` در ویم
</p>

```
vim <filename>   # Open <filename> in vim
```

<p dir="rtl">
باز کردن help docs های `<topic>` اگر وجود داشته باشد
</p>

```
:help <topic>    # Open up built-in help docs about <topic> if any exists 
```
```
:q               # خروج از ویم

:w               # ذخیره کردن فایل فعلی

:wq              # ذخیره کردن و خارج شدن از ویم

ZZ               # ذخیره کردن و خارج شدن از ویم

:q!              # خارج شدن بدون ذخیره کردن فایل

! *forces* :q to execute, hence quiting vim without saving

ZQ               # خارج شدن بدون ذخیره کردن فایل
```

<p dir="rtl">ذخیره کردن و خارج شدن از vim ورژن خلاصه شده‌ی wq:</p>

```
:x               # Save file and quit vim, shorter version of :wq
```

<p dir="rtl">برگشت به عقب</p>

```
 u                # Undo
```

<p dir="rtl">رفتن به جلو</p>

```
CTRL+R           # Redo
```

<p dir="rtl">راه رفتن در صفحه</p>

```
h                # رفتن یک کاراکتر به چپ

j                # رفتن یک کاراکتر به پایین

k                # رفتن یک کاراکتر به بالا

l                # رفتن یک کاراکتر به راست

Ctrl+B           # جابه‌جا شدن به عقب به اندازه یک صفحه

Ctrl+F           # جابه‌جا شدن به جلو به اندازه یک صفحه

Ctrl+D           # جابه‌جا شدن به جلو به اندازه نصف صفحه

Ctrl+U           # جابه‌جا شدن به عقب به اندازه نصف صفحه
```

<p dir="rtl"><strong>جابه‌جا شدن در خط</strong></p>

```
0                # رفتن به اول خط
$                # رفتن به آخر خط
^                # رفتن به اولین کاراکتر غیرخالی در خط
```

<p dir="rtl"><strong>جست و جو در متن</strong></p>

```
/word            # هایلایت کردن همه‌ی کلمه های بعد کِرسر

?word            # هایلایت کردن همه‌ی کلمه های قبل کِرسر

n                # جابه‌جایی کِرسر به کلمه های بعدی پیدا شده

N                # جابه‌جایی کِرسر به کلمه های قبلی پیدا شده
```

<p dir="rtl">عوض کردن 'foo' به 'bar' در هر خط از فایل</p>

```
:%s/foo/bar/g    # Change 'foo' to 'bar' on every line in the file
```

<p dir="rtl">عوض کردن 'foo' به 'bar' در خط فعلی</p>

```
:s/foo/bar/g     # Change 'foo' to 'bar' on the current line
```

<p dir="rtl">جایگزینی کاراکتر های خط جدید با کاراکتر های خط جدید</p>

```
:%s/\n/\r/g      # Replace new line characters with new line characters
```

<p dir="rtl"><strong>پرش به کاراکتر ها</strong></p>
<p dir="rtl">پرش به جلو و قرار گرفتن روی کاراکتر مورد نظر</p>

```
f<character>     # Jump forward and land on <character>
```

<p dir="rtl">پرش به جلو و قرار گرفتن قبل کاراکتر مورد نظر</p>

```
t<character>     # Jump forward and land right before <character>
```

<p dir="rtl"><strong>برای مثال:</strong></p>

<p dir="rtl">پرش به جلو و قرار گرفتن روی ></p>

```
f<               # Jump forward and land on <
```

<p dir="rtl">پرش به جلو و قرار گرفتن قبل از ></p>

```
t<               # Jump forward and land right before <
```
<p dir="rtl"><strong>جابه‌جا شدن با کلمه ها</strong></p>

```
w                # رفتن به جلو به اندازه‌ی یک کلمه
b                # رفتن به عقب به اندازه‌ی یک کلم
e                # رفتن به آخر کلمه‌ی فعلی
```
<p dir="rtl"><strong>سایر کاراکتر ها برای جابه‌جایی</strong></p>
<p dir="rtl">رفتن به اول فایل</p>

```
gg               # Go to the top of the file
```
<p dir="rtl">رفتن به آخر فایل</p>

```
G                # Go to the bottom of the file
```
<p dir="rtl">رفتن به شماره‌ی خط مورد نظر (NUM شماره است)</p>

```
:NUM             # Go to line number NUM (NUM is any number)
```
<p dir="rtl">رفتن به اول صفحه</p>

```
H                # Move to the top of the screen
```
<p dir="rtl">رفتن به وسط صفحه</p>

```
M                # Move to the middle of the screen
```
<p dir="rtl">رفتن به آخر صفحه</p>

```
L                # Move to the bottom of the screen
```

<h3>
<p dir="rtl"><strong>
داک های help
</strong></p>
</h3>

<p dir="rtl">
Vim دارای یک help doc داخلی است که می‌توان با help: <topic> به آن دسترسی داشت. برای مثال help navigation: داک مربوط به مکان‌یابی در فضای کار را به شما نشان می‌دهد! <br /><br />
help: همچنین می‌تواند بدون option مورد استفاده قرار گیرد.
این یه صورت یک help پیش‌فرض بالا می‌آید که شروع vim را قابل دسترس تر می‌کند!
</p>

<h3>
	<p dir="rtl"><strong>Modes:</strong></p>
</h3>

<div dir="rtl">
Vim بر پایه‌ی مفهومی‌ست به نام <strong>modes</strong>
<br /><br />
<ul>
    <li>
    Command Mode - ویم در این حالت بالا می‌آید،‌ برای مکان‌یابی و نوشتن دستورات استفاده می‌شود
</li>
<li>
    Insert Mode - برای ایجاد تغییر در فایل شما استفاده می‌شود
</li>
<li>
    Visual Mode - برای هایلایت کردن متن و انجام عملی روی آن ها استفاده می‌شود
</li>
<li>
    Ex Mode - برای وارد کردن دستورات توسط ":" در قسمت پایین استفاده می‌شود
</li>
</ul>
<br />
</div>

<p dir="rtl">رفتن به حالت insert, پیش از جایگاه cursor</p>

```
i                # Puts vim into insert mode, before the cursor position
```
<p dir="rtl">رفتن به حالت insert, پس از جایگاه cursor</p>

```   
a                # Puts vim into insert mode, after the cursor position
```

<p dir="rtl">رفتن به حالت visual</p>

```   
v                # Puts vim into visual mode
```
<p dir="rtl">رفتن به حالت ex</p>

```
:                # Puts vim into ex mode
```
<p dir="rtl">خروج از همه‌ی حالت ها و رفتن به حالت command</p>

```    
<esc>            # 'Escapes' from whichever mode you're in, into Command mode
```
<p dir="rtl">کپی و پیست در متن</p>

```
y                # کپی کردن متن انتخاب شده

yy               # کپی کردن خط فعلی

d                # حذف کردن متن انتخاب شده

dd               # حذف کردن خط فعلی

p                # پیست کردن متن کپی شده پس از جایگاه فعلی کِرسر

P                # پیست کردن متن کپی شده پیش از جایگاه فعلی کِرسر

x                # حذف کردن یک کاراکتر از جایگاه کِرسر
```

<h3>
<p dir="rtl"><strong>گرامر (Grammer) </strong></p>
</h3>

<div dir="rtl">
Vim را می توان به عنوان مجموعه ای از دستورات در قالب (Verb - Modifier - Noun) تصور کرد ، جایی که:
<br /><br />
<ul>
<li>
    Verb - عمل شما
</li>
<li>
    Modifier - چگونگی انجام عمل شما
</li>
<li>
    Noun - شیئی که عمل شما بر اساس آن عمل می کند
</li>
</ul>
اندکی از مثال های مهم Verbs ,Modifiers, Nouns:
<br /><br />
</div>

<p dir="rtl"><strong>فعل ها (Verbs)</strong></p>

```
d                # حذف
c                # تغییر
y                # کپی
v                # انتخاب 
```
<p dir="rtl"><strong>تغییردهنده ها (Modifiers)</strong></p>

```
i                # داخل
a                # اطراف
NUM              # شماره (NUM هر شماره‌ای است)
f                # جست و جو کردن چیزی و متوقف شدن روی آن
t                # جست و جو کردن چیزی و متوقف شدن قبل از آن
/                # جست و جو کردن رشته‌ای پس از کِرسر
?                # جست و جو کردن رشته‌ای پیش از کِرسر
```
<p dir="rtl"><strong>اسم ها (Nouns)</strong></p>

```
w                # کلمه
s                # جمله
p                # پاراگراف
b                # بلوک
```
<p dir="rtl"><strong>جمله ها و کامند های نمونه</strong></p>

```
d2w              # حذف دو کلمه
cis              # تغییر داخل جمله
yip              # کپی داخل پاراگراف (از پاراگرافی که داخل آن هستید کپی کنید)
ct<              # متن را از جایی که قرار دارید به براکت باز بعدی تغییر دهید
d$               # حذف تا پایان
```

<h3>
	<p dir="rtl">بعضی از شورتکات ها و ترفند ها</p>
</h3>

```
<!--TODO: Add more!-->

>                # ایجاد دندانه به اندازه یک بلوک

<                # حذف دندانه به اندازه یک بلوک

:earlier 15m     # برگرداندن همه چیز به ۱۵ دقیقه قبل
    
:later 15m       # برعکس کامند قبلی
    
ddp              # تغییر مکان خطوط متوالی(dd, then p)
    
.                # تکرار دستور قبلی
    
:w !sudo tee %   # ذخیره کردن فایل فعلی به عنوان روت
    
:set syntax=c    # تنظیم سینتکس هایلایتینگ روی 'c'
    
:sort            # مرتب کردن همه‌ی خطوط
    
:sort!           # مرتب کردن همه‌ی خطوط به صورت برعکس

:sort u          # مرتب کردن همه‌ی خطوط و پاک کردن تکراری ها

~                # تبدیل متن انتخاب شده به حروف (اگر بزرگ است، کوچک و اگر کوچک است، بزرگ)

u                # تبدیل متن انتخاب شده به حروف کوچک

U                # تبدیل متن انتخاب شده به حروف بزرگ

J                # اتصال خط فعلی به خط بعدی
```
<h4>
<p dir="rtl">
فولد (Fold)
</p>
</h4>

```
zf               # ایجاد فولد برای متن انتخاب شده
zo               # باز کردن فولد فعلی
zc               # بستن فولد فعلی
zR               # باز کردن همه‌ی فولد ها
zM               # بستن همه‌ی فولد ها 
```

<h3>
<p dir="rtl">
ماکرو ها (Macros)
</p>
</h3>

<p dir="rtl">
ماکرو ها اساسا عمل های قابل ضبط هستند. زمانی که شما شروع می‌کنید به ضبط ماکرو، هر عمل و دستوری را که استفاده می‌کنید، تا زمانی که ضبط را متوقف کنید، ضبط می‌شود. با فراخوانی ماکرو، دقیقاً همان توالی اعمال و دستورات، دوباره روی متن انتخاب شده اعمال می‌شود. 
</p>

```
qa               # Start recording a macro named 'a'
q                # Stop recording
@a               # Play back the macro
```
<h3>
<p dir="rtl">
کانفیگ vimrc./~
<p>
</h3>

<p dir="rtl">
vimrc. فایلی‌ست که استفاده می‌شود برای کانفیگ vim هنگام بالا آمدن
<br />
این‌جا یک نمونه‌ فایل vimrc. آورده شده:
</p>

```vim
" Example ~/.vimrc
" 2015.10

" Required for vim to be iMproved
set nocompatible

" Determines filetype from name to allow intelligent auto-indenting, etc.
filetype indent plugin on

" Enable syntax highlighting
syntax on

" Better command-line completion
set wildmenu

" Use case insensitive search except when using capital letters
set ignorecase
set smartcase

" When opening a new line and no file-specific indenting is enabled,
" keep same indent as the line you're currently on
set autoindent

" Display line numbers on the left
set number

" Indentation options, change according to personal preference

" Number of visual spaces per TAB
set tabstop=4

" Number of spaces in TAB when editing
set softtabstop=4

" Number of spaces indented when reindent operations (>> and <<) are used
set shiftwidth=4
" Convert TABs to spaces
set expandtab

" Enable intelligent tabbing and spacing for indentation and alignment
set smarttab
```

<h3>
<p dir="rtl">رفرنس ها</p>
</h3>

[Vim | Home](http://www.vim.org/index.php)

`$ vimtutor`

[A vim Tutorial and Primer](https://danielmiessler.com/study/vim/)

[What are the dark corners of Vim your mom never told you about? (St

[Arch Linux Wiki](https://wiki.archlinux.org/index.php/Vim)    
