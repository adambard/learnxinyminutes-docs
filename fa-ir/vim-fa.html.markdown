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
این به همراه پکیج های vim هنگام نصب کردن، نصب می‌شود. شما باید بتوانید با ران کردن "vimtutor" در کامندلاین از آموزش ها استفاده کنید. این همه‌ی ویژگی های عمده‌ی `vim` را به شما راهنمایی می‌کند.
</p>
<p dir="rtl">
<strong>
مکان‌یابی اولیه vim
</strong>
</p>

<p dir="rtl"></p>

<p dir="rtl">باز کردن فایل در ویم </p>

```
vim <filename>
```

<p dir="rtl">TODO</p>

```
:help <topic>
```

<p dir="rtl">خروج از vim</p>

```
:q               # Quit vim
```

<p dir="rtl">ذخیره کردن فایل فعلی</p>

```
:w               # Save current file
```

<p dir="rtl">ذخیره کردن و خارج شدن از vim</p>

```
:wq              # Save file and quit vim
```

<p dir="rtl">ذخیره کردن و خارج شدن از vim</p>

```
ZZ               # Save file and quit vim
```

<p dir="rtl">خارج شدن بدون ذخیره کردن فایل</p>

```
:q!              # Quit vim without saving file
```
<!--# ! *forces* :q to execute, hence quiting vim without saving-->

<p dir="rtl">خارج شدن بدون ذخیره کردن فایل</p>

```
ZQ               # Quit vim without saving file
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

<p dir="rtl">رفتن یک کاراکتر به چپ</p>

```
h                # Move left one character
```

<p dir="rtl">رفتن یک کاراکتر به پایین</p>

```
j                # Move down one line
```

<p dir="rtl">رفتن یک کاراکتر به بالا</p>

```
k                # Move up one line
```

<p dir="rtl">رفتن یک کاراکتر به راست</p>

```
l                # Move right one character
```

<p dir="rtl">جابه‌جا شدن به عقب به اندازه یک صفحه</p>

```
Ctrl+B           # Move back one full screen
```

<p dir="rtl">جابه‌جا شدن به جلو به اندازه یک صفحه</p>

```
Ctrl+F           # Move forward one full screen
```

<p dir="rtl">جابه‌جا شدن به جلو به اندازه نصف صفحه</p>

```
Ctrl+D           # Move forward 1/2 a screen
```

<p dir="rtl">جابه‌جا شدن به عقب به اندازه نصف صفحه</p>

```
Ctrl+U           # Move back 1/2 a screen
```

<p dir="rtl"><strong>جابه‌جایی در خط</strong></p>
<p dir="rtl">رفتن به اول خط</p>

```
0                # Move to beginning of line
```

<p dir="rtl">رفتن به آخر خط</p>

```
$                # Move to end of line
```

<p dir="rtl">TODO</p>

```
^                # Move to first non-blank character in line
```

<p dir="rtl"><strong>جست و جو در متن</strong></p>
<p dir="rtl">هایلایت کردن همه‌ی کلمه های بعد cursor</p>

```
/word            # Highlights all occurrences of word after cursor
```

<p dir="rtl">هایلایت کردن همه‌ی کلمه های قبل cursor</p>

```
?word            # Highlights all occurrences of word before cursor
```

<p dir="rtl">جابه‌جایی cursor به کلمه های بعدی پیدا شده</p>

```
n                # Moves cursor to next occurrence of word after search
```

<p dir="rtl">جابه‌جایی cursor به کلمه های قبلی پیدا شده</p>

```
N                # Moves cursor to previous occerence of word
```

<p dir="rtl">عوض کردن 'foo' به 'bar' در هر خط از فایل</p>

```
:%s/foo/bar/g    # Change 'foo' to 'bar' on every line in the file
```

<p dir="rtl">عوض کردن 'foo' به 'bar' در خط فعلی</p>

```
:s/foo/bar/g     # Change 'foo' to 'bar' on the current line
```

<p dir="rtl">TODO</p>

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
    # Moving by word

<p dir="rtl"><strong>جابه‌جایی با کلمه ها</strong></p>
<p dir="rtl">رفتن به جلو به اندازه‌ی یک کلمه</p>

```
w                # Move forward by one word
```
<p dir="rtl">رفتن به عقب به اندازه‌ی یک کلمه</p>

```
b                # Move back by one word
```
<p dir="rtl">رفتن به آخر کلمه‌ي فعلی</p>

```
e                # Move to end of current word
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
<p dir="rtl"><strong>داک های help</strong></p>
TODO typo
<p dir="rtl">
Vim دارای یک help doc داخلی است که می‌توان با help: <topic> به آن دسترسی داشت. برای مثال :help navigation داک مربوط به مکان‌یابی در فضای کار را به شما نشان می‌دهد! <br />
help: همچنین می‌تواند بدون option مورد استفاده قرار گیرد.
This will bring up a default help dialog that aims to make getting started with vim more approachable!
</p>

<p dir="rtl"><strong>Modes:</strong></p>


