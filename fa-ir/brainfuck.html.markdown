---
language: brainfuck
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
lang: fa-ir
---

برین فاک زبان برنامه نویسی تورینگ کامل بی نهایت ساده ایست که دارای فقط هشت
دستور است.

هر کارکتری به جر کارکتر های زیر در این زبان در نظر گرفته نمیشود.

`>` `<` `+` `-` `.` `,` `[` `]`

برین فاک به صورت یک آرایه ی سی هزار خانه ای کار میکند که در ابتدا تمامی خانه های آن صفر هستند.
همچنین یک اشاره گر در این برنامه به خانه ی فعلی اشاره میکند.

در زیر هشت دستور این زبان شرح داده شده است:

`+` : یک عدد به خانه ی فعلی اضافه می کند.
`-` : یک عدد از خانه ی فعلی کم می کند.
`>` : اشاره گر به خانه ی بعدی میرود -- به راست
`<` : اشاره گر به خانه ی قبلی میرود -- به چپ
`.` : کارکتر اسکی معادل مقدار خانه ی فعلی را چاپ میکند. -- به عنوان مثال 65 برای A
`,` : یک کارکتر را از ورودی خوانده و مقدار آن را در خانه ی فعلی زخیره میکند.
`[` : اگر مقدار خانه ی فعلی صفر باشد به محل بسته شدن کروشه جهش میکند. -- و از همه ی دستور های بین آن صرف نظر میشود.
در غیر این صورت به دستور بعدی میرود.
`]` : اگر مقدار خانه ی فعلی صفر باشد به خانه ی بعدی و در غیر این صورت به محل باز شدن کروشه جهش می کند. -- به عقب

دو علامت کروشه امکان ایجاد حلقه را فراهم میکنند.

در اینجا یک برنامه ی ساره برین فاک را مشاهده میکنید.

```
++++++ [ > ++++++++++ < - ] > +++++ .
```

این برنامه کارکتر A را بر روی خروجی چاپ میکند.
در این برنامه خانه ی اول به عنوان متغیر حلقه و خانه ی دوم برای مقدار عددی A
ابتدا عدد شش در خانه ی اول ایجاد شده. سپس  برنامه  وارد یک حلقه میشود که در هر بار 
تکرار آن اشاره گر به خانه ی دوم رفته و ده بار به خانه ی فعلی اضافه می کند.
-- و در انتهای حلقه به خانه ی اول برگشته تا حلقه کنترل شود
بعد از اتمام حلقه به خانه ی دوم میرود و پنج بار به این خانه اضافه کرده و سپس آنرا چاپ میکند.

```
, [ > + < - ] > .
```

در این برنامه ابتدا یک 
This program reads a character from the user input and copies the character into
cell #1. Then we start a loop. Move to cell #2, increment the value at cell #2,
move back to cell #1, and decrement the value at cell #1. This continues on
until cell #1 is 0, and cell #2 holds cell #1's old value. Because we're on
cell #1 at the end of the loop, move to cell #2, and then print out the value
in ASCII.

Also keep in mind that the spaces are purely for readability purposes. You
could just as easily write it as:

,[>+<-]>.

Try and figure out what this program does:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

This program takes two numbers for input, and multiplies them.

The gist is it first reads in two inputs. Then it starts the outer loop,
conditioned on cell #1. Then it moves to cell #2, and starts the inner
loop conditioned on cell #2, incrementing cell #3. However, there comes a
problem: At the end of the inner loop, cell #2 is zero. In that case,
inner loop won't work anymore since next time. To solve this problem,
we also increment cell #4, and then recopy cell #4 into cell #2.
Then cell #3 is the result.
```

And that's brainfuck. Not that hard, eh? For fun, you can write your own
brainfuck programs, or you can write a brainfuck interpreter in another
language. The interpreter is fairly simple to implement, but if you're a
masochist, try writing a brainfuck interpreter… in brainfuck.
