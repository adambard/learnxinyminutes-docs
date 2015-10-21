---
language: CSS
translators:
    - ["Arashk", "https://github.com/Arashk-A"]
lang: fa-ir
---

<p dir='rtl'>در روزهای آغازین وب هیچگونه عنصر بصری مشاهده نمیشد و محتوا به صورت متن خالی بود. </p>
<p dir='rtl'>اما با توسعه بیشتر مرورگرها صفحات وب کاملاً تصویری نیز رایج شد</p>
<p dir='rtl'>CSS زبان استانداردی که موجودیت آن برای حفظ جدایی بین محتوا (HTML) و نگاه و احساس از</p>
<p dir='rtl'>صفحات وب است.</p>

<p dir='rtl'>به طور خلاصه, کاری که CSS انجام میدهد ارائه نحوه ایست که شما را قادر به هدف قرار دادن</p>
<p dir='rtl'>عناصر مختلف در یک صفحه HTML کرده و امکان اختصاص خواص متفاوت بصری به آنها را میدهد.</p>


<p dir='rtl'>مانند هر زبانی, CSS نسخه های زیادی دارد که در اینجا توجه ما روی CSS2.0 است. با وجودی که این نسخه جدیدترین نسخه نمیباشد اما بیشترین پشتیبانی و سازگاری را در میان نسخه های مختلف را دارد</p>

<p dir='rtl'><strong>توجه: </strong> برای مشاهده برخی از نتایج جلوه های تصویری CSS به منظور یادگیری بیشتر شما باید چیزهای گوناگونی در محیطی مثل [dabblet](http://dabblet.com/) امتحان کنید. توجه اصلی این مقاله روی دستورات و برخی از نکات عمومی است.</p>


<p dir='rtl'>در CSS همه توضیحات داخل ستاره-بروم نوشته میشوند زیرا CSS دستوری برای توضیحات تک خطی مثل C ندارد</p>

```CSS
/* comments appear inside slash-asterisk, just like this line!
   there are no "one-line comments"; this is the only comment style */
```

<p dir='rtl'>به طور کلی دستورات CSS بسیار ساده هستند که در آن یک انتخابگر (selector) عنصری را در  روی صفحه هدف قرار میدهد.</p>

```CSS
selector { property: value; /* more properties...*/ }
```

<p dir='rtl'>با استفاده از ستاره می توان برای همه عناصر روی صفحه استایل تعریف کرد</p>


```CSS
* { color:red; }
```

<p dir='rtl'>فرض کنید عنصری مثل این بر روی صفحه قرار دارد</p>

```html
<div class='some-class class2' id='someId' attr='value' otherAttr='en-us foo bar' />
```
<p dir='rtl'>شما میتوانید با استفاده از نام کلاس آنرا انتخاب کنید</p>


```CSS
.some-class { }
```

<p dir='rtl'>یا با استفاده از نام دو کلاس</p>

```CSS
.some-class.class2 { }
```

<p dir='rtl'>یا با استفاده از نام id</p>

```CSS
#someId { }
```

<p dir='rtl'>یا با استفاده از نام خود عنصر</p>

```CSS
div { }
```

<p dir='rtl'>یا با استفاده از `attr`</p>

```CSS
[attr] { font-size:smaller; }
```

<p dir='rtl'>یا با استفاده از ارزشی که برای `attr` مشخص شده</p>

```CSS
[attr='value'] { font-size:smaller; }
```

<p dir='rtl'>با استفاده از ارزشی که برای `attr` مشخص شده و آن ارزش با `val` شروع میشود در CSS3</p>

```CSS
[attr^='val'] { font-size:smaller; }
```

<p dir='rtl'>با استفاده از ارزشی که برای `attr` مشخص شده و آن ارزش با `ue` به پایان میرسد در CSS3</p>

```CSS
[attr$='ue'] { font-size:smaller; }
```

<p dir='rtl'>یا با انتخاب بوسیله یکی از ارزشهایی که در لیست `otherAttr` بوسیله فاصله از هم جدا شده اند در CSS3</p>

```CSS
[attr$='ue'] { font-size:smaller; }
```

<p dir='rtl'>یا ارزش(`value`) دقیقاً خود ارزش(`value`) یا بوسیله `-` که یونیکد (U+002D) از حرف بعدی جدا شود</p>

```CSS
[otherAttr|='en'] { font-size:smaller; }
```

<p dir='rtl'>و مهمتر از همه اینکه میتوان آنها را ترکیب کرد. نکته مهمی که در اینجا باید مد نظر داشته باشید این است که هنگام ترکیب نباید هیچگونه فاصله ای بین آنها قرار گیرد زیرا در این حالت معنای دستور تغییر میکند</p>

```CSS
div.some-class[attr$='ue'] { }
```

<p dir='rtl'>CSS این امکان را به شما میدهد که یک عنصر را بوسیله والدین آن انتخاب کنید</p>
<p dir='rtl'>برای مثال دستور زیر همه عناصری را که نام کلاس آنها <span dir="ltr">`.class-name`</span> و دارای پدر و مادری با این مشخصه <span dir="ltr">`div.some-parent`</span> هستند را انتخاب میکند.</p>

```CSS
div.some-parent > .class-name {}
```


<p dir='rtl'>یا دستور زیر که همه عناصری را که نام کلاس آنها <span dir="ltr">`.class-name`</span> و داخل عنصری با مشخصه <span dir="ltr">`div.some-parent`</span> هستند را در هر عمقی که باشند (یعنی فرزندی از فرزندان <span dir="ltr">`div.some-parent`</span><span dir="ltr"> باشند) انتخاب میکند.</p>

```CSS
div.some-parent .class-name {}
```

<p dir='rtl'>نکته ای که در اینجا باید به آن توجه کنید این است که این رستور با فاصله ای بین نام دو کلاس همراه است و با مثال زیر که در بالا هم ذکر شد تفاوت دارد.</p>

```CSS
div.some-parent.class-name {}
```

<p dir='rtl'>دستور زیر همه عناصری را که نام کلاس آنها <span dir="ltr">`.this-element`</span> و بلافاصله بعد از عنصری با مشخصه <span dir="ltr">`.i-am-before`</span> قرار دارد را انتخاب میکند.</p>

```CSS
.i-am-before + .this-element { }
```

<p dir='rtl'>هر خواهر یا برادری که بعد از <span dir="ltr">`.i-am-before`</span> بیاید در اینجا لازم نیست بلافاصله بعد از هم قرار بگیرند ولی باید دارای پدر و مادری یکسان باشند.</p>

```CSS
.i-am-any-before ~ .this-element {}
```
<p dir='rtl'>در زیر چند نمونه از شبه کلاسها را معرفی میکنیم که به شما اجازه میدهد عناصر را بر اساس رفتار آنها در صفحه انتخاب کنید.</p>
<p dir='rtl'>برای مثال زمانی که اشاره گر ماوس روی عنصری بر روی صفحه قرار دارد.</p>

```CSS
selector:hover {}
```

<p dir='rtl'>یا زمانی از یک لینک بازید کردید.</p>

```CSS
selected:visited {}
```

<p dir='rtl'>یا زمانی از لینکی بازید نشده است.</p>

```CSS
selected:link {}
```

<p dir='rtl'>یا زمانی که روی یک عنصر ورودی متمرکز شده.</p>

```CSS
selected:focus {}
```

<h3 dir='rtl'>واحدها</h3>

```CSS
selector {

    /* واحدها اندازه */
    width: 50%; /* در اساس درصد */
    font-size: 2em; /* بر اساس اندازه font-size یعنی دو برابر اندازه فونت فعلی */
    width: 200px; /* بر اساس پیکسل */
    font-size: 20pt; /* بر اساس points (نکات) */
    width: 5cm; /* بر اساس سانتیمتر */
    min-width: 50mm; /* بر اساس میلیمتر */
    max-width: 5in; /* بر اساس اینچ. max-(width|height) */
    height: 0.2vh; /* بر اساس ارتفاع دید `vh = نسبت به 1٪ از ارتفاع دید` (CSS3) */
    width: 0.4vw; /* بر اساس عرض دید `vw = نسبت به 1٪ از عرض دید` (CSS3) */
    min-height: 0.1vmin; /* بر اساس کوچکترین مقدار از ارتفاع یا عرض دید (CSS3) */
    max-width: 0.3vmax; /* مانند مثال بالا برای بیشترین مقدار (CSS3) */

    /* رنگها */
    background-color: #F6E;  /* بر اساس short hex */
    background-color: #F262E2; /* بر اساس long hex format */
    background-color: tomato; /* بر اساس نام رنگ */
    background-color: rgb(255, 255, 255); /* بر اساس rgb */
    background-color: rgb(10%, 20%, 50%); /* بر اساس درصد rgb , (rgb percent) */
    background-color: rgba(255, 0, 0, 0.3); /* بر اساس rgba (نیمه شفاف) , (semi-transparent rgb) (CSS3) */
    background-color: transparent; /* شفاف */
    background-color: hsl(0, 100%, 50%); /* بر اساس hsl format (CSS3). */
    background-color: hsla(0, 100%, 50%, 0.3); /* بر اساس hsla ,مثل RGBAکه میتوان شفافیت را در آخر انتخاب کرد (CSS3) */


    /* عکسها */
    background-image: url(/path-to-image/image.jpg); /* گذاشتن نقل قول داخل url() اختیاری است*/

    /* فونتها */
    font-family: Arial;
    font-family: "Courier New"; /* اگر اسم فونت با فاصله همراه باشد باید داخل نقل قول یک یا دو نوشته شود */
    font-family: "Courier New", Trebuchet, Arial, sans-serif; /* اگر فونت اولی پیدا نشد مرورگر به سراغ نام بعدی میرود */
}
```

<h2 dir='rtl'>نحوه استفاده</h2>

<p dir='rtl'>هر دستور CSS را که می خواهید در فایلی با پسوند <span dir="ltr">.css</span> ذخیره کنید </p>
<p dir='rtl'>حالا با استفاده از کد زیر آنرا در قسمت `head` داخل فایل html خود تعریف کنید </p>

```html
<link rel='stylesheet' type='text/css' href='path/to/style.css' />
```

<p dir='rtl'>یا میتوان با استفاده از تگ `style` درون `head` دستورات CSS را به صورت درون برنامه ای تعریف کرد اما توسیه میشود تا جای ممکن از این کار اجتناب کنید. </p>

```html
<style>
   a { color: purple; }
</style>
```

<p dir='rtl'>همچنین شما میتوانید دستورات CSS را به عنوان یک مشخصه برای عنصر تعریف کنید ولی تا جای ممکن باید از این کار اجتناب کنید.</p>

```html
<div style="border: 1px solid red;">
</div>
```

<h2 dir='rtl'>حق تقدم یا اولویت</h2>

<p dir='rtl'>همانگونه که مشاهده کردید یک مشخصه می تواند به وسیله چندین انتخابگر انتخاب گردد.</p>
<p dir='rtl'>و همچنین یک ویژگی میتواند چندین بار برای یک عنصر تعریف شود.</p>
<p dir='rtl'>در این صورت یک دستور میتواند بر دستورات دیگر حق تقدم یا اولویت پیدا کند.</p>

<p dir='rtl'>به مثال زیر توجه کنید:</p>

```CSS
/*A*/
p.class1[attr='value']

/*B*/
p.class1 {}

/*C*/
p.class2 {}

/*D*/
p {}

/*E*/
p { property: value !important; }

```

<p dir='rtl'>و همچنین به کد زیر:</p>

```html
<p style='/*F*/ property:value;' class='class1 class2' attr='value'>
</p>

```
‍‍
<p dir='rtl'>حق تقدم یا اولویت برای مثال بالا به این صورت است:</p>
<p dir='rtl'>توجه داشته باشید که حق تقدم برای هر کدام از ویژگیها است نه برای کل مجموعه.</p>

<p dir='rtl'>E دارای بیشترین الویت برای اینکه از <span dir="ltr">`!important`</span> استفاده کرده.</p>
<p dir='rtl'>اما توصیه میشود تا جای ممکن از این کار اجتناب کنید مگر اینکه اینکار ضرورت داشته باشد</p>
<p dir='rtl'>اولویت بعدی با F است زیرا که از روش درون برنامه ای استفاده کرده </p>
<p dir='rtl'>اولویت بعدی با A است زیرا که بیشتر از بقیه مشخص تر تعریف شپه </p>
<p dir='rtl'>مشخص تر = مشخص کننده بیشتر. دارای ۳ مشخص کننده: ۱ تگ <span dir="ltr">`p`</span> + ۱ کلاس با نام <span dir="ltr">`class1`</span> + ۱ خاصیت <span dir="ltr">`attr="value"`</span></p>
<p dir='rtl'>اولویت بعدی با C است که مشخصه یکسانی با B دارد ولی بعد از آن تعریف شده است.</p>
<p dir='rtl'>اولویت بعدی با B</p>
<p dir='rtl'>و در آخر D</p>

<h2 dir='rtl'>سازگاری</h2>

<p dir='rtl'>بسیار از ویژگیهای CSS2 (و به تدریج CSS3) بر روی تمام مرورگرها و دستگاه ها سازگارند.اما همیشه حیاتی است که سازگاری CSS مورد استفاده خود را با مرورگر هدف چک کنید.</p>

<p dir='rtl'> یک منبع خوب برای این کار است</p>
[QuirksMode CSS](http://www.quirksmode.org/css/)

<p dir='rtl'>برای یک تست سازگاری سریع, منبع زیر میتواند کمک بزرگی برای این کار باشد.</p>
[CanIUse](http://caniuse.com/)

<h2 dir='rtl'> منابع دیگر </h2>


[Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)

[QuirksMode CSS](http://www.quirksmode.org/css/)

[Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)


