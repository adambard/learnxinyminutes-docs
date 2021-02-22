---
language: javascript
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
translators:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
filename: javascript-fa.js
lang: fa-ir
---

<p dir='rtl'>
جاوااسکریپت توسط برندن ایش از شرکت NetScape در سال 1995 ساخته شد. در ابتدا به عنوان یک زبان اسکریپت‌نویسی  در کنار جاوا (که برای موارد پیچیده تر در طراحی وب در نظر گرفته میشد) مورد استفاده بود، ولی در پی نفوذ بسیار گسترده آن در وب و همچنین پشتیبانی پیش-ساخته آن در مرورگر ها، امروزه به مراتب بیشتر از جاوا در برنامه نویسی سمت-کاربر در وب به کار برده میشود.
با این حال جاوااسکریپت فقط محدود به مرورگر های وب نمیشود. Node.js پروژه ایست که یک نسخه ی مستقل از اجراکننده ی موتور جاوااسکریپت V8 از گوگل کروم را در اختیار قرار میده که هر روزه درحال محبوب تر شدن نیز هست.
</p>

<p dir='rtl'>
قدر دان نظرات سازنده شما هستم! شما میتوانید از طریق زیر با من تماس بگیرید:
</p>

[@ExcitedLeigh](https://twitter.com/ExcitedLeigh), or
[l@leigh.net.au](mailto:l@leigh.net.au).

<p dir='rtl'>
// توضیحات همانند C هستند. توضیحات یک خطی با دو خط مورب شروع میشوند.,
</p>

<p dir='rtl'>
/* و توضیحات چند خطی با خط مورب-ستاره شروع،
   و با ستاره-خط مورب ختم میشوند */
</p>

```js
// Comments are like C. Single-line comments start with two slashes,
/* and multiline comments start with slash-star
   and end with star-slash */
```
<p dir='rtl'>
گزاره ها را میتوانید با نقطه ویرگول پایان دهید ;
</p>
```js
doStuff();
```
<p dir='rtl'>
ولی لزومی به این کار نیست. نقطه ویرگول به صورت خودکار در نظر گرفته میشوند.  
</p>
<p dir='rtl'>
وقتی که خط جدیدی شروع میشود. مگر در موارد خاص.
</p>
```js
doStuff()
```
<p dir='rtl'>برای اینگه درگیر آن موارد خاص نشویم، در اینجا از اون ها  </p>
<p dir='rtl'>صرف نظر میکنیم.</p>

<h2 dir='rtl'>1. اعداد، رشته ها و عملگرها</h2>

<p dir='rtl'>جاوااسکریپت فقط یک نوع عدد دارد و آن عدد اعشاری 64 بیتی IEEE 754 است.</p>
<p dir='rtl'>نترسید! و نگران اعداد صحیح نباشید! این اعداد اعشاری دارای 54 بیت مانتیس هستند که قابلیت ذخیره ی </p>
<p dir='rtl'>دقیق اعداد صحیح تا مقدار تقریبی 9x10¹⁵  را دارند.</p>
```js
3; // = 3
1.5; // = 1.5
```
<p dir='rtl'>
تمامی عملگر های محاسباتی آن طوری که انتظارش را دارید عمل خواهند کرد.
</p>
```js
1 + 1; // = 2
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7
```
<p dir='rtl'>و این حتی شامل تقسیم هم میشود.</p>
```js
5 / 2; // = 2.5
```
<p dir='rtl'>عملگر های بیتی هم به همین شکل. وقتی از یک عملگر بیتی استفاده میکنید، عدد اعشاری شما</p>
<p dir='rtl'>به عدد صحیح علامت دار *تا 32 بیت* تبدیل میشود.</p>
```js
1 << 2; // = 4
```
<p dir='rtl'>عملیات داخل پرانتز تقدم بالاتری دارند.</p>
```js
(1 + 3) * 2; // = 8
```
<p dir='rtl'>سه مقدار خاص وجود دارند که در واقع مقادیر عددی نیستند:</p>
```js
Infinity; // result of e.g. 1/0
-Infinity; // result of e.g. -1/0
NaN; // result of e.g. 0/0
```
<p dir='rtl'>مقادیر بولی هم تعریف شده هستند:</p>
```js
true;
false;
```
<p dir='rtl'>رشته ها با آپستروف و یا گیومه تعریف میشوند.</p>
```js
'abc';
"Hello, world";
```
<p dir='rtl'>و منفی کردن شرط با علامت تعجب</p>
```js
!true; // = false
!false; // = true
```
<p dir='rtl'>تساوی دو مقدار با ==</p>
```js
1 == 1; // = true
2 == 1; // = false
```
<p dir='rtl'>و عدم تساوی با !=</p>
```js
1 != 1; // = false
2 != 1; // = true
```
<p dir='rtl'>و سایر عمیلات های مقایسه</p>
```js
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true
```
<p dir='rtl'>رشته ها با علامت جمع به یکدیگر متصل میشوند</p>
```js
"Hello " + "world!"; // = "Hello world!"
```
<p dir='rtl'>و با علامت برگتر و یا کوچکتر با یکدیگر مقایسه میشوند.</p>
```js
"a" < "b"; // = true
```
<p dir='rtl'>نوع متغیر برای عملیات مقایسه تطبیق داده میشود</p>
```js
"5" == 5; // = true
```
<p dir='rtl'>مگر اینکه از سه مساوی استفاده شود!</p>
```js
"5" === 5; // = false
```
<p dir='rtl'>با استفاده از charAt میتوانید به کارکتر های یک رشته دسترسی پیدا کنید.</p>
```js
"This is a string".charAt(0);
```
<p dir='rtl'>از null برای نشان دادن عمدی مقدار هیج استفاده میشود.</p>
<p dir='rtl'>و از undefined برای نشان دادن اینکه در حال حاظر مقدار موجود نمی باشد، هرچند خود undefined یک مقدار محسوب میشود.</p>
```js
null; // used to indicate a deliberate non-value
undefined; // used to indicate a value is not currently present (although undefined
           // is actually a value itself)
```
<p dir='rtl'>false, null, undefined, NaN, 0 و "" مقدار نادرست و هر چیز دیگر مقدار درست طلقی میشوند.</p>
<p dir='rtl'>توجه داشته باشید که 0 نادرست و "0" درست طلقی میشوند حتی در عبارت 0=="0".</p>

<h2 dir='rtl'> 2. متغیر ها، آرایه ها و شئ ها </h2>

<p dir='rtl'>متغیر ها با کلید واژه var تعریف میشوند. اشیا در جاوااسکریپت دارای نوع پویا هستند، </p>
<p dir='rtl'>بدین شکل که برای تعریف نیازی به مشخص کردن نوع متعیر نیست. </p>
<p dir='rtl'>برای مقدار دهی از علامت مساوی استفاده میشود. </p>
```js
var someVar = 5;
```

<p dir='rtl'>اگر کلید واژه var را قرار ندهید، هیچ خطایی دریافت نخواهید کرد... </p>
```js
someOtherVar = 10;
```

<p dir='rtl'>در عوض  متغیر شما در گستره ی کل برنامه تعریف شده خواهد بود. </p>

<p dir='rtl'>متغیر هایی که تعریف شده ولی مقدار دهی نشوند، دارای مقدار undefined خواهند بود. </p>
```js
var someThirdVar; // = undefined
```

<p dir='rtl'>برای اعمال عملگر های محاسباتی، میانبر هایی وجود دارند: </p>
```js
someVar += 5; // equivalent to someVar = someVar + 5; someVar is 10 now
someVar *= 10; // now someVar is 100
```

<p dir='rtl'>حتی از این هم کوتاهتر برای اضافه یا کم کردن یک عدد با مقدار یک. </p>
```js
someVar++; // now someVar is 101
someVar--; // back to 100
```

<p dir='rtl'>آرایه ها در واقع لیستی مرتب شده از مقادیر مختلف از هر نوعی هستند. </p>
```js
var myArray = ["Hello", 45, true];
```

<p dir='rtl'>به اعضای یک آرایه میتوان از طریق قرار دادن کروشه در جلوی نام آن دسترسی پیدا کرد. </p>
<p dir='rtl'>نمایه ی آرایه از صفر شروع میشود. </p>
```js
myArray[1]; // = 45
```

<p dir='rtl'>آرایه ها ناپایدار و دارای طول قابل تغییر هستند </p>
```js
myArray.push("World");
myArray.length; // = 4
```

<p dir='rtl'>در جاوااسکریپت، اشیاء چیزی شبیه دیکشنری و یا نقشه در زبان های دیگر هستند: </p>
<p dir='rtl'>یک مجموعه ی نامرتب از جفت های کلید-مقدار. </p>
```js
var myObj = {key1: "Hello", key2: "World"};
```

<p dir='rtl'>کلید ها از نوع رشته هستند ولی در صورتی که مقدار معتبری برای اسم گزاری باشند نیازی به آوردن آنها درون گیومه نیست. </p>
```js
var myObj = {myKey: "myValue", "my other key": 4};
```

<p dir='rtl'>اعضای یک شئ را نیز میتوانید با استفاده از کروشه در مقابل نام آنها استخراج کنید. </p>
```js
myObj["my other key"]; // = 4
```

<p dir='rtl'>...و یا از طریق نقطه در صورتی که اسم عضو مورد نظر اسم معتبری برای اسم گزاری باشد.</p>
```js
myObj.myKey; // = "myValue"
```

<p dir='rtl'>اشیاء ناپایدار و قابل اضافه کردن عضو جدید هستند.</p>
```js
myObj.myThirdKey = true;
```

<p dir='rtl'>اگر سعی کنید عضوی را که وجود ندارد استخراج کنید، مقدار undefined را دریافت خواهید کرد. </p>
```js
myObj.myFourthKey; // = undefined
```

<h2 dir='rtl'>3. منطق و ساختار کنترل</h2>

<p dir='rtl'>ساختار if به شکلی که انتظارش را دارید کار میکند.</p>
```js
var count = 1;
if (count == 3){
    // evaluated if count is 3
} else if (count == 4) {
    // evaluated if count is 4
} else {
    // evaluated if it's not either 3 or 4
}
```

<p dir='rtl'>و همینطور حلقه while</p>
```js
while (true) {
    // An infinite loop!
}
```

<p dir='rtl'>حلقه do-while شبیه while است با این تفاوت که حداقل یکبار اجرا میشود.</p>
```js
var input
do {
    input = getInput();
} while (!isValid(input))
```

<p dir='rtl'>حلقه for همانند زبان C و جاوا کار می کند.</p>
<p dir='rtl'>مقدار دهی اولیه; شرط ادامه; چرخش حلقه</p>
```js
for (var i = 0; i < 5; i++){
    // will run 5 times
}
```

<p dir='rtl'>عملگر && و || به ترتیب "و" و "یا" ی منطقی هستند.</p>
```js
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // colour is either red or blue
}
```

<p dir='rtl'>از || همچنین میتوان برای تعیین مقدار پیشفرض استفاده کرد.</p>
```js
var name = otherName || "default";
```

<h2 dir='rtl'>4. توابع و مفاهیم گستره و بستار</h2>

<p dir='rtl'>توابع در جاوااسکریپت با استفاده از کلیدواژه ی function تعریف میشوند.</p>
```js
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"
```

<p dir='rtl'>توابع در جاوااسکریپت نوعی شئ پایه محسوب میشوند، بنابر این می توانید آنها را به اشیاء مختلف</p>
<p dir='rtl'>اضافه کنید و یا به عنوان پارامتر به توابع دیگر ارسال کنید.</p>
<p dir='rtl'>- برای مثال وقتی که با یک رویداد کار میکنید.</p>
```js
function myFunction(){
    // this code will be called in 5 seconds' time
}
setTimeout(myFunction, 5000);
```

<p dir='rtl'>توجه کنید که setTimeout تابعی تعریف شده در جاوااسکریپت نیست، ولی مرورگر ها و node.js از آن پشتیبانی میکنند.</p>


<p dir='rtl'>توابع نیازی به داشتن اسم ندارند. برای  مثال وقتی تابعی را به تابعی دیگر ارسال میکنید</p>
<p dir='rtl'>میتوانید آنرا به صورت بینام تعریف کنید.</p>
```js
setTimeout(function(){
    // this code will be called in 5 seconds' time
}, 5000);
```

<p dir='rtl'>توابع دارای محدوده ی متغیر های خود هستند.</p>
<p dir='rtl'>بر خلاف دیگر ساختار ها - مانند if</p>
```js
if (true){
    var i = 5;
}
i; // = 5 - not undefined as you'd expect in a block-scoped language
```

<p dir='rtl'>به همین دلیل الگوی خاصی به نام "تابعی که بلافاصله صدا زده میشود" پدید آمده </p>
<p dir='rtl'>تا از اضافه شدن متغیر های قسمتی از برنامه به گستره ی کلی برنامه جلوگیری شود.</p>
```js
(function(){
    var temporary = 5;
    // We can access the global scope by assiging to the 'global object', which
    // in a web browser is always 'window'. The global object may have a
    // different name in non-browser environments such as Node.js.
    window.permanent = 10;
})();
temporary; // raises ReferenceError
permanent; // = 10
```

<p dir='rtl'>یکی از برترین ویژگی های جاوااسکریپت مفهومی با نام بستار است</p>
<p dir='rtl'>بدین شکل که اگر تابعی درون تابع دیگری تعریف شود، تابع درونی به تمام متغیر های تابع خارجی دسترسی</p>
<p dir='rtl'>خواهد داشت، حتی بعد از اینکه تابع خارجی به اتمام رسیده باشد.</p>
```js
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout is asynchronous, so the sayHelloInFiveSeconds function will
    // exit immediately, and setTimeout will call inner afterwards. However,
    // because inner is "closed over" sayHelloInFiveSeconds, inner still has
    // access to the 'prompt' variable when it is finally called.
}
sayHelloInFiveSeconds("Adam"); // will open a popup with "Hello, Adam!" in 5s
```

<h2 dir='rtl'>5. دیگر اشیاء، سازنده ها و پیش‌نمونه ها</h2>

<p dir='rtl'>اشیاء میتوانند تابع داشته باشند.</p>
```js
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"
```

<p dir='rtl'>وقتی تابع یک شی صدا زده می شود، تابع میتواند به سایر مقادیر درون آن شی </p>
<p dir='rtl'>از طریق کلید واژه ی this دسترسی داشته باشد.</p>
```js
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"
```


<p dir='rtl'>اینکه مقدار this چه باشد بستگی به این دارد که تابع چگونه صدا زده شود</p>
<p dir='rtl'>نه اینکه تابع کجا تعریف شده است.</p>
<p dir='rtl'>بنابر این تابع بالا اگر بدین شکل صدا زده شود کار نخواهد کرد</p>
```js
var myFunc = myObj.myFunc;
myFunc(); // = undefined
```


<p dir='rtl'>به همین شکل، تابعی که در جای دیگر تعریف شده را میتوانید به یک شی الحاق کنید</p>
<p dir='rtl'>و بدین ترتیب تابع میتواند به مقادیر درون شی از طریق this دسترسی پیدا کند.</p>
```js
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"
```


<p dir='rtl'>اگر تابعی با کلید new صدا زده شوند، شی جدیدی ایجاد شده و تابع در گستره ی آن صدا زده میشود.</p>
<p dir='rtl'>توابعی که بدین شکل صدا زده شوند در واقع نقش سازنده را ایفا می کنند.</p>
```js
var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5
```


<p dir='rtl'>تمامی اشیاء در جاوااسکریپت دارای  یک پیش نمونه هستند</p>
<p dir='rtl'>به شکلی که اگر تابع صدا زده شده بر روی شی مستقیما روی آن تعریف نشده باشد</p>
<p dir='rtl'>اجرا کننده ی برنامه در لیست پیش نمونه به دنبال آن تابع خواهد گشت</p>

<p dir='rtl'>برخی اجرا کننده های جاوااسکریپت به شما اجازه ی دسترسی به پیش نمونه های یک شی را از</p>
<p dir='rtl'>طریق عضو جادویی __proto__ میدهند.</p>
<p dir='rtl'>هرچند این به شناخت پیش نمونه ها کمک میکند ولی در حیطه ی جاوااسکریپت استاندارد قرار نمیگیرد.</p>
<p dir='rtl'>در ادامه شکل استاندارد پیش نمونه ها مورد بررسی قرار میگیرند.</p>
```js
var myObj = {
    myString: "Hello world!",
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
};
myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42
```

<p dir='rtl'>این موضوع در مورد توابع نیز صدق میکند.</p>
```js
myObj.myFunc(); // = "hello world!"
```


<p dir='rtl'>اگر عضو مورد نظر در پیش نمونه ی شی یافت نشود، پیش نمونه ی پیش نمونه جستجو شده و الی آخر</p>
```js
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true
```


<p dir='rtl'>توجه داشته باشید که پیش نمونه ها کپی نمی شوند و هر شی جدید به پیش نمونه موجود اشاره میکند</p>
<p dir='rtl'>بدین ترتیب اگر تابعی به پیش نمونه اضافه شود تمامی اشیاء میتوانند به آن دسترسی پیدا کنند.</p>
```js
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43
```

<p dir='rtl'>پیش تر اشاره شد که __proto__ راه استانداردی برای دسترسی به پیش نمونه نیست و هیچ استانداردی نیز برای دسترسی به پیش نمونه ی یک شی موجود پیش بینی نشده است</p>
<p dir='rtl'>ولی دو راه برای ارائه پیش نمونه برای اشیاء جدید وجود دارد.</p>

<p dir='rtl'>اولی وقتیست که از تابع Object.create استفاده میشود - که اخیرا به زبان اضافه شده است و بنابراین بر روی همه ی پیاده سازی های آن وجود ندارد.</p>
```js
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43
```


<p dir='rtl'>راه دوم - که همه جا قابل استفاده است - مربوط به سازنده ها می شود.</p>
<p dir='rtl'>سازنده ها دارای عضوی با نام prototype هستند. این پیش نمونه ی خود سازنده نیست</p>
<p dir='rtl'>بلکه پیش نمونه ایست که به تمامی اشیاء ساخته شده توسط این سازنده الحاق میشود.</p>
```js
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function(){
        return this.myNumber;
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6
myNewObj2.getMyNumber(); // = 6
```


<p dir='rtl'>رشته ها و سایر سازنده های پیش ساخته ی زبان نیز دارای این ویژگی هستند.</p>
```js
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true
```


<p dir='rtl'>به جز این که این سازنده ها دقیقا مانند سازنده های دیگر نیستند.</p>
```js
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // This code won't execute, because 0 is falsy.
}
```


<p dir='rtl'>ولی به هر حال هم اشیاء عادی و هم اشیاء پیش ساخته هر دو در داشتن پیش نمونه مشترک هستند</p>
<p dir='rtl'>بنابر این شما میتوانید ویژگی و تابع جدیدی به رشته ها - به عنوان مثال - اضافه کنید.</p>


<p dir='rtl'>گاها به از این خاصیت با عنوان پلی فیل و برای اضافه کردن ویژگی های جدید به مجموعه ای از اشیاء فعلی زبان استفاده میشود </p>
<p dir='rtl'>که کاربرد فراوانی در پشتیبانی از نسخه های قدیمیتر مرورگر ها دارد.</p>
```js
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
"abc".firstCharacter(); // = "a"
```


<p dir='rtl'>برای مثال، پیشتر اشاره کردیم که Object.create در نسخه های جدید پشتیبانی نشده است</p>
<p dir='rtl'>ولی میتوان آن را به صورت پلی فیل استفاده کرد.</p>
```js
if (Object.create === undefined){ // don't overwrite it if it exists
    Object.create = function(proto){
        // make a temporary constructor with the right prototype
        var Constructor = function(){};
        Constructor.prototype = proto;
        // then use it to create a new, appropriately-prototyped object
        return new Constructor();
    }
}
```

<h2 dir='rtl'> منابع دیگر </h2>

The [Mozilla Developer
Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript) 
<p dir='rtl'>مرجعی بسیار خوب برای جاوااسکریپت به شکلی که در مرورگر ها مورد استفاده قرار گرفته است.</p>
<p dir='rtl'>از آنجایی که این منبع یک ویکی میباشد همانطور که مطالب بیشتری یاد میگیرید میتوانید به دیگران نیز در یادگیری آن کمک کنید.</p>

MDN's [A re-introduction to
JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
<p dir='rtl'>مشابه مطالبی که اینجا مطرح شده با جزییات بیشتر. در اینجا به شکل عمدی جاوااسکریپت فقط از دیدگاه زبان برنامه نویسی مورد بررسی قرار گرفته</p>
<p dir='rtl'>در حالی که در این منبع میتوانید بیشتر از کاربرد آن در صفحات وب آشنایی پیدا کنید.</p>
[Document Object
Model](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Javascript Garden](http://bonsaiden.github.io/JavaScript-Garden/) 
<p dir='rtl'>راهنمای دقیقی از قسمت های غیر ملموس زبان.</p>

<p dir='rtl'>اضافه بر این در ویرایش این مقاله، قسمت هایی از سایت زیر مورد استفاده قرار گرفته است.</p>
Louie Dinh's Python tutorial on this site, and the [JS
Tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
on the Mozilla Developer Network.
