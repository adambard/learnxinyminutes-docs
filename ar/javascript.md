---
name: JavaScript
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
filename: javascript.js
translators:
    - ["Learn X in Y Minutes (ar)", "https://github.com/adambard/learnxinyminutes-docs"]
---

<p dir="rtl">
أنشأ برندن إيتش جافاسكريبت في Netscape عام 1995. قصد بها في البداية لغة سكربت أبسط للمواقع بجانب جافا للتطبيقات الأعقد، لكن ارتباطها الوثيق بالصفحات ودعم المتصفحات جعلها أشهر من جافا في الواجهات.
</p>

<p dir="rtl">
جافاسكريبت ليست للمتصفح فقط: Node.js يوفّر وقت تشغيل مستقل لمحرك V8، وهو شائع متزايداً.
</p>

<p dir="rtl">
صياغتها شبيهة بـ C؛ إن عرفت C أو جافا فالأساسيات مألوفة. رغم التشابه في الاسم، نموذج الكائنات يختلف جذرياً عن جافا.
</p>

```js
// التعليق أحادي السطر يبدأ بـ //
/* التعليق متعدد الأسطر بين /* و *\/ */

// الجمل يمكن أن تنتهي بـ ;
doStuff();

// ... أو لا، فالفاصلة تُدرج تلقائياً عند سطر جديد في أغلب الأحيان
doStuff()

// حالات استثناء قد تسبب مفاجآت؛ هنا نستخدم الفاصلة المنقوطة باستمرار

///////////////////////////////////
// 1. الأعداد والنصوص والعوامل

// نوع عدد واحد (IEEE 754 مزدوج 64 بت). المانتيسا 52 بت تكفي لتخزين أعداد صحيحة حتى نحو 9×10¹⁵ بدقة.
3; // = 3
1.5; // = 1.5

// حساب أساسي متوقّع
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// القسمة غير الصحيحة
5 / 2; // = 2.5

// باقي القسمة
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// العمليات البتية تحوّل العدد إلى صحيح علامة *حتى* 32 بت
1 << 2; // = 4

// الأولوية بالأقواس
(1 + 3) * 2; // = 8

// قيم خاصة ليست أعداداً حقيقية:
Infinity; // مثلاً 1/0
-Infinity; // مثلاً -1/0
NaN; // مثلاً 0/0 — ليس رقماً

// المنطق
true;
false;

// النصوص بـ ' أو "
'abc';
"Hello, world";

// النفي بـ !
!true; // = false
!false; // = true

// المساواة الصارمة ===
1 === 1; // = true
2 === 1; // = false

// عدم المساواة !==
1 !== 1; // = false
2 !== 1; // = true

// مقارنات إضافية
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// دمج النصوص بـ +
"Hello " + "world!"; // = "Hello world!"

// يعمل مع غير النصوص أيضاً
"1, 2, " + 3; // = "1, 2, 3"
"Hello " + ["world", "!"]; // = "Hello world,!"

// سلوك قد يبدو غريباً
13 + !0; // 14
"13" + !0; // '13true'

// مقارنة نصوص بـ < و >
"a" < "b"; // = true

// == يحوّل الأنواع تلقائياً
"5" == 5; // = true
null == undefined; // = true

// === بدون تحويل
"5" === 5; // = false
null === undefined; // = false

// حرف بـ charAt
"This is a string".charAt(0);  // = 'T'

// مقطع فرعي بـ substring
"Hello world".substring(0, 5); // = "Hello"

// length خاصية وليست دالة
"Hello".length; // = 5

// null و undefined
null;      // عدم قيمة مقصود
undefined; // لا قيمة حالياً (ومع ذلك undefined قيمة بنفسها)

// قيم «زائفة» falsy: false و null و undefined و NaN و 0 و ""
// ملاحظة: 0 زائفة والنص "0" صادقة رغم أن 0 == "0"

///////////////////////////////////
// 2. المتغيرات والمصفوفات والكائنات

// var للتصريح. الأنواع ديناميكية. التعيين بـ =
var someVar = 5;

// بدون var لا خطأ...
someOtherVar = 10;

// ...لكن المتغير يُنشأ في النطاق العام

// تصريح بلا تعيين = undefined
var someThirdVar; // = undefined

// عدة متغيرات بفاصلة
var someFourthVar = 2, someFifthVar = 4;

// اختصارات حسابية
someVar += 5; // someVar = 10
someVar *= 10; // 100

// ++ و --
someVar++; // 101
someVar--; // 100

// المصفوفات قوائم مرتبة بأي أنواع
var myArray = ["Hello", 45, true];

// الوصول بـ [] والفهرس يبدأ من 0
myArray[1]; // = 45

// قابلة للتغيير وطول متغير
myArray.push("World");
myArray.length; // = 4

// تعيين في فهرس
myArray[3] = "Hello";

// إضافة/إزالة من المقدمة أو المؤخرة
myArray.unshift(3); // في البداية
someVar = myArray.shift(); // إزالة الأولى وإرجاعها
myArray.push(3); // في النهاية
someVar = myArray.pop(); // إزالة الأخيرة وإرجاعها

// join بفاصل
var myArray0 = [32,false,"js",12,56,90];
myArray0.join(";"); // = "32;false;js;12;56;90"

// slice من 1 شامل إلى 4 غير شامل
myArray0.slice(1,4); // = [false,"js",12]

// splice: حذف 4 من الفهرس 2 وإدراج "hi","wr","ld"
myArray0.splice(2,4,"hi","wr","ld"); // = ["js",12,56,90]
// myArray0 === [32,false,"hi","wr","ld"]

// الكائنات مثل القواميس: أزواج مفتاح–قيمة
var myObj = {key1: "Hello", key2: "World"};

// المفاتيح نصوص؛ الاقتباس اختياري إن كان المعرّف صالحاً
var myObj = {myKey: "myValue", "my other key": 4};

// الوصول بـ []
myObj["my other key"]; // = 4

// أو بنقطة إن كان المعرّف صالحاً
myObj.myKey; // = "myValue"

// كائنات قابلة للتغيير
myObj.myThirdKey = true;

// خاصية غير موجودة = undefined
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. المنطق وهياكل التحكم

// if كالمعتاد
var count = 1;
if (count == 3){
    // إن count يساوي 3
} else if (count == 4){
    // إن count يساوي 4
} else {
    // غير ذلك
}

// while
while (true){
    // حلقة لا نهائية
}

// do...while يعمل مرة واحدة على الأقل
var input;
do {
    input = getInput();
} while (!isValid(input));

// for: تهيئة؛ شرط؛ خطوة
for (var i = 0; i < 5; i++){
    // خمس مرات
}

// break إلى وسيم خارجي
outer:
for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j ==5) {
            break outer;
            // يخرج من الحلقة الخارجية
        }
    }
}

// for...in على خصائص الكائن
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18};
for (var x in person){
    description += person[x] + " ";
} // description = 'Paul Ken 18 '

// for...of على المكررات (نص، مصفوفة، arguments، إلخ)
var myPets = "";
var pets = ["cat", "dog", "hamster", "hedgehog"];
for (var pet of pets){
    myPets += pet + " ";
} // myPets = 'cat dog hamster hedgehog '

// && و، || أو
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // أحمر أو أزرق
}

// اختصار الدائرة لقيم افتراضية
var name = otherName || "default";

// switch يقارن بـ ===
// break بعد كل case
grade = 'B';
switch (grade) {
  case 'A':
    console.log("Great job");
    break;
  case 'B':
    console.log("OK job");
    break;
  case 'C':
    console.log("You can do better");
    break;
  default:
    console.log("Oy vey");
    break;
}


///////////////////////////////////
// 4. الدوال والنطاق والإغلاقات

// الدوال بكلمة function
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// return والقيمة في نفس السطر
function myFunction(){
    return // <- فاصلة منقوطة تُدرج هنا
    {thisIsAn: 'object literal'};
}
myFunction(); // = undefined

// دوال من الدرجة الأولى
function myFunction(){
    // بعد 5 ثوانٍ
}
setTimeout(myFunction, 5000);
// setTimeout من البيئة

function myFunction(){
    // كل 5 ثوانٍ
}
setInterval(myFunction, 5000);

// دالة مجهولة كمعامل
setTimeout(function(){
    // بعد 5 ثوانٍ
}, 5000);

// نطاق الدالة لا الكتلة
if (true){
    var i = 5;
}
i; // = 5

// IIFE
(function(){
    var temporary = 5;
    // العام في المتصفح window
    window.permanent = 10;
})();
temporary; // ReferenceError
permanent; // = 10

// closures
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
}
sayHelloInFiveSeconds("Adam"); // "Hello, Adam!" بعد 5 ثوانٍ

///////////////////////////////////
// 5. كائنات أبعد؛ بانيات وأنماط أولية

// الكائنات قد تحتوي دوالاً
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

// الدوال المرتبطة بالكائن تصل إليه بـ this
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

// قيمة this تعتمد على طريقة الاستدعاء لا مكان التعريف
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// يمكن إسناد دالة للكائن فتكتسب this
var myOtherFunc = function(){
    return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

// call و apply لتحديد this

var anotherFunc = function(s){
    return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// apply يأخذ مصفوفة معاملات

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// مفيد عند تمرير مصفوفة لدالة تتوقع معاملات متتابعة

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

// bind يربط this بشكل دائم

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// جزئي التطبيق (curry)

var product = function(a, b){ return a * b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// new ينشئ كائناً ويمرّره كـ this — دوال البناء

var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

// لا «صفوف» كلاسيكية؛ النمط الأصلي (prototype) يجمع الإنشاء والوراثة

// كل كائن له prototype؛ إن لم توجد الخاصية على الكائن يُبحث في النمط

// __proto__ مفيد للشرح لكنه ليس دائماً معيارياً
var myObj = {
    myString: "Hello world!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase();
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// ينطبق على الدوال
myObj.myFunc(); // = "hello world!"

// السلسلة تصعد في النمط الأصلي
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// مراجع لا نسخاً؛ تعديل النمط يظهر في كل المواضع
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// for...in يصعد سلسلة النمط
for (var x in myObj){
    console.log(myObj[x]);
}
// مثال مخرجات (نفس النصوص التي تطبعها console.log):
// ... Hello world! ثم 43 ثم الدالة ثم true

// خصائص الكائن نفسه فقط بـ hasOwnProperty
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
// بعد hasOwnProperty يبقى سطر الترحيب فقط في المثال

// طريقتان لكائن بنمط أولي محدد: Object.create

var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// أو عبر Constructor.prototype — ليس prototype الدالة بل للأشياء الجديدة
MyConstructor.prototype = {
    myNumber: 5,
    getMyNumber: function(){
        return this.myNumber;
    }
};
var myNewObj2 = new MyConstructor();
myNewObj2.getMyNumber(); // = 5
myNewObj2.myNumber = 6;
myNewObj2.getMyNumber(); // = 6

// الأنواع المدمجة لها بانيات تغلّف القيم
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// ليست متطابقة تماماً
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // لا يُنفَّذ — 0 falsy
}
if (new Number(0)){
   // يُنفَّذ — الكائنات truthy
}

// النمط المشترك يسمح بتوسيع String مثلاً
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// polyfill لميزات حديثة في بيئات قديمة

if (Object.create === undefined){ // لا تستبدل الموجود
    Object.create = function(proto){
        var Constructor = function(){};
        Constructor.prototype = proto;
        return new Constructor();
    };
}

// إضافات ES6

// let نطاق معجمي لا نطاق دالة مثل var
let name = "Billy";

name = "William";

// const لا إعادة تعيين بعد التهيئة

const pi = 3.14;

pi = 4.13; // ممنوع

// دوال سهم ES6 في نطاق معجمي

const isEven = (number) => {
    return number % 2 === 0;
};

isEven(7); // false

function isEven(number) {
    return number % 2 === 0;
};

// الدالة السهمية لا تُرفع (hoisted) — استدعاء قبل التعريف خطأ

add(1, 8);

const add = (firstNumber, secondNumber) => {
    return firstNumber + secondNumber;
};
```

<h2 dir="rtl">قراءة إضافية</h2>

<p dir="rtl"><a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript">MDN</a> توثيق ممتاز لجافاسكريبت في المتصفح.</p>

<p dir="rtl">مقال <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript">إعادة التعريف بجافاسكريبت</a> يوسّع ما هنا. هذا الدليل يركّز على اللغة فقط؛ لاستخدامها في الصفحات ابدأ بـ <a href="https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core">DOM</a>.</p>

<p dir="rtl"><a href="https://shamansir.github.io/JavaScript-Garden/">JavaScript Garden</a> لجوانب غير بديهية في اللغة.</p>

<p dir="rtl"><a href="https://www.amazon.com/gp/product/0596805527/">JavaScript: The Definitive Guide</a> مرجع كلاسيكي.</p>

<p dir="rtl"><a href="https://eloquentjavascript.net/">Eloquent JavaScript</a> كتاب/كتاب إلكتروني ممتاز.</p>

<p dir="rtl"><a href="https://javascript.info/">javascript.info</a> دروس حديثة للأساسيات والمتقدم.</p>
