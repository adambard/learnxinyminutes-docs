---
language: JavaScript
category: language
filename: javascript-zh-tw.js
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
    - ["Ariel Krakowski", "http://www.learneroo.com"]
translators:
    - ["Woody Chang", "https://github.com/kazettique"]
---

JavaScript 是由網景公司（Netscape）的布蘭登·艾克（Brendan Eich）於 1995 年創建的。它最初被設計為一種更簡單的網站腳本語言，用於補足 Java 在更複雜的網路應用程式中使用，但由於它與網頁的高度整合，以及瀏覽器對 JavaScript 的原生支援，使得它在網頁前端的應用遠比 Java 更加普及。

然而 JavaScript 並不僅限於網頁瀏覽器：Node.js，一個可提供 Google Chrome 的 V8 JavaScript 引擎執行環境的專案，正變得越來越熱門。

JavaScript 具備類似 C 語言的語法，所以若您曾使用過 C 或 Java 等語言，許多基本語法對您來說已經相當熟悉了。雖然在語法上、名稱上與 Java 很類似，但是 JavaScript 的物件模型卻與 Java 有顯著地不同。

```js
// 這是單行註解
/* 這是
   多行註解 */

// 陳述式可以用分號（;）終止
doStuff();

// ... 然而不一定要加分號，當換行時會自動插入分號（除了某些特殊情況）。
doStuff()

// 避免意外結果的情況，本文會繼續使用分號

///////////////////////////////////
// 1. 數字、字串和運算子

// JavaScript 只有一種數字型別（也就是 64 位元 IEEE 754 雙精度浮點數）。
// 雙精度浮點數有一個 52 位的尾數，足以精確儲存整數最大至 9✕10¹⁵ 的整數。
3; // = 3
1.5; // = 1.5

// 所有基本算術運算都如您預期。
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// 包括無法整除的除法運算。
5 / 2; // = 2.5

// 以及餘數運算。
10 % 2; // = 0
30 % 4; // = 2
18.5 % 7; // = 4.5

// 位元運算也是如此，當執行位元運算時，浮點數會轉換「最多」32位元的有符號整數
1 << 2; // = 4

// 以括號決定運算優先級。
(1 + 3) * 2; // = 8

// 有三個非數值的值：
Infinity; // 1/0 的結果
-Infinity; // -1/0 的結果
NaN; // 0/0 的結果

// 也有布林值。
true;
false;

// 透過單引號（'）或雙引號（"）建立字串。
'abc';
"Hello, world";

// 以驚嘆號（!）執行否定運算
!true; // = false
!false; // = true

// 相等運算 `===`
1 === 1; // = true
2 === 1; // = false

// 不相等運算 !==
1 !== 1; // = false
2 !== 1; // = true

// 更多比較運算子
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// 以加號(+)進行字串的串接
"Hello " + "world!"; // = "Hello world!"

// 也可以串接字串以外的資料型別
"1, 2, " + 3; // = "1, 2, 3"
"Hello " + ["world", "!"]; // = "Hello world,!"

// 這可能導致一些奇怪的行為
13 + !0; // 14
"13" + !0; // '13true'

// 以 `<` 和 `>` 進行比較運算
"a" < "b"; // = true

// 使用「兩個等號」（==）做運算時，會執行資料強制轉型
"5" == 5; // = true
null == undefined; // = true

// 除非使用「三個等號」（===）
"5" === 5; // = false
null === undefined; // = false

// 您可以使用 `charAt` 來取得字串中的字符
"This is a string".charAt(0);  // = 'T'

// 或使用 `substring` 獲得更大的區塊。
"Hello world".substring(0, 5); // = "Hello"

// `length` 是一個屬性，因此不要使用 `()`。
"Hello".length; // = 5

// 還有 `null` 和 `undefined` 兩個特殊值。
null;      // 用於表示刻意指定為空值
undefined; // 用來表示目前尚未指定值(儘管 `undefined` 實際上本身是一個值)

// `false`、`null`、`undefined`、`NaN`、`0` 及 `""`（空字串）皆為偽值（falsy），
// 其他的皆為真值（truthy）。
// 特別注意，`0` 是偽值，`"0"` 則是真值，儘管 0 == "0"。（因為隱含地轉型）

///////////////////////////////////
// 2. 變數、陣列與物件

// 以 `var` 關鍵字宣告變數。JavaScript 是動態型別，因此無需指定型別。
// 使用一個等號 `=` 來賦值。
var someVar = 5;

// 若忽略使用 `var` 關鍵字，也不會得到錯誤
someOtherVar = 10;

// ...但是您定義的變數將在自動建立在全域，而不在您定義的作用域中。

// 若在定義變數時未賦予初始值，則預設為 `undefined`。
var someThirdVar; // = undefined

// 若要一次宣告多個變數，可以使用逗號分隔
var someFourthVar = 2, someFifthVar = 4;

// 變數的數學運算有一些簡寫法：
someVar += 5; // 等效於 somevar = somevar + 5，現在為 10
someVar *= 10; // 現在 someVar 為 100

// 對於增減 1 的運算，還有更簡略的寫法：
someVar++; // 現在 someVar 為 101
someVar--; // 回到 100

// 陣列是以任何型別的資料所組成、有順序性的列表。
var myArray = ["Hello", 45, true];

// 可使用中括號（方括號）`[]` 語法存取其成員。
// 陣列的索引值（index）從 0 開始。
myArray[1]; // = 45

// 陣列是可變的，並有隨成員數量變動的長度 `length`。
myArray.push("World");
myArray.length; // = 4

// 於指定的索引值新增/修改陣列的成員
myArray[3] = "Hello";

// 從陣列的最前端或最後端新增或刪除元素
myArray.unshift(3); // 新增元素至最前端
someVar = myArray.shift(); // 移除第一個元素並回傳
myArray.push(3); // 新增元素至最後端
someVar = myArray.pop(); // 移除最後一個元素並回傳

// 以分號 `;` 結合陣列的所有元素
var myArray0 = [32, false, "js", 12, 56, 90];
myArray0.join(";"); // = "32;false;js;12;56;90"

// 取得索引 1（包括）到 4（排除）元素的子陣列
myArray0.slice(1, 4); // = [false, "js", 12]

// 從索引 2 開始刪除 4 個元素，然後在那裡插入字串 "hi"、"wr" 和"ld"，並回傳刪除的子陣列
myArray0.splice(2, 4, "hi", "wr", "ld"); // = ["js", 12, 56, 90]
// myArray0 === [32, false, "hi", "wr", "ld"]

// JavaScript 的物件等同於其他語言中的「字典」（Dictionary）或「映射」（Map）：
// 無順序性的鍵值對（key value pair）集合。
var myObj = { key1: "Hello", key2: "World" };

// 鍵的名稱是字串，若它們是有效的 JavaScript 標識字，則不需要使用引號。
// 值則可以是任何資料型別。
var myObj = { myKey: "myValue", "my other key": 4 };

// 物件屬性也可以使用下標語法存取
myObj["my other key"]; // = 4

// ...或使用 `.` 來存取，前提是該鍵值必須是有效的標識字。
myObj.myKey; // = "myValue"

// 物件是可變的；其值可修改，並可新增新的鍵值。
myObj.myThirdKey = true;

// 如果您嘗試存取不存在的值，將得到 `undefined` 值。
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. 邏輯和控制結構

// `if` 結構，如同其他語言
var count = 1;
if (count == 3){
    // count 等於 3 時執行
} else if (count == 4){
    // count 等於 4 時執行
} else {
    // 其他情況下執行
}

// `while` 迴圈
while (true){
    // 無窮迴圈！
}

// Do-while 迴圈類似 While 迴圈，但它至少執行一次
var input;
do {
    input = getInput();
} while (!isValid(input));

// `for` 迴圈和 C、Java 語言一樣：
// 初始化; 繼續條件; 迭代。
for (var i = 0; i < 5; i++){
    // 會執行 5 次
}

// 類似 Java，利用迴圈標籤（label）來終止外層的迴圈
outer:
for (var i = 0; i < 10; i++) {
    for (var j = 0; j < 10; j++) {
        if (i == 5 && j ==5) {
            break outer;
            // 不但終止內部迴圈，也終止外部迴圈
        }
    }
}

// for/in 陳述式可以迭代物件中的所有屬性。
var description = "";
var person = { fname: "Paul", lname: "Ken", age: 18 };
for (var x in person){
    description += person[x] + " ";
} // description 為 'Paul Ken 18 '

// for/of 陳述式允許迭代可迭代物件（包括內建的字串、陣列、例如類陣列（array-like）參數
// 或 NodeList 物件、Typedarray 物件、映射（Map）和集合（Set），
// 及用戶自定義的可迭代物件（iterables））。
var myPets = "";
var pets = ["cat", "dog", "hamster", "hedgehog"];
for (var pet of pets){
    myPets += pet + " ";
} // myPets 為 'cat dog hamster hedgehog '

// `&&` 是邏輯且（and）, `||` 是邏輯或（or）
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // colour 等於 "red" 或 "blue" 時執行
}

// `||` 可用來設定初始值，稱做「短路」（short circuit）陳述式
var name = otherName || "default";

// `switch` 陳述式使用 `===` 來檢查相等性。
// 在每個 case 後使用 `break`，否則之後的 case 也會被執行。
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
// 4. 函式、作用域與閉包

// JavaScript 函式是使用 `function` 關鍵字來宣告的。
function myFunction(thing) {
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

// 值得注意的是，要回傳的值必須開始於關鍵字 `return` 那一行，
// 否則會因為自動插入分號，而將回傳 `undefined`。
// 在使用 Allman 程式碼風格時要特別注意。
function myFunction()
{
    return // <- 分號在此自動插入
    {
        thisIsAn: 'object literal'
    };
}
myFunction(); // = undefined

// JavaScript 函式為一等物件（first-class objects），
// 所以它們可以被重新賦值給不同的變數，
// 並作為參數傳遞給其他函式 - 例如一個事件處理函式：
function myFunction() {
    // 這段程式碼將在 5 秒後執行
}
setTimeout(myFunction, 5000);
// 註：setTimeout 並不是 JS 語言的一部分，而是由瀏覽器和 Node.js 提供的 API。

// setInterval 是瀏覽器提供的另一個 API
function myFunction() {
    // 這段程式碼每 5 秒執行一次
}
setInterval(myFunction, 5000);

// 函式物件甚至不需要用名稱來宣告 - 你可以直接在另一個函數的參數中直接定義匿名函式。
setTimeout(function(){
    // 這段程式碼將在 5 秒後執行
}, 5000);

// JavaScript 具有函式作用域（scope）；函式擁有自己的作用域，但其他區塊（block）則沒有。
if (true){
    var i = 5;
}
i; // = 5 - 並非在其他語言所預期的 `undefined`

// 這導致了一種常見的使用方式，即「立即執行匿名函式」，它可以防止臨時變數洩漏到全域作用域。
(function(){
    var temporary = 5;
    // 我們可以透過賦值給「全域物件」來訪問全域作用域，
    // 在網頁瀏覽器中，這個全域物件始終是 `window`。
    // 在非瀏覽器環境中（例如 Node.js），全域物件可能有不同的名稱。
    window.permanent = 10;
})();
temporary; // 拋出錯誤 ReferenceError
permanent; // = 10

// 閉包：JavaScript 最強大的特性之一。
// 若一個函式在另一個函式內部定義，即使外部函式已經執行結束，
// 內部函式仍可以存取外部函式的所有變數。
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // 預設情況下，內部函式會被置於局部作用域中，就如同它們是以 `var` 宣告。
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    // setTimeout 是非同步的，所以 sayHelloInFiveSeconds 函式會立即退出。
    // 而 setTimeout 會在之後呼叫 inner 函式。
    // 然而，由於 inner 函式被「閉合包含」（closed over）在 sayHelloInFiveSeconds 函式中，
    // 因此當它最終被呼叫時，仍然可以存取 `prompt` 變數。
}
sayHelloInFiveSeconds("Adam"); // 將在 5 秒後跳出 "Hello, Adam!" 訊息

///////////////////////////////////
// 5. 更多的物件、建構函式、原型

// 物件可以包含函式
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // 回傳 "Hello world!"

// 當物件裡的函式被呼叫時，它們可以使用 `this` 關鍵字來存取所屬物件的其他成員。
myObj = {
    myString: "Hello world!",
    myFunc: function() {
        return this.myString;
    }
};
myObj.myFunc(); // 回傳 "Hello world!"

// `this` 的設定與函式如何被呼叫有關，而非定義的位置。
// 因此，若我們的函式不是在物件的脈絡（context）中被呼叫，就無法正常運作。
var myFunc = myObj.myFunc;
myFunc(); // = undefined

// 反之，一個函式可以被賦值給物件並透過 `this` 獲得對它的存取權限，
// 即使它在定義時並未依附於該物件上。
var myOtherFunc = function() {
    return this.myString.toUpperCase();
};
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // 回傳 "HELLO WORLD!"

// 我們也可以在呼叫函式時使用 `call` 或 `apply` 來指定函式的脈絡。
var anotherFunc = function(s) {
    return this.myString + s;
};
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

// `apply` 函式的用法幾乎一樣，差別在於要用陣列的格式傳遞參數
anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

// 這在處理接受一系列參數時很有用，特別是當您想要傳遞一個陣列時。
Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN（噢！）
Math.min.apply(Math, [42, 6, 27]); // = 6

// 然而 `call` 和 `apply` 只是暫時的。當我們希望它永久有效時，我們可以使用 `bind`。
var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

// `bind` 也可以用於部分應用，例如：柯里化（curry）的函式。
var product = function(a, b) { return a * b; };
var doubler = product.bind(this, 2);
doubler(8); // = 16

// 當您使用 `new` 關鍵字呼叫一個函式時，會建立一個新的物件，
// 並透過 `this` 關鍵字使該物件可用於該函式。用此方式呼叫的函式被稱為「建構函式」。
var MyConstructor = function(){
    this.myNumber = 5;
};
myNewObj = new MyConstructor(); // = { myNumber: 5 }
myNewObj.myNumber; // = 5

// 與大多數其他流行的物件導向語言不同，JavaScript 沒有從類別（class）藍圖建立
// 實例（instance）的概念；相反地，JavaScript 將實例化（instantiation）和
// 繼承（inheritance）結合成單一的概念：「原型」（prototype）。

// 每個 JavaScript 物件都有一個「原型」（prototype）。
// 當你試圖存取一個物件上不存在的屬性時，直譯器（interpreter）會嘗試查找它的原型。

// 某些 JS 實作允許你透過屬性 `__proto__` 存取原型。雖然這對於解釋原型很有幫助，
// 但它不是標準的操作方式；稍後會介紹使用原型的標準方法。
var myObj = {
    myString: "Hello world!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function() {
        return this.myString.toLowerCase();
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// 函式照常運作。
myObj.myFunc(); // = "hello world!"

// 當然，若您的屬性不在原型上，則會搜尋原型的原型，以此類推。
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

// 這裡沒有涉及任何複製；每個物件都儲存了一個指向其原型的參考。
// 這意味著我們可以修改原型，而此修改將反映在任何地方。
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

// for/in 陳述句允許循覽物件的屬性，沿著原型鏈向上查找，直到遇到 `null` 原型為止。
for (var x in myObj){
    console.log(myObj[x]);
}
// 印出：
// Hello world!
// 43
// [Function: myFunc]
// true

// 若只考慮直接附加在物件本身而非其原型上的屬性，請使用 `hasOwnProperty()` 做檢查。
for (var x in myObj){
    if (myObj.hasOwnProperty(x)){
        console.log(myObj[x]);
    }
}
// 印出：
// Hello world!

// 我們提到 `__proto__` 是非標準用法，而且沒有標準的方法來修改現有物件的原型。
// 然而，有兩種方法可以建立具有指定原型的新物件。

// 第一種方法是 `Object.create`。
var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43

// 第二種方法與建構函式有關，可在任何地方使用。建構函式有一個稱為 `prototype` 的屬性。
// 這「不是」建構函式本身的原型；而是當使用該建構函式和 `new` 關鍵字建立新物件時，所賦予的原型。
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

// 內建型別如字串和數字也有建構函式，可以建立等效的包裝物件（wrapper object）。
var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true

// 然而，它們並非嚴格相等。
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // 這段程式碼不會執行，因為 0 是偽值。
}
if (new Number(0)){
   // 這段程式碼將會執行，因為包裝過的數字是物件，而物件皆為真值。
}

// 然而，包裝物件和常見的內建型別共享一個原型，所以你實際上可以為字串擴充功能，例如：
String.prototype.firstCharacter = function(){
    return this.charAt(0);
};
"abc".firstCharacter(); // = "a"

// 此技巧常被用於「填充」（polyfilling），即在舊版的 JavaScript 中
// 實作新版 JavaScript 才有的功能，以便它們可以在較舊的環境（例如過時的瀏覽器）中使用。

// 舉例來說，方才提到的 `Object.create` 並非在舊瀏覽器上使用，
// 但我們仍然可以透過這個 polyfill 來補足其功能：
if (Object.create === undefined){ // 若此方法存在，則不覆蓋
    Object.create = function(proto){
        // 製作一個具有正確原型的暫時建構函式
        var Constructor = function(){};
        Constructor.prototype = proto;
        // 然後使用它來建立一個具有適當原型的新物件
        return new Constructor();
    };
}

// ES6 語法

// `let` 關鍵字讓您在語彙作用域（lexical scope）中定義變數，
// 而不像 `var` 關鍵字那樣在函式作用域（function scope）中定義。
let name = "Billy";

// 以 `let` 關鍵字定義的變數，可以賦予新值。
name = "William";

// `const` 關鍵字如同 `let` 可在語彙作用域中定義變數，差別在於歐，一旦賦值後就不能更改其值。
const pi = 3.14;

pi = 4.13; // 此操作並不合法。

// ES6 中有一種新的函式語法，稱為「lambda 語法」（lambda syntax）。
// 這允許函式在語彙作用域中定義，如同 `const` 和 `let` 來定義變數。
const isEven = (number) => {
    return number % 2 === 0;
};

isEven(7); // 回傳 false 值

// 「等效」於以下傳統函式的宣告方式：
function isEven(number) {
    return number % 2 === 0;
};

// 前面特別強調「等效」一詞，是因為使用 lambda 語法定義的函式不能在定義之前被呼叫。
// 以下爲錯誤示範：
add(1, 8);

const add = (firstNumber, secondNumber) => {
    return firstNumber + secondNumber;
};
```

## 延伸閱讀

[Mozilla 開發者網路（Mozilla Developer Network）][1] 為 JavaScript 詳細的文件，主要針對瀏覽器環境。此外，它是一個維基百科，當你學到更多時，你可以透過分享自己的知識來幫助他人。

MDN 的 [重新介紹 JavaScript][2] 一文中，提到關於本文之外的更多細節。本文聚焦於 JavaScript 語言本身；若您想學習更多關於如何在網頁中使用 JavaScript，可以從閱讀 [文件物件模型][3] 開始。

[JavaScript Garden][5] 是一份深入探討這門語言所有反直覺部分的指南。

[JavaScript 大全][6] 是一本經典的指南和參考書。

[精通 JavaScript][8] 由 Marijn Haverbeke 所著，是一本優秀的 JS 書籍/電子書，附帶終端機指令

[JavaScript: The Right Way][10] 是一本指南，旨在向新入門的開發者介紹 JavaScript，並幫助有經驗的開發者學習更多關於其最佳實踐的知識。

[現代 JavaScript 教學][11] 為現代的 JavaScript 教學網站，涵蓋了基礎知識（核心語言和瀏覽器操作）以及進階主題，並提供簡明扼要的解釋。

除了本文的直接貢獻者外，部分內容改編自本站 Louie Dinh 的 Python 教學，以及 MDN 的 [重新介紹 JavaScript][2]。

[1]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[2]: https://developer.mozilla.org/zh-TW/docs/Web/JavaScript/Language_overview
[3]: https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Using_the_Document_Object_Model
[5]: https://shamansir.github.io/JavaScript-Garden/zhtw/
[6]: https://www.tenlong.com.tw/products/9789865027322
[8]: https://www.tenlong.com.tw/products/9789865029890?list_name=srh
[10]: http://jstherightway.org/
[11]: https://javascriptinfo.dev.org.tw
