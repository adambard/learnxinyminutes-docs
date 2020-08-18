---
language: javascript
category: language
contributors:
    - ["Leigh Brenecki", "https://leigh.net.au"]
translators:
    - ["wikibook", "http://wikibook.co.kr"]
filename: javascript-kr.js
lang: ko-kr
---

자바스크립트는 넷스케이프의 브렌던 아이크(Brendan Eich)가 1995년에 만들었습니다. 
원래 자바스크립트는 웹사이트를 위한 단순한 스크립트 언어를 목표로 만들어졌는데, 
좀 더 복잡한 웹 애플리케이션을 만들기 위해 자바를 보완하는 역할이었지만 
웹 페이지와의 긴밀한 상호작용과 브라우저에 대한 지원 기능 덕분에 웹 프론트엔드에서 
자바보다 훨씬 더 보편적으로 쓰이게 됐습니다. 

그렇지만 자바스크립트는 웹 브라우저에만 국한되지 않습니다. 구글 크롬의 V8 자바스크립트 
엔진을 위한 독립형 런타임을 제공하는 Node.js는 점점 인기를 얻고 있습니다.

피드백 주시면 대단히 감사하겠습니다! [@ExcitedLeigh](https://twitter.com/ExcitedLeigh)나 
[l@leigh.net.au](mailto:l@leigh.net.au)를 통해 저와 만나실 수 있습니다.

```js
// 주석은 C와 비슷합니다. 한 줄짜리 주석은 두 개의 슬래시로 시작하고,
/* 여러 줄 주석은 슬래시 별표로 시작해서
   별표 슬래시로 끝납니다. */

// 구문은 세미콜론(;)으로 끝낼 수 있습니다.
doStuff();

// 하지만 꼭 그럴 필요는 없는데, 특정 경우를 제외하고
// 새 줄이 시작할 때마다 세미콜론이 자동으로 삽입되기 때문입니다. 
doStuff()

// 여기서는 세미콜론을 생략하겠습니다. 세미콜론을 생략할지 여부는
// 개인적인 취향이나 프로젝트의 스타일 가이드를 따릅니다.

///////////////////////////////////
// 1. 숫자, 문자열, 연산자

// 자바스크립트에는 단 하나의 숫자 타입(64비트 IEEE 754 배정도 숫자)만이
// 있습니다.
3 // = 3
1.5 // = 1.5

// 모든 기초 산술 연산은 기대한 대로 동작합니다.
1 + 1 // = 2
8 - 1 // = 7
10 * 2 // = 20
35 / 5 // = 7

// 나누어 떨어지지 않는 나눗셈도 포함됩니다.
5 / 2 // = 2.5

// 비트 연산도 지원됩니다. float을 대상으로 비트 연산을 수행하면
// 32비트까지 부호가 있는 int로 변환됩니다.
1 << 2 // = 4

// 괄호를 이용하면 우선순위를 지정할 수 있습니다. 
(1 + 3) * 2 // = 8

// 실제 숫자가 아닌 특별한 세 가지 값이 있습니다.
Infinity // 1/0 1/0과 같은 연산의 결과
-Infinity // -1/0과 같은 연산의 결과
NaN // 0/0과 같은 연산의 결과

// 불린 타입도 있습니다.
true
false

// 문자열은 '나 "로 생성합니다.
'abc'
"Hello, world"

// 부정 연산에는 ! 기호를 이용합니다.
!true // = false
!false // = true

// 동일성 연산은 ==
1 == 1 // = true
2 == 1 // = false

// 불일치 연산은 !=
1 != 1 // = false
2 != 1 // = true

// 그 밖의 비교 연산
1 < 10 // = true
1 > 10 // = false
2 <= 2 // = true
2 >= 2 // = true

// 문자열은 +로 연결할 수 있습니다.
"Hello " + "world!" // = "Hello world!"

// 그리고 <와 >로 비교할 수 있습니다.
"a" < "b" // = true

// 비교 시 타입 강제변환이 수행됩니다. 
"5" == 5 // = true

// ===를 쓰지 않는다면 말이죠.
"5" === 5 // = false

// charAt을 이용하면 문자열 내의 문자에 접근할 수 있습니다.
"This is a string".charAt(0)

// null과 undefined도 있습니다.
null // 의도적으로 값이 아님을 나타내는 데 사용합니다.
undefined // 값이 아직 설정되지 않음을 나타내는 데 사용합니다.

// null, undefinded, NaN, 0, ""은 거짓이며, 그 밖의 다른 모든 값은 참입니다.
// 참고로 0은 거짓이며, "0"은 참입니다(심지어 0 == "0"이더라도).

///////////////////////////////////
// 2. 변수, 배열, 객체

// 변수는 var 키워드로 선언합니다. 자바스크립트는 동적 타입 언어라서
// 타입을 지정할 필요가 없습니다. 값을 할당할 때는 = 문자 하나를 사용합니다.
var someVar = 5

// var 키워드를 지정하지 않아도 오류는 발생하지 않습니다.
someOtherVar = 10

// 그렇지만 변수가 여러분이 정의한 유효범위가 아니라 
// 전역 유효범위에 생성됩니다.

// 값을 할당하지 않은 채로 선언한 변수는 undefined로 설정됩니다. 
var someThirdVar // = undefined

// 변수에 수학 연산을 수행하는 축약형 표현은 다음과 같습니다.
someVar += 5 // someVar = someVar + 5;와 같음. 이제 someVar는 10. 
someVar *= 10 // somVar는 100

// 1을 더하거나 빼는 훨씬 더 짧은 표현도 있습니다.
someVar++ // 이제 someVar는 101
someVar-- // 다시 100으로 되돌아감

// 배열은 순차적인 임의 타입 값의 목록입니다.
var myArray = ["Hello", 45, true]

// 배열의 멤버는 대괄호로 둘러싼 인덱스를 이용해 접근할 수 있습니다.
// 배열의 인덱스는 0부터 시작합니다.
myArray[1] // = 45

// 자바스크립트의 객체는 다른 언어의 '사전'이나 '맵'과 같습니다.
// 즉, 키-값 쌍으로 구성된 비순차 컬렉션입니다.
{key1: "Hello", key2: "World"}

// 키는 문자열이지만 유효한 자바스크립트 식별자일 경우
// 작은따옴표는 필요하지 않습니다. 값은 어떤 타입이든 사용할 수 있습니다.
var myObj = {myKey: "myValue", "my other key": 4}

// 객체 속성에도 인덱스를 이용해 접근할 수 있습니다.
myObj["my other key"] // = 4

// 또는 키가 유효한 식별자일 경우 점 표기법을 이용해 접근할 수 있습니다.
myObj.myKey // = "myValue"

// 객체는 변경 가능합니다. 즉, 값을 변경하거나 새 키를 추가할 수 있습니다.
myObj.myThirdKey = true

// 설정되지 않은 값에 접근하려고 하면 undefined가 반환됩니다.
myObj.myFourthKey // = undefined

///////////////////////////////////
// 3. 로직과 제어 구조

// if 구조는 여러분이 예상한 대로 동작합니다.
var count = 1
if (count == 3){
    // count가 3일 경우 평가됨
} else if (count == 4) {
    // count가 4일 경우 평가됨
} else {
    // count가 3이나 4가 아닌 경우에 평가됨
}

// while도 마찬가지입니다.
while (true) {
    // 무한 루프!
}

// do-while 문은 항상 최소 한 번은 실행된다는 점을 제외하면
// while 문과 비슷합니다.
var input
do {
    input = getInput()
} while (!isValid(input))

// for 문은 C와 자바의 for 문과 같습니다.
// 초기화식; 지속 조건; 증감식
for (var i = 0; i < 5; i++){
    // 5번 실행됨
}

// &&는 논리 and이고 ||는 논리 or입니다.
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear"
}
if (colour == "red" || colour == "blue"){
    // 색은 빨강이거나 파랑
}

// &&와 ||은 "단축 평가"를 수행하는데, 기본값을 설정할 때 유용합니다.
var name = otherName || "default"

///////////////////////////////////
// 4. 함수, 유효범위, 클로저

// 자바스크립트 함수는 function 키워드로 선언합니다.
function myFunction(thing){
    return thing.toUpperCase()
}
myFunction("foo") // = "FOO"

// 함수는 "익명"으로, 즉 이름 없이 정의할 수도 있습니다.
function(thing){
    return thing.toLowerCase()
}
// (함수를 가리키는 이름이 없기 때문에 함수를 호출할 수 없습니다)

// 자바스크립트 함수는 일급 객체이므로 다른 변수에 재할당하고
// 다른 함수에 인자로 전달할 수 있습니다. 가령, 이벤트 핸들러를 만들 경우
function myFunction(){
    // 이 코드는 5초 내에 호출됨
}
setTimeout(myFunction, 5000)

// 다른 함수를 호출할 때 직접적으로 함수 구문을 작성할 수도 있습니다.

setTimeout(function myFunction(){
    // 이 코드는 5초 내에 호출됨
}, 5000)

// 자바스크립트에는 함수 유효범위가 있습니다. 
// 함수는 자체적인 유효범위를 가지지만 다른 블록은 유효범위를 가지지 않습니다.
if (true){
    var i = 5
}
i // = 5 - 블록 유효범위를 지원하는 언어에서는 undefined가 아닙니다.

// 이것은 "즉시 실행되는 익명 함수"라는 공통 패턴으로 이어지는데,
// 이 패턴은 임시 변수가 전역 유효범위로 유출되는 것을 방지합니다.
(function(){
    var temporary = 5
    // '전역 객체'에 할당하는 식으로 전역 유효범위에 접근할 수 있는데,
    // 브라우저에서 전역 객체는 항상 'window'입니다. 전역 객체는 
    // Node.js와 같은 브라우저가 아닌 환경에서는 다른 이름일 수도 있습니다.
    window.permanent = 10
    // 또는 앞에서 언급했다시피 var 키워드를 뺄 수도 있습니다.
    permanent2 = 15
})()
temporary // ReferenceError 발생
permanent // = 10
permanent2 // = 15

// 자바스크립트의 강력한 기능 중 하나는 클로저(closure)입니다.
// 함수가 다른 함수 안에서 정의되면 안쪽에 정의된 함수는 바깥 함수의
// 모든 변수에 접근할 수 있습니다.
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!"
    function inner(){
        alert(prompt)
    }
    setTimeout(inner, 5000)
    // setTimeout은 비동기적으로 동작하므로 이 함수는 5초 동안
    // 기다리지 않고 실행을 마칩니다. 하지만 5초가 지나면 inner에서도
    // prompt의 값에 접근할 수 있습니다.
}
sayHelloInFiveSeconds("Adam") // 5초 내로 "Hello, Adam!"이라고 적힌 팝업이 표시됨

///////////////////////////////////
// 5. 객체 심화; 생성자와 프로토타입

// 객체는 함수를 포함할 수 있습니다.
var myObj = {
    myFunc: function(){
        return "Hello world!"
    }
}
myObj.myFunc() // = "Hello world!"

// 객체에 포함된 함수가 호출되면 함수에서는 this 키워드를 이용해
// 해당 함수가 포함된 객체에 접근할 수 있습니다.
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString
    }
}
myObj.myFunc() // = "Hello world!"

// 여기서 설정한 것은 함수가 정의된 곳이 아닌 함수가 호출되는 
// 방식과 관련이 있습니다. 그래서 아래 함수는 객체 컨텍스트에서 
// 호출되지 않으면 동작하지 않습니다.
var myFunc = myObj.myFunc
myFunc() // = undefined

// 반대로 함수는 객체에 할당하고 this를 통해 해당 객체에 접근할 수 있습니다.
// 함수를 정의할 때 객체에 추가되지 않았더라도 마찬가지입니다.
var myOtherFunc = function(){
    return this.myString.toUpperCase()
}
myObj.myOtherFunc = myOtherFunc
myObj.myOtherFunc() // = "HELLO WORLD!"

// new 키워드로 함수를 호출하면 새로운 객체가 생성되고 this를 통해
// 함수에서 사용할 수 있게 됩니다. 이런 식으로 설계된 함수를 생성자라 합니다.

var MyConstructor = function(){
    this.myNumber = 5
}
myNewObj = new MyConstructor() // = {myNumber: 5}
myNewObj.myNumber // = 5

// 모든 자바스크립트 객체는 'prototype'을 가지고 있습니다. 어떤 객체에 대해
// 실제 객체에는 존재하지 않는 프로퍼티에 접근하면 인터프리터는 프로로타입에서
// 해당 프로퍼티를 찾습니다.

// 일부 자바스크립트 구현체에서는 __proto__라는 마법의 프로퍼티로
// 객체의 프로토타입에 접근하는 것을 허용하기도 합니다. 프로토타입을 
// 설명하기에는 이런 내용도 도움되겠지만 __proto__는 표준에 포함돼 
// 있지 않습니다. 나중에 프로토타입을 사용하는 표준 방법을 살펴보겠습니다.
var myObj = {
    myString: "Hello world!",
}
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
}
myObj.__proto__ = myPrototype
myObj.meaningOfLife // = 42

// 이 방법은 함수에도 통합니다.
myObj.myFunc() // = "hello world!"

// 물론 프로퍼티가 프로토타입에 존재하지 않으면
// 프로토타입의 프로토타입을 찾는 식으로 진행됩니다.
myPrototype.__proto__ = {
    myBoolean: true
}
myObj.myBoolean // = true

// 여기서 복사는 일어나지 않습니다. 각 객체에는 프로토타입에 대한
// 참조가 보관돼 있습니다. 이는 프로토타입을 변경하면 변경사항이 
// 모든 곳에 반영된다는 의미입니다.
myPrototype.meaningOfLife = 43
myObj.meaningOfLife // = 43

// 앞에서 __proto__가 표준에 포함돼 있지 않다고 이야기했는데, 
// 기존 객체의 프로토타입을 변경하는 표준 방법은 없습니다. 
// 하지만 특정 프로토타입을 가지고 새로운 객체를 생성하는 두 가지
// 방법이 있습니다.

// 첫 번째 방법은 Object.create를 이용하는 것인데, 
// Object.create는 최근에 자바스크립트에 추가된 것이라서 아직까지 
// 모든 구현체에서 이용할 수 있는 것은 아닙니다.
var myObj = Object.create(myPrototype)
myObj.meaningOfLife // = 43

// 두 번째 방법은 어디서나 통하는 방법인데, 생성자와 관련이 있습니다.
// 생성자에는 prototype이라는 프로퍼티가 있습니다. 이 프로퍼티는
// 생성자 함수 자체의 프로토타입이 *아니고* 생성자와 new 키워드를 이용해
// 객체가 생성될 때 새로운 객체가 받는 프로토타입입니다.
myConstructor.prototype = {
    getMyNumber: function(){
        return this.myNumber
    }
}
var myNewObj2 = new myConstructor()
myNewObj2.getMyNumber() // = 5

// 문자열과 숫자와 같은 내장 타입에도 동등한 래퍼 객체를
// 생성하는 생성자가 있습니다.
var myNumber = 12
var myNumberObj = new Number(12)
myNumber == myNumberObj // = true

// 하지만 정확히 같지는 않습니다.
typeof myNumber // = 'number'
typeof myNumberObj // = 'object'
myNumber === myNumberObj // = false
if (0){
    // 0은 거짓이라서 이 코드는 실행되지 않습니다.
}

// 하지만 래퍼 객체와 일반 내장 함수는 프로토타입을 공유하기 때문에 
// 가령 문자열에 실제로 기능을 추가할 수 있습니다.
String.prototype.firstCharacter = function(){
    return this.charAt(0)
}
"abc".firstCharacter() // = "a"

// 이러한 사실은 기존 자바스크립트 버전에서 자바스크립트의
// 새로운 기능을 구현하는 "폴리필(polyfilling)"에 자주 이용되므로
// 오래된 버전의 브라우저와 같이 기존 환경에서 사용될 수 있습니다.

// 예를 들어, Object.create가 모든 구현체에서 사용 가능한 것은 아니라고 
// 했지만 아래의 폴리필을 이용해 Object.create를 여전히 사용할 수 있습니다.
if (Object.create === undefined){ // 이미 존재하면 덮어쓰지 않음
    Object.create = function(proto){
        // 올바른 프로토타입을 가지고 임시 생성자를 만듬
        var Constructor = function(){}
        Constructor.prototype = proto
        // 그런 다음 임시 생성자를 이용해 새로운 적절한 프로토타입을
        // 포함한 객체를 생성
        return new Constructor()
    }
}
```

## 기타 참고 자료

[모질라 개발자 네트워크](https://developer.mozilla.org/en-US/docs/Web/JavaScript)에서는 
자바스크립트에 대한 훌륭한 문서를 제공합니다. 더불어 위키 형식이라서 좀 더 많은 사항을 
배우게 되면 여러분만의 지식을 공유함으로써 다른 사람들에게 도움을 줄 수도 있습니다.

MDN의 ['자바스크립트 재입문'](https://developer.mozilla.org/ko/docs/A_re-introduction_to_JavaScript)에서는 
여기서 다룬 개념의 상당수를 더욱 자세히 다루고 있습니다. 이 자료에서는 자바스크립트 언어 자체에 
대해서만 상당히 신중하게 다뤘습니다. 웹 페이지에서 자바스크립트를 사용하는 방법을 배우고 싶다면 
[문서 객체 모델(Document Object Model)](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)에 
관해 배우는 것으로 시작하길 바랍니다.

[자바스크립트 가든](http://bonsaiden.github.io/JavaScript-Garden/)에서는 자바스크립트 언어에서
직관에 어긋나는 모든 부분들을 심도 있게 다룹니다.

더불어 이 글에 직접적으로 기여한 분들로, 내용 중 일부는 이 사이트에 있는 
루이 딘(Louie Dihn)의 파이썬 튜토리얼과 모질라 개발자 네트워크에 있는 
[자바스크립트 튜토리얼](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)을 참고했습니다.
