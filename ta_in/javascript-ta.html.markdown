---
language: javascript
contributors:
    - ['Adam Brenecki', 'http://adam.brenecki.id.au']
    - ['Ariel Krakowski', 'http://www.learneroo.com']
translators:
  - ["Rasendran Kirushan", "https://github.com/kirushanr"]
filename: javascript-ta.js
lang: in-ta
---

javascript 1995 ஆம்  ஆண்டு Netscape இல் பணிபுரிந்த Brendan Eich
என்பவரால் உருவாக்கபட்டது.ஆரம்பத்தில் மிகவும் எளிமையான
ஸ்க்ரிப்டிங் மொழியாக இணையதளங்களில் பயன்படுத்தபட்டது.
இது ஜாவா (java ) வில் உருவாக்கபட்ட மிகவும் சிக்கலான இணைய செயலிகளுக்கு 
உதவும் முகமாக உருவாக்கபட்டது. எனினும் இணையதள பக்கங்களில் இதன் முழுதான பயன்பாடு 
மற்றும் உலாவிகளில்   பயன்படுத்த கூடிய வகையில் இருந்தமையாலும் Java வை விட 
இணையதளகளின் முகப்பு உருவாக்கத்தில் இன்றளவில் முன்னிலை பெற்றுள்ளது.

உலாவிகளுக்கு மட்டும் மட்டுபடுத்தபடவில்லை , Node.js மூலமாக 
மிகவும் பிரபல்யமடைந்து வருகின்றது , உதாரணமாக கூகிள்க்ரோம் உலாவியின் 
V8 JavaScript engine Node .js உதவியுடன் இயங்குகிறது .

உங்கள் கருத்துக்கள் மிகவும் வரவேற்கபடுகின்றன , என்னுடன் தொடர்புகொள்ள 
[@adambrenecki](https://twitter.com/adambrenecki), or
[adam@brenecki.id.au](mailto:adam@brenecki.id.au).

```js
// குறிப்புக்கள் C நிரலாக்கத்தை ஒத்தது .ஒரு வரி குறிப்புக்கள்  "//" குறியீடுடன் ஆரம்பமாகும் 

/* பலவரி குறிப்புக்கள் "/*" ஆரம்பமாகி "/*" இல் முடிவடையும்  */

// ஒரு கூற்று முற்றுபெற செய்ய ; இடல் வேண்டும் .
doStuff();

// ...ஆனால் அரைபுள்ளி இட வேண்டும் என்று அவசியம் இல்லை ஏன்  எனில் 
// ஒரு வரி புதிதாக இடப்படும் போது அரைபுள்ளிகள் தானாகவே இடப்படும் ஆனால் சில தருணங்களை தவிர .
doStuff()

// ஆனால் அவ்வாறான தருணங்கள் எதிர்பாராத முடிவுகளை தரலாம் 

// எனவே நாம் தொடர்ந்து ஒரு கூற்று நிறைவடையும் போது அரைபுள்ளி ஒன்றை இடுவோம் .

///////////////////////////////////
// 1. எண்கள்(Number) ,சரம் (String),செயற்குறிகள்(Operators) 

// JavaScript ஒரே ஒரு எண்வகை காணப்படுகிறது  தசமி  (which is a 64-bit IEEE 754 double).
// தசமி எண்வகை  (Doubles) 2^ 52 வரை சேமிக்க கூடியது
// முழு எண்வகையின் 9✕10¹⁵ சேமிக்க போதுமானது .
3; // = 3
1.5; // = 1.5

// அடிப்படை கணித பொறிமுறைகள் 
1 + 1; // = 2
0.1 + 0.2; // = 0.30000000000000004
8 - 1; // = 7
10 * 2; // = 20
35 / 5; // = 7

// வகுத்தல்
5 / 2; // = 2.5


//bitwise பொறிமுறையை உபயோகிக்கும் போது 
//உங்கள் தசம எண்ணின் பெறுமானமானது ஒரு நேர் அல்லது மறை அல்லது பூசியமாகவுள்ள முழு எண்ணாக 
//மாற்றம் பெறுகிறது  இது 32 இருமம்(bit) வரை செல்லலாம் 

1 << 2; // = 4

// நிரலாக்கத்தில் செயலியை அமுல்படுத்தும் வரிசைமுறையில் அடைப்பு குறிக்கு முன்னிலை வழங்கபடுகிறது 
(1 + 3) * 2; // = 8

// மெய் எண்  அல்லாத மூன்றுபெறுமானங்கள் உள்ளன :
Infinity; // result of e.g. 1/0
-Infinity; // result of e.g. -1/0
NaN; // result of e.g. 0/0, இது எண்  அல்ல என்பதை  குறிக்கும் 

// தர்க ரீதியில் ஆன கட்டமைப்பு காணப்படுகிறது .
true;
false;

// சரம் (string) ' அல்லது "  குறியீட்டினால்  உருவாக்கபடுகிறது 
'abc';
"Hello, world";

// ஒரு boolean பெறுமானத்தின் எதிர்மறை பெறுமானத்தை பெற ! குறியீடு பயன்படுத்தபடுகிறது 
!true; // = false
!false; // = true

//  சமமா என பார்க்க  ===
1 === 1; // = true
2 === 1; // = false

// சமனற்றவையா  என பார்க்க  !==
1 !== 1; // = false
2 !== 1; // = true

// மேலும் சில ஒப்பீடுகள் 
1 < 10; // = true
1 > 10; // = false
2 <= 2; // = true
2 >= 2; // = true

// இரண்டு சரங்களை(Strings) ஒன்றாக இணைப்பதற்கு +
"Hello " + "world!"; // = "Hello world!"

// இரண்டு மாறிகளை/பெறுமானங்களை  ஒப்பிட  < and >
"a" < "b"; // = true

// இரண்டு பெறுமானங்கள் / மாறிகள்  ஒரேவகையை சேர்ந்தவையா என பார்க்க 
"5" == 5; // = true
null == undefined; // = true

// ...இல்லாவிடின்  ===
"5" === 5; // = false
null === undefined; // = false 

// ...கிழே உள்ள கூற்றுகள் எதிர்பாராத 
வெளியீடுகளை தரலாம் ...
13 + !0; // 14
"13" + !0; // '13true'

// ஒரு சரத்தில்(string ) உள்ள எழுத்தை பெற  `charAt`
"This is a string".charAt(0);  // = 'T'


//... ஒரு சரத்தை(string )  சொற்களாக பிரிக்க (substring) `substring
"Hello world".substring(0, 5); // = "Hello"

// `length` ஒரு சரத்தில்(string) உள்ள சொற்களின் எண்ணிக்கை அல்லது நீளத்தை(length)அறிய 
"Hello".length; // = 5

// `null` மற்றும்  `undefined` இரு பெறுமானங்கள் உள்ளன  .
null;      // மதிப்பு அற்ற ஒரு பெறுமானத்தை குறிக்கும் 
undefined; // பெறுமானம் இன்னும் நிர்ணயிக்க படவில்லை என்பதை குறிக்கும்  (
           // `undefined` இருப்பினும் இதுவும் ஒரு பெறுமானமாக கருதபடுகிறது )

// ஆகியன  தர்க்க ரீதியாக பிழையானவை(false) , மற்றவை யாவும் சரியானவை (true).
// 0 மானது பிழையை (false) குறிக்கும்  "0" சரியை (true) குறிக்கும் எனினும் 0 == "0".

///////////////////////////////////
// 2. மாறிகள்  (Variables),அணிகள் (Arrays) மற்றும் பொருட்கள் (Objects)

// மாறிகளை உருவாக்க `var ` என்னும் குறியீட்டு சொல் (keyword ) பயன்படுகிறது .
//உருவாக்கப்படும் மாறிகள் எந்த வகையை சார்ந்தன என்பதை  JavaScript 
//தானாகவே நிர்ணயிக்கும் . மாறிக்கு  ஒரு பெறுமானத்தை வழங்க  `=` பாவிக்க 
var someVar = 5;

// //நீங்கள் மாறிகளை  நிறுவ  'var' குறியீட்டு சொல்லை பயன்படுத்தா விடினும் 
//அது தவறில்லை ...
someOtherVar = 10;

// ...ஆனால் நீங்கள் நிறுவிய மாறி(variable) எல்லா உங்கள் ப்ரோக்ராம் இன் சகல இடங்களிலும் 
//அணுக கூடியதாய் அமையும் , இல்லாவிடின் அது ஒரு குறிபிட்ட இடத்திற்கு மட்டும் 
//மட்டுபடுத்தபடும் .

//பெறுமானம் வழங்கபடாத மாறிகளுக்கு ,இயல்பாக/தானாக undefined என்ற பெறுமானம் 
//வழங்கப்படும் 
var someThirdVar; // = undefined

// மாறிகளில் கணித செயல்பாடுகளை நடத்த சுருக்கெழுத்து முறைகள் காணப்படுகின்றன :
someVar += 5; // இது  someVar = someVar + 5; ஐ  ஒத்தது someVar இன் பெறுமானம் இப்போது  10
someVar *= 10; //  someVar இன் பெறுமானம் இப்போது  100

//மிகவும் சுருக்கமான சுருகேழுத்து முறை கூட்டல்  அல்லது  கழித்தல் செயன்முறையை 
//மேற்கொள்ள    
someVar++; // someVar இன் பெறுமானம் இப்போது is 101
someVar--; // someVar இன் பெறுமானம் இப்போது 100

// அணிகள்(Arrays) எல்லாவகையான  பெறுமானங்களையும் உள்ளடக்க கூடியது 
var myArray = ["Hello", 45, true];

// அணிகள்(Arrays) உறுப்பினர்கள் சதுர அடைப்புக்குறிக்குள் அதன் தான இலக்கத்தை கொண்டு 
//அணுகமுடியும் .
// அணிகளில் உள்ள உறுப்புகள் 0 இருந்து  ஆரம்பமாகும் .
myArray[1]; // = 45

// அணிகள் உள்ள உறுப்புகளை மாற்றமுடியும்  அத்துடன் உறுப்புகளின் எண்ணிக்கையும் மாறலாம் .
myArray.push("World");
myArray.length; // = 4

// அணியில்(Array)  ஒரு குறிப்பிட்ட இடத்தில உள்ள பெறுமானத்தை மாற்ற .
myArray[3] = "Hello";

// JavaScript's பொருள் (objects) அகராதியை ஒத்தன  
// ஒழுங்கு படுத்த படாத சேகரிப்பு (collection) ஆகும் இதில் ஒரு சாவியும்(key) 
//அதுக்குரிய பெறுமானமும்(value) காணப்படும் .
var myObj = {key1: "Hello", key2: "World"};

// விசைகள் சரங்களை, ஆனால் அவர்கள் சரியான என்றால் மேற்கோள் அவசியம் இல்லை
//சாவிகளை உ.ம் : "key" என நிறுவலாம் ஆனால் , மேற்கோள்  ஆனது சாவி முன்பே நிறுவபட்டிருப்பின்
//அவசியம் இல்லை   
// சாவிகளுக்குரிய பெறுமானங்கள் எந்த வகையாகவும் இருக்கலாம் 
var myObj = {myKey: "myValue", "my other key": 4};

//பொருள் பண்புகளை சதுர அடைப்புக்குறிக்குள் அதன் சாவியின் பெயரை (key) கொண்டு 
//அணுகமுடியும் ,
myObj["my other key"]; // = 4

// ... அல்லது புள்ளி குறியீட்டை பயன்படுத்தி ,சாவியின் (key is a valid identifier)
//பெயர் மூலம் அணுக முடியும் 
myObj.myKey; // = "myValue"

// பொருட்கள்(ஒப்ஜெக்ட்ஸ்) மாற்றபடகூடியான சாவிகளின் பெறுமதிகளை மாற்ற முடியும் அத்துடன் புதிய 
//சாவிகளை(keys) இடவும் முடியும் 
myObj.myThirdKey = true;

//பெறுமதி வரையறுக்கபடாத ஒரு சாவியினை அணுகும் போது 
//அது வெளியிடும் பெறுமதி `undefined`.
myObj.myFourthKey; // = undefined

///////////////////////////////////
// 3. தர்க்கம் மற்றும் கட்டுப்பாட்டு கட்டமைப்பு

// கீழே காட்டப்பட்டுள்ள தொடரியல் ஜாவா வை ஒத்தது 

// The `if` ஒரு குறித்த தர்க்கம் சரியாயின் 
//அல்லது என்ற வடிவமைப்பை 
var count = 1;
if (count == 3){
    // count  இன் பெறுமானம் 3 சமமா என பார்க்கபடுகிறது 
} else if (count == 4){
    // count  இன் பெறுமானம் 4க்கு  சமமா என பார்க்கபடுகிறது 
} else {
    // count ஆனது 3  அல்ல  4 அல்ல  எனின் 
}

// ஒரு குறிப்பிட்ட  ஒப்பீடு உண்மையாக இருக்கும் வரை  `while`.
while (true){
    // இந்த இருக்கும் கூற்றுகள் முடிவிலி தடவை மறுபடி செயற்படுத்தப்படும் !
}

// while போல் அல்லாது do-while ,அவை ஒரு தடவையேனும் அதனுள் உள்ள கூற்றுகள் செயற்படுத்தபடும் 
var input;
do {
    input = getInput();
} while (!isValid(input))

// for (loop /சுற்று ) C , ஜாவாவை ஒத்தது 
//மாறிக்கு பெறுமானத்தை  வழங்கல் , மாறியானது தர்க்கத்தை பூர்த்தி செய்கிறதா என பார்த்தல் ,
//சுற்றுக்குள் இருக்கும் கூற்றை செயற்படுதல்  

for (var i = 0; i < 5; i++){
    // இந்த சுற்று 5 தடவைகள் தொடர்ந்து செயற்படுத்தபடும் 
}

//for /In  சுற்றுகள் prototype சங்கிலியில் உள்ள சகல காரணிகள் ஊடகவும் செல்லும் 
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18}; 
for (var x in person){
    description += person[x] + " ";
}

//ஒரு பொருளில் (Object) இடப்பட்ட பண்புகளை (properties) கருத்தில் கொள்ளும் போது
//குறிப்பிட்ட பண்புகளை அந்த Object கொண்டுள்ளதா என பார்க்க  
var description = "";
var person = {fname:"Paul", lname:"Ken", age:18}; 
for (var x in person){
    if (person.hasOwnProperty(x)){
        description += person[x] + " ";
    }
}

//for /in  ஆனது அணியில் உள்ள பண்புகள் ஒழுங்குபடுத்தப்பட்டவிதம் முக்கியம் 
//ஆயின் பாவிப்பதை தவிர்க்கவும் ஏனெனில் அது சரியான ஒழுங்கில் 
//வெளியீட்டை தரும் என்பது ஐயம் ஆகும் 

// && is logical and, || is logical or
if (house.size == "big" && house.colour == "blue"){
    house.contains = "bear";
}
if (colour == "red" || colour == "blue"){
    // colour is either red or blue
}

// && and || "short circuit", which is useful for setting default values.
var name = otherName || "default";



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
// 4. Functions, Scope and Closures

// JavaScript இல் functions நிறுவ  `function` keyword.பயன்படும் 
function myFunction(thing){
    return thing.toUpperCase();
}
myFunction("foo"); // = "FOO"

//ஒரு பெறுமானத்தை return செய்ய வேண்டும் எனின் இரண்டும் ஒரே வரியில் 
//இருக்க வேண்டும் இல்லாவிடின் return ஆனது `undefined ` return செய்யும் 
//காற் புள்ளி  தானாகவே இடப்படும் , நீங்கள்  Allman style உபயோகிக்கும் போது 
//அவதானமாக இருக்கவும் 
function myFunction()
{
    return // <- semicolon automatically inserted here
    {
        thisIsAn: 'object literal'
    }
}
myFunction(); // = undefined

// JavaScript functions ஆனது   first class objects ஆகும் ,எனவே அவற்றை மாறிகளுக்கு 
//assign செய்ய முடியும் அதுமட்டும் அல்லது functions களில் arguments ஆக அனுப்பமுடியும் 
// உதாரணமாக ஒரு event handler:
function myFunction(){
    //இந்த code 5 செக்கன்களில்  செயற்படுத்தப்படும் 
}
setTimeout(myFunction, 5000);
// Note: setTimeout ஆனது  ஜாவஸ்க்ரிப்ட்  சேர்ந்தது அன்று , ஆனால் அந்த வசதி 
//உலாவிகளிலும் ,Node .js  காணப்படுகிறது 

// Function objects கட்டாயம் பெயரிடப்பட வீண்டும் என்று அவசியம் இல்லை 
// அவை  anonymous(பெயரிடப்படாமல்) உருவாக்கபடலாம் 
setTimeout(function(){
        //இந்த code 5 செக்கன்களில்  செயற்படுத்தப்படும் 
}, 5000);

// JavaScript  function ஒரு குறிப்பிட்ட  scope(எல்லை) கொண்டுள்ளது ;
//functions தமக்கென  ஒரு scope கொண்டுள்ளன .

if (true){
    var i = 5;
}
i; // = 5 - //இது undefined அல்ல 

// இதன் காரணமாக anonymous functions உடனடியாக செயற்படுத்தபடுகின்றன 
//இதன் மூலம் தற்காலிக மாறிகள்(variable) குளோபல் scope
//இற்கு மாறுவதை தவிர்க்கலாம் .
(function(){
    var temporary = 5;
	//நாங்கள் ஒரு மாறியை எங்கிருந்தும் அணுக (access) அதை "global object"
	//ஒன்றுக்கு வழங்க வேண்டும் உலாவியில் அது எப்போதும் `window` ஆகும் .
	//உலாவி  அல்லாத சூழலில் (Node.js) வேறு பெயருடன் இருக்கும் 
    window.permanent = 10;
})();
temporary; // raises ReferenceError
permanent; // = 10

//JavaScript's மிகவும் சக்தி வாய்ந்த ஒரு வசதி closures ஆகும் 
//ஒரு function இன்னொரு function உள் உருவாக்கபடின்
//அது உருவாகப்பட்ட function  இன் மாறிகளை அணுக முடியும்  
function sayHelloInFiveSeconds(name){
    var prompt = "Hello, " + name + "!";
    // Inner functions ஆனது local scope இல்  காணப்படும் 
	//அது `var ` என்ற குறியீட்டு சொல்லால் நிறுவப்படும் 
    function inner(){
        alert(prompt);
    }
    setTimeout(inner, 5000);
    //setTimeout  ஆனது background இல் இயங்கும்  , எனவே sayHelloInFiveSeconds function,
	//செயற்பாடு முடிவடைய ,setTimeout ஆனது inner function call செய்யும்.

}
sayHelloInFiveSeconds("Adam"); // //இது ஒரு popup  ஐ ஐந்து செக்கன்களில் காட்டும் 

///////////////////////////////////
// 5.  Objects; Constructors and Prototypes  பற்றி மேலும் 

// Objects  functions ஐ கொண்டிருக்கலாம் 
var myObj = {
    myFunc: function(){
        return "Hello world!";
    }
};
myObj.myFunc(); // = "Hello world!"

//functions ஆனது objects உடன் இணைக்கப்பட்டுள போது அவை object ஐ அணுக முடியும் 
//அவை this என்ற  குறியீட்டு  சொல்லை பயன்படுத்தி இணைக்கபடுகின்றன 
myObj = {
    myString: "Hello world!",
    myFunc: function(){
        return this.myString;
    }
};
myObj.myFunc(); // = "Hello world!"

//எங்கள் function ஆனது தொழிற் படாமல் போகலாம் அது context(அமைப்பு ) of the object call செய்யபடவிடின் 
var myFunc = myObj.myFunc;
myFunc(); // = undefined


//function ஆனது ஒரு object உக்கு assign செய்யலாம் பிறகு அதை நாம் அணுகமுடியும் 
//`this` மூலம்  
var myOtherFunc = function(){
    return this.myString.toUpperCase();
}
myObj.myOtherFunc = myOtherFunc;
myObj.myOtherFunc(); // = "HELLO WORLD!"

//ஒரு function ஒரு அமைப்பை நாம் உருவாக்க முடியும் 
//அதை நாம் `call` அல்லது  `apply` மூலம் செயல்படுத்த முடியும்

var anotherFunc = function(s){
    return this.myString + s;
}
anotherFunc.call(myObj, " And Hello Moon!"); // = "Hello World! And Hello Moon!"

//apply செயற்பாட்டளவில் ஒத்தன ,ஆனால் அது array (அணி) argument 
//ஆக எடுக்கிறது.

anotherFunc.apply(myObj, [" And Hello Sun!"]); // = "Hello World! And Hello Sun!"

//இது தொடர்ச்சியான arguments ஐ நாம் function ஒன்றுக்குள் pass பண்ண 
//வேண்டும் எனில் மிகவும் உபயோகமானது 

Math.min(42, 6, 27); // = 6
Math.min([42, 6, 27]); // = NaN (uh-oh!)
Math.min.apply(Math, [42, 6, 27]); // = 6

//ஆனால் `call ` ,`apply ` இரண்டும் தற்காலிகமானவை 
//அவற்றை நிரந்தரமாக்க bind function ஐ பயன்படுத்தவும் 

var boundFunc = anotherFunc.bind(myObj);
boundFunc(" And Hello Saturn!"); // = "Hello World! And Hello Saturn!"

//`bind ` ஐ உபயோகித்து ஒரு function ஐ பகுதியாக apply செய்ய முடியும் 

var product = function(a, b){ return a * b; }
var doubler = product.bind(this, 2);
doubler(8); // = 16


//ஒரு function ஐ நாம் new என்ற குறியீட்டு சொல்லை பயன்படுத்தி 
//அழைக்கும் போது  புதிய object உருவாக்கப்படும் .இவ்வாறான  functions 
//constructors என அழைக்கப்படும் 

var MyConstructor = function(){
    this.myNumber = 5;
}
myNewObj = new MyConstructor(); // = {myNumber: 5}
myNewObj.myNumber; // = 5

//ஒவ்வொரு JavaScript object உம் ஒரு `prototype ` கொண்டுள்ளது 
//நீங்கள் object ஒன்றின் ஒரு property ஐ அணுகும் போது 
//அந்த property இல்லாவிடின்  interpreter  ஆனது 
//அதன் prototype உள்ளதா என பார்க்கும் 

//JS இன் சில செயலாக்கங்கள் ஒரு object இன் protoype ஐ 
//இலகுவாக `__proto__` மூலம் access செய்ய முடியும் .
//இது prototype பாவணை யை இலகுவாக்கினாலும் 
//இது சரியான ஒரு முறை அல்ல 
var myObj = {
    myString: "Hello world!"
};
var myPrototype = {
    meaningOfLife: 42,
    myFunc: function(){
        return this.myString.toLowerCase()
    }
};

myObj.__proto__ = myPrototype;
myObj.meaningOfLife; // = 42

// This works for functions, too.
myObj.myFunc(); // = "hello world!"

//உங்கள் property prototype இல் இல்லது இருப்பின் , protype இன் 
//prototype search செய்யப்படும் 
myPrototype.__proto__ = {
    myBoolean: true
};
myObj.myBoolean; // = true

//ஒவ்வொரு object உம்  அதன் protype க்கும்  reference (மேற்கோள் ) ஒன்றை வைத்திருக்கும்  
//நாம் ஒரு protype இணை மாற்றினால் அதன் மாற்றங்கள் எல்லா இடத்திலும் (program இல் )
//பிரதிபலிக்கும் 
myPrototype.meaningOfLife = 43;
myObj.meaningOfLife; // = 43

 
//நாம் முன்பு கூறியது போல் `__proto__` பயன்படுத்துவது சரியான முறை அல்ல 
//எனவே நாம் ஒரு protype ஐ object இல் உருவாக்க  இரண்டு வழிமுறைகள்
//உள்ளன

// முதல் முறை  Object.create இது அண்மையில் அறிமுகம் செய்ய பட்ட ஒன்று
//எனவே சில இடங்களில் இந்த முறை இன்னும் அறிமுகம் ஆகவில்லை

var myObj = Object.create(myPrototype);
myObj.meaningOfLife; // = 43


// இரண்டாவது முறை , இது சகல இடங்களிலும் வேலைசெய்யும், இது constructors மூலம்.
//constructors prototype என்னும் ஒரு காரணியை  கொண்டுள்ளது , இது  constructor function
//இன் prototype அன்று. ,இது நாம் new என்ற குறியீட்டு சொல்லையும்  அந்த constructor உபயோகித்து
//உருவாக்கபடுகிறது

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

// Built-in types like strings and numbers also have constructors that create
// equivalent wrapper objects.
// JavaScript இல் உள்ள strings மற்றும் numbers வகைகளும் constructors கொண்டுள்ளன
//இவை wrapper objects ஐ ஒத்தன

var myNumber = 12;
var myNumberObj = new Number(12);
myNumber == myNumberObj; // = true


//இவை மிக சிறிய அளவில் ஒத்தவை
typeof myNumber; // = 'number'
typeof myNumberObj; // = 'object'
myNumber === myNumberObj; // = false
if (0){
    // இந்த கூற்றானது செயல்படுத்தபடாது ஏனெனில் ௦ false ஆகும்
}

// However, the wrapper objects and the regular builtins share a prototype, so
// you can actually add functionality to a string, for instance.

//இருப்பினும்  wrapper objects மற்றும் regular builtins ஆகியன  prototype ஒன்றை கொண்டுள்ளன
String.prototype.firstCharacter = function(){
    return this.charAt(0);
}
"abc".firstCharacter(); // = "a"

// This fact is often used in "polyfilling", which is implementing newer
// features of JavaScript in an older subset of JavaScript, so that they can be
// used in older environments such as outdated browsers.

//இந்த முறையானது  "polyfilling" இல் உபயோகபடுத்தபடுகிறது.
//புதிய சில  வசதிகளை JavaScript பழைய JavaScript பிரதிகளில் இல்  உருவாக்குகிறது.
//இது பழைய சூழல்களில் உபயோகிகப்படும்.


//நாங்கள் முன்பு கூறி இருந்தோம்  Object.create சில இடங்களில் இந்த முறை இன்னும் 
//அறிமுகம் ஆகவில்லை என்று ஆனால் இதை polyfill ஐ  பயன்படுத்தி உருவாக்க
//முடியும்

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

## மேலும் JavaScript பற்றி கற்க

The [Mozilla Developer
Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript) provides
excellent documentation for JavaScript as it's used in browsers. Plus, it's a
wiki, so as you learn more you can help others out by sharing your own
knowledge.

MDN's [A re-introduction to
JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
covers much of the concepts covered here in more detail. This guide has quite
deliberately only covered the JavaScript language itself; if you want to learn
more about how to use JavaScript in web pages, start by learning about the
[Document Object
Model](https://developer.mozilla.org/en-US/docs/Using_the_W3C_DOM_Level_1_Core)

[Learn Javascript by Example and with Challenges](http://www.learneroo.com/modules/64/nodes/350) is a variant of this reference with built-in challenges.

[JavaScript Garden](http://bonsaiden.github.io/JavaScript-Garden/) is an in-depth
guide of all the counter-intuitive parts of the language.

[JavaScript: The Definitive Guide](http://www.amazon.com/gp/product/0596805527/) is a classic guide / reference book.

In addition to direct contributors to this article, some content is adapted
from Louie Dinh's Python tutorial on this site, and the [JS
Tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
on the Mozilla Developer Network.
