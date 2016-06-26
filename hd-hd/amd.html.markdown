---
category: tool
tool: amd
contributors:
    - ["Frederik Ring", "https://github.com/m90"]
filename: learnamd-hd.js
lang: hd
---
## एएमडी के साथ प्रारंभ करना

एपीआई को परिभाषित करने के लिए एक तंत्र को निर्दिष्ट ** ** अतुल्यकालिक मॉड्यूल परिभाषा
जावास्क्रिप्ट मॉड्यूल ऐसे मॉड्यूल और इसकी अतुल्यकालिक निर्भरता से भरा हुआ है। यह ब्राउज़र पर्यावरण जहां के लिए विशेष रूप से अच्छी तरह से अनुकूल है, और प्रदर्शन , प्रयोज्य, डीबगिंग, और क्रॉस-डोमेन जैसे मॉड्यूल्स को जल्दी सिंक्रनाइज़ लोडिंग करता hai।

### मूल अवधारणा
```javascript
// बुनियादी एएमडी एपीआई दो तरीकों लेकिन कुछ भी नहीं होते : ` define` और` require`
// और सभी मॉड्यूल परिभाषा और खपत के बारे में है :
// `define` एक मॉड्यूल को परिभाषित करता है
// ` require` निर्भरता का एक सेट का आयात करता है और
// पारित कर दिया कॉलबैक में उन्हें सेवन करती है

// एक नया नाम देकर हम मॉड्यूल को परिभाषित करने का उपयोग करके शुरू करते हैं
// जिसकी कोई निर्भरता है । हम एक नाम से गुजर रहा है ऐसा करेंगे
// और एक कारखाने समारोह को परिभाषित करने के लिए :
define('awesomeAMD', function(){
  var isAMDAwesome = function(){
    return true;
  };
// एक मॉड्यूल के कारखाने समारोह की मान है
  // जब प्राप्त होगा क्या अन्य मॉड्यूल या आवश्यकता कॉल
  // हमारे ` awesomeAMD` मॉड्यूल की आवश्यकता होती है ।
  // निर्यात मूल्य कुछ भी हो सकता है, (निर्माता ) काम करता है,
  // वस्तुओं, पुरातन, (जो कि बहुत ज्यादा मदद नहीं करेगा , हालांकि) भी अपरिभाषित ।
  return isAMDAwesome;
});

// अब, हमारे ` awesomeAMD` मॉड्यूल पर निर्भर करता है कि किसी अन्य मॉड्यूल परिभाषित करते हैं।
// हमारे परिभाषित करने के लिए एक अतिरिक्त तर्क है कि नोटिस
अब // मॉड्यूल की निर्भरता :
define('loudmouth', ['awesomeAMD'], function(awesomeAMD){
// निर्भरता कारखाने के तर्कों को पारित हो जाएगा
  // क्रम में वे निर्दिष्ट कर रहे हैं
  var tellEveryone = function(){
    if (awesomeAMD()){
      alert('This is sOoOo rad!');
    } else {
      alert('Pretty dull, isn\'t it?');
    }
  };
  return tellEveryone;
});

// हम अब परिभाषित का उपयोग करने के लिए कैसे जानते हैं के रूप में, के लिए ` require` का उपयोग करते हैं
// हमारे कार्यक्रम बंद किक । ` require` के हस्ताक्षर है :(arrayOfDependencies, callback)`.
require(['loudmouth'], function(loudmouth){
  loudmouth();
});

// इस ट्यूटोरियल रन कोड बनाने के लिए है, चलो एक बहुत ही बुनियादी लागू करते हैं
// (गैर अतुल्यकालिक ) की मौके पर यहीं एएमडी के संस्करण:
function define(name, deps, factory){
// निर्भरता के बिना मॉड्यूल नियंत्रित किया जाता है कैसे नोटिस
  define[name] = require(factory ? deps : [], factory || deps);
}

function require(deps, callback){
  var args = [];
 // पहले की जरूरत है सभी निर्भरता पुनः प्राप्त करते हैं
  // आवश्यकता कॉल द्वारा
  for (var i = 0; i < deps.length; i++){
    args[i] = define[deps[i]];
  }
// सभी कॉलबैक की निर्भरता को संतुष्ट
  return callback.apply(null, args);
}
// आप यहाँ कार्रवाई में इस कोड को देख सकते हैं: http://jsfiddle.net/qap949pd/
```

### Require.js के साथ वास्तविक दुनिया के उपयोग

परिचयात्मक उदाहरण के विपरीत, ` require.js` (सबसे लोकप्रिय एएमडी पुस्तकालय ) वास्तव में लागू करता है ** ** Amd ** में  *A * **, आप XHR के माध्यम से  मॉड्यूल और उनकी निर्भरता लोड करने के लिए सक्षम करने के लिए :
```javascript
/* file: app/main.js */
require(['modules/someClass'], function(SomeClass){
  // निर्भरता लोड होने तक कॉलबैक टाल दिया गया है
  var thing = new SomeClass();
});
console.log('So here we are, waiting!'); // this will run first
```

परंपरा के अनुसार , आप आमतौर पर एक फाइल में एक मॉड्यूल में ही रखते है । ` require.js` फ़ाइल पथ पर आधारित मॉड्यूल नाम को हल कर सकते हैं , तो आप अपने मॉड्यूल के नाम करने की जरूरत नहीं है , लेकिन बस उनके स्थान का उपयोग कर उन्हें संदर्भित कर सकते हैं । उदाहरण के `में someClass` आपके विन्यास की ` baseUrl` के सापेक्ष ` modules` फ़ोल्डर में माना गया है :

* app/
  * main.js
  * modules/
    * someClass.js
    * someHelpers.js
    * ...
  * daos/
    * things.js
    * ...

इसका मतलब यह है कि हम एक मॉड्यूल आईडी निर्दिष्ट किए बिना ` someClass` परिभाषित कर सकते हैं :

```javascript
/* file: app/modules/someClass.js */
define(['daos/things', 'modules/someHelpers'], function(thingsDao, helpers){
  // module definition, of course, will also happen asynchronously
  function SomeClass(){
    this.method = function(){/**/};
    // ...
  }
  return SomeClass;
});
```
अपने ` main.js` में डिफ़ॉल्ट पथ मानचित्रण व्यवहार का उपयोग ` requirejs.config ( configObj ) ` में परिवर्तन करने के लिए:

```javascript
/* file: main.js */
requirejs.config({
  baseUrl : 'app',
  paths : {
    // आप भी अन्य स्थानों से मॉड्यूल लोड कर सकते हैं
    jquery : '//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
});
require(['jquery', 'coolLibFromBower', 'modules/someHelpers'], function($, coolLib, helpers){
  //एक ` main` फ़ाइल में कम से कम एक बार की आवश्यकता को फोन करने की जरूरत है,
  // अन्यथा कोई कोड कभी चलेंगे
  coolLib.doFancyStuffWith(helpers.transform($('#foo')));
});
```
` require.js` आधारित क्षुधा आमतौर पर एक डाटा विशेषता के रूप में ` require.js` स्क्रिप्ट टैग को पारित कर दिया है कि एक एकल प्रवेश बिंदु (` main.js` ) होगा। यह स्वचालित रूप से भरी हुई है और pageload पर क्रियान्वित किया जाएगा :

```html
<!DOCTYPE html>
<html>
<head>
  <title>A hundred script tags? Never again!</title>
</head>
<body>
  <script src="require.js" data-main="app/main"></script>
</body>
</html>
```

### R.js का उपयोग कर एक पूरी परियोजना का अनुकूलन

कई लोगों को विकास के दौरान समझदार कोड संगठन के लिए एएमडी का उपयोग कर पसंद करते हैं, लेकिन अभी भी पेज लोड पर XHRs के सैकड़ों करने के बजाय उत्पादन में एक भी स्क्रिप्ट फ़ाइल जहाज करने के लिए चाहते हैं।

(राइनो भी समर्थन किया है, तो आप शायद Node.js में चलेगा ) ` require.js` ( अपनी परियोजना की निर्भरता ग्राफ का विश्लेषण , और अपने सभी मॉड्यूल युक्त एक एकल फाइल निर्माण कर सकते हैं कि ` r.js` नामक एक स्क्रिप्ट के साथ आता है ठीक से minified और उपभोग के लिए तैयार है, ) नाम दिया है।
Install it using `npm`:
```shell
$ npm install requirejs -g
```

अब आप एक विन्यास फाइल के साथ फ़ीड कर सकते हैं:
```shell
$ r.js -o app.build.js
```

हमारे ऊपर के उदाहरण के लिए विन्यास की तरह लग सकता है:
```javascript
/* file : app.build.js */
({
  name : 'main', // प्रवेश बिंदु के नाम
  out : 'main-built.js', // फ़ाइल का नाम करने के लिए उत्पादन में लिखने के लिए
  baseUrl : 'app',
  paths : {
    // ` empty :` का उपयोग कर , यह अभी भी समन्वय से लोड किया जाना चाहिए कि r.js बताता है
    // main.js में निर्दिष्ट स्थान
    jquery : 'empty:',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
})
```

उत्पादन में बनाया फ़ाइल का उपयोग करने के लिए, बस ` Data-main` स्वैप:
```html
<script src="require.js" data-main="app/main-built"></script>
```

एक अविश्वसनीय रूप से विस्तृत [निर्माण विकल्पों में से अवलोकन] (https://github.com/jrburke/r.js/blob/master/build/example.build.js) GitHub रेपो में उपलब्ध है।

### विषय इस ट्यूटोरियल में शामिल नहीं
* [लोडर प्लगइन्स / रूपांतरण] (http://requirejs.org/docs/plugins.html)
* [CommonJS शैली लोड हो रहा है और निर्यात] (http://requirejs.org/docs/commonjs.html)
* [उन्नत विन्यास] (http://requirejs.org/docs/api.html#config)
* [शिम विन्यास (गैर एएमडी मॉड्यूल लोडिंग)] (http://requirejs.org/docs/api.html#config-shim)
* [सीएसएस लदान और require.js साथ अनुकूलन] (http://requirejs.org/docs/optimization.html#onecss)
* (Https://github.com/jrburke/almond) [बनाता है के लिए almond.js का प्रयोग]

### अग्रिम पठन:

* [सरकारी कल्पना] (https://github.com/amdjs/amdjs-api/wiki/AMD)
* [क्यों एएमडी?] (Http://requirejs.org/docs/whyamd.html)
* [यूनिवर्सल मॉड्यूल परिभाषा] (https://github.com/umdjs/umd)

### कार्यान्वयन:

* [Require.js] (http://requirejs.org)
* [डोजो टूलकिट] (http://dojotoolkit.org/documentation/tutorials/1.9/modules/)
* [Cujo.js] (http://cujojs.com/)
* [Curl.js] (https://github.com/cujojs/curl)
* [Lsjs] (https://github.com/zazl/lsjs)
* [एमडी] (https://github.com/alexlawrence/mmd)
