---
category: tool
name: AMD
contributors:
    - ["Frederik Ring", "https://github.com/m90"]
filename: learnamd.js
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

## AMD 시작하기

**비동기 모듈 정의(Asynchronous Module Definition)** API는 모듈과 그 의존성을 비동기적으로 로드할 수 있도록 자바스크립트 모듈을 정의하는 메커니즘을 지정합니다. 이는 모듈의 동기적 로딩이 성능, 사용성, 디버깅 및 교차 도메인 접근 문제를 야기하는 브라우저 환경에 특히 적합합니다.

### 기본 개념

```javascript
// 기본 AMD API는 `define`과 `require`라는 두 가지 메서드로만 구성되며
// 모듈 정의와 소비에 관한 것입니다:
// `define(id?, dependencies?, factory)`은 모듈을 정의합니다.
// `require(dependencies, callback)`은 의존성 집합을 가져와
// 전달된 콜백에서 소비합니다.

// 먼저 define을 사용하여 의존성이 없는 새로운 명명된 모듈을 정의해 보겠습니다.
// 이를 위해 define에 이름과 팩토리 함수를 전달합니다:
define('awesomeAMD', function(){
  var isAMDAwesome = function(){
    return true;
  };
  // 모듈의 팩토리 함수 반환 값은
  // 다른 모듈이나 require 호출이 `awesomeAMD` 모듈을
  // 요구할 때 받게 되는 것입니다.
  // 내보낸 값은 무엇이든 될 수 있습니다. (생성자) 함수,
  // 객체, 원시 타입, 심지어 undefined도 가능합니다(별로 도움이 되지는 않지만).
  return isAMDAwesome;
});

// 이제 `awesomeAMD` 모듈에 의존하는 다른 모듈을 정의해 보겠습니다.
// 이제 모듈의 의존성을 정의하는 추가 인수가 있음을 주목하십시오:
define('loudmouth', ['awesomeAMD'], function(awesomeAMD){
  // 의존성은 지정된 순서대로 팩토리의 인수에
  // 전달됩니다.
  var tellEveryone = function(){
    if (awesomeAMD()){
      alert('This is sOoOo rad!');
    } else {
      alert('Pretty dull, isn\'t it?');
    }
  };
  return tellEveryone;
});

// 이제 define 사용법을 알았으니 `require`를 사용하여
// 프로그램을 시작해 보겠습니다. `require`의 시그니처는 `(arrayOfDependencies, callback)`입니다.
require(['loudmouth'], function(loudmouth){
  loudmouth();
});

// 이 튜토리얼에서 코드를 실행하기 위해, 바로 여기서 매우 기본적인
// (비동기적이지 않은) AMD 버전을 구현해 보겠습니다:
function define(name, deps, factory){
  // 의존성이 없는 모듈이 어떻게 처리되는지 주목하십시오.
  define[name] = require(factory ? deps : [], factory || deps);
}

function require(deps, callback){
  var args = [];
  // 먼저 require 호출에 필요한 모든 의존성을
  // 검색해 보겠습니다.
  for (var i = 0; i < deps.length; i++){
    args[i] = define[deps[i]];
  }
  // 콜백의 모든 의존성을 충족시킵니다.
  return callback.apply(null, args);
}
// 이 코드가 작동하는 것을 여기에서 볼 수 있습니다: http://jsfiddle.net/qap949pd/
```

### require.js를 사용한 실제 사용법

소개 예제와 달리 `require.js`(가장 인기 있는 AMD 라이브러리)는 실제로 **AMD**의 **A**를 구현하여 XHR을 통해 모듈과 그 의존성을 비동기적으로 로드할 수 있습니다:

```javascript
/* file: app/main.js */
require(['modules/someClass'], function(SomeClass){
  // 콜백은 의존성이 로드될 때까지 지연됩니다.
  var thing = new SomeClass();
});
console.log('So here we are, waiting!'); // 이것이 먼저 실행됩니다.
```

관례적으로, 보통 하나의 모듈을 하나의 파일에 저장합니다. `require.js`는 파일 경로를 기반으로 모듈 이름을 확인할 수 있으므로 모듈에 이름을 지정할 필요 없이 위치를 사용하여 참조할 수 있습니다. 예제에서 `someClass`는 구성의 `baseUrl`에 상대적인 `modules` 폴더에 있다고 가정합니다:

* app/
  * main.js
  * modules/
    * someClass.js
    * someHelpers.js
    * ...
  * daos/
    * things.js
    * ...

이는 모듈 ID를 지정하지 않고 `someClass`를 정의할 수 있음을 의미합니다:

```javascript
/* file: app/modules/someClass.js */
define(['daos/things', 'modules/someHelpers'], function(thingsDao, helpers){
  // 모듈 정의도 물론 비동기적으로 발생합니다.
  function SomeClass(){
    this.method = function(){/**/};
    // ...
  }
  return SomeClass;
});
```

기본 경로 매핑 동작을 변경하려면 `main.js`에서 `requirejs.config(configObj)`를 사용하십시오:

```javascript
/* file: main.js */
requirejs.config({
  baseUrl : 'app',
  paths : {
    // 다른 위치에서 모듈을 로드할 수도 있습니다.
    jquery : '//ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
});
require(['jquery', 'coolLibFromBower', 'modules/someHelpers'], function($, coolLib, helpers){
  // `main` 파일은 적어도 한 번 require를 호출해야 합니다.
  // 그렇지 않으면 코드가 실행되지 않습니다.
  coolLib.doFancyStuffWith(helpers.transform($('#foo')));
});
```

`require.js` 기반 앱은 일반적으로 `require.js` 스크립트 태그에 데이터 속성으로 전달되는 단일 진입점(`main.js`)을 가집니다. 페이지 로드 시 자동으로 로드되고 실행됩니다:

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

### r.js를 사용하여 전체 프로젝트 최적화

많은 사람들이 개발 중에는 합리적인 코드 구성을 위해 AMD를 사용하지만, 프로덕션에서는 페이지 로드 시 수백 개의 XHR을 수행하는 대신 단일 스크립트 파일을 제공하기를 원합니다.

`require.js`에는 `r.js`라는 스크립트가 함께 제공됩니다(node.js에서 실행할 가능성이 높지만 Rhino도 지원됨). 이 스크립트는 프로젝트의 의존성 그래프를 분석하고, 모든 모듈(적절하게 명명됨), 축소되고 소비 준비가 된 단일 파일을 빌드할 수 있습니다.

`npm`을 사용하여 설치하십시오:

```shell
$ npm install requirejs -g
```

이제 구성 파일을 제공할 수 있습니다:

```shell
$ r.js -o app.build.js
```

위의 예제에 대한 구성은 다음과 같을 수 있습니다:

```javascript
/* file : app.build.js */
({
  name : 'main', // 진입점 이름
  out : 'main-built.js', // 출력을 쓸 파일 이름
  baseUrl : 'app',
  paths : {
    // `empty:`는 r.js에게 이것이 `main.js`에 지정된 위치를 사용하여
    // CDN에서 여전히 로드되어야 함을 알려줍니다.
    jquery : 'empty:',
    coolLibFromBower : '../bower_components/cool-lib/coollib'
  }
})
```

프로덕션에서 빌드된 파일을 사용하려면 `data-main`을 간단히 바꾸십시오:

```html
<script src="require.js" data-main="app/main-built"></script>
```

GitHub 리포지토리에서 빌드 옵션에 대한 매우 상세한 [개요](https://github.com/jrburke/r.js/blob/master/build/example.build.js)를 볼 수 있습니다.

### 이 튜토리얼에서 다루지 않은 주제
* [로더 플러그인 / 변환](http://requirejs.org/docs/plugins.html)
* [CommonJS 스타일 로딩 및 내보내기](http://requirejs.org/docs/commonjs.html)
* [고급 구성](http://requirejs.org/docs/api.html#config)
* [Shim 구성 (비 AMD 모듈 로딩)](http://requirejs.org/docs/api.html#config-shim)
* [require.js를 사용한 CSS 로딩 및 최적화](http://requirejs.org/docs/optimization.html#onecss)
* [빌드에 almond.js 사용](https://github.com/jrburke/almond)

### 더 읽을거리:

* [공식 사양](https://github.com/amdjs/amdjs-api/wiki/AMD)
* [왜 AMD인가?](http://requirejs.org/docs/whyamd.html)
* [범용 모듈 정의](https://github.com/umdjs/umd)

### 구현:

* [require.js](http://requirejs.org)
* [dojo toolkit](http://dojotoolkit.org/documentation/tutorials/1.9/modules/)
* [cujo.js](http://cujojs.com/)
* [curl.js](https://github.com/cujojs/curl)
* [lsjs](https://github.com/zazl/lsjs)
* [mmd](https://github.com/alexlawrence/mmd)
