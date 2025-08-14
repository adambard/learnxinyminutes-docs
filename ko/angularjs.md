---
category: framework
name: AngularJS
contributors:
    - ["Walter Cordero", "http://waltercordero.com"]
filename: learnangular.txt
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

## AngularJS 튜토리얼.

AngularJS 버전 1.0은 2012년에 출시되었습니다.
Google 직원인 Miško Hevery는 2009년에 AngularJS 작업을 시작했습니다.
이 아이디어는 매우 성공적이었고, 현재 이 프로젝트는 Google에서 공식적으로 지원합니다.

AngularJS는 자바스크립트 프레임워크입니다. "script" 태그를 사용하여 HTML 페이지에 추가할 수 있습니다.
AngularJS는 지시문(Directives)으로 HTML 속성을 확장하고, 표현식(Expressions)으로 데이터를 HTML에 바인딩합니다.

## 이미 알고 있어야 할 사항

AngularJS를 공부하기 전에 다음에 대한 기본적인 이해가 있어야 합니다:

- HTML
- CSS
- 자바스크립트

```html
// AngularJS는 자바스크립트 프레임워크입니다. 자바스크립트로 작성된 라이브러리입니다.
// AngularJS는 자바스크립트 파일로 배포되며, 스크립트 태그를 사용하여 웹 페이지에 추가할 수 있습니다:
// <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>

///////////////////////////////////
// AngularJS는 HTML을 확장합니다

//AngularJS는 ng-지시문으로 HTML을 확장합니다.
//ng-app 지시문은 AngularJS 애플리케이션을 정의합니다.
//ng-model 지시문은 HTML 컨트롤(input, select, textarea)의 값을 애플리케이션 데이터에 바인딩합니다.
//ng-bind 지시문은 애플리케이션 데이터를 HTML 뷰에 바인딩합니다.
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>이름: <input type="text" ng-model="name"></p>
      <p ng-bind="name"></p>
    </div>
  </body>
</html>

/*
  * 예제 설명:
  * 웹 페이지가 로드되면 AngularJS가 자동으로 시작됩니다.
  * ng-app 지시문은 <div> 요소가 AngularJS 애플리케이션의 "소유자"임을 AngularJS에 알립니다.
  * ng-model 지시문은 입력 필드의 값을 애플리케이션 변수 이름에 바인딩합니다.
  * ng-bind 지시문은 <p> 요소의 innerHTML을 애플리케이션 변수 이름에 바인딩합니다.
*/
<tag> 여기에 해석될 내용이 있습니다 </tag>

///////////////////////////////////
// AngularJS 표현식

// AngularJS 표현식은 이중 중괄호 안에 작성됩니다: {{ expression }}.
// AngularJS 표현식은 ng-bind 지시문과 동일한 방식으로 데이터를 HTML에 바인딩합니다.
// AngularJS는 표현식이 작성된 곳에 정확히 데이터를 "출력"합니다.
// AngularJS 표현식은 자바스크립트 표현식과 매우 유사합니다: 리터럴, 연산자 및 변수를 포함할 수 있습니다.
// 예제 {{ 5 + 5 }} 또는 {{ firstName + " " + lastName }}
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>나의 첫 표현식: {{ 5 + 5 }}</p>
    </div>
  </body>
</html>

//ng-app 지시문을 제거하면 HTML은 표현식을 해결하지 않고 그대로 표시합니다:
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div>
      <p>나의 첫 표현식: {{ 5 + 5 }}</p>
    </div>
  </body>
</html>

// AngularJS 표현식은 ng-bind 지시문과 동일한 방식으로 AngularJS 데이터를 HTML에 바인딩합니다.
<!DOCTYPE html>
<html>
<script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>이름: <input type="text" ng-model="name"></p>
      <p>{{name}}</p>
    </div>
  </body>
</html>

// AngularJS 숫자는 자바스크립트 숫자와 같습니다:
<div ng-app="" ng-init="quantity=1;cost=5">
  <p>달러 총액: {{ quantity * cost }}</p>
</div>

//AngularJS 문자열은 자바스크립트 문자열과 같습니다:
<div ng-app="" ng-init="firstName='John';lastName='Doe'">
  <p>이름은 <span ng-bind="firstName + ' ' + lastName"></span></p>
</div>

//AngularJS 객체는 자바스크립트 객체와 같습니다:
<div ng-app="" ng-init="person={firstName:'John',lastName:'Doe'}">
  <p>이름은 {{ person.lastName }}</p>
</div>

//AngularJS 배열은 자바스크립트 배열과 같습니다:
<div ng-app="" ng-init="points=[1,15,19,2,40]">
  <p>세 번째 결과는 {{ points[2] }}</p>
</div>

// 자바스크립트 표현식과 마찬가지로 AngularJS 표현식은 리터럴, 연산자 및 변수를 포함할 수 있습니다.
// 자바스크립트 표현식과 달리 AngularJS 표현식은 HTML 내부에 작성할 수 있습니다.
// AngularJS 표현식은 조건문, 루프 및 예외를 지원하지 않지만 자바스크립트 표현식은 지원합니다.
// AngularJS 표현식은 필터를 지원하지만 자바스크립트 표현식은 지원하지 않습니다.

///////////////////////////////////
// AngularJS 지시문


//AngularJS 지시문은 ng- 접두사가 붙은 확장된 HTML 속성입니다.
//ng-app 지시문은 AngularJS 애플리케이션을 초기화합니다.
//ng-init 지시문은 애플리케이션 데이터를 초기화합니다.
//ng-model 지시문은 HTML 컨트롤(input, select, textarea)의 값을 애플리케이션 데이터에 바인딩합니다.
<div ng-app="" ng-init="firstName='John'">
  <p>이름: <input type="text" ng-model="firstName"></p>
  <p>작성한 내용: {{ firstName }}</p>
</div>

//ng-init 사용은 그다지 일반적이지 않습니다. 컨트롤러에 대한 장에서 데이터를 초기화하는 방법을 배우게 됩니다.

//ng-repeat 지시문은 HTML 요소를 반복합니다:
<div ng-app="" ng-init="names=['Jani','Hege','Kai']">
  <ul>
    <li ng-repeat="x in names">
      {{ x }}
    </li>
  </ul>
</div>

//객체 배열에 사용된 ng-repeat 지시문:
<div ng-app="" ng-init="names=[
{name:'Jani',country:'Norway'},
{name:'Hege',country:'Sweden'},
{name:'Kai',country:'Denmark'}]">
  <ul>
    <li ng-repeat="x  in names">
      {{ x.name + ', ' + x.country }}
    </li>
  </ul>
</div>

// AngularJS는 데이터베이스 CRUD(생성 읽기 업데이트 삭제) 애플리케이션에 적합합니다.
// 이러한 객체가 데이터베이스의 레코드라고 상상해 보십시오.

// ng-app 지시문은 AngularJS 애플리케이션의 루트 요소를 정의합니다.
// ng-app 지시문은 웹 페이지가 로드될 때 애플리케이션을 자동 부트스트랩(자동으로 초기화)합니다.
// 나중에 ng-app이 코드 모듈을 연결하기 위해 값(예: ng-app="myModule")을 가질 수 있는 방법을 배우게 됩니다.

// ng-init 지시문은 AngularJS 애플리케이션의 초기 값을 정의합니다.
// 일반적으로 ng-init을 사용하지 않습니다. 대신 컨트롤러나 모듈을 사용합니다.
// 나중에 컨트롤러와 모듈에 대해 더 자세히 배우게 됩니다.

//ng-model 지시문은 HTML 컨트롤(input, select, textarea)의 값을 애플리케이션 데이터에 바인딩합니다.
//ng-model 지시문은 다음을 수행할 수도 있습니다:
//애플리케이션 데이터에 대한 유형 유효성 검사 제공(숫자, 이메일, 필수).
//애플리케이션 데이터에 대한 상태 제공(유효하지 않음, 더티, 터치됨, 오류).
//HTML 요소에 대한 CSS 클래스 제공.
//HTML 요소를 HTML 양식에 바인딩.

//ng-repeat 지시문은 컬렉션(배열)의 각 항목에 대해 한 번씩 HTML 요소를 복제합니다.

///////////////////////////////////
// AngularJS 컨트롤러

// AngularJS 컨트롤러는 AngularJS 애플리케이션의 데이터를 제어합니다.
// AngularJS 컨트롤러는 일반 자바스크립트 객체입니다.

// AngularJS 애플리케이션은 컨트롤러에 의해 제어됩니다.
// ng-controller 지시문은 애플리케이션 컨트롤러를 정의합니다.
// 컨트롤러는 표준 자바스크립트 객체 생성자로 생성된 자바스크립트 객체입니다.

<div ng-app="myApp" ng-controller="myCtrl">

이름: <input type="text" ng-model="firstName"><br>
성: <input type="text" ng-model="lastName"><br>
<br>
전체 이름: {{firstName + " " + lastName}}

</div>

<script>
var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope) {
    $scope.firstName = "John";
    $scope.lastName = "Doe";
});
</script>

//애플리케이션 설명:

//AngularJS 애플리케이션은 ng-app="myApp"으로 정의됩니다. 애플리케이션은 <div> 내에서 실행됩니다.
//ng-controller="myCtrl" 속성은 AngularJS 지시문입니다. 컨트롤러를 정의합니다.
//myCtrl 함수는 자바스크립트 함수입니다.
//AngularJS는 $scope 객체로 컨트롤러를 호출합니다.
//AngularJS에서 $scope는 애플리케이션 객체(애플리케이션 변수 및 함수의 소유자)입니다.
//컨트롤러는 스코프에 두 개의 속성(변수)(firstName 및 lastName)을 생성합니다.
//ng-model 지시문은 입력 필드를 컨트롤러 속성(firstName 및 lastName)에 바인딩합니다.

//위의 예는 두 개의 속성(lastName 및 firstName)을 가진 컨트롤러 객체를 보여주었습니다.
//컨트롤러는 메서드(함수로서의 변수)를 가질 수도 있습니다:
<div ng-app="myApp" ng-controller="personCtrl">

이름: <input type="text" ng-model="firstName"><br>
성: <input type="text" ng-model="lastName"><br>
<br>
전체 이름: {{fullName()}}

</div>

<script>
var app = angular.module('myApp', []);
app.controller('personCtrl', function($scope) {
    $scope.firstName = "John";
    $scope.lastName = "Doe";
    $scope.fullName = function() {
        return $scope.firstName + " " + $scope.lastName;
    }
});
</script>

//더 큰 애플리케이션에서는 컨트롤러를 외부 파일에 저장하는 것이 일반적입니다.
//<script> </script> 태그 사이의 코드를 personController.js라는 외부 파일에 복사하기만 하면 됩니다:

<div ng-app="myApp" ng-controller="personCtrl">

이름: <input type="text" ng-model="firstName"><br>
성: <input type="text" ng-model="lastName"><br>
<br>
전체 이름: {{firstName + " " + lastName}}

</div>

<script src="personController.js"></script>

// 다음 예제를 위해 새 컨트롤러 파일을 만들겠습니다:
angular.module('myApp', []).controller('namesCtrl', function($scope) {
    $scope.names = [
        {name:'Jani',country:'Norway'},
        {name:'Hege',country:'Sweden'},
        {name:'Kai',country:'Denmark'}
    ];
});

//파일을 namesController.js로 저장합니다:
//그런 다음 애플리케이션에서 컨트롤러 파일을 사용합니다:

<div ng-app="myApp" ng-controller="namesCtrl">

<ul>
  <li ng-repeat="x in names">
    {{ x.name + ', ' + x.country }}
  </li>
</ul>

</div>

<script src="namesController.js"></script>

///////////////////////////////////
// AngularJS 필터

// 필터는 파이프 문자를 사용하여 표현식 및 지시문에 추가할 수 있습니다.
// AngularJS 필터는 데이터를 변환하는 데 사용할 수 있습니다:

- **currency**:  숫자를 통화 형식으로 지정합니다.
- **filter**:  배열에서 항목의 하위 집합을 선택합니다.
- **lowercase**: 문자열을 소문자로 지정합니다.
- **orderBy**: 표현식으로 배열을 정렬합니다.
- **uppercase**: 문자열을 대문자로 지정합니다.

//필터는 파이프 문자(|)와 필터를 사용하여 표현식에 추가할 수 있습니다.
//(다음 두 예제에서는 이전 장의 person 컨트롤러를 사용합니다)
//대문자 필터는 문자열을 대문자로 지정합니다:
<div ng-app="myApp" ng-controller="personCtrl">

<p>이름은 {{ lastName | uppercase }}</p>

</div>

//소문자 필터는 문자열을 소문자로 지정합니다:
<div ng-app="myApp" ng-controller="personCtrl">

<p>이름은 {{ lastName | lowercase }}</p>

</div>

//통화 필터는 숫자를 통화로 지정합니다:
<div ng-app="myApp" ng-controller="costCtrl">

<input type="number" ng-model="quantity">
<input type="number" ng-model="price">

<p>총액 = {{ (quantity * price) | currency }}</p>

</div>

//필터는 파이프 문자(|)와 필터를 사용하여 지시문에 추가할 수 있습니다.
//orderBy 필터는 표현식으로 배열을 정렬합니다:
<div ng-app="myApp" ng-controller="namesCtrl">

<ul>
  <li ng-repeat="x in names | orderBy:'country'">
    {{ x.name + ', ' + x.country }}
  </li>
</ul>

<div>

//입력 필터는 파이프 문자(|)와 필터, 콜론 및 모델 이름을 사용하여 지시문에 추가할 수 있습니다.
//필터는 배열의 하위 집합을 선택합니다:

<div ng-app="myApp" ng-controller="namesCtrl">

<p><input type="text" ng-model="test"></p>

<ul>
  <li ng-repeat="x in names | filter:test | orderBy:'country'">
    {{ (x.name | uppercase) + ', ' + x.country }}
  </li>
</ul>

</div>

///////////////////////////////////
// AngularJS AJAX - $http

//$http는 원격 서버에서 데이터를 읽기 위한 AngularJS 서비스입니다.

// 다음 데이터는 웹 서버에서 제공할 수 있습니다:
// http://www.w3schools.com/angular/customers.php
// **데이터 형식을 보려면 URL을 확인하십시오**

// AngularJS $http는 웹 서버에서 데이터를 읽기 위한 핵심 서비스입니다.
// $http.get(url)은 서버 데이터를 읽는 데 사용할 함수입니다.
<div ng-app="myApp" ng-controller="customersCtrl">

<ul>
  <li ng-repeat="x in names">
    {{ x.Name + ', ' + x.Country }}
  </li>
</ul>

</div>

<script>
var app = angular.module('myApp', []);
app.controller('customersCtrl', function($scope, $http) {
    $http.get("http://www.w3schools.com/angular/customers.php")
    .success(function(response) {$scope.names = response.records;});
});
</script>

애플리케이션 설명:

// AngularJS 애플리케이션은 ng-app으로 정의됩니다. 애플리케이션은 <div> 내에서 실행됩니다.
// ng-controller 지시문은 컨트롤러 객체의 이름을 지정합니다.
// customersCtrl 함수는 표준 자바스크립트 객체 생성자입니다.
// AngularJS는 $scope 및 $http 객체로 customersCtrl을 호출합니다.
// $scope는 애플리케이션 객체(애플리케이션 변수 및 함수의 소유자)입니다.
// $http는 외부 데이터를 요청하기 위한 XMLHttpRequest 객체입니다.
// $http.get()은 http://www.w3schools.com/angular/customers.php에서 JSON 데이터를 읽습니다.
// 성공하면 컨트롤러는 서버의 JSON 데이터로 스코프에 속성(이름)을 생성합니다.


// 다른 서버(요청 페이지와 다른)의 데이터 요청은 교차 사이트 HTTP 요청이라고 합니다.
// 교차 사이트 요청은 웹에서 일반적입니다. 많은 페이지가 다른 서버에서 CSS, 이미지 및 스크립트를 로드합니다.
// 최신 브라우저에서는 스크립트의 교차 사이트 HTTP 요청이 보안상의 이유로 동일한 사이트로 제한됩니다.
// PHP 예제의 다음 줄은 교차 사이트 액세스를 허용하기 위해 추가되었습니다.
header("Access-Control-Allow-Origin: *");


///////////////////////////////////
// AngularJS 테이블

// angular로 테이블을 표시하는 것은 매우 간단합니다:
<div ng-app="myApp" ng-controller="customersCtrl">

<table>
  <tr ng-repeat="x in names">
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

</div>

<script>
var app = angular.module('myApp', []);
app.controller('customersCtrl', function($scope, $http) {
    $http.get("http://www.w3schools.com/angular/customers.php")
    .success(function (response) {$scope.names = response.records;});
});
</script>

// 테이블을 정렬하려면 orderBy 필터를 추가하십시오:
<table>
  <tr ng-repeat="x in names | orderBy : 'Country'">
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

// 테이블 인덱스를 표시하려면 $index가 있는 <td>를 추가하십시오:
<table>
  <tr ng-repeat="x in names">
    <td>{{ $index + 1 }}</td>
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

// $even 및 $odd 사용
<table>
  <tr ng-repeat="x in names">
    <td ng-if="$odd" style="background-color:#f1f1f1">{{ x.Name }}</td>
    <td ng-if="$even">{{ x.Name }}</td>
    <td ng-if="$odd" style="background-color:#f1f1f1">{{ x.Country }}</td>
    <td ng-if="$even">{{ x.Country }}</td>
  </tr>
</table>

///////////////////////////////////
// AngularJS HTML DOM

//AngularJS에는 애플리케이션 데이터를 HTML DOM 요소의 속성에 바인딩하는 지시문이 있습니다.

// ng-disabled 지시문은 AngularJS 애플리케이션 데이터를 HTML 요소의 disabled 속성에 바인딩합니다.

<div ng-app="" ng-init="mySwitch=true">

<p>
<button ng-disabled="mySwitch">나를 클릭하세요!</button>
</p>

<p>
<input type="checkbox" ng-model="mySwitch">버튼
</p>

</div>

//애플리케이션 설명:

// ng-disabled 지시문은 애플리케이션 데이터 mySwitch를 HTML 버튼의 disabled 속성에 바인딩합니다.
// ng-model 지시문은 HTML 체크박스 요소의 값을 mySwitch의 값에 바인딩합니다.
// mySwitch의 값이 true로 평가되면 버튼이 비활성화됩니다:
<p>
<button disabled>나를 클릭하세요!</button>
</p>

// mySwitch의 값이 false로 평가되면 버튼이 비활성화되지 않습니다:
<p>
  <button>나를 클릭하세요!</button>
</p>

// ng-show 지시문은 HTML 요소를 표시하거나 숨깁니다.

<div ng-app="">

<p ng-show="true">나는 보입니다.</p>

<p ng-show="false">나는 보이지 않습니다.</p>

</div>

// ng-show 지시문은 ng-show의 값에 따라 HTML 요소를 표시(또는 숨김)합니다.
// true 또는 false로 평가되는 모든 표현식을 사용할 수 있습니다:
<div ng-app="">
<p ng-show="hour > 12">나는 보입니다.</p>
</div>

///////////////////////////////////
// AngularJS 이벤트

// AngularJS에는 자체 HTML 이벤트 지시문이 있습니다.

// ng-click 지시문은 AngularJS 클릭 이벤트를 정의합니다.
<div ng-app="myApp" ng-controller="myCtrl">

<button ng-click="count = count + 1">나를 클릭하세요!</button>

<p>{{ count }}</p>

</div>
<script>
var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope) {
    $scope.count = 0;
});
</script>

// ng-hide 지시문은 애플리케이션 일부의 가시성을 설정하는 데 사용할 수 있습니다.
// ng-hide="true" 값은 HTML 요소를 보이지 않게 만듭니다.
// ng-hide="false" 값은 요소를 보이게 만듭니다.
<div ng-app="myApp" ng-controller="personCtrl">

<button ng-click="toggle()">토글</button>

<p ng-hide="myVar">
이름: <input type="text" ng-model="firstName"><br>
성: <input type="text" ng-model="lastName"><br>
<br>
전체 이름: {{firstName + " " + lastName}}
</p>

</div>

<script>
var app = angular.module('myApp', []);
app.controller('personCtrl', function($scope) {
    $scope.firstName = "John",
    $scope.lastName = "Doe"
    $scope.myVar = false;
    $scope.toggle = function() {
        $scope.myVar = !$scope.myVar;
    };
});
</script>

//애플리케이션 설명:

// personController의 첫 번째 부분은 컨트롤러에 대한 장과 동일합니다.
// 애플리케이션에는 기본 속성(변수)이 있습니다: $scope.myVar = false;
// ng-hide 지시문은 myVar의 값(true 또는 false)에 따라 두 개의 입력 필드가 있는 <p> 요소의 가시성을 설정합니다.
// toggle() 함수는 myVar를 true와 false 사이에서 토글합니다.
// ng-hide="true" 값은 요소를 보이지 않게 만듭니다.


// ng-show 지시문은 애플리케이션 일부의 가시성을 설정하는 데에도 사용할 수 있습니다.
// ng-show="false" 값은 HTML 요소를 보이지 않게 만듭니다.
// ng-show="true" 값은 요소를 보이게 만듭니다.
// 다음은 ng-hide 대신 ng-show를 사용하는 위와 동일한 예입니다:
<div ng-app="myApp" ng-controller="personCtrl">

<button ng-click="toggle()">토글</button>

<p ng-show="myVar">
이름: <input type="text" ng-model="firstName"><br>
성: <input type="text" ng-model="lastName"><br>
<br>
전체 이름: {{firstName + " " + lastName}}
</p>

</div>

<script>
var app = angular.module('myApp', []);
app.controller('personCtrl', function($scope) {
    $scope.firstName = "John",
    $scope.lastName = "Doe"
    $scope.myVar = true;
    $scope.toggle = function() {
        $scope.myVar = !$scope.myVar;
    }
});
</script>

///////////////////////////////////
// AngularJS 모듈

// AngularJS 모듈은 애플리케이션을 정의합니다.
// 모듈은 애플리케이션의 다른 부분을 위한 컨테이너입니다.
// 모듈은 애플리케이션 컨트롤러를 위한 컨테이너입니다.
// 컨트롤러는 항상 모듈에 속합니다.

// 이 애플리케이션("myApp")에는 하나의 컨트롤러("myCtrl")가 있습니다:

<!DOCTYPE html>
<html>
<script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
<body>

<div ng-app="myApp" ng-controller="myCtrl">
{{ firstName + " " + lastName }}
</div>

<script>
var app = angular.module("myApp", []);
app.controller("myCtrl", function($scope) {
    $scope.firstName = "John";
    $scope.lastName = "Doe";
});
</script>

</body>
</html>

// AngularJS 애플리케이션에서는 모듈과 컨트롤러를 자바스크립트 파일에 넣는 것이 일반적입니다.
// 이 예제에서 "myApp.js"는 애플리케이션 모듈 정의를 포함하고 "myCtrl.js"는 컨트롤러를 포함합니다:

<!DOCTYPE html>
<html>
<script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
<body>

<div ng-app="myApp" ng-controller="myCtrl">
{{ firstName + " " + lastName }}
</div>

<script src="myApp.js"></script>
<script src="myCtrl.js"></script>

</body>
</html>

//myApp.js
var app = angular.module("myApp", []);

// 모듈 정의의 [] 매개변수는 종속 모듈을 정의하는 데 사용할 수 있습니다.

// myCtrl.js
app.controller("myCtrl", function($scope) {
    $scope.firstName  = "John";
    $scope.lastName= "Doe";
});

// 전역 함수는 자바스크립트에서 피해야 합니다. 다른 스크립트에 의해 쉽게 덮어쓰이거나 파괴될 수 있습니다.

// AngularJS 모듈은 모든 함수를 모듈에 로컬로 유지하여 이 문제를 줄입니다.

// HTML 애플리케이션에서는 스크립트를 <body> 요소의 끝에 배치하는 것이 일반적이지만
// AngularJS 라이브러리를 <head> 또는 <body>의 시작 부분에 로드하는 것이 좋습니다.

// 이는 angular.module에 대한 호출이 라이브러리가 로드된 후에만 컴파일될 수 있기 때문입니다.

<!DOCTYPE html>
<html>
<body>
<script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>

<div ng-app="myApp" ng-controller="myCtrl">
{{ firstName + " " + lastName }}
</div>

<script>
var app = angular.module("myApp", []);
app.controller("myCtrl", function($scope) {
    $scope.firstName = "John";
    $scope.lastName = "Doe";
});
</script>

</body>
</html>


///////////////////////////////////
// AngularJS 애플리케이션

// AngularJS 모듈은 AngularJS 애플리케이션을 정의합니다.
// AngularJS 컨트롤러는 AngularJS 애플리케이션을 제어합니다.
// ng-app 지시문은 애플리케이션을 정의하고, ng-controller 지시문은 컨트롤러를 정의합니다.
<div ng-app="myApp" ng-controller="myCtrl">
  이름: <input type="text" ng-model="firstName"><br>
  성: <input type="text" ng-model="lastName"><br>
  <br>
  전체 이름: {{firstName + " " + lastName}}
</div>
<script>
  var app = angular.module('myApp', []);
  app.controller('myCtrl', function($scope) {
      $scope.firstName= "John";
      $scope.lastName= "Doe";
  });
</script>

// AngularJS 모듈은 애플리케이션을 정의합니다:
var app = angular.module('myApp', []);

// AngularJS 컨트롤러는 애플리케이션을 제어합니다:
app.controller('myCtrl', function($scope) {
    $scope.firstName= "John";
    $scope.lastName= "Doe";
});
```

## 소스 및 참고 자료

**예제**

- [http://www.w3schools.com/angular/angular_examples.asp](http://www.w3schools.com/angular/angular_examples.asp)

**참고 자료**

- [http://www.w3schools.com/angular/angular_ref_directives.asp](http://www.w3schools.com/angular/angular_ref_directives.asp)
- [http://www.w3schools.com/angular/default.asp](http://www.w3schools.com/angular/default.asp)
- [https://teamtreehouse.com/library/angular-basics/](https://teamtreehouse.com/library/angular-basics/)
