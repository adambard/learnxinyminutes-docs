---
category: tool
tool: AngularJS
contributors:
    - ["Walter Cordero", "http://waltercordero.com"]
filename: learnangular-cn.html
translators:
   - ["Jiang Haiyun", "http://www.atjiang.com"]
lang: zh-cn
---

## AngularJS 教程。

AngularJS 1.0 版在 2012 年发布。
Miško Hevery, 一位 Google 员工， 从 2009 年开始开发 AngularJS。
结果发现这个想法很好，从而该项目现在也被 Google 官方所支持了。

AngularJS 是一个 JavaScript 框架。它可以通过一个 "script" 标签添加到一个 HTML 页面中。
AngularJS 通过指令扩展了 HTML 属性，并且通过表达式将数据绑定到 HTML。

## 你应该已经了解了的知识

在学习 AngularJS 之前， 你应该对以下知识有了基本的了解：

- HTML
- CSS
- JavaScript

```html
// AngularJS 是一个 JavaScript 框架。它是一个用 JavaScript 写的库。
// AngularJS 以一个 JavaScript 文件的形式发布，并且能通过一个 script 标签添加到一个网页中：
// <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>

///////////////////////////////////
// AngularJS 扩展 HTML

//AngularJS 通过 ng-directives 扩展 HTML。
//ng-app 指令定义一个 AngularJS 应用。
//ng-model 指令将 HTML 控件 (input, select, textarea) 的值绑定到应用的数据上。
//ng-bind 指令将应用的数据绑定到 HTML 视图上。
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>Name: <input type="text" ng-model="name"></p>
      <p ng-bind="name"></p>
    </div>
  </body>
</html>

/*
  * 例子解析：
  * AngularJS 在网页加载后自动开启。
  * ng-app 指令告诉 AngularJS： <div> 元素是 AngularJS 应用的 "所有者"。
  * ng-model 指令将 input 输入框的值绑定到应用的 name 变量上。
  * ng-bind 指令将 <p> 元素的 innerHTML 绑定到应用的 name 变量上。
*/
<tag> 这里是要解析的内容 </tag>

///////////////////////////////////
// AngularJS 表达式

// AngularJS 表达式写在双括号内： {{ 表达式 }}。
// AngularJS 表达式采用和 ng-bind 指令一样的方式将数据绑定到 HTML。
// AngularJS 将在编写表达式的原样位置上 "输出" 数据。
// AngularJS 表达式非常像 JavaScript 表达式：它们能包含文本，运算符和变量。
// 例如 {{ 5 + 5 }} 或 {{ firstName + " " + lastName }}
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>My first expression: {{ 5 + 5 }}</p>
    </div>
  </body>
</html>

//如果你删除了 ng-app 指令， HTML 将原样显示表达式，不对它进行解析：
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div>
      <p>My first expression: {{ 5 + 5 }}</p>
    </div>
  </body>
</html>

// AngularJS 表达式采用和 ng-bind 指令一样的方式将 AngularJS 数据绑定到 HTML。
<!DOCTYPE html>
<html>
<script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>Name: <input type="text" ng-model="name"></p>
      <p>{{name}}</p>
    </div>
  </body>
</html>

// AngularJS 的数字类似 JavaScript 的数字：
<div ng-app="" ng-init="quantity=1;cost=5">
  <p>Total in dollar: {{ quantity * cost }}</p>
</div>

//AngularJS 的字符串类似 JavaScript 的字符串：
<div ng-app="" ng-init="firstName='John';lastName='Doe'">
  <p>The name is <span ng-bind="firstName + ' ' + lastName"></span></p>
</div>

//AngularJS 的对象类似 JavaScript 的对象：
<div ng-app="" ng-init="person={firstName:'John',lastName:'Doe'}">
  <p>The name is {{ person.lastName }}</p>
</div>

//AngularJS 的数组类似 JavaScript 的数组：
<div ng-app="" ng-init="points=[1,15,19,2,40]">
  <p>The third result is {{ points[2] }}</p>
</div>

// 和 JavaScript 表达式一样， AngularJS 表达式能包含文本，运算符和变量。
// 和 JavaScript 表达式不同， AngularJS 表达式能写在 HTML 内。
// AngularJS 表达式不支持条件，循环和异常，而 JavaScript 表达式却支持。
// AngularJS 表达式支持过滤器，而 JavaScript 表达式不支持。

///////////////////////////////////
// AngularJS 指令


//AngularJS 指令使用前缀 ng- 扩展 HTML 属性。
//ng-app 指令初始化一个 AngularJS 应用。
//ng-init 指令初始化应用的数据。
//ng-model 指令将 HTML 控件 (input, select, textarea) 的值绑定到应用的数据上。
<div ng-app="" ng-init="firstName='John'">
  <p>Name: <input type="text" ng-model="firstName"></p>
  <p>You wrote: {{ firstName }}</p>
</div>

//使用 ng-init 并不常见。你将在有关控制器的章节中学习如何初始化数据。

//ng-repeat 指令会重复一个 HTML 元素：
<div ng-app="" ng-init="names=['Jani','Hege','Kai']">
  <ul>
    <li ng-repeat="x in names">
      {{ x }}
    </li>
  </ul>
</div>

//ng-repeat 指令用在一个对象数组上：
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

// AngularJS 最适合用于数据库 CRUD (Create Read Update Delete) 的应用。
// 只需设想这些对象都是来自一个数据库的记录。

// ng-app 指令定义一个 AngularJS 应用的根元素。
// ng-app 指令将在页面加载后自动启动（自动初始化）应用。
// 稍后你将学习如何为 ng-app 设置一个值（如 ng-app="myModule"）， 来连接代码模块。

// ng-init 指令为一个 AngularJS 应用定义初始值。
// 通常，你不太使用 ng-init。你会转而使用一个控制器或模块。
// 你将在稍后学到更多有关控制器和模块的内容。

//ng-model 指令将 HTML 控件 (input, select, textarea) 的值绑定到应用的数据上。
//ng-model 指令还能：
//为应用的数据提供类型验证 (number, email, required)。
//为应用的数据提供状态信息 (invalid, dirty, touched, error)。
//为 HTML 元素提供 CSS 类。
//将 HTML 元素绑定到 HTML 表单。

//ng-repeat 指令为集合（一个数组）中的每个元素克隆出 HTML 元素。

///////////////////////////////////
// AngularJS 控制器

// AngularJS 控制器控制 AngularJS 应用中的数据。
// AngularJS 控制器就是常规的 JavaScript 对象。

// AngularJS 应用由控制器控制。
// ng-controller 指令定义应用的控制器。
// 一个控制器就是一个 JavaScript 对象， 通过标准的 JavaScript 对象构建器创建。

<div ng-app="myApp" ng-controller="myCtrl">

First Name: <input type="text" ng-model="firstName"><br>
Last Name: <input type="text" ng-model="lastName"><br>
<br>
Full Name: {{firstName + " " + lastName}}

</div>

<script>
var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope) {
    $scope.firstName = "John";
    $scope.lastName = "Doe";
});
</script>

//应用的解析：

//AngularJS 应用通过 ng-app="myApp" 定义。该应用运行在 <div> 内。
//ng-controller="myCtrl" 属性是一个 AngularJS 指令。它定义了一个控制器。
//myCtrl 函数是一个 JavaScript 函数。
//AngularJS 将使用一个 $scope 对象来调用控制器。
//AngularJS 中， $scope 就是该应用对象（应用的变量和函数的所有者）。
//该控制器在 $scope 内创建了两个属性（即变量 firstName 和 lastName）。
//ng-model 指令将输入表单项绑定到控制器的属性上（firstName 和 lastName）。

//以上的例子演示了一个包含有两个属性 lastName 和 firstName 的控制器。
//一个控制器也可以有方法（函数的变量）：
<div ng-app="myApp" ng-controller="personCtrl">

First Name: <input type="text" ng-model="firstName"><br>
Last Name: <input type="text" ng-model="lastName"><br>
<br>
Full Name: {{fullName()}}

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

//在较大型的应用中， 通常是将控制器代码保存在外部文件中。
//只需将 <script> </script> 标签之间的代码复制到一个名为 personController.js 的外部文件中：

<div ng-app="myApp" ng-controller="personCtrl">

First Name: <input type="text" ng-model="firstName"><br>
Last Name: <input type="text" ng-model="lastName"><br>
<br>
Full Name: {{firstName + " " + lastName}}

</div>

<script src="personController.js"></script>

// 为方便下个例子使用，我们将创建一个新的控制器文件：
angular.module('myApp', []).controller('namesCtrl', function($scope) {
    $scope.names = [
        {name:'Jani',country:'Norway'},
        {name:'Hege',country:'Sweden'},
        {name:'Kai',country:'Denmark'}
    ];
});

//将文件保存为 namesController.js：
//然后在一个应用中使用该控制器：

<div ng-app="myApp" ng-controller="namesCtrl">

<ul>
  <li ng-repeat="x in names">
    {{ x.name + ', ' + x.country }}
  </li>
</ul>

</div>

<script src="namesController.js"></script>

///////////////////////////////////
// AngularJS 过滤器

// 过滤器可以通过一个管道符添加到表达式和指令上。
// AngularJS 过滤器能用来转换数据：

- **currency**:  将一个数字格式化成货币格式。
- **filter**:  从一个数组中选择一组子集元素。
- **lowercase**: 将一个字符串格式化成小写形式。
- **orderBy**: 依据一个表达式排序一个数组。
- **upper**: 将一个字符串格式化成大写形式。

//一个过滤器可以通过一个管道符 (|) 及一个过滤器表达式添加到一个表达式上。
//（在下面的两个例子中，我们将使用前一章中的 person 控制器）
//uppercase 过滤器将字符串格式化成大写格式：
<div ng-app="myApp" ng-controller="personCtrl">

<p>The name is {{ lastName | uppercase }}</p>

</div>

//lowercase 过滤器将字符串格式化成小写格式：
<div ng-app="myApp" ng-controller="personCtrl">

<p>The name is {{ lastName | lowercase }}</p>

</div>

//currency 过滤器将一个数字格式化成货币格式：
<div ng-app="myApp" ng-controller="costCtrl">

<input type="number" ng-model="quantity">
<input type="number" ng-model="price">

<p>Total = {{ (quantity * price) | currency }}</p>

</div> 

//一个过滤器可以通过一个管道符 (|) 及一个过滤器表达式添加到一个指令上。
//orderBy 过滤器根据一个表达式排序一个数组：
<div ng-app="myApp" ng-controller="namesCtrl">

<ul>
  <li ng-repeat="x in names | orderBy:'country'">
    {{ x.name + ', ' + x.country }}
  </li>
</ul>

<div>

//一个输入框过滤器可以通过一个管道符 (|) 
//以及后跟一个冒号和模式名的 filter 添加到一个指令上。
//该过滤器从一个数组中选择一个子集：

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

//$http 是一个从远程服务器读取数据的 AngularJS 服务。

// 以下数据可由一个 web 服务器提供：
// http://www.w3schools.com/angular/customers.php
// **访问 URL 来查看数据格式**

// AngularJS $http 是一个从 web 服务器上读取数据的核心服务。
// $http.get(url) 这个函数用来读取服务器数据。
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

// 应用解析：

// AngularJS 应用由 ng-app 定义。该应用运行在一个 <div> 中。
// ng-controller 指令命名控制器对象。
// customersCtrl 函数是一个标准的 JavaScript 对象构造器。
// AngularJS 会使用一个 $scope 和 $http 对象来调用 customersCtrl。
// $scope 就是该应用对象（应用的变量和函数的所有者）。
// $http 是一个用于请求外部数据的 XMLHttpRequest 对象。
// $http.get() 从 http://www.w3schools.com/angular/customers.php 读取 JSON 数据。
// 如果成功， 该控制器会根据来自服务器的 JSON 数据，在 $scope 中创建一个属性 (names)。


// 向不同的服务器（不同于请求页）请求数据，称作跨站 HTTP 请求。
// 跨站请求在网站上很普遍。许多网页会从不同的服务器加载 CSS，图片和脚本。
// 在现代浏览器中，基于安全原因，从脚本内进行跨站 HTTP 请求是被禁止的。
// 下面的这行代码，已被加入到我们的 PHP 例子中，以便允许跨站访问。
header("Access-Control-Allow-Origin: *");


///////////////////////////////////
// AngularJS 表格

// 使用 angular 显示表格非常简单：
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

// 要排序表格，添加一个 orderBy 过滤器：
<table>
  <tr ng-repeat="x in names | orderBy : 'Country'">
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

// 要显示表格索引值，添加一个带有 $index 的 <td>：
<table>
  <tr ng-repeat="x in names">
    <td>{{ $index + 1 }}</td>
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

// 使用 $even 和 $odd
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

//AngularJS 有用于将应用的数据绑定到 HTML DOM 元素属性的指令。

// ng-disabled 指令将 AngularJS 应用的数据绑定到 HTML 元素的 disabled 属性上。

<div ng-app="" ng-init="mySwitch=true">

<p>
<button ng-disabled="mySwitch">Click Me!</button>
</p>

<p>
<input type="checkbox" ng-model="mySwitch">Button
</p>

</div>

//应用解析：

// ng-disabled 指令将应用的 mySwitch 数据绑定到 HTML 按钮的 disabled 属性上。
// ng-model 指令将 HTML checkbox 元素的值绑定到 mySwitch 的值上。
// 如果 mySwitch 的值求值为 true，则该按钮将被禁用：
<p>
<button disabled>Click Me!</button>
</p>

// 如果 mySwitch 的值求值为 false，则该按钮将不会被禁用：
<p>
  <button>Click Me!</button>
</p>

// ng-show 指令显示或隐藏一个 HTML 元素。

<div ng-app="">

<p ng-show="true">I am visible.</p>

<p ng-show="false">I am not visible.</p>

</div>

// ng-show 指令基于 ng-show 的值显示（或隐藏）一个 HTML 元素。
// 你可以使用任何能求值成 true 或 false 的表达式：
<div ng-app="">
<p ng-show="hour > 12">I am visible.</p>
</div>

///////////////////////////////////
// AngularJS 事件

// AngularJS 有它自己的 HTML 事件指令。

// ng-click 指令定义一个 AngularJS 点击事件。
<div ng-app="myApp" ng-controller="myCtrl">

<button ng-click="count = count + 1">Click me!</button>

<p>{{ count }}</p>

</div>
<script>
var app = angular.module('myApp', []);
app.controller('myCtrl', function($scope) {
    $scope.count = 0;
});
</script>

// ng-hide 指令可用于设置一个应用的部分区域的可见性。
// 值 ng-hide="true" 使得一个 HTML 元素不可见。
// 值 ng-hide="false" 使得一个 HTML 元素可见。
<div ng-app="myApp" ng-controller="personCtrl">

<button ng-click="toggle()">Toggle</button>

<p ng-hide="myVar">
First Name: <input type="text" ng-model="firstName"><br>
Last Name: <input type="text" ng-model="lastName"><br>
<br>
Full Name: {{firstName + " " + lastName}}
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

//应用解析：

// personController 的第一部分和讲述控制器章节中的一样。
// 该应用有一个默认属性（一个变量）：$scope.myVar = false：
// ng-hide 指令依据 myVar 的值（true 或 false），
// 设置 <p> 元素的可见性，该元素含有两个输入框。
// 函数 toggle() 将 myVar 在 true 和 false 间进行切换。
// 值 ng-hide="true" 使得该元素不可见。


// ng-show 指令也能用来设置一个应用的某部分的可见性。
// 值 ng-show="false" 使得一个 HTML 元素不可见。
// 值 ng-show="true" 使得一个 HTML 元素可见。
// 这个例子与上面的一样，但用 ng-show 替代了 ng-hide：
<div ng-app="myApp" ng-controller="personCtrl">

<button ng-click="toggle()">Toggle</button>

<p ng-show="myVar">
First Name: <input type="text" ng-model="firstName"><br>
Last Name: <input type="text" ng-model="lastName"><br>
<br>
Full Name: {{firstName + " " + lastName}}
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
// AngularJS 模块

// 一个 AngularJS 模块定义一个应用。
// 模块是一个应用的不同部分所在的一个容器。
// 模块是应用控制器的一个容器。
// 控制器总是隶属于一个模块。

// 这个应用 ("myApp") 有一个控制器 ("myCtrl")：

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

// 在 AngularJS 应用中通常将模块和控制器放置在 JavaScript 文件中。
// 在本例中，"myApp.js" 包含了一个应用模块的定义，而 "myCtrl.js" 包含了控制器：

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

// 模块定义中的 [] 参数可用来定义依赖的模块。

// myCtrl.js
app.controller("myCtrl", function($scope) {
    $scope.firstName  = "John";
    $scope.lastName= "Doe";
});

// JavaScript 中应该避免使用全局函数。它们会非常容易地被覆盖
// 或被其它脚本破坏。

// AngularJS 脚本通过将所有函数保存在模块内，缓解了这种问题。

// 虽然 HTML 应用中通常是将脚本放置在
// <body> 元素的末尾，但还是推荐你要么在
// <head> 中要么在 <body> 的开头处加载 AngularJS 库。

// 这是因为对 angular.module 的调用只有在库被加载后才能被编译。

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
// AngularJS 应用

// AngularJS 模块定义 AngularJS 应用。
// AngularJS 控制器控制 AngularJS 应用。
// ng-app 指令定义该应用，ng-controller 定义该控制器。
<div ng-app="myApp" ng-controller="myCtrl">
  First Name: <input type="text" ng-model="firstName"><br>
  Last Name: <input type="text" ng-model="lastName"><br>
  <br>
  Full Name: {{firstName + " " + lastName}}
</div>
<script>
  var app = angular.module('myApp', []);
  app.controller('myCtrl', function($scope) {
      $scope.firstName= "John";
      $scope.lastName= "Doe";
  });
</script>

// AngularJS 模块定义应用：
var app = angular.module('myApp', []);

// AngularJS 控制器控制应用：
app.controller('myCtrl', function($scope) {
    $scope.firstName= "John";
    $scope.lastName= "Doe";
});
```

## 来源 & 参考

**例子**

- http://www.w3schools.com/angular/angular_examples.asp

**参考**

- http://www.w3schools.com/angular/angular_ref_directives.asp
- http://www.w3schools.com/angular/default.asp
