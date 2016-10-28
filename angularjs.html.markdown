---
category: tool
tool: AngularJS
contributors:
    - ["Walter Cordero", "http://waltercordero.com"]
filename: learnangular.html
---

## AngularJS Tutorial.

AngularJS version 1.0 was released in 2012.
Miško Hevery, a Google employee, started to work with AngularJS in 2009.
The idea turned out very well, and the project is now officially supported by Google.

AngularJS is a JavaScript framework. It can be added to an HTML page with a "script" tag.
AngularJS extends HTML attributes with Directives, and binds data to HTML with Expressions.

##What You Should Already Know

Before you study AngularJS, you should have a basic understanding of:
* HTML
* CSS
* JavaScript

```html
// AngularJS is a JavaScript framework. It is a library written in JavaScript.
// AngularJS is distributed as a JavaScript file, and can be added to a web page with a script tag:
// <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>

///////////////////////////////////
// AngularJS Extends HTML

//AngularJS extends HTML with ng-directives.
//The ng-app directive defines an AngularJS application.
//The ng-model directive binds the value of HTML controls (input, select, textarea) to application data.
//The ng-bind directive binds application data to the HTML view.
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
  * Example explained:
  * AngularJS starts automatically when the web page has loaded.
  * The ng-app directive tells AngularJS that the <div> element is the "owner" of an AngularJS application.
  * The ng-model directive binds the value of the input field to the application variable name.
  * The ng-bind directive binds the innerHTML of the <p> element to the application variable name.
*/
<tag> Here are content to be intrepreted </tag>

///////////////////////////////////
// AngularJS Expressions

// AngularJS expressions are written inside double braces: {{ expression }}.
// AngularJS expressions binds data to HTML the same way as the ng-bind directive.
// AngularJS will "output" data exactly where the expression is written.
// AngularJS expressions are much like JavaScript expressions: They can contain literals, operators, and variables.
// Example {{ 5 + 5 }} or {{ firstName + " " + lastName }}
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div ng-app="">
      <p>My first expression: {{ 5 + 5 }}</p>
    </div>
  </body>
</html>

//If you remove the ng-app directive, HTML will display the expression as it is, without solving it:
<!DOCTYPE html>
<html>
  <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.min.js"></script>
  <body>
    <div>
      <p>My first expression: {{ 5 + 5 }}</p>
    </div>
  </body>
</html>

// AngularJS expressions bind AngularJS data to HTML the same way as the ng-bind directive.
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

// AngularJS numbers are like JavaScript numbers:
<div ng-app="" ng-init="quantity=1;cost=5">
  <p>Total in dollar: {{ quantity * cost }}</p>
</div>

//AngularJS strings are like JavaScript strings:
<div ng-app="" ng-init="firstName='John';lastName='Doe'">
  <p>The name is <span ng-bind="firstName + ' ' + lastName"></span></p>
</div>

//AngularJS objects are like JavaScript objects:
<div ng-app="" ng-init="person={firstName:'John',lastName:'Doe'}">
  <p>The name is {{ person.lastName }}</p>
</div>

//AngularJS arrays are like JavaScript arrays:
<div ng-app="" ng-init="points=[1,15,19,2,40]">
  <p>The third result is {{ points[2] }}</p>
</div>

// Like JavaScript expressions, AngularJS expressions can contain literals, operators, and variables.
// Unlike JavaScript expressions, AngularJS expressions can be written inside HTML.
// AngularJS expressions do not support conditionals, loops, and exceptions, while JavaScript expressions do.
// AngularJS expressions support filters, while JavaScript expressions do not.

///////////////////////////////////
// AngularJS Directives


//AngularJS directives are extended HTML attributes with the prefix ng-.
//The ng-app directive initializes an AngularJS application.
//The ng-init directive initializes application data.
//The ng-model directive binds the value of HTML controls (input, select, textarea) to application data.
<div ng-app="" ng-init="firstName='John'">
  <p>Name: <input type="text" ng-model="firstName"></p>
  <p>You wrote: {{ firstName }}</p>
</div>

//Using ng-init is not very common. You will learn how to initialize data in the chapter about controllers.

//The ng-repeat directive repeats an HTML element:
<div ng-app="" ng-init="names=['Jani','Hege','Kai']">
  <ul>
    <li ng-repeat="x in names">
      {{ x }}
    </li>
  </ul>
</div>

//The ng-repeat directive used on an array of objects:
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

// AngularJS is perfect for database CRUD (Create Read Update Delete) applications.
// Just imagine if these objects were records from a database.

// The ng-app directive defines the root element of an AngularJS application.
// The ng-app directive will auto-bootstrap (automatically initialize) the application when a web page is loaded.
// Later you will learn how ng-app can have a value (like ng-app="myModule"), to connect code modules.

// The ng-init directive defines initial values for an AngularJS application.
// Normally, you will not use ng-init. You will use a controller or module instead.
// You will learn more about controllers and modules later.

//The ng-model directive binds the value of HTML controls (input, select, textarea) to application data.
//The ng-model directive can also:
//Provide type validation for application data (number, email, required).
//Provide status for application data (invalid, dirty, touched, error).
//Provide CSS classes for HTML elements.
//Bind HTML elements to HTML forms.

//The ng-repeat directive clones HTML elements once for each item in a collection (in an array).

///////////////////////////////////
// AngularJS Controllers

// AngularJS controllers control the data of AngularJS applications.
// AngularJS controllers are regular JavaScript Objects.

// AngularJS applications are controlled by controllers.
// The ng-controller directive defines the application controller.
// A controller is a JavaScript Object, created by a standard JavaScript object constructor.

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

//Application explained:

//The AngularJS application is defined by  ng-app="myApp". The application runs inside the <div>.
//The ng-controller="myCtrl" attribute is an AngularJS directive. It defines a controller.
//The myCtrl function is a JavaScript function.
//AngularJS will invoke the controller with a $scope object.
//In AngularJS, $scope is the application object (the owner of application variables and functions).
//The controller creates two properties (variables) in the scope (firstName and lastName).
//The ng-model directives bind the input fields to the controller properties (firstName and lastName).

//The example above demonstrated a controller object with two properties: lastName and firstName.
//A controller can also have methods (variables as functions):
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

//In larger applications, it is common to store controllers in external files.
//Just copy the code between the <script> </script> tags into an external file named personController.js:

<div ng-app="myApp" ng-controller="personCtrl">

First Name: <input type="text" ng-model="firstName"><br>
Last Name: <input type="text" ng-model="lastName"><br>
<br>
Full Name: {{firstName + " " + lastName}}

</div>

<script src="personController.js"></script>

// For the next example we will create a new controller file:
angular.module('myApp', []).controller('namesCtrl', function($scope) {
    $scope.names = [
        {name:'Jani',country:'Norway'},
        {name:'Hege',country:'Sweden'},
        {name:'Kai',country:'Denmark'}
    ];
});

//Save the file as  namesController.js:
//And then use the controller file in an application:

<div ng-app="myApp" ng-controller="namesCtrl">

<ul>
  <li ng-repeat="x in names">
    {{ x.name + ', ' + x.country }}
  </li>
</ul>

</div>

<script src="namesController.js"></script>

///////////////////////////////////
// AngularJS Filers

// Filters can be added to expressions and directives using a pipe character.
// AngularJS filters can be used to transform data:

**currency:  Format a number to a currency format.
**filter:  Select a subset of items from an array.
**lowercase: Format a string to lower case.
**orderBy: Orders an array by an expression.
**uppercase: Format a string to upper case.

//A filter can be added to an expression with a pipe character (|) and a filter.
//(For the next two examples we will use the person controller from the previous chapter)
//The uppercase filter format strings to upper case:
<div ng-app="myApp" ng-controller="personCtrl">

<p>The name is {{ lastName | uppercase }}</p>

</div>

//The lowercase filter format strings to lower case:
<div ng-app="myApp" ng-controller="personCtrl">

<p>The name is {{ lastName | lowercase }}</p>

</div>

//The currency filter formats a number as currency:
<div ng-app="myApp" ng-controller="costCtrl">

<input type="number" ng-model="quantity">
<input type="number" ng-model="price">

<p>Total = {{ (quantity * price) | currency }}</p>

</div> 

//A filter can be added to a directive with a pipe character (|) and a filter.
//The orderBy filter orders an array by an expression:
<div ng-app="myApp" ng-controller="namesCtrl">

<ul>
  <li ng-repeat="x in names | orderBy:'country'">
    {{ x.name + ', ' + x.country }}
  </li>
</ul>

<div>

//An input filter can be added to a directive with a pipe character (|) 
//and filter followed by a colon and a model name.
//The filter filter selects a subset of an array:

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

//$http is an AngularJS service for reading data from remote servers.

// The following data can be provided by a web server:
// http://www.w3schools.com/angular/customers.php
// **Check the URL to see the data format**

// AngularJS $http is a core service for reading data from web servers.
// $http.get(url) is the function to use for reading server data.
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

Application explained:

// The AngularJS application is defined by ng-app. The application runs inside a <div>.
// The ng-controller directive names the controller object.
// The customersCtrl function is a standard JavaScript object constructor.
// AngularJS will invoke customersCtrl with a $scope and $http object.
// $scope is the application object (the owner of application variables and functions).
// $http is an XMLHttpRequest object for requesting external data.
// $http.get() reads JSON data from http://www.w3schools.com/angular/customers.php.
// If success, the controller creates a property (names) in the scope, with JSON data from the server.


// Requests for data from a different server (than the requesting page), are called cross-site HTTP requests.
// Cross-site requests are common on the web. Many pages load CSS, images, and scripts from different servers.
// In modern browsers, cross-site HTTP requests from scripts are restricted to same site for security reasons.
// The following line, in our PHP examples, has been added to allow cross-site access.
header("Access-Control-Allow-Origin: *");


///////////////////////////////////
// AngularJS Tables

// Displaying tables with angular is very simple:
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

// To sort the table, add an orderBy filter: 
<table>
  <tr ng-repeat="x in names | orderBy : 'Country'">
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

// To display the table index, add a <td> with $index: 
<table>
  <tr ng-repeat="x in names">
    <td>{{ $index + 1 }}</td>
    <td>{{ x.Name }}</td>
    <td>{{ x.Country }}</td>
  </tr>
</table>

// Using $even and $odd
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

//AngularJS has directives for binding application data to the attributes of HTML DOM elements.

// The ng-disabled directive binds AngularJS application data to the disabled attribute of HTML elements.

<div ng-app="" ng-init="mySwitch=true">

<p>
<button ng-disabled="mySwitch">Click Me!</button>
</p>

<p>
<input type="checkbox" ng-model="mySwitch">Button
</p>

</div>

//Application explained:

// The ng-disabled directive binds the application data mySwitch to the HTML button's disabled attribute.
// The ng-model directive binds the value of the HTML checkbox element to the value of mySwitch.
// If the value of mySwitch evaluates to true, the button will be disabled: 
<p>
<button disabled>Click Me!</button>
</p>

// If the value of mySwitch evaluates to false, the button will not be disabled: 
<p>
  <button>Click Me!</button>
</p>

// The ng-show directive shows or hides an HTML element.

<div ng-app="">

<p ng-show="true">I am visible.</p>

<p ng-show="false">I am not visible.</p>

</div>

// The ng-show directive shows (or hides) an HTML element based on the value of ng-show.
// You can use any expression that evaluates to true or false:
<div ng-app="">
<p ng-show="hour > 12">I am visible.</p>
</div>

///////////////////////////////////
// AngularJS Events

// AngularJS has its own HTML events directives.

// The ng-click directive defines an AngularJS click event.
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

// The ng-hide directive can be used to set the visibility of a part of an application.
// The value ng-hide="true" makes an HTML element invisible.
// The value ng-hide="false" makes the element visible.
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

//Application explained:

// The first part of the personController is the same as in the chapter about controllers.
// The application has a default property (a variable): $scope.myVar = false;
// The ng-hide directive sets the visibility, of a <p> element with two input fields, 
// according to the value (true or false) of myVar.
// The function toggle() toggles myVar between true and false.
// The value ng-hide="true" makes the element invisible.


// The ng-show directive can also be used to set the visibility of a part of an application.
// The value ng-show="false" makes an HTML element invisible.
// The value ng-show="true" makes the element visible.
// Here is the same example as above, using ng-show instead of ng-hide:
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
// AngularJS Modules

// An AngularJS module defines an application.
// The module is a container for the different parts of an application.
// The module is a container for the application controllers.
// Controllers always belong to a module.

// This application ("myApp") has one controller ("myCtrl"):

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

// It is common in AngularJS applications to put the module and the controllers in JavaScript files.
// In this example, "myApp.js" contains an application module definition, while "myCtrl.js" contains the controller:

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

// The [] parameter in the module definition can be used to define dependent modules.

// myCtrl.js
app.controller("myCtrl", function($scope) {
    $scope.firstName  = "John";
    $scope.lastName= "Doe";
});

// Global functions should be avoided in JavaScript. They can easily be overwritten 
// or destroyed by other scripts.

// AngularJS modules reduces this problem, by keeping all functions local to the module.

// While it is common in HTML applications to place scripts at the end of the 
// <body> element, it is recommended that you load the AngularJS library either
// in the <head> or at the start of the <body>.

// This is because calls to angular.module can only be compiled after the library has been loaded.

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
// AngularJS Applications

// AngularJS modules define AngularJS applications.
// AngularJS controllers control AngularJS applications.
// The ng-app directive defines the application, the ng-controller directive defines the controller.
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

// AngularJS modules define applications:
var app = angular.module('myApp', []);

// AngularJS controllers control applications:
app.controller('myCtrl', function($scope) {
    $scope.firstName= "John";
    $scope.lastName= "Doe";
});
```

## Source & References

**Examples
* http://www.w3schools.com/angular/angular_examples.asp

**References
* http://www.w3schools.com/angular/angular_ref_directives.asp
* http://www.w3schools.com/angular/default.asp
* https://teamtreehouse.com/library/angular-basics/

Feedback is welcome! You can find me in:
[@WalterC_87](https://twitter.com/WalterC_87), or
[me@waltercordero.com](mailto:me@waltercordero.com).

