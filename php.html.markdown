---
language: php
author: Malcolm Fell
author_url: http://emarref.net/
---

This document describes PHP 5+.

## [Basic Syntax](http://www.php.net/manual/en/language.basic-syntax.php)

All statements must end with a semi-colon; All PHP code must be between <?php and ?> tags. PHP can also be
configured to respect the [short open tags](http://www.php.net/manual/en/ini.core.php#ini.short-open-tag) <? and ?>.

## [Comments](http://www.php.net/manual/en/language.basic-syntax.comments.php)

```php
<?php

// Two forward slashes start a one-line comment.

# So will a hash (aka pound symbol) but // is more common

/*
     Surrounding text in slash-asterisk and asterisk-slash
     makes it a multi-line comment.
*/
```

## [Types](http://www.php.net/manual/en/language.types.php)

Types are [weakly typed](http://en.wikipedia.org/wiki/Strong_and_weak_typing) and begin with the $ symbol.
A valid variable name starts with a letter or underscore, followed by any number of letters, numbers, or underscores.

### Scalars

```php
<?php

// Boolean values are case-insensitive
$boolean = true; // or TRUE or True
$boolean = false; // or FALSE or False

// Integers
$integer = 1234; // decimal number
$integer = -123; // a negative number
$integer = 0123; // octal number (equivalent to 83 decimal)
$integer = 0x1A; // hexadecimal number (equivalent to 26 decimal)

// Floats (aka doubles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Arithmetic
$sum = $number + $float;
$difference = $number - $float;
$product = $number * $float;
$quotient = $number / $float;

// Shorthand arithmetic
$number += 1; // Add 1 to $number
$number++; // Add 1 to $number after it is used
++$number; // Add 1 to $number before it is used.
$number /= $float // Divide and assign the quotient to $number

// Strings should be enclosed in single quotes;
$sgl_quotes = '$String'; // => '$String'

// Avoid using double quotes except to embed other variables
$dbl_quotes = "This is a $sgl_quotes." // => 'This is a $String'

// Escape special characters with backslash
$escaped = "This contains a \t tab character.";

// Enclose a variable in curly braces if needed
$money = "I have $${integer} in the bank."

// Since PHP 5.3, nowdocs can be used for uninterpolated multi-liners
$nowdoc = <<<'END'
Multi line
string
END;

$heredoc = <<<END
Multi line
$sgl_quotes
END; // Nowdoc syntax is available in PHP 5.3.0

// Manipulation
$concatenated = $sgl_quotes . $dbl_quotes;
```

### Compound

```php
<?php

// Arrays
$array = array(1, 2, 3);
$array = [1, 2, 3]; // As of PHP 5.4
$string = ["One", "Two", "Three"];
$string[0]; // Holds the value "One";

// Associative arrays, known as hashmaps in some languages.
$associative = ["One" => 1, "Two" => 2, "Three" => 3];
$associative["One"]; // Holds the value 1
```

## Output

```php
<?php

echo('Hello World!');
// Prints Hello World! to stdout.
// Stdout is the web page if running in a browser.

print('Hello World!'); // The same as echo

// echo is actually a language construct, so you can drop the parentheses.
echo 'Hello World!';
print 'Hello World!'; // So is print

echo 100;
echo $variable;
echo function_result();

// If short open tags are configured, or your PHP version is
// 5.4.0 or greater, you can use the short echo syntax
<?= $variable ?>
```

## [Operators](http://www.php.net/manual/en/language.operators.php)

### Assignment

```php
<?php

$x = 1;
$y = 2;
$x = $y; // A now contains the same value sa $y
$x = &$y;
// $x now contains a reference to $y. Changing the value of
// $x will change the value of $y also, and vice-versa.
```

### Comparison

```php
<?php

// These comparisons will always be true, even if the types aren't the same.
$a == $b // TRUE if $a is equal to $b after type juggling.
$a != $b // TRUE if $a is not equal to $b after type juggling.
$a <> $b // TRUE if $a is not equal to $b after type juggling.
$a < $b    // TRUE if $a is strictly less than $b.
$a > $b // TRUE if $a is strictly greater than $b.
$a <= $b // TRUE if $a is less than or equal to $b.
$a >= $b // TRUE if $a is greater than or equal to $b.

// The following will only be true if the values match and are the same type.
$a === $b // TRUE if $a is equal to $b, and they are of the same type.
$a !== $b // TRUE if $a is not equal to $b, or they are not of the same type.
1 == '1' // TRUE
1 === '1' // FALSE
```

## [Type Juggling](http://www.php.net/manual/en/language.types.type-juggling.php)

Variables can be converted between types, depending on their usage.

```php
<?php

$integer = 1;
echo $integer + $integer; // Outputs 2;

$string = '1';
echo $string + $string;
// Also outputs 2 because the + operator converts the strings to integers

$string = 'one';
echo $string + $string;
// Outputs 0 because the + operator cannot cast the string 'one' to a number
```

Type casting can be used to treat a variable as another type temporarily by using cast operators in parentheses.

```php
$boolean = (boolean) $integer; // $boolean is true

$zero = 0;
$boolean = (boolean) $zero; // $boolean is false

$integer = 5;
$string = strval($integer);
// There are also dedicated functions for casting most types

$var = null; // Null value
```

## [Control Structures](http://www.php.net/manual/en/language.control-structures.php)

### If Statements

```php
<?php

if (/* test */) {
    // Do something
}

if (/* test */) {
    // Do something
} else {
    // Do something else
}

if (/* test */) {
    // Do something
} elseif(/* test2 */) {
    // Do something else, only if test2
}

if (/* test */) {
    // Do something
} elseif(/* test2 */) {
    // Do something else, only if test2
} else {
    // Do something default
}
?>

<?php if (/* test */): ?>
This is displayed if the test is truthy.
<?php else: ?>
This is displayed otherwise.
<?php endif; ?>
```

### Switch statements

```php
<?php

switch ($variable) {
    case 'one':
        // Do something if $variable == 'one'
        break;
    case 'two':
    case 'three':
        // Do something if $variable is either 'two' or 'three'
        break;
    default:
        // Do something by default
}

```

### Loops

```php
<?php

$i = 0;
while ($i < 5) {
    echo $i++;
}

$i = 0;
do {
    echo $i++;
} while ($i < 5);

for ($x = 0; $x < 10; $x++) {
    echo $x; // Will echo 0 - 9
}

$wheels = ["bicycle" => 2, "car" => 4];

foreach ($wheels as $vehicle => $wheel_count) {
    echo "A $vehicle has $wheel_count wheels";
}

// This loop will stop after outputting 2
$i = 0;
while ($i < 5) {
    if ($i == 3) {
        break; // Exit out of the while loop and continue.
    }
    echo $i++;
}

// This loop will output everything except 3
$i = 0;
while ($i < 5) {
    if ($i == 3) {
        continue; // Skip this iteration of the loop
    }
    echo $i++;
}
```

## Functions

Functions are created with the ```function``` keyword.

```php
<?php

function my_function($my_arg) {
    $my_variable = 1;
}

// $my_variable and $my_arg cannot be accessed outside of the function
```

Functions may be invoked by name.

```php
<?php

my_function_name();

$variable = get_something(); // A function may return a value
```

A valid function name starts with a letter or underscore, followed by any
number of letters, numbers, or underscores. There are three ways to declare functions.

### [User-defined](http://www.php.net/manual/en/functions.user-defined.php)

```php
<?php

function my_function_name ($arg_1, $arg_2) {
    // $arg_1 and $arg_2 are required
}

// Functions may be nested to limit scope
function outer_function ($arg_1 = null) { // $arg_1 is optional
    function inner_function($arg_2 = 'two') { // $arg_2 will default to 'two'
    }
}

// inner_function() does not exist and cannot be called until
// outer_function() is called
```

This enables [currying](http://en.wikipedia.org/wiki/Currying) in PHP.

```php
function foo ($x, $y, $z) {
  echo "$x - $y - $z";
}

function bar ($x, $y) {
  return function ($z) use ($x, $y) {
    foo($x, $y, $z);
  };
}

$bar = bar('A', 'B');
$bar('C');
```

### [Variable](http://www.php.net/manual/en/functions.variable-functions.php)

```php
<?php

$function_name = 'my_function_name';

$function_name(); // will execute the my_function_name() function
```

### [Anonymous](http://www.php.net/manual/en/functions.anonymous.php)

Similar to variable functions, functions may be anonymous.

```php
<?php

function my_function($callback) {
    $callback('My argument');
}

my_function(function ($my_argument) {
    // do something
});

// Closure style
$my_function = function() {
    // Do something
};

$my_function();
```

## [Classes](http://www.php.net/manual/en/language.oop5.php)

Classes are defined with the ```class``` keyword.

```php
<?php

class MyClass {
    const MY_CONST = 'value';
    static $staticVar = 'something';
    public $property = 'value'; // Properties must declare their visibility
}

echo MyClass::MY_CONST; // Outputs "value";

final class YouCannotExtendMe {
}
```

Classes are insantiated with the ```new``` keyword. Functions are referred to as
methods if they belong to a class.

```php
<?php

class MyClass {
    function myFunction() {
    }

    final function youCannotOverrideMe() {
    }

    public static function myStaticMethod() {
    }
}

$cls = new MyClass(); // The parentheses are optional.

echo MyClass::$staticVar; // Access to static vars

echo $cls->property; // Access to properties

MyClass::myStaticMethod(); // myStaticMethod cannot be run on $cls
```

PHP offers some [magic methods](http://www.php.net/manual/en/language.oop5.magic.php) for classes.

```php
<?php

class MyClass {
    private $property;

    public function __get($key)
    {
        return $this->$key;
    }

    public function __set($key, $value)
    {
        $this->$key = $value;
    }
}

$x = new MyClass();
echo $x->property; // Will use the __get() method
$x->property = 'Something'; // Will use the __set() method
```

Classes can be abstract (using the ```abstract``` keyword), extend other classes (using the ```extends``` keyword) and
implement interfaces (using the ```implements``` keyword). An interface is declared with the ```interface``` keyword.

```php
<?php

interface InterfaceOne
{
    public function doSomething();
}

interface InterfaceTwo
{
    public function doSomething();
}

abstract class MyAbstractClass implements InterfaceOne
{
}

class MyClass extends MyAbstractClass implements InterfaceTwo
{
}

// Classes can implement more than one interface
class SomeOtherClass implements InterfaceOne, InterfaceTwo
{
}
```

### [Namespaces](http://www.php.net/manual/en/language.namespaces.rationale.php)

By default, classes exist in the global namespace, and can be explicitly called with a backslash.

```php
<?php

$cls = new \MyClass();
```

```php
<?php

namespace My\Namespace;

class MyClass
{
}

$cls = new My\Namespace\MyClass;
```

Or from within another namespace.

```php
<?php

namespace My\Other\Namespace;

use My\Namespace\MyClass;

$cls = new MyClass();
```

Or you can alias the namespace;

```php
<?php

namespace My\Other\Namespace;

use My\Namespace as SomeOtherNamespace;

$cls = new SomeOtherNamespace\MyClass();
```

### [Traits](http://www.php.net/manual/en/language.oop5.traits.php)

Traits are available since PHP 5.4.0 and are declared using the ```trait``` keyword.

```php
<?php

trait MyTrait {
    public function myTraitMethod()
    {
        // Do something
    }
}

class MyClass
{
    use MyTrait;
}

$cls = new MyClass();
$cls->myTraitMethod();
```

## More Information

Visit the [official PHP documentation](http://www.php.net/manual/) for reference and community input.

If you're interested in up-to-date best practices, visit [PHP The Right Way](http://www.phptherightway.com/).

If you're coming from a language with good package management, check out [Composer](http://getcomposer.org/).
