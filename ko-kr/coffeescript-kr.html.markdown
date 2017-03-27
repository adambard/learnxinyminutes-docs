---
language: coffeescript
category: language
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
filename: coffeescript-kr.coffee
translators:
    - ["wikibook", "http://wikibook.co.kr"]
lang: ko-kr
---

``` coffeescript
# 커피스크립트(CoffeeScript)는 최신 유행을 따르는 언어입니다.
# 커피스크립트는 여러 현대 언어의 트렌드를 따르는데,
# 그래서 주석을 작성할 때는 루비나 파이썬과 같이 해시를 씁니다.

###
블록 주석은 이처럼 작성하며, 자바스크립트 코드로 만들어지도록
'/ *'와 '* /'로 직접적으로 변환됩니다.

계속하기에 앞서 자바스크립트 시맨틱을 대부분 이해하고 있어야 합니다.
###

# 할당:
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# 조건문:
number = -42 if opposite #=> if(opposite) { number = -42; }

# 함수:
square = (x) -> x * x #=> var square = function(x) { return x * x; }

# 범위:
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# 객체:
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#  "root": Math.sqrt,
#  "square": square,
#  "cube": function(x) { return x * square(x); }
#}

# 가변 인자(splat):
race = (winner, runners...) ->
  print winner, runners

# 존재 여부 확인:
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# 배열 조건 제시법(comprehensions):
cubes = (math.cube num for num in list) #=> ...
```
