---
language: TypeScript
category: language
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Shawn Zhang", "https://github.com/shawnzhang009"]
filename: learntypescript-cn.ts
lang: zh-cn
---

TypeScript是一门为开发大型JavaScript应用而设计的语言。TypeScript在JavaScript的基础上增加了类、模块、接口、泛型和静态类型（可选）等常见的概念。它是JavaScript的一个超集：所有JavaScript代码都是有效的TypeScript代码，所以任何JavaScript项目都可以无缝引入TypeScript. TypeScript编译器会把TypeScript代码编译成JavaScript代码。

本文只关注TypeScript额外增加的区别于[JavaScript](../javascript-cn/)的语法，.

如需测试TypeScript编译器，你可以在[Playground](http://www.typescriptlang.org/Playground)码代码，它会自动编译成JavaScript代码然后直接显示出来。

```js
// TypeScript有三种基本类型
var isDone: boolean = false;
var lines: number = 42;
var name: string = "Anders";

// 如果不知道是什么类型，可以使用"any"(任意)类型
var notSure: any = 4;
notSure = "maybe a string instead";
notSure = false; // 亦可，定义为布尔型

// 对于集合的声明, 有类型化数组和泛型数组
var list: number[] = [1, 2, 3];
// 另外一种，使用泛型数组
var list: Array<number> = [1, 2, 3];

// 枚举：
enum Color {Red, Green, Blue};
var c: Color = Color.Green;

// 最后，"void"用于函数没有任何返回的特殊情况下
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// 函数是"第一等公民"(first class citizens), 支持使用箭头表达式和类型推断

// 以下是相等的，TypeScript编译器会把它们编译成相同的JavaScript代码
var f1 = function(i: number): number { return i * i; }
// 返回推断类型的值
var f2 = function(i: number) { return i * i; }
var f3 = (i: number): number => { return i * i; }
// 返回推断类型的值
var f4 = (i: number) => { return i * i; }
// 返回推断类型的值, 单行程式可以不需要return关键字和大括号
var f5 = (i: number) =>  i * i;

// 接口是结构化的，任何具有这些属性的对象都与该接口兼容
interface Person {
  name: string;
  // 可选属性，使用"?"标识
  age?: number;
  // 函数
  move(): void;
}

// 实现"Person"接口的对象，当它有了"name"和"move"方法之后可被视为一个"Person"
var p: Person = { name: "Bobby", move: () => {} };
// 带了可选参数的对象
var validPerson: Person = { name: "Bobby", age: 42, move: () => {} };
// 因为"age"不是"number"类型所以这不是一个"Person"
var invalidPerson: Person = { name: "Bobby", age: true };

// 接口同样可以描述一个函数的类型
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// 参数名并不重要，参数类型才是重要的
var mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) != -1;
}

// 类 - 成员默认为公共的(public)
class Point {
  // 属性
  x: number;

  // 构造器 - 这里面的public/private关键字会为属性生成样板代码和初始化值
  // 这个例子中，y会被同x一样定义，不需要额外代码
  // 同样支持默认值

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // 函数
  dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // 静态成员
  static origin = new Point(0, 0);
}

var p1 = new Point(10 ,20);
var p2 = new Point(25); //y为0

// 继承
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // 必须显式调用父类的构造器
  }

  // 重写
  dist() {
    var d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// 模块, "."可以作为子模块的分隔符
module Geometry {
  export class Square {
    constructor(public sideLength: number = 0) {
    }
    area() {
      return Math.pow(this.sideLength, 2);
    }
  }
}

var s1 = new Geometry.Square(5);

// 引入模块并定义本地别名
import G = Geometry;

var s2 = new G.Square(10);

// 泛型
// 类
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// 接口
interface Pair<T> {
  item1: T;
  item2: T;
}

// 以及函数
var pairToTuple = function<T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

var tuple = pairToTuple({ item1:"hello", item2:"world"});

// 引用定义文件
// <reference path="jquery.d.ts" />

// 模板字符串(使用反引号的字符串)
// 嵌入变量的模板字符串
var name = 'Tyrone';
var greeting = `Hi ${name}, how are you?`
// 有多行内容的模板字符串
var multiline = `This is an example
of a multiline string`;

```

## 参考资料
 * [TypeScript官网](http://www.typescriptlang.org/)
 * [TypeScript语言规范说明书(pdf)](http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - TypeScript介绍](http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [GitHub源码](https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - 类型定义仓库](http://definitelytyped.org/)
