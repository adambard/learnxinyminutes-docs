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

TypeScript 是一门为开发大型 JavaScript 应用而设计的语言。TypeScript 在 JavaScript 的基础上增加了类、模块、接口、泛型和静态类型（可选）等常见的概念。它是 JavaScript 的超集：所有 JavaScript 代码都是有效的 TypeScript 代码，因此任何 JavaScript 项目都可以无缝引入 TypeScript，TypeScript 编译器最终会把 TypeScript 代码编译成 JavaScript 代码。

本文只关注 TypeScript 额外增加的区别于 [JavaScript](../javascript-cn/) 的语法，.

如需测试 TypeScript 编译器，你可以到 [Playground](https://www.typescriptlang.org/play/) 编写代码，它会自动将你编写的 TypeScript 代码编译成 JavaScript 代码后，在右侧即时展示出来。

```ts
// TypeScript 有三种基本类型，布尔类型、数值类型、字符串类型
let isDone: boolean = false;
let lines: number = 42;
let name: string = 'Anders';

// 如果不知道是什么类型，可以使用 "any" (任意)类型
let notSure: any = 4;
notSure = '可以重新赋值，转换为字符串类型';
notSure = false; // 亦可，重新定义为布尔类型

// 使用 const 关键字将一个字面量修饰为常量
const numLivesForCat = 9;
numLivesForCat = 1; // 常量不能重新被赋值，所以这里会报错

// TypeScript 中的 collection 有两种表示形式, 一种是有类型的数组，另一种是泛型数组
let list: number[] = [1, 2, 3];
// 或者，使用泛型数组
let list: Array<number> = [1, 2, 3];

// 枚举：
enum Color {Red, Green, Blue}
let c: Color = Color.Green;

// 最后是 "void"，它用于表明函数没有任何返回值的特殊情况
function bigHorribleAlert(): void {
  alert('我是个烦人的弹出框！');
}

// 函数是"一等公民"(first class citizens), 支持使用 lambda 胖箭头表达式和类型推断

// 以下 f1-f5 五个函数是等价的，TypeScript 编译器会把它们编译成相同的 JavaScript 代码(可以到 Playground 验证)
// 一般的函数
let f1 = function(i: number): number { return i * i; };
// 根据返回值推断函数返回类型
let f2 = function(i: number) { return i * i; };
// 胖箭头表达式
let f3 = (i: number): number => { return i * i; };
// 根据返回值推断返回类型的胖箭头表达式
let f4 = (i: number) => { return i * i; };
// 根据返回值推断返回类型的胖箭头表达式, 省略花括号的同时，可以同时省去 return 关键字
let f5 = (i: number) =>  i * i;

// 接口是结构化的，任何具备接口中声明的全部属性的对象，都与该接口兼容
interface Person {
  name: string;
  // 使用 "?" 标识，表明该属性是一个非必需属性
  age?: number;
  // 函数
  move(): void;
}

// 实现 "Person" 接口的对象，当它具备 "name" 属性和 "move" 方法之后可被视为一个 "Person"
let p: Person = { name: 'Bobby', move: () => {} };
// 带可选属性的对象
let validPerson: Person = { name: 'Bobby', age: 42, move: () => {} };
// 由于该对象 "age" 属性的类型不是 "number" ，所以这不是一个 "Person"
let invalidPerson: Person = { name: 'Bobby', age: true };

// 接口同样可以描述一个函数的类型
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// 参数名并不重要，参数类型才是最重要的
let mySearch: SearchFunc;
mySearch = function(src: string, sub: string) {
  return src.search(sub) !== -1;
};

// 类 - 成员访问权限默认都是公共的 (public)
class Point {
  // 成员属性
  x: number;

  // 构造器 - 在构造器中使用 public/private 关键字修饰的变量，会被声明为类的成员属性。
  // 下面这个例子中，y 会像 x 一样被声明定义为类成员属性，而不再需要额外代码
  // 声明时，同样支持指定默认值

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // 成员函数
  dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // 静态成员
  static origin = new Point(0, 0);
}

let p1 = new Point(10 , 20);
let p2 = new Point(25); // y 为构造器中指定的默认值：0

// 继承
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // 必须显式调用父类的构造器
  }

  // 重写父类中的 dist() 函数
  dist() {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// 模块, "." 符号可以作为子模块的分隔符
module Geometry {
  export class Square {
    constructor(public sideLength: number = 0) {
    }
    area() {
      return Math.pow(this.sideLength, 2);
    }
  }
}

let s1 = new Geometry.Square(5);

// 为模块创建一个本地别名
import G = Geometry;

let s2 = new G.Square(10);

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
let pairToTuple = function<T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: 'hello', item2: 'world'});

// 引用定义文件
/// <reference path="jquery.d.ts" />

// 模板字符串(使用反引号的字符串)
// 嵌入变量的模板字符串
let name = 'Tyrone';
let greeting = `Hi ${name}, how are you?`;
// 有多行内容的模板字符串
let multiline = `This is an example
of a multiline string`;

```

## 参考资料
 * [TypeScript官网](http://www.typescriptlang.org/)
 * [TypeScript语言规范说明书(pdf)](http://go.microsoft.com/fwlink/?LinkId=267238)
 * [Anders Hejlsberg - TypeScript介绍](http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
 * [GitHub源码](https://github.com/Microsoft/TypeScript)
 * [Definitely Typed - 类型定义仓库](http://definitelytyped.org/)
