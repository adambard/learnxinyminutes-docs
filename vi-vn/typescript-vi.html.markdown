---
language: TypeScript
contributors:
    - ["Philippe Vlérick", "https://github.com/pvlerick"]
translators:
    - ["Thanh Duy Phan", "https://github.com/thanhpd"]
filename: learntypescript-vi.ts
lang: vi-vn
---

TypeScript là ngôn ngữ được viết nhằm tinh giản quá trình phát triển ứng dụng quy mô lớn được viết bằng JavaScript.
TypeScript bổ sung thêm các khái niệm phổ biến như Class, Module, Interface, Generic và Static typing (tùy chọn) vào JavaScript.
Ngôn ngữ này là tập lớn hơn của JavaScript: tất cả code JavaScript đều là code TypeScript đúng nên nó có thể được thêm vào các dự án một cách nhanh chóng. Trình biên dịch TypeScript sẽ sinh ra JavaScript.

Bài viết này sẽ chỉ tập trung tới các cú pháp bổ sung mà TypeScript thêm vào thay vì nói đến cả các cú pháp [JavaScript](javascript-vi.html.markdown).

Để thử dùng TypeScript với trình biên dịch, đi đến [Sân chơi TypeScript](http://www.typescriptlang.org/play) nơi mà bạn có thể nhập code, sử dụng chức năng hỗ trợ tự hoàn thành code - autocompletion và trực tiếp quan sát mã JavaScript được sinh ra.

```ts
// Đây là 3 khai báo kiểu biến cơ bản trong TypeScript
// (JavaScript chỉ có kiểu của giá trị, không có kiểu của biến)
let isDone: boolean = false;
let lines: number = 42;
let name: string = "Anders";

// Bạn có thể bỏ khai báo kiểu của biến nếu như nó đã được suy ra từ kiểu giá trị cơ bản
let isDone = false;
let lines = 42;
let name = "Anders";

// Có kiểu biến "any" tương thích với mọi kiểu của biến,
// được dùng khi ta không chắc chắn về kiểu của biến khi được khai báo
let notSure: any = 4;
notSure = "có thể là một biến kiểu string";
notSure = false; // cũng có thể là biến kiểu boolean

// Dùng từ khóa const cho khái báo biến không thay đổi (constant variable)
const numLivesForCat = 9;
numLivesForCat = 1; // Có lỗi!

// Khi khai báo tập hợp ta có thể dùng mảng có kiểu được khai báo trước - typed array
let list: number[] = [1, 2, 3];
// Ta cũng có thể sử dụng mảng kiểu chung - generic array
let list: Array<number> = [1, 2, 3];

// Để dùng enumeration - danh sách của một tập hợp:
enum Color { Red, Green, Blue };
let c: Color = Color.Green;

// Nếu function không trả về kết quả, sử dụng "void" cho kết quả trả về
function bigHorribleAlert(): void {
  alert("I'm a little annoying box!");
}

// Function trong TypeScript là first-class citizen (tạm dịch: phần tử hạng nhất), hỗ trợ thao tác tới các thực thể khác
// (vd: truyền vào như tham số, được trả về từ function, chỉnh sửa, gán vào một biến)
// TypeScript hỗ trợ sử dụng function với cú pháp lambda (mũi tên) và suy luận kiểu trả về

// Các cú pháp dưới đây tương đương với nhau,
// trình biên dịch sẽ tự nhận biết và sinh ra mã JavaScript giống nhau
let f1 = function (i: number): number { return i * i; }
// Kiểu trả về nếu không khai báo được tự suy diễn
let f2 = function (i: number) { return i * i; }
// Cú pháp mũi tên (arrow syntax)
let f3 = (i: number): number => { return i * i; }
// Cú pháp mũi tên với kiểu trả về được suy diễn
let f4 = (i: number) => { return i * i; }
// Cú pháp mũi tên với kiểu trả về được suy diễn
// khi không sử dụng dấu ngoặc nhọn {} thì không cần sử dụng return
let f5 = (i: number) => i * i;

// Interface mang tính cấu trúc, mọi thứ có các đặc điểm (property) đều tương thích
interface IPerson {
  name: string;
  // Đặc điểm có thể tùy chọn bằng sử dụng dấu "?"
  age?: number;
  // Có thể sử dụng function
  move(): void;
}

// Object sử dụng interface IPerson nói trên
// có thể được coi là 1 thực thể Person vì nó có đặc điểm name và chức năng move
let p: Person = { name: "Bobby", move: () => { } };
// Object sử dụng property tùy chọn
let validPerson: Person = { name: "Bobby", age: 42, move: () => { } };
// Khai báo dưới đây gây lỗi vì giá trị đặc điểm age không mang kiểu number
let invalidPerson: Person = { name: "Bobby", age: true };

// Interface cũng có thể mô tả đặc tả của function
interface SearchFunc {
  (source: string, subString: string): boolean;
}
// Chỉ có kiểu của tham số là quan trọng còn tên không quan trọng
let mySearch: SearchFunc;
mySearch = function (src: string, sub: string) {
  return src.search(sub) != -1;
}

// Class - các khai báo mặc định là public
class Point {
  // Property
  x: number;

  // Constructor - sử dụng tham số với từ khóa public/private
  // sẽ tạo ra property tương ứng (ví dụ với property y)
  // Có thể khai báo giá trị mặc định

  constructor(x: number, public y: number = 0) {
    this.x = x;
  }

  // Function
  dist() { return Math.sqrt(this.x * this.x + this.y * this.y); }

  // Biến Static
  static origin = new Point(0, 0);
}

let p1 = new Point(10, 20);
let p2 = new Point(25); // y sử dụng giá trị mặc định là 0

// Thừa kế - Inheritance
class Point3D extends Point {
  constructor(x: number, y: number, public z: number = 0) {
    super(x, y); // Bắt buộc phải gọi constructor của class cha
  }

  // Overwrite/Polymorphism - Ghi đè/Đa hình
  dist() {
    let d = super.dist();
    return Math.sqrt(d * d + this.z * this.z);
  }
}

// module, "." có thể được dùng như những module con
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

// Bí danh (alias) có thể được sử dụng để tham vấn module khác
import G = Geometry;

let s2 = new G.Square(10);

// Generic
// Class
class Tuple<T1, T2> {
  constructor(public item1: T1, public item2: T2) {
  }
}

// Interface
interface Pair<T> {
  item1: T;
  item2: T;
}

// Function
let pairToTuple = function <T>(p: Pair<T>) {
  return new Tuple(p.item1, p.item2);
};

let tuple = pairToTuple({ item1: "hello", item2: "world" });

// Các thư viện viết bằng JavaScript thường đi kèm file định nghĩa kiểu để có thể sử dụng cho TypeScript
// Thêm vào tham vấn tới file định nghĩa:
/// <reference path="jquery.d.ts" />

// Template Strings - Chuỗi dạng mẫu (string sử dụng dấu `)
// String Interpolation - Nội suy chuỗi with với template string
let name = 'Tyrone';
let greeting = `Chào ${name}, bạn khỏe không?`
// Chuỗi nhiều dòng với template string
let multiline = `Đây là ví dụ
cho chuỗi nhiều dòng`;

```

## Tìm hiểu thêm

* [Website TypeScript chính thức](http://www.typescriptlang.org/)
* [Đặc tả ngôn ngữ TypeScript] (https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md)
* [Anders Hejlsberg - Introducing TypeScript on Channel 9] (http://channel9.msdn.com/posts/Anders-Hejlsberg-Introducing-TypeScript)
* [Mã nguồn trên GitHub] (https://github.com/Microsoft/TypeScript)
* [Definitely Typed - repository for type definitions] (http://definitelytyped.org/)
