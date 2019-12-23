---
language: less
contributors:
  - ["Saravanan Ganesh", "http://srrvnn.me"]
translators:
    - ["Thanh Duy Phan", "https://github.com/thanhpd"]
filename: learnless-vi.less
lang: vi-vn
---

Less là một CSS pre-processor (bộ tiền xử lí CSS), nó thêm các tính năng như biến (variable), lồng (nesting), mixin và nhiều thứ khác. Less cùng với các CSS pre-processor khác như [Sass](http://sass-lang.com/) giúp lập trình viên viết được các đoạn CSS bảo trì được và không bị lặp lại (DRY - Don't Repeat Yourself).

```css


// Comment (chú thích) một dòng sẽ bị xóa khi Less được biên dịch thành CSS

/* Comment trên nhiều dòng sẽ được giữ lại */



/* Biến
==============================*/


/* Ta có thể lưu giá trị CSS (ví dụ như color) vào một biến.
   Sử dụng ký hiệu '@' để khai báo một biến. */

@primary-color: #a3a4ff;
@secondary-color: #51527f;
@body-font: 'Roboto', sans-serif;

/* Sau khi khai báo biến, ta có thể sử dụng nó ở trong tệp stylesheet.
   Nhờ sử dụng biến ta chỉ cần thay đổi một lần
   tại 1 nơi để thay đổi tất cả những đoạn sử dụng biến */

body {
	background-color: @primary-color;
	color: @secondary-color;
	font-family: @body-font;
}

/* Đoạn code trên sẽ được biên dịch thành: */

body {
	background-color: #a3a4ff;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}


/* Cách sử dụng này giúp ta dễ dàng bảo trì hơn
   việc phải đổi giá trị mỗi lần nó xuất hiện
   trong tệp stylesheet. */



/* Mixins
==============================*/


/* Nếu đang viết một đoạn code cho nhiều hơn một
   element, ta có thể sử dụng lại nó dễ dàng. */

.center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Ta có thể dùng mixin chỉ bằng việc thêm selector
   vào trong nội dung style của element khác */

div {
	.center;
	background-color: @primary-color;
}

/* Đoạn code trên sẽ được biên dịch thành: */

.center {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #a3a4ff;
}

/* Ta có thể ngăn không cho code mixin được biên dịch
   bằng cách thêm cặp ngoặc tròn đằng sau selector */

.center() {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
}

div {
  .center;
  background-color: @primary-color;
}

/* Đoạn code trên sẽ được biên dịch thành: */
div {
  display: block;
  margin-left: auto;
  margin-right: auto;
  left: 0;
  right: 0;
  background-color: #a3a4ff;
}



/* Nesting - Lồng
==============================*/


/* Less cho phép ta có thể lồng selector bên trong selector */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #f00;
	}
}

/* Selector bắt đầu bằng ký tự '&' sẽ thay thế ký tự '&'
   với selector cha. */
/* Ta cũng có thể lồng các pseudo-class với nhau */
/* Nên lưu ý không nên lồng quá nhiều lần sẽ làm code kém tính bảo trì.
   Kinh nghiệm cho thấy không nên lồng quá 3 lần.
   Ví dụ: */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: red;

		&:hover {
		  background-color: blue;
		}

		a {
		  color: white;
		}
	}
}

/* Biên dịch thành: */

ul {
  list-style-type: none;
  margin-top: 2em;
}

ul li {
  background-color: red;
}

ul li:hover {
  background-color: blue;
}

ul li a {
  color: white;
}



/* Function
==============================*/


/* Less cung cấp các function có thể được dùng để hoàn thành
   các công việc khác nhau. */

/* Function được gọi sử dụng tên của nó và truyền vào
   các tham số được yêu cầu. */

body {
  width: round(10.25px);
}

.header {
	background-color: lighten(#000, 0.5);
}

.footer {
  background-color: fadeout(#000, 0.25)
}

/* Biên dịch thành: */

body {
  width: 10px;
}

.header {
  background-color: #010101;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* Ta có thể định nghĩa function mới.
   Function khá tương tự với mixin bởi chúng đều có thể được tái
   sử dụng. Khi lựa chọn giữa việc sử dụng function hay mixin,
   hãy nhớ mixin được tối ưu cho việc tạo ra CSS trong khi
   function sẽ được sử dụng tốt hơn cho logic sẽ được sử dụng
   xuyên suốt Less code. Các ví dụ trong phần 'Toán tử' là ứng cử viên
   sáng giá cho việc dùng function có thể tái sử dụng được.
*/

/* Function này tính giá trị trung bình của hai số: */
.average(@x, @y) {
  @average-result: ((@x + @y) / 2);
}

div {
  .average(16px, 50px); // gọi mixin
  padding: @average-result;    // sử dụng giá trị trả về của mixin
}

/* Biên dịch thành: */

div {
  padding: 33px;
}



/* Mở rộng (Thừa kế)
==============================*/


/* Mở rộng là cách để chia sẻ thuộc tính của một selector cho selector khác */

.display {
  height: 50px;
}

.display-success {
  &:extend(.display);
	border-color: #22df56;
}

/* Biên dịch thành: */
.display,
.display-success {
  height: 50px;
}
.display-success {
  border-color: #22df56;
}

/* Nên mở rộng một khai báo CSS có trước thay vì tạo một mixin mới
   bởi cách nó nhóm các lớp có chung một style gốc.
   Nếu thực hiện với mixin, các thuộc tính sẽ bị trùng lặp
   cho mỗi khai báo có sử dụng mixin. Mặc dù không ảnh hưởng đến luồng công việc nhưng nó
   tạo ra các đoạn code CSS thừa sau khi được biên dịch.
*/


/* Partials and Imports - Chia nhỏ và nhập vào
==============================*/


/* Less cho phép ta tạo các partial file (tệp con).
   Sử dụng nó giúp ta có thể tổ chức code Less theo mô-đun có hệ thống.
   Các tệp con thường bắt đầu với ký tự gạch dưới '_', vd: _reset.less
   và được nhập vào file Less chính để được biên dịch thành CSS */

/* Quan sát ví dụ sau, ta sẽ đặt đoạn code dưới đây vào tệp tên là _reset.less */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Less cung cấp cú pháp @import cho phép nhập các partial vào một file.
   Cú pháp này trong Less sẽ nhập các file và kết hợp chúng lại với
   code CSS được sinh ra. Nó khác với cú pháp @import của CSS,
   bản chất là tạo một HTTP request mới để tải về tệp tin được yêu cầu. */

@import 'reset';

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}

/* Biên dịch thành: */

html, body, ul, ol {
  margin: 0;
  padding: 0;
}

body {
  font-size: 16px;
  font-family: Helvetica, Arial, Sans-serif;
}



/* Toán học
==============================*/


/* Less cung cấp các toán tử sau: +, -, *, / và %.
   Điều này rất có ích cho việc tính toán giá trị trực tiếp
   trong tệp Less thay vì phải tính toán thủ công.
   Dưới đây là ví dụ về việc tạo một khung thiết kế đơn giản có hai cột. */

@content-area: 960px;
@main-content: 600px;
@sidebar-content: 300px;

@main-size: @main-content / @content-area * 100%;
@sidebar-size: @sidebar-content / @content-area * 100%;
@gutter: 100% - (@main-size + @sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: @main-size;
}

.sidebar {
  width: @sidebar-size;
}

.gutter {
  width: @gutter;
}

/* Biên dịch thành: */

body {
  width: 100%;
}

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}

.gutter {
  width: 6.25%;
}


```

## Tập sử dụng Less

Nếu bạn cần xài thử Less trên trình duyệt, hãy ghé qua:
* [Codepen](http://codepen.io/)
* [LESS2CSS](http://lesscss.org/less-preview/)

## Tính tương thích

Less có thể được dùng trong bất kì dự án nào miễn là ta có chương trình để biên dịch nó thành CSS. Ta cần chắc chắn rằng đoạn CSS đang dùng tương thích với các phiên bản trình duyệt mong muốn.

[QuirksMode CSS](http://www.quirksmode.org/css/) và [CanIUse](http://caniuse.com) là nguồn thông tin tin cậy để kiểm tra tính tương thích của mã CSS.

## Tìm hiểu thêm
* [Tài liệu chính thức](http://lesscss.org/features/)
* [Less CSS - Hướng dẫn cho người mới bắt đầu](http://www.hongkiat.com/blog/less-basic/)