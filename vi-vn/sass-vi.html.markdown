---
language: sass
filename: learnsass-vi.scss
contributors:
  - ["Laura Kyle", "https://github.com/LauraNK"]
  - ["Sean Corrales", "https://github.com/droidenator"]
  - ["Kyle Mendes", "https://github.com/pink401k"]
  - ["Keith Miyake", "https://github.com/kaymmm"]
translators:
    - ["Thanh Duy Phan", "https://github.com/thanhpd"]
lang: vi-vn
---

Less là một ngôn ngữ mở rộng CSS/ CSS pre-processor, thêm các tính năng như biến (variable), lồng (nesting), mixin và nhiều thứ khác. Sass cùng với các CSS pre-processor khác như [Less](http://lesscss.org/) giúp lập trình viên viết được các đoạn CSS bảo trì được và không bị lặp lại (DRY - Don't Repeat Yourself).

Sass có hai lựa chọn sử dụng cú pháp khác nhau. Một là SCSS, sử dụng cú pháp giống như CSS nhưng bổ sung thêm các tính năng của Sass. Hai là Sass (cú pháp nguyên bản), sử dụng thụt đầu dòng - indention thay vì ngoặc nhọn và dấu chấm phẩy.
Bài hướng dẫn này sử dụng SCSS.

Nếu bạn đọc đã quen thuộc với CSS3 thì sẽ tương đối nhanh chóng để nắm được Sass. Nó không cung cấp thuộc tính để style CSS mới nhưng đưa ra những công cụ để có thể viết CSS hiệu quả hơn và có thể bảo trì dễ dàng hơn.

```sass


// Comment (chú thích) một dòng sẽ bị xóa khi Less được biên dịch thành CSS

/* Comment trên nhiều dòng sẽ được giữ lại */



/* Variable - Biến
============================== */



/* Ta có thể lưu giá trị CSS (ví dụ như color) vào một biến.
   Sử dụng ký hiệu '$' để khai báo một biến. */

$primary-color: #A3A4FF;
$secondary-color: #51527F;
$body-font: 'Roboto', sans-serif;

/* Sau khi khai báo biến, ta có thể sử dụng nó ở trong tệp stylesheet.
   Nhờ sử dụng biến ta chỉ cần thay đổi một lần
   tại 1 nơi để thay đổi tất cả những đoạn sử dụng biến */

body {
	background-color: $primary-color;
	color: $secondary-color;
	font-family: $body-font;
}

/* Đoạn code trên sẽ được biên dịch thành: */
body {
	background-color: #A3A4FF;
	color: #51527F;
	font-family: 'Roboto', sans-serif;
}

/* Cách sử dụng này giúp ta dễ dàng bảo trì hơn
   việc phải đổi giá trị mỗi lần nó xuất hiện
   trong tệp stylesheet. */



/* Control Directive - Chỉ thị
============================== */


/* Sass cho phép sử dụng @if, @else, @for, @while và @each để quản lý luồng code sinh ra CSS */

/* Khối điều kiện @if/@else hoạt động như các ngôn ngữ khác */

$debug: true !default;

@mixin debugmode {
	@if $debug {
		@debug "Debug mode enabled";

		display: inline-block;
	}
	@else {
		display: none;
	}
}

.info {
	@include debugmode;
}

/* Trong đoạn code trên, nếu $debug được đặt là true thì class .info sẽ được sinh ra và ngược lại.
   Lưu ý: @debug sẽ sinh ra thông tin debug trên dòng lệnh (command line).
   Chế độ này rất có ích khi thực hiện debug trên file SCSS. */

.info {
	display: inline-block;
}

/* @for là khối vòng lặp trên một khoảng các giá trị.
   Nó rất có ích cho việc đặt style của một tập hợp các phần tử.
   Có hai cách để lặp, "through" sẽ lặp tới kể cả giá trị cuối cùng, "to" sẽ lặp tới và dừng khi đến giá trị cuối cùng. */

// Lặp 3 lần (không kể 4)
@for $c from 1 to 4 {
	div:nth-of-type(#{$c}) {
		left: ($c - 1) * 900 / 3;
	}
}

// Lặp 3 lần (kể cả 3)
@for $c from 1 through 3 {
	.myclass-#{$c} {
		color: rgb($c * 255 / 3, $c * 255 / 3, $c * 255 / 3);
	}
}

/* Biên dịch thành */

div:nth-of-type(1) {
	left: 0;
}

div:nth-of-type(2) {
	left: 300;
}

div:nth-of-type(3) {
	left: 600;
}

.myclass-1 {
	color: #555555;
}

.myclass-2 {
	color: #aaaaaa;
}

.myclass-3 {
	color: white;
// SASS tự động chuyển mã #FFFFFF thành white (trắng)
}

/* Khối lặp @while rất cơ bản: */

$columns: 4;
$column-width: 80px;

@while $columns > 0 {
	.col-#{$columns} {
		width: $column-width;
		left: $column-width * ($columns - 1);
	}

	$columns: $columns - 1;
}

/* Sẽ được biên dịch thành: */

.col-4 {
	width: 80px;
	left: 240px;
}

.col-3 {
	width: 80px;
	left: 160px;
}

.col-2 {
	width: 80px;
	left: 80px;
}

.col-1 {
	width: 80px;
	left: 0px;
}

/* @each hoạt động giống như @for, nhưng sử dụng một danh sách (list) thay vì thứ tự số đếm.
   List được khai báo như những biến khác, sử dụng dấu cách để làm dấu phân cách. */

$social-links: facebook twitter linkedin reddit;

.social-links {
	@each $sm in $social-links {
		.icon-#{$sm} {
			background-image: url("images/#{$sm}.png");
		}
	}
}

/* Sẽ sinh ra: */

.social-links .icon-facebook {
	background-image: url("images/facebook.png");
}

.social-links .icon-twitter {
	background-image: url("images/twitter.png");
}

.social-links .icon-linkedin {
	background-image: url("images/linkedin.png");
}

.social-links .icon-reddit {
	background-image: url("images/reddit.png");
}


/* Mixin
==============================*/

/* Nếu đang viết một đoạn code cho nhiều hơn một
   element, ta có thể sử dụng lại nó dễ dàng.
   Sử dụng cú pháp '@mixin' kèm theo tên để tạo một mixin. */

@mixin center {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
}

/* Ta có thể dùng mixin bằng cú pháp '@include' kèm theo tên của mixin. */

div {
	@include center;
	background-color: $primary-color;
}

/* Được biên dịch thành: */
div {
	display: block;
	margin-left: auto;
	margin-right: auto;
	left: 0;
	right: 0;
	background-color: #A3A4FF;
}

/* Ta có thể dùng mixin để tạo nhanh các thuộc tính. */

@mixin size($width, $height) {
	width: $width;
	height: $height;
}

/* Trong ví dụ này ta có thể tạo nhanh 2 thuộc tính width và height
   bằng cách sử dụng mixin size và truyền vào tham số cho width và height. */

.rectangle {
	@include size(100px, 60px);
}

.square {
	@include size(40px, 40px);
}

/* Biên dịch thành: */
.rectangle {
  width: 100px;
  height: 60px;
}

.square {
  width: 40px;
  height: 40px;
}



/* Function - Hàm
============================== */



/* Less cung cấp các hàm có thể được dùng để hoàn thành
   các công việc khác nhau. */

/* Hàm được gọi sử dụng tên của nó và truyền vào
   các tham số được yêu cầu. */
body {
  width: round(10.25px);
}

.footer {
  background-color: fade_out(#000000, 0.25);
}

/* Biên dịch thành: */

body {
  width: 10px;
}

.footer {
  background-color: rgba(0, 0, 0, 0.75);
}

/* Ta có thể định nghĩa hàm mới.
   hàm khá tương tự với mixin bởi chúng đều có thể được tái
   sử dụng. Khi lựa chọn giữa việc sử dụng hàm hay mixin,
   hãy nhớ mixin được tối ưu cho việc tạo ra CSS trong khi
   hàm sẽ được sử dụng tốt hơn cho logic sẽ được sử dụng
   xuyên suốt Less code. Các ví dụ trong phần 'Toán tử toán học' là ứng cử viên
   sáng giá cho việc dùng hàm có thể tái sử dụng được.
*/

/* Hàm này sẽ tính độ tương đối giữa hai giá trị kích thước. */

@function calculate-percentage($target-size, $parent-size) {
  @return $target-size / $parent-size * 100%;
}

$main-content: calculate-percentage(600px, 960px);

.main-content {
  width: $main-content;
}

.sidebar {
  width: calculate-percentage(300px, 960px);
}

/* Biên dịch thành: */

.main-content {
  width: 62.5%;
}

.sidebar {
  width: 31.25%;
}



/* Mở rộng (Thừa kế)
============================== */



/* Mở rộng là cách để chia sẻ thuộc tính của một selector cho selector khác */

.display {
	@include size(5em, 5em);
	border: 5px solid $secondary-color;
}

.display-success {
	@extend .display;
	border-color: #22df56;
}

/* Biên dịch thành: */
.display, .display-success {
  width: 5em;
  height: 5em;
  border: 5px solid #51527F;
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



/* Nesting - Lồng
============================== */



/* Sass cho phép ta có thể lồng selector bên trong selector */

ul {
	list-style-type: none;
	margin-top: 2em;

	li {
		background-color: #FF0000;
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



/* Partials and Imports - Chia nhỏ thành tệp con và nhập vào
============================== */


/* Less cho phép ta tạo các partial file (tệp con).
   Sử dụng nó giúp ta có thể tổ chức code Less theo mô-đun có hệ thống.
   Các tệp con thường bắt đầu với ký tự gạch dưới '_', vd: _reset.less
   và được nhập vào file Less chính để được biên dịch thành CSS.
   File con không được biên dịch thành file CSS riêng. */

/* Quan sát ví dụ sau, ta sẽ đặt đoạn code dưới đây vào tệp tên là _reset.less */

html,
body,
ul,
ol {
  margin: 0;
  padding: 0;
}

/* Sass cung cấp cú pháp @import cho phép nhập các partial vào một file.
   Cú pháp này trong Sass sẽ nhập các file và kết hợp chúng lại với
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



/* Placeholder Selectors - Selector trống
============================== */



/* Khai báo trống rất hữu dụng khi ta cần tạo một khai báo CSS cần được mở rộng.
   Nếu bạn cần tạo một khai báo CSS gốc cho các lần mở rộng sau ta có thể
   sử dụng một khai báo trống. Khai báo trống bắt đầu với kí tự '$' thay vì
   sử dụng '.' hay '#'. Khai báo trống sẽ không xuất hiện trong code CSS được biên dịch. */

%content-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  @extend %content-window;
  background-color: #0000ff;
}

/* Biên dịch thành: */

.message-window {
  font-size: 14px;
  padding: 10px;
  color: #000;
  border-radius: 4px;
}

.message-window {
  background-color: #0000ff;
}



/* Toán tử toán học
============================== */



/* Sass cung cấp các toán tử sau: +, -, *, / và %.
   Điều này rất có ích cho việc tính toán giá trị trực tiếp
   trong tệp Sass thay vì phải tính toán thủ công.
   Dưới đây là ví dụ về việc tạo một khung thiết kế đơn giản có hai cột. */

$content-area: 960px;
$main-content: 600px;
$sidebar-content: 300px;

$main-size: $main-content / $content-area * 100%;
$sidebar-size: $sidebar-content / $content-area * 100%;
$gutter: 100% - ($main-size + $sidebar-size);

body {
  width: 100%;
}

.main-content {
  width: $main-size;
}

.sidebar {
  width: $sidebar-size;
}

.gutter {
  width: $gutter;
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

## SASS hay Sass?
Bạn đã bao giờ thắc mắc liệu Sass có phải là từ viết tắt hay không? Nhiều nguwòi lầm tưởng nó là từ viết tắt nhưng thực chất tên của ngôn ngữ này lại là một từ - Sass.
Do sự lầm tưởng như vậy và mọi người thường xuyên viết nó là "SASS", người sáng lập ra ngôn ngữ này đã đặt một cái tên hài hước cho nó là "Syntactically Awesome StyleSheets" (Thiết lập style có cú pháp một cách tuyệt vời đáng kinh ngạc).


## Tập sử dụng Sass
Nếu bạn muốn thử dùng Sass trên trình duyệt, hãy ghé qua [SassMeister](http://sassmeister.com/). Bạn có thể dùng cả hai cú pháp, hoặc mở cài đặt và chọn Sass hoặc SCSS.

## Tính tương thích
Sass có thể được dùng trong bất kì dự án nào miễn là ta có chương trình để biên dịch nó thành CSS. Ta cần chắc chắn rằng đoạn CSS đang dùng tương thích với các phiên bản trình duyệt mong muốn.

[QuirksMode CSS](http://www.quirksmode.org/css/) và [CanIUse](http://caniuse.com) là nguồn thông tin tin cậy để kiểm tra tính tương thích của mã CSS.


## Tìm hiểu thêm
* [Tài liệu chính thức](http://sass-lang.com/documentation/file.SASS_REFERENCE.html)
* [The Sass Way](http://thesassway.com/) cung cấp các hướng dẫn từ cơ bản đến nâng cao cùng với các tin tức.
