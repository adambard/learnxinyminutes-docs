---
language: html
filename: learnhtml-vi.html
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Robert Steed", "https://github.com/robochat"]
    - ["Xuan (Sean) Luong", "https://github.com/xuanluong"]
lang: vi-vn
---

HTML là viết tắt của HyperText Markup Language (Ngôn ngữ đánh dấu siêu văn bản).
Nó là một ngôn ngữ cho phép chúng ta viết nên những trang web.
Nó là một ngôn ngữ đánh dấu, cho phép chúng ta viết trang web bằng code để chỉ định cách thức mà văn bản và dữ liệu nên được trình bày.
Tập tin html thực chất chỉ là một tập tin văn bản đơn giản.
Đánh dấu có nghĩa là gì? Nó là một phương pháp tổ chức dữ liệu của một trang web bằng cách bao quanh dữ liệu bởi các thẻ (tags) mở và đóng.
Việc đánh dấu phục vụ mục đích cung cấp tầm quan trọng của phần văn bản mà nó bao quanh.
Cũng như các ngôn ngữ máy tính khác, HTML có nhiều phiên bản. Ở đây chúng ta nói về HTML5.

**Lưu ý :**  Bạn có thể thử nghiệm những thẻ và phần tử HTML khác nhau trong quá trình đọc bài viết bằng cách truy cập những trang web như [codepen](http://codepen.io/pen/) để có thể thấy được tác dụng của những thẻ hay phần tử HTML đó,
nhằm hiểu cách chúng hoạt động và làm quen với ngôn ngữ HTML.
Bài viết này chủ yếu bàn về cú pháp của HTML và một vài mẹo hữu dụng.


```html
<!-- Bình luận được bao quanh bởi các ký tự giống như trong ví dụ này -->

<!-- #################### Các thẻ #################### -->
   
<!-- Dưới đây là tập tin HTML ví dụ mà chúng ta sẽ phân tích. -->

<!doctype html>
	<html>
		<head>
			<title>Trang web của tôi</title>
		</head>
		<body>
			<h1>Xin chào!</h1>
			<a href = "http://codepen.io/anon/pen/xwjLbZ">Truy cập để biết cái gì sẽ được hiển thị</a>
			<p>Đây là một văn bản.</p>
			<p>Một văn bản khác.</p>
			<ul>
				<li>Đây là một danh sách không liệt kê</li>
				<li>Đây là một danh sách không liệt kê khác</li>
				<li>Danh sách không liệt kê cuối cùng của danh sách cha</li>
			</ul>
		</body>
	</html>

<!-- Một tập tin HTML luôn bắt đầu bằng việc thể hiện cho trình duyệt rằng nó là một trang HTML -->
<!doctype html>

<!-- Sau đó nó bắt đầu với một thẻ <html> mở -->
<html>

<!-- thẻ đó sẽ được đóng vào cuối tập tin bằng một thẻ đóng </html>. -->
</html>

<!-- Không nên viết gì sau thể đóng cuối cùng này. -->

<!-- Ở bên trong (giữa thẻ đóng và mở <html></html>), ta tìm thấy: -->

<!-- Phần đầu được định nghĩa bằng <head> (và phải được đóng lại bằng </head>). -->
<!-- Phần đầu chứa một vài định nghĩa và những thông tin khác không được dùng để hiển thị; đây gọi là siêu dữ liệu (metadata) -->

<head>
	<title>Trang web của tôi</title><!-- Thẻ <title> cho trình duyệt biết dòng chữ để hiển thị trên thanh tựa đề của trình duyệt vả tên tab. -->
</head>

<!-- Sau phần <head> ta sẽ gặp thẻ <body> -->
<!-- Cho tới đây, chưa có gì được hiển thị trên cửa sổ trình duyệt.  -->
<!-- Chúng ta phải đưa nội dùng vào phần <body> để hiển thị. -->

<body>
	<h1>Xin chào!</h1> <!-- Thẻ h1 tạo ra một đề mục. -->
	<!-- Ngoài <h1> ra ta còn có những đề mục cấp thấp hơn, từ h2 đến h6 -->
	<a href = "http://codepen.io/anon/pen/xwjLbZ">Truy cập để biết cái gì sẽ được hiển thị</a> <!-- một liên kết đền một url được cung cấp bởi thuộc tính href="" -->
	<p>Đây là một văn bản.</p> <!-- Thẻ <p> cho phép đưa văn bản vào trang html. -->
	<p>Một văn bản khác.</p>
	<ul> <!-- Thẻ <ul> tạo ra danh sách không đánh s. -->
	<!-- Để có một danh sách có đánh số ta dùng thể <ol> thay vì <ul> từ đó sẽ có số thứ tự  1. cho phần tử đầu tiên, 2. cho phần tử thứ hai, v.v... -->
		<li>Đây là một danh sách không liệt kê</li>
		<li>Đây là một danh sách không liệt kê khác</li>
		<li>Danh sách không liệt kê cuối cùng của danh sách cha</li>
	</ul>
</body>

<!-- Và ta đã có một tập tim HTML. Việc tạo ra tập tin HTML có thể được thực hiện một cách đơn giản. -->

<!-- Những ta cũng có thể thêm vào những loại thẻ HTML khác. -->

<!-- Chèn vào một ảnh. -->
<img src="http://i.imgur.com/XWG0O.gif"/> <!-- Nguồn của ảnh sẽ được khai báo qua thuộc tính src="" -->
<!-- Nguồn ảnh có thể là một URL hoặc đường dẫn tới một tập tin trong máy. -->

<!-- Ta cũng có thể tạo ra một table. -->

<table> <!-- Tạo bảng với thẻ <table>. -->
	<tr> <!-- <tr> dùng để tạo ra một hàng. -->
		<th>Cột một</th> <!-- <th> dùng để khai báo tên cột. -->
		<th>Cột hai</th>
	</tr>
	<tr>
		<td>hàng một, cột một</td> <!-- <td> dùng để tạo ra một ô trong table. -->
		<td>hàng một, cột hai</td>
	</tr>
	<tr>
		<td>hàng hai, cột một</td>
		<td>hàng hai, cột hai</td>
	</tr>
</table>

```

## Cách sử dụng

HTML được viết trong tập tin có phần mở rộng `.html`.

## Thông tin mở rộng

* [wikipedia](https://vi.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
