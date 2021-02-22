---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
    - ["Jacob Ward", "http://github.com/JacobCWard/"]
translators:
    - ["Thanh Duy Phan", "https://github.com/thanhpd"]
filename: markdown-vi.md
lang: vi-vn
---


Ngôn ngữ Markdown được sáng lập bởi John Gruber vào năm 2004. Nó được tạo ra với mục đích dễ đọc với cú pháp có thể được dễ dàng chuyển đổi qua HTML và các ngôn ngữ khác

Markdown có sự khác biệt trong cách cài đặt giữa các trình phân tích cú pháp. Hướng dẫn này sẽ đề cập, giải thích tới nếu tính năng có thể được sử dụng chung hay nó chỉ áp dụng cho một trình phân tích riêng biệt.

- [Phần tử HTML](#html-elements)
- [Đầu mục](#headings)
- [Định dạng văn bản](#simple-text-styles)
- [Đoạn văn](#paragraphs)
- [Danh sách](#lists)
- [Khối code](#code-blocks)
- [Đường kẻ ngang](#horizontal-rule)
- [Liên kết](#links)
- [Ảnh](#images)
- [Khác](#miscellany)

## Phần tử HTML
Markdown là tập cha của HTML, vì vậy bất cứ file HTML nào đều là Markdown đúng.

```md
<!-- Điều này đồng nghĩa ta có thể sử dụng các phần tử HTML
trong Markdown, ví dụ như phần tử chú thích/comment.
Tuy nhiên, nếu sử dụng một phần tử HTML trong file Markdown,
ta không thể sử dụng cú pháp Markdown cho nội dung bên trong phần tử đó. -->
```

## Đầu mục

Ta có thể tạo các phần tử đầu mục HTML từ `<h1>` cho đến `<h6>` dễ dàng
bằng cách thêm số lượng dấu thăng (#) đằng trước chuỗi cần tạo đầu mục.

```md
# Đây là đầu mục <h1>
## Đây là đầu mục <h2>
### Đây là đầu mục <h3>
#### Đây là đầu mục <h4>
##### Đây là đầu mục <h5>
###### Đây là đầu mục <h6>
```
Markdown còn cung cấp cách khác để tạo đầu mục hạng nhất h1 và hạng nhì h2.

```md
Đây là đầu mục h1
=============

Đây là đầu mục h2
-------------
```

## Định dạng văn bản

Văn bản có thể được định dạng dễ dàng như in nghiêng hay làm đậm sử dụng Markdown.

```md
*Đoạn văn bản này được in nghiêng.*
_Và đoạn này cũng như vậy._

**Đoạn văn bản này được in đậm.**
__Và đoạn này cũng vậy.__

***Đoạn văn bản này được in nghiêng và đậm.***
**_Cách này cũng tương tự_**
*__Và cách này nữa__*
```

Trong cài đặt Markdown để hiển thị file của GitHub,ta còn có gạch ngang:

```md
~~Đoạn văn bản này được gạch ngang.~~
```
## Đoạn văn

Đoạn văn bao gồm một hay nhiều dòng văn bản liên tiếp nhau được phân cách
bởi một hay nhiều dòng trống.

```md
Đây là đoạn văn thứ nhất.

Đây là đoạn văn thứ hai.
Dòng này vẫn thuộc đoạn văn thứ hai, do không có cách dòng.


Đây là đoạn văn thứ ba.
```

Nếu cần chèn thêm thẻ ngắt dòng `<br />` của HTML, ta có thể kết thúc đoạn văn bản
bằng cách thêm vào từ 2 dấu cách (space) trở lên và bắt đầu đoạn văn bản mới.

```md
Dòng này kết thúc với 2 dấu cách (highlight để nhìn thấy).

Có phần tử <br /> ở bên trên.
```

Khối trích dẫn được sử dụng với kí tự >

```md
> Đây là khối trích dẫn. Ta có thể
> ngắt dòng thủ công và thêm kí tự `>` trước mỗi dòng hoặc ta có thể để dòng tự ngắt nếu cần thiệt khi quá dài.
> Không có sự khác biệt nào, chỉ cần nó bắt đầu với kí tự `>`

> Ta còn có thể dùng nhiều mức
>> của khối trích dẫn.
> Như vậy có tốt không?

```

## Danh sách

Danh sách không có thứ tự có thể được tạo sử dụng dấu sao, dấu cộng hay dấu trừ đầu dòng.

```md
* Một mục
* Một mục
* Một mục nữa

hoặc

+ Một mục
+ Một mục
+ Một mục khác

hay

- Một mục
- Một mục
- Một mục sau
```

Danh sách có thứ tự được tạo bởi một số theo sau bằng một dấu chấm.

```md
1. Mục thứ nhất
2. Mục thứ hai
3. Mục thứ ba
```

Ta không nhất thiết phải điền số thứ thự cho chỉ mục đúng mà Markdown sẽ tự hiển thị danh sách theo thứ tự đã được sắp xếp, tuy nhiên cách làm này không tốt!

```md
1. Mục thứ nhất
1. Mục thứ hai
1. Mục thứ ba
```
(Sẽ hiển thị như ví dụ trước đó)

Ta còn có thể sử dụng danh sách con

```md
1. Mục thứ nhất
2. Mục thứ hai
3. Mục thứ ba
    * Mục nhỏ
    * Mục nhỏ
4. Mục thứ tư
```

Markdown còn cung cấp danh mục (checklist). Nó sẽ hiển thị ra hộp đánh dấu dạng HTML.

```md
Boxes below without the 'x' are unchecked HTML checkboxes.
- [ ] First task to complete.
- [ ] Second task that needs done
This checkbox below will be a checked HTML checkbox.
- [x] This task has been completed
```

## Khối code

Ta có thể đánh dấu một đoạn code (tương tự sử dụng phần tử HTML `<code>`) bằng việc thụt đầu dòng sử dụng bốn dấu cách (space) hoặc một dấu nhảy (tab)

```md
    This is code
    So is this
```

Ta còn có thể thêm dấu nhảy (hoặc thêm vào bốn dấu cách nữa) để căn chỉnh phần bên trong đoạn code

```md
    my_array.each do |item|
        puts item
    end
```

Code hiển thị cùng dòng có thể được đánh dấu sử dụng cặp ``.

```md
John didn't even know what the `go_to()` function did!
```

Trong Markdown của GitHub, ta còn có thêm cách để hiển thị code:

<pre>
<code class="highlight">&#x60;&#x60;&#x60;ruby
def foobar
    puts "Hello world!"
end
&#x60;&#x60;&#x60;</code></pre>

The above text doesn't require indenting, plus GitHub will use syntax
highlighting of the language you specify after the \`\`\`
Đoạn trên không cần sử dụng thụt đầu dòng, và GitHub sẽ tô sáng cú pháp sử dụng ngôn ngữ mà ta cung cấp sau đoạn kí tự \`\`\`

## Kẻ ngang

Dòng kẻ ngang (`<hr />`) có thể được thêm vào dễ dàng sử dụng từ 3 kí tự sao (*) hoặc gạch ngang (-), không quan trọng có khoảng cách giữa các kí tự hay không.


```md
***
---
- - -
****************
```

## Liên kết

Một trong những thứ tốt nhất khi làm việc với Markdown là khả năng tạo liên kết hết sức dễ dàng. Đoạn text hiển thị được đóng trong cặp ngoặc vuông [] kèm theo đường dẫn url trong cặp ngoặc tròn ().

```md
[Click me!](http://test.com/)
```
Ta còn có thể tạo tiêu đề cho liên kết sử dụng cặp ngoặc nháy bên trong cặp ngoặc tròn

```md
[Click me!](http://test.com/ "Link to Test.com")
```
Đường dẫn tương đối cũng hoạt động.

```md
[Go to music](/music/).
```

Markdown còn hỗ trợ liên kết kiểu tham chiếu.

<pre><code class="highlight">&#x5b;<span class="nv">Nhấn vào đây</span>][<span class="ss">link1</span>] để xem thêm!
&#x5b;<span class="nv">Ngoài ra nhấn vào đây</span>][<span class="ss">foobar</span>] nếu bạn muốn xem qua.

&#x5b;<span class="nv">link1</span>]: <span class="sx">http://test.com/</span> <span class="nn">"Tuyệt!"</span>
&#x5b;<span class="nv">foobar</span>]: <span class="sx">http://foobar.biz/</span> <span class="nn">"Tốt!"</span></code></pre>

Tiêu đề có thể được đóng trong dấu nháy hay ngoặc đơn, hoặc có thể được bỏ qua. Tham chiếu có thể được đặt bất kì đâu trong văn bản và ID của tham chiếu có thể là bất kì gì miễn là nó độc nhất.

Ngoài ra còn có kiểu đặt tên ngầm cho phép ta sử dụng đường dẫn làm ID.

<pre><code class="highlight">&#x5b;<span class="nv">This</span>][] is a link.

&#x5b;<span class="nv">this</span>]: <span class="sx">http://thisisalink.com/</span></code></pre>

Nhưng nó không được sử dụng rộng rãi.

## Ảnh

Hiển thị ảnh tương tự như liên kết nhưng có thêm dấu chấm than đằng trước

```md
![Thuộc tính alt cho ảnh](http://imgur.com/myimage.jpg "Tiêu đề tùy chọn")
```

Và kiểu tham chiếu cũng hoạt động như vậy.

<pre><code class="highlight">!&#x5b;<span class="nv">Đây là thuộc tính alt.</span>][<span class="ss">myimage</span>]

&#x5b;<span class="nv">myimage</span>]: <span class="sx">relative/urls/cool/image.jpg</span> <span class="nn">"Đây là tiêu đề"</span></code></pre>

## Khác

### Tự động đặt liên kết

```md
<http://testwebsite.com/> tương đương với
[http://testwebsite.com/](http://testwebsite.com/)
```

### Tự động đặt liên kết cho email

```md
<foo@bar.com>
```

### Hiển thị Kí tự đặc biệt

```md
Khi ta muốn viết *đoạn văn bản này có dấu sao bao quanh* nhưng ta không muốn nó bị in nghiêng, ta có thể sử dụng: \*đoạn văn bản này có dấu sao bao quanh\*.
```

### Phím bàn phím

Trong Markdown của Github, ta có thể sử dụng thẻ `<kbd>` để thay cho phím trên bàn phím.

```md
Máy treo? Thử bấm tổ hợp
<kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Del</kbd>
```
### Bảng biểu

Bảng biểu được hỗ trợ trên Markdown của GitHub, Jira, Trello, v.v và khá khó viết:

```md
| Cột 1        | Cột2     | Cột 3         |
| :----------- | :------: | ------------: |
| Căn trái     | Căn giữa | Căn phải      |
| blah         | blah     | blah          |
```
Hoặc có thể sử dụng kết quả dưới đây

```md
Cột 1 | Cột 2 | Cột 3
:-- | :-: | --:
blah | blah | blah
```

---
Để biết thêm thông tin, hãy ghé qua hướng dẫn chính thức về cú pháp của John Gruber [tại đây](http://daringfireball.net/projects/markdown/syntax) và cheatsheet của Adam Pritchard [tại đây](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet).
