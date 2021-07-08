---
language: xml
filename: learnxml.xml
contributors:
  - ['João Farias', 'https://github.com/JoaoGFarias']
  - ['Rachel Stiyer', 'https://github.com/rstiyer']
  - ['Deepanshu Utkarsh', 'https://github.com/duci9y']
translators:
  - ['Thanh Duy Phan', 'https://github.com/thanhpd']
lang: vi-vn
---

XML là ngôn ngữ đánh dấu được thiết kế để lưu trữ và truyền tải dữ liệu. Nó có thể được đọc hiểu bởi cả người và máy.

Không giống như HTML, XML không biểu đạt cách hiển thị hoặc định dạng dữ liệu, chỉ chứa dữ liệu mà thôi.

Có những sự khác biệt rõ ràng giữa **nội dung** và **các đánh dấu**. Nói vắn tắt thì nội dung có thể là bất cứ gì trong khi các đánh dấu được định nghĩa trước.

## Một số định nghĩa và giới thiệu

Các XML Document (Văn bản XML) được cấu thành bởi các _elements (phần tử)_ và chúng có thể có các _attributes (thuộc tính)_ để mô tả, đồng thời cũng có thể chứa các nội dung theo ngữ cảnh và một hoặc nhiều phần tử con. Tất cả XML document phải có một phần tử gốc đóng vai trò tổ tiên cho tất cả các phần tử khác trong văn bản.

Các trình phân tích cú pháp XML (XML Parser) được thiết kế để phân tích rất chặt chẽ, và sẽ dừng phân tích các văn bản không đúng định dạng. Vì vậy cần đảm bảo tất cả văn bản XML tuân theo [Các luật cú pháp XML](http://www.w3schools.com/xml/xml_syntax.asp).

```xml
<!-- Đây là một bình luận. Nó không được phép chứa hai dấu gạch ngang (-) liên tiếp -->
<!-- Comments can span
  trải dài nhiều dòngmultiple lines -->

<!-- Element - Phần tử -->
<!-- Một element là thành phần XML cơ bản nhất. Có hai loại, thử nhất là rỗng nô -->
<element1 thuoc-tinh="gia trialue" /> <Các element rỗng không chứa bất kì nội dung gìtent -->
<!-- và không rỗng nội dung: -->
<element2 thuoc-tinh="gia tri">Nội dung</element2>
<!-- Tên của element chỉ được phép chứa chữ cái và chữ số -->

<empty /> <!-- Một element có thể là một element với tag rỗng… -->
<!-- …không chứa bất cứ nội dung gì và chỉ là markup đơn thuần. -->

<notempty> <!-- Hoặc nó chứa một tag bắt đầu… -->
  <!-- …nội dung… -->
</notempty> <!-- và kết thúc với tag đóng. -->

<!-- Tên element phân biệt chữ hoa và thường. -->
<element />
<!-- không giống như -->
<eLEMENT />

<!-- Attribute - Thuộc tính -->
<!-- Một thuộc tính là một cặp key-value và tồn tại bên trong element. -->
<element thuoctinh="giatri" thuoctinhkhac="giatrikhac" nhieugiatri="danhsach phanbiet bangdaucach" />
<!-- Một thuộc tính chỉ xuất hiện một lần trong một element. Nó chỉ chứa một giá trị.
  Một cách giải quyết là sử dụng danh sách giá trị được phân biệt bởi dấu cách. -->

<!-- Nesting element - Phần tử lồng nhau -->
<!-- Nội dung một element có thể chứa các element khác -->
<cha>
  <con>Text</con>
  <elementrong />
</cha>
<!-- Danh pháp cây tiêu chuẩn được tuân theo. Mỗi phần tử được gọi là một nút.
   Phần tử cấp trên là cha, phần tử cấp dưới là con.
   Các element trong cùng một element cha có mức tương đương nhau như anh chị em. -->

<!-- XML bảo lưu dấu cách. -->
<child>
  Văn bản
</child>
<!-- sẽ không giống như -->
<child>Văn bản</child>
```

## Một văn bản XML - XML document

Đây là thứ làm cho XML rất linh hoạt do nó giúp con người cũng đọc được. Văn bản sau đây cho ta biết nó định nghĩa một hiệu sách bản ba quyển sách, trong đó có một cuốn tên Learning XML bởi Erik T. Ray. Tất cả những việc này chưa cần phải sử dụng XML Parser.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Đây là phần mở đầu, không bắt buộc cần nhưng nên có -->
<hieusach>
  <sach danhmuc="NAUAN">
    <tieude lang="en">Everyday Italian</tieude>
    <tacgia>Giada De Laurentiis</tacgia>
    <nam>2005</nam>
    <giaban>30.00</giaban>
  </sach>
  <sach danhmuc="TREEM">
    <tieude lang="en">Harry Potter</tieude>
    <tacgia>J K. Rowling</tacgia>
    <nam>2005</nam>
    <giaban>29.99</giaban>
  </sach>
  <sach danhmuc="WEB">
    <tieude lang="en">Learning XML</tieude>
    <tacgia>Erik T. Ray</tacgia>
    <nam>2003</nam>
    <giaban>39.95</giaban>
  </sach>
</hieusach>
```

## Tính đúng đắn và việc xác minh

Một văn bản XML là _đúng đắn (well-formed)_ nếu nó có cú pháp chính xác. Ty nhiên, ta có thể thêm nhiều ràng buộc vào văn bản, sử dụng Document Type Definition (DTD) - Định dạng loại văn bản. Một văn bản mà các phần tử và thuộc tính được khai báo trong một DTĐ và tuân theo ngữ pháp được đưa ra trong DTD đó được gọi là _valid - được chấp nhận_ và tuân theo DTD bên cạnh việc đúng đắn.

```xml
<!-- Khai báo DTD lấy từ tệp bên ngoài: -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE hieusach SYSTEM "Hieusach.dtd">
<!-- Khai báo hieusach là phần tử gốc và 'Hieusach.dtd' là đường dẫn
  tới tệp DTD. -->
<hieusach>
  <sach danhmuc="NAUAN">
    <tieude lang="en">Everyday Italian</tieude>
    <tacgia>Giada De Laurentiis</tacgia>
    <nam>2005</nam>
    <giaban>30.00</giaban>
  </sach>
</hieusach>

<!-- Tệp DTD: -->
<!ELEMENT hieusach (sach+)>
<!-- Element hieusach có thể chứa một hoặc nhiều element sach. -->
<!ELEMENT sach (tieude, giaban)>
<!-- Mỗi sach cần có các element con tên tieude và giaban. -->
<!ATTLIST sach danhmuc CDATA "Vanhoc">
<!-- Mỗi sach cần có một thuộc tính danhmuc. Nếu không có, giá trị mặc định
  sẽ là 'Vanhoc'. -->
<!ELEMENT tieude (#PCDATA)>
<!-- Element tieude chỉ được chứa nội dung dữ liệu kĩ tự được phân tích.
Nói cách khác, nó có thể
  chỉ chứa văn bản được phân tích bởi parser và không được phép chứa element con
  So sánh với CDATA, hay dữ liệu kí tự -->
<!ELEMENT giaban (#PCDATA)>
]>

<!-- DTD có thể được khai báo bên trong chính tệp XML.-->

<?xml version="1.0" encoding="UTF-8"?>

<!DOCTYPE hieusach [
<!ELEMENT hieusach (sach+)>
<!ELEMENT sach (tieude, giaban)>
<!ATTLIST sach danhmuc CDATA "Vanhoc">
<!ELEMENT tieude (#PCDATA)>
<!ELEMENT giaban (#PCDATA)>
]>

<hieusach>
  <sach danhmuc="NAUAN">
    <tieude lang="en">Everyday Italian</tieude>
    <giaban>30.00</giaban>
  </sach>
</hieusach>
```

## DTD Compatibility and XML Schema Definitions (Tương thích DTD và định nghĩa XML Schema)

Hỗ trợ cho DTD khá nhiều do chúng đã quá cũ. Tuy nhiên, nhiều tính năng hiện đại của XML như namespace không được hỗ trợ bởi DTD. XML Schema Definition (XSD) - Định nghĩa lược đồ XML được coi như sẽ thay thế DTD để định nghĩa cấu trúc ngữ pháp của văn bản XML.

## Tra cứu

- [Validate your XML (Xác minh XML)](http://www.xmlvalidation.com)

## Đọc thêm

- [Hướng dẫn XML Schema Definitions](http://www.w3schools.com/schema/)
- [Hướng dẫn DTD](http://www.w3schools.com/xml/xml_dtd_intro.asp)
- [Hướng dẫn XML](http://www.w3schools.com/xml/default.asp)
- [Dùng XPath queries để phân tích cú pháp XML](http://www.w3schools.com/xml/xml_xpath.asp)
