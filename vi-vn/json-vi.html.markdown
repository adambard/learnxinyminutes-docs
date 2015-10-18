---
language: json
filename: learnjson-vi.json
contributors:
  - ["Anna Harren", "https://github.com/iirelu"]
  - ["Marco Scannadinari", "https://github.com/marcoms"]
  - ["himanshu", "https://github.com/himanshu81494"]
translators:
  - ["Thanh Phan", "https://github.com/thanhpd"]
lang: vi-vn
---

Do JSON là một ngôn ngữ trao đổi dữ liệu hết sức đơn giản, đây có thể sẽ là bài
đơn giản nhất của Học X trong Y phút (Learn X in Y Minutes) từ trước tới nay.

JSON ở dạng thuần túy nhất không có chú thích cho câu lệnh (comment) nào, nhưng
hầu hết các trình phân tích cú pháp (parser) đều chấp nhận chú thích theo phong
cách của ngôn ngữ C (`//`, `/* */`). Một số trình phân tích cú pháp còn chấp
nhận dấu phẩy cuối cùng (vd: một dấu phẩy sau phần tử cuối cùng của một mảng
hoặc sau thuộc tính cuối cùng của một object), nhưng những trường hợp này nên
tránh để có sự tương thích tốt hơn.

Để phục vụ cho mục đích bài học này, tất cả cú pháp JSON ở đây sẽ đều là 100% hợp lệ.
May mắn thay, chúng cũng tự trình bày cho chính mình mà không cần thêm giải thích.

Các kiểu dữ liệu được JSON hỗ trợ bao gồm: số (*numbers*), chuỗi kí tự
(*string*), toán tử đúng/sai (*boolean*), mảng (*array*), *object* và *null*.
Các trình duyệt hỗ trợ bao gồm: Mozilla Firefox phiên bản 3.5 trở lên,
Internet Explorer 8 trở lên, Google Chrome, Opera 10 trở lên, Safari 4 trở lên.
Kiểu tệp JSON có dạng ".json". Kiểu MIME (Multipurpose Internet Mail Extensions)
cho JSON là "application/json". Điểm yếu của JSON đó là thiếu các định dạng cho
kiểu dữ liệu cũng như quy chuẩn cú pháp chặt chẽ sử dụng DTD.

```json
{
  "khóa": "dữ liệu",

  "các khóa": "phải luôn được đặt trong dấu ngoặc kép",
  "số": 0,
  "chuỗi kí tự": "Xin chàø. Tất cả kí tự unicode đều được chấp nhận, sử dụng với dạng \"kí tự\"."
  "có đúng không?": true,
  "không có gì": null,

  "số rất lớn": 1.2e+100,

  "objects": {
    "chú thích": "Hầu hết các cấu trúc dữ liệu bạn sẽ dùng sẽ sử dụng object.",

    "mảng": [0, 1, 2, 3, "Mảng có thể chứa bất kì thứ gì bên trong.", 5],

    "một object khác": {
      "chú thích": "Những thứ này có thể lồng vào nhau, rất tiện."
    }
  },

  "ngớ ngẩn": [
    {
      "nguồn cung cấp kali": ["chuối"]
    },
    [
      [1, 0, 0, 0],
      [0, 1, 0, 0],
      [0, 0, 1, "neo"],
      [0, 0, 0, 1]
    ]
  ],

  "phong cách khác": {
    "chú thích": "kiểm tra cái này xem!"
  , "vị trí dấu phẩy": "không quan trọng - chỉ cần nó ở trước khóa tiếp theo là được"
  , "chú thích khác": "tiện phải không"
  },

  "nó rất ngắn": "Và bạn đã xong rồi đấy. Bạn đã biết tất cả những thứ mà JSON có thể cung cấp."
}
```
