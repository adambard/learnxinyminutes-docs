---
language: Python
filename: learnpython-vi.py
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
translators:
    - ["Xuan (Sean) Luong, https://github.com/xuanluong"]
lang: vi-vn

---

Python được tạo ra bởi Guido van Rossum vào đầu những năm 90s. Ngày nay nó là một trong những ngôn ngữ phổ biến
nhất còn tồn tại. Tôi thích Python vì sự rõ ràng, trong sáng về mặt cú pháp. Về cơ bản, Python có thể coi
như một loại mã giả (pseudocode) có thể thực thi được.

Mọi phản hồi đều sẽ được tích cực ghi nhận! Bạn có thể liên lạc với tôi qua Twitter [@louiedinh](http://twitter.com/louiedinh) hoặc louiedinh [at] [google's email service]

Lưu ý: Bài viết này áp dụng riêng cho Python 3. Truy cập [vào đây](http://learnxinyminutes.com/docs/pythonlegacy/) nếu bạn muốn học phiên bản cũ Python 2.7

```python

# Dòng bình luận (comment) bắt đầu bằng dấu thăng (#)

""" Những chuỗi ký tự (string) nằm trên nhiều dòng
    có thể được viết bằng cách dùng 3 dấu nháy " và thường
    được dùng trong quá trình viết tài liệu (documentation).
"""

####################################################
## 1. Các kiểu dữ liệu cơ bản và Các phép toán
####################################################

# Bạn có những con số
3  # => 3

# Tính toán với những con số là những điều có thể bạn sẽ làm
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# Kết quả của phép chia số nguyên sẽ được làm tròn xuống cho cả số dương và số âm
5 // 3       # => 1
5.0 // 3.0   # => 1.0 # phép chia số nguyên cũng áp dụng được cho kiểu dữ liệu float biểu diễn số thực
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# Kết quả của phép chia luôn là số thực
10.0 / 3  # => 3.3333333333333335

# Phép toán lấy phần dư (modulo)
7 % 3  # => 1

# Phép lũy thừa (x**y, x lũy thừa y)
2**3  # => 8

# Áp đặt thứ tự tính toán bằng dấu ngoặc
(1 + 3) * 2  # => 8

# Kiểu Boolean cũng là một kiểu dữ liệu cơ bản (Lưu ý: ký tự đầu tiên viết hoa)
True
False

# Phủ định bằng từ khóa 'not'
not True   # => False
not False  # => True

# Các phép toán với kiểu Boolean
# Lưu ý từ khóa "and" và "or" là case-sensitive
True and False  # => False
False or True   # => True

# Lưu ý khi sử dụng các phép toán của kiểu Boolean với số nguyên 'int'
# False là 0 và True là 1
# Đừng nhầm lẫn các phép toán Boolean cho số nguyên và các phép toán and/or trên bit (& và |)
0 and 2     # => 0
-5 or 0     # => -5
0 == False  # => True
2 == True   # => False
1 == True   # => True
-5 != False != True #=> True

# So sánh bằng với ==
1 == 1  # => True
2 == 1  # => False

# So sánh không bằng với !=
1 != 1  # => False
2 != 1  # => True

# Các phép so sánh khác
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Các phép so sánh có thể xâu chuỗi với nhau!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is vs. ==) từ khóa is kiểm tra xem 2 biến có cùng tham chiếu một đối tượng, còn == kiếm tra
# xem hai đối tượng có cùng giá trị hay không.
a = [1, 2, 3, 4]  # a trỏ tới một danh sách (list) mới, [1, 2, 3, 4]
b = a             # b trỏ tới nơi mà a cũng đang trỏ tới
b is a            # => True, a và b cùng trỏ tới một đối tượng
b == a            # => True, đối tượng mà a và b trỏ tới có cùng giá trị
b = [1, 2, 3, 4]  # b trỏ tới một danh sách mới, [1, 2, 3, 4]
b is a            # => False, a và b không cùng trỏ tới một đối tượng
b == a            # => True, đối tượng mà a và b trỏ tới không có cùng giá trị

# Chuỗi ký tự được tạo ra bằng dấu nháy kép " hoặc nháy đơn '
"Đây là một chuỗi ký tự."
'Đây cũng là một chuỗi ký tự.'

# Chuỗi ký tự có thể được cộng với nhau can be added too! Tuy nhiên nên tránh làm như vậy
"Xin " + "chào!"  # => "Xin chào!"
# Các chuỗi ký tự không phải là biến (literals) có thể được nối với nhau mà không cần dùng phép cộng '+'
"Xin " "chào!"    # => "Xin chào!"

# Một chuỗi ký tự có thể xem như một danh sách (list) các ký tự
"Đây là một chuỗi ký tự"[0]  # => 'Đ'

# Bạn có thể tìm chiều dài một chuỗi
len("Đây là một chuỗi")  # => 16

# .format có thể được dùng để định dạng chuỗi, ví dụ như:
"{} có thể được {}".format("Chuỗi ký tự", "định dạng")  # => "Chuỗi ký tự có thể được định dạng"

# Bạn có thể lặp lại đối số (arguments) khi định dạnh để không phải gõ nhiều lần
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
# => "Jack be nimble, Jack be quick, Jack jump over the candle stick"

# Bạn có thể dùng từ khóa nếu bạn không muốn đếm
"{name} wants to eat {food}".format(name="Bob", food="lasagna")  # => "Bob wants to eat lasagna"

# Nếu code Python 3 của bạn cần phải chạy với Python 2.5 hoặc các bản cũ hơn, bạn cũng có thể
# dùng cách định dạng cũ:
"%s can be %s the %s way" % ("Strings", "interpolated", "old")  # => "Strings can be interpolated the old way"


# None là một đối tượng
None  # => None

# Đừng dùng so sánh bằng "==" để so sánh đối tượng với None
# Thay vào đó dùng is. Nó sẽ kiểm tra xem một đối tượng có đồng nhất với None hay không.
"etc" is None  # => False
None is None   # => True

# None, 0, và chuỗi/danh sách (list)/từ điển (dict)/tuple rỗng khi chuyển về kiểu Boolean đều có giá trị là False.
# Tất cả những giá trị khác đều là True
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False
bool(())  # => False

####################################################
## 2. Biến và Các kiểu dữ liệu gộp (Collections)
####################################################

# Hàm print trong Python
print("Tôi là Python. Rất hân hạnh được làm quen!")  # => Tôi là Python. Rất hân hạnh được làm quen!

# Hàm print mặc định in thêm ký tự xuống dòng
# Dùng đối số tùy chọn (optional argument) để thay đổi cách kết thúc chuỗi.
print("Hello, World", end="!")  # => Hello, World!

# Một cách đơn giản để lấy dữ liệu vào từ bàn phím
input_string_var = input("Nhập dữ liệu: ") # Trả về dữ liệu vào là một chuỗi
# Lưu ý: Trong những phiên bản cũ của Python input() có tên là raw_input()

# Không cần phải khai báo biến mà chỉ có gán giá trị cho biến.
# Quy ước là sử dụng chữ_viết_thường_có_dấu_gạch_dưới
some_var = 5
some_var  # => 5

# Truy cập một biến chưa được gán trước đó sẽ tạo ra biệt lệ (exception).
# Đọc mục Luồng điều khiển để hiểu thêm về việc giải quyết các biệt lệ (exception handling)
some_unknown_var  # Sinh ra một biệt lệ kiểu NameError

# if có thể dùng như một biểu thức
# Tương đương với phép toán ba ngôi trong C: '?:'
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# Kiểu danh sách (list) lưu trữ chuỗi đối tượng tuần tự
li = []
# Bạn có thể bắt đầu với một danh sách đã có sãn các phần tử
other_li = [4, 5, 6]

# Thêm phần tử vào cuối danh sách bằng phương thức append
li.append(1)    # li bây giờ là [1]
li.append(2)    # li bây giờ là [1, 2]
li.append(4)    # li bây giờ là [1, 2, 4]
li.append(3)    # li bây giờ là [1, 2, 4, 3]
# Xóa phần tử cuối cùng bằng phương thức pop
li.pop()        # => 3 and li is now [1, 2, 4]
# Sau đó ta có thể đưa đối tượng trở lại danh sách
li.append(3)    # li trở lại là [1, 2, 4, 3].

# Truy cập một danh sách như bạn làm với một mảng (array)
li[0]   # => 1
# Truy cập phần tử cuối cùng
li[-1]  # => 3

# Truy cập ngoài giới hạn sẽ tạo ra biệt lệ IndexError
li[4]  # Sinh ra một biệt lệ kiểu IndexError

# Bạn có thể truy cập một đoạn bằng phép cắt (slice).
# Chỉ mục bắt đầu được tính làm điểm bắt đầu còn chỉ mục kết thúc thì không, mà là chỉ mục của phần tử tiếp theo phần tử kết thúc
# (Về mặt toán học thì đây là một đoạn đóng/mở, hay nửa đoạn)
li[1:3]   # => [2, 4]
# Lấy từ vị trí thứ 3 đến hết
li[2:]    # => [4, 3]
# Lấy từ đầu đến vị trí thứ 3
li[:3]    # => [1, 2, 4]
# Lấy những phần tử có chỉ mục chẵn
li[::2]   # =>[1, 4]
# Trả về bản sao của danh sách bị đảo ngược
li[::-1]  # => [3, 4, 2, 1]
# Kết hợp 3 tham số để làm những phép cắt phức tạp hơn
# li[start:end:step]

# Tạo ra một bản sao sâu (deep copy) bằng phép cắt
li2 = li[:]  # => li2 = [1, 2, 4, 3] but (li2 is li) will result in false.

# Xóa phần tử nào đó của danh sách bằng "del"
del li[2]  # li is now [1, 2, 3]

# Xóa đi phần tử đầu tiên mang một giá trị nhất định
li.remove(2)  # li bây giờ là [1, 3]
li.remove(2)  # Sinh ra biệt lệ kiểu ValueError vì 2 không tồn tại trong danh sách

# Chèn một phần tử vào một vị trí cụ thể
li.insert(1, 2)  # li bây giờ lại là [1, 2, 3]

# Tìm chỉ mục của của phần tử đầu tiên mang một giá trị nhất định
li.index(2)  # => 1
li.index(4)  # Sinh ra biệt lệ a ValueError as 4 is not in the list

# Bạn có thể cộng dồn các danh sách
# Lưu ý: giá trị của li và other_li không đổi
li + other_li  # => [1, 2, 3, 4, 5, 6]

# Nối danh sách bằng "extend()"
li.extend(other_li)  # Now li is [1, 2, 3, 4, 5, 6]

# Kiểm tra sự tồn tại của một phần tử trong danh sách bằng "in"
1 in li  # => True

# Xác định độ dài bằng "len()"
len(li)  # => 6


# Tuple cũng giống như danh sách nhưng không thể thay đổi giá trị được (immutable)
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # Sinh ra biệt lệ kiểu TypeError

# Lưu ý rằng tuple có độ dài là 1 phải có dấu phẩy theo sau phần tử cuối
# nhưng tuples có độ dài khác, ngay cả tuple rỗng, thì không cần như vậy
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# Hầu hết các phép toán của danh sách đều áp dụng được cho tuples
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# Bạn có thể gán giá trị cho nhiều biến một lúc bằng tuple (tuple unpacking)
a, b, c = (1, 2, 3)  # a is now 1, b is now 2 and c is now 3
# Sau đây là unpacking kiểu mở rộng
a, *b, c = (1, 2, 3, 4)  # a bây giờ là 1, b là [2, 3] và c là 4
# Tuple được tự động tạo ra nếu bạn không để dấu ngoặc đơn
d, e, f = 4, 5, 6
# Hoán đổi hai biến trở nên dễ dàng
e, d = d, e  # d bây giờ là 5 và e là 4


# Kiểu dữ liệu từ điển (dictionaries) lưu trữ ánh xạ từ các khóa (keys) đến các giá trị (values)
empty_dict = {}
# Sau đây là một từ điển có sẵn phần tử
filled_dict = {"one": 1, "two": 2, "three": 3}

# Lưu ý rằng khóa của từ điển phải có kiểu dữ liệu thuộc loại immutable. Điều này để bảo đảm rằng
# khóa đó luôn được chuyển hóa thành một giá trị băm (hash value) duy nhất khi tìm kiếm trong từ điển
# Những kiểu immutable bao gồm số nguyên (int), số thực (float), chuỗi ký tự (string), hay tuple
invalid_dict = {[1,2,3]: "123"}  # => Sinh ra biệt lệ kiểu TypeError: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # Tuy nhiên, giá trị có thể thuộc bất kỳ kiểu gì

# Truy cập giá trị của một từ khóa bằng dấu []
filled_dict["one"]  # => 1

# Tất cả khóa trong một từ điển có thể được chuyển thành một đối tượng khả lặp (iterable).
# Chúng ta cần phải gọi hàm list() để chuyển một iterable thành một danh sách.
# Chúng ta sẽ bàn về vấn đề này sau. Lưu ý - Thứ tự của khóa trong từ điển sẽ không được đảm bảo.
# Những gì bạn thấy khi chạy dòng code dưới đây có thể sẽ không hoàn toàn giống như vậy.
list(filled_dict.keys())  # => ["three", "two", "one"]


# Tất cả các giá trị có thể chuyển thành một đối tượng khả lặp bằng cách gọi hàm "values()".
# Chúng ta cũng vẫn phải gọi hàm list() nếu muốn chuyển nó thành một danh sách. Lưu ý - thứ
# tự của giá trị cũng không được đảm bảo
list(filled_dict.values())  # => [3, 2, 1]


# Sự tồn tại của khóa trong từ điển có thể kiểm tra được thông qua từ khóa "in"
"one" in filled_dict  # => True
1 in filled_dict      # => False

# Truy xuất giá trị của một khóa không tồn tại trong từ điển sẽ tạo ra biệt lệ KeyError
filled_dict["four"]  # KeyError

# Dừng phương thức "get()" để tránh tạo ra biệt lệ KeyError
filled_dict.get("one")      # => 1
filled_dict.get("four")     # => None
# Phương thức get hỗ trợ một đối số mặt định khi không thể tìm thấy giá trị ứng với từ khóa
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)  # => 4

# "setdefault()" chèn một giá trị ứng với khóa nếu khóa đó không có sẵn trong từ điển
filled_dict.setdefault("five", 5)  # filled_dict["five"] is set to 5
filled_dict.setdefault("five", 6)  # filled_dict["five"] is still 5

# Thêm khóa và giá trị vào từ điển
filled_dict.update({"four":4})  # => {"one": 1, "two": 2, "three": 3, "four": 4}
filled_dict["four"] = 4         # another way to add to dict

# Xóa một khóa ra khỏi từ điển bằng từ khóa del
del filled_dict["one"]  # Removes the key "one" from filled dict

# Bắt đầu từ Python 3.5 bạn có thể unpack từ điển trong một từ điển khác
{'a': 1, **{'b': 2}}  # => {'a': 1, 'b': 2}
{'a': 1, **{'a': 2}}  # => {'a': 2}



# Kiểu tập hợp (set) lưu trữ ... tập hợp
empty_set = set()
# Khởi tạo giá trị một tập hợp với nhiều giá tri. Vâng, nhìn nó khá giống từ điển.
some_set = {1, 1, 2, 2, 3, 4}  # some_set is now {1, 2, 3, 4}

# Tương tự như khóa của từ điển, phần tử của một tập hợp cũng phải là immutable
invalid_set = {[1], 1}  # => Sinh ra biệt lệ TypeError: unhashable type: 'list'
valid_set = {(1,), 1}

# Thêm một phần tử vào tập hợp
filled_set.add(5)  # filled_set is now {1, 2, 3, 4, 5}

# Thực hiện phép giao hai tập hợp bằng phép toán &
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# Thực hiện phép hợp bằng phép toán |
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# Lấy hiệu của hai tập hơp bằng phép toán -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Lấy hiệu đối xứng bằng phép toán ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Kiểm tra tập hợp bên trái là tập cha của bên phải
{1, 2} >= {1, 2, 3} # => False

# Kiểm tra xem tập hợp bên trái có phải là tập con của tập hợp bên phải
{1, 2} <= {1, 2, 3} # => True

# Kiểm tra sự tồn tại của một phần tử trong tập hợp bằng từ khóa in
2 in filled_set   # => True
10 in filled_set  # => False



####################################################
## 3. Luồng điều khiển và kiểu khả lặp
####################################################

# Đầu tiên hãy tạo ra một biến
some_var = 5

# Sau đây là một câu lệnh if. Khoảng cách lề rất quan trọng trong Python
# Quy ước chung là dùng khoảng trắng chứ không phải ký tự tab
# Chuỗi sau sẽ được in ra "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # Phần elif là tùy chọn.
    print("some_var is smaller than 10.")
else:                  # else cũng là tùy chọn.
    print("some_var is indeed 10.")


"""
Lặp qua một danh sách bằng for
in ra:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # Bạn có thể dùng format() để gán một giá trị vào giữa chuỗi (string interpolation)
    print("{} is a mammal".format(animal))

"""
"range(number)" trả về một đối tượng khả lặp kiểu số
từ 0 đến giá trị của number
in ra:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(lower, upper)" trả về một đối tượng khả lặp kiểu số
từ giá trị lower đến giá trị upper
in ra:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)" trả về một đối tượng khả lặp kiểu số
từ giá trị lower đến giá trị upper, tăng dần theo giá trị
của step. Nếu không có giá trị của step thì mặc định là 1.
in ra:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)
"""

Vòng lặp while tiếp tục lặp khi điều kiện còn được thỏa mãn
in ra:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # cách viết ngán cho x = x + 1

# Handle exceptions with a try/except block
# Đối phó với biệt lệ bằng khối lệnh try/except
try:
    # Dùng "raise" để ném ra một biệt lệ
    raise IndexError("This is an index error")
except IndexError as e:
    pass                 # pass có nghĩa là không làm gì cả. Thông thường đây là nơi để khắc phụ vấn đề làm xảy ra biệt lệ
except (TypeError, NameError):
    pass                 # Nhiều biệt lệ có thể được đối phó cùng một lúc nếu cần
else:                    # Không bắt buộc phải sử dụng else nhưng nếu dùng thì nó phải sau tất cả các khối except
    print("All good!")   # Chỉ thực thi nếu không có biệt lệ phát sinh
finally:                 # Luôn thực thi trong mọi hoàn cảnh
    print("We can clean up resources here")

# Thay vì dùng try/finally để thu hồi tài nguyên (resources) ta có thể dùng with
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Python hỗ trợ kiểu dữ liệu khả lặp (iterable).
# Một đối tượng khả lặp có thể được xem như là một chuỗi các đối tượng tuần tự (sequence)
# Đối tượng trả về bởi hàm range là một khả lặp.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['one', 'two', 'three']). Đây là một đối tượng khả lặp

# Ta có thể lặp qua đối tượng
for i in our_iterable:
    print(i)  # In ra một, hai, ba

# Tuy nhiên chúng ta không thể truy cập phần tử bằng chỉ mục
our_iterable[1]  # Sinh ra biệt lệ TypeError

# Một đối tượng khả lặp là đối tượng có thể tạo ra một iterator
our_iterator = iter(our_iterable)

# iterator là một đối tượng ghi nhớ được trạng thái trong quá trình nó được duyệt qua
# đối tượng kế tiếp có thể truy cập được bằng hàm next
next(our_iterator)  # => "one"

# Nó ghi nhớ trạng thái trong quá trình lặp
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# Sau khi iterator đã trả về tất cả dữ liệu, nó sẽ sinh ra biệt lệ kiểu StopIteration
next(our_iterator)  # Sinh ra biệt lệ StopIteration

# Ta có thể lấy tất cả phần tử của một iterator bằng cách gọi hàm list với nó
list(filled_dict.keys())  # => Returns ["one", "two", "three"]


####################################################
## 4. Hàm
####################################################

# Dùng từ khóa def để định nghĩa hàm
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y  # từ khóa return để trả về một giá trị

# Gọi một hàm với đối số
add(5, 6)  # => In ra "x is 5 and y is 6" và trả về 11

# Một cách khác để gọi hàm là dùng đối số có từ khóa (keyword arguments)
add(y=6, x=5)  # Đối số có từ khóa có thể xuất hiện với thứ tự bất kỳ

# Bạn có thể định nghĩa hàm có số lượng đối số vị trí (positional arguments) không biết trước
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# Số lượng tham số từ khóa cũng có thể không cần biết trước
def keyword_args(**kwargs):
    return kwargs

# Thử gọi hàm để xem điều gì xảy ra
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# Có thể định nghĩa hàm dùng cả hai loại đối số
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) in ra:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Khi gọi hàm, bạn có thể làm ngược với khi định nghĩa
# Dùng dấu * để lấy giá trị từ args và ** với giá trị từ kwargs
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # tương đương với foo(1, 2, 3, 4)
all_the_args(**kwargs)         # tương đương với foo(a=3, b=4)
all_the_args(*args, **kwargs)  # tương đương với foo(1, 2, 3, 4, a=3, b=4)

# Trả về nhiều giá trị (gán vào một tuple)
def swap(x, y):
    return y, x  # Trả về nhiều giá trị dưới dạng một tuple mà không cần dấu ngoặc.
                 # (Lưu ý là dấu ngoặc đơn đã được bỏ đi những vẫn có thể được thêm vào)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # dấu ngoặc đơn đã được bỏ đi những vẫn có thể được thêm vào

# Tầm vực của hàm
x = 5

def set_x(num):
    # Biến cục bộ x không đồng nhất với biến toàn cục x
    x = num    # => 43
    print(x)   # => 43

def set_global_x(num):
    global x
    print(x)   # => 5
    x = num    # biến toàn cục x được gán giá trị là 6
    print(x)   # => 6

set_x(43)
set_global_x(6)


# Hàm trong Python cũng là đối tượng
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# Có những hàm không tên
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# Có những hàm cấp cao được hỗ trọ sẵn
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# list comprehension có thể dùng để hay thế map và filter
# list comprehension lưu giá trị xuất vào một danh sách mà bản thân nó có thể lồng trong danh sách khác
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# Tập hơp và từ điển cũng có thể được tao ra thông qua set comprehension và dict comprehension
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. Mô đun
####################################################

# Bạn có thể import một mô đun
import math
print(math.sqrt(16))  # => 4.0

# Bạn có thể lấy một hàm cụ thể từ một mô đun
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# Hoặc import tất cả hàm từ một mô đun
# Cảnh báo: đây không phải là một cách hay
from math import *

# Có thể làm tên của module ngắn lại
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Mô đun trong Python chỉ là những tập tin Python bình thường. Bạn
# có thể viết mô đun của mình và import chúng. Tên của mô đun
# cũng là tên của tập tin.

# You can find out which functions and attributes
# are defined in a module.
# Bạn có thể liệt kê những hàm và thuộc tính
# được định nghĩa trong một mô đun
import math
dir(math)

# Nếu bạn có một tập tin code Python gọi là math.py ở cùng
# thư mục với tập tin hiện tai, tập tin math.py sẽ
# được nạp vào thay vì mô đun được cung cấp sẵn (built-in) trong Python.
# Điều này xảy ra vì thư mục hiện tại có ưu tiên
# hơn những thư viện cung cấp sẵn.


####################################################
## 6. Lớp (classes)
####################################################

# Ta dùng từ khóa "class" đề định nghĩa một lớp
class Human:

    # Một thuộc tính của lớp được chia sẽ bởi tất cả đối tượng của lớp này
    species = "H. sapiens"

    # Hàm khởi tạo cơ bản sẽ được goi khi một đối tượng được tạo ra.
    # Lưu ý 2 dấu gạch dưới ở đầu và cuối ám chỉ đối tượng
    # hoặc thuộc tính dùng bở Python những tồn tại trong không gian tên
    # do người dùng kiểm soát. Phương thức (hoặc thuộc tính) như: __init__, __str__,
    # __repr__ v.v.. là những phương thức đặc biệt.
    # Bạn không nên tự đặt những tên như vậy.
    def __init__(self, name):
        # Gán đối số vào thuộc tính name của đối tượng
        self.name = name

        # Khởi tạo thuộc tính
        self._age = 0

    # Một phương thức trên đối tượng. Tất cả đều có đối số đầu tiên là "self"
    def say(self, msg):
        print ("{name}: {message}".format(name=self.name, message=msg))

    # Một phương thức trên đối tượng khác
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # Một phương thức trên lớp được chia sẻ với mọi đối tượng
    # Lớp đó cũng là đối số thứ nhất của phương thức đó
    @classmethod
    def get_species(cls):
        return cls.species

    # Một phương thức tĩnh được gọi mà không có lớp hay đối tượng đi kèm
    @staticmethod
    def grunt():
        return "*grunt*"

    # Một thuộc tính chỉ giống như một hàm truy xuất.
    # Nó biến phương thức age() thành một thuộc tính chỉ đọc cùng tên.
    # Tuy nhiên trong Python không nhất thiết phải viết những hàm đọc và ghi quá đơn giản
    @property
    def age(self):
        return self._age

    # Đây là hàm để ghi giá trị cho thuộc tính
    @age.setter
    def age(self, age):
        self._age = age

    # Đây là hàm để xóa thuộc tính
    @age.deleter
    def age(self):
        del self._age


# Khi trình thông dịch Python đọc một tập tin mã nguồn, nó thực thi tất cả code trong đó.
# Kiểm tra giá trị của __name__ bảo đảm rằng đoạn mã bên dưới chỉ thực thi khi
# mô đun này là chương trình chính
if __name__ == '__main__':
    # Khởi tạo một đối tượng
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i và j là thực thể của kiểu Human, nói cách khác: chúng là những đối tượng Human

    # Gọi những phương thức trên lớp
    i.say(i.get_species())          # "Ian: H. sapiens"
    # Thay đổi thuộc tính chung
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # Gọi phương thức tĩnh
    print(Human.grunt())            # => "*grunt*"
    
    # Không thể gọi phương thức tĩnh với một thực thể/đối tượng
    # bởi vì i.grunt() sẽ tự động đặt "self" (tức là đối tượng i) làm đối số thứ nhất
    print(i.grunt())                # => TypeError: grunt() takes 0 positional arguments but 1 was given
                                    
    # Thay đổi thuộc tính của đối tượng
    i.age = 42
    # Truy cập thuộc tính
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # Xóa thuộc tính
    del i.age
    # i.age                         # => dòng nãy sẽ tạo ra biệt lệ AttributeError


####################################################
## 6.1 Đa thừa kế
####################################################

# Một định nghĩa lớp khác
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # Lớp này có phương thức say
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # Và một phương thức khác
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)

# Để tận dụng việc mô đun hóa thành từng tập tin, bạn có thể đặt những lớp định nghĩa ở trên vào các tập tin riêng,
# ví dụ như human.py và bat.py

# Để import hàm từ tập tin khác dừng cấu trúc sau
# from "filename-without-extension" import "function-or-class"

# superhero.py
from human import Human
from bat import Bat

# Batman thừa kế từ lớp Human và Bat
class Batman(Human, Bat):

    # Batman có giá trị riêng cho thuộc tính trên lớp species
    species = 'Superhero'

    def __init__(self, *args, **kwargs):
	# Cách điển hình để thừa kế thuộc tính là gọi super
	# super(Batman, self).__init__(*args, **kwargs)
	# Tuy nhiên với đa thừa kế, super() sẽ chỉ gọi lớp cơ sở tiếp theo trong danh sách MRO.
	# Vì thế, ta sẽ gọi cụ thể hàm __init__ của các lớp chả.
	# Sử dụng *args và **kwargs cho phép việc truyền đối số gọn gàng hơn,
	# trong đó mỗi lớp cha sẽ chịu trách nhiệm cho những phần thuộc về nó
        Human.__init__(self, 'anonymous', *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
	# ghi đè giá trị của thuộc tính name
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    # Kiểm tra kiểu đối tượng
    if isinstance(sup, Human):
        print('I am human')
    if isinstance(sup, Bat):
        print('I am bat')
    if type(sup) is Batman:
        print('I am Batman')

    # Truy xuất thứ tự phương thức của các lớp cha (Method Resolution search Order), vốn được dùng bởi cả getattr() và super9)
    # Thuộc tính này động và có thể được cập nhật
    print(Batman.__mro__)       # => (<class '__main__.Batman'>, <class 'human.Human'>, <class 'bat.Bat'>, <class 'object'>)

    # Gọi phương thức của lớp cha nhưng dùng thuộc tính trên chính lớp hiện tại
    print(sup.get_species())    # => Superhero

    # Gọi phương thức được nạp chồng
    print(sup.sing())           # => nan nan nan nan nan batman!

    # Gọi phương thức của Human, bởi vì thứ tự thừa kế ảnh hưởng đến phương thức được gọi
    sup.say('I agree')          # => Sad Affleck: I agree

    # Gọi phương thức chỉ tồn tại ở lớp cha thứ 2
    print(sup.sonar())          # => ))) ... (((

    # Thuộc tính cấp lớp được thừa kế
    sup.age = 100
    print(sup.age)

    # Thuộc tính thừa kế từ lớp cha thứ 2 có giá trị mặc định đã bị ghi đè
    print('Can I fly? ' + str(sup.fly))



####################################################
## 7. Phần nâng cao
####################################################

# Generator giúp ta viết những đoạn code lười biếng (áp dụng nguyên tắc lazy evaluation)
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# Generators tiết kiệm bộ nhớ vì nó chỉ tải dữ liệu khi cần
# xử lý giá trị kế tiếp của một đối tượng khả lặp. Điều này cho phép generator thực hiện
# những thao tác mà bình thường không làm được trên những khoảng giá trị lớn
# Lưu ý: `range` thay thế `xrange` trong Python3.

for i in double_numbers(range(1, 900000000)):  # `range` là một generator.
    print(i)
    if i >= 30:
        break

# Cũng như danh sách có list comprehension, generator cũng có generator
# comprehension
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # in -1 -2 -3 -4 -5 ra màn hình dòng lệnh

# Một generator cũng có thể bị ép kiểu thành danh sách
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# Decorators
# Trong ví dụ này hàm `beg` 'phủ lên' hàm `say`. Nếu say_please là True thì nó
# sẽ thay đội giá trị trả về
from functools import wraps


def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Làm ơn! Tui rất nghèo :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Mua bia cho tui nhé?"
    return msg, say_please


print(say())                 # Mua bia cho tui nhé?
print(say(say_please=True))  # Mua bia cho tui nhé? Làm ơn! Tui rất nghèo :(
```

## Sẵn sàng để học nhiều hơn?

### Miễn phí trên mạng

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)
* [Python 3 Computer Science Circles](http://cscircles.cemc.uwaterloo.ca/)
* [Dive Into Python 3](http://www.diveintopython3.net/index.html)
* [A Crash Course in Python for Scientists](http://nbviewer.jupyter.org/gist/anonymous/5924718)
