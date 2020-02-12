---
language: Python
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Roberto Fernandez Diaz", "https://github.com/robertofd1995"]
translators:
    - ["Ahmad Hegazy", "https://github.com/ahegazy"]
lang: ar-ar
filename: learnpython-ar.py
---

لقد أُنشئت لغة البايثون بواسطة جايدو ڤان روسم في بداية التسعينات. هي الأن أحد أشهر اللغات الموجودة. 
لقد أحببت لغة البايثون بسبب وضوحها. هي في الأساس عبارة عن سودوكود قابل للتنفيذ.

ردود أفعالكم عن المقال مُقدرة بشدة. يمكنكم التواصل مع الكاتب الاساسي من خلال [@louiedinh](http://twitter.com/louiedinh) أو louiedinh [at] [google's email service]

ملحوظة: هذا المقال يُطبق على بايثون 3 فقط. راجع المقال [هنا](http://learnxinyminutes.com/docs/pythonlegacy/) إذا أردت تعلم لغة البايثون نسخة 2.7 الأقدم

```python

# تعليق من سطر واحد يبدأ برمز الرقم.

""" يمكن كتابة تعليق يتكون من أكثر من سطر
    باستخدام ثلاثة علامات "
    ، وعادة يُستخدم في كتابة التوثيقات.
"""

####################################################
## 1. أنواع البيانات البدائية والعمليات
####################################################

# لديك أرقام
3  # => 3

# العمليات الحسابية هي ما تتوقعه
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# نتائج قسمة الأرقام الصحيحية تُقرب للأصغر سواءًا كانت الأرقام موجبة أو سالبة.
5 // 3       # => 1
5.0 // 3.0   # => 1.0 # يعمل في حالة الكسور أيضا
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# ناتج القسمة هو دائما كسر
10.0 / 3  # => 3.3333333333333335

# عملية باقي القسمة
7 % 3  # => 1

# الأُس  (س ** ص، رفع س لقوى ص)
2**3  # => 8

# أفرض ترتيب العمليات الحسابية بالأقواس
(1 + 3) * 2  # => 8

# القيم الثنائية هي المعروفة عموما (ﻻحظ: تكبير أول حرف)
True
False

# أنفي بـ (not)
not True   # => False
not False  # => True

# العمليات على القيم الثنائية
# ﻻحظ ﻻيهم حالة الحرف (كبير أو صغير) في "and" و "or"
True and False  # => False
False or True   # => True

# True و False هما في الواقع 1 و 0 لكن بمسميات مختلفة
True + True # => 2
True * 8    # => 8
False - 5   # => -5

# عمليات المقارنة تنظر الي القيمة الرقمية لل True وال False
0 == False  # => True
1 == True   # => True
2 == True   # => False
-5 != False # => True

# عند استخدام المنطق الثنائي على القيم الصحيحة يتم تحويلهم الي قيم ثنائية لإجرات العمليات عليهم، لكن قيمهم الأصلية تعود
# ﻻ تخلط بين bool(قيمة صحيحة) و العمليات المنطقية الثناية and/or (&,|)
bool(0)     # => False
bool(4)     # => True
bool(-6)    # => True
0 and 2     # => 0
-5 or 0     # => -5

# مقارنة التساوي ب ==
1 == 1  # => True
2 == 1  # => False

# مقارنة الاختلاف ب !=
1 != 1  # => False
2 != 1  # => True

# مقارنات أخرى
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# لمعرفة هل القيمة في نطاق معين 
1 < 2 and 2 < 3  # => True
2 < 3 and 3 < 2  # => False

# التسلسل يجعلها تبدو أجمل
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is مقابل ==) is تتحق من أن المتغيرين يشيران إلي نفس العنصر,
# لكن == تتحقق من أن العنصرين المُشار اليهما بالمتغيرين لهما نفس القيمة.
a = [1, 2, 3, 4]  # اجعل a تشير إلي قائمة جديدة, [1, 2, 3, 4]
b = a             # اجعل a تُشير الي ما تُشير إليه b
b is a            # => True, a و b يُشيران إلي نفس العنصر
b == a            # => True, قيمة عنصر a و b متساوية
b = [1, 2, 3, 4]  # اجعل b تشير الي قائمة جديدة , [1, 2, 3, 4]
b is a            # => False, a و b do ﻻ يشيران إلي نفس العنصر
b == a            # => True,  قيمة عنصر a و b متساوية

# يمكنك إنشاء الكلمات (تسلسلات الحروف) عن طريق " أو '
"This is a string."
'This is also a string.'

# يمكنك جمع هذا النوع أيضا! لكن حاول ألا تفعل هذا.
"Hello " + "world!"  # => "Hello world!"
# يمكنك الربط بين الكلمات بدون استخدام '+' (لكن ليس المتغيرات)
"Hello " "world!"    # => "Hello world!"

# يمكنك معاملة الكلمات كقائمة من الحروف
"This is a string"[0]  # => 'T'

# يمكنك معرفة طول الكلمة
len("This is a string")  # => 16

# .format يمكنك استخدامها لبناء الجمل بشكل معين, مثل هذا:
"{} can be {}".format("Strings", "interpolated")  # => "Strings can be interpolated"

# يمكنك تكرار معاملات بناء الجملة لتقليل الكتابة.
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
# => "Jack be nimble, Jack be quick, Jack jump over the candle stick"

# يمكنك استخدام الكلمات المفتاحية إذا لم تُرد العد.
"{name} wants to eat {food}".format(name="Bob", food="lasagna")  # => "Bob wants to eat lasagna"

# إذا كان كود بايثون 3 الخاص بك يحتاج لبايثون 2.5 أو نسخة أقدم
# يمكنك استخدام أسلوب بناء الجمل القديم:
"%s can be %s the %s way" % ("Strings", "interpolated", "old")  # => "Strings can be interpolated the old way"

# يمكنك أبضا بناء الجمل باستخدام f-strings أو حروف بناء الجمل (في بايثون 3.6 فما فوق)
name = "Reiko"
f"She said her name is {name}." # => "She said her name is Reiko"
# يمكنك ببساطة وضع أي كود بايثون داخل أقواس وستقوم بإخراج الجملة.
f"{name} is {len(name)} characters long."


# None عبارة عن كائن
None  # => None

# ﻻ تستخدم رمز المساواة "==" لمقارنة العناصر ب None
# استخدم is بدلا منه. يقوم بالتحقق من مساواة هوية العنصر
"etc" is None  # => False
None is None   # => True

# None, 0, قوائم/جمل/قواميس/صفوف فارغة كلها تُترجم إلي False.
# كل القيم الأخرى True.
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False
bool(())  # => False

####################################################
## 2. المتغيرات والمجموعات
####################################################

# بايثون لديها دالة عرض "print"
print("I'm Python. Nice to meet you!")  # => I'm Python. Nice to meet you!

# الافتراضي دالة print تطبع سطر جديد في النهاية.
# استخدم المعامل end لتغيير أخر الجملة المعروضة.
print("Hello, World", end="!")  # => Hello, World!

# طريقة بسيطة لطلب مدخل من الطرفية
input_string_var = input("Enter some data: ") # يقوم بإعادة البيانات ك "string"
# لاحظ: في النسخ القديمة من بايثون، دالة input() كان اسمها raw_input()

# ﻻ يوجد تعريفات للمتغيرات، يتم تعيين قيمة المتغير مباشرة.
# العٌرف تسمية المتغيرات حروف_صغيرة_مع_خطوط_سُفلية
some_var = 5
some_var  # => 5

# محاولة استخدام متغير غير مُعين يعتبر خطأ
# إقرأ جزء 3.مسار التحكم لمعرفة المزيد عن التحكم في الأخطاء
some_unknown_var  # يعرض خطأ NameError

# يمكن استخدام if كتعبير واحد
# مساوِ للتعبير الأتي في لغة السي '?:' عملية ثلاثية
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# القوائم تحفظ المتسلسلات
li = []
# يمكنك البدأ بقائمة مليئة
other_li = [4, 5, 6]

# إضافة بيانات لأخر القائمة عن طريق append
li.append(1)    # li is now [1]
li.append(2)    # li is now [1, 2]
li.append(4)    # li is now [1, 2, 4]
li.append(3)    # li is now [1, 2, 4, 3]
# حذف أخر عنصر في القائمة عن طريق pop
li.pop()        # => 3 and li is now [1, 2, 4]
# هيا نعيده ثانية
li.append(3)    # li is now [1, 2, 4, 3] again.

# يمكنك الوصول لعناصر القائمة كما تفعل في ال array
# Access a list like you would any array
li[0]   # => 1
# للوصول لأخر عنصر
li[-1]  # => 3

# محاولة الوصول لعنصر خارج نطاق القائمة يعتبر خطأ: IndexError
li[4]  # يعرض خطأ IndexError

# يمكنك النظر للنطاقات باستخدام تركيب التقطيع
# مؤشر/رقم/فهرس البداية مُضمن، مؤشر النهاية ﻻ
# (لمحبي الرياضيات هو نطاق مفتوح/مغلق)
li[1:3]   # => [2, 4]
# إحذف أول عنصر ثم إعرض القائمة
li[2:]    # => [4, 3]
# إحذف أخر عنصر ثم إعرض القائمة
li[:3]    # => [1, 2, 4]
# حدد عنصر ثم إحذف الذي يليه ثم حدد عنصر وهكذا
li[::2]   # =>[1, 4]
# اعرض نسخة معكوسة من القائمة
li[::-1]  # => [3, 4, 2, 1]
# إستخدم أي تجميعة من الطرق المذكورة لعمل تقطيعات متقدمة
# li[start:end:step]

# عمل نسخة من طبقة واحدة باستخدم التقطيع
li2 = li[:]  # => li2 = [1, 2, 4, 3] لكن عند عمل(li2 is li) سينتج False.

# إمسح أي عنصر من القائمة باستخدام "del"
del li[2]  # li is now [1, 2, 3]

# إمسح أول ظهور لقيمة.
li.remove(2)  # li is now [1, 3]
li.remove(2)  # يعرض خطأ ValueError لأن 2 غير موجود في القائمة

# أضف عنصر في خانة معينة
li.insert(1, 2)  # li is now [1, 2, 3] مرة أخرى

# أحصل على مؤشر/رقم لأول ظهور للقيمة
li.index(2)  # => 1
li.index(4)  # يعرض خطأ ValueError لأن 4 غير موجودة في القائمة

# يمكنك جمع قوائم
# لاحظ: لا يتم تعديل قيمة li و other_li
li + other_li  # => [1, 2, 3, 4, 5, 6]

# إستخدم دالة "extend()" لربط القوائم
li.extend(other_li)  # Now li is [1, 2, 3, 4, 5, 6]

# راجع وجود قيمة في القائمة باستخدام "in"
1 in li  # => True

# إحصل على طول القائمة باستخدام دالة "len()"
len(li)  # => 6


# الصفوف تشبه القوائم لكنها غير قابلة للتغيير.
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # يعرض خطأ TypeError

# لاحظ أن صف طوله عنصر واحد يحتاج لإضافة فاصلة "," بعد أخر عنصر
# لكن الصفوف من أي طول أخر، حتى صفر لا تحتاج.
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# يمكنك عمل معظم عمليات القوائم على الصفوف.
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# يمكنك تفريغ الصفوف (أو القوائم) في متغيرات
a, b, c = (1, 2, 3)  # a is now 1, b is now 2 and c is now 3
# يمكنك أيضا عمل تفريغ واسع
a, *b, c = (1, 2, 3, 4)  # a is now 1, b is now [2, 3] and c is now 4
# الصفوف تُنشأ تلقائيا إذا تركت الأقواس
d, e, f = 4, 5, 6  # تم توسعة الصف 4, 5 ,6 في المتغيرات d, e, f
# بالترتيب حيث d = 4, e = 5 و f = 6
# الأن إنظر إلي مدى سهولة التبديل بين قيم متغيرين
e, d = d, e  # d is now 5 and e is now 4


# القواميس تُخزن خرائط من المفتاح للقيمة
empty_dict = {}
# هذا قاموس مملوء
filled_dict = {"one": 1, "two": 2, "three": 3}

# لاحظ أن القواميس يجب أن تكون أنواع غير قابلة للتغيير.
# هذا للتأكد من أن المفتاح يمكن تحويله لقيمة ثابتة للوصول السريع.
# الأنواع الغير قابلة للتغير تتضمن: الأرقام الصحيحة، الكسور، الكلمات، الصفوف.
invalid_dict = {[1,2,3]: "123"}  # =>يعرض خطأ TypeError: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # القيم يمكن أن تكون من أي نوع.

# يمكنك البحث عن قيمة باستخدام []
filled_dict["one"]  # => 1

# يمكنك الحصول على كل المفاتيح باستخدام "keys()".
# نحتاج لإرسالها لدالة list() لتحويلها لقائمة. سنتعلم هذا لاحقًا
# لاحظ - لنسخ بايثون قبل 3.7، ترتيب مفاتيح القاموس غير مضمون. نتائجك
# يمكن ألا تساوي المثال بالأسفل. مع ذلك، من أول بايثون 3.7،
# عناصر القاموس تحتفظ بالترتيب الذي تم إضافة المفاتيح به في القاموس.
list(filled_dict.keys())  # => ["three", "two", "one"] in Python <3.7
list(filled_dict.keys())  # => ["one", "two", "three"] in Python 3.7+

# يمكنك الحصول على كل القيم باستخدام "values()".
# مرة أخرى نستخدم list() للحصول عليها كقائمة.
# نفس الكلام السابق بخصوص ترتيب المفاتيح
list(filled_dict.values())  # => [3, 2, 1]  in Python <3.7
list(filled_dict.values())  # => [1, 2, 3] in Python 3.7+

# إفحص للتأكد من وجود مغتاح في القاموس باستخدام "in"
"one" in filled_dict  # => True
1 in filled_dict      # => False

# البحث عن مفتاح غير موجود يعرض خطأ KeyError
filled_dict["four"]  # KeyError

# استخدم "get()" لتجنب الخطأ KeyError
filled_dict.get("one")      # => 1
filled_dict.get("four")     # => None
# دالة get تدعم إدخال قيمة افتراضية عند عدم وجود البحث
filled_dict.get("one", 4)   # => 1
filled_dict.get("four", 4)  # => 4

# "setdefault()" تقوم بإدخال قيمة جديدة في القاموس في حالة عدم وجود المفتاح فقط.
filled_dict.setdefault("five", 5)  # filled_dict["five"] is set to 5
filled_dict.setdefault("five", 6)  # filled_dict["five"] is still 5

# إضافة عنصر للقاموس
filled_dict.update({"four":4})  # => {"one": 1, "two": 2, "three": 3, "four": 4}
filled_dict["four"] = 4         # طريقة أخرى للإضافة

# مسح المفاتيح من القاموس باستخدام del
del filled_dict["one"]  # Removes the key "one" from filled dict

# من بايثون 3.5 فما فوق يمكنك أيضا استخدام خيارات تفريغ إضافية
{'a': 1, **{'b': 2}}  # => {'a': 1, 'b': 2}
{'a': 1, **{'a': 2}}  # => {'a': 2}


# المجموعات تُخزن .. مجموعات
empty_set = set()
# .تهيئة مجموعة بمجموعة قيم. نعم، تشبه قليلا تهيئة القاموس. أسف
some_set = {1, 1, 2, 2, 3, 4}  # some_set is now {1, 2, 3, 4}

# مثل مفتاح القاموس، عناصر المجموعة يجب أن تكون غير قابلة للتغيير.
invalid_set = {[1], 1}  # => يعرض خطأ TypeError: unhashable type: 'list'
valid_set = {(1,), 1}

# إضافة عنصر أخر للمجموعة
filled_set = some_set
filled_set.add(5)  # filled_set is now {1, 2, 3, 4, 5}
# المجموعات لا يمكن أن تحتوي على عناصر مكررة
filled_set.add(5)  # it remains as before {1, 2, 3, 4, 5}

# تقاطع مجموعتين باستخدام &
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# اتحاد مجموعتين باستخدام | 
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# الفرق بين مجموعتين باستخدام -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# الفروق بين مجموعتين باستخدام ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# لفحص هل المجموعة على اليسار مجموعة عُليا للمجموعة على اليمين (تحتوي على كل عناصرها)
{1, 2} >= {1, 2, 3} # => False

# لفحص هل المجموعة على اليسار مجموعة فرعية من المجموعة على اليمين
{1, 2} <= {1, 2, 3} # => True

# للتأكد من وجود عن في مجموعة استخدم in 
2 in filled_set   # => True
10 in filled_set  # => False



####################################################
## 3. مسار التحكم والعمليات التكرارية #Control Flow and Iterables
####################################################

# هيا ننشيء متغير
some_var = 5

# الأن الأمر if. الفجوات (المسافات قبل الأوامر) مهمة في البايثون!
# العُرف استخدام أربع مسافات. ليس تبويب.
# هذا السطر البرمجي يطبع "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # This elif clause is optional.
    print("some_var is smaller than 10.")
else:                  # This is optional too.
    print("some_var is indeed 10.")


"""
For عبارة عن حلقات تدور حول عناصر قوائم 
:ثم تطبع
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # يمكنك استخدام format() لترجمة كلمات بشكل معين.
    print("{} is a mammal".format(animal))

"""
"range(number)" يقوم بإعادة مجموعة من الأرقام يمكن الدوران حولها
من الصفر إلي رقم معين 
ثم يطبع: 
    0
    1
    2
    3
"""

for i in range(4):
    print(i)

"""
"range(lower, upper)" يقوم بإعادة مجموعة من الأرقام يمكن الدوران حولها من القيمة السُفلى
lower حتى القيمة العُليا upper
ثم يطبع:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)" يقوم بإعادة مجموعة من الأرقام يمكن الدوران حولها من القيمة السُفلى
lower حتى القيمة العُليا upper، ثم يقوم بالزيادة قيمة الstep.
إذا لم تُحدد ال step, القيمة الأفتراضية 1.
ثم يطبع:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)

"""
While هي عبارة عن حلقات تدور حتى عدم تحقق شرط معين.
وتطبع: 
    0
    1
    2
    3
"""
x = 0 for
while x < 4:
    print(x)
    x += 1  # اختصار ل x = x + 1

# يمكنك التحكم في الأخطاء والاستثناءات باستخدام مجموعة try/except 
try:
    # استخدم "raise" لرفع خطأ.
    raise IndexError("This is an index error")
except IndexError as e:
    pass                 # Pass: هو مجرد أمر ﻻ تفعل شيء. عادة تقوم بتصحيح الخطأ هنا.
except (TypeError, NameError):
    pass                 # يمكنك التحكم في أكثر من خطأ في نفس الوقت، إذا أقتضت الضرورة
else:                    # فقرة اختيارية في مجموعة try/except. يجب أن يتبع جميع مجموعات معارضة الأخطاء
    print("All good!")   # تُنفذ في حالة أن السطور البرمجية داخل ال try لم ترفع أي خطأ
finally:                 #  تُنفذ في كل الحالات
    print("We can clean up resources here")

# بدلا من مجموعة try/finally لتنظيف الموارد يمكنك استخدام سطر with 
with open("myfile.txt") as f:
    for line in f:
        print(line)

# يتيح البايثون تجريد أساسي يسمى المُكرَر.
# المُكرٍَر عبارة عن متغير يمكن التعامل معه كسلسلة.
# الكائن الذي يعود من دالة نطاق، يسمى المُكرَر.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['one', 'two', 'three'])
# هذا عبارة عن متغير يعرض عناصر مفاتيح المُكرَر.

# يمكننا الدوران حوله.
for i in our_iterable:
    print(i)  # Prints one, two, three

# مع ذلك ﻻ يمكننا الوصول للعناصر بالمؤشر.
our_iterable[1]  # يرفع خطأ TypeError

# المُكرَر هو عبارة عن عنصر يعلم كيفية إنشاء مُكرِر
our_iterator = iter(our_iterable)

# المُكرِر هو عبارة عن عنصر يمكنه تذكر الحالة أثناء مرورنا بعناصره.
# يمكننا الحصول على العنصر التالي عن طريق "next()"
next(our_iterator)  # => "one"

# يحفظ الحالة أثناء الدوران.
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# بعد عرض المُكرِر كل عناصره، يرفع استثناء StopIteration
next(our_iterator)  # يرفع StopIteration

# يمكنك الحصول على كل عناصر المُكرر بمناداة دالة list() عليه.
list(filled_dict.keys())  # => Returns ["one", "two", "three"]


####################################################
## 4. الدوال
####################################################

# إستخدم "def" لإنشاء دوال جديدة.
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y  # يمكنك إرجاع قيمة من الدالة بسطر return

# مناداة دوال بمعطيات
add(5, 6)  # => prints out "x is 5 and y is 6" and returns 11

# طريقة أخرى لمناداة دوال باستخدام كلمات مفتاحية.
add(y=6, x=5)  # الكلمة المفتاحية يمكن أن تُعطى بأي ترتيب.

# يمكنك تعريف دوال تأخذ عدد متغير من المُعطيات

def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# يمكنك تعريف دوال تأخذ عدد متغير من الكلمات المفتاحية كمعطيات أيضا.
def keyword_args(**kwargs):
    return kwargs

# هيا ننادي على الدالة لنرى ماذا سيحدث 
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# يمكنك فعل الأثنين معًا في نفس الوقت، إذا أردت
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# عندما تنادي على دوال، يمكنك عمل عكس المعطيات/المفاتيح!
# استخدم * لتوسعة الصفوف، واستخدم ** لتوسعة المفاتيح.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # مساوٍ ل all_the_args(1, 2, 3, 4)
all_the_args(**kwargs)         # مساوٍ ل to all_the_args(a=3, b=4)
all_the_args(*args, **kwargs)  # مساوٍ ل to all_the_args(1, 2, 3, 4, a=3, b=4)

# يقوم بإعادة مجموعة من القيم (بتعيين الصفوف)
def swap(x, y):
    return y, x  # يقوم بإعادة مجموعة من القيم على شكل صفوف بدون الأقواس
                 # (لاحظ: الأقواس حُذفت لكن يمكن إضافتها)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # مرة أخرى الأقواس حُذفت لكن يمكن إضافتها.

# مجال الدالة
x = 5

def set_x(num):
    # المتغير المحلي x ليس هو المتغير العام x
    x = num    # => 43
    print(x)   # => 43

def set_global_x(num):
    global x
    print(x)   # => 5
    x = num    #المتغير العام x الأن مساوٍ ل 6
    print(x)   # => 6

set_x(43)
set_global_x(6)


# بايثون تدعم دوال الفئة أولية [first class functions] (أي أنه يمكن إرسال الدوال كمعطيات لدوال أخرى)
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# يوجد أيضا دوال مجهولة
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# يوجد دوال مدمجة من درجة أعلى
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# يمكن إشتمال القوائم على خرائط وفلاتر حسنة الشكل
# هذه القوائم تحفظ المُخرج كقائمة والتي بدورها يمكن أن تكون قائمة مداخلة
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# يمكنك بناء مجموعات وقواميس على هذا المنوال أيضا
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. الوحدات البرمجية (الموديولات)
####################################################

# يمكنك استدعاء موديولات
import math
print(math.sqrt(16))  # => 4.0

# يمكنك استدعاء دالة معينة من موديول
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# يمكنك استدعاء كل الدوال من مديول.
# تحذير: هذا الفعل غير موصى به
from math import *

# يمكنك تصغير اسم موديول
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# موديولات البايثون عبارة عن ملفات بايثون عادية.
# يمكنك كتابة الموديولات الخاصة بك, واستدعاها.
# اسم الموديول يكون نفس اسم الملف.

# يمكنك معرفة أي الدوال والصفات مُعرفة في الموديول.
import math
dir(math)

# إذا كان لديك سكربت بايثون يسمى math.py
# في نفس المجلد الموجود به السكربت الخاص بك، الملف الخاص بك math.py
# سَيُستدعى بدلا من موديول البايثون بنفس الاسم
# هذا يحدث لأن المجلدات المحلية لديها أولوية عن مكتبات البايثون المُدمجة


####################################################
## 6. الفئات/القوالب (الكلاسات)
####################################################

# نستخدم السطر البرمجي "class" لإنشاء قالب
class Human:

    # صفة القالب. مشتركة بين كل نسخ القالب
    species = "H. sapiens"

    # مُهيئ إبتدائي، يُنادى عليه عندما يتم استدعاء القالب.
    # لاحظ أن الشرطة السٌفلية المُكررة مرتين __ قبل وبعد الاسم تُعبر عن الكائنات
    # أو الصفات المُستخدمة عن طريق  بايثون لكنها تعيش في مساحة تحكم المُستخدم.
    # العمليات -الدوال- (أو الكائنات أو الصفات) مثل: __init__, __str__,__repr__ ألخ.
    # تُسمى عمليات خاصة (أو أحيانا تسمى عمليات سحرية أو dunder methods)
    # يجب عليك ألا تُسمي مثل هذه الاسماء بنفسك.
    def __init__(self, name):
        # ساوِ المُعطى بالصفة name الخاصة بهذه النسخة من القالب.
        self.name = name
        
        # هيئ الصفة
        self._age = 0

    # عملية/دالة خاصة بنسخة القالب. كل العمليات تأخذ "self" كأول مُعطى
    # An instance method. All methods take "self" as the first argument
    def say(self, msg):
        print("{name}: {message}".format(name=self.name, message=msg))

    # عملية أخرى خاصة بنسخة القالب.
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # عمليات القالب مشتركة بين كل أجزاء القالب
    # يتم مناداتهم عن طريق جعل القالب المُنادي أول معطى
    # They are called with the calling class as the first argument
    @classmethod
    def get_species(cls):
        return cls.species

    # تُنادى العملية الثابتة بدون قالب أو نسخة قالب
    @staticmethod
    def grunt():
        return "*grunt*"

    # الخاصية تشبه تماما إمر الطلب
    # تُحَوِل العملية age() إلي صفة مقروءة فقط بنفس الاسم.
    # ﻻ حاجة لكتابة أوامر طلب أو تهيئة
    @property
    def age(self):
        return self._age

    # هذا يتيح تهيئة الخاصية 
    @age.setter
    def age(self, age):
        self._age = age

    # هذا يتيح حذف الخاصية
    @age.deleter
    def age(self):
        del self._age


# عندما يقرأ مُترجم البايثون ملف مصدري يقوم بتنفيذ كل الكود.
# فحص ال __name__ يجعل هذا الجزء من الكود يُنَفَذ فقط 
# في حالة أن هذا الموديول هو البرنامج الرئيسي
if __name__ == '__main__':
    # Instantiate a class
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i و j نُسخ من النوع Human, أول بكلمات أخرى: هما كائنات للقالب Human

    # نادي على عملية القالب
    i.say(i.get_species())          # "Ian: H. sapiens"
    # عدل الخاصية المُشتركة
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # نادي على العملية الثابتة
    print(Human.grunt())            # => "*grunt*"
    
    # لا يمكن مناداة العملية الثابتة من نسخة الكائن
    # لأن i.grunt() سيقوم تلقائيا بوضع "self" (الكائن i) كمُعطى للعملية
    print(i.grunt())                # => TypeError: grunt() takes 0 positional arguments but 1 was given
                                    
    # حدًث الخاصية لهذه النسخة
    i.age = 42
    # أحصل على الخاصية
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # إحذف الخاصية
    del i.age
    # i.age                         # => سوف يرفع استثناء AttributeError


####################################################
## 6.1 الإرث
####################################################

# الإرث يتيح لقالب ابن أن يُعَرف ويرث بعض عمليات/دوال ومتغيرات القالب الأب.

# باستخدام القالب Human المُعَرف بالأعلى كأساس أو كقالب أب،
# يمكننا تعريف قالب ابن،Superhero ، يرث متغيرات القالب مثل "species", "name", و "age"، 
# وأيضا العمليات، مثل "sing", "grunt"
# من القالب Human، لكنه أيضا لديه خواصه الفريدة

# للاستفادة من التقطيع بالملف يمكنك وضع القوالب بالأعلى في ملفاتهم الخاصة،
# مثلا، human.py

# لاستيراد دالة من ملف أخر استخدم الطريقة التالية
# from "اسم الملف بدون مُلحق" import "اسم الدالة أو القالب"

from human import Human

# حدد القالب/ات الأب كمُعطى أثناء تعريف القالب.
class Superhero(Human):

    # إذا أردت أن يرث القالب الابن كل تعريفات القالب الأب بدون تعديل
    # يمكنك استخدام الكلمة المفتاحية "pass" (بدون شيء أخر)
    # لكن في هذه الحالة تم أهمالها لإنشاء قالب ابن فريد:
    # pass

    # القوالب الابن يمكنها تعديل صفات القوالب الأب
    species = 'Superhuman'

    # القوالب الابن ترث تلقائيا عمليات الإنشاء الخاصة بالقالب الأب بالإضافة إلي مُعطياتهم
    # لكن يمكن أيضا تعريف مُعطيات إضافية أو تعريفات
    # وتعديل العمليات مثل منشيء القالب.
    # هذا المُنشيء يرث المُعطى "name" من القالب "Human" 
    # ويضيف المعطيات"superpower" و "movies":
    def __init__(self, name, movie=False,
                 superpowers=["super strength", "bulletproofing"]):

        # إضافة صفة جديدة للقالب
        self.fictional = True
        self.movie = movie
        # كن على علم بالقيم الافتراضية المتغيرة، حيث أن القيم الافتراضية تُشارك
        self.superpowers = superpowers

        # الدالة "super" تتيح لك الوصول لعمليات القالب الأب
        # التي تم تغييرها عن طريق الابن، في هذه الحالة، العملية __init__<
        # هذا السطر يُنادي على منشيء القالب الأب.
        super().__init__(name)

    # تعديل العملية sing
    def sing(self):
        return 'Dun, dun, DUN!'

    # إضافة عملية جديدة للنسخة
    def boast(self):
        for power in self.superpowers:
            print("I wield the power of {pow}!".format(pow=power))


if __name__ == '__main__':
    sup = Superhero(name="Tick")

    # فحص نوع النسخة
    if isinstance(sup, Human):
        print('I am human')
    if type(sup) is Superhero:
        print('I am a superhero')

    # إحصل على ترتيب قرار البحث للعملية (Method Resolution search Order) المُستخدمة بواسطة العمليات getattr() و super()
    # هذه الصفة ديناميكية ويمكن أن تُحَدًث.
    print(Superhero.__mro__)    # => (<class '__main__.Superhero'>,
                                # => <class 'human.Human'>, <class 'object'>)

    # نادي العملية الأب لكن استخدم صفات القالب الخاص بها.
    print(sup.get_species())    # => Superhuman

    # نادي العملية المُعدلة.
    print(sup.sing())           # => Dun, dun, DUN!

    # نادي العملية من القالب Human
    sup.say('Spoon')            # => Tick: Spoon

    # نادي عملية موجودة فقط في Superhero
    sup.boast()                 # => I wield the power of super strength!
                                # => I wield the power of bulletproofing!

    # وَرَثَ صفات القالب
    sup.age = 31
    print(sup.age)              # => 31

    # صفة موجودة فقط في القالب Superhero
    print('Am I Oscar eligible? ' + str(sup.movie))

####################################################
## 6.2 الإرث المُتعدد
####################################################

# تعريف قالب أخرA
# bat.py
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # هذا القالب لديه عملية تسمى say
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # ولديه عمليته الخاصة به أيضا
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)

# تعريف قالب أخر يرث من Superhero و Bat
# superhero.py
from superhero import Superhero
from bat import Bat

# عَرٍف Batman كقالب ابن يرث من كلا من Superhero و Bat
class Batman(Superhero, Bat):

    def __init__(self, *args, **kwargs):
        # عادة لكي ترث صفة يجد أن تنادي على super:
        # super(Batman, self).__init__(*args, **kwargs)      
        # لكننا في هذه الحالة نتعامل مع إرث متعدد هنا، و super()
        # تعمل فقط مع القالب التالي في قائمة ال MRO.
        # لذا بدلا من ذلك ننادي على __init__ صراحة لكل الأباء.
        # استخدام *args و **kwargs يتيح طريقة نظيفة لتمرير المعطيات.
        # لكل أب "تقشير طبقة من البصل".
        Superhero.__init__(self, 'anonymous', movie=True, 
                           superpowers=['Wealthy'], *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # تعديل قيمة الصفة name
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    # إحصل على ترتيب قرار البحث للعملية (Method Resolution search Order) المُستخدمة بواسطة العمليات getattr() و super()
    # هذه الصفة ديناميكية ويمكن أن تُحَدًث.
    print(Batman.__mro__)       # => (<class '__main__.Batman'>, 
                                # => <class 'superhero.Superhero'>, 
                                # => <class 'human.Human'>, 
                                # => <class 'bat.Bat'>, <class 'object'>)

    # نادي على العملية الخاصة بالأب لكن استخدم الصفات الخاصة بالقالب الابن
    print(sup.get_species())    # => Superhuman

    # نادي على العملية المُعدلة
    print(sup.sing())           # => nan nan nan nan nan batman!

    # نادي على العملية من القالب Human, لأن الترتيب في الأرث مهم.
    sup.say('I agree')          # => Sad Affleck: I agree

    # نادي على العملية الموجودة فقط في القالب الأب الثاني
    print(sup.sonar())          # => ))) ... (((

    # الصفة الموروثة من القالب الأب
    sup.age = 100
    print(sup.age)              # => 100

    # الصفة الموروثة من القالب الأب الثاني، الذي تم تعديل قيمته الافتراضية
    print('Can I fly? ' + str(sup.fly)) # => Can I fly? False



####################################################
## 7. مُتَقدم
####################################################

# المولدات تُساعدك على كتابة كود كسول.
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# المولدات فعالة من حيث الذاكرة، لأنها تُحمٍل الذاكرة بالبيانات التي تحتاج
# لإجراء العملية عليها في الخطوة التالية في المُكَرِر.
# هذا يتيح إجراء عمليات على قيم كبيرة ممنوعة في حالات أخرى.
# ﻻحظ: `range` بديل ل `xrange` في بايثون 3.
for i in double_numbers(range(1, 900000000)):  # `range` is a generator.
    print(i)
    if i >= 30:
        break

# كما يمكنك إنشاء قوائم اشتمال، يمكنك إنشاء مولدات اشتمال أيضا
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # prints -1 -2 -3 -4 -5 to console/terminal

# يمكنك أيضا تغيير نوع مولد الاشتمال مباشرة إلي قائمة
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# المُحسنات
# في هذا المثال الدالة`beg` تُغلف الدالة `say`. 
# إذا كانت say_please تساوي True 
# إذا ستُغير الرسالة الراجعة من الدالة
from functools import wraps

def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Please! I am poor :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Can you buy me a beer?"
    return msg, say_please


print(say())                 # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(
```

## جاهز للمزيد?

### مجانا عبر الانترنت

* [أتمتتة المهمات المُملة عبر بايثون](https://automatetheboringstuff.com)
* [أفكار لمشروعات بلغة البايثون](http://pythonpracticeprojects.com)
* [التوثيقات الرسمية](http://docs.python.org/3/)
* [دليل المُسافر لبايثون](http://docs.python-guide.org/en/latest/)
* [دورة بايثون](http://www.python-course.eu/index.php)
* [أولى الخطوات مع بايثون](https://realpython.com/learn/python-first-steps/)
* [قائمة مُختارة من إطارات عمل بايثون الرائعة, المكتبات والبرمجيات](https://github.com/vinta/awesome-python)
* [ثلاثون خاصية وخدعة للغة البايثون ربما لم تعرف بها](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [الدليل الرسمي لنمط البايثون](https://www.python.org/dev/peps/pep-0008/)
* [بايثون 3 دوائر علوم الحاسب](http://cscircles.cemc.uwaterloo.ca/)
* [غُص في بايثون 3](http://www.diveintopython3.net/index.html)
* [دورة سريعة في البايثون للعلماء](http://nbviewer.jupyter.org/gist/anonymous/5924718)
