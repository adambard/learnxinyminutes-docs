---
language: python3
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Eray AYDIN", "http://erayaydin.me/"]
lang: tr-tr
filename: learnpython3-tr.py
---

Python,90ların başlarında Guido Van Rossum tarafından oluşturulmuştur. En popüler olan dillerden biridir. Beni Python'a aşık eden sebep onun syntax beraklığı. Çok basit bir çalıştırılabilir söz koddur.

Not: Bu makale Python 3 içindir. Eğer Python 2.7 öğrenmek istiyorsanız [burayı](http://learnxinyminutes.com/docs/python/) kontrol edebilirsiniz.

```python

# Tek satırlık yorum satırı kare(#) işareti ile başlamaktadır.

""" Çok satırlı olmasını istediğiniz yorumlar
    üç adet tırnak(") işareti ile
    yapılmaktadır
"""

####################################################
## 1. Temel Veri Türleri ve Operatörler
####################################################

# Sayılar
3  # => 3

# Tahmin edebileceğiniz gibi matematik
1 + 1  # => 2
8 - 1  # => 7
10 * 2  # => 20

# Bölme işlemi varsayılan olarak onluk döndürür
35 / 5  # => 7.0

# Tam sayı bölmeleri, pozitif ve negatif sayılar için aşağıya yuvarlar
5 // 3     # => 1
5.0 // 3.0 # => 1.0 # onluklar için de bu böyledir
-5 // 3  # => -2
-5.0 // 3.0 # => -2.0

# Onluk kullanırsanız, sonuç da onluk olur
3 * 2.0 # => 6.0

# Kalan operatörü
7 % 3 # => 1

# Üs (2 üzeri 4)
2**4 # => 16

# Parantez ile önceliği değiştirebilirsiniz
(1 + 3) * 2  # => 8

# Boolean(Doğru-Yanlış) değerleri standart
True
False

# 'değil' ile terse çevirme
not True  # => False
not False  # => True

# Boolean Operatörleri
# "and" ve "or" büyük küçük harf duyarlıdır
True and False #=> False
False or True #=> True

# Bool operatörleri ile sayı kullanımı
0 and 2 #=> 0
-5 or 0 #=> -5
0 == False #=> True 
2 == True #=> False 
1 == True #=> True

# Eşitlik kontrolü ==
1 == 1  # => True
2 == 1  # => False

# Eşitsizlik Kontrolü !=
1 != 1  # => False
2 != 1  # => True

# Diğer karşılaştırmalar
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Zincirleme şeklinde karşılaştırma da yapabilirsiniz!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# Yazı(Strings) " veya ' işaretleri ile oluşturulabilir
"Bu bir yazı."
'Bu da bir yazı.'

# Yazılar da eklenebilir! Fakat bunu yapmanızı önermem.
"Merhaba " + "dünya!"  # => "Merhaba dünya!"

# Bir yazı(string) karakter listesi gibi işlenebilir
"Bu bir yazı"[0]  # => 'B'

# .format ile yazıyı biçimlendirebilirsiniz, şu şekilde:
"{} da ayrıca {}".format("yazılar", "işlenebilir")

# Biçimlendirme işleminde aynı argümanı da birden fazla kullanabilirsiniz.
"{0} çeviktir, {0} hızlıdır, {0} , {1} üzerinden atlayabilir".format("Ahmet", "şeker çubuğu")
#=> "Ahmet çeviktir, Ahmet hızlıdır, Ahmet , şeker çubuğu üzerinden atlayabilir"

# Argümanın sırasını saymak istemiyorsanız, anahtar kelime kullanabilirsiniz.
"{isim} yemek olarak {yemek} istiyor".format(isim="Ahmet", yemek="patates") #=> "Ahmet yemek olarak patates istiyor"

# Eğer Python 3 kodunuz ayrıca Python 2.5 ve üstünde çalışmasını istiyorsanız, 
# eski stil formatlamayı kullanabilirsiniz:
"%s bu %s yolla da %s" % ("yazılar", "eski", "biçimlendirilebilir")


# Hiçbir şey(none) da bir objedir
None  # => None

# Bir değerin none ile eşitlik kontrolü için "==" sembolünü kullanmayın
# Bunun yerine "is" kullanın. Obje türünün eşitliğini kontrol edecektir.
"vb" is None  # => False
None is None  # => True

# None, 0, ve boş yazılar/listeler/sözlükler hepsi False değeri döndürü.
# Diğer veriler ise True değeri döndürür
bool(0)  # => False
bool("")  # => False
bool([]) #=> False
bool({}) #=> False


####################################################
## 2. Değişkenler ve Koleksiyonlar
####################################################

# Python bir yazdırma fonksiyonuna sahip
print("Ben Python. Tanıştığıma memnun oldum!")

# Değişkenlere veri atamak için önce değişkeni oluşturmanıza gerek yok. 
# Düzenli bir değişken için hepsi_kucuk_ve_alt_cizgi_ile_ayirin
bir_degisken = 5
bir_degisken  # => 5

# Önceden tanımlanmamış değişkene erişmek hata oluşturacaktır.
# Kontrol akışları başlığından hata kontrolünü öğrenebilirsiniz.
bir_bilinmeyen_degisken  # NameError hatası oluşturur

# Listeler ile sıralamaları tutabilirsiniz
li = []
# Önceden doldurulmuş listeler ile başlayabilirsiniz
diger_li = [4, 5, 6]

# 'append' ile listenin sonuna ekleme yapabilirsiniz
li.append(1)    # li artık [1] oldu
li.append(2)    # li artık [1, 2] oldu
li.append(4)    # li artık [1, 2, 4] oldu
li.append(3)    # li artık [1, 2, 4, 3] oldu
# 'pop' ile listenin son elementini kaldırabilirsiniz
li.pop()        # => 3 ve li artık [1, 2, 4]
# Çıkarttığımız tekrardan ekleyelim
li.append(3)    # li yeniden [1, 2, 4, 3] oldu.

# Dizi gibi listeye erişim sağlayın
li[0]  # => 1
# Son elemente bakın
li[-1]  # => 3

# Listede olmayan bir elemente erişim sağlamaya çalışmak IndexError hatası oluşturur
li[4]  # IndexError hatası oluşturur

# Bir kısmını almak isterseniz.
li[1:3]  # => [2, 4]
# Başlangıç belirtmezseniz
li[2:]  # => [4, 3]
# Sonu belirtmesseniz
li[:3]  # => [1, 2, 4]
# Her ikişer objeyi seçme
li[::2]   # =>[1, 4]
# Listeyi tersten almak
li[::-1]   # => [3, 4, 2, 1]
# Kombinasyonları kullanarak gelişmiş bir şekilde listenin bir kısmını alabilirsiniz
# li[baslangic:son:adim]

# "del" ile isteğe bağlı, elementleri listeden kaldırabilirsiniz
del li[2]   # li artık [1, 2, 3] oldu

# Listelerde de ekleme yapabilirsiniz
# Not: değerler üzerinde değişiklik yapılmaz.
li + diger_li   # => [1, 2, 3, 4, 5, 6] 

# Listeleri birbirine bağlamak için "extend()" kullanılabilir
li.extend(diger_li)   #  li artık [1, 2, 3, 4, 5, 6] oldu

# Listedeki bir elementin olup olmadığı kontrolü "in" ile yapılabilir
1 in li   # => True

# Uzunluğu öğrenmek için "len()" kullanılabilir
len(li)   # => 6


# Tüpler listeler gibidir fakat değiştirilemez.
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # TypeError hatası oluşturur

# Diğer liste işlemlerini tüplerde de uygulayabilirsiniz
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# Tüpleri(veya listeleri) değişkenlere açabilirsiniz
a, b, c = (1, 2, 3)     # 'a' artık 1, 'b' artık 2 ve 'c' artık 3
# Eğer parantez kullanmazsanız varsayılan oalrak tüpler oluşturulur
d, e, f = 4, 5, 6
# 2 değeri birbirine değiştirmek bu kadar kolay
e, d = d, e     # 'd' artık 5 ve 'e' artık 4


# Sözlükler anahtar kodlarla verileri tutar
bos_sozl = {}
# Önceden doldurulmuş sözlük oluşturma
dolu_sozl = {"bir": 1, "iki": 2, "uc": 3}

# Değere bakmak için [] kullanalım
dolu_sozl["bir"]   # => 1

# Bütün anahtarları almak için  "keys()" kullanılabilir. 
# Listelemek için list() kullanacağınız çünkü dönen değerin işlenmesi gerekiyor. Bu konuya daha sonra değineceğiz.
# Not - Sözlük anahtarlarının sıralaması kesin değildir.
# Beklediğiniz çıktı sizinkiyle tam uyuşmuyor olabilir.
list(dolu_sozl.keys())   # => ["uc", "iki", "bir"]


# Tüm değerleri almak için "values()" kullanacağız. Dönen değeri biçimlendirmek için de list() kullanmamız gerekiyor
# Not - Sıralama değişebilir.
list(dolu_sozl.values())   # => [3, 2, 1]


# Bir anahtarın sözlükte olup olmadığını "in" ile kontrol edebilirsiniz
"bir" in dolu_sozl   # => True
1 in dolu_sozl   # => False

# Olmayan bir anahtardan değer elde etmek isterseniz KeyError sorunu oluşacaktır.
dolu_sozl["dort"]   # KeyError hatası oluşturur

# "get()" metodu ile değeri almaya çalışırsanız KeyError sorunundan kurtulursunuz
dolu_sozl.get("bir")   # => 1
dolu_sozl.get("dort")   # => None
# "get" metoduna parametre belirterek değerin olmaması durumunda varsayılan bir değer döndürebilirsiniz.
dolu_sozl.get("bir", 4)   # => 1
dolu_sozl.get("dort", 4)   # => 4

# "setdefault()" metodu sözlükte, belirttiğiniz anahtarın [olmaması] durumunda varsayılan bir değer atayacaktır
dolu_sozl.setdefault("bes", 5)  # dolu_sozl["bes"] artık 5 değerine sahip
dolu_sozl.setdefault("bes", 6)  # dolu_sozl["bes"] değişmedi, hala 5 değerine sahip

# Sözlüğe ekleme
dolu_sozl.update({"dort":4}) #=> {"bir": 1, "iki": 2, "uc": 3, "dort": 4}
#dolu_sozl["dort"] = 4  #sözlüğe eklemenin bir diğer yolu

# Sözlükten anahtar silmek için 'del' kullanılabilir
del dolu_sozl["bir"]  # "bir" anahtarını dolu sözlükten silecektir


# Setler ... set işte :D 
bos_set = set()
# Seti bir veri listesi ile de oluşturabilirsiniz. Evet, biraz sözlük gibi duruyor. Üzgünüm.
bir_set = {1, 1, 2, 2, 3, 4}   # bir_set artık {1, 2, 3, 4}

# Sete yeni setler ekleyebilirsiniz
dolu_set = bir_set

# Sete bir diğer öğe ekleme 
dolu_set.add(5)   # dolu_set artık {1, 2, 3, 4, 5} oldu

# Setlerin çakışan kısımlarını almak için '&' kullanabilirsiniz
diger_set = {3, 4, 5, 6}
dolu_set & diger_set   # => {3, 4, 5}

# '|' ile aynı olan elementleri almayacak şekilde setleri birleştirebilirsiniz
dolu_set | diger_set   # => {1, 2, 3, 4, 5, 6}

# Farklılıkları almak için "-" kullanabilirsiniz
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# Bir değerin olup olmadığının kontrolü için "in" kullanılabilir
2 in dolu_set   # => True
10 in dolu_set   # => False


####################################################
## 3. Control Flow and Iterables
####################################################

# Let's just make a variable
some_var = 5

# Here is an if statement. Indentation is significant in python!
# prints "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # This elif clause is optional.
    print("some_var is smaller than 10.")
else:                  # This is optional too.
    print("some_var is indeed 10.")


"""
For loops iterate over lists
prints:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # You can use format() to interpolate formatted strings
    print("{} is a mammal".format(animal))

"""
"range(number)" returns a list of numbers
from zero to the given number
prints:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
While loops go until a condition is no longer met.
prints:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Shorthand for x = x + 1

# Handle exceptions with a try/except block
try:
    # Use "raise" to raise an error
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # Pass is just a no-op. Usually you would do recovery here.
except (TypeError, NameError):
    pass    # Multiple exceptions can be handled together, if required.
else:   # Optional clause to the try/except block. Must follow all except blocks
    print("All good!")   # Runs only if the code in try raises no exceptions

# Python offers a fundamental abstraction called the Iterable.
# An iterable is an object that can be treated as a sequence.
# The object returned the range function, is an iterable.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable) #=> range(1,10). This is an object that implements our Iterable interface

# We can loop over it.
for i in our_iterable:
    print(i)    # Prints one, two, three

# However we cannot address elements by index.
our_iterable[1]  # Raises a TypeError

# An iterable is an object that knows how to create an iterator.
our_iterator = iter(our_iterable)

# Our iterator is an object that can remember the state as we traverse through it.
# We get the next object by calling the __next__ function.
our_iterator.__next__()  #=> "one"

# It maintains state as we call __next__.
our_iterator.__next__()  #=> "two"
our_iterator.__next__()  #=> "three"

# After the iterator has returned all of its data, it gives you a StopIterator Exception
our_iterator.__next__() # Raises StopIteration

# You can grab all the elements of an iterator by calling list() on it.
list(filled_dict.keys())  #=> Returns ["one", "two", "three"]


####################################################
## 4. Functions
####################################################

# Use "def" to create new functions
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y    # Return values with a return statement

# Calling functions with parameters
add(5, 6)   # => prints out "x is 5 and y is 6" and returns 11

# Another way to call functions is with keyword arguments
add(y=6, x=5)   # Keyword arguments can arrive in any order.

# You can define functions that take a variable number of
# positional arguments
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)

# You can define functions that take a variable number of
# keyword arguments, as well
def keyword_args(**kwargs):
    return kwargs

# Let's call it to see what happens
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# You can do both at once, if you like
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# When calling functions, you can do the opposite of args/kwargs!
# Use * to expand tuples and use ** to expand kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # equivalent to foo(1, 2, 3, 4)
all_the_args(**kwargs)   # equivalent to foo(a=3, b=4)
all_the_args(*args, **kwargs)   # equivalent to foo(1, 2, 3, 4, a=3, b=4)


# Function Scope                                                                
x = 5

def setX(num):
    # Local var x not the same as global variable x
    x = num # => 43
    print (x) # => 43
    
def setGlobalX(num):
    global x
    print (x) # => 5
    x = num # global var x is now set to 6
    print (x) # => 6

setX(43)
setGlobalX(6)


# Python has first class functions
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# There are also anonymous functions
(lambda x: x > 2)(3)   # => True

# TODO - Fix for iterables
# There are built-in higher order functions
map(add_10, [1, 2, 3])   # => [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# We can use list comprehensions for nice maps and filters
# List comprehension stores the output as a list which can itself be a nested list
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]

####################################################
## 5. Classes
####################################################


# We subclass from object to get a class.
class Human(object):

    # A class attribute. It is shared by all instances of this class
    species = "H. sapiens"

    # Basic initializer, this is called when this class is instantiated.
    # Note that the double leading and trailing underscores denote objects
    # or attributes that are used by python but that live in user-controlled
    # namespaces. Methods(or objects or attributes) like: __init__, __str__, 
    # __repr__ etc. are called magic methods (or sometimes called dunder methods)  
    # You should not invent such names on your own.
    def __init__(self, name):
        # Assign the argument to the instance's name attribute
        self.name = name

    # An instance method. All methods take "self" as the first argument
    def say(self, msg):
        return "{name}: {message}".format(name=self.name, message=msg)

    # A class method is shared among all instances
    # They are called with the calling class as the first argument
    @classmethod
    def get_species(cls):
        return cls.species

    # A static method is called without a class or instance reference
    @staticmethod
    def grunt():
        return "*grunt*"


# Instantiate a class
i = Human(name="Ian")
print(i.say("hi"))     # prints out "Ian: hi"

j = Human("Joel")
print(j.say("hello"))  # prints out "Joel: hello"

# Call our class method
i.get_species()   # => "H. sapiens"

# Change the shared attribute
Human.species = "H. neanderthalensis"
i.get_species()   # => "H. neanderthalensis"
j.get_species()   # => "H. neanderthalensis"

# Call the static method
Human.grunt()   # => "*grunt*"


####################################################
## 6. Modules
####################################################

# You can import modules
import math
print(math.sqrt(16))  # => 4

# You can get specific functions from a module
from math import ceil, floor
print(ceil(3.7))  # => 4.0
print(floor(3.7))   # => 3.0

# You can import all functions from a module.
# Warning: this is not recommended
from math import *

# You can shorten module names
import math as m
math.sqrt(16) == m.sqrt(16)   # => True

# Python modules are just ordinary python files. You
# can write your own, and import them. The name of the
# module is the same as the name of the file.

# You can find out which functions and attributes
# defines a module.
import math
dir(math)


####################################################
## 7. Advanced
####################################################

# Generators help you make lazy code
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# A generator creates values on the fly.
# Instead of generating and returning all values at once it creates one in each
# iteration.  This means values bigger than 15 wont be processed in
# double_numbers.
# Note range is a generator too. Creating a list 1-900000000 would take lot of
# time to be made
# We use a trailing underscore in variable names when we want to use a name that 
# would normally collide with a python keyword
range_ = range(1, 900000000)
# will double all numbers until a result >=30 found
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break


# Decorators
# in this example beg wraps say
# Beg will call say. If say_please is True then it will change the returned
# message
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


print(say())  # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(
```

## Ready For More?

### Free Online

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)

* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [Python Course](http://www.python-course.eu/index.php)

### Dead Tree

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

