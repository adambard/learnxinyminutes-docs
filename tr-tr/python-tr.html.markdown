---
language: python
filename: learnpython-tr.py
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["Haydar KULEKCI", "http://scanf.info/"]
lang: tr-tr
---
Python Guido Van Rossum tarafından 90'ların başında yaratılmıştır. Şu anda 
varolanlar arasında en iyi dillerden birisidir. Ben (Louie Dinh) Python
dilinin syntax'ının belirginliğine aşığım. O basit olarak çalıştırılabilir 
pseudocode'dur.

Geri bildirimlerden son derece mutluluk duyarım! Bana [@louiedinh](http://twitter.com/louiedinh)
adresinden ya da louiedinh [at] [google's email service] adresinden ulaşabilirsiniz.

Çeviri için geri bildirimleri de [@kulekci](http://twitter.com/kulekci) 
adresine yapabilirsiniz. 

Not: Bu yazıdaki özellikler Python 2.7 için geçerlidir, ama Python 2.x için de 
uygulanabilir. Python 3 için başka bir zaman tekrar bakınız. 


```python
# Tek satır yorum hash işareti ile başlar.
""" Çoklu satır diziler üç tane çift tırnak 
    arasında yazılır. Ve yorum olarak da 
    kullanılabilir
"""


####################################################
## 1. İlkel Veri Tipleri ve Operatörler
####################################################

# Sayılar
3 #=> 3

# Matematik beklediğiniz gibi
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Bölünme biraz ilginç. EĞer tam sayılar üzerinde bölünme işlemi yapıyorsanız
# sonuç otomatik olarak kırpılır. 
5 / 2 #=> 2

# Bölünme işlemini düzenlemek için kayan noktalı sayıları bilmeniz gerekir.
2.0     # Bu bir kayan noktalı sayı
11.0 / 4.0 #=> 2.75 ahhh...daha iyi

# İşlem önceliğini parantezler ile sağlayabilirsiniz.
(1 + 3) * 2 #=> 8

# Boolean değerleri bilindiği gibi
True
False

# not ile nagatif(mantıksal) değerini alma
not True #=> False
not False #=> True

# Eşitlik ==
1 == 1 #=> True
2 == 1 #=> False

# Eşitsizlik !=
1 != 1 #=> False
2 != 1 #=> True

# Daha fazla karşılaştırma
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# Karşılaştırma zincirleme yapılabilir!
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Karakter dizisi " veya ' ile oluşturulabilir
"This is a string."
'This is also a string.'

# Karakter dizileri birbirleri ile eklenebilir
"Hello " + "world!" #=> "Hello world!"

# A string can be treated like a list of characters
# Bir string'e karakter listesi gibi davranabilirsiniz. 
"This is a string"[0] #=> 'T'

# % karakter dizisini(string) formatlamak için kullanılır, bunun gibi:
"%s can be %s" % ("strings", "interpolated")

# String'leri formatlamanın yeni bir yöntem ise format metodudur. 
# Bu metod tercih edilen yöntemdir.
"{0} can be {1}".format("strings", "formatted")
# Eğer saymak istemiyorsanız anahtar kelime kullanabilirsiniz.
"{name} wants to eat {food}".format(name="Bob", food="lasagna")

# None bir objedir
None #=> None

# "==" eşitliğini non objesi ile karşılaştırmak için kullanmayın.
# Onun yerine "is" kullanın.
"etc" is None #=> False
None is None  #=> True

# 'is' operatörü obje kimliği için test etmektedir. Bu ilkel değerler
# için kullanışlı değildir, ama objeleri karşılaştırmak için kullanışlıdır.

# None, 0 ve boş string/list'ler False olarak değerlendirilir.
# Tüm eşitlikler True döner
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Değişkenler ve Kolleksiyonlar
####################################################

# Ekrana yazdırma oldukça kolaydır.
print "I'm Python. Nice to meet you!"


# Değişkenlere bir değer atamadan önce tanımlamaya gerek yoktur.
some_var = 5    # Değişken isimlerinde gelenek küçük karakter ve alt çizgi 
                # kullanmaktır.
some_var #=> 5

# Daha önceden tanımlanmamış ya da assign edilmemeiş bir değişkene erişmeye 
# çalıştığınızda bir hata fırlatılacaktır. Hata ayıklama hakkında daha fazla 
# bilgi için kontrol akışı kısmına göz atınız.
some_other_var  # isim hatası fırlatılır

# isterseniz "if"i bir ifade gibi kullanabilirsiniz.
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

# Listeler
li = []
# Önceden değerleri tanımlanmış listeler
other_li = [4, 5, 6]

# Bir listenin sonuna birşeyler eklemek
li.append(1)    #li şu anda [1]
li.append(2)    #li şu anda [1, 2]
li.append(4)    #li şu anda [1, 2, 4]
li.append(3)    #li şu anda [1, 2, 4, 3]
# pop ile sondan birşeyler silmek
li.pop()        #=> 3 and li is now [1, 2, 4]
# Tekrar sonuna eklemek
li.append(3)    # li is now [1, 2, 4, 3] again.

# Dizi gibi listenin elemanlarına erişmek
li[0] #=> 1
# Son elemanın değerine ulaşmak
li[-1] #=> 3

# Listede bulunmayan bir index'teki elemana erişirken "IndexError" hatası 
# fırlatılır 
li[4] # IndexError fırlatılır

# slice syntax'ı ile belli aralıktakı değerlere bakabilirsiniz.
# (Açık ve kapalı aralıklıdır.)
li[1:3] #=> [2, 4]
# Başlangıcı ihmal etme
li[2:] #=> [4, 3]
# Sonu ihmal etme
li[:3] #=> [1, 2, 4]

# "del" ile istenilen bir elemanı listeden silmek
del li[2] # li is now [1, 2, 3]

# Listeleri birbiri ile birleştirebilirsiniz.
li + other_li #=> [1, 2, 3, 4, 5, 6] - Not: li ve other_li yanlız bırakılır

# extend ile listeleri birleştirmek
li.extend(other_li) # Now li is [1, 2, 3, 4, 5, 6]

# bir değerin liste içerisinde varlığını "in" ile kontrol etmek
1 in li #=> True

# "len" ile listenin uzunluğunu bulmak
len(li) #=> 6

# Tüpler listeler gibidir sadece değişmezler(immutable)
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # TypeError fırlatılır.

# Litelerde yapılanların hepsini tüplerde de yapılabilir
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Tüplerin(veya listelerin) içerisindeki değerleri değişkenelere
# atanabilir
a, b, c = (1, 2, 3)     # a şu anda 1, b şu anda 2 ve c şu anda 3
# Eğer parantez kullanmaz iseniz tüpler varsayılan olarak oluşturulur
d, e, f = 4, 5, 6
# şimdi iki değeri değiş tokuş etmek çok kolaydır.
e, d = d, e     # d şimdi 5 ve e şimdi 4


# Sözlükler (Dictionaries) key-value saklanır.
empty_dict = {}
# Sözlüklere önceden değer atama örneği
filled_dict = {"one": 1, "two": 2, "three": 3}

# Değere ulaşmak için [] kullanılır
filled_dict["one"] #=> 1

# Tüm anahtarlara(key) "keys()" metodu ile ulaşılır
filled_dict.keys() #=> ["three", "two", "one"]
# Not - Sözlüklerin anahtarlarının sıralı geleceği garanti değildir
# Sonuçlarınız değer listesini aldığınızda tamamen eşleşmeyebilir 

# Tüm değerleri almak için "values()" kullanabilirsiniz.
filled_dict.values() #=> [3, 2, 1]
# Not - Sıralama ile ilgili anahtarlar ile aynı durum geçerlidir.

# Bir anahtarın sözlükte oluş olmadığını "in" ile kontrol edilebilir
"one" in filled_dict #=> True
1 in filled_dict #=> False

# Olmayan bir anahtar çağrıldığında KeyError fırlatılır.
filled_dict["four"] # KeyError

# "get()" metodu KeyError fırlatılmasını önler
filled_dict.get("one") #=> 1
filled_dict.get("four") #=> None
# get() metodu eğer anahtar mevcut değilse varsayılan bir değer atama
# imknaı sağlar.
filled_dict.get("one", 4) #=> 1
filled_dict.get("four", 4) #=> 4

# "setdefault()" metodu sözlüğe yeni bir key-value eşleşmesi eklemenin 
# güvenli bir yoludur.
filled_dict.setdefault("five", 5) #filled_dict["five"] is set to 5
filled_dict.setdefault("five", 6) #filled_dict["five"] is still 5


# Sets store ... well sets
empty_set = set()
# Bir demek değer ile bir "set" oluşturmak
some_set = set([1,2,2,3,4]) # some_set is now set([1, 2, 3, 4])

# Python 2.7'den beri {}'ler bir "set" tanımlaman için kullanılabilir
filled_set = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Bir set'e daha fazla eleman eklemek
filled_set.add(5) # filled_set is now {1, 2, 3, 4, 5}

# "&" işareti ile iki set'in kesişimlerini alınabilir
other_set = {3, 4, 5, 6}
filled_set & other_set #=> {3, 4, 5}

# | işareti ile 
filled_set | other_set #=> {1, 2, 3, 4, 5, 6}

# "-" işareti ile iki set'in farkları alınabilir
{1,2,3,4} - {2,3,5} #=> {1, 4}

# "in" ile değerin set içerisinde olup olmadığını kontrol edebilirsiniz
2 in filled_set #=> True
10 in filled_set #=> False


####################################################
## 3. Akış Denetimi
####################################################

# Bir değişken oluşturmak
some_var = 5

# Buradaki bir if ifadesi. Girintiler(Intentation) Python'da önemlidir!
# "some_var is smaller than 10" yazdırılır.
if some_var > 10:
    print "some_var is totally bigger than 10."
elif some_var < 10:    # elif ifadesi isteğe bağlıdır
    print "some_var is smaller than 10."
else:           # Bu da isteğe bağlıdır.
    print "some_var is indeed 10."


"""
For döngüleri listeler üzerinde iterasyon yapar
Ekrana yazdırılan:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # Biçimlendirmeleri string'e katmak için % kullanabilirsiniz
    print "%s is a mammal" % animal
    
"""
"range(number)" ifadesi sıfırdan verilen sayıya kadar bir sayı listesi döner
Ekrana yazdırılan:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
While döngüsü koşul sağlanmayana kadar devam eder
Ekrana yazdırılan:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Shorthand for x = x + 1

# try/except bloğu ile hatalar ayıklanabilir

# Python 2.6 ve üstü için çalışacaktır:
try:
    # "raise" bir hata fırlatmak için kullanılabilir
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # Pass is just a no-op. Usually you would do recovery here.


####################################################
## 4. Fonksiyonlar
####################################################


# Yeni bir fonksiyon oluşturmak için "def" kullanılır
def add(x, y):
    print "x is %s and y is %s" % (x, y)
    return x + y    # Return values with a return statement

# Fonksiyonu parametre ile çağırmak
add(5, 6) #=> prints out "x is 5 and y is 6" and returns 11

# Diğer bir yol fonksiyonları anahtar argümanları ile çağırmak
add(y=6, x=5)   # Anahtar argümanlarının sırası farklı da olabilir

# Değişken sayıda parametresi olan bir fonksiyon tanımlayabilirsiniz
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)

# Değişken sayıda anahtar argümanlı parametre alan fonksiyonlar da 
# tanımlayabilirsiniz.
def keyword_args(**kwargs):
    return kwargs

# Şu şekilde kullanılacaktır
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

# Eğer isterseniz ikisini aynı anda da yapabilirsiniz
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Fonksiyonu çağırırken, args/kwargs'ın tam tersini de yapabilirsiniz!
# Tüpü yaymak için * ve kwargs'ı yaymak için ** kullanın.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args) # foo(1, 2, 3, 4) ile eşit
all_the_args(**kwargs) # foo(a=3, b=4) ile eşit
all_the_args(*args, **kwargs) # foo(1, 2, 3, 4, a=3, b=4) ile eşit

# Python first-class fonksiyonlara sahiptir
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# Anonymous fonksiyonlar da vardır
(lambda x: x > 2)(3) #=> True

# Dahili yüksek seviye fonksiyonlar vardır
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# Map etme(maps) ve filtreleme(filtres) için liste kullanabiliriz. 
[add_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]


####################################################
## 5. Sınıflar
####################################################

# We subclass from object to get a class.
class Human(object):
    
    # Bir sınıf özelliği. Bu sınıfın tüm "instance"larına paylaşılmıştır.
    species = "H. sapiens"

    # Basic initializer
    def __init__(self, name):
        # Metoda gelen argümanın değerini sınıfın elemanı olan "name" 
        # değişkenine atama
        self.name = name

    # Bir instance metodu. Tüm metodlar ilk argüman olarak "self" 
    # parametresini alır
    def say(self, msg):
       return "%s: %s" % (self.name, msg)

    # Bir sınıf metodu tüm "instance"lar arasında paylaşılır
    # İlk argüman olarak sınıfı çağırarak çağrılırlar
    @classmethod
    def get_species(cls):
        return cls.species

    # Bir statik metod bir sınıf ya da instance referansı olmadan çağrılır
    @staticmethod
    def grunt():
        return "*grunt*"


# Bir sınıf örneği oluşturmak
i = Human(name="Ian")
print i.say("hi")     # "Ian: hi" çıktısı verir

j = Human("Joel")
print j.say("hello")  # "Joel: hello" çıktısı verir

# Sınıf metodunu çağıralım
i.get_species() #=> "H. sapiens"

# Paylaşılan sınıf özellik değiştirelim.
Human.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# Statik metodu çağırma
Human.grunt() #=> "*grunt*"


####################################################
## 6. Modüller
####################################################

# Modülleri sayfaya dahil edebilirsiniz
import math
print math.sqrt(16) #=> 4

# Modül içerisinden spesifik bir fonksiyonu getirebilirsiniz
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# Modüldeki tüm fonksiyonları dahil edebilirsiniz
# Uyarı: bu önerilmez
from math import *

# Modülün adını kısaltabilirsiniz
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Python modülleri sıradan python dosyalarıdır. Kendinize bir modül 
# yazabilirsiniz, ve dahil edebilirsiniz. Modülün adı ile dosya adı
# aynı olmalıdır.

# Modüllerde tanımlanmış fonksiyon ve metodları öğrenebilirsiniz.
import math
dir(math)



```

## Daha fazlası için hazır mısınız?

### Ücretsiz Dökümanlar

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### Dead Tree

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
