---
language: Python
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Batuhan Osman T.", "https://github.com/BTaskaya"]
translators:
    - ["Eray AYDIN", "http://erayaydin.me/"]
lang: tr-tr
filename: learnpython-tr.py
---

Python,90ların başlarında Guido Van Rossum tarafından oluşturulmuştur. En popüler olan dillerden biridir. Beni Python'a aşık eden sebep onun syntax beraklığı. Çok basit bir çalıştırılabilir söz koddur.

Not: Bu makale Python 3 içindir. Eğer Python 2.7 öğrenmek istiyorsanız [burayı](http://learnxinyminutes.com/docs/pythonlegacy/) kontrol edebilirsiniz.

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
## 3. Kontrol Akışları ve Temel Soyutlandırma
####################################################

# Bir değişken oluşturalım
bir_degisken = 5

# Burada bir "if" ifadesi var. Girinti(boşluk,tab) python için önemlidir!
# çıktı olarak "bir_degisken 10 dan küçük" yazar
if bir_degisken > 10:
    print("bir_degisken 10 dan büyük")
elif bir_degisken < 10:    # Bu 'elif' ifadesi zorunlu değildir.
    print("bir_degisken 10 dan küçük")
else:                  # Bu ifade de zorunlu değil.
    print("bir_degisken değeri 10")


"""
Döngülerle lsiteleri döngüye alabilirsiniz
çıktı:
    köpek bir memeli hayvandır
    kedi bir memeli hayvandır
    fare bir memeli hayvandır
"""
for hayvan in ["köpek", "kedi, "fare"]:
    # format ile kolayca yazıyı biçimlendirelim
    print("{} bir memeli hayvandır".format(hayvan))

"""
"range(sayi)" bir sayı listesi döndür
0'dan belirttiğiniz sayıyıa kadar
çıktı:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
'While' döngüleri koşul çalıştıkça işlemleri gerçekleştirir.
çıktı:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Uzun hali x = x + 1

# Hataları kontrol altına almak için try/except bloklarını kullanabilirsiniz
try:
    # Bir hata oluşturmak için "raise" kullanabilirsiniz
    raise IndexError("Bu bir index hatası")
except IndexError as e:
    pass    # Önemsiz, devam et.
except (TypeError, NameError):
    pass    # Çoklu bir şekilde hataları kontrol edebilirsiniz, tabi gerekirse.
else:   # İsteğe bağlı bir kısım. Eğer hiçbir hata kontrol mekanizması desteklemiyorsa bu blok çalışacaktır
    print("Her şey iyi!")   # IndexError, TypeError ve NameError harici bir hatada bu blok çalıştı

# Temel Soyutlandırma, bir objenin işlenmiş halidir.
# Aşağıdaki örnekte; Obje, range fonksiyonuna temel soyutlandırma gönderdi.

dolu_sozl = {"bir": 1, "iki": 2, "uc": 3}
temel_soyut = dolu_sozl.keys()
print(temel_soyut) #=> range(1,10). Bu obje temel soyutlandırma arayüzü ile oluşturuldu

# Temel Soyutlandırılmış objeyi döngüye sokabiliriz.
for i in temel_soyut:
    print(i)    # Çıktısı: bir, iki, uc

# Fakat, elementin anahtarına değerine.
temel_soyut[1]  # TypeError hatası!

# 'iterable' bir objenin nasıl temel soyutlandırıldığıdır.
iterator = iter(temel_soyut)

# 'iterator' o obje üzerinde yaptığımız değişiklikleri hatırlayacaktır
# Bir sonraki objeyi almak için __next__ fonksiyonunu kullanabilirsiniz.
iterator.__next__()  #=> "bir"

# Bir önceki __next__ fonksiyonumuzu hatırlayıp bir sonraki kullanımda bu sefer ondan bir sonraki objeyi döndürecektir
iterator.__next__()  #=> "iki"
iterator.__next__()  #=> "uc"

# Bütün nesneleri aldıktan sonra bir daha __next__ kullanımınızda, StopIterator hatası oluşturacaktır.
iterator.__next__() # StopIteration hatası

# iterator'deki tüm nesneleri almak için list() kullanabilirsiniz.
list(dolu_sozl.keys())  #=> Returns ["bir", "iki", "uc"]


####################################################
## 4. Fonksiyonlar
####################################################

# "def" ile yeni fonksiyonlar oluşturabilirsiniz
def topla(x, y):
    print("x = {} ve y = {}".format(x, y))
    return x + y    # Değer döndürmek için 'return' kullanmalısınız

# Fonksiyonu parametleri ile çağırıyoruz
topla(5, 6)   # => çıktı "x = 5 ve y = 6" ve değer olarak 11 döndürür

# Bir diğer fonksiyon çağırma yöntemi de anahtar değerleri ile belirtmek
topla(y=6, x=5)   # Anahtar değeri belirttiğiniz için parametre sıralaması önemsiz.

# Sınırsız sayıda argüman da alabilirsiniz
def argumanlar(*argumanlar):
    return argumanlar

argumanlar(1, 2, 3)   # => (1, 2, 3)

# Parametrelerin anahtar değerlerini almak isterseniz
def anahtar_par(**anahtarlar):
    return anahtar

# Çalıştırdığımızda
anahtar_par(anah1="deg1", anah2="deg2")   # => {"anah1": "deg1", "anah2": "deg2"}


# İsterseniz, bu ikisini birden kullanabilirsiniz
def tum_argumanlar(*argumanlar, **anahtarla):
    print(argumanlar)
    print(anahtarla)
"""
tum_argumanlar(1, 2, a=3, b=4) çıktı:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Fonksiyonu çağırırken de aynısını kullanabilirsiniz
argumanlar = (1, 2, 3, 4)
anahtarla = {"a": 3, "b": 4}
tum_argumanlar(*argumanlar)   # = foo(1, 2, 3, 4)
tum_argumanlar(**anahtarla)   # = foo(a=3, b=4)
tum_argumanlar(*argumanlar, **anahtarla)   # = foo(1, 2, 3, 4, a=3, b=4)


# Fonksiyonlarda kullanacağımız bir değişken oluşturalım                                  
x = 5

def belirleX(sayi):
    # Fonksiyon içerisindeki x ile global tanımladığımız x aynı değil
    x = sayi # => 43
    print (x) # => 43
    
def globalBelirleX(sayi):
    global x
    print (x) # => 5
    x = sayi # global olan x değişkeni artık 6
    print (x) # => 6

belirleX(43)
globalBelirleX(6)


# Sınıf fonksiyonları oluşturma
def toplama_olustur(x):
    def topla(y):
        return x + y
    return topla

ekle_10 = toplama_olustur(10)
ekle_10(3)   # => 13

# Bilinmeyen fonksiyon
(lambda x: x > 2)(3)   # => True

# TODO - Fix for iterables
# Belirli sayıdan yükseğini alma fonksiyonu
map(ekle_10, [1, 2, 3])   # => [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# Filtreleme işlemi için liste comprehensions da kullanabiliriz
[ekle_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]

####################################################
## 5. Sınıflar
####################################################


# Sınıf oluşturmak için objeden alt sınıf oluşturacağız.
class Insan(object):

    # Sınıf değeri. Sınıfın tüm nesneleri tarafından kullanılabilir
    tur = "H. sapiens"

    # Basit başlatıcı, Sınıf çağrıldığında tetiklenecektir.
    # Dikkat edin, iki adet alt çizgi(_) bulunmakta. Bunlar
    # python tarafından tanımlanan isimlerdir. 
    # Kendinize ait bir fonksiyon oluştururken __fonksiyon__ kullanmayınız!
    def __init__(self, isim):
        # Parametreyi sınıfın değerine atayalım
        self.isim = isim

    # Bir metot. Bütün metotlar ilk parametre olarak "self "alır.
    def soyle(self, mesaj):
        return "{isim}: {mesaj}".format(isim=self.isim, mesaj=mesaj)

    # Bir sınıf metotu bütün nesnelere paylaştırılır
    # İlk parametre olarak sınıf alırlar
    @classmethod
    def getir_tur(snf):
        return snf.tur

    # Bir statik metot, sınıf ve nesnesiz çağrılır
    @staticmethod
    def grunt():
        return "*grunt*"


# Sınıfı çağıralım
i = Insan(isim="Ahmet")
print(i.soyle("merhaba"))     # çıktı "Ahmet: merhaba"

j = Insan("Ali")
print(j.soyle("selam"))  # çıktı "Ali: selam"

# Sınıf metodumuzu çağıraim
i.getir_tur()   # => "H. sapiens"

# Paylaşılan değeri değiştirelim
Insan.tur = "H. neanderthalensis"
i.getir_tur()   # => "H. neanderthalensis"
j.getir_tur()   # => "H. neanderthalensis"

# Statik metodumuzu çağıralım
Insan.grunt()   # => "*grunt*"


####################################################
## 6. Moduller
####################################################

# Modülleri içe aktarabilirsiniz
import math
print(math.sqrt(16))  # => 4.0

# Modülden belirli bir fonksiyonları alabilirsiniz
from math import ceil, floor
print(ceil(3.7))  # => 4.0
print(floor(3.7))   # => 3.0

# Modüldeki tüm fonksiyonları içe aktarabilirsiniz
# Dikkat: bunu yapmanızı önermem.
from math import *

# Modül isimlerini değiştirebilirsiniz.
# Not: Modül ismini kısaltmanız çok daha iyi olacaktır
import math as m
math.sqrt(16) == m.sqrt(16)   # => True

# Python modulleri aslında birer python dosyalarıdır.
# İsterseniz siz de yazabilir ve içe aktarabilirsiniz Modulün
# ismi ile dosyanın ismi aynı olacaktır.

# Moduldeki fonksiyon ve değerleri öğrenebilirsiniz.
import math
dir(math)


####################################################
## 7. Gelişmiş
####################################################

# Oluşturucular uzun uzun kod yazmamanızı sağlayacak ve yardımcı olacaktır
def kare_sayilar(nesne):
    for i in nesne:
        yield i + i

# Bir oluşturucu(generator) değerleri anında oluşturur.
# Bir seferde tüm değerleri oluşturup göndermek yerine teker teker her oluşumdan
# sonra geri döndürür.  Bu demektir ki, kare_sayilar fonksiyonumuzda 15'ten büyük
# değerler işlenmeyecektir.
# Not: range() da bir oluşturucu(generator)dur. 1-900000000 arası bir liste yapmaya çalıştığınızda
# çok fazla vakit alacaktır.
# Python tarafından belirlenen anahtar kelimelerden kaçınmak için basitçe alt çizgi(_) kullanılabilir. 
range_ = range(1, 900000000)
# kare_sayilar'dan dönen değer 30'a ulaştığında durduralım
for i in kare_sayilar(range_):
    print(i)
    if i >= 30:
        break


# Dekoratörler
# Bu örnekte,
# Eğer lutfen_soyle True ise dönen değer değişecektir.
from functools import wraps


def yalvar(hedef_fonksiyon):
    @wraps(hedef_fonksiyon)
    def metot(*args, **kwargs):
        msj, lutfen_soyle = hedef_fonksiyon(*args, **kwargs)
        if lutfen_soyle:
            return "{} {}".format(msj, "Lütfen! Artık dayanamıyorum :(")
        return msj

    return metot


@yalvar
def soyle(lutfen_soyle=False):
    msj = "Bana soda alır mısın?"
    return msj, lutfen_soyle


print(soyle())  # Bana soda alır mısın?
print(soyle(lutfen_soyle=True))  # Ban soda alır mısın? Lutfen! Artık dayanamıyorum :(
```

## Daha Fazlasına Hazır Mısınız?

### Ücretsiz Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)
* [Python 3 Computer Science Circles](http://cscircles.cemc.uwaterloo.ca/)

### Kitaplar

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

