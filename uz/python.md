---
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
    - ["Rommel Martinez", "https://ebzzry.io"]
    - ["Roberto Fernandez Diaz", "https://github.com/robertofd1995"]
    - ["caminsha", "https://github.com/caminsha"]
    - ["Stanislav Modrak", "https://stanislav.gq"]
    - ["John Paul Wohlscheid", "https://gitpi.us"]
translators:
    - ["GitHub Copilot", "https://github.com/features/copilot"]
---

Python Guido van Rossum tomonidan 90-yillarning boshida yaratilgan. U hozir 
mavjud bo'lgan eng mashhur tillardan biri. Men Python ga uning sintaktik 
aniqligi uchun oshiq bo'ldim. Bu asosan bajariladigan psevdokod.

```python
# Bir qatorli izohlar raqam belgisi bilan boshlanadi.

""" Ko'p qatorli satrlar uchta " yordamida
    yozilishi mumkin va ko'pincha
    hujjat sifatida ishlatiladi.
"""

####################################################
## 1. Asosiy ma'lumot turlari va operatorlar
####################################################

# Sizda raqamlar bor
3  # => 3

# Matematika siz kutganday
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# Butun bo'lish manfiy cheksizlik tomon yaxlitlaydi
5 // 3       # => 1
-5 // 3      # => -2
5.0 // 3.0   # => 1.0  # floatlar uchun ham ishlaydi
-5.0 // 3.0  # => -2.0

# Bo'lish natijasi har doim float
10.0 / 3  # => 3.3333333333333335

# Modul operatsiyasi
7 % 3   # => 1

# Darajaga ko'tarish (x**y, x ning y-darajasi)
2**3  # => 8

# Qavslar bilan ustunlikni ta'minlash
1 + 3 * 2    # => 7
(1 + 3) * 2  # => 8

# Boolean qiymatlari primitiv (Eslatma: bosh harflar)
True   # => True
False  # => False

# not bilan inkor qilish
not True   # => False
not False  # => True

# Boolean operatorlar
# "and" va "or" katta-kichik harflarga sezgir
True and False  # => False
False or True   # => True

# Tenglik ==
1 == 1  # => True
2 == 1  # => False

# Notenglik !=
1 != 1  # => False
2 != 1  # => True

# Boshqa taqqoslashlar
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Zanjir taqqoslashlar
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# Satrlar " yoki ' bilan yaratiladi
"Bu bir satr."
'Bu ham bir satr.'

# Satrlarni qo'shish mumkin
"Salom " + "dunyo!"  # => "Salom dunyo!"

# Satr belgilar ro'yxati kabi ishlatilishi mumkin
"Salom dunyo!"[0]  # => 'S'

# Satr uzunligini topish
len("Bu bir satr")  # => 11

# Python 3.6 dan boshlab f-satrlardan foydalanishingiz mumkin
ism = "Ahmad"
f"U o'z ismini {ism} dedi."  # => "U o'z ismini Ahmad dedi"

# None obyekt
None  # => None

# None bilan taqqoslash uchun "==" emas, "is" dan foydalaning
"boshqa" is None  # => False
None is None      # => True

####################################################
## 2. O'zgaruvchilar va kolleksiyalar
####################################################

# Python da print funksiyasi bor
print("Men Python. Tanishganimdan xursandman!")

# Konsol orqali ma'lumot olishning oddiy usuli
kirish_matn = input("Ma'lumot kiriting: ")  # Ma'lumotni satr sifatida qaytaradi

# E'lonlar yo'q, faqat tayinlashlar
# O'zgaruvchilarni nomlashda snake_case uslubi qo'llaniladi
biror_ozgaruvchi = 5
biror_ozgaruvchi  # => 5

# Ro'yxatlar ketma-ketliklarni saqlaydi
li = []
# Oldindan to'ldirilgan ro'yxat bilan boshlashingiz mumkin
boshqa_li = [4, 5, 6]

# Ro'yxat oxiriga append bilan element qo'shing
li.append(1)    # li endi [1]
li.append(2)    # li endi [1, 2]
li.append(4)    # li endi [1, 2, 4]
li.append(3)    # li endi [1, 2, 4, 3]

# Oxirgi elementni pop bilan olib tashlang
li.pop()        # => 3 va li endi [1, 2, 4]

# Qaytarib joylaymiz
li.append(3)    # li yana [1, 2, 4, 3]

# Ro'yxat elementlariga massiv kabi kirish
li[0]   # => 1
# Oxirgi elementni ko'rish
li[-1]  # => 3

# Ro'yxat uzunligi
len(li)  # => 4

# Range bu raqamlar ketma-ketligi
# range(boshlanish, tugash, qadam)
for i in range(4):
    print(i)  # 0, 1, 2, 3 ni chop etadi

for i in range(4, 8):
    print(i)  # 4, 5, 6, 7 ni chop etadi

for i in range(4, 8, 2):
    print(i)  # 4, 6 ni chop etadi

####################################################
## 3. Boshqaruv oqimi va iteratorlar
####################################################

# O'zgaruvchi yaratamiz
some_var = 5

# Bu if operatori. Chekinish Python da muhim!
# "some_var 10 dan katta" ni chop etadi
if some_var > 10:
    print("some_var 10 dan ham katta.")
elif some_var < 10:    # Bu elif bandini ixtiyoriy
    print("some_var 10 dan kichik.")
else:                  # Bu ham ixtiyoriy
    print("some_var aynan 10.")

# For tsikllari ro'yxatlar ustida takrorlaydi
# Chop etadi:
#    it 0
#    mushuk 1
#    sichqon 2
for hayvon in ["it", "mushuk", "sichqon"]:
    print(f"{hayvon} {['it', 'mushuk', 'sichqon'].index(hayvon)}")

# While tsikllari shart rost bo'lgunga qadar ishlaydi
x = 0
while x < 4:
    print(x)
    x += 1  # x = x + 1 uchun qisqartma

####################################################
## 4. Funktsiyalar
####################################################

# def bilan yangi funktsiya yarating
def qoshish(x, y):
    print(f"x {x} va y {y}")
    return x + y  # return operatori bilan qiymat qaytaring

# Funktsiyani chaqirish
qoshish(5, 6)  # => "x 5 va y 6" ni chop etadi va 11 qaytaradi

# Kalit so'z argumentlari bilan funktsiyani chaqirishning boshqa usuli
qoshish(y=6, x=5)  # Kalit so'z argumentlari tartibi ahamiyatsiz

# O'zgaruvchan miqdordagi pozitsiya argumentlari bilan funktsiya
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# O'zgaruvchan miqdordagi kalit so'z argumentlari bilan ham funktsiya yozishingiz mumkin
def keyword_args(**kwargs):
    return kwargs

# Uni chaqirib ko'ramiz
keyword_args(katta="poyafzal", kichik="mushuk")  # => {"katta": "poyafzal", "kichik": "mushuk"}

####################################################
## 5. Sinflar
####################################################

# Sinfni yaratish uchun "class" operatoridan foydalanamiz
class Odam:
    # Sinf atributi. Sinf ning barcha nusxalari tomonidan baham ko'riladi
    tur = "H. sapiens"

    # Asosiy initsializator. Bu obyekt yaratilganda chaqiriladi.
    def __init__(self, ism):
        # Argumentni obyektning ism atributiga tayinlash
        self.ism = ism

    # Nusxa usuli. Barcha usullar "self" ni birinchi argument sifatida oladi
    def gap_ayt(self):
        return f"Salom, men {self.ism}"

    # Sinf usuli barcha nusxalar orasida baham ko'riladi
    @classmethod
    def tur_ol(cls):
        return cls.tur

# Sinf nusxasini yaratish
i = Odam("Ian")
print(i.gap_ayt())  # "Salom, men Ian" ni chop etadi

j = Odam("Joel")
print(j.gap_ayt())  # "Salom, men Joel" ni chop etadi

# Sinf usulini chaqirish
i.tur_ol()  # => "H. sapiens"

# Baham ko'rilgan atributni o'zgartirish
Odam.tur = "H. neanderthalensis"
i.tur_ol()  # => "H. neanderthalensis"
j.tur_ol()  # => "H. neanderthalensis"

```

## Qo'shimcha ma'lumotlar

* [Python rasmiy qo'llanmasi](https://docs.python.org/)
* [Python.org dagi o'quv materiali](https://docs.python.org/tutorial/)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/) - bepul onlayn kitob