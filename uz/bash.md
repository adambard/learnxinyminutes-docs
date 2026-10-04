---
name: Bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
    - ["Jonathan Wang", "https://github.com/Jonathansw"]
    - ["Leo Rudberg", "https://github.com/LOZORD"]
    - ["Betsy Lorton", "https://github.com/schbetsy"]
    - ["John Detter", "https://github.com/jdetter"]
    - ["Harry Mumford-Turner", "https://github.com/harrymt"]
    - ["Martin Nicholson", "https://github.com/mn113"]
    - ["Mark Grimwood", "https://github.com/MarkGrimwood"]
    - ["Emily Grace Seville", "https://github.com/EmilySeville7cfg"]
filename: LearnBash.sh
translators:
    - ["GitHub Copilot", "https://github.com/features/copilot"]
---

Bash - unix qobig'ining nomi bo'lib, u GNU operatsion tizimi uchun qobiq sifatida tarqatilgan
va ko'pchilik Linux distributivlarida standart qobiq sifatida ishlatiladi.
Quyidagi deyarli barcha misollar qobiq skriptining bir qismi bo'lishi yoki
qobiqda to'g'ridan-to'g'ri bajarilishi mumkin.

```bash
#!/usr/bin/env bash
# Skriptning birinchi qatori shebang bo'lib, u tizimga skriptni qanday bajarishni bildiradi
# shebang: https://en.wikipedia.org/wiki/Shebang_(Unix)
# Siz allaqachon tushunganizdek, izohlar # bilan boshlanadi. Shebang ham izoh.

# Oddiy salom dunyo misoli:
echo "Salom dunyo!" # => Salom dunyo!

# Har bir buyruq yangi qatordan boshlanadi yoki nuqta-verguldan keyin:
echo "Bu birinchi buyruq"; echo "Bu ikkinchi buyruq"
# => Bu birinchi buyruq
# => Bu ikkinchi buyruq

# O'zgaruvchi e'lon qilish:
oʻzgaruvchi="Biron bir satr"

# Lekin bunday emas:
oʻzgaruvchi = "Biron bir satr" # => "oʻzgaruvchi: buyruq topilmadi" xatoligi qaytaradi
# Bash `oʻzgaruvchi` buyruq deb hisoblaydi va xatolik beradi
# chunki u topilmaydi.

# Yoki bunday ham emas:
oʻzgaruvchi= "Biron bir satr" # => "Biron bir satr: buyruq topilmadi" xatoligi
# Bash "Biron bir satr" buyruq deb hisoblaydi va xatolik beradi
# chunki u topilmaydi. Bu holatda "oʻzgaruvchi=" qismi "Biron bir satr"
# buyruqning doirasi uchun yaroqli oʻzgaruvchi tayinlash sifatida ko'riladi.

# O'zgaruvchini ishlatish:
echo "$oʻzgaruvchi" # => Biron bir satr
echo '$oʻzgaruvchi' # => $oʻzgaruvchi
# Oʻzgaruvchining o'zini ishlatganingizda — uni tayinlashda, eksport qilishda va boshqalarda —
# uning nomini $ belgisisiz yozasiz. Agar oʻzgaruvchining qiymatini ishlatmoqchi bo'lsangiz, $ ishlatishingiz kerak.
# Esda tuting, ' (bitta tirnoq) oʻzgaruvchilarni kengaytirmaydi!
# Oʻzgaruvchilarni qo'sh tirnoqlarsiz ham yozish mumkin, lekin bu tavsiya etilmaydi
# chunki Bash bo'sh joyli oʻzgaruvchilarni qanday ishlashi bilan bog'liq.

# Parametr kengaytmasi ${...}:
echo "${oʻzgaruvchi}" # => Biron bir satr
# Bu parametr kengaytmasining oddiy ishlatilishi, yuqoridagi ikki misolga o'xshash.
# Parametr kengaytmasi oʻzgaruvchidan qiymat oladi.
# U qiymatni "kengaytiradi" yoki chop etadi.
# Kengaytirish vaqtida qiymat yoki parametr o'zgartirilishi mumkin.
# Quyida ushbu kengaytmaga qo'shiladigan boshqa o'zgarishlar mavjud.

# Oʻzgaruvchilarda satr almashtirish:
echo "${oʻzgaruvchi/Biron/Qandaydir}" # => Qandaydir bir satr
# Bu "Biron" ning birinchi uchrashuvi "Qandaydir" bilan almashtiriladi.

# Oʻzgaruvchidan pastki satr:
uzunlik=7
echo "${oʻzgaruvchi:0:uzunlik}" # => Biron b
# Bu qiymatning faqat birinchi 7 belgisini qaytaradi
echo "${oʻzgaruvchi: -5}" # => r satr
# Bu oxirgi 5 belgini qaytaradi (-5 dan oldin bo'sh joy majburiy).

# Satr uzunligi:
echo "${#oʻzgaruvchi}" # => 16

# Bilvosita kengaytirish:
boshqa_oʻzgaruvchi="oʻzgaruvchi"
echo ${!boshqa_oʻzgaruvchi} # => Biron bir satr
# Bu `boshqa_oʻzgaruvchi` ning qiymatini kengaytiradi.

# Oʻzgaruvchi uchun standart qiymat:
echo "${foo:-"FooBoʻshYokiMavjudBoʻlmasaStandartQiymat"}"
# => FooBoʻshYokiMavjudBoʻlmasaStandartQiymat
# Bu null (foo=) va bo'sh satr (foo="") uchun ishlaydi; nol (foo=0) 0 ni qaytaradi.
# Esda tuting, bu faqat standart qiymatni qaytaradi va oʻzgaruvchi qiymatini o'zgartirmaydi.

# 6 ta elementli massiv e'lon qilish:
massiv=(bir ikki uch tort besh olti)
# Birinchi elementni chop etish:
echo "${massiv[0]}" # => "bir"
# Barcha elementlarni chop etish:
echo "${massiv[@]}" # => "bir ikki uch tort besh olti"
# Elementlar sonini chop etish:
echo "${#massiv[@]}" # => "6"
# Uchinchi elementdagi belgilar sonini chop etish
echo "${#massiv[2]}" # => "3"
# To'rtinchidan boshlab 2 ta elementni chop etish:
echo "${massiv[@]:3:2}" # => "tort besh"
# Barcha elementlarni har birini yangi qatorda chop etish.
for element in "${massiv[@]}"; do
    echo "$element"
done

# O'rnatilgan oʻzgaruvchilar:
# Ba'zi foydali o'rnatilgan oʻzgaruvchilar mavjud:
echo "Oxirgi dasturning qaytarish qiymati: $?"
echo "Skriptning PID: $$"
echo "Skriptga uzatilgan argumentlar soni: $#"
echo "Skriptga uzatilgan barcha argumentlar: $@"
echo "Skriptning turli oʻzgaruvchilarga ajratilgan argumentlari: $1 $2..."

# Jingalak qavsli kengaytirish {...}
# ixtiyoriy satrlar yaratish uchun ishlatiladi:
echo {1..10} # => 1 2 3 4 5 6 7 8 9 10
echo {a..z} # => a b c d e f g h i j k l m n o p q r s t u v w x y z
# Bu boshlang'ich qiymatdan oxirgi qiymatgacha oraliqni chiqaradi.
# Esda tuting, bu yerda oʻzgaruvchilarni ishlatish mumkin emas:
dan=1
gacha=10
echo {$dan..$gacha} # => {$dan..$gacha}

# Endi echo va oʻzgaruvchilarni qanday ishlatishni bilganimizdan so'ng,
# Bash ning boshqa asoslarini o'rganaylik!

# Joriy katalogimiz `pwd` buyrug'i orqali mavjud.
# `pwd` "print working directory" ma'nosini anglatadi.
# Shuningdek, o'rnatilgan `$PWD` oʻzgaruvchisini ham ishlatishimiz mumkin.
# Quyidagilar ekvivalent ekanligini kuzating:
echo "Men $(pwd) da turman" # `pwd` ni bajaradi va chiqishni interpolatsiya qiladi
echo "Men $PWD da turman" # oʻzgaruvchini interpolatsiya qiladi

# Agar terminalingizda yoki skriptdan juda ko'p chiqish olsangiz,
# `clear` buyrug'i ekraningizni tozalaydi:
clear
# Ctrl-L ham chiqishni tozalash uchun ishlaydi.

# Kirishdan qiymat o'qish:
echo "Ismingiz nima?"
read ism
# Esda tuting, yangi oʻzgaruvchi e'lon qilishimiz kerak emas edi.
echo "Salom, $ism!"

# Bizda odatiy if tuzilmasi mavjud.
# Agar $ism qiymati joriy foydalanuvchining login foydalanuvchi nomi bilan teng bo'lmasa, shart rost:
if [[ "$ism" != "$USER" ]]; then
    echo "Sizning ismingiz foydalanuvchi nomingiz emas"
else
    echo "Sizning ismingiz foydalanuvchi nomingiz"
fi

# If so'rovlarida && va || ishlatish uchun, bir nechta kvadrat qavs juftligi kerak:
read yosh
if [[ "$ism" == "Steve" ]] && [[ "$yosh" -eq 15 ]]; then
    echo "Bu $ism Steve bo'lsa VA $yosh 15 bo'lsa ishlaydi."
fi

if [[ "$ism" == "Daniya" ]] || [[ "$ism" == "Zach" ]]; then
    echo "Bu $ism Daniya YOKI Zach bo'lsa ishlaydi."
fi

# Satr bo'sh yoki belgilanmagan ekanligini tekshirish uchun -z va bo'sh emasligini tekshirish uchun -n dan foydalaning
if [[ -z "$ism" ]]; then
    echo "Ism belgilanmagan"
fi

# Raqamlar uchun boshqa taqqoslash operatorlari quyida keltirilgan:
# -ne - teng emas
# -lt - kichik
# -gt - katta
# -le - kichik yoki teng
# -ge - katta yoki teng

# Satrni Regex naqshiga qarshi tekshiruvchi `=~` operator ham mavjud:
email=me@example.com
if [[ "$email" =~ [a-z]+@[a-z]{2,}\.(com|net|org) ]]
then
    echo "Yaroqli email!"
fi

# Shartli bajarish ham mavjud
echo "Har doim bajariladi" || echo "Faqat birinchi buyruq muvaffaqiyatsiz bo'lsa bajariladi"
# => Har doim bajariladi
echo "Har doim bajariladi" && echo "Faqat birinchi buyruq muvaffaqiyatsiz bo'lmasa bajariladi"
# => Har doim bajariladi
# => Faqat birinchi buyruq muvaffaqiyatsiz bo'lmasa bajariladi

# Buyruqdan keyin bitta & ampersand uni fonda ishga tushiradi. Fon buyrug'ining
# chiqishi terminalga chop etiladi, lekin u kirishdan o'qiy olmaydi.
sleep 30 &
# Fon ishlarini ro'yxatlash
jobs # => [1]+  Running                 sleep 30 &
# Fon ishini oldinga olib kelish
fg
# Ctrl-C jarayonni o'ldirish uchun yoki Ctrl-Z uni to'xtatish uchun
# Ctrl-Z bilan to'xtatilganidan keyin fon jarayonini davom ettirish
bg
# 2-raqamli ishni o'ldirish
kill %2
# %1, %2, va boshqalar fg va bg uchun ham ishlatilishi mumkin

# `ping` buyrug'ini faqat 5 ta paket yuborish uchun taxallus sifatida qayta aniqlash
alias ping='ping -c 5'
# Taxallusdan qochish va uning o'rniga shu nomdagi buyruqni ishlatish
\ping 192.168.1.1
# Barcha taxalluslarni chop etish
alias -p

# Ifodalar quyidagi format bilan belgilanadi:
echo $(( 10 + 5 )) # => 15

# Boshqa dasturlash tillaridan farqli o'laroq, bash qobiq bo'lib, u joriy
# katalog kontekstida ishlaydi. Joriy katalogdagi fayllar va kataloglarni
# ls buyrug'i bilan ro'yxatlashingiz mumkin:
ls # Joriy katalogda mavjud fayllar va pastki kataloglarni ro'yxatlaydi

# Bu buyruq uning bajarilishini boshqaradigan parametrlarga ega:
ls -l # Har bir fayl va katalogni alohida qatorda ro'yxatlaydi
ls -t # Katalog mazmunini oxirgi o'zgartirilgan sanaga ko'ra saralaydi (kamayish tartibida)
ls -R # Ushbu katalog va uning barcha pastki kataloglarini rekursiv ravishda `ls` qiladi

# Oldingi buyruqning natijalari (stdout) quvur | yordamida keyingi buyruqqa kirish (stdin) sifatida uzatilishi mumkin.
# Bunday zanjirlangan buyruqlar "quvur liniyasi" deb ataladi va bir vaqtda ishga tushiriladi.
# `grep` buyrug'i berilgan naqshlar bilan kirishni filtrlaydi.
# Joriy katalogdagi .txt fayllarni ro'yxatlash usuli:
ls -l | grep "\.txt"

# Fayllarni stdout ga chop etish uchun `cat` dan foydalaning:
cat fayl.txt

# Faylni `cat` yordamida o'qishimiz ham mumkin:
Mazmun=$(cat fayl.txt)
# "\n" yangi qator belgisini chop etadi
# "-e" yangi qator qochish belgilarini qochish belgilari sifatida talqin qilish uchun
echo -e "FAYL BOSHI\n$Mazmun\nFAYL OXIRI"
# => FAYL BOSHI
# => [fayl.txt ning mazmuni]
# => FAYL OXIRI

# Fayllar yoki kataloglarni bir joydan ikkinchi joyga nusxalash uchun `cp` dan foydalaning.
# `cp` manbalarning YANGI versiyalarini yaratadi,
# shuning uchun nusxani tahrirlash asliga ta'sir qilmaydi (va aksincha).
# Esda tuting, agar manzil allaqachon mavjud bo'lsa, uni ustiga yozadi.
cp manbayiFayl.txt nusxa.txt
cp -r manbaKatalog/ manzil/ # rekursiv ravishda nusxalash

# Kompyuterlar orasida fayl almashishni rejalashtirgan bo'lsangiz, `scp` yoki `sftp` ni ko'rib chiqing.
# `scp` `cp` ga juda o'xshash tarzda ishlaydi.
# `sftp` ko'proq interaktiv.

# Fayllar yoki kataloglarni bir joydan ikkinchi joyga ko'chirish uchun `mv` dan foydalaning.
# `mv` `cp` ga o'xshash, lekin u manbani o'chiradi.
# `mv` fayllarni qayta nomlash uchun ham foydali!
mv m4nb4y.txt manzil.txt # uzr, l33t xakerlar...

# Bash joriy katalog kontekstida ishlagani sababli, siz buyrug'ingizni
# boshqa katalogda bajarishni xohlashingiz mumkin. Manzil o'zgartirish uchun cd mavjud:
cd ~    # uy katalogiga o'tish
cd      # uy katalogiga ham boradi
cd ..   # bir katalog yuqoriga chiqish
        # (^^aytaylik, /home/username/Downloads dan /home/username ga)
cd /home/username/Documents   # belgilangan katalogga o'tish
cd ~/Documents/..    # endi uy katalogida (agar ~/Documents mavjud bo'lsa)
cd -    # oxirgi katalogga o'tish
# => /home/username/Documents

# Kataloglar bo'ylab ishlash uchun subshell lardan foydalaning
(echo "Avval, men shu yerdaman: $PWD") && (cd birorKatalog; echo "Keyin, men shu yerdaman: $PWD")
pwd # hali ham birinchi katalogda

# Yangi kataloglar yaratish uchun `mkdir` dan foydalaning.
mkdir yangiKatalog
# `-p` bayrog'i kerakli yangi oraliq kataloglarni yaratadi.
mkdir -p yangiKatalog/bilan/oraliq/kataloglar
# agar oraliq kataloglar allaqachon mavjud bo'lmagan bo'lsa, yuqoridagi
# buyruqni `-p` bayrog'isiz ishga tushirish xatolik qaytaradi

# Buyruq kirishi va chiqishini (stdin, stdout va stderr) "qayta yo'naltirish operatorlari" yordamida
# qayta yo'naltirishingiz mumkin. Quvurdan farqli o'laroq, quvur chiqishni buyruqqa uzatadi,
# qayta yo'naltirish operatori buyruqning kirishini fayl yoki oqimdan oladi yoki
# uning chiqishini fayl yoki oqimga yuboradi.

# ^EOF$ ga qadar stdin dan o'qing va hello.py ni
# "EOF" orasidagi qatorlar bilan ustiga yozing ("bu yerda hujjat" deb ataladi):
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF
# Birinchi "EOF" qo'shtirnoq ichida bo'lmasa, oʻzgaruvchilar kengaytiriladi

# hello.py Python skriptini turli stdin, stdout va
# stderr qayta yo'naltirishlari bilan ishga tushiring:
python hello.py < "kirish.in" # kirish.in ni skriptga kirish sifatida uzatish

python hello.py > "chiqish.out" # skriptdan chiqishni chiqish.out ga qayta yo'naltirish

python hello.py 2> "xato.err" # xato chiqishini xato.err ga qayta yo'naltirish

python hello.py > "chiqish-va-xato.log" 2>&1
# ham chiqish ham xatolarni chiqish-va-xato.log ga qayta yo'naltirish
# &1 fayl deskriptor 1 (stdout) ni anglatadi, shuning uchun 2>&1 stderr (2) ni stdout (1) ning
# joriy manziliga qayta yo'naltiradi, bu chiqish-va-xato.log ga qayta yo'naltirilgan.

python hello.py > /dev/null 2>&1
# barcha chiqish va xatolarni qora tuynuk /dev/null ga qayta yo'naltirish, ya'ni chiqish yo'q

# Xato chiqishi fayl mavjud bo'lsa ustiga yozadi,
# agar qo'shishni xohlasangiz, ">>" dan foydalaning:
python hello.py >> "chiqish.out" 2>> "xato.err"

# chiqish.out ni ustiga yozish, xato.err ga qo'shish va qatorlarni sanash:
info bash 'Basic Shell Features' 'Redirections' > chiqish.out 2>> xato.err
wc -l chiqish.out xato.err

# Buyruqni ishga tushiring va uning fayl deskriptorini chop eting (masalan, /dev/fd/123)
# qarang: man fd
echo <(echo "#salamdunyo")

# chiqish.out ni "#salamdunyo" bilan ustiga yozish:
cat > chiqish.out <(echo "#salamdunyo")
echo "#salamdunyo" > chiqish.out
echo "#salamdunyo" | cat > chiqish.out
echo "#salamdunyo" | tee chiqish.out >/dev/null

# Vaqtinchalik fayllarni batafsil tozalash (interaktiv uchun '-i' qo'shing)
# DIQQAT: `rm` buyruqlarini bekor qilib bo'lmaydi
rm -v chiqish.out xato.err chiqish-va-xato.log
rm -r vaqtinchKatalog/ # rekursiv ravishda o'chirish
# Tizim axlatiga fayllarni qo'yadigan `trash` ga ega bo'lish uchun
# `trash-cli` Python paketini o'rnatishingiz mumkin va ularni to'g'ridan-to'g'ri o'chirmaydi
# agar ehtiyotkor bo'lishni xohlasangiz https://pypi.org/project/trash-cli/ ga qarang

# Buyruqlar $( ) dan foydalanib boshqa buyruqlar ichida almashtirilishi mumkin:
# Quyidagi buyruq joriy katalogdagi fayllar va kataloglar sonini ko'rsatadi.
echo "Bu yerda $(ls | wc -l) ta element bor."

# Xuddi shu narsa backtic `` yordamida bajarilishi mumkin, lekin ular ichma-ich joylashtirilmaydi -
# afzal qilingan usul $( ) dan foydalanish.
echo "Bu yerda `ls | wc -l` ta element bor."

# Bash Java va C++ dagi switch ga o'xshash `case` operatoridan foydalanadi:
case "$Oʻzgaruvchi" in
    # Qanoatlantirmoqchi bo'lgan shartlar uchun naqshlarni ro'yxatlang
    0) echo "Nol bor.";;
    1) echo "Bir bor.";;
    *) echo "Bu null emas.";; # hamma narsaga mos keladi
esac

# `for` tsikllari berilgan argumentlar soni uchun takrorlanadi:
# $Oʻzgaruvchi ning mazmuni uch marta chop etiladi.
for Oʻzgaruvchi in {1..3}
do
    echo "$Oʻzgaruvchi"
done
# => 1
# => 2
# => 3

# Yoki uni "an'anaviy for tsikli" usulida yozing:
for ((a=1; a <= 3; a++))
do
    echo $a
done
# => 1
# => 2
# => 3

# Ular fayllar ustida ham ishlatilishi mumkin..
# Bu fayl1 va fayl2 da `cat` buyrug'ini ishga tushiradi
for Oʻzgaruvchi in fayl1 fayl2
do
    cat "$Oʻzgaruvchi"
done

# ..yoki buyruq chiqishida
# Bu `ls` chiqishida `cat` ni bajaradi.
for Chiqish in $(ls)
do
    cat "$Chiqish"
done

# Bash naqshlarni ham qabul qilishi mumkin, masalan joriy katalogdagi
# barcha Markdown fayllarini `cat` qilish uchun
for Chiqish in ./*.markdown
do
    cat "$Chiqish"
done

# while tsikli:
while [ true ]
do
    echo "tsikl tanasi shu yerda..."
    break
done
# => tsikl tanasi shu yerda...

# Siz funksiyalarni ham aniqlashingiz mumkin
# Ta'rif:
function foo ()
{
    echo "Argumentlar skript argumentlari kabi ishlaydi: $@"
    echo "Va: $1 $2..."
    echo "Bu funksiya"
    qaytarishQiymati=0    # Oʻzgaruvchi qiymatlari qaytarilishi mumkin
    return $qaytarishQiymati
}
# `foo` funksiyasini ikkita argument bilan chaqiring, arg1 va arg2:
foo arg1 arg2
# => Argumentlar skript argumentlari kabi ishlaydi: arg1 arg2
# => Va: arg1 arg2...
# => Bu funksiya
# Qaytarish qiymatlari $? bilan olinishi mumkin
natija=$?
# 9 dan ortiq argumentlar ham jingalak qavslar yordamida mumkin, masalan ${10}, ${11}, ...

# yoki oddiy
bar ()
{
    echo "Funksiyalarni e'lon qilishning boshqa usuli!"
    return 0
}
# `bar` funksiyasini argumentlarsiz chaqiring:
bar # => Funksiyalarni e'lon qilishning boshqa usuli!

# Funksiyangizni chaqirish
foo "Mening ismim" $Ism

# O'rganishingiz kerak bo'lgan juda ko'p foydali buyruqlar mavjud:
# fayl.txt ning oxirgi 10 qatorini chop etadi
tail -n 10 fayl.txt

# fayl.txt ning birinchi 10 qatorini chop etadi
head -n 10 fayl.txt

# fayl.txt qatorlarini saralangan tartibda chop etish
sort fayl.txt

# takrorlangan qatorlarni hisobot berish yoki tashlab qoldirish, -d bilan ularni hisobot beradi
uniq -d fayl.txt

# ',' belgisidan oldingi faqat birinchi ustunni chop etadi
cut -d ',' -f 1 fayl.txt

# fayl.txt da 'okay' ning har bir uchrashuvi 'great' bilan almashtiriladi
# (regex mos)
sed -i 's/okay/great/g' fayl.txt
# bu -i bayrog'i fayl.txt o'zgartirilishini anglatishiga e'tibor bering
# -i yoki --in-place kirish faylini o'chiradi (zaxira nusxa saqlash uchun --in-place=.backup dan foydalaning)

# fayl.txt ning ba'zi regex ga mos keladigan barcha qatorlarini stdout ga chop etish
# Misol "foo" bilan boshlanib "bar" bilan tugaydigan qatorlarni chop etadi
grep "^foo.*bar$" fayl.txt

# regex ga mos keladigan qatorlar sonini chop etish uchun "-c" opsiyasini uzating
grep -c "^foo.*bar$" fayl.txt

# Boshqa foydali opsiyalar:
grep -r "^foo.*bar$" birorKatalog/ # rekursiv `grep`
grep -n "^foo.*bar$" fayl.txt # qator raqamlarini berish
grep -rI "^foo.*bar$" birorKatalog/ # rekursiv `grep`, lekin ikkilik fayllarni e'tiborsiz qoldirish

# xuddi shu boshlang'ich qidiruvni amalga oshiring, lekin "baz" ni o'z ichiga olgan qatorlarni filtrlang
grep "^foo.*bar$" fayl.txt | grep -v "baz"

# agar siz literal ravishda satrni qidirishni xohlasangiz,
# regex emas, `fgrep` (yoki `grep -F`) dan foydalaning
fgrep "foobar" fayl.txt

# `trap` buyrug'i skriptingiz signal olganda buyruqni bajarishga imkon beradi.
# Bu yerda, `trap` agar u ro'yxatlangan uchta signaldan birini olsa, `rm` ni bajaradi.
trap "rm $TEMP_FILE; exit" SIGHUP SIGINT SIGTERM

# `sudo` superuser sifatida buyruqlarni bajarish uchun ishlatiladi
# odatda u interaktiv ravishda superuser parolini so'raydi
ISM1=$(whoami)
ISM2=$(sudo whoami)
echo "$ISM1 edim, keyin kuchliroq $ISM2 bo'ldim"

# Bash qobig'ining o'rnatilgan buyruqlari hujjatlarini bash `help` o'rnatilgan buyrug'i bilan o'qing:
help
help help
help for
help return
help source
help .

# Bash man sahifa hujjatlarini `man` bilan o'qing
apropos bash
man 1 bash
man bash

# `info` bilan info hujjatlarini o'qing (`?` yordam uchun)
apropos info | grep '^info.*('
man info
info info
info 5 info

# bash info hujjatlarini o'qish:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```

Batafsil ma'lumot uchun [Bash hujjatlarini](https://www.gnu.org/software/bash/manual/bashref.html) ko'ring.
