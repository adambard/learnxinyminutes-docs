---
name: ruby
language: ruby
filename: learnruby-tr.rb
contributors:
  - ["Seçkin KÜKRER", "https://github.com/LeaveNhA"]
lang: tr-tr
---

# Dile nazik bir giriş.

## Ruby Nedir ?

Ruby, doğrudan bir Google aramasıyla aklınızdakini bulmanız zor olabilir. İngilizce bu kelime, `Ruby` (IPA: ˈruːbi) "kırmızı taş" anlamına gelen Fransızca kökenli bir kelime olan `rubi`'den gelmektedir.

Yaratıcısı tarafından, yine esinlenilen bir dil olarak ortaya çıkan `Ruby`, Perl, Smalltalk, Eiffel, Ada, Lisp programlama dillerinin en iyi özelliklerini almıştır. ! [İmperativ]() programlama mentalitesi üzerine kurmayı seçtiği bu teknoloji, günümüzde sektöründe öncü.


## Tarihçe

Ruby 1995’te halka duyurulduğundan beri, dünya çapında programcıların dikkatini çekmeye başlamıştır. 2006 Ruby’nin altın yılı olmuştur. Dünyanın en büyük şehirlerinde aktif kullanıcı grupları ve Ruby ile ilgili konferanslar gerçekleştirilmiştir.

Daha sonraları `Ruby`, dünya çapında programlama dillerinin büyümesini ve popülaritesini ölçen dizinlerin (TIOBE dizini gibi) çoğunda ilk 10 içinde yer almıştır. Büyümenin çoğu, Ruby ile yazılmış yazılımların popülaritesiyle ilgilidir, özellikle de Ruby on Rails web çatısıyla.

! [kaynak]()

## Sektördeki Konumu ve Geleceği ?

Çoğu uzmana göre, şu anda sadece `Rails` teknolojisi için bir betik dili olarak sıkışmış durumda.

Bazıları ise, dilin kendi geleceğini, 2020 içinde yayınlanması planlanan `Ruby 3` ile sağlamlaştıracağını ve yeni imkanlar ve sektörek kullanım ve tercihler ile popüleritesinin artacağını düşünüyor.

## Her Şey Nesne

Matz'ın incelemiş olduğu diller sonucunda, teknik olarak en iyi sözdizimin kaynağını “Perl’den daha güçlü ama Python’dan daha nesneye yönelik bir betik dili” olarak tanımlamış.

Her şeyin Nesne olarak görüldüğü bir programlama teknolojisi, bütünlük kavramı açısından herkese kucak açan bir pürüzsüzlük sunuyor. `Ruby`'nin neden tartışmasız, saf bir Nesne yönelimli bir programlama dili olduğuna dair örnekleri aşağıda vereceğim.

## Diğer Gerçeklemeler

- [JRuby](http://jruby.org/), JVM’in (Java Virtual Machine) üstünde çalışan Ruby’dir, JVM’in eniyileyen JIT derleyicisi, çöp toplayıcısı, eşzamanlı thread’leri, araç ekosistemi, ve muazzam sayıdaki kütüphanelerinden faydalanır.
- [Rubinius](http://rubini.us/), ‘Ruby’da yazılmış Ruby’dir’. LLVM’in üstüne inşa edilmiştir ve ayrıca diğer dillerin üstüne inşa edebilecekleri şık bir sanal makine de sunar.
- [TruffleRuby](https://github.com/oracle/truffleruby), GraalVM’in üstünde çalışan yüksek performanslı bir Ruby gerçeklemesidir.
- [IronRuby](http://www.ironruby.net/), “.NET Web Çatısı’yla sıkı sıkıya bağlı” bir gerçeklemedir.

Diğer gerçeklemeler için, lütfen ileri okumaya danışınız.

```ruby
# Bu karakter ile başlayan satırlar, yorum satırı olarak değerlendirilir.
# Diğer yorum satırı tanımlamaları için tanımlamalar ve ifadeler kısmına danışın.

## Örnek yapısı; bu örnek dosyadaki her Ruby ifadesi, Ruby yorumlayıcısı
## tarafından yorumlanarak sonucu `=>` ifadesinin sağına yazılır.
## örnek ifade #=> örnek sonuç
## formatındadır.
## Bazen satır aşımını önlemek için
## örnek ifade
## #=> örnek sonuç
## şeklinde yer verilecektir.

# --------------------------------
# Veriler ve Temsilleri
# --------------------------------

## --
## Sayılar:
## --
### Ruby, tamsayı veri tipini destekler. Sayısal değerlerin sisteminizdeki temsili
### bu veri yapısıdır.

# Tam sayı örneği.
1453 #=> 1453 

## Okunabilirlik için, binlik ya da ondalık kısmını `_` ile
## ayırmak mümkündür ve bu karakter tümüyle görmezden gelinir.
3_14 #=> 314

## Negatif sayılar `-` ile başlıyor.
-3750 #=> -3750

## Oktal sayılar
03603 #=> 1923

## Onaltılık sayılar
0x23B #=> 571

## İkilik sayılar
0b11110000011 #=> 1923

## Büyük sayılar temsili
12345678901234567890  #=> 12345678901234567890

## Kayan noktalı sayılar

## Bir kayan-noktalı/Ondalıklı sayı.
3.14 #=> 3.14

## Bilimsel notasyon
1.0e3 #=> 1000.0

## Bir ipucu,
## üsten önce işaret!
3e+9 #=> 3000000000.0

## --
# Mantıksal Değerler
## --

## Mantıksal doğru ifadesi.
true #=> true

## Mantıksal yanlış ifadesi.
false #=> false

## --
# Metinler
## --

## Metin sabitleri
'Bu, bir metin ifadesi.'

## Kaçışlar için 
'Kaçışlar için "\\"' #=> "Kaçışlar için \"\\\""

## Alternatif ise çift tırnaklı ifadeler.
"Bu da bir metin ifadesi."

## Kaçışlarda farkı ise,
"Kaçılar için '\\'" #=> "Kaçılar için '\\'"
## bazı kaçış notasyonlarına gerek kalmaması.

## Bazı notasyon karakterleri

### Yeni Satır (New Line 0x0a)
"\n" #=> "\n"

### Boşluk (Space 0x20)
"\s" #=> "\s"

## --
# Karakterler 
## --

## Basitçe önlerine soru işareti getirilmiş
## tek karakter sabitleridir.
?a #=> "a"


## --
# Semboller
## --
## Ruby'de semboller, temsilleri bakımından 
## Clojure'daki semboller ile benzerlerdir.
:sembol #=> :sembol

## Kendileri, birinci sınıf değerdir.
:türk.class #=> Symbol
## Ve aynı zamanda Unicode desteği vardır. (1.9 sürümünden beri)


## --
# Diziler
## --
## Basitçe, Ruby dizileri birilerinden virgül ile ayrılmış,
## değer veya değer sahibi referansların köşeli parantezler
## ile çevrelenmesi ile oluşturulur. ([])
[1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

## Metinler için de durum aynı.
["Mustafa", "Kemal", "ATATÜRK"] #=> ["Mustafa", "Kemal", "ATATÜRK"]

## Aynı zamanda, Ruby dizileri tip bağımsız nesne ardışıklarıdır.
[1881, "Mustafa", "Kemal", "ATATÜRK", 1923, "∞"]
## Aynı zamanda Unicode destekler (1.9 sürümünden beri)

## --
# Eşlemeler
## --
## Ruby eşlemeleri, süslü parantezler içinde virgül ile ayrılan,
## anahtar ve değer ikililieridir.

## Bir tane de olabilir,
{"izmir" => "kızları"} #=> {"izmir" => "kızları"}

## Ya da, birden fazla...
{"izmir" => "kızları", "paris" => "sokakları"} #=> {"izmir" => "kızları", "paris" => "sokakları"}

## Aslında her değeri anahtar veya değer olarak
## yerleştirmek mümkün.

## Sembolleri,
{:zafer => "30 Ağustos!"} #=> {:zafer=>"30 Ağustos!"}

## Rakamları bile.
{28101923 => "Efendiler, yarın Cumhuriyeti'i ilân edeceğiz!"}
#=> {28101923=>"Efendiler, yarın Cumhuriyeti'i ilân edeceğiz!"}

## Semboller için ufak bir sözdizimsel şekerleme mevcut ki,
{istanbul: "beyefendi"} #=> {:istanbul=>"beyefendi"}
## Bu kullanıma göre, sembol anahtarlar ile değerler arasına
## `=>` yerine basitçe sembolün başına gelecek `:` sembolü
## getiriliyor.


## --
# Aralıklar
## --
## Ruby aralıkları temeliyle başlangıç ve bitiş
## değerleri arasındaki aralığın veriye dönüştürülmesi
## için bir dil olanağıdır.

## (başlangıç..bitiş) notasyonu kullanılabilir.
(0..10) #=> 0..10
## REPL'ın bize verdiği ifade sizi yanıltmasın, bu bir aralıktır.
## Meraklılarıyla, dökümanın devamında içindeki değerleri
## gezeceğiz.

## Range.new notasyonu ile de ilklenebilirler.
Range.new(0, 10) #=> 0..10

## --
# Düzenli İfadeler
## --
## İki / operatörünün ortasına tanımlanırlar.
//.class #=> Regexp

## Örnek bir düzenli ifade, a harfi için.
/[a]/ #=> /[a]/

# --------------------------------
# Değelerin Manipüle edilmesi
# --------------------------------

## --
## Rakamlar
## --

## Aritmatik, bildiğimiz şekilde.
## !! infix notasyon

235 + 1218 #=> 1453
123 - 35 #=> 88
2 * 2 #=> 4
1 / 1 #=> 1

## Bit tabanlı işlemler.
2 & 5 #=> 0
3 | 9 #=> 11
2 ^ 5 #=> 7
## Aslında C tipi ailesi dillerdeki gibi. Sezgisel bir yaklaşımla, hayatta kalınabilir.
## Ama yine de dökümantasyona başvurulmasını tavsiye ederim.


## --
## Mantıksal
## --

## ! operatörü teklidir, ve aldığı değerin mantıksal tersini alır.
!true #=> false
!false #=> true


## --
## Metinler
## --

### Boş mu ?
"".empty? #=> true

### Bir bölümünü alalım.
"Ölürüm TÜRKİYEM!".slice(7, 7) #=> "Türkiye"
## Bir başka şekilde, indis notasyonu ile,
"Ölürüm Türkiye'm!"[7, 7] #=> "Türkiye"

## Küçük harfe dönüştürelim
"LAY-LAY-LOM sana göre sevmeler...".downcase
#=> "lay-lay-lom sana göre sevmeler..."

## Büyük harfa dönüştürelim
"beşiktaş".upcase #=> "BEŞIKTAŞ"

## Karakterlerine ayıralım
"BEŞİKTAŞ".chars #=> ["B", "E", "Ş", "İ", "K", "T", "A", "Ş"]

## Çevrelemek için
"Ahmet Mete IŞIKARA".center(30)
#=> "      Ahmet Mete IŞIKARA      "

## İçerik kontrolü için include metodu
"aşk".include?(?a) #=> true
## argümanı metin tipinde de verebilirdik, ama
## yukarıdaki temsillerde gördüğümüz gibi,
## yorumlayıcı, karakter sabitini metin olarak işliyor zaten.

## Konumunu alalım.
"Dayı".index("a") #=> 1
## Elbette, tasarımında sağlıklı kararlar alınmış her
## dil gibi, Ruby'de 0'dan saymaya başlıyor.

## Metin yerleştirme yapalım
"Ali Baba'nın x çiftliği var.".sub("x", "bir")
#=> "Ali Baba'nın bir çiftliği var."

## Birden fazla eşleşme için, değiştirme yapalım
"Dal sarkar x kalkar, x kalkar dal sarkar.".gsub("x", "kartal")
#=> "Dal sarkar kartal kalkar, kartal kalkar dal sarkar."

## Düzenli ifadeler ile, cümledeki sesli harfleri değiştirelim.
"Bir berber bir bere...".gsub(/[ie]/, "*")
#=> "B*r b*rb*r b*r b*r*..."

## Diğer işlevler için ileri okumadaki kaynağa başvurunuz.


## --
## Eşlemeler
## --

## basit bir eşleme ile başlayalım.
{:boy => 1.74} #=> {:boy => 1.74}

## Belirli bir anahtar, eşlememizde barınıyor mu diye
## kontrol ediyoruz.
{:boy => 1.74}.has_key? :boy
#=> true
## Parantezlerin yokluğu sizi yanıltmasın,
## bu bir fonksiyon çağırısıdır.

## Eşlemeden veri çekiyoruz
{:boy => 1.74}.fetch :boy
#=> 1.74

## Eşlemelere veri ekliyoruz
{:boy => 1.74}.merge!(kilo: 74)
#=> {:boy=>1.74, :kilo=>74}

## Anahtarlarımıza bakalım
{:boy=>1.74, :kilo=>74}.keys
#=> [:boy, :kilo]

## Değerlerimize bakalım
{:boy=>1.74, :kilo=>74}.values
#=> [1.74, 74]

## Dizi olarak almak istersek
{:boy=>1.74, :kilo=>74}.to_a
#=> [[:boy, 1.74], [:kilo, 74]]
## Endişelenmeyin, dönüşümler için koca bir bölüm
## ayırdım.


## --
## Diziler
## --

## Örnek bir dizi ile başlayalım.
["Mustafa", "Kemal", "ATATÜRK"]
#=> ["Mustafa", "Kemal", "ATATÜRK"]

## İlk değeri alıyoruz
["Mustafa", "Kemal", "ATATÜRK"].first
#=> "Mustafa"

## Son Değeri,
["Mustafa", "Kemal", "ATATÜRK"].last
#=> "ATATÜRK"

## Indis araması için `fetch` metodu.
["Mustafa", "Kemal", "ATATÜRK"].fetch 1
#=> "Kemal"

## Var olamyan bir indis ararsak hata alıyoruz.

## Fakat seçimli ikinci argüman bize indisin
## bulunamaması halinde döndürülecek değeri
## belirleme imkanı veriyor.
["Mustafa", "Kemal", "ATATÜRK"].fetch 20101927, "Nutuk"
#=> "Nutuk"

## Birden fazla değer almak için, slice metodunu
## kullanabiliriz
["Fatih", "Sultan", "Mehmet"].slice 1..2
#=> ["Sultan", "Mehmet"]

## Ya da, indis notasyonu da kullanılabilir.
["Fatih", "Sultan", "Mehmet"][1..2]
#=> ["Sultan", "Mehmet"]

## Baştan n tane eleman almak için take metodunu kullanıyoruz
["Fatih", "Sultan", "Mehmet"].take 2
#=> ["Fatih", "Sultan"]

## Rastgele bir dizi elemanı elde etmek için sample metodunu
## kullanıyoruz
["Fatih", "Sultan", "Mehmet"].sample
#=> "Fatih"

## `sample` metodu seçimli bir argüman kabul eder.
## bu argüman rastgele istenen eleman sayısını temsil eder
["Fatih", "Sultan", "Mehmet"].sample 2
#=> ["Fatih", "Sultan"]

## Aradığınız eleman, dizide var mı kontrolü için
## include? metodu kullanılıyor
["Fatih", "Sultan", "Mehmet"].include? "Fatih"
#=> true

## Dizinizdeki elemanları koşul dahilinde seçimlemek için
## select metodunu kullanıyoruz
["Fatih", "Sultan", "Mehmet"].select {|s| s.include? ?a}
#=> ["Fatih", "Sultan"]
## Süzme işleminin koşulu, a karakteri içeren nesneler için olumlu.
## Not: filter metodu, select için bir takma addır.

## Ters bir yöntem, süzgeçleme için ise;
["Fatih", "Sultan", "Mehmet"].reject {|s| s.include? ?a}
#=> ["Mehmet"]
## koşulumuz aynıydı, seçimleme metodumuzu değiştirdik.

### Yapısal düzenlemeler için:
## Dizileri ters çevirmek,
["Fatih", "Sultan", "Mehmet"].reverse
#=> ["Mehmet", "Sultan", "Fatih"]

## Sıralamak için sort metodu,
["İş", "Aşk", "Para"].sort
#=> ["Aşk", "Para", "İş"]

## Ön koşulla sıralamak için,
["İş", "Aşk", "Para"].sort {|a,b| b <=> a }
#=> ["İş", "Para", "Aşk"]
## Koşulumuz basitçe tersine sıralamak için bir tanımdır.
## ileride karşılaştırma operatörlerini göreceğiz.

## Tekrarlı elemanların temizlenmesi için
[1,2,3,4,5,6,7,1,2,4,1,5,6,1,2,5].uniq
#=> [1, 2, 3, 4, 5, 6, 7]

## Dizilerin birleştirilmesi için
[1,2] + [3,4]
#=> [1, 2, 3, 4]
## infix notasyon sizi yanıltmasın,
## tasarımı gereği, her şey sınıflar ve metotlarına çağırım
## olarak yürüyor.
## Kanıtlayalım;
[1,2].+([3,4])
#=> [1, 2, 3, 4]

## Peki ya aynı elemanları içerebilecek dizileri birleştirmek
## istersek ?
[1,2] | [2,3,4]
#=> [1, 2, 3, 4]
## | operatörü bizi, nihai sonuçtaki tekrarlı veriden koruyor.

## Peki ya bir diziyi, eleman bazında diğeriyle 
## süzmek istersek ?
[1,2] - [2,3,4]
#=> [1]
## Notasyon sizi yanıltmasın, Küme gibi davranan bir dizi işlevi.

### Veri Dönüşümleri için:
## Dizideki her elemana uygulamak istediğiniz bir
## dönüşümü, map metodu ile uygulayabilirsiniz,
["Kontak İsmi",
 "Kontak Telefon Numarası"].map {|element| "<label>#{element}</label>"}
#=> ["<label>Kontak İsmi</label>", "<label>Kontak Telefon Numarası</label>"]
## HTML konusu için yine LearnXinYminutes'e danışabilirsiniz.

## Son elde ettiğimiz veriyi birleştirmek için,
["<label>Kontak İsmi</label>",
 "<label>Kontak Telefon Numarası</label>"].join ""
#=> "<label>Kontak İsmi</label><label>Kontak Telefon Numarası</label>"

## Veriyi indirgemek için ise reduce metodu kullanırız,
["<label>Kontak İsmi</label>",
 "<label>Kontak Telefon Numarası</label>"]
 .reduce("") {|akümülatör, veri| akümülatör + veri}
#=> "<label>Kontak İsmi</label><label>Kontak Telefon Numarası</label>"
## Akümülatör, her operasyondan dönen ara-geçici değer.
## Bu değeri, parantez içinde ilkledik,
## eğer vermeseydik, dizinin ilk elemanı olacaktı.

## Tabi, daha kolay bir yolu var;
["<label>Kontak İsmi</label>", 
 "<label>Kontak Telefon Numarası</label>"].reduce(:+)
#=> "<label>Kontak İsmi</label><label>Kontak Telefon Numarası</label>"
## reduce metodu, ikili bir operasyonu (akümülatör için metot!)
## sembol olarak vermenize izin verir ve bu, reduce için
## indirgeme fonksiyonu olarak kullanılır.

## Nüansları olsa da, son üç Ruby çağırımı aynı sonucu vermektedir.


## --
## Semboller
## --

## Ruby sembolleri, çalışma zamanında değiştirilemezler.
## Ama metinsel değerlerden semboller elde etmek mümkündür.
## Bunu dönüşümler kısmında işlemek daha doğru olacak diye düşündüm.

# --------------------------------
# Dönüşümler
# --------------------------------

## --
# Rakamlar 
## --

## Sayısal değerlerin diğer tiplere dönüşümü;

## Sayısal -> Metinsel
1923.to_s #=> "1923"

## Sayısal -> Mantıksal
!1923 #=> false
## Farkedebileceğiniz gibi,
## sayısal değerler, mantıksal doğru'ya değerlendiriliyor.

## Sayısal -> Sembol
## Maalesef, Ruby bile Sayısal değerden Sembol değerlerine
## doğrudan dönüşüm için metot barındırmıyor.
## Bunu yine de başarmak istersek, değeri önce
## Metinsel'e dönüştürerek Sembol dönüşümü için hazırlarız.
1923.to_s.to_sym
#=> :"1923"

## Sayısal -> Dizi | bölümlenerek
## Yine doğrudan bir dönüşüm yoktur.
## Böyle bir doğrudan dönüşüm teşvik de edilmez.
## Ama ihtiyaç olabilecek bir dönüşüm.
1923.to_s.split('')
#=> ["1", "9", "2", "3"]
## Öncelikle Metinsel dönüşüm yapılır, sonra
## her bir karakter için ayrılır.
## Yine her bir rakamı sayısal bir şekilde elde etmek için
## her birini Metinsel'den Sayısal değere dönüştürecek
## ifade aşağıdaki gibidir.
1923.to_s.split('').map { |i| i.to_i }
#=> [1, 9, 2, 3]


## --
# Mantıksal 
## --

## Mantıksal -> Metinsel
true.to_s #=> "true"
false.to_s #=> "false"

## Mantıksal değeler için gerçeklenmiş başka bir dönüşüm
## metodu olmadığı için, bu kısmı dilde ufak bir nüansa ayırmak
## istedim.
## Kaynak için ileri okumaya başvurunuz.

## Hangi programlama dilinden gelirseniz gelin,
## dilde doğruluk değerleri diye bir küme vardır.
## Hangi değerlerin mantıksal doğru değerine dönüşeceği,
## bu değer yerine geçebileceği
## fikri üzerine kurulu bir küme.
## Ve Ruby'de nil değeri, false dışında, mantıksal yanlış değerine çözümlenen tek şey.
## Bu ön bilgi ile doyduysak, başlayalım.

!!nil   #=> false
!!false #=> false
!!0     #=> true
!!""    #=> true
## Şimdi ne oldu burada ?
## `!!` ifadesi; değilinin değili, yani kendisi. Tek bir farkla.
## Verinin türü değiştiriliyor. Mantıksal olarak yorumlanıyor.
## Yukarıda, nil ve false ifadeleri mantıksal olarak yanlış  olarak yorumlanıyor.
## Diğerleri ise mantıksal doğru.


## --
# Metinsel 
## --

## Metinsel -> Sayısal
## Öncelikle dilde ufak bir tuzağa dikkat çekmek isterim.
"".to_i #=> 0
"asd".to_i #=> 0
## Sayısal yorumlaması geçersiz ve boş her metinsel değer,
## 0 rakamına değerlendirilir.

## Başarılı bir dönüşüm,
"1234".to_i #=> 1234

## Sayı sistemini belirleyebileceğiniz seçimli bir argüman
## kabul ediyor, to_i metodu.
"1234".to_i 5 #=> 194
## 1234 sayısının, beşlik tabandaki karşılığı.

## Tam sayı dışında doğrudan dönüşümler
## dil olanağı olarak sunulmuş durumda;

## Kompleks sayı olarak,
"1234".to_c #=> (1234+0i)

## Ondalık (Kayan-noktalı) sayı olarak,
"1234".to_f #=> 1234.0

## Rasyonel sayı olarak,
"1234".to_r #=> (1234/1)

## Metinsel -> Mantıksal

## Mantıksal değerin kendisi için tersinin, tersini alırız
!!"seçkin" #=> true

## Mantıksal tersi için ise tersini,
!"seçkin" #=> false

## Metinsel -> Sembol
"cengiz".to_sym #=> :cengiz

## Metinsel -> Dizi
"Cengiz Han".split #=> ["Cengiz", "Han"]

## split metodu, seçimli argümanının varsayılan değeri
## boşluk karakteridir.

## Metinsel -> Dizi | bölümlenerek
"Cengiz Han".split ""
#=> ["C", "e", "n", "g", "i", "z", " ", "H", "a", "n"]


## --
# Sembol 
## --

## Sembol -> Metinsel
:metin.to_s #=> "metin"
## Başka bir dönüşüm için dilin bir teşviki yoktur.

## --
# Diziler 
## --

## Dizi -> Metinsel
[1,2,3,4,5].to_s #=> "[1, 2, 3, 4, 5]"


## --
# Eşlemeler 
## --

## Eşleme -> Dizi
{a: 1, b: 2, c: 3}.to_a
#=> [[:a, 1], [:b, 2], [:c, 3]]
## Her bir anahtar-değer çifti bir dizi olarak
## değerlendirilir ve elemanları sırasıyla
## anahtar ve değerdir.

## Eşleme -> Metinsel
{a: 1, b: 2, c: 3}.to_s
#=> "{:a=>1, :b=>2, :c=>3}"
## inspect metodu için bir takma addır.


# --------------------------------
# Tanımlamalar, ifadeler, yorumlama.
# --------------------------------

## --
## Yorumlama
## --

## Dökümanın başından beri gördüğümüz bu tek satır yorumlama operatörü
## (#)
## kendinden sonra gelen her şeyin, satır boyunca yorumlama olarak
## değerlendirilmesi gerektiğini Ruby yorumlayıcısına söyler.

## Ruby, farklı yorumlama imkanları da sağlamaktadır.
## Örneğin;
=begin
  Başlangıç ifadesi (=begin), sonlandırma ifadesi (=end)
  ile arasında kalan her şeyi yorum satırı olarak ele alır.
=end

## --
## Global değişkenler.
## --

## Ruby evrensel değişkenleri, kapsamı en geniş değişken türüdür.
## Her yerden erişilebilir...

## Basitçe dolar sembolü ( $ ) ile başlarlar.
$evrensel_bir_değişken = 42 #=> 42

## Bir çok metadoloji ve yöntem ve teknoloji, size
## evrensel değişkenler kullanmanın projenizi karmaşıklaştıracağı
## ve bakımını zorlaştıracağını söyler; Haklıdırlar...

## --
## Varlık değişkenleri.
## --

## At ( @ ) sembölü ile başlarlar ve isimlerinin de ifade ettiği
## gibi, kendileri bir Sınıf'ın değeridir.

class Varlık
    def initialize()
        @varlık_değişkeni = 101
    end
    
    def göster()
        puts "Varlık değişkeni: #@varlık_değişkeni"
    end
end

varlık1 = Varlık.new()

varlık1.göster()
#=> Varlık değişkeni: 101

## Sınıf tanımı şimdilik kafanızı karıştırmasın.
## İlgilenmemiz gereken kısım;
## @varlık_değişkeni = 101
## ifadesidir. Ve nesne özelinde tanımlama yapar.
## Kapsamı sadece o nesnedir.

## ! NOT: ilklenmemiş varlık değişkenleri nil ön değeri ile
## yaşam döngüsüne başlar.

## --
## Sınıf değişkenleri.
## --

## Sınıf değişkenleri iki at ( @@ ) sembölü ile başlarlar.
## Tanımlar, herhangi bir metot içinde
## kullanılmadan önce ilklenmelidirler.
## İlklenmemiş bir Sınıf Değişkenine referansta bulunmak,
## bir hata oluşturur.

class Sınıf
    @@sınıf_nesne_sayısı = 0

    def initialize()
        @@sınıf_nesne_sayısı += 1
    end
    
    def göster()
        puts "Sınıf sayısı: #@@sınıf_nesne_sayısı"
    end
end

sınıf_varlığı1 = Sınıf.new()
sınıf_varlığı2 = Sınıf.new()
sınıf_varlığı3 = Sınıf.new()

sınıf_varlığı1.göster()
#=> Sınıf sayısı: 3


## --
## Yerel değişkenler.
## --

## Yerel değişkenlerin isimlendirmesi küçük harf
## ya da alt çizgi ( _ ) ile başlar ve kapsamı tanımın
## yapıldığı sınıf, modül, metot yada blok içinde kalır.

## --
## Sabitler.
## --

## Ruby sabitleri, büyük harf ile tanımlanırlar ve
## kapsamları için iki senaryo mevcuttur;

## - Sınıf ya da Modül içinde tanımlanırlarsa
## Tanımın yapıldığı blok içinden erişilebilir.

## - Sınıf ya da Modül dışında yapılan tanımlar ise
## Evrensel bir kapsama sahiptir ve her yerden
## erişilebilirler.

## Örneğin:

class Sabit
   SABİT = 299_792_458

   def göster
      puts "Sabit değer: #{SABİT}"
   end
end

# Create Objects
sabit = Sabit.new()

sabit.göster()
#=> Sabit değer: 299792458

## İfadenin tanımındaki alt çizgiler sizi yanıltmasın
## binlik ayracı olarak kullandım ve Ruby yorumlayıcısı
## tamamen görmezden geliyor.
## Bknz: Veriler ve Temsiller: Sayılar.

## --
## Sözde Değişkenler.
## --

## Ruby özel bir dil.
## Öyle ki, Bazı sözde-değişkenler ile
## size, ihtiyacınız olabileceği erişimi sağlar.
## Ama onlara atama yapamazsınız.

## Sözde değişkenler ve kullanımları
## ile ilgili İleri okumaya başvurunuz.


# --------------------------------
# Konvansiyonlar ve teşvikler.
# --------------------------------

## Konvansiyonlar:

## --
## İsimlendirme Konvansiyonları:
## Döküman boyunca yaptığım gibi,
## tanımlayıcılar ve erişilebilir tanımlanmış ifadeler
## için lütfen önerildiği gibi İngilizce kullanın.
## İsimlendirme, Bilgisayar bilimlerinde yeterince
## ağır bir zemin ve dilin teşvik ettiği rehber
## ve önerdiği konvansiyonları takip ederek
## bakımı, okuması ve geliştirmesi kolay projeler
## gerçeklemek mümkündür.

## --
## Semboller, Metotlar ve Değişkenler için
##-Snake Case ( snake_case ) kullanılması önerilir.

## --
## Sınıflar için Camel Case (CamelCase):
## Sınıf isimlendirmeleri için önerildiği gibi,
## Camel Case isimlendirme notasyonuna sadık kalın.

## --
## Dosyalar ve Klasörler için Snake Case (snake_case):
## Dosya ve klasörleri isimlendirmek için lütfen
## Snake Case isimlendirme konvansiyonuna sadık kalın.

## --
## Dosya Başına Bir Sınıf:
## Her dosyada bir sınıf barındırmaya özen gösterin.

## ---
## Bu kısımdaki teşvik içerikleri
## rubocop-hq/ruby-style-guide'dan gelmektedir.

## ! Rehbere göre bu dökümanı düzenle!

## --
## Girintileme:
## Girintileme için TAB yerine, iki boşluk kullanın.

def bir_metot
    birşeyler_yap
end
## Yerine;
def bir_metot
  birşeyler_yap
end

## --
## Satır Başına Karakter:
## Satır başına maksimum 80 karakter olacak şekilde
## dökümanı yapılandırın.

## --
## Satır Sonları:
## Unix-Stili satır sonlarını kulanın.
## Eğer Git kullanıyorsanız;
## $ git config --global core.autocrlf true
## ifadesi sizi bu zahmetten kurtaracaktır.

## --
## Satır Başına Bir İfade:
## Satır başına bir ifade kullanın.

puts 'test!'; puts 'test!'
## Yerine;
puts 'test!'
puts 'test!'

## --
## Boşluklar ve Operatörler:
## Operatörler, virgüller, ifade ayraçları
## aralarında boşluk bırakın.

toplam=1+2
x,z=1,2
class FooError<StandardError;end
## Yerine;
toplam = 1 + 2
x , z = 1 , 2
class FooError < StandardError; end
## Bir kaç istisna hariç
## - Üs operatörü
## - Rasyonel sayı gösteriminde kullanılan eğik çizgi.
## - Güvenli gösterim operatörü.


### Daha fazlası için ileri okumadaki
### bu rehbere danışabilirsiniz...

# --------------------------------
# Bloklar, Kontrol blokları ve örnekler.
# --------------------------------

## --
## Ruby Blokları:
## Süslü parantezler ya da `do`, `end` ile çevrelenen,
## değişkenleri ortama bağlı işlevlerdir.
## Diğer dillerde !{Closure} ( closure ) karşılığı
## bulur.
## Ruby'de bloklar, ifadeleri gruplamanın bir şeklidir.
## Bloklar tanımlandıklarında çalıştırılmazlar,
## Ruby, bu yönetimi akıllıca yapar.

## Örneğin;

## Tanımlamamız
def selamla_sonra_çağır
  puts 'Selamlar!'
  yield ## tüm sihir burada!
end

## Şimdi tanımı çağıralım
selamla_sonra_çağır {puts 'Çağrı, gerçekleşti!'}
#= Selamlar!
#= Çağrı, gerçekleşti! 
#=> nil
## Çağırım, kendini çağıran kaynağa nil döndürmekte.
## Değerlendirmenin sonucunda, Ruby yorumlayıcısı,
## ifadenin değerini nil olarak çıktılar.
## Fakat, puts çağrıları, girdi/çıktı işlevimizi
## yerine getirir ve metinleri ekrana basar.

## Blokların argüman almaları da mümkündür:
def selamla_sonra_değer_ile_çağır
  puts 'Selamlar!'
  yield('Hain Kostok') ## tüm sihir burada!
end

selamla_sonra_değer_ile_çağır {|isim| puts "Sana da selam, #{isim}!"}
#= Selamlar!
#= Sana da selam, Hain Kostok!
#=> nil

## Detaylı bilgi için, ileri okumaya başvurunuz.

## --
## Eğer ( if ) kontrol ifadesi:
## Algoritmanızda dallanma imkanı sağlar.
## Şablonu:
## if koşul_ifadesi [then]
##   yürütülecek_kod
## [elsif bir_diğer_koşul [then]
##   yürütülecek_diğer_kod]
## [else
##   yürütülecek_bir_diğer_kod]
## end

## Bu kalıba sadık kalarak, dallanmalarımızı kodlarız.
## Köşeli parantezler, sezgisel olarak anlaşılacağı üzere
## seçimli ifadelerdir.

## Örnek:
if true
  puts 'Koşul ifadesi, buradan devam edecek!'
else
  puts 'Buradan değil.'
end
#= Koşul ifadesi, buradan devam edecek!
#=> nil

## --
## Eğer ( if ) düzenleyicisi:
## Kompak bir dil olanağıdır. Aynı şekilde, çalıştırılacak kod
## ve bir koşul ifadesi alır. Ve koşul ifadesine bakarak
## ifadenin yürütüleceğine karar verir.
## Şablonu:
## çalıştırılacak_kod if koşul_ifadesi

## Örnek:

puts 'Bu ifade yürütülecek!' if true
#= Bu ifade yürütülecek!
#=> nil


## --
## Durum ( case ) kontrol ifadesi:
## Bir koşul ifadesi ve bir ya da daha fazla karşılaştırma ifadesi
## alarak, eşleşen bloğu yürütür.
## Şablonu:
## case koşullanacak_ifade
## [when karşılaştırma_ifadesi [, karşılaştırma_ifadeleri ...] [then]
##   yürütülecek_kod ]...
## [else
##   eşleşme_olmazsa_yürütülecek_kod ]
## end

yaş = 27
case yaş
when 0 .. 2
   puts "bebek"
when 3 .. 6
   puts "küçük çocuk"
when 7 .. 12
   puts "çocuk"
when 13 .. 18
   puts "genç"
else
   puts "yetişkin"
end
#= yetişkin
#=> nil

## --
## .. Sürece ( while ) kontrol ifadesi:
## Aldığı koşul ifadesini kontrol eder,
## kontrol bloğunu çağırır ve tekrar kontrol eder.
## Koşul ifadesi doğru olduğu sürece, kontrol bloğu
## çağırılmaya devam eder.
## Şablonu:
## while koşul_ifadesi [do]
##   yürütülecek_kod
## end

## Örnek:

$n = 0
$sayı = 5

while $n < $sayı  do
   puts("Döngü içinde n = #$n" )
   $n +=1
end
#= Döngü içinde n = 0
#= Döngü içinde n = 1
#= Döngü içinde n = 2
#= Döngü içinde n = 3
#= Döngü içinde n = 4
#=> nil

## --
## .. Sürece ( while ) düzenleyicisi:
## Eğer düzenleyecisi gibi, kompak bir dil olanağıdur.
## Kontrol ifadesinin işlevini yerine getirir,
## ama satır içi kullanıma müsade ederek.
## Şablonu:
## çalıştırılacak_kod while koşul_ifadesi
## Yada:
## begin
##   çalıştırılacak_kod
## end while koşul_ifadesi

## --
## İçin ( for ) kontrol ifadesi:
## N kere, I kere, X kere gibi ifadelerin dildeki kontrol
## karşılığıdır. Çoklu veri üzerinde iterasyonlar yapmanızı
## veri üzerinde operasyonlar yürütmenizi sağlar.
## Şablonu:
## for değişken [, başka_değişken ...] in ifade [do]
##   yürütülecek_kod
## end

## Örnek:
for i in 1..5
  puts i
end
#= 0
#= 1
#= 2
#= 3
#= 4
#= 5
#=> 0..5

## Ardışıkları itere etmek için tek yol bu değil tabii.
## İlerleyen kısımda buna yer verilecektir.

## --
## Sonlandırıcı ( break ) kontrol ifadesi:
## Bu kontrol ifadesi yürütüldüğünde, çalışma zamanını 
## en iç tekrarlı bloktan çıkarır.

## Örnek:
for i in 1..5
  break if i > 2
  puts i
end
#= 0
#= 1
#= 2
#=> nil
## break kontrol ifadesi, if düzenleyecisi ile çevrelenmiştir.
## if i > 2
##   break
## end
## ifadesi ile eşdeğerdir.
## ifade yürütüldüğü anda, en yakın tekrarlı blok terkedilir.
## Yorumlayıcı, sonraki ifadeden yürütmeye devam eder.

## Diğer kontrol ifadeleri ve kullanımları için ileri okumaya başvurunuz...


# --------------------------------
# Özel anahtar kelimeler; kullanımları ve örnekleri.
# --------------------------------

## --
## __ENCODING__: 
## Bu anahtar kelime size yorumlayıcı kodlama türünü verecektir.

__ENCODING__
#=> "#<Encoding:UTF-8>"

## Platform, araç ve çalışma zamanı yürütme
## yönteminize bağlı olarak alacağınız çıktı
## değişiklik gösterebilir.

## --
## __LINE__:
## Geçerli dosyada, yürütme satır numarasını verir.

__LINE__
#=> 67

## Platform, araç ve çalışma zamanı yürütme
## yönteminize bağlı olarak alacağınız çıktı
## değişiklik gösterebilir.

## --
## BEGIN ve END:
## BEGIN:
## Dosyadaki tüm içerikten önce yürütülür.
## END:
## Dosyadaki tüm içeriklerden sonra yürütülür.

## --
## alias:
## Herhangi bir tanımlayıcı için takma ad tanımlamanıza
## olanak sağlar.

$eski = 0
alias $yeni $eski

$yeni
#=> 0

## --
## and:
## Düşük öncelikli bir Mantıksal VE operatörü.

## --
## begin / end ve rescue:
## İstisnalar begin / end blokları
## arasında ele alınır ve `rescue` anahtar kelimesi ile
## işlenirler.
## İstisnalar ve mantalitesine dair ön girişi
## Teşvik edilen paradigma ve anlatımı kısmında bulabilirsiniz.

## Hata yönetimi, Ruby'de de özenle işlenmiş bir konudur.

## Örnek:

begin
  yanlış_bir_hesaplama = 2/0
  puts "Hesaplama sonucu: #{yanlış_bir_hesaplama}"
  rescue ZeroDivisionError => hata_nesnesi
    puts "Sıfıra bölümle ilgili bir hata yakalandı: #{hata_nesnesi.message}"
end
#= Sıfıra bölümle ilgili bir hata yakalandı: divided by 0
#=> nil

## Örneğimizde matematiksel sistemimiz için hatalı bir
## işlem gerçekleştiriyoruz. Sonrasında hatayı ilgili
## hata durumu için belirlediğimi alanda yönetiyoruz.
## Örnekte hatayı çıktılayarak yönettik, gerçek dünyada
## biraz daha kompleks gerçekleşebilir.
## Gerçek dünya örnekleri için ileri okumaya başvurabilirsiniz.


## --
## defined?:
## defined?, argümanını metinsel olarak açıklayan bir dil olanağıdır.

## Örnek:
RUBY_VERSION
#=> "2.4.0"

defined? RUBY_VERSION
#=> "constant"

defined? nil
#=> "nil"

defined? puts
#=> "method"


## --
## ensure:
## Hata yönetiminin bir parçası olarak dilde görev atfedilen ensure,
## blok içinde, hata olsun ya da olmasın yürütüleceği garanti edilen
## dil ifadeleri için bir imkandır.

## Örnek:

begin
  yanlış_bir_hesaplama = 2/0
  puts "Hesaplama sonucu: #{yanlış_bir_hesaplama}"
  rescue ZeroDivisionError => hata_nesnesi
    puts "Sıfıra bölümle ilgili bir hata yakalandı: #{hata_nesnesi.message}"
  ensure
    puts "Hesaplama bloğu sonlandı!"
end
#= Sıfıra bölümle ilgili bir hata yakalandı: divided by 0
#= Hesaplama bloğu sonlandı!
#=> nil


## --
## self:
## Nesnenin kendisine erişim sağlayan bir dil olanağı.

## Örnek:

dünya = "Dünya!"
#=> "Dünya!"

dünya
#=> "Dünya!"

dünya.class
#=> String

def dünya.selamla
  "Merhaba, " + self
end
#=> :selamla

dünya.selamla
#=> "Merhaba, Dünya!"

## Nesnenin kendisine bir metot tanımladık,
## bunu yaparken de değerine erişim sağladık.

## --
## super:
## Nesne yönelimli programlama (spesifik olarak, obje tabanlı)
## paradigmasına göre, kalıtım konseptinde, türeyen sınıfın
## türetildiği sınıfa erişimi (üst sınıfı, atası, hiyerarşik üstü)
## bu anahtar kelime ile gerçekleşir.

class A
  def initialize(a)
    @a = a
  end
end

class B < A
  def initialize(a, b)
    @b = b
    super a
  end
end

b = B.new 1, 2
#=> #<B:0x00007f852d04c7e8 @b=2, @a=1>
## super ile üst sınıfın ilklenmesi gerçekleştirildi,
## aldığımız çıktıda da @a=1 çıktısıyla gözlemlenebilir.

## Bu konunun, dilin paradigma teşviği ile ilgili
## olduğunu ve anlamazsanız, Paradigma başlığını bitirdikten
## sonra tekrar bu örneği değerlendirmeniz gerektiğini hatırlatırım.

## --
## yield:
## Ruby blokları kısmında anlattık, ama, burada da bir nüanstan
## bahsetmeden geçemeyeceğim.
## Çalıştırılabilir ifadeleri çalıştırmanın birden fazla yolu vardır.
## Fakat yield, en performanslı dil olanağı olarak dökümanda işlenmiş.
## Kaynak için ileri okumaya danışın.



# --------------------------------
# G/Ç ( I/O )
# --------------------------------

=begin
  G/Ç, Girdi/Çıktı ( Input/Output ) kısaltmasıdır.
  Temelde, sistemden girdi almak ve çıktı yaratmak amacıyla vardır.
  Girdi örnekleri:
    - Klavyeden bastığınız herhangi bir tuş.
    - Fare hareketleriniz ya da tıklamalarınız.
    - Mikrofonunuzun aldığı sesler.
  
  Çıktı örnekleri:
    - Herhangi bir dil ifadesinin sonucu.
    - Dijital bir ses dosyasının sese dönüşmesi.
    - Ekranda gördükleriniz.
  
  Fakat endişelenmeyin, G/Ç derken, şu anda 
  biz sadece Ruby'de,
    - Dosya okuma/yazma.
    - Ekrana metin yazdırma / Bilgi okuma.
    - Ağ soketleri. ( biraz da olsa )
  işlerinden bahsediyor olacağız.
=end

defined? IO
#=> "constant"

IO.class
#=> Class

## IO sınıfı, File ve Socket gibi pratik kullanımı olan sınıfların atasıdır.
## Septikler için;

File.superclass
#=> IO
## Gözlemlediğiniz üzere, superclass metodu, üst sınıfı veriyor.

## --
## Dosya Okuma ve Yazma:
## Ruby'de dosya okuma ve yazma işlemleri için, File
## sınıfını kullanacağız.

## Dosyaya yazmak için;
File.write 'test.txt', "a,b,c"
#=> 5
## 5, ifadenin ürettiği dönüş değeridir.
## ve, çıktılanan karakter sayısını verir.

## Dosyadan okuma için;
## Bu kısım, açıklayıcı olması açısından
## ifadeleri teker teker işleyeceğiz.

File
#=> File
## Sınıfımız.

File.readlines 'test.txt'
#=> ["a,b,c"]
## readlines File sınıfının bir metodu ve aldığı argüman dosya yoludur.

File.readlines('test.txt').first
#=> "a,b,c"
## Dönüş değeri bir diziydi, her bir satır bir eleman olacak şekilde.
## Biz, kendi verilerimizi, kendi ayıracımızla kaydetmeyi seçtik.
## Eğer, `\n` satır ifadesi ile ayırmayı seçseydik, readlines
## metodu zaten işlevi gereği, bize değerleri ayrı ayrı verecekti.

File.readlines('test.txt').first.split ','
#=> ["a", "b", "c"]
## verilerimizi aldık.

## Eğer yeni satır karakterini ayıraç olarak kullansaydık;
File.write 'ntest.txt', ['a', 'b', 'c'].join("\n")
#=> 5

File.readlines('ntest.txt').map(&:chomp)
#=> ["a", "b", "c"]
## Bu da genel kullanımlı bir yaklaşımdır.

## --
## Ekrana bilgi yazdırma ve Okuma:
## Konsol'a bilgi çıktılamak için,
## önceden tanımlanmış $stdout global nesnesini kullanacağız.
## Pratik kullanımda, prints işlevinden bir farkı yoktur.
## Aynı sonuca ikisi ile de ulaşabilirsiniz.

$stdout.print "Bu bir çıktı.\n"
#= Bu bir çıktı.
#=> nil

## Şimdi kullanıcıdan bilgi okuyalım:
$stdin.gets
#! Bu kısımda hiç bir çıktı verilmez ve aksine
#! sizden girdi beklenir. Bir metin yazın ve onaylamak için
#! enter tuşunu kullanın.
#- Bu bir girdi metni!
#=> "Bu bir girdi metni!\n"

## Aldığımız veriyi temizlenin yolunu biliyoruz.
## Dönüş değerine chomp metodunu uygulamak.
$stdin.gets.chomp
#- Bu bir girdi metni!
#=> "Bu bir girdi metni!"


## --
## Ağ girdi/çıktı yönetimi
## Ruby'de soketler (Socket)
## haricen çalışma zamanına dahil edilir.

require 'socket'
#=> true

soket = TCPSocket.new('google.com', 80)
#=> #<TCPSocket:fd 13, AF_INET, 192.168.0.11, 63989>
## Alacağınız çıktı değişiklik gösterebilir.
## Soketi oluşturduk ve bir değişkene atadık.
## Şimdi bunun üzerinden okuma ve yazma işlemlerini
## gerçekleştireceğiz.

soket.write "GET / HTTP/1.1"
#=> 14

soket.write "\r\n\r\n"
#=> 4
## İki write metodunun sonucu da, sokete yazılan verinin
## uzunluğudur.

## Şimdi okuma zamanı, soketi açtık, isteğimizi bildirdik.
## Şimdi soket üzerinden aldığımız cevabı ekrana yazdıralım.

soket.recv 80
#=> "HTTP/1.1 200 OK\r\nDate: Thu, 03 Sep 2020 10:48:21 GMT\r\nExpires: -1\r\nCache-Control"
## Alacağınız çıktı değişiklik gösterebilir.
## Ancak, başarılı şekilde okuma yaptık.



# --------------------------------
# Teşviğinde bulunduğu paradigma ve derinlemesine anlatımı.
# --------------------------------

## --
## Nesne Yönelimli Programlama Nedir?
## Kısaca NYP, en basit anlatımıyla;
## nesnelerle programlamadır.
## Nesne paradigması, her programcıya doğal ve sezgisel gelir.
## Bunun sebebi, zaten gerçekliği algılama şeklimize uygun olmasıdır.
## Araba, onu bir araya getiren nesnelerden oluşur,
## tekerlekleri, direksiyonu, kasası, ve diğer parçalarıyla.
## Ama bu, tam tanım değildir. NYP'de, Nesneler,
## Bilgilere ( evet, varlık olarak başka nesneler de sayılabilir )
## ve bu bilgileri yönetecek ( hesaplamalar gerçekleştirecek 
## ya da aksiyonlar alacak -- G/Ç -- gibi ) metotlara sahiptir.

## Bir nesnenin en net tanımı böyle yapılabilirken,
## NYP, bir gerçek dünya problemi için bir araya getirilmiş
## -- çoğunlukla birden fazla -- sınıfların yapıları,
## ilişkileri ve işlevlerini ele alır.

## Bir paradigma olarak NYP, bizlere her varlığı nesne olarak
## modellemeyi ve problem uzayımızdaki nesnelerle olan ilişkilerini
## Ruby'de NYP için sağlanan imkanlarla yönetmeyi öğütler.

## Sınıf içerisinde saklanan bilgiye öznitelik ya da özellik,
## işlevlere ise metot denilir.
## NYP jargonu için ileri okumaya başvurabilirsiniz.

## --
## Ruby'de NYP teşviki:

## Nesneler, Sınıfların gerçeklenmiş halleridir.
## Tam tersi ile, Sınıflar ise, nesnelerin soyut kalıplarıdır.

## Bir sınıf tanımı yapalım ve gerçekleyelim:

class Araba
end
#=> nil
## Evet, evet. Tanımımız hiç bir öznitelik ( attributes ) ya da
## metot ( method ) içermiyor.

## Şimdi bir özellik ekleyelim
class Araba
  def initialize(hız)
    @hız = hız
  end
end

araba = Araba.new 100
#=> #<Araba:0x00007f7f300e59c8 @hız=100>

## En naif haliyle, hız bilgisi saklayan bir araba sınıfı gerçekledik.
## initialize metodu, Ruby imkanları ile, nesne yaşam döngünüzün ilk adımıdır.
## Bu döngüyü aşağıdaki gibi betimlemek mümkündür.
## İlkleme ( initialize ) -> [İşlevlerin çağırımı] -> Sonlandırma
## İlkleme, initialize metodu ile ele alınır, alınacak tüm argümanlar,
## sınıfın iş mantığı doğrultusuyla, bu ilk işlevde yönetilir ve nesne
## kullanıma hazır hale getirilir.

## Şimdi bir işlev ekleyelim.

class Araba
  def initialize(hız)
    @hız = hız
  end
  
  def git!
    puts 'Hınn, hınn!'
  end
end

araba = Araba.new 100
#=> #<Araba:0x00007f7f300e59c8 @hız=100>

## Şimdi metodu çağırıyoruz.
araba.git!
#= Hınn, hınn!
#=> nil

## Başlığın amacı sadece Ruby'nin NYP olanaklarını ve
## teşviğini işlemek değil. Paradigmaya bir giriş kazandırmak.
## Bundan dolayı, etkileşim içinde birden fazla sınıf görmeliyiz.

class Tekerlek
  YERLİ = 5
  İTHAL = 1

  def initialize (tür)
    @güç = tür
  end

  def döndür!
    @güç -= 1
  end
end

class Araba
  def initialize (hız)
    @hız = hız
    @tekerlekler = (1..4).map {|| Tekerlek.new(Tekerlek::YERLİ)}
  end

  def git!
    if @tekerlekler.map(&:döndür!).filter {|ömür| ömür < 0}.first then
      puts 'Paat!'
    else
      puts 'Hınnn, hınnn!'
    end
  end
end

## nesnemizi oluşturuyoruz
araba = Araba.new 100

## altı sefer, araba nesnesinin git! metodunu çağırıyoruz.
(0..6).map {|| araba.git! }
#= Hınnn, hınnn!
#= Hınnn, hınnn!
#= Hınnn, hınnn!
#= Hınnn, hınnn!
#= Hınnn, hınnn!
#= Paat!
#= Paat!

## İş mantığımıza göre, arabamızın dört tekeri ve ve Yerli olanlar
## 5 birim dayanıklılığa sahip. ;)
## Bu beş birim tükenince, araba gitmek yerine,
## patlak teker çıktısı alıyoruz.


## Şimdiye kadar gördüklerimizi bir analiz edelim;
## Araba, sınıfın ismi. Her sınıf, tanımlamasak da, temel bir
## kurucu metot içerecek şekilde dil işleyicisi tarafından
## ele alınıyor.
## Bizim bir tanımımız var ama.
## Hız bilgisi alıyoruz.
## bu bilgi, sınıf özniteliğidir. Sınıf, bu bilgiye kendi içinden erişebilir.
## Bir de, binek araçların dört tekerleği olduğu fikriyle,
## nesne içinde, kurucu metot içinde dört tane Tekerlek nesnesi gerçekliyor
## ve saklıyoruz.
## İş mantığımıza göre onlara erişmemiz gerekiyor.
## git! metodu içinde, erişiyor ve kullanıyoruz.
## metotların sonundaki ünlem işareti bir konvansiyondur,
## metotların saf olmayan çağırımlar gerçeklediği anlamına gelmektedir.
## Kendilerini ( ya da sahip olduğu diğer nesneleri ) değiştirdikleri,
## bir girdi/çıktı gerçekleştirdikleri yada buna benzer yan etki içeren
## bir ifade barındırdıkları anlamına gelir.

## Sizi temin ederim ki, NYP, bu dökümanı ( hali hazırda ~1560 satır )
## genel anlatımı için bile ikiye katlayabilir.
## Lütfen detaylı bilgi için ileri okumaya başvurunuz.
```

# İleri okumalar.

Tümüyle İngilizce olan bu ileri okumalara inat, bu detaylı özgün Türkçe içeriği üretmek istedim.
Dilerim, benden sonra katkıda bulunanlar olur.

- [Ruby Style Guide](https://rubystyle.guide), Ruby stil rehberi.
- [Ruby-Doc üzerinde Proc](https://ruby-doc.org/core-2.4.0/Proc.html), Ruby Blokları ve Proc kavramı için.
- [Ruby-Doc üzerinde String](https://ruby-doc.org/core-2.6/String.html) sınıfı, işlevleri, metotları.
- [Ruby-Doc üzerinde TrueClass](https://ruby-doc.org/core-2.5.1/TrueClass.html#method-i-to_s) Dildeki mantıksal ifadelerin gerçeklemesi olan TrueClass (ve FalseClass için de aynı bağlantı üzerinden içeriğe ulaşılabilir) dönüşüm içeriği kaynağı.
- [Ruby Gerçeklemeleri Listesi](https://github.com/codicoscepticos/ruby-implementations) Ruby'nin farklı platformlardaki gerçeklemeleri. Opal ve Topaz dikkat çekenleridir.
- [The Object-Oriented Thought Process](https://www.amazon.com/Object-Oriented-Thought-Process-Developers-Library/dp/0321861272) kitap, bir paradigma olarak NYP ve düşünce yapısından bahsediyor. Bir paradigma olarak, NYP, türetildiği temel paradigmadan ne almış, başka paradigmalara ne kadar imkan sağlıyor ve paralel paradigma uyumu konusunda tüm sorularınıza cevap bulabilirsiniz. Yazar, belli etmese de, pragmatik bir yaklaşımda.
- [Block Argument](https://docs.ruby-lang.org/en/2.4.0/syntax/methods_rdoc.html#label-Block+Argument) Ruby Blokları ve yield hakkındaki Ruby resmi döküman sayfası ve alt başlığı.
- [A Theory of Objects]() Class-Based Languages başlığında inceleniyorlar.
