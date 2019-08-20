---
language: edn
filename: learnedn-tr.edn
lang: tr-tr
contributors:
  - ["Seçkin KÜKRER", "https://github.com/LeaveNhA"]
---

# Y = 20 Dakika.

### Genişletilebilir Veri Notasyonu (EDN, Extensible Data Notation).

### Okunuşu: (Türkçe: ey-di-en), (English: eed-n)

### Kodlama Türü: UTF-8

EDN Clojure sözdiziminin bir alt kümesidir. Bu alt küme, amacı gereği kod barındırmaz. Ve kendisi bir tip sistemi değildir. Bir şeması da yoktur. En basit tabirle; Genişletilebilir Veri Notasyonu kabul edilebilir elemanların bir kümesidir.

EDN elementleri, akışları ve dosyaları UTF-8 kullanılarak kodlanmalıdır. Üstelik, dökümanı çevreleyen işaretçiler de olmadığı için akış, dağıtık programlama mesaj arayüzü ve diğer dinamik sistemler için idealdir.


```clojure
; Yorumlar, yorumlarımız, noktalı virgül ile başlıyor.
;; Genellikle ikili olarak kullanılıyorlar.

;; |--------------------------------|
;  |--------- Genel Yapısı ---------|
;; |--------------------------------|

;; Boşluklar --whitespaces--, elementler için en yaygın ayıraçtır.
"Mustafa" "Kemal" "ATATÜRK"
;; Fakat okunuşu arttırdığı gerekçesiyle "," (virgüller --commas--) EDN yorumlayıcısı tarafından görmezden gelinir ve boşluk olarak nitelendirilir.
"Mustafa","Kemal","PAŞA"
;; Üstelik bu yenilikçi sözdizimsel kurala rağmen, {}, [] () gibi koleksiyon karakterlerini ayırmak için boşluğa ya da boşluğa çözümlenen virgüle ihtiyacınız yoktur.
[["MUSTAFA"] ["KEMAL"] [[{"ATA" "ATATÜRK"}]]]
;; Üst düzey vektör elemanlarını birbirinden ayıran boşlukları da kaldırabilirsiniz.
;; Fakat bu size, okunması zor bir vektör dışında hiç bir şey vermeyecektir.

;; |--------------------------------|
;  |-------- Atomik Yapılar --------|
;; |--------------------------------|

; Mantıksal Değerler
;; Mantıksal Doğru, çoğu teknolojide aynı gösterimi var.
true
;; Mantıksal Yanlış.
false

; Karakter Katarları
;; Karakter katarları, --SADECE-- çift tırnak ile belirtilebilir.
"İzmirin dağlarında çiçekler açar!"
;; C, C++, Java v.b. gibi dillerin desteklediği kaçış sekanslarını da destekler.
"Altın güneş orda sırmalar saçar.\nBozulmuş düşmanlar yel gibi kaçar."
;; Kaçış sekansları için bknz: $!$

; Karakter Sabitleri
;; Karakter sabitleri önlerinde bir ters eğik çizgi ile temsil edilirler.
\T \Ü \R \K
;; Üstelik, belirli kaçıl sekanslarının da karşılığı Karakter Sabiti olarak var.
\newline \return

; Anahtar Kelimeler
;; Anahtar Kelimeler, önlerinde bir ":" iki nokta --colon--
:yımırta
:kaşar
:bıngıl

; Semboller
;; Semboller tanımlayıcıları temsil etmek için kullanılır.
;; "/" karakteri, Sembol Sabitlerinde isim-uzayı ayıracı olarak kullanılıyor.
izmir/kızları
;; "mutfak" isim uzayındaki "ekmek-bıçağı" isimli sembole çözümlenir.

banyo/fayans
parke
laminat

; Sayısal Değerler
;; Tam Sayı sabiti.
1991
;; Kayan Noktalı Sabiti.
19.67

; Listeler
;; Listeler, yukarıdaki temel tiplerin ardışıklanmasıdır.
(bomba :bomba nokta \c \o \m)

; Vektörler
;; Vektörler bir bakıma Listelere benzeseler de, bir çok açıdan farklıdırlar.
;; Mesela Listenin aksine Vektörler, Rastgele Erişime imkan verir.
[[] "şimdi" "asker"]

; Eşlemeler
;; Sıfır veya daha fazla Anahtar-Değer çifti kabul eder.
;; Not: Clojure Veri Yapıları Soyutlaması ile Eşlemeler de, teknik olarak ardışık olarak işlenebilir.
{:canı :neler-ister?
 :uykuda "mevlam"}
;; Bu ve diğer tüm Veri Yapıları Homojendir, birbirilerini barındırabilir, kapsayabilir, içerebilirler.
;; Ayrıca okunurluk gibi farklı sebeplerle virgül kullanımında özgürsünüz.
{{:id_ "129u391824981237981237" :kim "BEN"}, göster!}

; Kümeler
;; Kümeler eşsiz eleman barındıran bir yapıdır.
;; Matematikteki karşılığını veriyor dersek yanlış olmaz.
#{:sen 3 milyar 750 milyon}

;; |--------------------------------|
;  |------ Etiketli Elemanlar ------|
;; |--------------------------------|

;; EDN (Genişletilebilir Veri Notasyonu), # sembolü ile genişletilebilir.

#benimuygulamam/bağlantı {:içerik "Y dakikada EDN Öğren" :url "https://learnxinyminutes.com/docs/tr-tr/edn-tr" :tıhlama-aksiyonu yırrttılll!}

;; Ve bu yapıyı yorumlayacak bir de yapı gerekiyor.
(defn ->bağlantı [props]
  (str "<a href='" (:url props) "'" ">"
        (:içerik props)
        "</a>"))

;; Bu örnekte yorumlayıcıya, basit bir fonksiyon veriyoruz.
;; `clojure.edn/read-string` aslında bir ayarlar Eşlemesi kabul ediyor.
;; (Bu tür fonksiyon genişlemeleri, Clojure ekosisteminde yaygındır.)

(clojure.edn/read-string
  {:readers {'benimuygulamam/bağlantı ->bağlantı}}
  "#benimuygulamam/bağlantı {:içerik \"Y dakikada EDN Öğren\" :url \"https://learnxinyminutes.com/docs/tr-tr/edn-tr\" :tıhlama-aksiyonu yırrttılll!}")
;=> "<a href='https://learnxinyminutes.com/docs/tr-tr/edn-tr'>Y dakikada EDN Öğren</a>"

;; |--------------------------------|
;  |--- Ön Tanımlı Genişletmeler ---|
;; |--------------------------------|

; Tarih Etiketi
;; Bu etiket `inst` ön-ekinden sonra bir RFC-3339 formatında bir karakter katarı beklemektedir.
#inst "2013-10-21T14:50:00+00:00" ; => Formatlanmış bir şekilde: 21/10/2013 14:50:00

; UUID Etiketi
;; Bu etiket `uuid` ön-ekinden sonra bir UUID karşılığını karakter katarı olarak kabul eder.
#uuid "11k12fae-7d3c-11k0-a765-0010ckke6hgk"

```

# Son Ek
Bu içerik, EDN'i tanıtmakta kısıtlı bir açıyla, özet bilgiler sunmaktadır.
Fakat, Clojure ve diğer Veri Odaklı dillerde, Verinin yolculuğunu anlamak için önemli bir rol oynamaktadır.
EDN'in var olan probleme çözümü ve artı/eksilerinin doğru şekilde kavranması mühimdir.
Ben bu dökümanı hazırlarken, EDN ve gerçek dünya kullanımını anlatan yoktu. Fakat ümidim, Clojure ve diğer teknolojiler üzerinde kullanımının artmasından sonra birinin bu ihtiyacı giderecek özgün kaynak çıkarmasıdır.

Başarılar!

# Referanslar

- [EDN Formatı Standardı](https://github.com/edn-format/edn)
- [Gerçeklemeler](https://github.com/edn-format/edn/wiki/Implementations)
- [Etiketlenmiş Elementler](http://www.compoundtheory.com/clojure-edn-walkthrough/)
- [Clojure.Docs EDN İçeriği](https://clojuredocs.org/clojure.edn)
