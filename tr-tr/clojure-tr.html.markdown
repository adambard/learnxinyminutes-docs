---
language: clojure
lang: tr-tr
filename: learnclojure-tr.clj
contributors:
    - ["Adam Bard", "http://adambard.com/"]
    - ["Seçkin KÜKRER", "https://leavenha.github.io"]
translators:
    - ["Seçkin KÜKRER", "https://leavenha.github.io"]
---

[JVM]: https://tr.wikipedia.org/wiki/Java_sanal_makinesi
[STM]: https://en.wikipedia.org/wiki/Software_transactional_memory

Clojure, Lisp dialekti, barınan bir dildir. [JVM][JVM] üzerinde barınıyor. Clojure, Lisp'in tüm gücü ve kendi mantalitesi ile mükemmel bir genel-amaçlı programlama dilidir. Clojure, Eş-zamanlı programlama, Makrolar, Fonksiyonel Programlama, Tembel yapılar ve daha fazlasını vaadediyor.

(Bu örnekleri çalıştırmak için Clojure 1.2 versionu veya daha yenisine sahip olmalısınız.)


```clojure
; Noktalı Virgül, satırı yorumlamak için kullanılır.

; Clojure programları formlardan meydana gelir,
; Parantezlerle çevirili değerler, boşluk ile ayrılır. --Virgül ile değil--
;
; Clojure okuyucusu*, listedeki ilk elemanı çağırılacak bir fonksiyon
; Veya makro, geri kalan ifadeleri o çağırıma argüman olarak kabul eder.
;

; Bir dosyadaki ilk çağırım isim-uzayı tanımlamak için `ns` olmalı.
(ns clojure-öğren)

;
; Bir diğer yorumlama seçeneği de, ifade-içi. Bu diyez (`#`), ve alt çizgi
; İle başlar ve herhangi bir s-ifade'ye uygulanabilir.
;
#_(bu çağırım değerlendirilmeyecektir)

; Öncelikle fonksiyon çağırımları ve temel işlemler:

; Örnek bir fonksiyon çağırımı:
; (örnek-bir-fonksiyon ilk-argüman ikinci-argüman)

; `str` aldığı argümanları bir karakter katarı olarak geri verir.
(str "Merhaba" " " "dünya!") ; => "Merhaba dünya!"

; Matematik, oldukça sezgisel ve basit
(+ 1 1) ; => 2
(- 2 1) ; => 1
(* 1 2) ; => 2
(/ 2 1) ; => 2

; Eşitlik için `=`
(= 1 1) ; => true
(= 2 1) ; => false

; `not` beklediğiniz gibi, mantıksal ifadeleri tersine çevirir.
(not true) ; => false

; Clojure formları, iç-içe çağırılabilir
; Değerlendirilen çağırımlar bir üst form'a argüman
; Olarak verilir.
(+ 1 (- 3 2)) ; = 1 + (3 - 2) => 2

; Tipler
;;;;;;;;;;;;;

; Clojure, Java'nın temel tipleri olan mantıksal (boolean),
; Tam sayılar (int) ve karakter katarlarını (string) kullanır.
; Değerleri denetlemek için `class` fonksiyonunu kullanın.
(class 1) ; Tam sayı sabitleri ön-tanımlı olarak `java.lang.Long` ile tanımlanır.
(class 1.); Kayan noktalı sayı sabitleri
; Ön-tanımlı olarak `java.lang.Double` ile tanımlanır.
(class ""); Karakter katarı sabitleri her zaman, --sadece-- çift tırnak
; ile tanımlanır ve ön-tanımlı olarak `java.lang.String` tipindedir.
(class false) ; Mantıksal değer sabitleri, `java.lang.Boolean`.
(class nil); "Null", (tanımsız) değerler `nil` ile tanımlanır.

; Clojure okuyucusu her paranter ifadesini bir çağırım olarak
; değerlendirdiğinden bir liste tanımlamak için çağırımı durdurmalıyız.
'(+ 1 2) ; => (+ 1 2)
; ((quote (+ 1 2)) için bir kısa yoldur)

; Alıntılanmış listeleri çağırabilirsiniz.
(eval '(+ 1 2)) ; => 3

; Koleksiyonlar ve Ardışıklar
;;;;;;;;;;;;;;;;;;;

; Listeler bağlı-liste veri yapısı,
; Vektörler dizi altyapısı kullanır. 
(class '(1 2 3)); => clojure.lang.PersistentList
(class [1 2 3]); => clojure.lang.PersistentVector

; Bir liste `(1 2 3)` şeklinde gösterilebilir, yazılabilir.
; Fakat bu listeyi, Alıntılamalıyız --Quote--.
; Bu, onu bir fonksiyon çağırımı olarak değil,
; bir liste olarak değerlendirilmesini sağlayacaktır.
; Ayrıca, `(list 1 2 3)` tamamen `'(1 2 3)` ifadesi ile
; eşdeğerdir.

; 'Koleksiyonlar' sadece bir veri grubudur.
; Vektörler ve Listeler, koleksiyondur:
(coll? '(1 2 3)) ; => true
(coll? [1 2 3]) ; => true

; 'Ardışıklar' (seqs), bir veri listesinin soyut tanımlamasıdır.
; Sadece listeler ardışıktır.
(seq? '(1 2 3)) ; => true
(seq? [1 2 3]) ; => false

; Bir ardışık, ulaşıldığında sadece giriş verisi vermelidir.
; Yani, ardışıklar tembel olabilir. | Sonsuz ardışıklar tanımlanabilir.
(range 4) ; => (0 1 2 3)
(range) ; => (0 1 2 3 4 ...) (sonsuz bir ardışık)
(take 4 (range)) ;  (0 1 2 3)

; Bu yapılarda ekleme işlemi için `cons` kullanılır.
(cons 4 [1 2 3]) ; => (4 1 2 3)
(cons 4 '(1 2 3)) ; => (4 1 2 3)

; `conj` bir koleksiyona en verimli şekilde veri ekler.
; Bu, listeler için liste başına, vektörler için ise vektör sonuna demektir.
(conj [1 2 3] 4) ; => [1 2 3 4]
(conj '(1 2 3) 4) ; => (4 1 2 3)

; `concat` koleksiyonları birleştirmek için kullanılır.
(concat [1 2] '(3 4)) ; => (1 2 3 4)

; `filter` ve `map` koleksiyonlarla işlem yapmak için
; ön-tanımlı yüksek-seviyeli fonksiyonlardır.
;
; ps: `inc` argümanını bir arttıran bir fonksiyon.
(map inc [1 2 3]) ; => (2 3 4)
(filter even? [1 2 3]) ; => (2)

; Koleksiyonları indirgemek için `reduce` kullanılır.
(reduce + [1 2 3 4])
; = (+ (+ (+ 1 2) 3) 4)
; => 10

; Reduce, bir ilk-tanım değeri alabilir.
(reduce conj [] '(3 2 1))
; = (conj (conj (conj [] 3) 2) 1)
; => [3 2 1]

; Fonksiyonlar
;;;;;;;;;;;;;;;;;;;;;

; Yeni bir fonksiyon oluşturmak için `fn` kullanın.
; Bir fonksiyon her zaman son ifadesini döndürür.
(fn [] "Merhaba Dünya!") ; => fn

; Fonksiyonu çağırmak için bir çift paranteze daha ihtiyaç var.
((fn [] "Merhaba Dünya!")) ; => "Merhaba Dünya!"

; İsim uzayında bir değişken tanımlamak için `def`
; kullanılır.
(def x 1)
x ; => 1

; Bir değişkene fonksiyon değeri atamak için,
(def merhaba-dünya (fn [] "Merhaba Dünya!"))
(merhaba-dünya) ; => "Merhaba Dünya!"

; Bu süreci, `defn` ile kısaltabiliriz.
(defn hello-world [] "Merhaba Dünya!")

; `defn` fonksiyon çağırımındaki üçüncü eleman
; --vektör-- bir argüman listesidir. Fonksiyonun alacağı
; argümanları tanımlar.
(defn merhaba [isim]
  (str "Merhaba " isim))
(merhaba "Dünya!") ; => "Merhaba Dünya!"

; Ayrıca, `#()` kısa yolunu, fonksiyon deklare etmek için
; kullanabiliriz.
(def merhaba2 #(str "Merhaba " %1))
(merhaba2 "Dünya!") ; => "Merhaba Dünya!"

; Çok düzeyli fonksiyonlar da tanımlanabilir,
(defn merhaba3
  ([] "Merhaba Dünya!")
  ([isim] (str "Merhaba " isim)))
(merhaba3) ; => "Merhaba Dünya!"
(merhaba3 "A. NESİN!") ; => "Hello A. NESİN!"

; Fonksiyonlar, belirsiz-sayıda argüman alabilir,
; ve bunları sizin için bir ardışıkta depolayabilir.
(defn argüman-sayısı [& argümanlarım]
  (str "Verilen argüman sayısı:" (count argümanlarım) ", argümanlar: " argümanlarım))
(argüman-sayısı "Öğün" "Çalış" "Güven")
; => "Verilen argüman sayısı:3, argümanlar: ("Öğün" "Çalış" "Güven")"

; Elbette, sıradan ve belirsiz-sayılı fonksiyon argümanlarını
; harmanlayabilirsiniz.
(defn merhabalar [ev-sahibi & misafirler]
  (str "Merhabalar, " misafirler ". Benim adım " ev-sahibi "."))
(merhabalar "İklim" "Ayşe" "Fatma" "Nurdan")
; => "Merhabalar, (\"Ayşe\" \"Fatma\" \"Nurdan\"). Benim adım İklim."


; Eşlemeler
;;;;;;;;;;

; Hash-Maps, Array-Maps
; Hash-Eşlemeleri ve Dizi-Eşlemeleri bir arayüzü paylaşırlar.
; Hash-Eşlemeleri daha hızlıdır, fakat anahtar sıralaması tutmazlar.
(class {:a 1 :b 2 :c 3}) ; => clojure.lang.PersistentArrayMap
(class (hash-map :a 1 :b 2 :c 3)) ; => clojure.lang.PersistentHashMap

; Dizi-Eşlemeleri bir çok işlem sırasında otomatik olarak Hash-Eşlemelerine
; dönüşürler. Eğer yeterince büyürlerse, endişelenmenize gerek yoktur.

; Eşlemeler anahtar değeri olarak herhangi hash-ifadesi (hashable)
; alabilirler. Ama çoğunlukla, bu iş için anahtar-kelimeler `keyword`
; kullanılır.
; Anahtar-kelimeler, karakter katarları gibidirler, fakat
; bir kaç artıları vardır.
(class :a) ; => clojure.lang.Keyword

(def karakterkatarı-eşlemesi {"a" 1, "b" 2, "c" 3})
karakterkatarı-eşlemesi  ; => {"a" 1, "b" 2, "c" 3}

(def anahtar-eşlemesi {:a 1, :b 2, :c 3})
anahtar-eşlemesi ; => {:a 1, :c 3, :b 2}

; Bu arada, virgüller her zaman boşluk olarak değerlendirilir
; ve etkisizdirler.

; Bir eşlemeleden fonksiyon notasyonu ile değer çağırmak,
(karakterkatarı-eşlemesi "a") ; => 1
(anahtar-eşlemesi :a) ; => 1

; Keyword tipleri kendi değerlerini argüman olarak aldıkları bir
; eşlemeden değer notasyonu ile çağırabilirler.
(:b anahtar-eşlemesi) ; => 2

; Bu notasyonu, bir karakter katarı ile denemeyiniz.
;("a" karakterkatarı-eşlemesi)
; => Exception: java.lang.String cannot be cast to clojure.lang.IFn

; Verilmemiş bir değeri çağırmak, `nil` döndürecektir.
(karakterkatarı-eşlemesi "d") ; => nil

; Eşlemelere yeni değerler eklemek için `assoc` kullanırız.
(def yeni-anahtar-eşlemesi (assoc anahtar-eşlemesi :d 4))
yeni-anahtar-eşlemesi ; => {:a 1, :b 2, :c 3, :d 4}

; Ama unutmayın, Clojure veri yapıları değişmezdir!
anahtar-eşlemesi ; => {:a 1, :b 2, :c 3}

; Değer silmek için ise `dissoc` kullanılır.
(dissoc anahtar-eşlemesi :a :b) ; => {:c 3}

; Kümeler
;;;;;;

(class #{1 2 3}) ; => clojure.lang.PersistentHashSet
(set [1 2 3 1 2 3 3 2 1 3 2 1]) ; => #{1 2 3}

; `conj` ile bir değer eklenir.
(conj #{1 2 3} 4) ; => #{1 2 3 4}

; `disj` ile değer çıkarılır.
(disj #{1 2 3} 1) ; => #{2 3}

; Fonksiyon notasyonu kümelerde de tanımlıdır.
; Kendi içlerinde değer arayan bir fonksiyon olarak
; kullanılabilirler.
(#{1 2 3} 1) ; => 1
(#{1 2 3} 4) ; => nil

; `clojure.sets` isim-uzayında daha fazla fonksiyon vardır.

; Kullanışlı Formlar
;;;;;;;;;;;;;;;;;

; Clojure için mantıksal yapılar bir özel-form'dur.
; Ve diğer fonksiyonlar gibi kullanılabilir.
; `if` fonksiyonunun ilk argümanı bir test ifadesidir.
(if true "ya şundadır" "ya bunda") ; => "ya şundadır"
; İkinci ifade doğru, üçüncü ifade ise yanlışsa ifadeleridir.
; Eğer test terimi doğru olarak değerlendirilirse,
; doğru ifadesi, yanlışsa yanlış ifadesi değerlendirilir ve döndürülür.
;
; Bir yanlışsa ifadesi yoksa `nil` döndürülür.
(if false "a") ; => nil

; Yerel geçici-değişken tanımlamak için `let` kullanılır.
; İfadelerin varlığı `let` çağırımı ile sınırlıdır.
(let [a 1 b 2]
  (> a b)) ; => false

; İfade ve çağırımları `do` ile gruplayabilirsiniz.
; Çağırımların sonuncusu `do` ifadesinin değeri olarak 
; döndürülecektir.
(do
  (print "Selamlar!")
  "Dünya!") ; => "Dünya!" (prints "Selamlar!")

; Fonksiyonlar kapalı bir `do` ifadesi ile çevrelenmiştir.
(defn yazdır-ve-selamla! [isim]
  (println "Merhaba, " isim "!")
  (str "Merhaba, " isim "!"))
(yazdır-ve-selamla! "Zübeyde") ;=> "Merhaba, Zübeyde!" ("Merhaba, Zübeyde!" yazdırır.)

; `let` ifadesi de kapalı bir `do` ile gelmektedir.
(let [isim "Ayten"]
  (print "Merhabalar, " isim)
  (str "Merhabalar, " isim)) ; => "Merhabalar, " ("Merhabalar, Ayten" yazdırır)

; Sıralama-makroları (-> ve ->>) ile veri dönüşümünü daha temiz ifade
; edebilirsiniz.
; Bu makrolar ilk argümanı sonraki her çağırımın içine yerleştirir.
;
; `->` makrosu, ifadeyi çağırımların ilk argümanı olacak şekilde yerleştirir.
(->
   {:a 1 :b 2} 
   (assoc :c 3) ;=> (assoc {:a 1 :b 2} :c 3)
   (dissoc :b))
   
; Bu ifade aşağıdaki şekilde yazılabilir:
; (dissoc (assoc {:a 1 :b 2} :c 3) :b)
; ve `{:a 1 :c 3}` olarak değer bulur.

; Sondan-Sıralama-Makrosu (->>) ise aynı şeyi yapar,
; tek fark ise, ifadeyi, çağırımların son argümanı olarak yerleştirir.
;
(->>
   (range 10)    ;=> '(0 1 2 3 4 5 6 7 8 9)
   (map inc)     ;=> (map inc (range 10))
   (filter odd?) ;=> (filter odd? (map inc (range 10)))
   (into []))    ;=> (into [] (filter odd? (map inc (range 10))))
                 ; Sonuç: [1 3 5 7 9]

; Bir ifadedeki önceki veri dönüşümlerinin sonucunu nereye
; koyacağınız konusunda daha fazla özgürlük istediğiniz bir durumda,
; Sıralama-Makrolarından daha özgür bi' şey kullanmak istersiniz;
; `as->` makrosu ile dönüşümlerin çıktısına bir isim atayabilir
; ve ardışık çağırımlarda yer tutucu olarak kullanabilirsiniz.

(as-> [1 2 3] girdi
  (map inc girdi);=> ifadeyi isterseniz çağırımın son argümanı olarak,
  (nth girdi 2) ;=>  veya çağırımın ilk argümanı olarak, 
  (conj [4 5 6] girdi [8 9 10])) ;=> ya da istediğiniz sırada kullanabilirsiniz.
;=> [4 5 6 4 [8 9 10]]



; Modüller
;;;;;;;;;;;;;;;

; `use` çağırdığınız modüldeki tüm tanımlara erişmenize olanak verir.
(use 'clojure.set)

; Şu anda, küme fonksiyonlarını kullanabiliriz.
(intersection #{1 2 3} #{2 3 4}) ; => #{2 3}
(difference #{1 2 3} #{2 3 4}) ; => #{1}

; Ayrıca eklenecek fonksiyonları seçebilirsiniz de:
(use '[clojure.set :only [intersection]])

; Bir modülü eklemek için `require` kullanılır. 
(require 'clojure.string)

; İsim-uzayı kapsamlı çağırımlar aşağıdaki şekildedir:
; isim-uzayı/fonksiyon-ismi --isim uzayı ismi ve fonksiyon ismi
; arasına eğik çizgi koymanız yeterli.
; Burada, modül `clojure.string` ve fonksiyon ismi `blank?`
(clojure.string/blank? "") ; => true

; Ekleme sırasında, bir modüle takma-ad verilebilir.
(require '[clojure.string :as str])
(str/replace "Bu bir özet metindir, test için kullanılabilir!"
  #"[aeıioöuü]" str/upper-case)
; => "BU bIr ÖzEt mEtIndIr, tEst IçIn kUllAnIlAbIlIr!"
; (#"", burada düzenli ifadeler için bir sözdizimsel-şekerlemeyi ifade eder)

; Bir isim-uzayı tanımlamasında `require` kullanılabilir.
; `ns` bir makrodur ve `require` (ve `use`, ama lütfen kullanmayın)
; dahil olmak üzere bir çok çağırım için işlevsellik sağlamaktadır.
; Bu notasyonu kullanırsanız, modüllerinizi alıntılamak zorunda kalmazsınız.
(ns test
  (:require
    [clojure.string :as str]
    [clojure.set :as set]))


; Java
;;;;;;;;;;;;;;;;;

; Java, kocaman ve kullanışlı bir standart kütüphaneye sahip,
; Clojure, Java etkileşimi ile, bundan yararlanabilirsiniz.

; `import` diğer modüller gibi, bir java modülü de ele alabilir.
; Date, bir Java modülü.
(import java.util.Date)

; `ns` çağırımında da kullanabilirsiniz.
(ns test
  (:import java.util.Date
           java.util.Calendar))
           
; Bir Java nesnesinden oluşturmak için `new` çağırımını kullanabilirsiniz.
(new Date)

; Ayrıca Clojure Okuyucusu, size bunun daha farklı bir yolunu sunar:
; Sınıf isminin sonuna koyulacak bir nokta `.` ile
; bu yapılabilir.
(Date.) ; <bir tarih nesnesi>

; `.` --nokta-- çağırımı, size nesnelerdeki metotlara erişme imkanı verir.
(. (new Date) getTime) ; <bir zaman-damgası>
(.getTime (Date.)) ; Üstteki ifade ile tamamen aynı sonucu verir.

; Sınıf içindeki statik metotlara erişmek için `/` ayracını
; sınıf ile metot ismi birleştirmek için kullanabilirsiniz.
; (örnekSınıf/statikMetot)
(System/currentTimeMillis) ; <bir zaman-damgası> (`system` her zaman sunulur)

; Sınıflarla işlem yaparken, `doto` bu süreci kolaylaştırabilir.
; İlk argüman sınıf nesnesi, sonraki her çağırım, nesne üzerinde yapılır.
(import java.util.Calendar)
(doto (Calendar/getInstance)
  (.set 2000 1 1 0 0 0) ; => `set` metodu, `doto` ifadesine verilen
                        ; sınıf nesnesi üzerinde çağırılır.
  .getTime) ; => Bir tarih nesnesi. set to 2000-01-01 00:00:00


; STM
;;;;;;;;;;;;;;;;;

; 'Software Transactional Memory' Clojure'un değişmez veri yapılarını
; ele alırken kullandığı bir mekanizmadır. Clojure içinde bunu kullanan
; birkaç yapı vardır.

; Bir `atom` en basitidir. Bir ilkleme-değeri verin.
(def benim-atomum (atom {}))

; Bir atomu güncellemek için `swap!` kullanılır.
; `swap!` fonksiyonu, ilk argüman olarak aldığı atomu, ikinci argüman
; olarak aldığı fonksiyona uygular. Bu fonksiyona ek argümanlar ise
; fonksiyondan sonra gelirler.
(swap! benim-atomum assoc :a 1)
; benim-atomum'un değerini (assoc {} :a 1) ifadesinin sonucu ile değiştirir.
(swap! benim-atomum assoc :b 2)
; benim-atomum'un değerini (assoc {:a 1} :b 2) ifadesinin sonucu ile değiştirir.

; `deref` ile, atomun değerini çözümleyebilirsiniz. 
benim-atomum  ;=> Atom<#...> (Atom ifadesi döndürür)
@benim-atomum ; => {:a 1 :b 2}

; İşte, `atom` kullanan basit bir sayaç.
(def sayaç (atom 0)) ;=> Şu anki isim uzayına, `sayaç` ile, 0 başlangıç
; değeri ile bir atom tanımladık.
(defn sayaç-arttır [benim-atomum]
  (swap! sayaç inc)) ;=> Atom'un değerini bir arttır.

(sayaç-arttır sayaç)
(sayaç-arttır sayaç)
(sayaç-arttır sayaç)
(sayaç-arttır sayaç)
(sayaç-arttır sayaç)
(sayaç-arttır sayaç)

@sayaç ; => 6

; Diğer STM yapıları `ref`'ler ve `agent`'lar.
; Ref'ler: http://clojure.org/refs
; Agent'lar: http://clojure.org/agents
```

### Çevirim-içi içerikler

Bu içerik, Rich Hickey'nin derin yazılım geliştirme anlayışına ve John McCarthy'nin vizyonu olan Lisp'in, Clojure'a miras verdiklerini anlamak için elbette yeterli değildir. Fakat fonksiyonel paradigma ve bu paradigmanın modern bir Lisp lehçesinde kullanımına göz kırpmış oldunuz.

Clojure.org, bir çok içerik ve makale var. (İngilizce içerik):
[http://clojure.org/](http://clojure.org/)

Clojuredocs.org, örneklerle bezenmiş Clojure dökümantasyonu:
[http://clojuredocs.org/quickref/Clojure%20Core](http://clojuredocs.org/quickref/Clojure%20Core)

4Clojure, interaktif bir şekilde FP ve Clojure yeteneklerinizi geliştirmenize olanak veriyor:
[http://www.4clojure.com/](http://www.4clojure.com/)

Clojure-doc.org, Başlangıç için bir içeriklere sahip:
[http://clojure-doc.org/](http://clojure-doc.org/)

BraveClojure, bir başka clojure öğreten web sitesi:
[https://www.braveclojure.com/](https://www.braveclojure.com/)
