---
language: bf
filename: brainfuck-tr
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io"]
translators:
    - ["Haydar KULEKCI", "http://scanf.info/"]
lang: tr-tr
---

Brainfuck (normalde brainfuck olarak bütün harfleri küçük olarak yazılır.) 
son derece minimal bir programlama dilidir. (Sadece 8 komut) ve tamamen 
Turing'dir.

```
"><+-.,[]" (tırnak işaretleri hariç) karakterleri dışındaki her karakter
gözardı edilir.

Brainfuck 30,000 hücresi olan ve ilk değerleri sıfır olarak atanmış bir
dizidir. İşaretçi ilk hücreyi işaret eder.

Sekiz komut vardır:
+ : Geçerli hücrenin değerini bir artırır.
- : Geçerli hücrenin değerini bir azaltır.
> : Veri işaretçisini bir sonraki hücreye hareket ettirir(sağdaki hücreye).
< : Veri işaretçisini bir önceki hücreye hareket ettirir(soldaki hücreye).
. : Geçerli hücrenin ASCII değerini yazdırır (örn: 65 = 'A').
, : Bir girdilik karakteri aktif hücre için okur.
[ : Eğer geçerli hücredeki değer sıfır ise, ]ifadesine atlar.
    Diğer durumlarda bir sonraki yönergeye geçer.
] : Eğer geçerli hücredeki değer sıfır ise, bir sonraki yönergeye geçer.
    Diğer durumlarda, [ ifadesine karşılık gelen yönergelere döner.

[ ve ] bir while döngüsü oluşturur. Açıkça, dengeli olmalıdırlar.

Basit bir brainfuck programına göz atalım.

++++++ [ > ++++++++++ < - ] > +++++ .

Bu program 'A' karaterini ekrana basar. İlk olarak, #1'inci hücre 6'ya artırılır.
#1'inci hücre döngü için kullanılacaktır. Sonra, ([) döngüsüne girilir ve
#2'inci hücreye hareket edilir. #2'inci hücre 10 kez artırılır, #1'inci hücreye
geri dönülür. #1 hücresini bir azaltır. Bu döngü 6 kez gerçekleşir. (Bu 6 kez
azaltmak demektir, #1 hücresi 0 değerini alır ve bu noktada ] ifadesini atlar).

Bu noktada, biz #1 hücresindeyiz, değeri şu anda 0 ve #2 hücresinin değeri
60'tır. Biz #2 hücresine hareket diyoruz ve bu hücreyi 5 defa artırıyoruz.
#2'nin şu anki değeri 65 olur. Sonra #2 hücresinin ASCII karşılığını
yazdırıyoruz. 65 değerinin ASCII karşılığı 'A'dır. Ekrana 'A' yazılacaktır.


, [ > + < - ] > .

Bu program kullanıcıdan bir girdi okur, ve karakteri bir diğer hücreye yazdırır,
ve daha sonra aynı karakteri ekrana yazdırır.

, ifadesi kullanıcıdan karakteri #1 hücresine okur. Sonra bir döngü
başlar. #2 hücresine hareket edilir, #2 hücresinin değeri bir artırılır, #1
hücresine geri dönülür, ve #1 hücresinin değer bir azaltılır. Bu #1 hücresinin
değeri 0 olana kadar devam eder ve #2 hücresi #1'in eski değerini tutar. Çünkü
biz #1 hücresindeki verileri döngü süresince #2 hücresine taşıyoruz, ve sonunda
#2 hücresinin ASCII değerini yazdırıyoruz.

Boşluk karakteri sadece okunabilirliği artırmak içindir. Aşağıdaki gibi de
yazabilirsiniz.

,[>+<-]>.


Bu uygulamanın ne yaptığına bakalım:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Bu program 2 sayı alır, ve birbiri ile çarpar.

Özetle, ilk olarak iki girdi alır. Sonra, #1 hücresinde şarta bağlı harici bir
döngü başlar. Sonra #2 ye hareket edilir, ve içerde #2 hücresine bağlı bir döngü
daha başlar ve #3 hücresinin değerini artırır. Ama, Bir problem vardır: iç
döngünün sonunda #2'inci hücrenin değeri 0 olacaktır. Bunu çözmek için #4
hücresinin de değerini yükseltiyoruz, ve sonra #4 hücresinin değerini #2'ye
kopyalıyoruz.
```

İşte brainfuck. Zor değil değil mi? Eğlenmek için kendi programınızı
yazabilirsiniz, veya farklı bir dilde brainfuck yorumlayıcısı yazabilirsiniz.
Yorumlayıcı oldukça basittir, ama mazoşist iseniz, brainfuck içerisinde bir
brainfuck yorumlayıcısı yazmayı deneyebilirsiniz.
