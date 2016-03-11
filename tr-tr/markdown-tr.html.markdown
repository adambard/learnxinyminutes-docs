---
language: markdown
contributors:
    - ["Dan Turkel", "http://danturkel.com/"]
translators:
    - ["Eray AYDIN", "http://erayaydin.me/"]
lang: tr-tr
filename: markdown-tr.md
---

Markdown, 2004 yılında John Gruber tarafından oluşturuldu. Asıl amacı kolay okuma ve yazmayı sağlamakla beraber kolayca HTML (artık bir çok diğer formatlara) dönüşüm sağlamaktır.


```markdown
<!-- Markdown, HTML'i kapsar, yani her HTML dosyası geçerli bir Markdown dosyasıdır, bu demektir
ki Markdown içerisinde HTML etiketleri kullanabiliriz, örneğin bu yorum elementi, ve
markdown işleyicisinde etki etmezler. Fakat, markdown dosyası içerisinde HTML elementi oluşturursanız,
bu elementin içeriğinde markdown söz dizimlerini kullanamazsınız. -->

<!-- Markdown ayrıca işleyiciden işleyiciye farklılık gösterebilir. Bu rehberde
evrensel özelliklere uygun anlatımlar olacaktır. Bir çok işleyici bu rehberdeki
anlatımları destekler -->

<!-- Başlıklar -->
<!-- Kolayca <h1>'den <h6>'ya HTML etiketleri oluşturabilirsiniz.
Kare (#) sayısı bu elementin numarasını belirleyecek ve devamında getirdiğiniz
yazı bu elementin içeriği olacaktır
-->
# Bu bir <h1>
## Bu bir <h2>
### Bu bir <h3>
#### Bu bir <h4>
##### Bu bir <h5>
###### Bu bir <h6>

<!-- Markdown ayrıca h1 ve h2 için 2 alternatif yol daha taşır -->
Bu bir h1
=========

Bu bir h2
---------

<!-- Basit yazı stilleri -->
<!-- Markdown ile yazılar kolayca italik ve kalın belirtilebilir -->
*Bu yazı italik.*
_Bu yazı da italik._

**Bu yazı kalın.**
__Bu yazı da kalın.__

***Bu yazı hem kalın hem italik.***
**_Bu da öyle!_**
*__Hatta bu bile!__*

<!-- GitHub Flavored Markdown'da ayrıca üstü çizgili karakter de desteklenir: -->
~~Bu yazı üstü çizili olarak gözükecek.~~

<!-- Paragraflar bir veya daha fazla boş satırla ayrılır. -->

Bu bir paragraf. Paragrafın içeriğine devam ediyorum, eğlenceli değil mi?

Şimdi 2. paragrafıma geçtim.
Hala 2. paragraftayım, çünkü boş bir satır bırakmadım.

Bu da 3. paragrafım!

<!-- HTML'de her satır için <br /> etiketi kullanmak ister misiniz, Bir
paragrafı bitirdikten sonra 2 veya daha fazla boşluk bırakın ve yeni paragrafa
başlayın, bu bir <br /> etiketi sayılacaktır  -->

Bu yazının sonunda 2 boşluk var (bu satırı seçerek kontrol edebilirsiniz).  

Bir üst satırda <br /> etiketi var!

<!-- Blok yazılarının yapımı oldukça kolay, (>) karakteri ile yapabilirsiniz  -->

> Bu bir blok etiketi. Satırlara ayırmak için
> her satırın başında `>` karakter yerleştirmeli veya tek satırda bütün içeriği yazabilirsiniz.
> Satır `>` karakteri ile başladığı sürece sorun yok. 

> Ayrıca alt alta da blok elementi açabilirsiniz
>> iç içe yani
> düzgün değil mi ?

<!-- Listeler -->
<!-- Numarasız listeler için yıldız, artı, veya tire kullanabilirsiniz -->

* Nesne
* Nesne
* Bir başka nesne

veya

+ Nesne
+ Nesne
+ Bir başka nesne

veya

- Nesne
- Nesne
- Son bir nesne

<!-- Numaralı liste için başına sıralı bir şekilde sayı eklemeniz yeterli -->

1. İlk nesne
2. İkinci nesne
3. Üçüncü nesne

<!-- İsterseniz sıralı bir şekilde yazmak zorunda değilsiniz, markdown
biçimlendirirken sizin için sıralayacaktır, fakat bunu önermiyorum. Markdown dosyasının
düzgün gözükmesi için önerilen metodu uygulamanızı tavsiye ederim -->

1. İlk nesne
1. İkinci nesne
1. Üçüncü nesne

<!-- (Bunun çıktısı ile, sıralı olarak yazdığımız örneğin çıktısı aynı olacaktır) -->

<!-- Ayrıca alt alta liste oluşturabilirsiniz -->

1. İlk nesne
2. İkinci nesne
3. Üçüncü nesne
    * Alt nesne
    * Alt nesne
4. Dördüncü nesne

<!-- Ayrıca görev listeleri de bulunmakta. HTML seçim kutusu(checkbox) oluşturacaktır. -->
Kutunun içerisinde `x` yoksa eğer seçim kutusu boş olacaktır.
- [ ] Yapılacak ilk görev.
- [ ] Yapılması gereken bir başka görev
Aşağıdaki seçim kutusu ise içi dolu olacaktır.
- [x] Bu görev başarıyla yapıldı

<!-- Kod blokları -->
<!-- Kod bloklarını(<code> elementi) belirtmek için 4 adet boşluk veya bir
tab karakterini kullanabilirsiniz -->

    Bu bir kod
    öyle mi?

<!-- Ayrıca kod içerisinde girinti kullanmak istiyorsanız tekrar `tab` veya `4 boşluk`
kullanabilirsiniz -->

    my_array.each do |item|
        puts item
    end

<!-- Yazı içerisinde kod belirtmek için sorgu tırnağı (`) kullanabilirsiniz -->

Ahmet `go_to()` fonksiyonun ne yaptığını bilmiyor!

<!-- GitHub Flavored Markdown'da, kod içerisinde aydınlatma kullanabilirsiniz -->

\`\`\`ruby <!-- buradaki ters slaş (\) işaretlerini kullanmayın, sadece ```ruby ! -->
def foobar
    puts "Hello world!"
end
\`\`\` <!-- burada da (\) işaretlerini kullanmayın, sadece ``` -->

<!-- Yukarıdaki örnekte girinti kullanmanıza gerek yok, GitHub da 
``` işaretinden sonra belirttiğiniz yazılım diline göre gerekli
syntax aydınlatmaları uygulanacaktır -->

<!-- Düz çizgi (<hr />) -->
<!-- Düz çizgiler 3 veya daha fazla yıldız/çizgi ile yapılabilir. Boşluklar önemsiz. -->

***
---
- - -
****************

<!-- Linkler -->
<!-- Markdown'daki en güzel şeylerden biri kolayca link oluşturmaktır. 
Linkte göstermek istediğiniz yazıyı [] içerisine yerleştirin ve sonuna parantezler içerisinde ()
gideceği adresi belirtin -->

[Bana tıkla!](http://test.com)

<!-- Ayrıca linke `title` özelliği eklemek için tırnakları kullanabilirsiniz -->

[Bana tıkla!](http://test.com "Test.com'a gider")

<!-- Bağıl yollar da çalışıyor. -->
[Müzik dinle](/muzik/).

<!-- Markdown ayrıca referans linklerini de destekler -->

[Bu linke tıklayarak][link1] daha detaylı bilgi alabilirsiniz!
[Ayrıca bu linki de inceleyin][foobar] tabi istiyorsanız.

[link1]: http://test.com/ "harika!"
[foobar]: http://foobar.biz/ "okey!"

<!--Başlık ayrıca tek tırnak veya parantez içinde olabilir, veya direk yazılabilir.
Referans döküman içerisindeki herhangi bir yer olabilir ve referans IDsi 
benzersiz olduğu sürece sorunsuz çalışacaktır. -->

<!-- Ayrıca "dolaylı adlandırma" bulunmaktadır, "dolaylı adlandırma", linkin yazısının
aynı zamanda onun idsi olmasıdır -->

[Bu][] bir link.
[bu]: http://bubirlink.com

<!-- Fakat bu çok tercih edilen bir yöntem değil. -->

<!-- Resimler -->
<!-- Resimler aslında linklere çok benziyor fakat başında ünlem bulunuyor! -->
![Bu alt etiketine gelecek içerik](http://imgur.com/resmim.jpg "Bu da isteğe bağlı olan bir başlık")

<!-- Referanslar resimler için de geçerli -->
![Bu alt etiketi.][resmim]

[resmim]: bagil/linkler/de/calisiyor.jpg "Başlık isterseniz buraya girebilirsiniz"

<!-- Çeşitli -->
<!-- Oto-linkler -->

<http://testwebsite.com/> ile
[http://testwebsite.com/](http://testwebsite.com) aynı şeyler

<!-- Oto-linkler epostaları da destekler -->

<foo@bar.com>

<!-- Kaçış karakterleri -->

Bu yazının *yıldızlar arasında gözükmesini* istiyorum fakat italik olmamasını istiyorum,
bunun için, şu şekilde: \*bu yazı italik değil, yıldızlar arasında\*.

<!-- Tablolar -->
<!-- Tablolar sadece GitHub Flavored Markdown'da destekleniyor ve açıkçası
performansı çok yoruyorlar, fakat illa ki kullanmak isterseniz: -->

| Sütun1       | Sütun 2  | Sütün 3       |
| :----------- | :------: | ------------: |
| Sola dayalı  | Ortalı   | Sağa dayalı   |
| test         | test     | test          |

<!-- ayrıca, bunun aynısı -->

Sütun 1 | Sütun 2 | Sütun 3
:-- | :-: | --:
Çok çirkin göözüküyor | değil | mi?

<!-- Bitiş! -->

```

Daha detaylı bilgi için, John Gruber'in resmi söz dizimi yazısını [buradan](http://daringfireball.net/projects/markdown/syntax) veya Adam Pritchard'ın mükemmel hatırlatma kağıdını [buradan](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) inceleyebilirsiniz.
