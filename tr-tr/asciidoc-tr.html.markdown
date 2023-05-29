---
language: asciidoc
contributors:
    - ["Ryan Mavilia", "http://unoriginality.rocks/"]
    - ["Abel Salgado Romero", "https://twitter.com/abelsromero"]
    - ["Fatih Küçükkarakurt", "https://github.com/fkkarakurt"]
filename: asciidoc-tr.md
lang: tr-tr
---

AsciiDoc, Markdown'a benzer bir biçimlendirme dilidir ve kitaplardan bloglara kadar her şey için kullanabilirsiniz. 2002 yılında Stuart Rackham tarafından oluşturulmutur. Basit bir dildir. Ama bu onu güçsüz yapmaz ve büyük miktarda özelleştirmeye izin verir.

### Belge Başlığı

Başlıklar isteğe bağlıdır ve boş satırlar içeremezler. Başlıkları kullanırken dikkat edilmesi gereken şey, içerikten en az bir satırla ayrılmaları gerektiğidir.

### Yalnız Başlık Kullanımına Bir Örnek

```asciidoc
= Belge Başlığı

Belgenin ilk cümlesini yazarken arada bir satır boşluk var. 
```

### Başlığın Yazar Satırı ile  Kullanımı

```asciidoc
= Belge Başlığı
Ad Soyad <ad.soyad@hotmail.com.tr>

Belgenin ilk cümlesini yazarken arada yine bir satır boşluk var. 
```

### Birden Fazla Yazar Varsa

```asciidoc
= Belge Başlığı
Cahit Arf <cahit@arf.com>; Oktay Sinanoğlu<oktay@so.com>; Mehmet Tomak <mehmet@tomak.com>

Belgenin ilk cümlesini yazarken arada yine bir satır boşluk var. 
```

### Versiyon ve Revizyon Bilgisi (Bunu yapmak için bir yazar satırına ihtiyacınız var)

```asciidoc
= Belge Başlığı v19.23
Mete Atatüre <mete@atature.com>
v19.23, 1923-29-10

Belgenin ilk cümlesini yazarken arada yine bir satır boşluk var. 
```

### Paragraflar

```asciidoc
Onsekiz, ondokuz, yirmi, yirmibeş... Yaşlarımızdır. Deli rüzgârların estiği dağlar başlarımızdır. Bamsı Beyrekleriz Banu Çiçekler Düşlerimizdir.

Şölenler eyleriz toylu, düğünlü, kıvrak omuzları bakır güğümlü, sülün göğüsleri sıkı düğümlü, kırk ince belli kız eşlerimizdir. Anarız en eski Türk çağlarını, aşarız her gece Kaf dağlarını, Tanrı Dağları'na konar, döneriz... Zümrüt-ü Ankalar kuşlarımızdır. 

Vakta ki, dil sustu, namlu konuştu...  +
Kurşunlara hedef döşlerimizdir.

Eğer boş bir satır isterseniz yukarıdaki gibi + işaretini kullanabilirsiniz.
```

### Metin Biçimlendirme

```asciidoc
_İtalik Metin_
*Bold Metin*
*_Bold ve İtalik Metin_*
`Monospace Fontuna Sahip Metin`
`*Bold ve Monospace Metin*`
```

### Bölüm Başlıkları

```asciidoc
= Seviye 0 (Bu sadece Belgenin Başlığı için)

== Seviye 1 <h2>

=== Seviye 2 <h3>

==== Seviye 3 <h4>

===== Seviye 4 <h5>
```

### Listeler

Eğer maddeler halinde bir liste oluşturacaksanız, yıldız (*) işaretini kullanabilirsiniz.

```asciidoc
* 1919
* 1920
* 1923
```

Eğer numaralı bir liste oluşturacaksanız, nokta  (.) işaretini kullanabilirsiniz.

```asciidoc
. Galatasaray
. Beşiktaş
. Fenerbahçe
```

Üstelik hem maddeli listelerde hem de numaralı listelerde iç içe kullanım yoluna da başvurmanız mümkün.

```asciidoc
* 1919
* * 19 Mayıs
* * * Samsun
* * * * Bandırma Vapuru
```

```asciidoc
. 1923
. . 23 Nisan
. . . Ankara
. . . . Meclis
```

### Öneriler

AsciiDoc dökümanlarını uygulamak için kullanabileceğiniz iki araç var:
- [AsciiDoc](https://asciidoc.org/):  Orijinal bir Python uygulaması. Teknik içerikler veya sadece düz bir metin düzenlemesi için kolaylıkla kullanabilirsiniz. 
- [Asciidoctor](https://asciidoctor.org/): Bu bir Ruby uygulaması ve hem Java hem de JavaScript ile rahatlıkla kullanabilirsiniz. Hala aktif olarak geliştiriliyor. Oldukça zengin içeriklere sahip. Hem kurulumu hem de dökümantasyonu oldukça kolay. 

Asciidoctor uygulamasını incelemek isterseniz aşağıdaki linkler işinizi kolaylaştıracaktır:
- [Markdown ve Asciidoctor Karşılaştırması](https://docs.asciidoctor.org/asciidoc/latest/asciidoc-vs-markdown/#comparison-by-example): Markdown ve Asciidoctor arasındaki temel farkları inceleyebilirsiniz.
- [Asciidoctor için Hızlı Başlangıç](http://asciidoctor.org/docs/#get-started-with-asciidoctor): Kurulum ve temel kullanım için buraya bakabilirsiniz.
- [Asciidoctor Dökümantasyonu](https://docs.asciidoctor.org/): Asciidoctor uygulamasına yönelik olarak tüm özellikleri ve kullanım ile ilgili örnekleri burada bulabilirsiniz. 
