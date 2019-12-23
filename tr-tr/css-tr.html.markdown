---
language: css
contributors:
    - ["Mohammad Valipour", "https://github.com/mvalipour"]
    - ["Marco Scannadinari", "https://github.com/marcoms"]
    - ["Geoffrey Liu", "https://github.com/g-liu"]
    - ["Connor Shea", "https://github.com/connorshea"]
    - ["Deepanshu Utkarsh", "https://github.com/duci9y"]
    - ["Brett Taylor", "https://github.com/glutnix"]
    - ["Tyler Mumford", "https://tylermumford.com"]
filename: learncss-tr.css
translators:
    - ["Fatih Turan", "http://fatihturan.com"]
lang: tr-tr
---

Web sayfaları bir sayfanın içeriğini belirleyen HTML ile inşa edilirler. CSS (Basamaklı Biçim Sayfaları) ise bir sayfanın **görünümünü** belirleyen ayrı bir dildir.

CSS kodu statik *kurallardan* oluşur. Her kural bir ya da daha fazla *seçici* alır ve görsel *özelliklere* belirli *değerleri* verir. Sonrasında bu özellikler seçiciler tarafından belirlenen sayfa unsurlarına uygulanır.

Bu rehber, CSS 3'ün yeni özellikleri ile genişletilen CSS 2 ile dikkate alınarak yazılmıştır.

**NOT:** CSS görsel sonuçlar ürettiğinden dolayı, öğrenmek için herşeyi bir CSS oyun alanı içinde ([dabblet](http://dabblet.com) gibi) denemeniz gerekmektedir. Bu makale sözdizimi kuralları ve genel ipuçları üzerine odaklanmaktadır.

## Sözdizimi

```css
/* yorumlar bu satırdaki gibi taksim-yıldız içinde görünür
CSS'te "tek satırlık yorumlar" bulunmamaktadır; bu sadece tek bir yorum yazma stilidir */

/* ####################
   ## SEÇİCİLER
   #################### */

/* seçici bir sayfadaki unsuru hedeflemek için kullanılır. */
seçici { özellik: değer; /* daha fazla özellikler...*/ }

/*
İşte bir örnek:

<div class='class1 class2' id='anID' attr='value' otherAttr='en-us foo bar' />
*/

/*   */

/* CSS sınıflarının birini kullanarak hedefleyebilirsiniz */
.class1 { }

/* veya her iki sınıfı birden!*/
.class1.class2 { }

/* veya sadece ögenin adını yazarak */
div { }

/* veya onun ID adını */
#anID { }

/* veya onun aldığı bir özelliği kullanarak! */
[attr] { font-size:smaller; }

/* veya onun aldığı özelliğin belirli bir değeri varsa */
[attr='value'] { font-size:smaller; }

/* bir değer ile başlıyorsa (CSS 3) */
[attr^='val'] { font-size:smaller; }

/*  veya bir değer ile bitiyorsa (CSS 3)*/
[attr$='ue'] { font-size:smaller; }

/* veya boşlukla ayrılmış liste içinde bir değer içeriyorsa */
[otherAttr~='foo'] { }
[otherAttr~='bar'] { }

/*  veya tire ile ayrılmış bir liste içinde bir değer içeriyorsa, örneğin: "-" (U+002D) */
[otherAttr|='en'] { font-size:smaller; }

/*  Farklı seçicileri birleştirerek daha fazla odaklanmış bir seçici oluşturabilirsiniz. Seçiciler arasında boşluk bırakmayın. */
div.some-class[attr$='ue'] { }

/*  Başka bir ögenin alt ögesi olan bir ögeyi seçebilirsiniz. */
div.some-parent > .class-name { }

/* veya bir başka ögeden türeyeni seçebilirsiniz. Alt ögeler onların ebeveynlerinin direkt türünden gelir, sadece ağacın bir alt ögeleridirler. Soyundan gelenler ağacın herhangi bir alt seviyesinde olabilir. */

div.some-parent .class-name { }

/* Uyarı: Seçiciler arasında bir boşluk bırakmazsanız aynı seçicinin başka bir anlamı olur.
Ne olduğunu tahmin edebilir misiniz?  */

div.some-parent.class-name { }

/*  Ayrıca bir ögenin bitişik kardeşini temel alarak bir ögeyi seçebilirsiniz. */
.i-am-just-before + .this-element { }

/*  veya kendisinden önce gelen herhangi bir kardeş ögeyi */
.i-am-any-element-before ~ .this-element { }

/* Yalnızca belli bir durumda bir öge seçmek için kullanılan sahte sınıflar adı verilen bazı seçiciler vardır. */

/* Örneğin, imleç bir ögenin üzerine geldiğinde  */
selector:hover { }

/* veya bir bağlantı ziyaret edildiğinde  */
selector:visited { }

/*  veya ziyaret edilmediğinde */
selected:link { }

/*  veya bir ögeye odaklanıldığında */
selected:focus { }

/*  Ebeveyninin ilk alt ögesi olan herhangi bir öge */
selector:first-child {}

/*  Ebeveyninin son alt ögesi olan herhangi bir öge */
selector:last-child {}

/* Sahte sınıflar gibi sahte elementler de bir dokümanın belirli bir parçasına  stil vermenize izin verir. */

/* Seçilen ögenin sanal ilk alt ögesiyle eşleşir. */
selector::before {}

/* Seçilen ögenin sanal son alt ögesiyle eşleşir. */
selector::after {}

/* Uygun yerlerde yıldız karakteri ile bütün ögeleri seçmek için joker olarak kullanılabilir. */

* { } /* Bütün ögeler */
.parent * { } /* Tüm alt ögeler */
.parent > * { } /* Tüm çocuk ögeler */

/* ####################
   ## ÖZELLİKLER
   #################### */

selector {
	
	/* Ölçü birimleri kesin veya göreceli olabilir.*/

	/* Göreceli birimler */
    width: 50%;       /* Ebeveyn elementin yüzdesel olarak genişliği */
    font-size: 2em;   /* Öğenin özgün yazı tipi boyutunda katları */
    font-size: 2rem;  /* veya kök ögenin yazı tipi boyutu */
    font-size: 2vw;   /* Görüntüleme çerçevesinin genişliğinin %1 olarak katları (CSS 3) */
    font-size: 2vh;   /* veya onun yüksekliğinin */
	font-size: 2vmin; /* Bir vh veya vw'nin hangisi küçükse */
	font-size: 2vmax; /* veya daha büyük... */

	/* Kesin birimler */
    width: 200px;     /* Piksel */
    font-size: 20pt;  /* Nokta */
    width: 5cm;       /* Santimetre */
    min-width: 50mm;  /* Milimetre */
    max-width: 5in;   /* İnç */

    /* Renkler */
    color: #F6E;                 /* Kısa onaltılık (HEX) biçimi */
    color: #FF66EE;              /* Uzun onaltılık (HEX) biçimi */
    color: tomato;               /* Bir isim verilen renk */
    color: rgb(255, 255, 255);   /* RGB değerleri verilen türde */
    color: rgb(10%, 20%, 50%);   /* RGB yüzdeleri verilen türde */
    color: rgba(255, 0, 0, 0.3); /* RGBA değerleri verilen türde (CSS 3) Not: 0 <= a <= 1 */
    color: transparent;          /* Şeffaflık değerinin sıfır olması ile eşdeğer */
    color: hsl(0, 100%, 50%);    /* HSL yüzdeleri verilen türde (CSS 3) */
    color: hsla(0, 100%, 50%, 0.3); /* HSL ile beraber şeffaflık değeri verilen türde */

    /* Kenarlıklar */
    border-width:5px;
    border-style:solid;
    border-color:red;      /* background-color'ın ayarlanışına benzer şekilde */
    border: 5px solid red; /* Bu aynı şeyin kısayol ile yazılışıdır */
    border-radius:20px;    /* Bu bir CSS3 özelliğidir  */

    /* Görseller ve Ögelerin Arkaplanları  */
    background-image: url(/img-path/img.jpg); /* url() içindeki tırnak işaretleri isteğe bağlı */

    /* Yazı tipleri */
    font-family: Arial;
    /* Eğer yazı tipi ailesi isminde bir boşluk var ise tırnak işareti içine alınmalıdır. */
    font-family: "Courier New";
    /* Eğer ilk sıradaki bulunamazsa, tarayıcı bir sonrakini kullanır */
    font-family: "Courier New", Trebuchet, Arial, sans-serif;
}
```

## Kullanım

CSS dosyasınızı `.css`uzantısı ile kaydedin.

```html
<!-- CSS dosyanızı sayfanın içindeki <head> alanına dahil etmeniz gerekiyor. Bu önerilen yöntemdir. Bakın: http://stackoverflow.com/questions/8284365 -->
<link rel='stylesheet' type='text/css' href='path/to/style.css'>

<!-- Bazı CSS kodları satır içi olarak yazabilirsiniz. -->
<style>
   a { color: purple; }
</style>

<!-- Veya ögenin üzerinde CSS özelliklerini direkt ayarlayabilirsiniz. -->
<div style="border: 1px solid red;">
</div>
```

## Öncelik veya Basamak

Bir öge birden çok seçici tarafından hedef alınabilir ve bir özellik kümesine birden fazla kez sahip olabilir. Bunun gibi durumlarda, kurallardan biri diğerlerine göre önceliklidir. Daha spesifik bir seçiciye sahip kurallar, daha az spesifik bir seçicinin önceliğini alır ve kural daha sonra stil sayfasında bir önceki kuralın üzerine yazar.

Bu işleme geçiş denir ve olayısıyla Geçişli/Basamaklı Stil Sayfaları adı da buradan gelmiştir.

Aşağıdaki CSS göz önüne alındığında:

```css
/* A */
p.class1[attr='değer']

/* B */
p.class1 { }

/* C */
p.class2 { }

/* D */
p { }

/* E */
p { özellik: değer !important; }
```

ve aşağıdaki biçimlendirmeyi:

```html
<p style='/*F*/ özellik:değer;' class='class1 class2' attr='değer'>
```

Stilin önceliği ise aşağıdaki gibidir. Unutmayın, öncelik **her bir özellik için ayrı ayrı geçerlidir**, tüm blok için geçerli değildir.

* `E` `!important` kelimesi yüzünden en yüksek önceliğe sahiptir. Kullanımından kaçınmanız önerilir.
* `F` satıriçi stil olduğu için bir sonraki önceliğe sahiptir.
* `A` bir sonraki önceliğe sahiptir. Çünkü her şeyden daha "özgüdür". 3 belirteci vardır: `p` ögesinin adı, sınıf` class1`, bir öznitelik `attr = 'değer'.
* `C`, `B` ile aynı özdeşliğe sahip olsa da, bundan sonra geldiğinden dolayı öncelik hakkına sahiptir.
* `B` bir sonraki önceliğe sahiptir.
* Sonuncu önceliğe sahip olan`D`'dir.

## Medya Sorguları

CSS Medya Sorguları, CSS 3'te belirli CSS kurallarının ne zaman uygulanması gerektiğini (örneğin basılan zaman veya belirli boyutlar veya piksel yoğunluğu olan bir ekranda olduğunda) belirlemenize izin veren bir özelliktir. Medya Sorguları, seçicilere önceliğk eklemez.

```css
/* Tüm cihazlarda kullanılacak olan bir kural */
h1 {
  font-size: 2em;
  color: white;
  background-color: black;
}

/* h1 ögesini değiştirip bir yazıcıda  daha az mürekkep kullanın*/
@media print {
  h1 {
    color: black;
    background-color: white;
  }
}

/* En az 480 piksel genişliğinde bir ekran gösterildiğinde font yüksekliğini daha büyük yap */
@media screen and (min-width: 480px) {
  h1 {
    font-size: 3em;
    font-weight: normal;
  }
}
```

Medya sorguları aşağıdaki bu özellikleri içerebilir:
`width`, `height`, `device-width`, `device-height`, `orientation`, `aspect-ratio`, `device-aspect-ratio`, `color`, `color-index`, `monochrome`, `resolution`, `scan`, `grid`. Bu özelliklerin birçoğunu `min-` veya `max-` öneki ile kullanabilirsiniz. 

`resolution` özelliği eski cihazlarda desteklenmediğinden ötürü `device-pixel-ratio` kullanın.

Eğer `viewport` meta etiketi sağlanmadıkça birçok akıllı telefon ve tabletler, sayfayı masaüstü bilgisayardaymış gibi göstermeye çalışacaktır.

```html
<head>
  <meta name="viewport" content="width=device-width; initial-scale=1.0">
</head>
```

## Uyumluluk

CSS 2'deki çoğu özellik (ve CSS 3'deki birçoğu) bütün tarayıcılar ve cihazlar için bulunmaktadır. Ancak yeni bir özelliği kullanmadan önce kontrol etmek her zaman iyi bir uygulamadır.

## Kaynaklar

* [CanIUse](http://caniuse.com) (Detaylı uyumluluk bilgileri)
* [Dabblet](http://dabblet.com/) (CSS oyun alanı)
* [Mozilla Geliştirici Ağının CSS belgelendirmesi](https://developer.mozilla.org/en-US/docs/Web/CSS) (Eğitseller ve referanslar)
* [Codrops' CSS Referansı](http://tympanus.net/codrops/css_reference/) (Referans)

## Daha Fazla Okuma

* [Understanding Style Precedence in CSS: Specificity, Inheritance, and the Cascade](http://www.vanseodesign.com/css/css-specificity-inheritance-cascaade/)
* [Selecting elements using attributes](https://css-tricks.com/almanac/selectors/a/attribute/)
* [QuirksMode CSS](http://www.quirksmode.org/css/)
* [Z-Index - The stacking context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Understanding_z_index/The_stacking_context)
* [SASS](http://sass-lang.com/) and [LESS](http://lesscss.org/) for CSS pre-processing
* [CSS-Tricks](https://css-tricks.com)
