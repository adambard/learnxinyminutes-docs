---
status: not-yet-finished!
category: tool
tool: jquery
contributors:
    - ["Seçkin KÜKRER", "https://github.com/leavenha"]
filename: jquery.js
---

# TODO

[ ] İngilizce alanları temizle.
[ ] Verdiğin dış-bağlantıları kontrol et.
[ ] Yazım hataları için tekrar oku.
[ ] Yayınla!

# Tanım

jQuery, (IPA: ˈd͡ʒeɪˌkwɪəɹiː).
j + Query, olarak isimlendirilmiş, çünkü çoğunlukla HTML elementlerini sorgulamak ve onları manipüle etmek için kullanılır.

jQuery, 2006 yılında geliştirilmiş ve günümüzde halen kullanımı yaygın, görece en popüler çapraz-platform JavaScript kütüphanelerinden birisidir. Şimdilerde jQuery ekibi tarafından gelişimi devam etmektedir. Dünyanın bir çok yerinden büyük şirketler ve ve bağımsız yazılım ekipleri tarafından kullanılmaktadır.

Genel kullanımı animasyonlardır; Galeri, Ek menü, sayfa geçişleri, ve diğer tüm gerçeklemelere sağladığı kolaylıkla birlikte Flash'ın alternatifi olarak yorumlanabilir. Fakat [AJAX] işlemleri de dahil olmak üzere, olay-yönetimi döküman manipülasyonu ve bir çok programlama görevini kolaylaştırır.

Resmi sitesinden [jQuery] [dosyasını] indirip web sitenize yükleyebilirsiniz. jQuery günümüz JavaScript kütüphaneleri gibi küçültülmüş boyutlarda bulut-dağıtıcı sistemler sayesinde bağımsız olarak da sitenize eklenebilir.

Kütüphanenin kullanımı ile, jQueryUI gibi ek paketlerle gelişmiş ve modern arayüzler gerçekleyebilirsiniz.

Fakat, jQuery'ye giriş yapmadan önce elbetteki bu kütüphanenin üzerine kurulduğu teknoloji olan [JavaScript'i öğrenmelisiniz].

```js

// Bu belgedeki değişken isimleri Türkçe, ve [CamelCase] notasyonu uygulamaktadır.
// Bu belgedeki kod parçalarının çıktıları, onları uyguladığınız dökümanın içeriğine bağlı olarak değişmektedir.

// *. Konsept
jQuery DOM nesnelerini seçmek için inovatif bir yol sunar.
// `$` değişkeni, `jQuery` kütüphanesine işaret eder.
// Fonksiyon notasyonu ile DOM nesnelerini elde eder ve üzerinde işlemler gerçekleştirirsiniz.
$(window)
// => jQuery [Window] (1)
// Bize ilgili HTML dökümanındaki window nesnesini verir.

// 1. Seçiciler
// Tüm nesneleri seçmek için `*` çağırımı yapılır.
var hepsi = $('*');
// => jQuery [<html class="js multiplebgs boxshadow, <head>, <meta>,
// .... <meta>, <title>, <meta>, <meta>,
// .... <meta>, <link>, <link>, …] (1134) = $1

// Seçiciler, jQuery'de bir nesne seçmek için kullanılırlar.
var sayfa = $(window);
// => sayfa, açık döküman nesnesini seçer.

var tumParagraflar = $('p');
// => jQuery [<p>, <p>, <p>] (3)

// Seçiciler aynı zamanda CSS seçicileri olabilir.
var mavi = $('.mavi');
// => jQuery [<p class="mavi"] (1)

// Aynı zamanda birlikte kullanılabilirler.
var maviParagraf = $('p.mavi');
// => jQuery [<p class="mavi">] (1)

// Özellik seçicileri de mevcuttur, HTML nesnesinin özelliği için seçim yaparlar.
var isimSecicisi = $("input[name*='kayit.form']");
// => jQuery [<input name="kayit.form.sifre">, <input name="kayit.form.dogumtarihi"> ...] (10)
// Diğer özellik seçiciler;
/*
- Özelliğin içinde arayan; *=
- Özelliğin içinde verilen kelimeleri arayan; ~=
  |-(kelimeler boşlukla ayrılmalı, *='den farkına dikkat ediniz.)
- Özelliğin başlangıç verisini arayan; ^=
- Özelliğin bitiş verisini arayan; $=
- Özelliği tamamen karşılaştıran; =
- Özelliğin eşitsizlik durumunu karşılaştıran; !=

Diğer tüm seçiciler için resmi siteyi kontrol ediniz.
*/


```

## Notes

Yaygın bir yanlış bilineni düzeltmek adına; jQuery bir çalışma-çatısı değil, bir kütüphanedir^1.

## Further Reading
