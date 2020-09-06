---
category: tool
tool: jquery
contributors:
    - ["Seçkin KÜKRER", "https://github.com/leavenha"]
filename: jquery-tr-tr.js
lang: tr-tr
---

# Tanım

jQuery, (IPA: ˈd͡ʒeɪˌkwɪəɹiː).
j + Query, olarak isimlendirilmiş, çünkü çoğunlukla HTML elementlerini sorgulamak ve onları manipüle etmek için kullanılır.

jQuery, 2006 yılında geliştirilmiş ve günümüzde halen kullanımı yaygın, görece en popüler çapraz-platform JavaScript kütüphanelerinden birisidir. Şimdilerde jQuery ekibi tarafından gelişimi devam etmektedir. Dünyanın bir çok yerinden büyük şirketler ve bağımsız yazılım ekipleri tarafından kullanılmaktadır.

Genel kullanım amacı animasyonlardır; Galeri, ek menü, sayfa geçişleri, ve diğer tüm gerçeklemelere sağladığı kolaylıkla birlikte Flash'ın alternatifi olarak yorumlanabilir. [Ajax][ajax-wikipedia-page] işlemleri de dahil olmak üzere olay-yönetimi, döküman manipülasyonu ve bir çok programlama görevini kolaylaştırır.

Resmi sitesinden ([jQuery][jquery-official-website]) indirip web sitenize yükleyebilirsiniz. jQuery günümüz JavaScript kütüphaneleri gibi, küçültülmüş boyutlarda bulut tabanlı İçerik Dağıtım Ağı sistemleri sayesinde bağımsız olarak da sitenize eklenebilir.

Kütüphanenin kullanımı ile, jQueryUI gibi ek paketlerle gelişmiş ve modern arayüzler gerçekleyebilirsiniz.

Fakat, jQuery'ye giriş yapmadan önce elbetteki bu kütüphanenin üzerine kurulduğu teknoloji olan [JavaScript'i öğrenmelisiniz][javascript-learnxinyminutes-page].

```js

// Bu belgedeki değişken isimleri Türkçe,
// ve [Lower Camel Case] notasyonu uygulamaktadır.
// Bu belgedeki kod parçalarının çıktıları,
// onları uyguladığınız dökümanın içeriğine bağlı olarak değişmektedir.

// Döküman boyunca, aşağıdaki gösterimde
// Kod - Çıktı ikilisi ile içeriğin anlamlandırılması
// kolaylaştırılmaya çalışmıştır.
// ornek_kod_parcasi();
// => "ÖRNEK ÇIKTI"

// *. Konsept
// jQuery DOM nesnelerini seçmek için inovatif bir yol sunar.
// `$` değişkeni, `jQuery` kütüphanesine işaret eder.
// Fonksiyon notasyonu ile DOM nesnelerini elde eder
// ve üzerinde işlemler gerçekleştirirsiniz.
$(window)
// => jQuery [Window] (1)
// Bize tarayıcının belirlediği window nesnesini verir.

// 1. Seçiciler
// Tüm nesneleri seçmek için `*` çağırımı yapılır.
const hepsi = $('*');
// => jQuery [<html>, <head>, <meta>,
// .... <meta>, <title>, <meta>, <meta>,
// .... <meta>, <link>, <link>, …] (1134) = $1

// Seçiciler, jQuery'de bir nesne seçmek için kullanılırlar,
const sayfa = $(window);
// => jQuery [window] (1)
// Sayfa, açık döküman nesnesini seçer.

// Elementler, kendileri için seçicidirler.
const tumParagraflar = $('p');
// => jQuery [<p>, <p>, <p>] (3)

// Seçiciler aynı zamanda CSS seçicileri olabilir.
const mavi = $('.mavi');
// => jQuery [<p class='mavi'] (1)

// Aynı zamanda element ile birlikte kullanılabilirler.
const maviParagraf = $('p.mavi');
// => jQuery [<p class='mavi'>] (1)

// Özellik seçicileri de mevcuttur,
// Elementin özelliği için seçim yaparlar.
const isimSecicisi = $('input[name*="kayit.form"]');
// => jQuery [<input name='kayit.form.sifre'>,
//            <input name='kayit.form.dogumtarihi'> ...] (10)

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

// 2. Olaylar ve Efektler
// - Olaylar 
// jQuery kullanıcı ile tarayıcı arasındaki etkileşimi olaylar ile ele alır.

// En yaygın kullanımı tartışmasız ki Dökümanın Yüklenmesi olayıdır.

// $.ready fonksiyonu, argüman olarak aldığı fonksiyonu,
// seçilen eleman tamamen yüklendiğinde çağıracaktır.
$(document).ready(function(){
  // Dökümanın tamamı yüklendiğine göre, iş mantığımı çağırabiliriz.
  console.info('Döküman yüklendi!');
})
// => jQuery [#document] (1)

// Bir dökümanın tamamının yüklenmeden,
// herhangi bir iş mantığı çalıştırmanın
// neden kötü bir fikir olduğunu merak ediyorsanız,
// ileri okuma kısmına danışabilirsiniz.

// Önce Olay tanımlayalım.

// Tıklama olayı için `$.click` olay tetikleyicisi kullanılıyor.
$('.mavi').click(function(){
  // Unutmayın ki, önceden tanımlanmış
  // bir fonksiyonu da argüman olarak verebilirsiniz.
  console.info('Mavi butona tıkladın!');
})
// => jQuery [<button>, <button>, <button>, <button>, <button>, …] (365)

// Çift Tıklama olayı için `$.dblclick` olay tetikleyicisi kullanılıyor.
$('.mavi').dblclick(function(){
  console.info('Mavi butona çift tıkladın!');
})
// => jQuery [<button>, <button>, <button>, <button>, <button>, …] (365)

// Seçilen Elemente birden fazla tetiklenecek fonksiyon tanımalamak
// istersek, Olayları ve Fonksiyonları Anahtar-Değer yapısı sağlayan
// Objeleri kullanarak da çağırabiliriz.

// => tetiklenecekFonksiyon
$('.mor').on({
  click: () => console.info('Tek tıklama ile tetiklendim!'),
  dblclick: () => console.info('Çift tıklama ile tetiklendim!'),
  // ...
});
// => jQuery [<button>, <button>, <button>, <button>, <button>, …] (365)

// Diğer olay tetikleyicileri;
/*
Elemente,
- Fokus/Odaklanma; $.focus
- Fokus/Odaklanmanın kaybedilmesi; $.blur
- Farenin alanına girmesi; $.mouseenter
- Farenin alanından çıkması; $.mouseleave

Diğer tüm olay tetikleyicileri için resmi siteyi kontrol ediniz.
*/

// Tanımlanan olayları tetiklemek için,
// Kullanıcı-Tarayıcı etkileşimi yerine elle çağrı yapmak da mümkün.
// Tanımlama ile çağırım arasındaki fark sadece sağlanan argümanlardır.
// Argümansız çağırım, olayı tetikler.

// Tıklama olayını tetiklemek için.
$('.mavi').click();
// => Mavi butona tıkladın!
// => jQuery [<button>] (1) 

// Çift Tıklama olayını tetiklemek için.
$('.mavi').dblclick();
// => Mavi butona çift tıkladın!
// => jQuery [<button>] (1) 

// - Efektler
// jQuery bir çok ön-tanımlı efekt sunmakta.
// Bu efektler, belirli parametlerle, farklı iş mantıklarını
// gerçeklemenize izin verebilir.
// Önce parametresiz işlevlere göz atalım.

// Elementleri saklayabilir,
$('#slaytresmi').hide();
// => jQuery [<img id='slaytresmi'>] (1)

// Gizlenen elementleri tekrar görünür yapabilir,
$('#slaytresmi').show();
// => jQuery [<img id='slaytresmi'>] (1)

// Yada dilediğiniz CSS niteliğini anime edebilirsiniz,

// Bu parametre, anime etmek istediğiniz CSS özelliklerini
// belirleyen Obje bilgisidir.
// Yükseklik ve Genişlik bilgileri için değerler belirliyoruz.
const animeEdilecekCSSOzellikleri =
  {
    weight: "300px",
    height: "300px"
  };

// Diğer anime edilebilir CSS özellikleri;
/*
Elementin,
- Opaklık; opacity
- Dış çevre mesafesi; margin
- Çerçeve yüksekliği; borderWidth
- Satır yüksekliği; lineHeight

Diğer tüm özellikler için resmi siteyi kontrol ediniz.
*/

// Bu parametre animasyonun süresini belirler.
const milisaniyeCinsindenAnimasyonSuresi =
  1200;

// Bu parametre, 'linear' yada 'swing' metin
// bilgilerinden birini alır ve animasyonun
// akıcılığını belirler.
// x ∈ {'linear', 'swing'}
const animasyonAkiciligi = 'linear';

// Bu parametre, bir fonksiyondur ve
// animasyondan sonra çağırılır.
// Bir geri-çağırım (callback*) olarak değerlendirilebilir.
const animasyonGeriCagirimFonksiyonu = function(){
  console.info('Animasyon bitti!');
};

// Şimdi tanımlanan bilgilerimizle animasyonu çağırıyoruz.
$('#slaytresmi').animate(animeEdilecekCSSOzellikleri,
                         milisaniyeCinsindenAnimasyonSuresi,
                         animasyonAkiciligi,
                         animasyonGeriCagirimFonksiyonu);
// => jQuery [<img id='slaytresmi'>] (1)

// Kütüphane `$.animate` fonksiyonu için, anime edeceğiniz
// CSS özellikleri dışındaki tüm argümanlar için
// ön tanımlı değerler sağlamaktadır.
// Bu değerler için resmi siteyi kontrol ediniz.

// Diğer ön tanımlı efektler;
/*
Elementi,
- Yukarı kaydırır; $.slideUp
- Verilen saydamlık değerine anime eder; $.fadeTo
- Görünür yada görünmez yapar (geçerli durumuna bağlı); $.toggle

Diğer tüm efektler için resmi siteyi kontrol ediniz.
*/

// 3. Manipülasyon

// jQuery'de, HTML elementlerinin isteğiniz doğrultusunda
// değiştirilmesi için araçlar sunulmakta.

// Bir ön-tanımlı CSS sınıfımız olduğunu hayal edebilirsiniz.
// Bu sınıfı istediğimiz elemente uygulamak için,
$('#slaytresmi').addClass('inanilmaz-bir-cerceve-sinifi');
// => jQuery [<img id='slaytresmi' class='inanilmaz-bir-cerceve-sinifi'>] (1)

// Bu CSS sınıfını istediğimiz zaman silebiliriz,
$('#slaytresmi').removeClass('inanilmaz-bir-cerceve-sinifi');
// => jQuery [<img id='slaytresmi'>] (1)

// Bu HTML elementini, istediğimiz başka bir element ile çevreleyebiliriz,
$('#slaytresmi').wrap('<div class="farkli-bir-cerceve"></div>');
// => jQuery [<img id='slaytresmi'>] (1)
// Sonucun gözlemlenebilmesi için, elementin çevreleme işlemi sonrası
// döküman üzerindeki yapısını temel bir seçici ile gözlemleyebiliriz;
$('.farli-bir-cerceve')
// => jQuery [<div class='farkli-bir-cerceve>] (1)
// => <div class="farkli-bir-cerceve">
//      <img id='slaytresmi'>
//    </div>

// Elemente içerik ekleyebiliriz,
// Eklemeler döküman içeriğinin sonuna yapılacaktır.
// Bu süreci daha iyi gözlemleyebilmek için içeriğine bakmamız yeterli,
// Ekleme öncesinde;
$('.farkli-bir-cerceve');
// => jQuery [<div class='farkli-bir-cerceve>] (1)
// => <div class="farkli-bir-cerceve">
//      <img id='slaytresmi'>
//    </div>

// `$.append` fonksiyonu ile ekleme işlemini yapıyoruz.
$('.farkli-bir-cerceve').append('<h1>Bu çerçeve farklı!</h1>');
// => jQuery [<div class='farkli-bir-cerceve>] (1)
// => <div class="farkli-bir-cerceve">
//      <img id='slaytresmi'>
//      <h1>Bu çerçeve farklı!</h1>
//    </div>

// Dökümandan element silebiliriz,
$('.farkli-bir-cerceve > h1').remove();
// => jQuery [<h1>] (1)

// Dökümanın güncel halini görmek için seçiciyi çağırıyoruz,
$('.farkli-bir-cerceve');
// => jQuery [<div class='farkli-bir-cerceve>] (1**
// => <div class="farkli-bir-cerceve">
//      <img id='slaytresmi'>
//    </div>

// Elementlerin özniteliklerini değiştirebilir yada
// silebiliriz.
// Öznitelik erişici ve değiştiricisi,
// Bir fonksiyon notasyonuyla yapılanmış durumda.
// Eğer bir öznitelik bilgisini almak istiyorsak, ilgili öznitelik
// ismini;

$('.farkli-bir-cerceve > img').attr('id');
// => 'slaytresmi'

// Eğer bir öznitelik bilgisini güncellemek istiyorsak,
// ilgili öznitelik ismi ve sonrasında yeni değerini argüman
// olarak $.attr fonksiyonuna iletiyoruz;

$('.farkli-bir-cerceve > img').attr('id', 'cercevelislaytresmi');
// => jQuery [<img id='cercevelislaytresmi'>] (1)

// Diğer ön fonksiyonlar;
/*
Elementin,
- Yükseklik değeri, $.height
- HTML döküman içeriği, $.html
- Girdi içeriği, $.val
- Verilen CSS sınıfına sahipliği, $.hasClass

Diğer tüm manipülasyon fonksiyonları için resmi siteyi kontrol ediniz.
*/

```

## Notlar

- Yaygın bir yanlış bilineni düzeltmek adına; jQuery bir çalışma-çatısı değil, bir *kütüphanedir*.
- [Lower Camel Case][lower-camel-case-notasyonu] notasyonu için Wikipedia sayfası.

## İleri Okuma

### İngilizce

- [jQuery][jquery-official-website] resmi sitesi.

- [Jakob Jenkov | $(document).ready article](http://tutorials.jenkov.com/jquery/document-ready.html)

[jquery-official-website]: https://jquery.com
[ajax-wikipedia-page]: https://en.wikipedia.org/wiki/Ajax_(programming)
[javascript-learnxinyminutes-page]: https://learnxinyminutes.com/docs/javascript/
[lower-camel-case-notasyonu]: https://en.wikipedia.org/wiki/Camel_case#Programming_and_coding
