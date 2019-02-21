---
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

///////////////////////////////////
// 2. Events and Effects
// jQuery is very good at handling what happens when an event is triggered
// A very common event used is the ready event on the document
// You can use the 'ready' method to wait until the element has finished loading
$(document).ready(function(){
  // Code won't execute until the document is loaded
});
// You can also use defined functions
function onAction() {
  // This is executed when the event is triggered
}
$('#btn').click(onAction); // Invokes onAction on click

// Some other common events are:
$('#btn').dblclick(onAction); // Double click
$('#btn').hover(onAction); // Hovering over
$('#btn').focus(onAction); // On focus
$('#btn').blur(onAction); // Losses focus
$('#btn').submit(onAction); // On submit
$('#btn').select(onAction); // When an element is selected
$('#btn').keydown(onAction); // When a key is pushed down
$('#btn').keyup(onAction); // When a key is released
$('#btn').keypress(onAction); // When a key is pressed
$('#btn').mousemove(onAction); // When the mouse is moved
$('#btn').mouseenter(onAction); // Mouse enters the element
$('#btn').mouseleave(onAction); // Mouse leaves the element


// These can all also trigger the event instead of handling it
// by simply not giving any parameters
$('#btn').dblclick(); // Fires double click on the element

// You can handle multiple events while only using the selector once
$('#btn').on(
  {dblclick: myFunction1} // Triggered on double click
  {blur: myFunction1} // Triggered on blur
);

// You can move and hide elements with some effect methods
$('.table').hide(); // Hides the element(s)

// Note: calling a function in these methods will still hide the element
$('.table').hide(function(){
    // Element hidden then function executed
});

// You can store selectors in variables
var tables = $('.table');

// Some basic document manipulation methods are:
tables.hide(); // Hides element(s)
tables.show(); // Shows (un-hides) element(s)
tables.toggle(); // Changes the hide/show state
tables.fadeOut(); // Fades out
tables.fadeIn(); // Fades in
tables.fadeToggle(); // Fades in or out
tables.fadeTo(0.5); // Fades to an opacity (between 0 and 1)
tables.slideUp(); // Slides up
tables.slideDown(); // Slides down
tables.slideToggle(); // Slides up or down

// All of the above take a speed (milliseconds) and callback function
tables.hide(1000, myFunction); // 1 second hide animation then function

// fadeTo has a required opacity as its second parameter
tables.fadeTo(2000, 0.1, myFunction); // 2 sec. fade to 0.1 opacity then function

// You can get slightly more advanced with the animate method
tables.animate({margin-top:"+=50", height: "100px"}, 500, myFunction);
// The animate method takes an object of css and values to end with,
// optional options parameter to tune the animation,
// and of course the callback function

///////////////////////////////////
// 3. Manipulation

// These are similar to effects but can do more
$('div').addClass('taming-slim-20'); // Adds class taming-slim-20 to all div

// Common manipulation methods
$('p').append('Hello world'); // Adds to end of element
$('p').attr('class'); // Gets attribute
$('p').attr('class', 'content'); // Sets attribute
$('p').hasClass('taming-slim-20'); // Returns true if it has the class
$('p').height(); // Gets height of element or sets height


// For many manipulation methods, getting info on an element
// will ONLY get the first matching element
$('p').height(); // Gets only the first 'p' tag's height

// You can use each to loop through all the elements
var heights = [];
$('p').each(function() {
  heights.push($(this).height()); // Adds all 'p' tag heights to array
});


``**

## Notes

Yaygın bir yanlış bilineni düzeltmek adına; jQuery bir çalışma-çatısı değil, bir kütüphanedir^1.

## Further Reading

