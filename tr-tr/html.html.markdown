---
language: html
filename: learnhtml.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Kemal MUTLU", "https://github.com/kemtake"]
---

HTML, HyperText Markup Language(Hiper Metin İşaretleme Dili) anlamına gelir.

HTML dünya çapında web için sayfalar yazmamızı sağlayan bir dildir.
It is a markup language, it enables us to write webpages using code to indicate
how text and data should be displayed.  In fact, html files are simple text
files.

Bu işaretleme nedir? Sayfanın verilerini, açılış etiketleri ve kapanış etiketleri ile çevreleyerek düzenleme yöntemidir. Bu işaretleme, içerdiği metne önem vermeyi sağlar. Diğer bilgisayar dillerinde olduğu gibi, HTML’nin birçok sürümü vardır. Burada HTML5 hakkında konuşacağız.Bu işaret nedir? Sayfanın verilerini, açılış etiketleri ve kapanış etiketleri ile çevreleyerek düzenleme yöntemidir. Bu işaretleme, içerdiği metne önem vermeyi sağlar. Diğer bilgisayar dillerinde olduğu gibi, HTML’nin birçok sürümü vardır. Burada HTML5 hakkında konuşacağız.

**NOT :** Etkilerini görmek, nasıl çalıştıklarını anlamak ve dil ile ilgili bilgi edinmek için kodeken gibi bir sitedeki öğretici boyunca ilerledikçe farklı etiketleri ve öğeleri test edebilirsiniz. Bu makale temel olarak HTML sözdizimi ve bazı yararlı ipuçlarıyla ilgilidir.

```html
<!-- Yorumlar bu çizgi gibi eklenir! -->

<!--
	Yorumlar
  birden
	fazla
	satıra
	yayılabilir!
-->

<!-- #################### Etiketler #################### -->

<!-- İşte, analiz edeceğimiz örnek bir HTML dosyası. -->


<!doctype html>
	<html>
		<head>
			<title>Benim Sitem</title>
		</head>
		<body>
			<h1>Merhaba dünya!</h1>
			<a href="http://codepen.io/anon/pen/xwjLbZ">
				Bunun ne olduğuna bir bak.
			</a>
			<p> Bu bir paragraftır.</p>
			<p> Bu başka bir paragraf. </p>
			<ul>
				<li>Bu, numaralandırılmamış bir listede bulunan bir maddedir (madde listesi)</li>
				<li>Bu başka bir öğe</li>
				<li>Ve bu listedeki son öğe </li>
			</ul>
		</body>
	</html>

<!--
Bir HTML dosyası , tarayıcıya her zaman sayfanın HTML olduğunu belirterek başlar.
-->
<!doctype html>

<!-- Bundan sonra, bir <html> etiketi açılarak başlar. -->
<html>

<!-- dosyanın sonu </html> etiketi ile kapatılır.  -->
</html>

<!--  Bu son etiketten sonra hiçbir şey görünmemelidir. -->

<!--Açılış ve kapanış etiketleri arasında (<html> </html>) şunları buluruz: -->

<!-- <head> tarafından tanımlanan bir başlık (bu, </head> ile kapatılmalıdır). -->
<!--
Baslik, gösterilmeyen bazi aciklamalar ve ek bilgiler icerir; bu üstveridir.
-->

<head>
	<!--
  <title> etiketi, tarayıcıya tarayıcıda gösterilecek başlığı gösterir
pencerenin başlık çubuğu ve sekme adı.
	-->
	<title>Benim Sitem</title>
</head>

<!-- <head> bölümünden sonra, <body> etiketi gelir. -->
<!-- Bu noktaya kadar, tarif edilen hiçbir şey tarayıcı penceresinde görünmeyecektir. -->
<!-- <body> etiketinden sonra  görüntülenecek içerikle doldurulmalı. -->

<body>
	<!-- h1 etiketi bir başlık oluşturur. -->
	<h1>Merhaba Dünya!</h1>
	<!--
		Ayrıca başlıklar <h1> etiketinden  <h6> etiketine kadar gidebilir.<h1> etiketi
    en büyük <h6> etiketi en küçük  başlığı yazmamızı sağlar.
	-->

	<!-- href="" özniteliğine verilen URL'ye bir köprü oluşturur.  -->
	<a href="http://codepen.io/anon/pen/xwjLbZ">
		Bunun ne olduğuna bir bak.
	</a>

	<!--  <p> etiketi, html sayfasına metin eklememize izin verir. -->
	<p>Bu bir paragraftır.</p>
	<p>Bu başka bir paragraf.</p>

	<!-- <ul> etiketi bir madde imi listesi oluşturur. -->
	<!--
  Numaralandırılmış bir listeye sahip olmak için <ol> etiketini kullanabiliriz.
  1. madde 2. madde vb. şekilde gider.
	-->
	<ul>
		<li>Bu, numaralandırılmamış bir listede bulunan bir maddedir (madde listesi)</li>
		<li> Bu başka bir öğe</li>
		<li>Ve bu listedeki son öğe</li>
	</ul>
</body>

<!-- İşte bir html dosyası oluşturmak bu kadar basit. -->

<!-- Ancak birçok farklı HTML etiketi türü de eklenebilir. -->

<!-- <img /> etiketi bir resim eklemek için kullanılır. -->
<!--
Resmin kaynağı src = "" özniteliği kullanılarak belirtilir.
Kaynak, bir URL veya bilgisayarınızdaki bir dosyanın yolu olabilir.
-->
<img src="http://i.imgur.com/XWG0O.gif"/>

<!-- Tablo oluşturmak da mümkündür. -->

<!--  Bir <table> elemanı açarız. -->
<table>

	<!-- <tr> bir satır oluşturmamızı sağlar. -->
	<tr>

		<!-- <th> tablo sütununa bir başlık vermemize izin verir. -->
		<th>Birinci Başlık</th>
		<th>İkinci Başlık</th>
	</tr>

	<tr>

		<!-- <td> bir tablo hücresi oluşturmamızı sağlar. -->
		<td>ilk satır, ilk sütun</td>
		<td>ilk satır, ikinci sütun</td>
	</tr>

	<tr>
		<td>ikinci satır, ilk sütun </td>
		<td>ikinci satır, ikinci sütun</td>
	</tr>
</table>

```

## Kullanım

HTML, .html veya .htm ile biten dosyalara yazılır. Mim türü text/html .

## Daha fazla öğrenmek için

* [wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
