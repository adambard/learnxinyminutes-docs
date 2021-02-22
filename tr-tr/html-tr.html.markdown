---
language: html
filename: learnhtml-tr.txt
contributors:
    - ["Christophe THOMAS", "https://github.com/WinChris"]
translators:
    - ["Kemal MUTLU", "https://github.com/kemtake"]
    - ["Nuri Akman", "https://github.com/vedia"]
lang: tr-tr
---

HTML, HyperText Markup Language (Hiper Metin İşaretleme Dili) anlamına gelir.

Web sayfaları yazmamızı sağlayan bir dildir. Bu işaretleme dili, metin ve verilerin nasıl gösterilmesi gerektiği kodlanarak web sayfaları yazmamızı sağlar. Aslında, html dosyaları basit metin dosyalarıdır.

Bu işaretleme nedir? Sayfanın verilerini, açılış etiketleri ve kapanış etiketleri ile çevreleyerek düzenleme yöntemidir. Bu işaretleme, içerdiği metne anlam vermeyi sağlar. Diğer bilgisayar dillerinde olduğu gibi, HTML’nin birçok sürümü vardır. Burada HTML5 hakkında konuşacağız. 

**NOT :** Etkilerin nasıl çalıştıklarını anlamak, çıktılarını görebilmek için [codepen](https://codepen.io/) gibi bir siteden de faydalanabilirsiniz. Bu makale temel olarak HTML sözdizimi ve bazı yararlı ipuçlarıyla ilgilidir.

```html
<!-- Yorumlar bu satır gibi eklenir! -->

<!--
	Yorumlar
    birden
	fazla
	satıra
	yayılabilir!
-->

<!-- #################### Başlık #################### -->

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
			<p>Bu bir paragraftır.</p>
			<p>Bu başka bir paragraf.</p>
			<ul>
				<li>Bu, numaralandırılmamış bir listede bulunan bir öğe/maddedir (madde imi)</li>
				<li>Bu başka bir öğe</li>
				<li>Ve bu listedeki son öğe </li>
			</ul>
		</body>
	</html>

<!--
Bir HTML dosyası, tarayıcıya her zaman sayfanın HTML olduğunu belirterek başlar.
-->
<!doctype html>

<!-- Bundan sonra, bir <html> etiketi açılarak başlar. -->
<html>

<!-- dosyanın sonu </html> etiketi ile kapatılır.  -->
</html>

<!-- Sayfada, bu son etiketten sonra hiçbir şey bulunmamalıdır. -->

<!-- Açılış ve kapanış etiketleri arasında (<html> </html>) şunları bulunur: -->

<!-- <head> ile tanımlanan bir sayfa başlığu (bu, </head> ile kapatılmalıdır). -->
<!-- Baslik, gösterilmeyen bazi aciklamalar ve ek bilgiler icerir; buna üstveri denir. -->

<head>
	<!-- <title> etiketi, tarayıcıda gösterilecek başlığı gösterir. Pencerenin başlık çubuğu ve sekme adı.-->
	<title>Benim Sitem</title>
</head>

<!-- <head> bölümünden sonra, <body> etiketi gelir. -->
<!-- Bu noktaya kadar, tarif edilen hiçbir şey tarayıcı penceresinde görünmez. -->
<!-- <body> etiketinden sonra  görüntülenecek içeriğe yer verilir. -->

<body>
	<!-- h1 etiketi bir başlık oluşturur. -->
	<h1>Merhaba Dünya!</h1>
	<!--
		Ayrıca başlıklar <h1> etiketinden  <h6> etiketine kadar gidebilir.
		<h1> etiketi en önemli, <h6> etiketi en düşük öncelikli başlığı yazmamızı sağlar.
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
          Numaralandırılmış bir listeye sahip olmak için <ol> etiketi de kullanılabilir. Bu durumda 1. madde 2. madde vb. şekilde gider.
	-->
	<ul>
		<li>Bu, numaralandırılmamış bir bir liste kalemidir (madde imi)</li>
		<li>Bu başka bir öğe</li>
		<li>Ve bu listedeki son öğe</li>
	</ul>
</body>

<!-- İşte bir html dosyası oluşturmak bu kadar basit. -->

<!-- Ancak birçok farklı HTML etiketi türü de eklenebilir. -->

<!-- <img /> etiketi bir resim eklemek için kullanılır. -->
<!--
Resmin kaynağı, src = "" özniteliği kullanılarak belirtilir.
Kaynak, bir URL veya bilgisayarınızdaki bir dosyanın yolu olabilir.
-->
<img src="http://i.imgur.com/XWG0O.gif"/>

<!-- HTML'de bir Tablo oluşturmak da mümkündür. -->

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
		<td>ilk satırın, ilk hücresi (sutunu)</td>
		<td>ilk satırın, ikinci hücresi (sutunu)</td>
	</tr>

	<tr>
		<td>ikinci satırın, ilk hücresi (sutunu) </td>
		<td>ikinci satırın, ikinci hücresi (sutunu)</td>
	</tr>
</table>

```

## Kullanım

HTML içeriği, .html veya .htm ile biten dosyalara yazılır. Mim türü text/html .
HTML olarak yazılmış dosyalar,  `.html` veya `.htm` dosya uzantısına sahiptirler. mime type'ı ise `text/html` dir.

## Daha fazla bilgi için

* [wikipedia](https://en.wikipedia.org/wiki/HTML)
* [HTML tutorial](https://developer.mozilla.org/en-US/docs/Web/HTML)
* [W3School](http://www.w3schools.com/html/html_intro.asp)
