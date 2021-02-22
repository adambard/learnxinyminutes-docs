---
language: Dynamic Programming
filename: dynamic-tr.txt
contributors:
    - ["Akashdeep Goel", "https://github.com/akashdeepgoel"]
translators:
    - ["Mehmet Cem Yaraş", "https://www.linkedin.com/in/yarascem/"]
lang: tr-tr
---

# Dinamik Programlama

## Giriş

Dinamik Programlama, göreceğimiz gibi belirli bir problem sınıfını çözmek için kullanılan güçlü bir tekniktir. Fikir çok basittir, verilen girdiyle ilgili bir sorunu çözdüyseniz, aynı sorunun tekrar çözülmesini önlemek için sonucunu gelecekte referans olarak kaydedilmesine dayanır.

Her zaman hatırla! "Geçmiş hatırlayamayanlar, aynı şeyleri tekrar yaşamaya mahkumlardır!"

## Bu tür sorunların çözüm yolları

1. Yukarıdan aşağıya:
Verilen problemi çözerek çözmeye başlayın. Sorunun zaten çözüldüğünü görürseniz, kaydedilen cevabı döndürmeniz yeterlidir. Çözülmemişse, çözünüz ve cevabı saklayınız. Bu genellikle düşünmek kolaydır ve çok sezgiseldir. Buna Ezberleştirme denir.

2. Aşağıdan yukarıya:
Sorunu analiz edin ve alt problemlerin çözülme sırasını görün ve önemsiz alt sorundan verilen soruna doğru başlayın. Bu süreçte, problemi çözmeden önce alt problemlerin çözülmesi gerekmektedir. Buna Dinamik Programlama denir.

## Örnek

En Uzun Artan Subsequence problemi belirli bir dizinin en uzun artan alt dizini bulmaktır. `S = {a1, a2, a3, a4, ............., an-1}` dizisi göz önüne alındığında, en uzun bir alt kümeyi bulmak zorundayız, böylece tüm j ve i, `j<I` için , `aj<ai` alt kümesinde. Her şeyden önce, en son alt dizgenin (LSi) değerini dizinin son elemanı olan ai'nin her indeksinde bulmalıyız. Daha sonra en büyük LSi, verilen dizideki en uzun alt dizin olacaktır. Başlamak için, ai, dizinin elemanı olduğundan (Son öğe) LSi atanır. Sonra tüm j için `j<i` ve `aj<ai` gibi, En Büyük LSj'yi buluruz ve LSi'ye ekleriz. Sonra algoritma `O(n2)` zaman alır.

En uzun artan alt dizinin uzunluğunu bulmak için sözde kod:
Bu algoritmaların karmaşıklığı dizi yerine daha iyi veri yapısı kullanılarak azaltılabilir. Büyük dizin ve dizin gibi selefi dizi ve değişkeni saklama çok zaman kazandıracaktır.

Yönlendirilmiş asiklik grafiğinde en uzun yolu bulmak için benzer bir kavram uygulanabilir.

```python
for i=0 to n-1
    LS[i]=1
    for j=0 to i-1
        if (a[i] >  a[j] and LS[i]<LS[j])
            LS[i] = LS[j]+1
for i=0 to n-1
    if (largest < LS[i])
    
```  

### Bazı Ünlü Dinamik Programlama Problemleri

- Floyd Warshall Algorithm - Tutorial and C Program source code: [http://www.thelearningpoint.net/computer-science/algorithms-all-to-all-shortest-paths-in-graphs---floyd-warshall-algorithm-with-c-program-source-code]()
- Integer Knapsack Problem - Tutorial and C Program source code: [http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---the-integer-knapsack-problem]()
- Longest Common Subsequence - Tutorial and C Program source code : [http://www.thelearningpoint.net/computer-science/algorithms-dynamic-programming---longest-common-subsequence]()

## Online Kaynaklar

- [codechef](https://www.codechef.com/wiki/tutorial-dynamic-programming)
