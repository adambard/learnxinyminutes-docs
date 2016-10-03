---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
  - ["Xavier Yao", "http://github.com/xavieryao"]
filename: coffeescript-id.coffee
translators:
  - ["Rizky Luthfianto", "http://github.com/rilut"]
lang: id-id
---

CoffeeScript adalah bahasa sederhana yang diterjemahkan saat kompilasi ke dalam JavaScript,
dan bukan diterjemahkan pada saat *runtime*.  
CoffeeScript mencoba agar kode JavaScript yang dihasilkan tetap mudah dibaca
dan kompatibel dengan semua *runtime* JavaScript.

Lihat juga [website CoffeeScript](http://coffeescript.org/) yang memiliki tutorial lengkap tentang CoffeeScript.

```CoffeeScript
# CoffeeScript adalah bahasa hipster.
# Mengikuti tren bahasa modern lainnya.
# Sehingga, seperti Ruby dan Python, untuk komentar digunakan tanda pagar.

###
Ini adalah contoh blok komentar, yang nanti diterjemahkan langsung ke '/ *' dan '* /'
pada kode JavaScript yang dihasilkan.

Anda diharapkan sedikit memahami semantik JavaScript sebelum melanjutkan tutorial ini.
###

# Pengisian nilai variabel:
angka = 42 #=> var angka = 42;
kebalikan = true #=> var kebalikan = true;

# Kondisi:
angka = -42 if kebalikan #=> if(kebalikan) { angka = -42; }

# Fungsi:
kuadrat = (x) -> x * x #=> var kuadrat = function(x) { return x * x; }

isi = (wadah, cairan = "kopi") ->
  "Mengisi #{wadah} dengan #{cairan}..."
#=>var isi;
#
#isi = function(wadah, cairan) {
#  if (cairan == null) {
#    cairan = "kopi";
#  }
#  return "Mengisi " + wadah + " dengan " + cairan + "...";
#};

# Rentang:
list = [1..5] # => var list = [1, 2, 3, 4, 5];

# Objek:
fungsi_matematika =
  akar:   Math.sqrt
  kuadrat: kuadrat
  kubik:   (x) -> x * kuadrat x
#=> var fungsi_matematika = {
#    "akar": Math.sqrt,
#    "kuadrat": kuadrat,
#    "kubik": function(x) { return x * kuadrat(x); }
#   };

# *Splat*:
balapan = (pemenang, pelari...) ->
  print pemenang, pelari
#=>balapan = function() {
#    var pelari, pemenang;
#    pemenang = arguments[0], pelari = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#    return print(pemenang, pelari);
#  };

# Cek keberadaan:
alert "Elvis ada!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("Elvis ada!"); }

# Komprehensi *array*:
kubik_kubik = (fungsi_matematika.kubik angka for angka in list)
#=>kubik_kubik = (function() {
#     var _i, _len, _hasil;
#     _hasil = [];
#     for (_i = 0, _len = list.length; _i < _len; _i++) {
#         angka = list[_i];
#         _hasil.push(fungsi_matematika.kubik(angka));
#     }
#     return _hasil;
#})();

sayur_sayuran = ['brokoli', 'bayam', 'kemangi']
makan sayuran for sayuran in sayur_sayuran when sayuran isnt 'kemangi'
#=>sayur_sayuran = ['brokoli', 'bayam', 'kemangi'];
#
#for (_k = 0, _len2 = sayur_sayuran.length; _k < _len2; _k++) {
#  sayuran = sayur_sayuran[_k];
#  if (sayuran !== 'kemangi') {
#    makan(sayuran);
#  }
#}
```

## Referensi Tambahan

- [Smooth CoffeeScript (EN)] (http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto (EN)] (https://leanpub.com/coffeescript-ristretto/read)
