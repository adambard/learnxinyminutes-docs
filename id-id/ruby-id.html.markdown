---
language: ruby
filename: learnruby-id.rb
contributors:
  - ["David Underwood", "http://theflyingdeveloper.com"]
  - ["Joel Walden", "http://joelwalden.net"]
  - ["Luke Holder", "http://twitter.com/lukeholder"]
  - ["Tristan Hume", "http://thume.ca/"]
  - ["Nick LaMuro", "https://github.com/NickLaMuro"]
  - ["Marcos Brizeno", "http://www.about.me/marcosbrizeno"]
  - ["Ariel Krakowski", "http://www.learneroo.com"]
  - ["Dzianis Dashkevich", "https://github.com/dskecse"]
  - ["Levi Bostian", "https://github.com/levibostian"]
  - ["Rahil Momin", "https://github.com/iamrahil"]
  - ["Gabriel Halley", "https://github.com/ghalley"]
  - ["Persa Zula", "http://persazula.com"]
  - ["Jake Faris", "https://github.com/farisj"]
translators:
  - ["Ukaza Perdana", "https://github.com/ukazap"]
lang: id-id
---

```ruby
# Ini adalah sebuah komentar

=begin
Ini adalah komentar multibaris
Tak seorang pun menggunakannya
Kamu juga tidak perlu
=end

# Pertama-tama dan yang terpenting: Semuanya adalah objek.

# Angka adalah objek

3.class #=> Fixnum

3.to_s #=> "3"


# Beberapa aritmetika dasar
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7
2**5 #=> 32
5 % 3 #=> 2

# Operator-operator bitwise
3 & 5 #=> 1
3 | 5 #=> 7
3 ^ 5 #=> 6

# Aritmetika tidak lain adalah pemanis sintaks (syntactic sugar)
# untuk memanggil sebuah metode pada suatu objek
1.+(3) #=> 4
10.* 5 #=> 50

# Nilai-nilai khusus adalah objek
nil # setara dengan "null" di bahasa-bahasa lain
true # kebenaran
false # ketidakbenaran

nil.class #=> NilClass
true.class #=> TrueClass
false.class #=> FalseClass

# Kesamaan
1 == 1 #=> true
2 == 1 #=> false

# Ketidaksamaan
1 != 1 #=> false
2 != 1 #=> true

# selain false itu sendiri, nil adalah nilai lain yang "salah"
!nil   #=> true
!false #=> true
!0     #=> false

# Perbandingan lain
1 < 10 #=> true
1 > 10 #=> false
2 <= 2 #=> true
2 >= 2 #=> true

# Operator pembanding yang dikombinasikan ("spaceship operator")
1 <=> 10 #=> -1
10 <=> 1 #=> 1
1 <=> 1 #=> 0

# Operator-operator logika
true && false #=> false
true || false #=> true
!true #=> false

# Terdapat versi-versi operator logika yang berbeda dengan lebih sedikit awalan.
# Mereka digunakan sebagai kendali alur untuk merangkai beberapa pernyataan
# hingga salah satunya mengembalikan (return) nilai true atau false.

# `lakukan_suatu_lainnya` hanya dipanggil jika `lakukan_sesuatu` berhasil.
lakukan_sesuatu() and lakukan_suatu_lainnya()
# `catat_error` hanya dipanggil jika `lakukan_sesuatu` gagal.
lakukan_sesuatu() or catat_error()


# String adalah objek

'Aku adalah string'.class #=> String
"Aku juga adalah string".class #=> String

wadah = 'menggunakan string interpolation'
"Aku bisa #{wadah} ketika memakai tanda kutip ganda"
#=> "Aku bisa menggunakan string interpolation ketika memakai tanda kutip ganda"

# Gunakan tanda kutip tunggal daripada tanda kutip ganda jika memungkinkan
# String bertanda kutip ganda melakukan kalkulasi tambahan di dalam

# Kombinasikan string, tapi tidak dengan angka
'halo ' + 'dunia'  #=> "halo dunia"
'halo ' + 3 #=> TypeError: can't convert Fixnum into String
'halo ' + 3.to_s #=> "halo 3"

# Kombinasikan string dengan operator
'halo ' * 3 #=> "halo halo halo "

# Membubuhkan ke string
'halo' << ' dunia' #=> "halo dunia"

# cetak ke output dan buat baris baru (newline) di akhir
puts "Aku mencetak!"
#=> Aku mencetak!
#=> nil

# cetak ke output tanpa baris baru
print "Aku mencetak!"
#=> Aku mencetak! => nil

# Variabel
x = 25 #=> 25
x #=> 25

# Catat bahwa pemberian nilai mengembalikan nilai yang diberikan
# Artinya kamu bisa melakukan pemberian nilai secara jamak:

x = y = 10 #=> 10
x #=> 10
y #=> 10

# Berdasarkan adat, gunakan gaya snake_case untuk menulis nama variabel
snake_case = true

# Gunakan nama variabel yang deskriptif
path_to_project_root = '/good/name/'
path = '/bad/name/'

# Simbol (adalah objek)
# Simbol adalah konstanta yang dapat didaur ulang yang tidak dapat diubah
# (immutable), secara internal diwakili oleh nilai integer. Seringkali 
# digunakan sebagai pengganti string untuk menyampaikan nilai yang mengandung
# makna spesifik secara efisien.

:menunggu.class #=> Symbol

status = :menunggu

status == :menunggu #=> true

status == 'menunggu' #=> false

status == :diterima #=> false

# Array

# Ini adalah sebuah array
array = [1, 2, 3, 4, 5] #=> [1, 2, 3, 4, 5]

# Array bisa menampung item dengan beragam tipe

[1, 'halo', false] #=> [1, "halo", false]

# Array bisa di-indeks-kan
# Dari depan
array[0] #=> 1
array.first #=> 1
array[12] #=> nil

# Sama dengan aritmetika, pengaksesan [var]
# hanyalah pemanis sintaks
# untuk memanggil metode [] pada suatu objek
array.[] 0 #=> 1
array.[] 12 #=> nil

# Dari belakang
array[-1] #=> 5
array.last #=> 5

# Dengan indeks awal dan panjang (jumlah item)
array[2, 3] #=> [3, 4, 5]

# Membalik sebuah Array
a=[1,2,3]
a.reverse! #=> [3,2,1]

# Atau menggunakan jangkauan (range)
array[1..3] #=> [2, 3, 4]

# Tambahkan ke array seperti ini
array << 6 #=> [1, 2, 3, 4, 5, 6]
# Atau seperti ini
array.push(6) #=> [1, 2, 3, 4, 5, 6]

# Periksa apakah suatu item ada dalam sebuah array
array.include?(1) #=> true

# Hash adalah kamus utama Ruby berupa pasangan kunci/nilai (key/value pair).
# Hash ditandai dengan kurung kurawal:
hash = { 'warna' => 'hijau', 'angka' => 5 }

hash.keys #=> ['warna', 'angka']

# Nilai dalam Hash bisa diperoleh menggunakan kunci:
hash['warna'] #=> 'hijau'
hash['angka'] #=> 5

# Meminta hash untuk kunci yang tidak ada akan mengembalikan nil:
hash['tidak ada di sini'] #=> nil

# Sejak Ruby 1.9, ada sintaks khusus ketika menggunakan simbol sebagai kunci:

hash_baru = { defcon: 3, action: true }

hash_baru.keys #=> [:defcon, :action]

# Periksa ada/atau tidaknya kunci dan nilai dalam hash
hash_baru.key?(:defcon) #=> true
hash_baru.value?(3) #=> true

# Tip: Baik array maupun hash adalah Enumerable
# Mereka berbagi banyak metode yang berguna diantaranya each, map, count, dll.

# Struktur-struktur kendali

if true
  'pernyataan if'
elsif false
  'else if, opsional'
else
  'else, opsional juga'
end

for penghitung in 1..5
  puts "iterasi #{penghitung}"
end
#=> iterasi 1
#=> iterasi 2
#=> iterasi 3
#=> iterasi 4
#=> iterasi 5

# NAMUN, tidak ada orang yang menggunakan pengulangan for.
# Sebagai ganti, gunakan metode "each" dan memberinya sebuah blok (block).
# Blok adalah serangkaian kode yang bisa dimasukkan ke metode seperti "each".
# Ia serupa dengan lambda, fungsi anonim atau closure di bahasa lainnya.
#
# Metode "each" dari range menjalankan blok untuk setiap elemen dari range.
# Bloknya diberikan penghitung sebagai parameter.
# Memanggil metode "each" dengan blok terlihat seperti ini:

(1..5).each do |penghitung|
  puts "iterasi #{penghitung}"
end
#=> iterasi 1
#=> iterasi 2
#=> iterasi 3
#=> iterasi 4
#=> iterasi 5

# Kamu juga bisa mengurung blok dalam kurung kurawal:
(1..5).each { |penghitung| puts "iterasi #{penghitung}" }

# Isi dari struktur-struktur data juga bisa di-iterasi menggunakan each.
array.each do |elemen|
  puts "#{elemen} adalah bagian dari array"
end
hash.each do |kunci, nilai|
  puts "#{kunci} adalah #{nilai}"
end

# Jika kamu masih membutuhkan indeks, bisa menggunakan "each_with_index"
# dan definisikan variabel indeks
array.each_with_index do |elemen, indeks|
  puts "#{elemen} adalah nomor #{indeks} dalam array"
end

penghitung = 1
while penghitung <= 5 do
  puts "iterasi #{penghitung}"
  penghitung += 1
end
#=> iterasi 1
#=> iterasi 2
#=> iterasi 3
#=> iterasi 4
#=> iterasi 5

# Ada kumpulan fungsi pengulangan lainnya yang berguna di Ruby,
# contohnya "map", "reduce", "inject", daftarnya sangat panjang. Map,
# misalnya, mengambil array yang di-iterasi-nya, melakukan sesuatu pada
# setiap elemen sesuai definisi pada blok, dan mengembalikan array baru.
array = [1,2,3,4,5]
berganda = array.map do |elemen|
  elemen * 2
end
puts berganda
#=> [2,4,6,8,10]
puts array
#=> [1,2,3,4,5]

nilai = 'B'

case nilai
when 'A'
  puts 'Pertahankan, nak'
when 'B'
  puts 'Semoga lebih beruntung di lain waktu'
when 'C'
  puts 'Kamu bisa lebih baik'
when 'D'
  puts 'Susah payah'
when 'F'
  puts 'Kamu gagal!'
else
  puts 'Sistem penilaian lainnya, heh?'
end
#=> "Semoga lebih beruntung di lain waktu"

# case juga bisa menggunakan range
nilai = 82
case nilai
when 90..100
  puts 'Hore!'
when 80...90
  puts 'Cukup bagus'
else
  puts 'Kamu gagal!'
end
#=> "Cukup bagus"

# penanganan kesalahan (exception handling):
begin
  # kode di sini yang mungkin membangkitkan exception
  raise NoMemoryError, 'Kamu kehabisan memori.'
rescue NoMemoryError => variabel_exception
  puts 'NoMemoryError dibangkitkan', variabel_exception
rescue RuntimeError => variabel_exception_lainnya
  puts 'RuntimeError dibangkitkan sekarang'
else
  puts 'Ini dijalankan bila tidak ada exceptions sama sekali'
ensure
  puts 'Kode ini akan berjalan bagaimanapun juga'
end

# Fungsi (atau metode)

def gandakan(x)
  x * 2
end

# Fungsi dan semua blok secara tersirat mengembalikan nilai pernyataan terakhir
gandakan(2) #=> 4

# Tanda kurung bersifat optional, boleh ditiadakan jika tidak ambigu
gandakan 3 #=> 6

gandakan gandakan 3 #=> 12

def jumlah(x, y)
  x + y
end

# Argumen-argumen dari metode dipisahkan dengan koma
sum 3, 4 #=> 7

sum sum(3, 4), 5 #=> 12

# yield
# Semua metode secara tersirat mempunyai parameter blok opsional
# yang bisa dipanggil dengan kata kunci 'yield'

def kurung
  puts '{'
  yield
  puts '}'
end

kurung { puts 'halo dunia' }

# {
# halo dunia
# }


# Kamu bisa memasukkan blok ke sebuah fungsi
# "&" adalah penanda blok yang masuk
def tamu_tamu(&blok)
  blok.call 'beberapa_argumen'
end

# Kamu bisa memasukkan daftar argumen yang akan dikonversi menjadi array
# Itulah gunanya operator splat ("*")
def tamu_tamu(*array)
  array.each { |tamu| puts tamu }
end

# Bila metode mengembalikan array, bisa memberi nilai dengan destrukturisasi
# (destructuring assignment):
def makanan
    ['tempe penyet', 'sayur asam', 'nasi goreng']
end
sarapan, makan_siang, makan_malam = makanan
sarapan #=> 'tempe penyet'
makan_malam #=> 'nasi goreng'

# Menurut adat, nama metode yang mengembalikan boolean diakhiri tanda tanya
5.even? # false
5.odd? # true

# Dan jika suatu metode berakhiran tanda seru, ia melakukan sesuatu yang merusak
# seperti mengubah penerimanya. Banyak metode mempunyai versi ! untuk melakukan
# perubahan dan versi non-! untuk sekedar mengembalikan perubahannya
nama_perusahaan = "Putra Sejahtera"
nama_perusahaan.upcase #=> "PUTRA SEJAHTERA"
nama_perusahaan #=> "Putra Sejahtera"
nama_perusahaan.upcase! # kali ini kita benar-benar mengubah nama_perusahaan!
nama_perusahaan #=> "PUTRA SEJAHTERA"


# Definisikan kelas menggunakan kata kunci class
class Manusia

  # Variabel kelas. Ini dibagi oleh semua instans (instance) dari kelas ini.
  @@spesies = 'H. sapiens'

  # Inisialisasi dasar
  def initialize(nama, usia = 0)
    # Berikan argumen ke variabel instans "nama" dalam instans ini
    @nama = nama
    # Jika tidak diberi usia, nilai default dalam daftar argumen digunakan.
    @usia = usia
  end

  # Metode setter dasar
  def nama=(nama)
    @nama = nama
  end

  # Metode getter dasar
  def nama
    @nama
  end

  # Fungsi di atas bisa disingkat dengan metode attr_accessor sebagai berikut
  attr_accessor :nama

  # Metode getter/setter juga bisa dibuat secara terpisah seperti ini
  attr_reader :nama
  attr_writer :nama

  # Metode kelas menggunakan self untuk membedakannya dari metode instans.
  # Ia hanya bisa dipanggil pada kelas, bukan pada instans-nya.
  def self.katakan(pesan)
    puts pesan
  end

  def spesies
    @@spesies
  end
end


# Membuat instans kelas
jim = Manusia.new('Jim Halpert')

dwight = Manusia.new('Dwight K. Schrute')

# Mari panggil beberapa metode
jim.spesies #=> "H. sapiens"
jim.nama #=> "Jim Halpert"
jim.nama = "Jim Halpert II" #=> "Jim Halpert II"
jim.nama #=> "Jim Halpert II"
dwight.spesies #=> "H. sapiens"
dwight.nama #=> "Dwight K. Schrute"

# Panggil metode kelas
Manusia.katakan('Hai') #=> "Hai"

# Lingkup variabel didefinisikan berdasarkan bagaimana kita memberikannya nama
# Variabel yang berawalan $ memiliki lingkup global
$var = "Aku adalah variabel global"
defined? $var #=> "global-variable"

# Variabel yang berawalan @ memiliki lingkup instans
@var = "Aku adalah variabel instans"
defined? @var #=> "instance-variable"

# Variabel yang berawalan @@ memiliki lingkup kelas
@@var = "Aku adalah variabel kelas"
defined? @@var #=> "class variable"

# Variabel yang berawalan huruf kapital adalah konstanta
Var = "Aku adalah konstanta"
defined? Var #=> "constant"

# Kelas juga adalah objek sehingga kelas bisa memiliki variabel instans.
# Variabel kelas dibagi diantara kelas dan semua pewarisnya.

# kelas dasar
class Manusia
  @@foo = 0

  def self.foo
    @@foo
  end

  def self.foo=(nilai)
    @@foo = nilai
  end
end

# kelas turunan
class Buruh < Manusia
end

Manusia.foo # 0
Buruh.foo # 0

Manusia.foo = 2 # 2
Buruh.foo # 2

# Variabel instans milik kelas tidak dibagikan dengan pewaris kelas tersebut.

class Manusia
  @bar = 0

  def self.bar
    @bar
  end

  def self.bar=(nilai)
    @bar = nilai
  end
end

class Dokter < Manusia
end

Manusia.bar # 0
Dokter.bar # nil

module ContohModul
  def foo
    'foo'
  end
end

# Include modul mengikat metode-metodenya pada instans-instans kelas
# Extend modul mengikat metode-metodenya pada kelas

class Orang
  include ContohModul
end

class Buku
  extend ContohModul
end

Orang.foo     # => NoMethodError: undefined method `foo' for Orang:Class
Orang.new.foo # => 'foo'
Buku.foo       # => 'foo'
Buku.new.foo   # => NoMethodError: undefined method `foo'

# Callbacks dijalankan ketika meng-include dan meng-extend sebuah modul

module ContohUrusan
  def self.included(base)
    base.extend(MetodeKelas)
    base.send(:include, MetodeInstans)
  end

  module MetodeKelas
    def bar
      'bar'
    end
  end

  module MetodeInstans
    def qux
      'qux'
    end
  end
end

class Sesuatu
  include ContohUrusan
end

Sesuatu.bar     # => 'bar'
Sesuatu.qux     # => NoMethodError: undefined method `qux'
Sesuatu.new.bar # => NoMethodError: undefined method `bar'
Sesuatu.new.qux # => 'qux'
```

## Sumber tambahan

- [Learn Ruby by Example with Challenges](http://www.learneroo.com/modules/61/nodes/338) - Varian dari referensi ini dengan tantangan dalam browser.
- [An Interactive Tutorial for Ruby](https://rubymonk.com/) - Belajar Ruby melalui serangkaian tutorial interaktif.
- [Dokumentasi resmi](http://www.ruby-doc.org/core-2.1.1/)
- [Ruby from other languages](https://www.ruby-lang.org/en/documentation/ruby-from-other-languages/)
- [Programming Ruby](http://www.amazon.com/Programming-Ruby-1-9-2-0-Programmers/dp/1937785491/) - Edisi lama yang [gratis](http://ruby-doc.com/docs/ProgrammingRuby/) tersedia online.
- [Ruby Style Guide](https://github.com/bbatsov/ruby-style-guide) - Panduan penulisan kode Ruby oleh komunitas.
- [Try Ruby](http://tryruby.org) - Pelajari dasar bahasa pemrograman Ruby, secara interaktif di browser.
