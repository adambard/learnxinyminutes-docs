---
language: PHP
contributors:
    - ["Malcolm Fell", "http://emarref.net/"]
    - ["Trismegiste", "https://github.com/Trismegiste"]
filename: learnphp-id.php
translators:
  - ["Ahmad Zafrullah", "https://github.com/23Pstars"]
lang: id-id
---

Dokumen ini menjelaskan tentang PHP5 keatas.

```php
<?php // Skrip PHP harus diawali dengan tag <?php

// Jika dokumen PHP hanya mengandung kode PHP, sebaiknya tidak menggunakan
// tag penutup PHP untuk menghindari ketidaksengajaan tampilnya sesuatu.

// Dua garis miring diawal digunakan untuk komentar satu baris.

/*
     Membatasi teks dalam garis miring-bintang dan bintang-garis miring
     membuat komentar untuk banyak-baris sekaligus.
*/

// Gunakan "echo" or "print" untuk menampilkan sesuatu
print('Halo '); // Menampilkan "Halo " tanpa baris baru

// () boleh tidak digunakan dalam menggunakan "print" dan "echo"
echo "Dunia\n"; // Menampilkan "Dunia" dengan baris baru
// (semua perintah harus diakhiri dengan titik koma)

// Apapun yang berada diluar tag <?php akan ditampilkan secara otomatis
?>
Halo Dunia, lagi!
<?php


/************************************
 * Tipe Data & Variabel
 */

// Variabel diawali dengan simnbol $.
// Nama variabel yang benar diawali dengan huruf atau garis-bawah,
// diikuti dengan beberapa huruf, angka, dan garis-bawah lainnya.

// Nilai Boolean adalah case-insensitive
$boolean = true;  // atau TRUE atau True
$boolean = false; // atau FALSE atau False

// Nilai Integer
$int1 = 12;   // => 12
$int2 = -12;  // => -12
$int3 = 012;  // => 10 (awalan 0 menandakan bilangan Oktal)
$int4 = 0x0F; // => 15 (awalan 0x menandakan bilangan Heksadesimal)
// Bilangan Biner Integer tersedia mulai dari PHP 5.4.0.
$int5 = 0b11111111; // 255 (awalan 0b menandakan bilangan Biner)

// Nilai Floats (dikenal juga sebagai Doubles)
$float = 1.234;
$float = 1.2e3;
$float = 7E-10;

// Menghapus variable
unset($int1);

// Aritmatika
$jumlah         = 1 + 1; // 2
$selisih        = 2 - 1; // 1
$perkalian      = 2 * 2; // 4
$pembagian      = 2 / 1; // 2

// Aritmatika singkat
$angka = 0;
$angka += 1;        // Menjumlahkan $angka dengan 1
echo $angka++;      // Menampilkan 1 (dijumlahkan dengan 1 setelah ditampilkan)
echo ++$angka;      // Menampilkan 3 (dijumlahkan dengan 1 sebelum ditampilkan)
$angka /= $float;   // Membagi dan menyimpan hasil pembagian pada $angka;

// String biasanya diawali dan ditutup dengan petik satu.
$sgl_quotes = '$String'; // => '$String'

// Hindari menggunakan petik dua kecuali menyertakan variabel lain
$dbl_quotes = "Ini adalah $sgl_quotes."; // => 'Ini adalah $String.'

// Karakter khusus hanya berlaku pada petik dua
$berfungsi          = "Ini mengandung \t karakter tab.";
$tidak_berfungsi    = 'Ini hanya mengandung garis miring dan huruf t: \t';

// Batasi variabel dengan kurung kurawal jika diperlukan
$uang = "Saya memiliki $${angka} di Bank.";

// Sejak PHP 5.3, nowdocs dapat digunakan untuk tak-terinterpolasi banyak-baris
$nowdoc = <<<'END'
Banyak baris
string
END;

// Heredocs akan melakukan interpolasi
$heredoc = <<<END
Banyak baris
$sgl_quotes
END;

// Menyambung string dapat dilakukan menggunakan .
echo 'String ini ' . 'tersambung';

// String dapat dijadikan parameter pada "echo"
echo 'Banyak', 'Parameter', 'String';  // Menampilkan 'BanyakParameterString'


/********************************
 * Konstan
 */

// Sebuah konstan didifinisikan menggunakan fungsi define()
// dan tidak bisa diganti/rubah selama program berjalan!

// Nama konstan yang benar diawali dengan huruf dan garis-bawah,
// diikuti dengan beberapa huruf, angka, atau garis-bawah.
define("FOO", "sesuatu");

// Mengakses konstan memungkinkan untuk dapat dipanggil tanpa menggunakan simbol $
echo FOO; // Menampilkan 'sesuatu'
echo 'Keluaran ini adalah ' . FOO;  // Menampilkan 'Keluaran ini adalah sesuatu'



/********************************
 * Larik (Array)
 */

// Semua larik dalam PHP bersifat asosiatif (saling berhubungan).

// Berfungsi pada semua versi PHP
$asosiatif = array('Satu' => 1, 'Dua' => 2, 'Tiga' => 3);

// Pada PHP 5.4 diperkenalkan cara penulisan (sintaks) baru
$asosiatif = ['Satu' => 1, 'Dua' => 2, 'Tiga' => 3];

echo $asosiatif['Satu']; // menampilkan 1

// Daftar literal secara tidak langsung ditentukan oleh kunci integer
$larik = ['Satu', 'Dua', 'Tiga'];
echo $larik[0]; // => "Satu"

// Menambahkan sebuah elemen pada akhir larik
$larik[] = 'Empat';
// atau
array_push($larik, 'Lima');

// Menghapus elemen dari larik
unset($larik[3]);

/********************************
 * Keluaran
 */

echo('Halo Dunia!');
// Menampilkan Halo Dunia! ke "stdout".
// "stdout" adalah sebuah halaman web ketika dijalankan dalam peramban (browser).

print('Halo Dunia!'); // Sama seperti "echo"

// "echo" dan "print" merupakan bahasa konstruksi, jadi tanda kurung dapat dihilangkan
echo 'Halo Dunia!';
print 'Halo Dunia!';

$paragraf = 'paragraf';

echo 100;           // Menampilkan variabel skalar secara langsung
echo $paragraf;     // atau sebuat variabel

// Jika PHP tag-singkat telah dikonfigurasi, atau versi PHP yang digunakan
// adalah 5.4.0 keatas, dapat digunakan sintaks "echo" singkat

?>
<p><?= $paragraf ?></p>
<?php

$x = 1;
$y = 2;
$x = $y; // $x sekarang berisi nilai yang sama dengan $y
$z = &$y;
// $z sekarang berisi referensi ke $y. Mengubah nilai dari $z
// akan mengubah nilai dari $y juga, begitupun sebaliknya.
// $x tetap tidak berubah sebagaimana nilai asli dari $y

echo $x; // => 2
echo $z; // => 2
$y = 0;
echo $x; // => 2
echo $z; // => 0

// Menampilkan tipe dan nilai dari variabel ke "stdout"
var_dump($z); // prints int(0)

// Menampilkan variabel ke "stdout" dalam format yang mudah dibaca
print_r($larik); // menampilkan: Array ( [0] => Satu [1] => Dua [2] => Tiga )

/********************************
 * Logika
 */
$a = 0;
$b = '0';
$c = '1';
$d = '1';

// menegaskan lemparan sebuah peringatan jika pernyataan tidak benar

// Perbandingan berikut akan selalu benar, meskipun memiliki tipe yang berbeda.
assert($a == $b); // kesamaan
assert($c != $a); // ketidak-samaan
assert($c <> $a); // versi lain dari ketidak-samaan
assert($a < $c);
assert($c > $b);
assert($a <= $b);
assert($c >= $d);

// Dibawah ini hanya akan bernilai benar jika nilainya memiliki tipe yang sama.
assert($c === $d);
assert($a !== $d);
assert(1 === '1');
assert(1 !== '1');

// Operator 'Spaceship' (sejak PHP 7)
// Mengembalikan 0 jika nilai pada kedua sisi adalah sama
// Mengembalikan 1 jika nilai pada sisi kiri lebih besar
// Mengembalikan -1 jika nilai pada sisi kanan lebih besar

$a = 100;
$b = 1000;

echo $a <=> $a; // 0 karena keduanya sama
echo $a <=> $b; // -1 karena $a < $b
echo $b <=> $a; // 1 karena $b > $a

// Variabel dapat dikonversi menjadi tipe lain, sesuai penggunaannya.

$integer = 1;
echo $integer + $integer; // => 2

$string = '1';
echo $string + $string; // => 2 (string dipaksa menjadi integer)

$string = 'satu';
echo $string + $string; // => 0
// Menghasilkan 0 karena operator (+) tidak dapat memaksa string 'satu' menjadi sebuah integer

// Perubahan tipe dapat dimanfaatkan untuk diperlakukan sebagai tipe lainnya

$boolean = (boolean) 1; // => true

$nol = 0;
$boolean = (boolean) $nol; // => false

// Terdapat juga fungsi khusus untuk melakukan perubahan terhadap beberapa tipe
$integer = 5;
$string = strval($integer);

$var = null; // Nilai Null


/********************************
 * Struktur Kontrol
 */

if (true) {
    print 'Saya tampil';
}

if (false) {
    print 'Saya tidak tampil';
} else {
    print 'Saya tampil';
}

if (false) {
    print 'Tidak tampil';
} elseif(true) {
    print 'Tampil';
}

// operator ternary
print (false ? 'Tidak tampil' : 'Tampil');

// cara pintas operator ternary mulai dirilis sejak PHP 5.3
// persamaan dari "$x ? $x : 'Kerjakan'"
$x = false;
print($x ?: 'Kerjakan');

// operator null coalesce sejak PHP 7
$a = null;
$b = 'Ditampilkan';
echo $a ?? 'a belum di-set'; // menampilkan 'a belum di-set'
echo $b ?? 'b belum di-set'; // menampilkan 'Ditampilkan'


$x = 0;
if ($x === '0') {
    print 'Tidak ditampilkan';
} elseif($x == '1') {
    print 'Tidak ditampilkan';
} else {
    print 'Tampil';
}


// Alternatif sintaks untuk kebutuhan templat:
?>

<?php if ($x): ?>
Ini ditampilkan jika pengujian benar.
<?php else: ?>
Selain tersebut ini yang akan ditampilkan.
<?php endif; ?>

<?php

// Gunakan "switch" untuk menghemat logika.
switch ($x) {
    case '0':
        print 'Switch mendukung tipe paksaan';
        break; // Kata kunci "break" harus disertakan, jika tidak
               // maka logika tersebut akan berlanjut ke bagian "dua" dan "tiga"
    case 'dua':
    case 'tiga':
        // Lakukan sesuatu jika $x bernilai "dua" atau "tiga"
        break;
    default:
        // Aksi cadangan
}

// "while", "do...while" dan perulangan "for"
$i = 0;
while ($i < 5) {
    echo $i++;
}; // Menampilkan "01234"

echo "\n";

$i = 0;
do {
    echo $i++;
} while ($i < 5); // Menampilkan "01234"

echo "\n";

for ($x = 0; $x < 10; $x++) {
    echo $x;
} // Menampilkan "0123456789"

echo "\n";

$roda = ['sepeda' => 2, 'mobil' => 4];

// Perulangan "foreach" dapat melakukan iterasi pada larik (array) 
foreach ($roda as $jumlah_roda) {
    echo $jumlah_roda;
} // Menampilkan "24"

echo "\n";

// Iterasi dapat dilakukan terhadap "key" (kunci) dan "value" (nilai)
foreach ($roda as $mesin => $jumlah_roda) {
    echo "$mesin memiliki $jumlah_roda buah roda";
}

echo "\n";

$i = 0;
while ($i < 5) {
    if ($i === 3) {
        break; // Menghentikan proses perulangan
    }
    echo $i++;
} // Menampilkan "012"

for ($i = 0; $i < 5; $i++) {
    if ($i === 3) {
        continue; // Melewati tahapan iterasi saat ini
    }
    echo $i;
} // Menampilkan "0124"


/********************************
 * Fungsi
 */

// Fungsi didefinisikan dengan "function":
function fungsi_saya () {
    return 'Halo';
}

echo fungsi_saya(); // => "Halo"

// Nama fungsi yang baik dan benar diawali dengan sebuah huruf atau garis-bawah, diikuti oleh
// beberapa huruf, angka, atau garis-bawah.

function jumlah ($x, $y = 1) { // $y merupakan opsional, jika tidak ditentukan akan bernilai 1
    $hasil = $x + $y;
    return $hasil;
}

echo jumlah(4); // => 5
echo jumlah(4, 2); // => 6

// $hasil tidak dapat diakses dari luar fungsi
// print $hasil; // Akan menghasilkan sebuah "warning".

// Sejak PHP 5.3 fungsi dapat dideklarasikan menjadi tanpa-nama (anonymous);
$inc = function ($x) {
    return $x + 1;
};

echo $inc(2); // => 3

function foo ($x, $y, $z) {
    echo "$x - $y - $z";
}

// Fungsi dapat mengembalikan fungsi juga
function bar ($x, $y) {
    // Gunakan "use" untuk mengakses variabel diluar fungsi
    return function ($z) use ($x, $y) {
        foo($x, $y, $z);
    };
}

$bar = bar('A', 'B');
$bar('C'); // Menampilkan "A - B - C"

// Fungsi uang memiliki nama dapat dipanggil berdasarkan string
$nama_fungsi = 'jumlah';
echo $nama_fungsi(1, 2); // => 3
// Bermanfaat untuk menentukan fungsi mana yang akan dipanggil secara dinamis.
// Atau, dapat juga menggunakan fungsi call_user_func(callable $callback [, $parameter [, ... ]]);

// Akses semua parameter yang dikirim ke sebuah fungsi
function parameter() {
    $jumlah_param = func_num_args();
    if( $jumlah_param > 0 ) {
        echo func_get_arg(0) . ' | ';
    }
    $daftar_param = func_get_args();
    foreach( $daftar_param as $kunci => $param ) {
        echo $kunci . ' - ' . $param . ' | ';
    }
}

parameter('Halo', 'Dunia'); // Halo | 0 - Halo | 1 - Dunia |

// Sejak PHP 5.6, mendapatkan jumlah variabel yang ada pada parameter 
function variabel($kata, ...$daftar) {
	echo $kata . " || ";
	foreach ($daftar as $item) {
		echo $item . ' | ';
	}
}

variable("Pemisah", "Halo", "Dunia") // Pemisah || Halo | Dunia | 

/********************************
 * Penyertaan ("include")
 */

<?php
// Skrip PHP yang berada dalam dokumen "include" juga harus dibuka dengan tag PHP.

include 'dokumen-saya.php';
// Kode yang ada dalam dokumen-saya.php sekarang dapat diakses dari cakupan saat ini.
// Jika dokumen tidak dapat disertakan (include, seperti dokumen tidak ditemukan), maka pesan peringatan akan muncul.

include_once 'dokumen-saya.php';
// Jika dokumen-saya telah disertakan (include) oleh perintah sebelumnya, maka
// dokumen tersebut tidak akan disertakan lagi. Ini bertujuan untuk menghindari kesalahan 
// yang diakibatkan oleh deklarasi ganda.

require 'dokumen-saya.php';
require_once 'dokumen-saya.php';
// Memiliki fungsi yang sama dengan "include", namun jika dokumen tidak ditemukan 
// atau tidak dapat disertakan maka akan menghasilkan pesan kesalahan fatal.

// Isi dari dokumen-saya.php:
<?php

return 'Apapun yang kamu suka.';
// akhir dari dokumen

// "include" dan "require" dapat mengembalikan sebuah nilai.
$nilai = include 'dokumen-saya.php';

// Dokumen akan disertakan berdasarkan lokasi direktori dokumen (file path) yang diberikan, jika tidak didefinisikan
// maka akan digunakan konfigurasi dari "include_path". Jika dokumen tidak ditemukan dalam "include_path",
// fungsi include akan melakukan pengecekan pada direktori yang sama dengan dokumen yang menggunakan fungsi include tersebut,
// jika tidak ditemukan juga maka pesan gagal akan dimunculkan.
/* */

/********************************
 * Kelas (class)
 */

// Kelas didefinisikan dengan kata "class"

class KelasSaya
{
    const NILAI_KONSTAN = 'nilai'; // Sebuah konstan

    static $nilaiStatis = 'statis';

    // Variabel statis dan hak jenis aksesnya
    public static $variabelStatisPublik = 'nilaiStatisPublik';
    // Hanya dapat diakses dalam kelas
    private static $variabelStatisPrivat = 'nilaiStatisPrivat';
    // Dapat diakses dalam kelas dan kelas turunan
    protected static $variabelStatisTerlindungi = 'nilaiStatisTerlindungi';

    // Properti harus mendeklarasikan hak aksesnya
    public $properti    = 'publik';
    public $PropertiInstansi;
    protected $variabel = 'terlindungi'; // Dapat diakses dari kelas itu sendiri dan kelas turunannya
    private $variabel   = 'tersembunyi';   // Hanya dapat diakses dari kelas itu sendiri

    // Membuat konstruktor dengan perintah __construct
    public function __construct($PropertiInstansi) {
        // Akses variabel instansi menggunakan perintah $this
        $this->PropertiInstansi = $PropertiInstansi;
    }

    // Method dideklarasikan sebagai fungsi didalam kelas
    public function methodSaya()
    {
        print 'KelasSaya';
    }

    // Perintah "final" membuat sebuah fungsi tidak dapat di-override oleh kelas turunannya
    final function tidakDapatDiOverride()
    {
    }

/*
 * Deklarasi properti atau method pada kelas sebagai statis membuat properti atau method tersebut
 * dapat diakses tanpa melakukan instansiasi kelas. Properti statis tidak dapat diakses melalui
 * objek kelas yang hasil instansiasi, sedangkan method statis bisa.
 */

    public static function methodStatisSaya()
    {
        print 'Saya adalah statis';
    }
}

// Konstan pada kelas dapat diakses secara statis
echo KelasSaya::NILAI_KONSTAN;      // Menampilkan 'nilai'

echo KelasSaya::$nilaiStatis;       // Menampilkan 'statis'
KelasSaya::methodStatisSaya();      // Menampilkan 'Saya adalah statis'

// Instansi kelas menggunakan perintah "new"
$kelas_saya = new KelasSaya('Sebuah properti instansiasi');
// Tanda kurung adalah opsional jika tidak ingin menggunakan argumen.

// Akses anggota kelas menggunakan ->
echo $kelas_saya->properti;             // => "publik"
echo $kelas_saya->propertiInstansi;     // => "Sebuah properti instansi"
$kelas_saya->methodSaya();              // => "KelasSaya"

// Menurunkan kelas menggunakan kata kunci "extends"
class KelasSayaLainnya extends KelasSaya
{
    function tampilkanPropertiTerlindungi()
    {
        echo $this->properti;
    }

    // "override" terhadap sebuah method
    function methodSaya()
    {
        parent::methodSaya();
        print ' > KelasSayaLainnya';
    }
}

$kelas_saya_lainnya = new KelasSayaLainnya('Instansiasi properti');
$kelas_saya_lainnya->tampilkanPropertiTerlindung();     // => Menampilkan "terlindungi"
$kelas_saya_lainnya->methodSaya();                      // Menampilkan "KelasSaya > KelasSayaLainnya"

final class SayaTidakBisaDiturunkan
{
}

// Gunakan method ajaib (magic method) untuk membuat fungsi "getters" dan "setters"
class PetaKelasSaya
{
    private $properti;

    public function __get($key)
    {
        return $this->$key;
    }

    public function __set($key, $value)
    {
        $this->$key = $value;
    }
}

$x = new PetaKelasSaya();
echo $x->properti;          // akan memanggil method __get()
$x->properti = 'Sesuatu';   // akan memanggil method __set();

// Kelas dapat dijadikan abstrak (menggunakan kata kunci "abstract"), atau
// meng-implementasikan interfaces (menggunakan kata kunci "implements").
// Sebuah interface dideklarasikan dengan perintah "interface".

interface InterfaceSatu
{
    public function kerjakanSesuatu();
}

interface InterfaceDua
{
    public function kerjakanYangLain();
}

// interface dapat diturunkan
interface InterfaceTiga extends InterfaceDua
{
    public function kerjakanYangBerbeda();
}

abstract class KelasAbstrakSaya implements InterfaceSatu
{
    public $x = 'kerjakanSesuatu';
}

class KelasKongkritSaya extends KelasAbstrakSaya implements InterfaceTwo
{
    public function kerjakanSesuatu()
    {
        echo $x;
    }

    public function kerjakanYangLain()
    {
        echo 'kerjakanYangLain';
    }
}

// Kelas dapat diimplementasikan pada banyak interface
class KelasLainnya implements InterfaceSatu, InterfaceDua
{
    public function kerjakanSesuatu()
    {
        echo 'kerjakanSesuatu';
    }

    public function kerjakanYangLain()
    {
        echo 'kerjakanYangLain';
    }
}


/********************************
 * Sifat (Traits)
 */

// Traits mulai tersedia sejak PHP 5.4.0 dan dideklarasikan menggunakan kata kunci "trait"

trait TraitSaya
{
    public function methodTraitSaya()
    {
        print 'Saya menggunakan Trait';
    }
}

class KelasTraitSaya
{
    use TraitSaya;
}

$kls = new KelasTraitSaya();
$kls->methodTraitSaya();    // menampilkan "Saya menggunakan Trait"


/********************************
 * Namespaces
 */

// Bagian ini telah dibatasi, karena deklarasi "namespace"
// karena harus ditempatkan diawal dokumen.

<?php

// Secara default, kelas tersedia sebagai namespace umum, dan dapat
// secara khusus dipanggil dengan garis-miring terbalik (backslash).

$kls = new \KelasSaya();


// Menentukan namespace untuk sebuah dokumen
namespace Saya\Namespace;

class KelasSaya
{
}

// (dari dokumen lainnya)
$kls = new Saya\Namespace\KelasSaya;

// Atau dari dalam namespace lainnya.
namespace Saya\Lainnya\Namespace;

use Saya\Namespace\KelasSaya;

$kls = new KelasSaya();

// Namespace dapat menggunakan alias

namespace Saya\Lainnya\Namespace;

use Saya\Namespace as SuatuKelasLainnya;

$kls = new SuatuKelasLainnya\KelasSaya();


/**********************
* Late Static Binding
*
*/

class KelasInduk {
    public static function siapa() {
        echo "Ini adalah " . __CLASS__ . "\n";
    }
    public static function coba() {
        // kata kunci "self" merujuk pada method yang berada dalam satu kelas
        self::who();
        // kata kunci "static" merujuk pada method yang berada di kelas dimana method itu dijalankan
        static::who();
    }
}

KelasInduk::coba();
/*
Ini adalah KelasInduk
Ini adalah KelasInduk
*/

class KelasAnak extends KelasInduk {
    public static function siapa() {
        echo "Tapi ini adalah " . __CLASS__ . "\n";
    }
}

KelasAnak::tes();
/*
Ini adalah KelasInduk
Tapi ini adalah KelasAnak
*/

/**********************
*  Magic constants
*  
*/

// Mendapatkan nama dari suatu kelas. Harus dideklarasikan didalam kelas tersebut.
echo "Nama kelas ini adalah " . __CLASS__;

// Mendapatkan alamat lengkap direktori
echo "Alamat direktori ini adalah " . __DIR__;

    // Beberapa yang banyak digunakan
    require __DIR__ . '/vendor/autoload.php';

// Mendapatkan alamat lengkap dokumen
echo "Alamat dokumen ini adalah " . __FILE__;

// Mendapatkan nama fungsi
echo "Nama fungsi ini adalah " . __FUNCTION__;

// Mendapatkan nomor baris perintah
echo "Nomor baris perintah ini adalah " . __LINE__;

// Mendapatkan nama method. Hanya mengembalikan sebuah nilai jika berada didalam trait atau deklarasi objek.
echo "Nama method ini adalah " . __METHOD__;

// Mendapatkan nama namespace 
echo "Namespace saat ini adalah " . __NAMESPACE__;

// Mendapatkan nama dari trait. Hanya mengembalikan sebuah nilai jika berada didalam trait atau deklarasi objek.
echo "Namespace saat ini adalah " . __TRAIT__;

/**********************
*  Penanganan Kesalahan (Error)
*  
*/

// Penanganan error sederhana menggunakan "try...catch"

try {
    // Kerjakan sesuatu
} catch (Exception $e) {
    // Penanganan exception
}

// Menggunakan "try...catch" blok pada namespace

try {
    // Kerjakan sesuatu
} catch (\Exception $e) {
    // Penanganan exception
}

// Exception khusus

class ExceptionSaya extends Exception {}

try {

    $kondisi = true;

    if ($kondisi) {
        throw new ExceptionSaya('Terjadi sesuatu');
    }

} catch (ExceptionSaya $e) {
    // Penanganan untuk exception khusus
}

```

## Informasi lainnya

Kunjungi [Dokumentasi resmi PHP](http://www.php.net/manual/) untuk referensi dan masukan komunitas.

Jika anda tertarik untuk belajar lebih dalam, kunjungi 
[PHP The Right Way](http://www.phptherightway.com/).

Jika anda terbiasa dengan manajemen paket, kunjungi
[Composer](http://getcomposer.org/).

Untuk standar umum, kunjungi PHP Framework Interoperability Group's
[PSR standards](https://github.com/php-fig/fig-standards).
