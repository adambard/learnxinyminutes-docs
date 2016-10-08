---
language: java
contributors:
    - ["Jake Prather", "http://github.com/JakeHP"]
    - ["Jakukyo Friel", "http://weakish.github.io"]
    - ["Madison Dickson", "http://github.com/mix3d"]
    - ["Simon Morgan", "http://sjm.io/"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["Cameron Schermerhorn", "http://github.com/cschermerhorn"]
    - ["Rachel Stiyer", "https://github.com/rstiyer"]
filename: LearnJava-id.java
translators:
  - ["Ahmad Zafrullah", "https://github.com/23Pstars"]
lang: id-id
---

Java adalah bahasa pemrograman yang memiliki tujuan umum dan berorientasi kelas dan objek.
[Baca lebih lanjut.](http://docs.oracle.com/javase/tutorial/java/)

```java
// Komentar satu baris diawali dengan // (dua garis miring)
/*
Ini adalah contoh komentar banyak-baris.
*/
/**
Ini adalah contoh komentar JavaDoc. Digunakan untuk mendeskripsikan sebuah kelas,
atau beberapa sifat dari kelas tersebut.
*/

// Menyertakan kelas ArrayList dalam paket java.util
import java.util.ArrayList;
// Menyertakan semua kelas yang ada dalam paket java.security
import java.security.*;

// Setiap dokumen .java sebuah kelas publik dengan nama yang sama dengan nama kelas.
public class BelajarJava {

    // Untuk menjalankan program java, program harus memiliki sebuah method utama (main) sebagai awalan.
    public static void main (String[] args) {

        // System.out.println() digunakan untuk menampilkan satu baris teks.
        System.out.println("Halo Dunia!");
        System.out.println(
            "Integer: " + 10 +
            " Double: " + 3.14 +
            " Boolean: " + true);

        // System.out.print() hanya menampilkan teks tanpa baris baru.
        System.out.print("Halo ");
        System.out.print("Dunia");

        // System.out.printf() memudahkan dalam mengatur format penampilan.
        System.out.printf("pi = %.5f", Math.PI); // => pi = 3.14159

        ///////////////////////////////////////
        // Variabel
        ///////////////////////////////////////

        /*
        *  Deklarasi Variabel
        */
        // Deklarasi variabel menggunakan format <tipe> <nama>
        int nilai;
        // Deklarasi banyak variabel menggunakan format yang sama <tipe> <nama1>, <tipe> <nama2>, <tipe> <nama3>
        int nilai1, nilai2, nilai3;

        /*
        *  Inisialisasi Variabel
        */

        // Inisialisasi sebuah variabel menggunakan <tipe> <nama> = <nilai>
        int nilai = 1;
        // Inisialisasi banyak variabel menggunakan format yang sama <tipe> <nama1>, <nama2>, <nama3> = <nilai>
        int nilai1, nilai2, nilai3;
        nilai1 = nilai2 = nilai3 = 1;

        /*
        *  Tipe Variabel
        */
        // Byte - 8 bit signed untuk bilangan bulat komplemen 2
        // (-128 <= byte <= 127)
        byte nilaiByte = 100;

        // Short - 8 bit signed untuk bilangan bulat komplemen 2
        // (-32,768 <= short <= 32,767)
        short nilaiShort = 10000;

        // Integer - 32 bit signed untuk bilangan bulat komplemen 2
        // (-2,147,483,648 <= int <= 2,147,483,647)
        int nilaiInt = 1;

        // Long - 64 bit signed untuk bilangan bulat komplemen 2
        // (-9,223,372,036,854,775,808 <= long <= 9,223,372,036,854,775,807)
        long nilaiLong = 100000L;
        // Karakter "L" pada akhir nilai menyatakan tipe Long;
        // selainnya akan dianggap sebagai nilai bilangan bulat.

        // Catatan: Java tidak memiliki tipe unsigned.

        // Float - Presisi-satu 32-bit standar IEEE 754 untuk Floating Point
        // 2^-149 <= float <= (2-2^-23) * 2^127
        float nilaiFloat = 234.5f;
        // Karakter "f" atau "F" pada akhir nilai menyatakan tipe Float;
        // selainnya akan dianggap sebagai nilai double.

        // Double - Presisi-dua 64-bit standar IEEE 754 untuk Floating Point
        // 2^-1074 <= x <= (2-2^-52) * 2^1023
        double nilaiDouble = 123.4;

        // Boolean - true & false
        boolean nilaiBoolean = true;
        boolean nilaiBoolean = false;

        // Char - Sebuah karakter Unicode 16-bit
        char nilaiChar = 'A';

        // Variabel "final" tidak dapat di-set kembali nilainya pada objek lain,
        final int WAKTU_SAYA_BEKERJA_TIAP_MINGGU = 9001;
        // tapi dapat dilakukan inisialisasi diwaktu yang lain.
        final double E;
        E = 2.71828;


        // BigInteger - Bilangan bulat yang memiliki presisi dinamis
        //
        // BigInteger adalah tipe data yang memungkinkan pembuat program untuk memanipulasi
        // bilangan bulat lebih panjang dari 64-bit. Bilangan bulat tersebut tersimpan dalam
        // bentuk kumpulan byte (array) dan dimanipulasi menggunakan fungsi yang sudah tersedia
        // pada BigInteger
        //
        // BigInteger dapat diinisialisasi menggunakan kumpulan byte atau teks.
        
        BigInteger nilaiBigInteger = new BigInteger(kumpulanByte);


        // BigDecimal - Bilangan signed desimal yang memiliki presisi dinamis
        //
        // Tipe BigDecimal memiliki dua bagian: sebuah bilangan bulat dengan nilai presisi
        // dinamis tanpa skala dan sebuah bilangan bulat skala 32-bit.

        // BigDecimal memungkinkan pembuat program untuk memegang kontrol penuh
        // terhadap batas desimal. BigDecimal baik digunakan untuk nilai tukar mata uang
        // dimana sangat mementingkan presisi nilai desimal.
        //
        // BigDecimal dapat diinisialisasi dengan int, long, double, String,
        // atau dengan melakukan inisialisasi nilai tanpa skala (BigInteger) 
        // dan nilai dengan skala (int). 

        BigDecimal nilaiBigDecimal = new BigDecimal(nilaiBigInteger, nilaiInt);
        
        // Perlu diperhatikan konstruktor yang digunakan apakah float atau double
        // karena dapat mengakibatkan ketidak-akurasian float/double yang akan digunakan
        // dalam BigDecimal. Sebaiknya gunakan nilai String pada konstruktor
        // jika membutuhkan nilai pasti.
        
        BigDecimal sepuluhSen = new BigDecimal("0.1");


        // Strings
        String nilaiString1 = "Ini adalah contoh String!";

        // Karakter \n berfungsi untuk membuat baris baru 
        String nilaiString2 = "Menampilkan baris baru?\nTidak masalah!";
        // Karakter \t berfungsi untuk membuat tab antar karakter
        String nilaiString3 = "Ingin menambahkan sebuah tab?\tTidak masalah!";
        System.out.println(nilaiString1);
        System.out.println(nilaiString2);
        System.out.println(nilaiString3);

        // Larik (array)
        // Ukuran array harus ditentukan ketika instansiasi
        // Format berikut adalah beberapa cara deklarasi array
        // <tipe data>[] <nama variabel> = new <tipe data>[<ukuran array>];
        // <tipe data> <nama variabel>[] = new <tipe data>[<ukuran array>];
        int[] barisAngka = new int[10];
        String[] barisString = new String[1];
        boolean barisBoolean[] = new boolean[100];

        // Cara lain untuk mendeklarasikan dan menginisialisasi sebuah array
        int[] y = {9000, 1000, 1337};
        String nama[] = {"Andi", "Budi", "Agus"};
        boolean bools[] = new boolean[] {true, false, false};

        // Indeks sebuah array - Mengakses sebuah elemen
        System.out.println("barisAngka @ 0: " + barisAngka[0]);

        // Array menggunakan indeks 0 yang tetap.
        barisAngka[1] = 1;
        System.out.println("barisAngka @ 1: " + barisAngka[1]); // => 1

        // Lainnya yang perlu diketahui
        // ArrayLists - Sama seperti array biasa, namum penggunaannya sudah ditentukan,
        //              dan ukurannya dapat berubah-ubah.
        // LinkedLists - Implementasi dari doubly-linked list. Semua operasi yang digunakan
        //               hampir sama dengan operasi yang dimiliki oleh sebuah doubly-linked list.
        // Maps - Sebuah kumpulan objek yang menyatakan hubungan antara kunci dan nilai. Map merupakan
        //        sebuah interface sehingga tidak dapat diinstansiasi. Jenis kunci dan nilai yang digunakan
        //        pada Map harus spesifik pada saat instansiasi ketika diimplementasikan pada sebuah kelas.
        //        Setiap kunci hanya memiliki sebuah nilai, dan hanya muncul sekali.
        // HashMaps - Kelas ini menggunakan tabel-hash untuk mengimplementasikan interface Map.
        //            Hal ini memungkinkan waktu eksekusi ketika melakukan operasi dasar (mengakses
        //            dan menambahkan elemen) menjadi konstan, meskipun memiliki banyak set data.

        ///////////////////////////////////////
        // Operator
        ///////////////////////////////////////
        System.out.println("\n->Operator");

        int i1 = 1, i2 = 2; // Cara singkat untuk deklarasi banyak nilai

        // Kemudahan dalam artimatika
        System.out.println("1+2 = " + (i1 + i2)); // => 3
        System.out.println("2-1 = " + (i2 - i1)); // => 1
        System.out.println("2*1 = " + (i2 * i1)); // => 2
        System.out.println("1/2 = " + (i1 / i2)); // => 0 (int/int menghasilkan int juga)
        System.out.println("1/2 = " + (i1 / (double)i2)); // => 0.5

        // Modulus
        System.out.println("11%3 = "+(11 % 3)); // => 2

        // Operator Perbandingan
        System.out.println("3 == 2? " + (3 == 2)); // => false
        System.out.println("3 != 2? " + (3 != 2)); // => true
        System.out.println("3 > 2? " + (3 > 2)); // => true
        System.out.println("3 < 2? " + (3 < 2)); // => false
        System.out.println("2 <= 2? " + (2 <= 2)); // => true
        System.out.println("2 >= 2? " + (2 >= 2)); // => true

        // Operator Boolean
        System.out.println("3 > 2 && 2 > 3? " + ((3 > 2) && (2 > 3))); // => false
        System.out.println("3 > 2 || 2 > 3? " + ((3 > 2) || (2 > 3))); // => true
        System.out.println("!(3 == 2)? " + (!(3 == 2))); // => true

        // Operator Bitwise
        /*
        ~      Unary bitwise complement
        <<     Signed left shift
        >>     Signed/Arithmetic right shift
        >>>    Unsigned/Logical right shift
        &      Bitwise AND
        ^      Bitwise exclusive OR
        |      Bitwise inclusive OR
        */

        // Peningkatan
        int i = 0;
        System.out.println("\n->Pengurangan/Peningkatan");
        // Operator ++ dan -- masing-masing melakukan peningkatan dan penurunan 1 nilai.
        // Jika diletakkan sebelum variabel, maka akan di tambah/kurang 1 sebelum dilakukan perintah lainnya;
        // jika setelah variabel, maka akan ditambah/kurang 1 setelah dilakukan perintah lainnya;
        System.out.println(i++); // i = 1, prints 0 (peningkatan setelahnya)
        System.out.println(++i); // i = 2, prints 2 (peningkatan sebelumnya)
        System.out.println(i--); // i = 1, prints 2 (pengurangan setelahnya)
        System.out.println(--i); // i = 0, prints 0 (pengurangan sebelumnya)

        ///////////////////////////////////////
        // Struktur Kontrol
        ///////////////////////////////////////
        System.out.println("\n->Struktur Kontrol");

        // Perintah "if" hampir sama dengan bahasa C
        int j = 10;
        if (j == 10) {
            System.out.println("Saya ditampilkan");
        } else if (j > 10) {
            System.out.println("Saya tidak ditampilkan");
        } else {
            System.out.println("Saya juga tidak ditampilkan");
        }

        // Perulangan "while"
        int fooWhile = 0;
        while(fooWhile < 100) {
            System.out.println(fooWhile);
            // Tingkatkan penghitung
            // 100 kali iterasi, fooWhile 0,1,3,...,99
            fooWhile++;
        }
        System.out.println("Nilai fooWhile: " + fooWhile);

        // Perulangan "do...while"
        int fooDoWhile = 0;
        do {
            System.out.println(fooDoWhile);
            // Tingkatkan penghitung
            // 99 kali iterasi, fooDoWhile 0->99
            fooDoWhile++;
        } while(fooDoWhile < 100);
        System.out.println("Nilai fooDoWhile: " + fooDoWhile);

        // Perulangan "for"
        // Struktur perulangan "for" => for(<awal_pernyataan>; <kondisi>; <langkah/tahapan>)
        for (int fooFor = 0; fooFor < 10; fooFor++) {
            System.out.println(fooFor);
            // 10 kali iterasi, foofor 0-9
        }
        System.out.println("Nilai fooFor: " + fooFor);
        
        // Perulangan "for" bertingkat dengan label "exit"
        outer:
        for (int i = 0; i < 10; i++) {
          for (int j = 0; j < 10; j++) {
            if (i == 5 && j ==5) {
              break outer;
              // Menghentikan semua perulangan, tidak hanya perulangan bagian dalam saja 
            }
          }
        }
        
        // Perulangan "for each"
        // Perulangan "for" juga dapat melakukan iterasi terhadap larik (array) dari objek
        // yang mana mengimplementasikan interface Ieterable.
        int[] fooList = {1, 2, 3, 4, 5, 6, 7, 8, 9};
        // Struktur perulangan "for each" => for (<objek> : <iterable>)
        // dibaca: setiap elemen dalam iterable
        // catatan: tipe objek harus sama dengan tipe iterable

        for (int bar : fooList) {
            System.out.println(bar);
            // Melakukan interasi sebanyak 9 kali dan menampilkan 1-9 tiap baris
        }

        // "switch case"
        // "switch" dapat digunakan pada byte, short, char, dan tipe data bilangan bulat (int).
        // "switch" juga dapat digunakan pada tipe "enum" (dijelaskan nanti), kelas String,
        // dan beberapa kelas khusus yang mengandung tipe data primitif:
        // Character, Byte, Short, dan Integer.
        int bulan = 3;
        String bulanString;
        switch (bulan) {
            case 1: bulanString = "Januari";
                    break;
            case 2: bulanString = "Februari";
                    break;
            case 3: bulanString = "Maret";
                    break;
            default: bulanString = "Bulan lainnya";
                     break;
        }
        System.out.println("Hasil switch case: " + bulanString);
        
        // Mulai dari Java 7 keatas, "switch" memiliki format:
        String jawabanSaya = "mungkin";
        switch(jawabanSaya) {
            case "ya":
                System.out.println("Anda menjawab ya.");
                break;
            case "tidak":
                System.out.println("Anda menjawab tidak.");
                break;
            case "mungkin":
                System.out.println("Anda menjawab mungkin.");
                break;
            default:
                System.out.println("Anda menjawab " + jawabanSaya);
                break;
        }

        // Pengkondisian dengan cara singkat
        // Karakter '?' dapat digunakan untuk penilaian atau logika secara cepat antara dua pernyataan.
        // Dibaca "Jika (pernyataan) adalah benar, gunakan <nilai pertama>, sisanya gunakan <nilai kedua>
        int foo = 5;
        String bar = (foo < 10) ? "A" : "B";
        System.out.println(bar); // Menampilkan A, karena pernyataannya benar


        ////////////////////////////////////////
        // Konversi Data dan Tipe Data (Typecasting)
        ////////////////////////////////////////

        // Konversi Data

        // Konversi String ke Integer
        Integer.parseInt("123");    // menghasilkan nilai versi Integer dari "123"

        // Konversi Integer ke String
        Integer.toString(123);      // menghasilkan nilai versi String dari 123

        // Untuk konversi lainnya silakan coba kelas berikut:
        // Double
        // Long
        // String

        // Typecasting
        // Objek dalam Java juga dapat dikonversi, banyak penjelasan dan aturan
        // dengan beberapa konsep sederhana. Silakan cek di alamat berikut:
        // http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html


        ///////////////////////////////////////
        // Kelas dan Fungsi
        ///////////////////////////////////////

        System.out.println("\n->Kelas & Fungsi");

        // (penjelasan mengenai kelas "Sepeda" ada dibawah)

        // Gunakan "new" untuk melakukan instansiasi pada kelas
        Sepeda laju = new Sepeda();

        // Memanggil method objek
        laju.tambahKecepatan(3); // Dapat juga digunakan "setter" dan "getter" method
        laju.setIrama(100);

        // Method "toString()" menghasilkan representasi string dari objek.
        System.out.println("informasi jalur: " + laju.toString());

        // Dua Pasang Inisialisasi
        // Bahasa Java tidak memiliki sintaks untuk membuat koleksi dari "static" sekaligus
        // dengan mudah, kecuali dengan cara berikut:

        private static final Set<String> NEGARA = new HashSet<String>();
        static {
           validCodes.add("INDONESIA");
           validCodes.add("MALAYSIA");
           validCodes.add("SINGAPURA");
        }

        // Terdapat cara yang baik untuk menulis skrip dengan mudah,
        // dengan menggunakan Dua-Kurung Kurawal Inisialisasi (Double Brace Initialization)

        private static final Set<String> NEGARA = new HashSet<String>() {{
            add("INDONESIA");
            add("MALAYSIA");
            add("SINGAPURA");
        }}

        // Kurung kurawal yang pertama membuat sebuah AnonymousInnerClas
        // dan kurung kurawal yang kedua mendeklarasikan instance dari blok
        // inisialisasi. Blok ini kemudian dipanggil ketika InnerClass dibentuk.
        // Cara ini tidak hanya berfungsi pada koleksi data, juga dapat digunakan
        // pada semua kelas bukan-"final".

    } // Akhir dari method utama
} // Akhir dari kelas BelajarJava


// Kelas bukan-"public" lainnya dapat dimasukkan kedalam satu dokumen .java,
// namun tidak dianjurkan, sebaiknya memisahkan menjadi beberapa dokumen terpisah.

// Sintaks pendeklarasian kelas:
//<public/private/protected> class <nama kelas> {
//    // isi data, konstruktor, dan fungsi.
//    // dalam Java, fungsi biasa disebut juga "method"
// }

class Sepeda {

    // Variabel dari kelas Sepeda
    public int irama;       // Public: dapat diakses dari manapun
    private int kecepatan;  // Private: hanya dapat diakses dari dalam kelas
    protected int rodaGigi;     // Protected: dapat diakses dari dalam kelas dan turunan kelas
    String nama;            // Default: hanya dapat diakses kelas yang berada dalam paket yang sama

    static String namaKelas;    // Variabel "static"

    // Blok Static
    // Java tidak memiliki implementasi untuk konstruktor "static", namun
    // memiliki blok status yang dapat digunakan untuk inisialisasi variabel
    // dalam kelas (variabel "static").
    // Blok ini akan dipanggil secara otomatis ketika kelas dijalankan.
    static {
        namaKelas = "Sepeda";
    }

    // Konstruktor adalah salah satu cara untuk membuat kelas
    // Ini adalah bagian konstruktor
    public Sepeda() {
        // Dapat juga dipanggil konstruktor lainnya:
        // this(1, 50, 5, "Bontrager");
        rodaGigi = 1;
        irama = 50;
        kecepatan = 5;
        nama = "Bontrager";
    }

    // Ini adalah bagian konstruktor yang menggunakan argumen (parameter)
    public Sepeda(int iramaAwal, int kecepatanAwal, int rodaGigiAwal,
        String nama) {
        this.rodaGigi = rodaGigiAwal;
        this.irama = iramaAwal;
        this.kecepatan = kecepatanAwal;
        this.nama = nama;
    }

    // Sintaks untuk method:
    // <public/private/protected> <tipe kembalian> <nama fungsi>(<args>)

    // Kelas Java terkadang mengimplementasikan "getters" dan "setters" untuk data.

    // Sintaks untuk deklarasi method:
    // <public/private/protected> <tipe kembalian> <nama fungsi>(<args>)
    public int getIrama() {
        return irama;
    }

    // Tipe "void" tidak memiliki kembalian (return) nilai
    public void setIrama(int nilaiBaru) {
        irama = nilaiBaru;
    }

    public void setRodaGigi(int nilaiBaru) {
        rodaGigi = nilaiBaru;
    }

    public void tambahKecepatan(int nilaiTambahan) {
        kecepatan += nilaiTambahan;
    }

    public void kurangiKecepatan(int nilaiPengurangan) {
        kecepatan -= nilaiPengurangan;
    }

    public void setNama(String namaBaru) {
        nama = namaBaru;
    }

    public String getNama() {
        return nama;
    }

    // Method untuk menampilkan nilai dari tiap atribut yang dimiliki objek Sepeda.
    @Override // Diturunkan dari kelas "Object" (Pustaka Java).
    public String toString() {
        return "roda gigi: " + rodaGigi + " irama: " + irama + " kecepatan: " + kecepatan +
            " nama: " + nama;
    }
} // akhir dari kelas Sepeda

// PennyFarthing adalah kelas turunan dari Sepeda
class PennyFarthing extends Sepeda {
    // (Penny Farthings adalah sepeda dengan roda depan yang besar,
    // dan tidak memiliki roda gigi.)
    // (Penny Farthings are those bicycles with the big front wheel.
    // They have no gears.)

    public PennyFarthing(int startCadence, int startSpeed) {
        // Call the parent constructor with super
        super(startCadence, startSpeed, 0, "PennyFarthing");
    }

    // You should mark a method you're overriding with an @annotation.
    // To learn more about what annotations are and their purpose check this
    // out: http://docs.oracle.com/javase/tutorial/java/annotations/
    @Override
    public void setRodaGigi(int rodaGigi) {
        roda rodaGigi = 0;
    }
}

// Interfaces
// Sintaks untuk deklarasi Interface
// <level akses> interface <nama interface> extends <interface induk> {
//     // Konstan
//     // Deklarasi method
// }

// Contoh - Makanan:
public interface dapatDimakan {
    public void makan();    // Setiap kelas yang menggunakan interface "dapatDimakan",
                            // harus mengimplementasikan method "makan".
}

public interface dapatDicerna {
    public void cerna();
}


// Membuat kelas dengan mengimplementasikan dua interface dalam satu waktu.
public class Buah implements dapatDimakan, dapatDicerna {
  
    @Override
    public void makan() {
        // ...
    }

    @Override
    public void cerna() {
        // ...
    }
}

// Dalam Java, kelas hanya dapat diturunkan sekali, tapi dapat mengimplementasikan
// banyak interface. Contoh:
public class ContohKelas extends ContohKelasInduk implements InterfaceSatu,
    InterfaceDua {

    @Override
    public void MethodInterfaceSatu() {
    }

    @Override
    public void MethodInterfaceDua() {
    }

}

// Kelas Abstrak (Abstract)
// Sintaks untuk deklarasi kelas abstrak
// Abstract Class declaration syntax
// <level akses> abstract <nama kelas abstrak> extends <induk kelas abstrak> {
//     // Konstan dan variabel
//     // Deklarasi method

// Menjadikan kelas sebagai abstrak adalah memungkinkan kelas berisi method abstrak
// yang harus didefinisikan pada kelas turunannya. Mirip dengan Interface, kelas abstrak
// tidak dapat dilakukan instansiasi, namun harus diturunkan pada kelas lain dan method abstrak
// harus didefinisikan. Perbedaannya dengan Interface ialah kelas abstrak dapat berisi method
// kongkrit dan method abstrak. Pada Interface method tidak dapat memiliki isi, artinya hanya
// method statis, dan variabel langsung ditentukan menjadi final, tidak seperti kelas abstrak.
// Kelas abstrak juga dapat memiliki method "main".

public abstract class Hewan
{
    public abstract void bersuara();

    // Method biasa dapat memiliki isi
    public void makan()
    {
        System.out.println("Saya adalah hewan dan Saya makan.");
        // Catatan: Kita dapat mengakses variabel private yang ada disini.
        umur = 30;
    }

    // Tidak perlu dilakukan inisialisasi, berbeda dengan Interface
    // sebuah variabel adalah final dan harus dilakukan inisialisasi. 
    protected int umur;

    public void tampilkanUmur()
    {
        System.out.println(umur);  
    }

    // Kelas abstrak dapat memiliki fungsi utama (main).
    public static void main(String[] args)
    {
        System.out.println("Saya adalah kelas abstrak!");
    }
}

class Kucing extends Hewan
{
    // Catatan: kelas ini harus melakukan override method abstrak
    // yang ada pada kelas abstrak (induk).
    @Override
    public void bersuara()
    {
        System.out.println("Moe");
        // umur = 30;	==> ERROR!	umur merupakan variabel private pada abstrak Hewan
    }

    // CATATAN: Akan muncul error jika menggunakan 
    // keterangan @Override pada method utama (main),
    // Java tidak mengizinkan hal tersebut.
    // Kejadian ini sering disebut sebagai METHOD HIDING.
    // Pertanyaan-jawaban yang menarik dapat dilihat: http://stackoverflow.com/questions/16313649/
    public static void main(String[] args)
    {
        Kucing moe = new Kucing();
        noe.bersuara();
        moe.makan();
        moe.tampilkanUmur();
    }
}

// Kelas Final

// Sintaks untuk deklarasi kelas Final
// <level akses> final <nama kelas final> {
//     // Konstann dan variabel
//     // Deklarasi method
// }

// Kelas Final merupakan kelas yang tidak dapat diturunkan sehingga menjadikan
// method tersebut turunan method terakhir. Disisi lain, kelas final merupakan
// lawan dari kelas abstrak karena kelas abstrak dapat diturunkan lagi, sedangkan
// kelas final tidak dapat diturunkan lagi.
public final class Serigala extends Hewan
{
    // Catatan: method abstrak harus di-override pada kelas abstrak.
    @Override
    public void bersuara()
    {
        System.out.println("Auuww");
    }
}

// Method Final
public abstract class Mamalia()
{
    // Sintaks untuk method final:
    // <level akses> final <tipe kembalian> <nama fungsi>(<args>)

    // Method final, seperti kelas final tidak dapat di-override oleh kelas turunan,
    // sehingga menjadikannya implementasi terakhir dari method.
    public final boolean apakahBerdarahDingin()
    {
        return true;
    }
}


// Tipe Enum
//
// Tipe Enum merupakan tipe data spesial yang memungkinkan sebuah nilai dijadikan
// konstan awal (predefined). Variabel setidaknya harus memiliki nilai yang sama
// dengan salah satu dari enum-enum yang telah ditentukan. Karena nilainya merupakan
// konstan, untuk itu penamaannya menggunakan huruf kapital (uppercase). Dalam Java,
// Enum didefinisikan dengan kata kunci "enum". Contohnya nama-nama hari dalam semunggu:

public enum Hari {
    SENIN, SELASA, RABU, KAMIS,
    JUMAT, SABTU, MUNGGU 
}

// Cara menggunakan Enum:
public class CobaEnum {
    
    // Variabel Enum
    Hari hari;
    
    // Konstruktor
    public CobaEnum(Hari hari) {
        this.hari = hari;
    }
    
    public void tampilkanKeterangan() {
        switch (day) {
            case SENIN:
                System.out.println("Senin adalah hari yang menyebalkan.");
                break;
                    
            case JUMAT:
                System.out.println("Jumat adalah hari yang singkat.");
                break;
                         
            case SABTU: 
            case MINGGU:
                System.out.println("Akhir pekan adalah hari yang menyenangkan.");
                break;
                        
            default:
                System.out.println("Hari kerja yang biasa saja.");
                break;
        }
    }
    
    public static void main(String[] args) {
        CobaEnum hariPertama = new CobaEnum(Hari.SENIN);
        hariPertama.tampilkanKeterangan();      // Senin adalah hari yang menyebalkan.
        CobaEnum hariKetiga = new CobaEnum(Hari.RABU);
        hariPertama.tampilkanKeterangan();      // Hari kerja yang biasa saja.
    }
}

// Tipe enum memiliki banyak kegunaan selain yang dicontohkan diatas.
// Tipe enum dapat memiliki isi seperti method dan variabel.
// Penjelasan lebih detail di https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

```

## Referensi Lainnya

Link-link berikut hanya menyediakan pemahaman lebih lanjut mengenai topik diatas. 
Tip, trik, dan contoh lainnya dapat melakukan pencarian melalui Google atau mesin pencari yang lain. 

**Panduan resmi Oracle**

* [Java Tutorial Trail from Sun / Oracle](http://docs.oracle.com/javase/tutorial/index.html)

* [Java Access level modifiers](http://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html)

* [Object-Oriented Programming Concepts](http://docs.oracle.com/javase/tutorial/java/concepts/index.html):
    * [Inheritance](http://docs.oracle.com/javase/tutorial/java/IandI/subclasses.html)
    * [Polymorphism](http://docs.oracle.com/javase/tutorial/java/IandI/polymorphism.html)
    * [Abstraction](http://docs.oracle.com/javase/tutorial/java/IandI/abstract.html)

* [Exceptions](http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html)

* [Interfaces](http://docs.oracle.com/javase/tutorial/java/IandI/createinterface.html)

* [Generics](http://docs.oracle.com/javase/tutorial/java/generics/index.html)

* [Java Code Conventions](http://www.oracle.com/technetwork/java/codeconvtoc-136057.html)

**Tutorial dan Praktik Online**

* [Learneroo.com - Learn Java](http://www.learneroo.com)

* [Codingbat.com](http://codingbat.com/java)


**Buku**:

* [Head First Java](http://www.headfirstlabs.com/books/hfjava/)

* [Thinking in Java](http://www.mindview.net/Books/TIJ/)

* [Objects First with Java](http://www.amazon.com/Objects-First-Java-Practical-Introduction/dp/0132492660)

* [Java The Complete Reference](http://www.amazon.com/gp/product/0071606300)
