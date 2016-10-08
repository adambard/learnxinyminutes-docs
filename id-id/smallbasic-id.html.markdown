---
language: SmallBASIC
filename: learnsmallbasic-id.bas
contributors:
    - ["Chris Warren-Smith", "http://smallbasic.sourceforge.net"]
translators:
    - ["Rizky Luthfianto", "http://github.com/rilut"]
lang: id-id
---

## Tentang

SmallBASIC adalah *interpreter* bahasa BASIC yang mudah dan cepat dipelajari yang ideal untuk perhitungan sehari-hari, skrip dan prototipe. Fitur SmallBASIC termasuk trigonometri, matriks dan fungsi aljabar, yang dibangun di IDE, *library* string yang canggih, sistem, suara, dan perintah grafis bersama dengan sintaks pemrograman terstruktur.

## Pengembangan

SmallBASIC pada awalnya dikembangkan oleh Nicholas Christopoulos pada akhir tahun 1999 untuk Palm Pilot. pengembangan proyek telah dilanjutkan oleh Chris Warren-Smith sejak sekitar tahun 2005.

Versi SmallBASIC telah dibuat untuk sejumlah perangkat genggam termasuk Franklin eBookman dan Nokia 770. Juga berbagai versi desktop yang telah dirilis berdasarkan berbagai GUI. Platform yang didukung saat ini adalah Linux dan Windows berbasis SDL2 dan Android berbasis NDK. Sebuah versi baris perintah pada desktop juga tersedia, meskipun tidak biasanya dirilis dalam bentuk biner.

Sekitar tahun 2008, sebuah perusahaan merilis lingkungan pemrograman BASIC dengan nama yang mirip. SmallBASIC tidak berhubungan dengan itu.

```
REM ini adalah komentar
'dan ini juga komentar

REM mencetak kalimat
print "halo"
? "Tanda ? adalah singkatan dari PRINT"

REM Struktur kontrol
FOR index = 0 TO 10 STEP 2
  ? "Ini adalah nomor baris"; indeks
NEXT
J=0
REPEAT
 J++
UNTIL J=10
WHILE J>0
 J--
WEND

REM Pernyataan "Select case"
Select Case "Cool"
 Case "null", 1,2,3,4,5,6,7,8,"Cool","blah"
 Case "Not cool"
   PRINT "Epic fail"
 Case Else
   PRINT "Fail"
End Select

REM menangkap kesalahan dengan TRY / CATCH
Try
  fn = Freefile
  Open filename For Input As #fn
Catch err
  Print "gagal membuka file"
End Try

REM Fungsi dan subrutin buatan pengguna
func add2(x, y)
  'Variabel dapat dinyatakan sebagai lokal dalam lingkup/scope dari SUB atau FUNC
  local k
  k = "k akan lenyap ketika FUNC ini mengembalikan nilai"
  add2 = x + y
akhir
Print add2(5,5)
sub cetak_ini(ini)
  print ini
end
cetak_ini "INI"

REM Menampilkan garis dan piksel
At 0,ymax/2+txth("Q")
Color 1: ? "sin(x)":
Color 8: ? "cos(x)":
Color 12: ? "tan(x)"
Line 0,ymax/2,xmax,ymax/2
For i=0 to xmax
  Pset i,ymax/2-sin(i*2*pi/ymax)*ymax/4 color 1
  Pset i,ymax/2-cos(i*2*pi/ymax)*ymax/4 color 8
  Pset i,ymax/2-tan(i*2*pi/ymax)*ymax/4 color 12
Next
showpage

REM SmallBASIC cocok untuk bereksperimen dengan fraktal dan efek menarik lainnya
Delay 3000
Randomize
ff = 440.03
For j = 0 to 20
  r = rnd * 1000 % 255
  b = rnd * 1000 % 255
  g = rnd * 1000 % 255
  c = rgb(r,b,g)
  ff += 9.444
  for i=0 to 25000
    f += ff
    x = min(xmax, -x + cos(f*i))
    y = min(ymax, -y + sin(f*i))
    pset x, y color c
    if (i%1000==0) then
      showpage
    fi
  next
Next j

REM Untuk sejarawan komputer, SmallBASIC dapat menjalankan program
REM dari buku dan majalah komputer lama, misalnya:
10 LET A=9
20 LET B=7
30 PRINT A*B
40 PRINT A/B

REM SmallBASIC juga memiliki dukungan untuk beberapa konsep modern seperti JSON
aa = array("{\"kucing\":{\"nama\":\"harry\"},\"peliharaan\":\"true\"}")
If (ismap(aa) == false) Then
  throw "bukan tipe data map"
End If
Print aa

PAUSE

```

## Artikel

* [Persiapan](http://smallbasic.sourceforge.net/?q=node/1573)
* [Selamat Datang di SmallBASIC](http://smallbasic.sourceforge.net/?q=node/838)

## GitHub

* [Source code](https://github.com/smallbasic/SmallBASIC)
* [Referensi snapshot](http://smallbasic.github.io/)
