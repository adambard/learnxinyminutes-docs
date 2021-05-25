---
category: tool
tool: bash
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
    - ["Alexandre Medeiros", "http://alemedeiros.sdf.org"]
    - ["Denis Arh", "https://github.com/darh"]
    - ["akirahirose", "https://twitter.com/akirahirose"]
    - ["Anton Strömkvist", "http://lutic.org/"]
    - ["Rahil Momin", "https://github.com/iamrahil"]
    - ["Gregrory Kielian", "https://github.com/gskielian"]
    - ["Etan Reisner", "https://github.com/deryni"]
filename: LearnBash-ms.sh
translators:
    - ["hack1m", "https://github.com/hack1m"]
lang: ms-my   
---

Bash adalah nama daripada unix shell, yang mana telah diagihkan sebagai shell untuk sistem operasi GNU dan sebagai shell lalai pada Linux dan macOS. Hampir semua contoh di bawah boleh menjadi sebahagian daripada skrip shell atau dijalankan terus dalam shell.

[Baca lebih lanjut di sini.](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Baris pertama daripada skrip ialah shebang yang mana memberitahu sistem bagaimana untuk melaksana
# skrip: http://en.wikipedia.org/wiki/Shebang_(Unix)
# Seperti yang anda sudah gambarkan, komen bermula dengan #. Shebang juga ialah komen.

# Contoh mudah hello world:
echo Hello world!

# Setiap arahan bermula pada baris baru, atau selepas semikolon:
echo 'This is the first line'; echo 'This is the second line'

# Mengisytihar pembolehubah kelihatan seperti ini:
Variable="Some string"

# Tetapi bukan seperti ini:
Variable = "Some string"
# Bash akan memutuskan yang pembolehubah adalah arahan ia mesti laksanakan dan memberi ralat
# kerana ia tidak boleh dijumpai.

# Atau seperti ini:
Variable= 'Some string'
# Bash akan memutuskan yang ‘Beberapa rentetan’ adalah arahan ia mesti laksanakan dan memberi
# ralat kerana ia tidak dijumpai. (Dalam kes ini ‘Variable=' sebahagian dilihat
# sebagai penetapan pembolehubah sah hanya untuk skop ‘Beberapa rentetan’
# arahan.)

# Menggunakan pembolehubah:
echo $Variable
echo "$Variable"
echo '$Variable'
# Apabila anda guna pembolehubah itu sendiri - menetapkan, mengeksport, atau lain-lain - anda menulis
# nama ia tanpa $. Atau anda ingin menggunakan nilai pembolehubah, anda mesti guna $.
# Perlu diingatkan ‘(Petikan tunggal) tidak akan memperluaskan pembolehubah!

# Penggantian rentetan dalam pembolehubah
echo ${Variable/Some/A}
# Ini akan menukarkan sebutan pertama bagi "Some" dengan "A"

# Subrentetan daripada pembolehubah
Length=7
echo ${Variable:0:Length}
# Ini akan kembalikan hanya 7 aksara pertama pada nilai

# Nilai lalai untuk pembolehubah
echo ${Foo:-"DefaultValueIfFooIsMissingOrEmpty"}
# Ini berfungsi untuk null (Foo=) dan rentetan kosong (Foo=“”); sifar (Foo=0) kembali 0.
# Perlu diingatkan ia hanya kembalikan nilai lalai dan tidak mengubah nilai pembolehubah.

# Pembolehubah terbina:
# Terdapat beberapa pembolehubah terbina berguna, seperti
echo "Last program's return value: $?"
echo "Script's PID: $$"
echo "Number of arguments passed to script: $#"
echo "All arguments passed to script: $@"
echo "Script's arguments separated into different variables: $1 $2..."

# Membaca nilai dari input:
echo "What's your name?"
read Name # Perlu diingatkan kita tidak perlu isytihar pembolehubah baru
echo Hello, $Name!

# Kita ada yang biasa jika struktur:
# guna 'man test' untuk maklumat lanjut tentang bersyarat
if [ $Name -ne $USER ]
then
    echo "Your name isn't your username"
else
    echo "Your name is your username"
fi

# Terdapat juga pelaksanaan bersyarat
echo "Always executed" || echo "Only executed if first command fails"
echo "Always executed" && echo "Only executed if first command does NOT fail"

# Untuk guna && dan || bersama kenyataan ‘if’, anda perlu beberapa pasang daripada tanda kurung siku:
if [ $Name == "Steve" ] && [ $Age -eq 15 ]
then
    echo "This will run if $Name is Steve AND $Age is 15."
fi

if [ $Name == "Daniya" ] || [ $Name == "Zach" ]
then
    echo "This will run if $Name is Daniya OR Zach."
fi

# Eskspresi ia ditandai dengan format berikut:
echo $(( 10 + 5 ))

# Tidak seperti bahasa pengaturcaraan lain, bash adalah shell jadi ia berfungsi dalam konteks
# daripada direktori semasa. Anda boleh menyenaraikan fail dan direktori dalam direktori
# semasa dengan arahan ini:
ls

# Arahan ini mempunyai opsyen yang mengawal perlaksanaannya:
ls -l # Senarai setiap fail dan direktori pada baris yang berbeza

# Keputusan arahan sebelum boleh diberikan kepada arahan selepas sebagai input.
# arahan grep menapis input dengan memberi paten. Ini bagaimana kita boleh senaraikan
# fail .txt di dalam direktori semasa:
ls -l | grep "\.txt"

# Anda boleh mengubah hala arahan input dan output (stdin, stdout, dan stderr).
# Baca dari stdin sampai ^EOF$ dan menulis ganti hello.py dengan baris
# antara “EOF":
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Jalankan hello.py dengan pelbagai penghantaran semula stdin, stdout, dan stderr:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# Output ralat akan menulis ganti fail jika ia wujud,
# jika anda ingin menambah sebaliknya, guna ‘>>”:
python hello.py >> "output.out" 2>> "error.err"

# Menulis ganti output.out, menambah ke error.err, dan mengira baris:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Jalankan arahan dan cetak fail Deskriptor (e.g. /dev/fd/123)
# lihat: man fd
echo <(echo "#helloworld")

# Menulis ganti output.out dengan “#helloworld":
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Membersihkan fail semantara keseluruhan (tambah ‘-i’ untuk interaktif)
rm -v output.out error.err output-and-error.log

# Arahan boleh digantikan dalam arahan lain menggunakan $():
# Arahan berikut memaparkan jumlah fail dan direktori dalam
# direktori semasa.
echo "There are $(ls | wc -l) items here."

# Perkara yang sama boleh dilakukan dengan menggunakan backticks `` tetapi ia tidak boleh bersarang - cara yang terbaik
# ialah menggunakan $( ).
echo "There are `ls | wc -l` items here."

# Bash menggunakan penyataan case yang berfungsi sama seperti ‘switch’ pada Java dan C++:
case "$Variable" in
    # Senarai paten untuk syarat yang ada ingin ketemui
    0) echo "There is a zero.";;
    1) echo "There is a one.";;
    *) echo "It is not null.";;
esac

# ‘for loops iterate' untuk sebanyak mana argumen yang ditetapkan:
# Kandungan dari $Variable dicetakan sebanyak tiga kali.
for Variable in {1..3}
do
    echo "$Variable"
done

# Atau tulis ia cara "traditional for loop":
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Ia juga boleh digunakan untuk bertindak ke atas fail..
# Ini akan menjalankan arahan 'cat' pada file1 dan file2
for Variable in file1 file2
do
    cat "$Variable"
done

# ..atau output daripada arahan
# Ini akan 'cat' output dari ls.
for Output in $(ls)
do
    cat "$Output"
done

# while loop:
while [ true ]
do
    echo "loop body here..."
    break
done

# Anda juga boleh mendefinasikan fungsi
# Definasi:
function foo ()
{
    echo "Arguments work just like script arguments: $@"
    echo "And: $1 $2..."
    echo "This is a function"
    return 0
}

# atau lebih mudah
bar ()
{
    echo "Another way to declare functions!"
    return 0
}

# Memanggil fungsi
foo "My name is" $Name

# Terdapat banyak arahan yang berguna yang perlu anda belajar:
# cetak 10 baris terakhir dalam file.txt
tail -n 10 file.txt
# cetak 10 baris pertama dalam file.txt
head -n 10 file.txt
# menyusun baris fail.txt
sort file.txt
# laporan atau meninggalkan garisan berulang, dengan -d ia melaporkan
uniq -d file.txt
# cetak hanya kolum pertama sebelum aksara ','
cut -d ',' -f 1 file.txt
# menggantikan setiap kewujudan 'okay' dengan 'great' dalam file.txt, (serasi regex)
sed -i 's/okay/great/g' file.txt
# cetak ke stdoout semua baris dalam file.txt yang mana sepadan beberapa regex
# contoh cetak baris yang mana bermula dengan “foo” dan berakhir dengan “bar”
grep "^foo.*bar$" file.txt
# beri opsyen “-c” untuk sebaliknya mencetak jumlah baris sepadan regex
grep -c "^foo.*bar$" file.txt
# jika anda secara literal mahu untuk mencari rentetan,
# dan bukannya regex, guna fgrep (atau grep -F)
fgrep "^foo.*bar$" file.txt


# Baca dokumentasi Bash shell terbina dengan 'help' terbina:
help
help help
help for
help return
help source
help .

# Baca dokumentasi Bash manpage dengan man
apropos bash
man 1 bash
man bash

# Baca dokumentasi info dengan info (? for help)
apropos info | grep '^info.*('
man info
info info
info 5 info

# Baca dokumentasi bash info:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
