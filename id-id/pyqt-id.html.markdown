---
category: tool
tool: PyQt
language: Python
filename: learnqt-id.py
contributors:
    - ["Nathan Hughes", "https://github.com/sirsharpest"]
translators:
    - ["Rizky Luthfianto", "http://github.com/rilut"]
lang: id-id
---

**Qt** adalah framework terkenal untuk pengembangan perangkat lunak *cross-platform* yang dapat dijalankan pada berbagai platform perangkat lunak dan perangkat keras dengan sedikit atau tanpa perubahan dalam kode, dengan tetap memiliki kekuatan dan kecepatan aplikasi *native*. **Qt** ditulis dalam bahasa C++.


Tulisan ini diadaptasi dari **Intro Qt untuk C++** oleh [Aleksey Kholovchuk](https://github.com/vortexxx192). Kode-kode yang tertulis di sini akan menghasilkan fungsionalitas yang sama. Bedanya, versi ini dibangun menggunakan **PyQt**!

```Python
import sys
from PyQt4 import QtGui
	
def window():
# Buat objek aplikasi
    app = QtGui.QApplication(sys.argv)
# Buat sebuah widget, sebagai tempat di mana label kita akan ditempatkan
    w = QtGui.QWidget()
# Tambahkan label untuk widget
    b = QtGui.QLabel(w)
# Set teks untuk label
    b.setText("Halo, Dunia!")
# Set parameter penempatan dan ukuran
    w.setGeometry(100, 100, 200, 50)
    b.move(50, 20)
# Set judul pada jendela
    w.setWindowTitle("PyQt")
# Tampilkan segalanya
    w.show()
# Jalankan apa yang telah kita atur. Setelah semua selesai kita atur.
    sys.exit(app.exec_())

if __name__ == '__main__':
    window()
```

Untuk menunjukkan beberapa fitur yang lebih canggih di **PyQt**, kita akan membangun elemen tambahan.
Di sini, kita akan membuat Kotak Popup Dialog, yang berguna untuk meminta pengguna untuk mengkonfirmasi keputusan atau untuk menampilkan informasi.

```Python 
import sys
from PyQt4.QtGui import *
from PyQt4.QtCore import *


def window():
    app = QApplication(sys.argv)
    w = QWidget()
    # Buat tombol b dan tempelkan pada widget w
    b = QPushButton(w)
    b.setText("Tekan aku!")
    b.move(50, 50)
    # Perintahkan tombol b untuk memanggil fungsi ini ketika diklik
    # Perhatikan bahwa kita tidak menggunakan simbol "()" pada pemanggilan fungsi kali ini
    b.clicked.connect(ShowDialog)
    w.setWindowTitle("Dialog PyQt")
    w.show()
    sys.exit(app.exec_())

# Fungsi ini akan membuat jendela dialog dengan tombol
# yang menunggu untuk diklik untuk keluar dari program
def ShowDialog():
    d = QDialog()
    b1 = QPushButton("ok", d)
    b1.move(50, 50)
    d.setWindowTitle("Dialog")
    # Modalitas ini memberitahu popup untuk memblokir induk saat ini aktif
    d.setWindowModality(Qt.ApplicationModal)
    # Pada klik, kita ingin seluruh proses untuk berhenti
    b1.clicked.connect(sys.exit)
    d.exec_()

if __name__ == '__main__':
    window()
```
