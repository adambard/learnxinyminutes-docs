---
category: tool
tool: OpenCV
filename: learnopencv-de.py
contributors:
    - ["Yogesh Ojha", "http://github.com/yogeshojha"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---
### Opencv

OpenCV (Open Source Computer Vision) ist eine Bibliothek von Programmierfunktionen, 
die hauptsächlich auf maschinelles Sehen in Echtzeit ausgerichtet ist.
Ursprünglich wurde OpenCV von Intel entwickelt. Später wurde es von von 
Willow Garage und dann Itseez (das später von Intel übernommen wurde) unterstützt.
OpenCV unterstützt derzeit eine Vielzahl von Sprachen, wie C++, Python, Java uvm.

#### Installation

Bitte lese diese Artikel für die Installation von OpenCV auf deinen Computer.

* Windows Installationsanleitung: [https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows]()
* Mac Installationsanleitung (High Sierra): [https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a]()
* Linux Installationsanleitung (Ubuntu 18.04): [https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv]()

### Hier werden wir uns auf die Pythonimplementierung von OpenCV konzentrieren.

```python
# Bild in OpenCV lesen
import cv2
img = cv2.imread('Katze.jpg')

# Bild darstellen
# Die imshow() Funktion wird verwendet um das Display darzustellen.
cv2.imshow('Image',img)
# Das erste Argument ist der Titel des Fensters und der zweite Parameter ist das Bild
# Wenn du den Fehler Object Type None bekommst ist eventuell dein Bildpfad falsch.
# Bitte überprüfe dann den Pfad des Bildes erneut.
cv2.waitKey(0)
# waitKey() ist eine Tastaturbindungsfunktion, sie nimmt Argumente in 
# Millisekunden an. Für GUI Ereignisse MUSST du die waitKey() Funktion verwenden.

# Ein Bild schreiben
cv2.imwrite('graueKatze.png',img)
# Das erste Arkument ist der Dateiname und das Zweite ist das Bild

# Konveriere das Bild zu Graustufen
gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

# Videoaufnahme von der Webcam
cap = cv2.VideoCapture(0)
# 0 ist deine Kamera, wenn du mehrere Kameras hast musst du deren Id eingeben
while(True):
    # Erfassen von Einzelbildern
    _, frame = cap.read()
    cv2.imshow('Frame',frame)
    # Wenn der Benutzer q drückt -> beenden
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
# Die Kamera muss wieder freigegeben werden
cap.release()

# Wiedergabe von Videos aus einer Datei
cap = cv2.VideoCapture('film.mp4')
while(cap.isOpened()):
    _, frame = cap.read()
    # Das Video in Graustufen abspielen
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    cv2.imshow('frame',gray)
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
cap.release()

# Zeichne eine Linie in OpenCV
# cv2.line(img,(x,y),(x1,y1),(color->r,g,b->0 to 255),thickness)
cv2.line(img,(0,0),(511,511),(255,0,0),5)

# Zeichne ein Rechteck
# cv2.rectangle(img,(x,y),(x1,y1),(color->r,g,b->0 to 255),thickness)
# thickness = -1 wird zum Füllen des Rechtecks verwendet
cv2.rectangle(img,(384,0),(510,128),(0,255,0),3)

# Zeichne ein Kreis
cv2.circle(img,(xCenter,yCenter), radius, (color->r,g,b->0 to 255), thickness)
cv2.circle(img,(200,90), 100, (0,0,255), -1)

# Zeichne eine Ellipse
cv2.ellipse(img,(256,256),(100,50),0,0,180,255,-1)

# Text auf Bildern hinzufügen
cv2.putText(img,"Hello World!!!", (x,y), cv2.FONT_HERSHEY_SIMPLEX, 2, 255)

# Bilder zusammenfüggen
img1 = cv2.imread('Katze.png')
img2 = cv2.imread('openCV.jpg')
dst = cv2.addWeighted(img1,0.5,img2,0.5,0)

# Schwellwertbild
# Binäre Schwellenwerte
_,thresImg = cv2.threshold(img,127,255,cv2.THRESH_BINARY)
# Anpassbare Schwellenwerte 
adapThres = cv2.adaptiveThreshold(img,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY,11,2)

# Weichzeichnung von einem Bild
# Gausßscher Weichzeichner
blur = cv2.GaussianBlur(img,(5,5),0)
# Rangordnungsfilter
medianBlur = cv2.medianBlur(img,5)

# Canny-Algorithmus
img = cv2.imread('Katze.jpg',0)
edges = cv2.Canny(img,100,200)

# Gesichtserkennung mit Haarkaskaden
# Lade die Haarkaskaden von https://github.com/opencv/opencv/blob/master/data/haarcascades/ herunter
import cv2
import numpy as np
face_cascade = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')
eye_cascade = cv2.CascadeClassifier('haarcascade_eye.xml')

img = cv2.imread('Mensch.jpg')
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

aces = face_cascade.detectMultiScale(gray, 1.3, 5)
for (x,y,w,h) in faces:
    cv2.rectangle(img,(x,y),(x+w,y+h),(255,0,0),2)
    roi_gray = gray[y:y+h, x:x+w]
    roi_color = img[y:y+h, x:x+w]
    eyes = eye_cascade.detectMultiScale(roi_gray)
    for (ex,ey,ew,eh) in eyes:
        cv2.rectangle(roi_color,(ex,ey),(ex+ew,ey+eh),(0,255,0),2)

cv2.imshow('img',img)
cv2.waitKey(0)

cv2.destroyAllWindows()
# destroyAllWindows() zerstört alle Fenster
# Wenn du ein bestimmtes Fenter zerstören möchtest musst du den genauen Namen des 
# von dir erstellten Fensters übergeben.
```

### Weiterführende Literatur:
* Lade Kaskade hier herunter [https://github.com/opencv/opencv/blob/master/data/haarcascades]()
* OpenCV Zeichenfunktionen [https://docs.opencv.org/2.4/modules/core/doc/drawing_functions.html]()
* Eine aktuelle Sprachenreferenz kann hier gefunden werden [https://opencv.org]()
* Zusätzliche Ressourcen können hier gefunden werden [https://en.wikipedia.org/wiki/OpenCV]()
* Gute OpenCV Tutorials
    * [https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_tutorials.html]()
    * [https://realpython.com/python-opencv-color-spaces]()
    * [https://pyimagesearch.com]()
    * [https://www.learnopencv.com]()
    * [https://docs.opencv.org/master/]()
