---
category: tool
tool: OpenCV
filename: learnopencv.py
contributors:
    - ["Yogesh Ojha", "http://github.com/yogeshojha"]
---
### Opencv

OpenCV (Open Source Computer Vision) is a library of programming functions mainly aimed at real-time computer vision. 
Originally developed by Intel, it was later supported by Willow Garage then Itseez (which was later acquired by Intel). 
Opencv currently supports wide variety of languages like, C++, Python, Java etc

#### Installation
Please refer to these articles for installation of OpenCV on your computer.

* Windows Installation Instructions: [https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows](https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows)
* Mac Installation Instructions (High Sierra): [https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a](https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a)
* Linux Installation Instructions (Ubuntu 18.04): [https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv](https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv)

### Here we will be focusing on python implementation of OpenCV

```python
# Reading image in OpenCV
import cv2
img = cv2.imread('cat.jpg')

# Displaying the image
# imshow() function is used to display the image
cv2.imshow('Image',img)
# Your first arguement is the title of the window and second parameter is image
# If you are getting error, Object Type None, your image path may be wrong. Please recheck the pack to the image
cv2.waitKey(0)
# waitKey() is a keyboard binding function and takes arguement in milliseconds. For GUI events you MUST use waitKey() function.

# Writing an image
cv2.imwrite('catgray.png',img)
# first arguement is the file name and second is the image

# Convert image to grayscale
gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

# Capturing Video from Webcam
cap = cv2.VideoCapture(0)
#0 is your camera, if you have multiple camera, you need to enter their id
while(True):
    # Capturing frame-by-frame
    _, frame = cap.read()
    cv2.imshow('Frame',frame)
    # When user presses q -> quit
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
# Camera must be released
cap.release()

# Playing Video from file
cap = cv2.VideoCapture('movie.mp4')
while(cap.isOpened()):
    _, frame = cap.read()
    # Play the video in grayscale
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    cv2.imshow('frame',gray)
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
cap.release()

# Drawing The Line in OpenCV
# cv2.line(img,(x,y),(x1,y1),(color->r,g,b->0 to 255),thickness)
cv2.line(img,(0,0),(511,511),(255,0,0),5)

# Drawing Rectangle
# cv2.rectangle(img,(x,y),(x1,y1),(color->r,g,b->0 to 255),thickness)
# thickness = -1 used for filling the rectangle
cv2.rectangle(img,(384,0),(510,128),(0,255,0),3)

# Drawing Circle
cv2.circle(img,(xCenter,yCenter), radius, (color->r,g,b->0 to 255), thickness)
cv2.circle(img,(200,90), 100, (0,0,255), -1)

# Drawing Ellipse
cv2.ellipse(img,(256,256),(100,50),0,0,180,255,-1)

# Adding Text On Images
cv2.putText(img,"Hello World!!!", (x,y), cv2.FONT_HERSHEY_SIMPLEX, 2, 255)

# Blending Images
img1 = cv2.imread('cat.png')
img2 = cv2.imread('openCV.jpg')
dst = cv2.addWeighted(img1,0.5,img2,0.5,0)

# Thresholding image
# Binary Thresholding
_,thresImg = cv2.threshold(img,127,255,cv2.THRESH_BINARY)
# Adaptive Thresholding
adapThres = cv2.adaptiveThreshold(img,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY,11,2)

# Blur Image
# Gaussian Blur
blur = cv2.GaussianBlur(img,(5,5),0)
# Median Blur
medianBlur = cv2.medianBlur(img,5)

# Canny Edge Detection
img = cv2.imread('cat.jpg',0)
edges = cv2.Canny(img,100,200)

# Face Detection using Haar Cascades
# Download Haar Cascades from https://github.com/opencv/opencv/blob/master/data/haarcascades/
import cv2
import numpy as np
face_cascade = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')
eye_cascade = cv2.CascadeClassifier('haarcascade_eye.xml')

img = cv2.imread('human.jpg')
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

faces = face_cascade.detectMultiScale(gray, 1.3, 5)
for (x,y,w,h) in faces:
    # Draw a rectangle around detected face
    cv2.rectangle(img,(x,y),(x+w,y+h),(255,0,0),2)
    roi_gray = gray[y:y+h, x:x+w]
    roi_color = img[y:y+h, x:x+w]
    eyes = eye_cascade.detectMultiScale(roi_gray)
    for (ex,ey,ew,eh) in eyes:
        # Draw a rectangle around detected eyes
        cv2.rectangle(roi_color,(ex,ey),(ex+ew,ey+eh),(0,255,0),2)

cv2.imshow('img',img)
cv2.waitKey(0)

cv2.destroyAllWindows()
# destroyAllWindows() destroys all windows. 
# If you wish to destroy specific window pass the exact name of window you created.
```

### Further Reading:

* Download Cascade from [https://github.com/opencv/opencv/blob/master/data/haarcascades](https://github.com/opencv/opencv/blob/master/data/haarcascades)
* OpenCV drawing Functions [https://docs.opencv.org/2.4/modules/core/doc/drawing_functions.html](https://docs.opencv.org/2.4/modules/core/doc/drawing_functions.html)
* An up-to-date language reference can be found at [https://opencv.org](https://opencv.org)
* Additional resources may be found at [https://en.wikipedia.org/wiki/OpenCV](https://en.wikipedia.org/wiki/OpenCV)
* Good OpenCv Tutorials
    * [https://realpython.com/python-opencv-color-spaces](https://realpython.com/python-opencv-color-spaces)
    * [https://pyimagesearch.com](https://pyimagesearch.com)
    * [https://www.learnopencv.com](https://www.learnopencv.com)
