---
category: tool
tool: OpenCV
filename: learnopencv.py
contributors:
    - ["Yogesh Ojha", "http://github.com/yogeshojha"]
translators:
    - ["GengchenXU", "https://github.com/GengchenXU"]
lang: zh-cn
---
### Opencv

Opencv（开源计算机视觉）是一个编程功能库，主要面向实时计算机视觉。最初由英特尔开发，后来由Willow Garage，然后Itseez（后来被英特尔收购）支持。Opencv 目前支持多种语言，如C++、Python、Java 等

#### 安装
有关在计算机上安装 OpenCV，请参阅这些文章。

* Windows 安装说明： [https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows]()
* Mac 安装说明 (High Sierra): [https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a]()
* Linux 安装说明 （Ubuntu 18.04): [https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv]()

### 在这里，我们将专注于 OpenCV 的 python 实现

```python
# OpenCV读取图片
import cv2
img = cv2.imread('cat.jpg')

# 显示图片
# imshow() 函数被用来显示图片
cv2.imshow('Image',img)
# 第一个参数是窗口的标题，第二个参数是image
# 如果你得到错误，对象类型为None，你的图像路径可能是错误的。请重新检查图像包
cv2.waitKey(0)
# waitKey() 是一个键盘绑定函数，参数以毫秒为单位。对于GUI事件，必须使用waitKey()函数。

# 保存图片
cv2.imwrite('catgray.png',img)
# 第一个参数是文件名，第二个参数是图像

# 转换图像灰度
gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

# 从摄像头捕捉视频
cap = cv2.VideoCapture(0)
#0 是你的相机，如果你有多台相机，你需要输入他们的id
while(True):
    # 一帧一帧地获取
    _, frame = cap.read()
    cv2.imshow('Frame',frame)
    # 当用户按下q ->退出
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
# 相机必须释放
cap.release()

# 在文件中播放视频
cap = cv2.VideoCapture('movie.mp4')
while(cap.isOpened()):
    _, frame = cap.read()
    # 灰度播放视频
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    cv2.imshow('frame',gray)
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
cap.release()

# 在OpenCV中画线
# cv2.line(img,(x,y),(x1,y1),(color->r,g,b->0 to 255),thickness)(注 color颜色rgb参数 thickness粗细)
cv2.line(img,(0,0),(511,511),(255,0,0),5)

# 画矩形
# cv2.rectangle(img,(x,y),(x1,y1),(color->r,g,b->0 to 255),thickness)
# 粗细= -1用于填充矩形
cv2.rectangle(img,(384,0),(510,128),(0,255,0),3)

# 画圆
cv2.circle(img,(xCenter,yCenter), radius, (color->r,g,b->0 to 255), thickness)
cv2.circle(img,(200,90), 100, (0,0,255), -1)

# 画椭圆
cv2.ellipse(img,(256,256),(100,50),0,0,180,255,-1)

# 在图像上增加文字
cv2.putText(img,"Hello World!!!", (x,y), cv2.FONT_HERSHEY_SIMPLEX, 2, 255)

# 合成图像
img1 = cv2.imread('cat.png')
img2 = cv2.imread('openCV.jpg')
dst = cv2.addWeighted(img1,0.5,img2,0.5,0)

# 阈值图像
# 二进制阈值
_,thresImg = cv2.threshold(img,127,255,cv2.THRESH_BINARY)
# Adaptive Thresholding
adapThres = cv2.adaptiveThreshold(img,255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY,11,2)

# 模糊的形象
# 高斯模糊
blur = cv2.GaussianBlur(img,(5,5),0)
# 模糊中值
medianBlur = cv2.medianBlur(img,5)

# Canny 边缘检测
img = cv2.imread('cat.jpg',0)
edges = cv2.Canny(img,100,200)

# 用Haar Cascades进行人脸检测
# 下载 Haar Cascades 在 https://github.com/opencv/opencv/blob/master/data/haarcascades/
import cv2
import numpy as np
face_cascade = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')
eye_cascade = cv2.CascadeClassifier('haarcascade_eye.xml')

img = cv2.imread('human.jpg')
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
# destroyAllWindows() destroys all windows. 
# 如果您希望销毁特定窗口，请传递您创建的窗口的确切名称。
```

### 进一步阅读：

* Download Cascade from [https://github.com/opencv/opencv/blob/master/data/haarcascades]()
* OpenCV 绘图函数 [https://docs.opencv.org/2.4/modules/core/doc/drawing_functions.html]()
* 最新的语言参考 [https://opencv.org]()
* 更多的资源 [https://en.wikipedia.org/wiki/OpenCV]()
* 优秀的的 OpenCV 教程
    * [https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_tutorials.html]()
    * [https://realpython.com/python-opencv-color-spaces]()
    * [https://pyimagesearch.com]()
    * [https://www.learnopencv.com]()
