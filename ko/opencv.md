---
category: framework
name: OpenCV
filename: learnopencv.py
contributors:
    - ["Yogesh Ojha", "http://github.com/yogeshojha"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

### OpenCV

OpenCV(오픈 소스 컴퓨터 비전)는 주로 실시간 컴퓨터 비전을 목표로 하는 프로그래밍 함수 라이브러리입니다.
원래 인텔에서 개발했으며, 나중에 Willow Garage와 Itseez(나중에 인텔에 인수됨)의 지원을 받았습니다.
OpenCV는 현재 C++, Python, Java 등 다양한 언어를 지원합니다.

#### 설치
컴퓨터에 OpenCV를 설치하려면 다음 문서를 참조하십시오.

* Windows 설치 지침: [https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows](https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows)
* Mac 설치 지침 (High Sierra): [https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a](https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a)
* Linux 설치 지침 (Ubuntu 18.04): [https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv](https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv)

### 여기서는 OpenCV의 Python 구현에 중점을 둘 것입니다

```python
# OpenCV에서 이미지 읽기
import cv2
img = cv2.imread('cat.jpg')

# 이미지 표시
# imshow() 함수는 이미지를 표시하는 데 사용됩니다.
cv2.imshow('Image', img)
# 첫 번째 인수는 창의 제목이고 두 번째 매개변수는 이미지입니다.
# Object Type None 오류가 발생하는 경우 이미지 경로가 잘못되었을 수 있습니다. 이미지 경로를 다시 확인하십시오.
cv2.waitKey(0)
# waitKey()는 키보드 바인딩 함수이며 밀리초 단위로 인수를 받습니다. GUI 이벤트의 경우 waitKey() 함수를 사용해야 합니다.

# 이미지 쓰기
cv2.imwrite('catgray.png', img)
# 첫 번째 인수는 파일 이름이고 두 번째는 이미지입니다.

# 이미지를 회색조로 변환
gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

# 웹캠에서 비디오 캡처
cap = cv2.VideoCapture(0)
# 0은 카메라입니다. 여러 대의 카메라가 있는 경우 해당 ID를 입력해야 합니다.
while True:
    # 프레임별 캡처
    _, frame = cap.read()
    cv2.imshow('Frame', frame)
    # 사용자가 q를 누르면 종료
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
# 카메라를 해제해야 합니다.
cap.release()

# 파일에서 비디오 재생
cap = cv2.VideoCapture('movie.mp4')
while cap.isOpened():
    _, frame = cap.read()
    # 비디오를 회색조로 재생
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
    cv2.imshow('frame', gray)
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break
cap.release()

# OpenCV에서 선 그리기
# cv2.line(img, (x,y), (x1,y1), (color->r,g,b->0 to 255), thickness)
cv2.line(img, (0, 0), (511, 511), (255, 0, 0), 5)

# 사각형 그리기
# cv2.rectangle(img, (x,y), (x1,y1), (color->r,g,b->0 to 255), thickness)
# thickness = -1은 사각형을 채우는 데 사용됩니다.
cv2.rectangle(img, (384, 0), (510, 128), (0, 255, 0), 3)

# 원 그리기
# cv2.circle(img, (xCenter,yCenter), radius, (color->r,g,b->0 to 255), thickness)
cv2.circle(img, (200, 90), 100, (0, 0, 255), -1)

# 타원 그리기
cv2.ellipse(img, (256, 256), (100, 50), 0, 0, 180, 255, -1)

# 이미지에 텍스트 추가
cv2.putText(img, "Hello World!!!", (x, y), cv2.FONT_HERSHEY_SIMPLEX, 2, 255)

# 이미지 블렌딩
img1 = cv2.imread('cat.png')
img2 = cv2.imread('openCV.jpg')
dst = cv2.addWeighted(img1, 0.5, img2, 0.5, 0)

# 이미지 임계 처리
# 이진 임계 처리
_, thresImg = cv2.threshold(img, 127, 255, cv2.THRESH_BINARY)
# 적응형 임계 처리
adapThres = cv2.adaptiveThreshold(img, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 11, 2)

# 이미지 블러
# 가우시안 블러
blur = cv2.GaussianBlur(img, (5, 5), 0)
# 미디언 블러
medianBlur = cv2.medianBlur(img, 5)

# 캐니 에지 검출
img = cv2.imread('cat.jpg', 0)
edges = cv2.Canny(img, 100, 200)

# Haar Cascades를 사용한 얼굴 검출
# https://github.com/opencv/opencv/blob/master/data/haarcascades/에서 Haar Cascades 다운로드
import cv2
import numpy as np

face_cascade = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')
eye_cascade = cv2.CascadeClassifier('haarcascade_eye.xml')

img = cv2.imread('human.jpg')
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

faces = face_cascade.detectMultiScale(gray, 1.3, 5)
for x, y, w, h in faces:
    # 검출된 얼굴 주위에 사각형 그리기
    cv2.rectangle(img, (x, y), (x + w, y + h), (255, 0, 0), 2)
    roi_gray = gray[y : y + h, x : x + w]
    roi_color = img[y : y + h, x : x + w]
    eyes = eye_cascade.detectMultiScale(roi_gray)
    for ex, ey, ew, eh in eyes:
        # 검출된 눈 주위에 사각형 그리기
        cv2.rectangle(roi_color, (ex, ey), (ex + ew, ey + eh), (0, 255, 0), 2)

cv2.imshow('img', img)
cv2.waitKey(0)

cv2.destroyAllWindows()
# destroyAllWindows()는 모든 창을 닫습니다.
# 특정 창을 닫으려면 생성한 창의 정확한 이름을 전달하십시오.
```

### 더 읽을거리:

* Cascade 다운로드 [https://github.com/opencv/opencv/blob/master/data/haarcascades](https://github.com/opencv/opencv/blob/master/data/haarcascades)
* OpenCV 그리기 함수 [https://docs.opencv.org/2.4/modules/core/doc/drawing_functions.html](https://docs.opencv.org/2.4/modules/core/doc/drawing_functions.html)
* 최신 언어 참조는 [https://opencv.org](https://opencv.org)에서 찾을 수 있습니다.
* 추가 자료는 [https://en.wikipedia.org/wiki/OpenCV](https://en.wikipedia.org/wiki/OpenCV)에서 찾을 수 있습니다.
* 좋은 OpenCV 튜토리얼
    * [https://realpython.com/python-opencv-color-spaces](https://realpython.com/python-opencv-color-spaces)
    * [https://pyimagesearch.com](https://pyimagesearch.com)
    * [https://www.learnopencv.com](https://www.learnopencv.com)
