---
category: tool
tool: OpenCV
filename: learnopencv.py
contributors:
    - ["Yogesh Ojha", "http://github.com/yogeshojha"]
    - ["Cillian Smith", "https://github.com/smithc36-tcd"]
---
### Opencv

OpenCV (Open Source Computer Vision) is a library of programming functions mainly aimed at real-time computer vision. 
Originally developed by Intel, it was later supported by Willow Garage then Itseez (which was later acquired by Intel). 
Opencv currently supports wide variety of languages like, C++, Python, Java etc


## Python
#### Python Installation
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

## C++ OpenCV Implementation
#### C++ Installation

```bash
# Install minimal prerequisites (Ubuntu 18.04 as reference)
sudo apt update && sudo apt install -y cmake g++ wget unzip
# Download and unpack sources
wget -O opencv.zip https://github.com/opencv/opencv/archive/4.x.zip
unzip opencv.zip
# Create build directory
mkdir -p build && cd build
# Configure
cmake  ../opencv-4.x
# Build
cmake --build .
```

More detailed installation information can be found below. 
* Linux : [https://docs.opencv.org/4.x/d7/d9f/tutorial_linux_install.html](https://docs.opencv.org/4.x/d7/d9f/tutorial_linux_install.html)
* Windows : [https://docs.opencv.org/4.x/d3/d52/tutorial_windows_install.html](https://docs.opencv.org/4.x/d3/d52/tutorial_windows_install.html)
* MacOS : [https://docs.opencv.org/4.x/d0/db2/tutorial_macos_install.html](https://docs.opencv.org/4.x/d0/db2/tutorial_macos_install.html)

### Opening Images and Video
```cpp
#include "opencv2/core.hpp"
#include "opencv2/imgproc.hpp"
#include "opencv2/video.hpp"

// Using namespaces for simplicity
using namespace std;
using namespace cv;

int main() {

  /* Reading an Image
   * imread(filename, optional-formatting)
   * IMREAD_COLOR, IMREAD_UNCHANGED, IMREAD_GRAYSCALE
   */
  Mat src = cv::imread("sample_image.jpg", IMREAD_COLOR);
  if (src.empty()) {
    std::cerr << "No Image found" << endl;
    return -1;
  }

  // Display an image
  imshow("Title", src);
  // Waits for 1000 miliseconds
  waitKey(1000);
  // waitKey(0) will indefinately for a keypress
  
  // Writing an image to a file
  bool isSuccess = imwrite("sample.jpg", image); //write the image to a file as JPEG 
  //bool isSuccess = imwrite("sample.png", image); //write the image to a file as PNG
  if (!isSuccess)
  {
      std::cerr << "Failed to save the image" << std::endl;
      return -1;
  }

  // loading a video file
  VideoCapture cap("sample_video.mp4");
  if (!cap.isOpened()) {
    std::cerr << "Error opening video file" << std::endl;
    return -1;
  }

  // Capturing from webcam (webcam is indexed from 0, additional cameras
  // can be indexed from 1,2, ...)
  VideoCapture cam(0);

  // Displaying a video or webcam
  while (1) {

    // Create a Mat object to store current frame
    Mat frame;
    // Capture frame-by-frame
    cap >> frame;

    if (frame.empty()) { // Check if video is finished
      break;
    }
    // Display Frame
    imshow("Video", frame);
  }
  // release the video capture object
  cap.release();
 
  // Closes all windows
  destroyAllWindows();
  return 0;
}
```

### Basic Vision operations
```cpp

// Format conversion (RBG, HSV, RGBA, etc.)
Mat src = imread( "Sample.jpg", IMREAD_COLOR);
Mat gray_image;
cvtColor( image, gray_image, CV_BGR2GRAY );
// Other colour codes can be found here 
// https://docs.opencv.org/3.4/d8/d01/group__imgproc__color__conversions.html#ga4e0972be5de079fed4e3a10e24ef5ef0jk

// Thresholding
Mat src_gray = imread("sample.jpg", IMREAD_GREYSCALE);
// max_binary value = 255
// threshold value is pixel value of threshold
// threshold type can be set to THRESH_BINARY, THRESH_BINARY_INV, THRESH_TRUNC,
//    THRESH_TOZERO, THRESH_TOZERO_INV
threshold( src_gray, dst, threshold_value, max_binary_value, threshold_type );


// Blending images
src1 = imread("image1.jpg", IMREAD_COLOR);
src2 = imread("image2.jpg", IMREAD_COLOR);
// where 0 <= aplha <= 1  
float aplha = 0.5, beta = 1 - alpha; 
addWeighted( src1, alpha, src2, beta, 0.0, dest);
imshow( "Blended Image", dest );
waitKey(0);

// Image Smoothing
// Size is the size of the neighbourhood
// Point specifies the center as a anchor
blur( src, dst, Size( i, i ), Point(-1,-1) );
GaussianBlur( src, dst, Size( i, i ), 0, 0 );
// Median only considers square neighbourhoods so an int is passed
medianBlur ( src, dst, i );
// Src, dest, size, std dev in color, std dev in coord
bilateralFilter ( src, dst, i, i*2, i/2 );

// Edge Detection ( Canny )
Mat src_gray, dettected_edges
blur( src_gray, detected_edges, Size(3,3) );
int ratio = 3 // specified by Canny
Canny( detected_edges, detected_edges, lowThreshold, lowThreshold*ratio, kernel_size );

// Drawing Shapes
Mat image = Mat::zeros( w, w, CV_8UC3 ); // Create image of shape width 

  int thickness = 2;
  vec3 lineColor = Scalar(0,0,0) // black
  
  line(image, start_pt, end_pt, Scalar(0, 0, 0), thickness, LINE_8);
  circle(image, center, w/2, Scalar(0, 0, 255), FILLED, LINE_8);
  
  
//  Histogram Backprojection 
// Backproject an image onto a given sample historgram, returns a probability image
Mat backProject_image(Mat image, string sample_string, int bins)
{
    Mat sample = imread(sample_string, IMREAD_COLOR);

    Mat hist;
    int r_bins = bins;
    int g_bins = bins;
    int b_bins = bins;
    int histSize[] = {r_bins, g_bins, b_bins};

    float R_range[] = {0, 255};
    float G_range[] = {0, 255};
    float B_range[] = {0, 255};
    const float *ranges[] = {R_range, G_range, B_range};

    int channels[] = {0, 1, 2};

    calcHist(&sample, 1, channels, Mat(), hist, 3, histSize, ranges, true, false);

    normalize(hist, hist, 0, 255, NORM_MINMAX, -1, Mat());

    Mat backprojected_image;
    calcBackProject(&image, 1, channels, hist, backprojected_image, ranges, 1, true);

    return backprojected_image;
}


```

### More help and documentation
* OpenCV Documentation [https://docs.opencv.org/4.x/index.html](https://docs.opencv.org/4.x/index.html)
* C++ OpenCV Tutorials [https://www.opencv-srf.com/p/introduction.html](https://www.opencv-srf.com/p/introduction.html)
* Learn OpenCV [https://learnopencv.com/getting-started-with-opencv/](https://learnopencv.com/getting-started-with-opencv/)

