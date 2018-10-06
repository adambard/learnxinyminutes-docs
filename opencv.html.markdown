---
language: c++/python
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
* Windows Installation Instructions: 
<https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_setup/py_setup_in_windows/py_setup_in_windows.html#install-opencv-python-in-windows>

* Mac Installation Instructions (High Sierra):
https://medium.com/@nuwanprabhath/installing-opencv-in-macos-high-sierra-for-python-3-89c79f0a246a

* Linux Installation Instructions (Ubuntu 18.04):
<https://www.pyimagesearch.com/2018/05/28/ubuntu-18-04-how-to-install-opencv/>

### Here we will be focusing on python implementation of OpenCV

* __Reading image in OpenCV__
```
import cv2
img = cv2.imread('cat.jpg')
# Simple isn't it?
# Displaying the image
# imshow() function is used to display the image
cv2.imshow('Image',img)
# Your first arguement is the title of the window and second parameter is image
# If you are getting error, Object Type None, your image path may be wrong. Please recheck the pack to the image
cv2.waitKey(0)
# waitKey() is a keyboard binding function and takes arguement in milliseconds. For GUI events you MUST use waitKey() function.
```

Further Reading:

An up-to-date language reference can be found at
<https://opencv.org/>

Additional resources may be found at 
<https://en.wikipedia.org/wiki/OpenCV>

Good OpenCv Tutorials
<https://opencv-python-tutroals.readthedocs.io/en/latest/py_tutorials/py_tutorials.html>
<https://realpython.com/python-opencv-color-spaces/>
<https://pyimagesearch.com>
<https://www.learnopencv.com>
