---
name: Processing
filename: learnprocessing.pde
contributors:
    - ["Phone Than Ko", "http://github.com/phonethantko"]
    - ["Divay Prakash", "https://github.com/divayprakash"]
---

## 소개

Processing은 디지털 아트 및 멀티미디어 콘텐츠 제작을 위한 프로그래밍 언어로, 비프로그래머가 시각적 맥락에서 컴퓨터 프로그래밍의 기초를 배울 수 있도록 합니다.

이 언어는 Java 언어를 기반으로 하지만, 구문은 Java와 JavaScript 구문 모두에 크게 영향을 받았습니다. [자세히 보기](https://processing.org/reference/)

이 언어는 정적으로 유형이 지정되며, 스크립트를 컴파일하고 실행하기 위한 공식 IDE도 함께 제공됩니다.

```
/* ---------
   주석
   ---------
*/

// 한 줄 주석은 //로 시작합니다.

/*
   Processing은 Java를 기반으로 하므로,
   주석 구문은 Java와 동일합니다(위에서 보셨듯이)!
   여러 줄 주석은 여기에서 볼 수 있듯이 래핑됩니다.
*/

/* ---------------------------------------
   Processing 프로그램 작성 및 실행
   ---------------------------------------
*/

// Processing에서 프로그램 진입점은 void 반환 유형을 가진 setup()이라는 함수입니다.
// 참고! 구문은 C++와 매우 유사합니다.
void setup() {
  // 실행 시 콘솔에 고전적인 "Hello World!" 출력을 인쇄합니다.
  println("Hello World!"); // 세미콜론 함정이 있는 또 다른 언어죠?
}

// 일반적으로 이름에서 알 수 있듯이 한 번만 실행되므로 setup() 메서드 내에
// 모든 정적 코드를 넣습니다.
// 배경색 설정, 캔버스 크기 설정 등이 포함될 수 있습니다.
background(color); // 배경색 설정
size(width,height,[renderer]); // 선택적 렌더러 정의 매개변수로 캔버스 크기 설정
// 이 문서 전체에서 더 많이 보게 될 것입니다.

// 코드를 무기한 실행하려면 draw() 메서드에 배치해야 합니다.
// 코드를 계속 실행하려면 draw()가 있어야 하며, 물론
// draw() 메서드는 하나만 있을 수 있습니다.
int i = 0;
void draw() {
  // 이 코드 블록은 중지될 때까지 영원히 반복됩니다.
  print(i);
  i++; // 증감 연산자!
}

// 이제 작동하는 스크립트를 작성하고 실행하는 방법을 알았으므로,
// Processing에서 지원하는 데이터 유형 및 컬렉션을 살펴보겠습니다.

/* ------------------------
   데이터 유형 및 컬렉션
   ------------------------
*/

// Processing 참조에 따르면 Processing은 다음과 같이 8개의 기본 데이터 유형을 지원합니다.

boolean booleanValue = true; // 불리언
byte byteValueOfA = 23; // 바이트
char charValueOfA = 'A'; // 문자
color colourValueOfWhiteM = color(255, 255, 255); // 색상 (color() 메서드 사용)
color colourValueOfWhiteH = #FFFFFF; // 색상 (해시 값 사용)
int intValue = 5; // 정수 (소수점 없는 숫자)
long longValue = 2147483648L; // "L"은 긴 숫자로 표시하기 위해 숫자에 추가됩니다.
float floatValue = 1.12345; // 부동 소수점 (32비트 부동 소수점 숫자)
double doubleValue = 1.12345D; // 더블 (64비트 부동 소수점 숫자)

// 참고!
// 데이터 유형 "long"과 "double"은 언어에서 작동하지만,
// processing 함수는 이러한 데이터 유형을 사용하지 않으므로
// 함수에 전달하기 전에 각각 (int) 및 (float) 구문을 사용하여
// "int" 및 "float" 데이터 유형으로 변환해야 합니다.

// Processing에서는 사용할 수 있는 기본 복합 데이터 유형이 많이 있습니다.
// 주로 시간을 절약하기 위해 가장 일반적으로 사용되는 것들을 간략하게 살펴보겠습니다.

// 문자열
// char 데이터 유형은 ''를 사용하는 반면, String 데이터 유형은 ""(큰따옴표)를 사용합니다.
String sampleString = "Hello, Processing!";
// 문자열은 char 데이터 유형의 배열로도 구성할 수 있습니다. 곧
// 배열에 대해 논의할 것입니다.
char source = {'H', 'E', 'L', 'L', 'O'};
String stringFromSource = new String(source); // HELLO
// Java에서와 같이 문자열은 "+" 연산자를 사용하여 연결할 수 있습니다.
print("Hello " + "World!"); // Hello World!

// 배열
// Processing의 배열은 객체 자체를 포함하여 모든 데이터 유형을 담을 수 있습니다.
// 배열은 객체와 유사하므로 "new" 키워드로 생성해야 합니다.
int[] intArray = new int[5];
int[] intArrayWithValues = {1, 2, 3}; // 데이터로 채울 수도 있습니다.

// ArrayList
// 함수는 배열과 유사합니다. arraylist는 모든 데이터 유형을 담을 수 있습니다.
// 유일한 차이점은 arraylist가 Java "List" 인터페이스의
// 크기 조정 가능한 배열 구현 형태이므로 동적으로 크기가 조정된다는 것입니다.
ArrayList<Integer> intArrayList = new ArrayList<Integer>();

// 객체
// Java를 기반으로 하므로 Processing은 객체 지향 프로그래밍을 지원합니다.
// 즉, 기본적으로 자신만의 데이터 유형을 정의하고
// 필요에 맞게 조작할 수 있습니다.
// 물론 원하는 객체에 대해 클래스를 미리 정의해야 합니다.
// 형식 --> ClassName InstanceName
SomeRandomClass myObject // 나중에 인스턴스화
//또는
SomeRandomClass myObjectInstantiated = new SomeRandomClass();

// Processing은 기본적으로 더 많은 컬렉션(예: 딕셔너리 및 리스트)을
// 제공하지만, 단순화를 위해 여기서는 논의하지 않겠습니다.

/* ------------
   수학
   ------------
*/

// 산술
1 + 1 // 2
2 - 1 // 1
2 * 3 // 6
3 / 2 // 1
3.0 / 2 // 1.5
3.0 % 2 // 1.0

// Processing에는 수학 연산을 단순화하는 함수 세트도 함께 제공됩니다.
float f = sq(3); // f = 9.0
float p = pow(3, 3); // p = 27.0
int a = abs(-13); // a = 13
int r1 = round(3.1); // r1 = 3
int r2 = round(3.7); // r2 = 4
float sr = sqrt(25); // sr = 5.0

// 벡터
// Processing은 PVector 클래스를 사용하여 환경에서 벡터를 쉽게 구현할 수 있는
// 방법을 제공합니다. 2차원 또는 3차원 벡터를 설명할 수 있으며
// 행렬 연산에 유용한 메서드 세트가 함께 제공됩니다.
// PVector 클래스 및 해당 함수에 대한 자세한 정보는 여기에서 찾을 수 있습니다.
// (https://processing.org/reference/PVector.html)

// 삼각법
// Processing은 함수 세트를 제공하여 삼각 연산도 지원합니다.
// sin(), cos(), tan(), asin(), acos(), atan() 및 편리한 변환을 위한
// degrees() 및 radians()도 있습니다.
// 그러나 이러한 함수는 매개변수로 라디안 단위의 각도를 사용하므로
// 미리 변환해야 합니다.
float one = sin(PI/2); // one = 1.0
// 눈치채셨겠지만, 삼각법 사용을 위한 상수 세트가 있습니다.
// PI, HALF_PI, QUARTER_PI 등...

/* -------------
   제어 흐름
   -------------
*/

// 조건문
// If 문 - Java의 if 문과 동일한 구문입니다.
if (author.getAppearance().equals("hot")) {
  print("Narcissism at its best!");
} else {
  // 여기에서 다른 조건을 확인할 수 있습니다.
  print("Something is really wrong here!");
}
// if-else 문의 바로 가기도 사용할 수 있습니다.
int i = 3;
String value = (i > 5) ? "Big" : "Small"; // "Small"

// Switch-case 구조는 여러 조건을 간결하게 확인하는 데 사용할 수 있습니다.
// break 문을 사용하는 것이 중요합니다. `break` 문이 없으면
// 프로그램은 case가 true인 후 모든 다음 case를 실행합니다.
int value = 2;
switch(value) {
  case 0:
    print("Nought!"); // 이것은 실행되지 않습니다.
    break; // 다음 문으로 점프
  case 1:
    print("Getting there..."); // 이것도 실행되지 않습니다.
    break;
  case 2:
    print("Bravo!"); // 이 줄이 실행됩니다.
    break;
  default:
    print("Not found!"); // 이 줄은 우리 값이 다른 값일 경우 실행됩니다.
    break;
}

// 반복문
// For 문 - 다시 말하지만, Java와 동일한 구문입니다.
for(int i = 0; i < 5; i++){
  print(i); // 0부터 4까지 인쇄
}

// While 문 - 다시 말하지만, Java 구문에 익숙하다면 새로운 것이 없습니다.
int j = 3;
while(j > 0) {
  print(j);
  j--; // 코드가 무한정 실행되는 것을 방지하기 위해 이것이 중요합니다.
}

// loop()| noLoop() | redraw() | exit()
// 이것들은 프로그램 흐름을 구성하기 위한 Processing 관련 함수입니다.
loop(); // draw() 메서드를 영원히 실행하도록 허용하는 반면
noLoop(); // 한 번만 실행하도록 허용합니다.
redraw(); // draw() 메서드를 한 번 더 실행합니다.
exit(); // 프로그램을 중지합니다. draw()가 계속 실행되는
// 프로그램에 유용합니다.
```

## Processing으로 그리기

이제 언어의 기본을 이해했으므로 Processing의 가장 좋은 부분인 그리기를 살펴보겠습니다.

```
/* ------
   도형
   ------
*/

// 2D 도형

// 점
point(x, y); // 2D 공간에서
point(x, y, z); // 3D 공간에서
// 좌표 공간에 점을 그립니다.

// 선
line(x1, y1, x2, y2); // 2D 공간에서
line(x1, y1, z1, x2, y2, z2); // 3D 공간에서
// (x1, y1)과 (x2, y2)로 정의된 두 점을 연결하는 선을 그립니다.

// 삼각형
triangle(x1, y1, x2, y2, x3, y3);
// 좌표 매개변수로 정의된 세 점을 연결하는 삼각형을 그립니다.

// 사각형
rect(a, b, c, d, [r]); // 모든 모서리의 반지름을 정의하는 선택적 매개변수
rect(a, b, c, d, [tl, tr, br, bl]); // 각 모서리의 반지름을 정의하는
// 선택적 매개변수 세트
// {a, b}를 왼쪽 위 좌표로 하고 c와 d를 너비와
// 높이로 하는 사각형을 그립니다.

// 사변형
quad(x, y, x2, y2, x3, y3, x4, y4);
// 각 모서리 점의 좌표를 정의하는 매개변수로 사변형을
// 그립니다.

// 타원
ellipse(x, y, width, height);
// 점 {x, y}에 너비와 높이가 지정된 타원을 그립니다.

// 호
arc(x, y, width, height, start, stop, [mode]);
// 처음 네 개의 매개변수는 자명하지만,
// start와 end는 호가 시작하고 끝나는 각도(라디안)를 정의합니다.
// 선택적 매개변수 [mode]는 채우기를 정의합니다.
// PIE는 파이 모양의 윤곽선을, CHORD는 현 모양의 윤곽선을, OPEN은
// 획 없는 CHORD를 제공합니다.

// 곡선
// Processing은 curve()와 bezier()를 사용하는 두 가지 곡선 구현을 제공합니다.
// 간단하게 유지하기 위해 더 이상 자세한 내용은 다루지 않겠습니다.
// 그러나 스케치에 구현하려면 다음 참조를 확인하십시오:
// (https://processing.org/reference/curve_.html)
// (https://processing.org/reference/bezier_.html)

// 3D 도형

// 3D 공간은 size() 메서드의 렌더러 매개변수에 "P3D"를 설정하여
// 구성할 수 있습니다.
size(width, height, P3D);
// 3D 공간에서는 3D 도형을 렌더링하기 위해 특정 좌표로
// 변환해야 합니다.

// 상자
box(size);  // 크기로 정의된 동일한 길이의 정육면체
box(w, h, d); // 너비, 높이 및 깊이가 별도로 정의된 상자

// 구
sphere(radius); // 크기는 반지름 매개변수를 사용하여 정의됩니다.
// 구 렌더링 메커니즘은 삼각형 테셀레이션으로 구현됩니다.
// 즉, 렌더링되는 세부 정보의 양은 sphereDetail(res) 함수로
// 제어됩니다.
// 자세한 정보는 여기: (https://processing.org/reference/sphereDetail_.html)

// 불규칙한 도형
// Processing 함수에서 제공하지 않는 것을 그리고 싶다면 어떻게 해야 할까요?
// beginShape(), endShape(), vertex(x,y)를 사용하여 각 점을
// 지정하여 도형을 정의할 수 있습니다. 자세한 정보는 여기:
// (https://processing.org/reference/beginShape_.html)
// PShape 클래스를 사용하여 사용자 정의 도형을 사용할 수도 있습니다:
// (https://processing.org/reference/PShape.html)

/* ---------------
   변환
   ---------------
*/

// 변환은 그린 도형의 좌표 공간과 정점을 추적하는 데
// 특히 유용합니다. 특히;
// 행렬 스택 메서드; pushMatrix(), popMatrix() 및 translate(x,y)
pushMatrix(); // 현재 좌표계를 스택에 저장
// ... 여기에 모든 변환 적용 ...
popMatrix(); // 저장된 좌표계 복원
// 이를 사용하면 충돌 없이 좌표계를 보존하고 시각화할 수 있습니다.

// 이동
translate(x, y); // 점 {x, y}로 이동, 즉 - 원점을 해당 점으로 설정
translate(x, y, z); // 함수의 3D 대응

// 회전
rotate(angle); // angle 매개변수로 지정된 양만큼 회전
// 회전을 수행하기 위한 3개의 3D 대응이 있으며, 각 차원에 대해
// 하나씩 있습니다. 즉: rotateX(angle), rotateY(angle), rotateZ(angle)

// 크기 조정
scale(s); // 좌표계를 확장하거나 축소하여 크기를 조정합니다.

/* --------------------
   스타일 및 텍스처
   --------------------
*/

// 색상
// 앞에서 설명했듯이 배경색은 background() 함수를 사용하여
// 구성할 수 있습니다. 미리 색상 객체를 정의한 다음
// 함수에 인수로 전달할 수 있습니다.
color c = color(255, 255, 255); // 흰색!
// 기본적으로 Processing은 RGB 색상 구성표를 사용하지만 colorMode()를 사용하여
// HSB로 구성할 수 있습니다. 자세한 내용은 여기:
// (https://processing.org/reference/colorMode_.html)
background(c); // 이제 배경색이 흰색이어야 합니다.
// fill() 함수를 사용하여 도형을 채울 색상을 선택할 수 있습니다.
// 색상이 적용되도록 도형을 그리기 전에 구성해야 합니다.
fill(color(0, 0, 0));
// 도형의 윤곽선만 색칠하려면 stroke() 함수를 사용할 수 있습니다.
stroke(255, 255, 0, 200); // 획 색상을 노란색으로 설정하고 투명도를
// 낮은 값으로 설정합니다.

// 이미지
// Processing은 이미지를 렌더링하고 여러 가지 방법으로 사용할 수 있습니다. 대부분
// PImage 데이터 유형으로 저장됩니다.
filter(shader); // Processing은 이미지 조작을 위한 여러 필터 함수를 지원합니다.
texture(image); // PImage는 도형의 텍스처 매핑을 위한 인수로 전달될 수 있습니다.
```

더 나아가고 싶다면 Processing이 지원하는 더 많은 기능이 있습니다.
모델, 셰이더 등을 렌더링합니다. 짧은 문서에서 다루기에는 너무 많으므로
여기서는 생략하겠습니다. 관심이 있다면 참조를 확인하십시오.

```
// 계속하기 전에 라이브러리를 가져오는 방법에 대해 조금 더 다루겠습니다.
// Processing 기능을 다른 차원으로 확장할 수 있습니다.

/* -------
   가져오기
   -------
*/

// Processing의 힘은 라이브러리와 패키지를 스케치로
// 가져올 때 더욱 시각화될 수 있습니다.
// 가져오기 문은 소스 코드 상단에 아래와 같이 작성할 수 있습니다.
import processing.something.*;
```

## DTC?

코딩할 준비가 되셨나요? 손을 더럽혀 봅시다!

openprocessing의 예제를 통해 Processing이 몇 줄의 코드로 얼마나 많은 것을
할 수 있는지 시각화해 보겠습니다.

아래 코드를 Processing IDE에 복사하여 마법을 확인하십시오.

```
// 면책 조항: 저는 현재 인턴십으로 바쁘기 때문에 이 프로그램을 작성하지 않았으며
// 이 스케치는 간단한 코드로 멋진 것을 보여주기 때문에 openprocessing에서
// 가져왔습니다.
// 출처: (https://www.openprocessing.org/sketch/559769)

float theta;
float a;
float col;
float num;

void setup() {
  size(600,600);
}

void draw() {
  background(#F2F2F2);
  translate(width/2, height/2);
  theta = map(sin(millis()/1000.0), -1, 1, 0, PI/6);

  float num=6;
  for (int i=0; i<num; i++) {
    a =350;
    rotate(TWO_PI/num);
    branch(a);
  }

}

void branch(float len) {
  col=map(len, 0, 90, 150, 255);
  fill(col, 0, 74);
  stroke (col, 0, 74);
  line(0, 0, 0, -len);
  ellipse(0, -len, 3, 3);
  len *= 0.7;

  if (len>30) {
    pushMatrix();
    translate(0, -30);
    rotate(theta);
    branch(len);
    popMatrix();

    pushMatrix();
    translate(0, -30);
    rotate(-theta);
    branch(len);
    popMatrix();

  }
}
```

Processing은 배우기 쉽고 특히 많은 코드를 입력하지 않고도 멀티미디어
콘텐츠(3D 포함)를 만드는 데 유용합니다. 코드를 읽고 프로그램 흐름에 대한
대략적인 아이디어를 얻을 수 있을 정도로 간단합니다.

그러나 외부 라이브러리, 패키지 및 자신만의 클래스를 도입하면
적용되지 않습니다. (저를 믿으세요! Processing 프로젝트는 정말 거대해질 수 있습니다...)

## 유용한 자료

 - [Processing 웹사이트](http://processing.org)
 - [Processing 스케치](http://openprocessing.org)
