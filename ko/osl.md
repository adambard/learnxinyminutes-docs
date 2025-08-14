---
name: OSL
filename: learnosl.osl
contributors:
  - ["Preetham Pemmasani", "https://github.com/Preetham-ai"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

OSL(Open Shading Language)은 Sony가 Arnold Renderer용으로 설계한 프로그래밍 언어로 셰이더를 만드는 데 사용됩니다.

[여기에서 더 읽어보십시오.](https://raw.githubusercontent.com/imageworks/OpenShadingLanguage/master/src/doc/osl-languagespec.pdf)

```c
// 한 줄 주석은 //로 시작합니다.

/* 여러 줄 주석은 유지됩니다. */

// 문장은 ;로 끝낼 수 있습니다.
divide(1,2);

///////////////
// 1. 기본 //
///////////////

// 변수 선언
color Blue; // 변수 초기화
int _num = 3;
float Num = 3.00;
float c[3] = {0.1, 0.2, 3.14}; // 배열

// 수학은 예상대로 작동합니다.
3 + 1;   // 4
74 - 3;   // 71
20 * 2; // 40
75/3;  // 25.0

// 그리고 모듈로 나눗셈은 정수에서만 작동합니다.
10 % 2; // 0
31 % 4; // 1

// 비트 연산은 정수에서만 작동합니다.
- 0 // 1 (단항 부정)
~ 00100011 // 11011100 (비트 보수)
1 << 2; // 4 (왼쪽 시프트)
12 >> 1; // 3 (오른쪽 시프트)
1 & 0; // 0 (비트 AND)
1 | 0; // 1 (비트 OR)
1 ^ 1; // 0 (비트 XOR)

// 부울도 있습니다.
true;
false;

// 부울은 정수와 비교할 수 없습니다.
true == 1 // 오류
false == 0 // 오류

// 부정은 ! 기호를 사용합니다.
!0; // 1
!1; // 0
!2; // 0
//...등등

// 관계 연산자는 다음과 같이 정의됩니다:
0 == 0 // true (같음)
0 != 1 // true (같지 않음)
5 < 3 // false (미만)
3 <= 3 // true (이하)
69 > 69 // false (초과)
99 >= 52 // true (이상)


// 함수는 C 및 C++와 동일합니다.
float sum(float a, float b){
	return a+b;
}

int subtract(int a, int b){
	return a-b;
}

sum(2,3); // 5

////////////////
// 2. 셰이더 //
////////////////

// 셰이더는 재료 및 조명의 사용자 정의 동작을 설명합니다.
// 셰이더의 구문은 C의 main 함수와 유사합니다.
// 입력 및 출력은 기본 유형으로 초기화되어야 합니다.
shader multiply(float a = 0.0,
				float b = 0.0,
				output float c = 0.0){
    c = a*b;
}

// 이중 대괄호[[ ]]는 셰이더의 메타데이터를 분류하는 데 사용됩니다.
surface plastic
	[[ string help = "사실적인 나무 셰이더" ]]
(
	color Plastic = color (0.7, 0.5, 0.3) [[ string help = "기본 색상" ]],
	float Reflectivity = 0.5 [[ float min = 0, float max = 1 ]],
){...}

// 셰이더 유형은 다양합니다.

/* 표면 셰이더는 표면의 기본 재료 속성과 빛에 대한 반응 방식을 결정합니다. */
// 조명 셰이더는 발광 객체에 사용되는 SURFACE 셰이더 유형입니다.
// 변위 셰이더는 위치와 법선을 사용하여 지오메트리를 변경합니다.
// 볼륨 셰이더는 공기/연기/먼지와 같은 매체를 장면에 추가합니다.

volume multiply(float a = 0.0, float b = 0.0, output float c = 0.0){
    c = 2*a+b;
}

////////////////////////////////////////
// 3. 데이터 유형 및 전역 변수 //
////////////////////////////////////////

// 데이터 유형

// 1. void 유형은 값을 반환하지 않는 함수를 나타냅니다.

// 2. int (정수)
	int x = -12; // 최소 크기 32비트
	int new2 = 0x01cf; // 16진수도 지정할 수 있습니다.

	///////////////////////////////////////
	// 평가 순서
	///////////////////////////////////////

	// 위에서 아래로, 위가 우선 순위가 높습니다.
	//--------------------------//
	//        연산자         //
	//--------------------------//
	// int++, int--             //
	// ++ int --int - ~ !       //
	// * / %                    //
	// + -                      //
	// << >>                    //
	// < <= > >=                //
	// == !=                    //
	// &
	// ^                        //
	// |
	// &&                       //
	// ||                       //
	// ?:
	// = += -= *= /=            //
	//--------------------------//

// 3. float (부동 소수점 숫자)
	int A = 2.3; // 최소 IEEE 32비트 float
	int Z = -4.1e2; // Z = -4.1 * 10^2

	// 평가 순서는 int와 유사합니다.
	// ( ~ ! % << >> ^ | & && || )와 같은 연산은 float에서 사용할 수 없습니다.

// 4. string
	// 구문은 C와 유사합니다.
	string new = "Hello World";
	// 일부 특수 문자:
	/*
	'"'; // 큰따옴표
	'\n'; // 줄 바꿈 문자
	'\t'; // 탭 문자(텍스트 왼쪽 정렬)
	'\v'; // 수직 탭
	'\\'; // 백슬래시
	'\r'; // 캐리지 리턴
	'\b'; // 백스페이스 문자
	*/

	// 문자열은 공백으로 연결됩니다.
	"Hello " "world!"; // "Hello world!"
	// concat 함수도 사용할 수 있습니다.
	string concat ("Hello ","World!"); // "Hello world!"

	// printf 함수는 C와 동일합니다.
	int i = 18;
	printf("I am %d years old",i); // 저는 18살입니다.

	// 문자열 함수도 사용할 수 있습니다.
	int strlen (string s); // 문자열의 길이를 반환합니다.
	int len = strlen("Hello, World!"); // len = 13

	// startswith는 문자열이 접두사로 시작하면 1을 반환하고, 그렇지 않으면 0을 반환합니다.
	int starts = startswith("The quick brown fox", "The"); // starts = 1

	// endswith는 문자열이 접미사로 시작하면 1을 반환하고, 그렇지 않으면 0을 반환합니다.
	int ends = endswith("The quick brown fox", "fox"); // ends는 1이 됩니다.

// 5. color (빨강, 초록, 파랑)
	color p = color(0,1,2); // 검정
	color q = color(1); // 흰색 (color(1,1,1)과 동일)
	color r = color("rgb", 0.23, 0.1, 0.8); // RGB로 명시적으로 지정
	color s = color("hsv", 0.23, 0.1, 0.8); // HSV로 지정
	// HSV는 (색조, 채도, 광도)를 의미합니다.
	// HSL은 (색조, 채도, 밝기)를 의미합니다.
	// YIQ, XYZ 및 xyY 형식도 사용할 수 있습니다.
	// (R,G,B)의 개별 값에도 액세스할 수 있습니다.
	float Red = p[0]; // 0 (빨간색 구성 요소에 액세스)
	float Green = p[1]; // 1 (녹색 구성 요소에 액세스)
	float Blue = p[2]; // 2 (파란색 구성 요소에 액세스)

	// 다음과 같이 액세스할 수도 있습니다.
	float Red = p.r; // 0 (빨간색 구성 요소에 액세스)
	float Green = p.g; // 1 (녹색 구성 요소에 액세스)
	float Blue = p.b; // 2 (파란색 구성 요소에 액세스)

	// 수학 연산자는 우선 순위가 감소하는 순서로 다음과 같이 작동합니다.
	color C = (3,2,3) * (1,0,0); // (3, 0, 0)
	color D = (1,1,1) * 255; // (255, 255, 255)
	color E = (25,5,125) / 5; // (5, 1, 25)
	color F = (30,40,50) / (3,4,5); // (10, 10, 10)
	color A = (1,2,3) + (1,0,0); // (2, 2, 3)
	color B = (1,2,3) - (1,0,0); // (0, 2, 3)
	// ( - == != )와 같은 연산자도 사용됩니다.

// 6. point (x,y,z)는 3D 공간에서 점의 위치입니다.
// 7. vector (x,y,z)는 길이와 방향을 가지지만 위치는 없습니다.
// 8. normal (x,y,z)은 표면에 수직인 특수 벡터입니다.
	// 이러한 연산자는 색상과 동일하며 동일한 우선 순위를 가집니다.
	L = point(0.5, 0.6, 0.7);
	M = vector(30, 100, 70);
	N = normal(0, 0, 1);

	// 이 3가지 유형은 좌표계에 할당될 수 있습니다.
	L = point("object", 0.5, 0.6, 0.7); // 로컬 공간에 상대적
	M = vector("common", 30, 100, 70); // 월드 공간에 상대적
	// ("shader", "world", "camera", "screen", "raster", "NDC")도 있습니다.

	float x = L[0]; // 0.5 (x-구성 요소에 액세스)
	float y = L[1]; // 0.6 (y-구성 요소에 액세스)
	float z = L[2]; // 0.7 (z-구성 요소에 액세스)

	// 다음과 같이 액세스할 수도 있습니다.
	float x = M.x; // 30 (x-구성 요소에 액세스)
	float y = M.y; // 100 (y-구성 요소에 액세스)
	float z = M.z; // 70 (z-구성 요소에 액세스)

	float a = dot ((1,2,3), (1,2,3)); // 14 (점곱)
	vector b = cross ((1,2,3), (1,2,3)); // (0,0,0) (외적)
	float l = length(L); // 1.085 (벡터의 길이)
	vector normalize (vector L); // (0.460, 0.552, 0.644) 벡터 정규화

	point p0 = point(1, 2, 3);
	point p1 = point(4, 5, 6);
	point Q = point(0, 0, 0);

	// 두 점 사이의 거리 찾기
	float len = distance(point(1, 2, 3), point(4, 5, 6)); // 5.196
	// Q에서 P0과 P1을 연결하는 선까지의 수직 거리
	float distance (point P0, point P1, point Q); // 2.45


// 9. matrix
	// 다른 좌표계 간에 벡터를 변환하는 데 사용됩니다.
	// 일반적으로 4x4 (또는) 16개의 float입니다.
	matrix zero = 0; // 4x4 영 행렬을 만듭니다.
	/* 0.0, 0.0, 0.0, 0.0,
       0.0, 0.0, 0.0, 0.0,
       0.0, 0.0, 0.0, 0.0,
       0.0, 0.0, 0.0, 0.0 */

	matrix ident = 1; // 4x4 항등 행렬을 만듭니다.
	/* 1.0, 0.0, 0.0, 0.0,
	   0.0, 1.0, 0.0, 0.0,
	   0.0, 0.0, 1.0, 0.0,
	   0.0, 0.0, 0.0, 1.0 */

	matrix m = 7; // 7의 스케일링 인자를 가진 4x4 스칼라 행렬을 만듭니다.
	/* 7.0, 0.0, 0.0, 0.0,
	   0.0, 7.0, 0.0, 0.0,
	   0.0, 0.0, 7.0, 0.0,
	   0.0, 0.0, 0.0, 7.0 */

	float x = m[1][1]; // 7

	// 행렬은 행 우선 순서로 float를 사용하여 구성할 수 있습니다.
	// 행렬은 일반적으로 16개의 요소가 있는 4x4입니다.
	matrix myMatrix = matrix(1.0, 0.0, 0.0, 0.0,    // 1행
                             0.0, 2.0, 0.0, 0.0,    // 2행
                             0.0, 0.0, 3.0, 0.0,    // 3행
                             0.0, 0.0, 0.0, 4.0); // 4행

	// 행렬 변환은 구현하기 쉽습니다.
	matrix a = matrix ("shader", 1); // 셰이더를 공통으로 변환
	matrix m = matrix ("object", "world"); // 객체를 월드로 변환

	// 우선 순위가 감소하는 순서로 사용할 수 있는 연산자는 다음과 같습니다:
	// ( - * / == !=)

	float determinant (matrix M) // 24 (행렬의 행렬식 반환)
	float transpose (matrix M) // 행렬의 전치 반환
	/* 1.0, 0.0, 0.0, 0.0,
       0.0, 2.0, 0.0, 0.0,
       0.0, 0.0, 3.0, 0.0,
       0.0, 0.0, 0.0, 4.0 */

// 10. array
	// OSL의 배열은 C와 유사합니다.
	float a[5]; // 크기 5의 배열 a 초기화
	int b[3] = {90,80,70}; // 크기 3의 배열 선언
	int len = arraylength(b); // 3
	int f = b[1]; // 80
	float anotherarray[3] = b; // 배열은 동일한 유형인 경우 복사할 수 있습니다.

// 11. struct (구조체)
	// OSL의 구조체는 C 및 C++와 유사합니다.
	struct RGBA { // 구조체 정의
		color rgb;
		float alpha;
	};


	RGBA col; // 구조체 선언
	RGBA b = { color(0.1, 0.2, 0.3), 1 }; // 다음과 같이 선언할 수도 있습니다.

	r.rgb = color (1, 0, 0); // 한 필드에 할당
	color c = r.rgb; // 구조체 필드에서 읽기

// 12. closure
	// 클로저는 실행 시 고려되지 않는 데이터를 저장하는 데 사용됩니다.
	// 조작하거나 읽을 수 없습니다.
	// null 클로저는 항상 할당될 수 있습니다.
	// OSL은 현재 색상만 클로저로 지원합니다.

	// 클로저의 몇 가지 예는 다음과 같습니다:

	// 확산 BSDF 클로저:
	closure color oren_nayar_diffuse_bsdf(normal N, color alb, float roughness)
	closure color burley_diffuse_bsdf(normal N, color alb, float roughness);

	// 유전체 BSDF 클로저:
	closure color dielectric_bsdf(normal N, vector U, color reflection_tint,
	    color transmission_tint, float roughness_x, float roughness_y,
	    float ior, string distribution);

	// 도체 BSDF 클로저:
	closure color conductor_bsdf(normal N, vector U, float roughness_x,
	    float roughness_y, color ior, color extinction, string distribution);

	// 일반화된 Schlick BSDF 클로저:
	closure color generalized_schlick_bsdf(normal N, vector U,
	    color reflection_tint, color transmission_tint,
	    float roughness_x, float roughness_y, color f0, color f90,
	    float exponent, string distribution);

	// 반투명 BSDF 클로저:
	closure color translucent_bsdf(normal N, color albedo);

	// 투명 BSDF 클로저:
	closure color transparent_bsdf();

	// 지하 BSSRDF 클로저:
	closure color subsurface_bssrdf();

	// 광택 BSDF 클로저:
	closure color sheen_bsdf(normal N, color albedo, float roughness);

	// 비등방성 VDF 클로저: (볼륨)
	closure color anisotropic_vdf(color albedo, color extinction,
	    float anisotropy);

	// 중간 VDF 클로저: (볼륨)
	closure color medium_vdf(color albedo, float transmission_depth,
	    color transmission_color, float anisotropy, float ior, int priority);

	closure color uniform edf(color emittance); // 방출 클로저
	closure color holdout(); // 홀드아웃은 그 아래의 객체를 숨깁니다.

	// BSDF는 이 클로저를 사용하여 계층화할 수 있습니다.
	closure color layer (closure color top, closure color base);



// 전역 변수
// 렌더러가 아는 정보를 포함합니다.
// 이러한 변수는 선언할 필요가 없습니다.

point P // 음영 처리 중인 점의 위치
vector I // 보기 위치에서 음영 처리 위치까지의 입사 광선 방향
normal N // P 지점의 표면 법선
normal Ng // 범프 매핑과 관계없이 P 지점의 표면 법선
float u // UV 2D x - 지오메트리의 매개변수 좌표
float v // UV 2D y - 지오메트리의 매개변수 좌표
vector dPdu // 표면에 접하는 u에 대한 P의 변화
vector dPdv // 표면에 접하는 v에 대한 P의 변화
float time // 현재 시간
float dtime // 커버된 시간
vector dPdtime // 시간에 대한 P의 변화

/////////////////////
// 4. 제어 흐름 //
/////////////////////

// OSL의 조건문은 C 또는 C++와 동일합니다.

// If/Else
if (5>2){
	int x = s;
	int l = x;
}
else{
	int x = s + l;
}

// 'while' 루프
int i = 0;
while (i < 5) {
    i += 1;
    printf("Current value of i: %d\n", i);
}

// 'do-while' 루프는 루프 본문 뒤에 테스트가 오는 곳입니다.
int i = 0;
do {
    printf("Current value of i: %d\n", i);
    i += 1;
} while (i < 5);

// 'for' 루프
for (int i = 0; i < 5; i += 1) {
    printf("Current value of i: %d\n", i);
}

/////////////////////
// 5. 함수 //
/////////////////////

// 수학 상수
	M_PI // π
	M_PI_35 // π/35
	m_E // e
	M_LN2 // ln 2
	M_SQRT2 // √2
	M_SQRT1_2 // √(1/2)

// 지오메트리 함수
	vector N = vector(0.1, 1, 0.2); // 법선 벡터
	vector I = vector(-0.5, 0.2, 0.8); // 입사 벡터

	// Faceforward는 벡터의 방향을 알려줍니다.
	vector facing_dir = faceforward(N, I); // facing_dir = (-0.5, 0.2, 0.8)

	// 3개의 인수를 가진 faceforward
	vector ref = vector(0.3, -0.7, 0.6); // 참조 법선
	facing_dir = faceforward(N, I, ref); // facing_dir = (0.5, -0.2, -0.8)

	// reflect는 법선을 따라 반사된 벡터를 반환합니다.
	vector refl = reflect(I, N); // refl = (-0.7, -0.4, 1.4)

	// refract는 법선을 따라 굴절된 벡터를 반환합니다.
	float ior = 1.5; // 굴절률
	vector refr = refract(I, N, ior); // refr = (-0.25861, 0.32814, 0.96143)

	/* Fresnel은 반사(R) 및 투과(T) 벡터와 함께
	반사(Kr) 및 투과(Kt)된 빛의 스케일링 인자를 계산합니다. */
	float Kr, Kt;
	vector R, T;
	fresnel(I, N, ior, Kr, Kt, R, T);
/* Kr = 0.03958, Kt = 0.96042
	R = (-0.19278, -0.07711, 0.33854)
	T = (-0.25861, 0.32814, 0.96143) */

	// 주어진 축을 따라 점을 회전
	point Q = point(1, 0, 0);
	float angle = radians(90); // 90도
	vector axis = vector(0, 0, 1);
	point rotated_point = rotate(Q, angle, axis);
	// rotated_point = point(0, 1, 0)

	// 2개의 점으로 이루어진 선을 따라 점을 회전
	point P0 = point(0, 0, 0);
	point P1 = point(1, 1, 0);
	angle = radians(45); // 45도
	Q = point(1, 0, 0);
	rotated_point = rotate(Q, angle, P0, P1);
	// rotated_point = point(0.707107, 0.707107, 0)

	// 점 p에서 표면의 법선 계산
	point p1 = point(1, 0, 0); // 반지름 1의 구에 있는 점
	vector normal1 = calculatenormal(p1);
	// normal1 = vector(1, 0, 0)

	// 단위 변환은 쉽습니다.
	float transformu ("cm", float x) // cm로 변환
	float transformu ("cm", "m", float y) // cm를 m로 변환

// 변위 함수
	void displace (float 5); // 5 amp 단위로 변위
	void bump (float 10); // 10 amp 단위로 범프


// 노이즈 생성

type noise (type noise (string noisetype, float u, float v, ...)); // 노이즈
	type noise (string noisetype, point p,...); // 좌표 대신 점
	/* 일부 노이즈는 ("perlin", "snoise", "uperlin", "noise", "cell", "hash"
	"simplex", "usimplex", "gabor" 등) */

	// 노이즈 이름

	// 1. Perlin Noise (perlin, snoise):
	// 텍스처에 자주 사용되는 부드럽고 소용돌이치는 노이즈를 생성합니다.
	// 범위: [-1, 1] (부호 있는)
	color cloud_texture = noise("perlin", P);

	// 2. Simplex Noise (simplex, usimplex):
	// Perlin 노이즈와 유사하지만 더 빠릅니다.
	// 범위: [-1, 1] (simplex의 경우 부호 있는), [0, 1] (usimplex의 경우 부호 없는)
	float bump_amount = 0.2 * noise("simplex", P * 5.0);

	// 3. UPerlin Noise (uperlin, noise):
	// Perlin과 유사
	// 범위: [0, 1] (부호 없는)
	color new_texture = noise("uperlin", P);

	// 4. Cell Noise (cell):
	// 각 단위 블록 내에서 블록형, 셀룰러 및 상수 값을 생성합니다.
	// 범위: [0, 1] (부호 없는)
	color new_texture = noise("cell", P);

	// 5. Hash Noise (hash):
	// 각 지점에서 무작위, 비상관 값을 생성합니다.
	// 범위: [0, 1] (부호 없는)
	color new_texture = noise("hash", P);

	// Gabor Noise (gabor)
	// Gabor Noise는 Perlin 노이즈의 고급 버전이며 더 많은 제어를 제공합니다.
	// 범위: [-1, 1] (부호 있는)
	// Gabor Noise 매개변수

	// 비등방성 (기본값: 0)
	// 비등방성 제어:
	// 0: 등방성 (모든 방향에서 동일한 주파수)
	// 1: 사용자 정의 방향 벡터를 가진 비등방성 (기본값: (1,0,0))
	/* 2: 하이브리드 모드, 방향 벡터를 따라 비등방성이지만
	수직으로 방사형 등방성. */

	// 방향 (기본값: (1,0,0))
	// 비등방성 방향 지정 (anisotropic이 1인 경우에만 사용).

	// 대역폭 (기본값: 1.0)
	// 노이즈의 주파수 범위 제어.

	// 임펄스 (기본값: 16)
	// 셀당 사용되는 임펄스 수를 제어하여 세부 수준에 영향을 미칩니다.

	// do_filter (기본값: 1)
	// 안티앨리어싱(필터링) 활성화/비활성화.

	result = noise(
        "gabor",
        P,
        "anisotropic", anisotropic,
        "direction", direction,
        "bandwidth", bandwidth,
        "impulses", impulses,
        "do_filter", do_filter
    );

	// 특정 노이즈는 유형으로 전달하는 대신 사용할 수도 있습니다.
	// pnoise는 주기적 노이즈입니다.
	float n1 = pnoise("perlin", 0.5, 1.0);
	// Gabor 유형의 2D 주기적 노이즈
	float n2 = pnoise("gabor", 0.2, 0.3, 2.0, 3.0);
	// 2D 비주기적 심플렉스 노이즈
	float n3 = snoise(0.1, 0.7);
	// 2D 주기적 심플렉스 노이즈
	type psnoise (float u, float v, float uperiod, float vperiod);
	float n4 = psnoise(0.4, 0.6, 0.5, 0.25);
	// 2D 셀룰러 노이즈
	float n5 = cellnoise(0.2, 0.8);
	// 2D 해시 노이즈
	int n6 = hash(0.7, 0.3);

// 단계 함수
	// 단계 함수는 입력과 임계값을 비교하는 데 사용됩니다.

	// 유형은 float, color, point, vector 또는 normal일 수 있습니다.
	type step (type edge, type x); // x >= edge이면 1을 반환하고, 그렇지 않으면 0을 반환합니다.
	color checker = step(0.5, P);  // P는 표면의 점입니다.
	/* P 값이 0.5 미만인 픽셀은 검은색이고, 0.5 이상인 픽셀은 흰색입니다. */
	float visibility = step(10, distance(P, light_position));
	// 빛은 10단위 내에서 완전히 보이고, 그 이상에서는 완전히 보이지 않습니다.

	type linearstep (type edge0, type edge1, type x); /* linearstep은 x <= edge0이면 0을 반환하고, x >= edge1이면 1을 반환하며, 선형 보간을 사용합니다. */
	color gradient = linearstep(0, 1, P);
	// P는 0과 1 사이의 표면의 점입니다.
	// P가 0에서 1로 이동함에 따라 색상이 검정에서 흰색으로 부드럽게 변합니다.
	float fade = linearstep(0.85, 1, N.z);  // N.z는 z-구성 요소입니다.
	// 법선이 수직에 가까운 객체 가장자리(N.z가 1에 가까움)는 페이드 아웃됩니다.

	type smoothstep (type edge0, type edge1, type x); /* smoothstep은 x <= edge0이면 0을 반환하고, x >= edge1이면 1을 반환하며, Hermite 보간을 사용합니다. */
	float soft_mask = smoothstep(0.2, 0.8, noise(P));  /* noise(P)는 0과 1 사이의 노이즈 값입니다. soft_mask는 noise(P)를 기반으로 0과 1 사이에서 부드럽게 변하며, linearstep보다 부드러운 곡선을 가집니다. */

// 스플라인
	// 스플라인은 제어점 집합을 기반으로 하는 부드러운 곡선입니다.

	/* 보간 유형은 "catmull-rom", "bezier", "bspline", "hermite", "linear" 또는 "constant"입니다. */

	// 매듭 벡터를 가진 스플라인
	float[] knots = {0, 0, 0, 0.25, 0.5, 0.75, 1, 1, 1};
	point[] controls = {point(0),point(1, 2, 1),point(2, 1, 2),point(3, 3, 1)};
	spline curve1 = spline("bezier", 0.5, len(knots), controls);
	// curve1은 u = 0.5에서 평가된 Bezier 스플라인입니다.

	// 제어점을 가진 스플라인
	spline curve2 = spline("catmull-rom", 0.25, point(0, 0, 0), point(1, 2, 1),
	                       point(2, 1, 2), point(3, 3, 1));
	// curve2는 u = 0.25에서 평가된 Catmull-Rom 스플라인입니다.

	// 단일 float 값을 가진 상수 스플라인
	float value = 10;
	u = 0.1;
	spline curve5 = spline("constant", u, value);
	// curve5는 u = 0.1에서 평가된 값 10을 가진 상수 스플라인입니다.

	// 점 및 벡터 제어점을 가진 Hermite 스플라인
	point q0 = point(0, 0, 0), q1 = point(3, 3, 3);
	vector t0 = vector(1, 0, 0), t1 = vector(-1, 1, 1);
	u = 0.75;
	spline curve3 = spline("hermite", u, q0, t0, q1, t1);
	// curve3는 u = 0.75에서 평가된 Hermite 스플라인입니다.

	// float 제어점을 가진 선형 스플라인
	float f0 = 0, f1 = 1, f2 = 2, f3 = 3;
	u = 0.4;
	spline curve4 = spline("linear", u, f0, f1, f2, f3);
	// curve4는 u = 0.4에서 평가된 선형 스플라인입니다.

	// 역 스플라인도 존재합니다.

	// 제어 값을 가진 역 스플라인
	float y0 = 0, y1 = 1, y2 = 2, y3 = 3;
	float v = 1.5;
	float u1 = splineinverse("linear", v, y0, y1, y2, y3);
	// u1 = 0.5 (y1과 y2 사이의 선형 보간)

	// 매듭 벡터를 가진 역 스플라인
	float[] knots = {0, 0, 0, 0.25, 0.5, 0.75, 1, 1, 1};
	float[] values = {0, 1, 4, 9};
	v = 6;
	float u2 = splineinverse("bezier", v, len(knots), values);
	// u2 = 0.75 (v = 6에서 평가된 Bezier 스플라인 역)

	// 상수 값을 가진 역 스플라인
	v = 10;
	float u3 = splineinverse("constant", v, 10);
	// u3 = 0 (상수 스플라인은 항상 10을 반환하므로)

	// 주기적 값을 가진 역 스플라인
	float y4 = 0, y5 = 1, y6 = 0;
	v = 0.5;
	float u4 = splineinverse("periodic", v, y4, y5, y6);
	// u4 = 0.75 (v = 0.5에서 평가된 주기적 스플라인 역)


// 미적분 연산자
	// x, y, z에 대한 f의 편미분은 Dx, Dy, Dz를 사용합니다.
	float a = 3.14;
	float dx = Dx(a); // x에 대한 a의 편미분

	point p = point(1.0, 2.0, 3.0);
	vector dp_dx = Dx(p); // x에 대한 p의 편미분

	vector dv_dy = Dy(N); // y에 대한 법선의 편미분

	color c = color(0.5, 0.2, 0.8);
	color dc_dz = Dz(c); // z에 대한 c의 편미분


	float area (point p) // p 위치의 표면적을 반환합니다.

	float filterwidth (float x) // 인접 샘플에서 x의 변화를 반환합니다.

// 텍스처 함수
	// 좌표 (x,y)에서 텍스처를 조회합니다.
	color col1 = texture("texture.png", 0.5, 0.2);
	// texture.png에서 (0.5, 0.2)의 색상 조회

	// 좌표 (x,y)에서 텍스처의 3D 조회
	color col3 = texture3d("texture3d.vdb", point(0.25, 0.5, 0.75));

	// 매개변수는 ("blur","width","wrap","fill","alpha","interp", ...)입니다.
	color col2 = texture("texture.png",1.0,0.75,"blur",0.1,"wrap", "periodic");
	// 0.1의 블러와 주기적 랩 모드로 (1.0, 0.75)의 색상 조회

// 조명 함수

	float surfacearea (); // 조명이 덮는 표면적을 반환합니다.
	int backfacing (); // 법선이 뒷면을 향하면 1을 출력하고, 그렇지 않으면 0을 출력합니다.
	int raytype (string name); // 광선이 특정 광선 유형이면 1을 반환합니다.

	// 위치에서 방향으로 광선 추적
	point pos = point(0, 0, 0); // 광선의 시작 위치
	vector dir = vector(0, 0, 1); // 광선의 방향
	int hit = trace(pos, dir); // 맞으면 1을 반환하고, 그렇지 않으면 0을 반환합니다.
```

### 더 읽을거리

* OSL에 대한 Blender 문서 [https://docs.blender.org/manual/en/latest/render/shader_nodes/osl.html](https://docs.blender.org/manual/en/latest/render/shader_nodes/osl.html)
* OSL에 대한 C4D 문서 [https://docs.otoy.com/cinema4d//OpenShadingLanguageOSL.html](https://docs.otoy.com/cinema4d//OpenShadingLanguageOSL.html)
* [GitHub](https://github.com/AcademySoftwareFoundation/OpenShadingLanguage)의 Open Shading Language
* [공식 OSL 문서](https://open-shading-language.readthedocs.io/en/main/)

```