---
name: OpenSCAD
filename: learnopenscad.scad
contributors:
    - ["Tom Preston", "https://github.com/tompreston/"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

[OpenSCAD](https://openscad.org/)로 코드를 사용하여 3D 모델을 그립니다.

```openscad
// 주석은 이렇습니다.

// 3D 기본 요소
cube(10);
cube([5, 10, 20]);
sphere(10);

// 변환
translate([20, 0, 0]) cube(10);
rotate([0, 20, 30]) cube(10);

translate([20, 0, 0]) rotate([0, 20, 30]) cube(10);
rotate([0, 20, 30]) translate([20, 0, 0]) cube(10);

// 수정자
//
//     * 비활성화
//     ! 표시만
//     # 강조 / 디버그
//     % 투명 / 배경
//
// 예를 들어, 번역하기 전에 원점에서 회전된 큐브만 표시합니다.
translate([20, 0, 0]) !rotate([0, 20, 30]) cube(10);

// 형식 지정
// 다음 모델은 동일합니다. 공식 문서는 두 번째를 선호합니다.
rotate([0, 20, 30]) translate([20, 0, 0]) cube(10);

rotate([0, 20, 30])
    translate([20, 0, 0])
    cube(10);

rotate([0, 20, 30]) {
    translate([20, 0, 0]) {
        cube(10);
    }
}

// 루프
num_cubes = 5;
r = 20;
cube_len = 5;

for (i = [0:num_cubes]) {
    echo(str("Plot cube ", i));
    rotate([0, i * 360 / num_cubes, 0])
        translate([r, 0, 0])
        cube(cube_len, center=true);
}

// 부울 연산
//
//            union() - 두 도형의 합
//       difference() - 첫 번째 도형에서 두 번째 도형을 뺀 것
//     intersection() - 두 도형이 교차하는 부분만
//
cube_l = 20;
cube_w = 10;
cube_h = 10;

hole_pos_l = 10;
hole_pos_h = 5;
hole_r = 3;

difference() {
    cube([cube_l, cube_w, cube_h]);
    translate([hole_pos_l, 0, hole_pos_h])
        rotate([-90, 0, 0])
        cylinder(cube_w, r=hole_r);
}

// 함수는 값을 계산합니다.
function inch2mm(i) = i * 25.4;

cube(inch2mm(2));

// 모듈은 나중에 사용할 객체를 만듭니다.
module house(roof="flat", paint=[1,0,0]) {
    color(paint)
    if (roof=="flat") {
        translate([0,-1,0]) cube();
    } else if (roof=="pitched") {
        rotate([90,0,0])
            linear_extrude(height=1)
            polygon(points=[[0,0],[0,1],[0.5,1.5],[1,1],[1,0]]);
    }
    else if (roof=="domical") {
        translate([0,-1,0]) {
            translate([0.5,0.5,1])
                sphere(r=0.5,$fn=20);
            cube();
        }
    }
}

house("pitched");
translate([2, 0, 0]) house("domical");

// 다른 파일에서 모듈과 함수를 가져옵니다.
include <filename> // 파일의 내용을 이 파일에 작성된 것처럼 가져옵니다.
use <filename>     // 모듈과 함수를 가져오지만 명령을 실행하지 않습니다.
```

## 더 읽을거리

* 공식 문서 [openscad.org/documentation.html](https://openscad.org/documentation.html)
* 치트 시트 [openscad.org/cheatsheet/index.html](https://openscad.org/cheatsheet/index.html)
* Vim 바인딩 [github.com/sirtaj/vim-openscad](https://github.com/sirtaj/vim-openscad)