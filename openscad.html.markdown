---
language: openscad
filename: learnopenscad.scad
contributors:
    - ["Tom Preston", "https://github.com/tompreston/"]
---

Draw 3D models with code using [OpenSCAD](https://openscad.org/).

```
// Comments look like this

// 3D Primitives
cube(10);
cube([5, 10, 20]);
sphere(10);

// Transformations
translate([20, 0, 0]) cube(10);
rotate([0, 20, 30]) cube(10);

translate([20, 0, 0]) rotate([0, 20, 30]) cube(10);
rotate([0, 20, 30]) translate([20, 0, 0]) cube(10);

// Modifiers
//
//     * disable
//     ! show only
//     # highlight / debug
//     % transparent / background
//
// For example, show only the rotated cube at the origin, before we translate it.
translate([20, 0, 0]) !rotate([0, 20, 30]) cube(10);

// Formatting
// The following models are the same. The official docs prefer the second.
rotate([0, 20, 30]) translate([20, 0, 0]) cube(10);

rotate([0, 20, 30])
    translate([20, 0, 0])
    cube(10);

rotate([0, 20, 30]) {
    translate([20, 0, 0]) {
        cube(10);
    }
}

// Loops
num_cubes = 5;
r = 20;
cube_len = 5;

for (i = [0:num_cubes]) {
    echo(str("Plot cube ", i));
    rotate([0, i * 360 / num_cubes, 0])
        translate([r, 0, 0])
        cube(cube_len, center=true);
}

// Boolean operations
//
//            union() - the sum of both shapes
//       difference() - the first shape, minus the second shape
//     intersection() - only parts of both shapes which intersect
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

// Functions calculate values
function inch2mm(i) = i * 25.4;

cube(inch2mm(2));

// Modules create objects you want to use later
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

// Import modules and function from other files
include <filename> // Import the content of the file as if they were written in this file
use <filename>     // Import modules and functions, but do not execute any commands
```

## Further Reading
* Official docs https://openscad.org/documentation.html
* Cheat sheet https://openscad.org/cheatsheet/index.html
* Vim bindings https://github.com/sirtaj/vim-openscad
