---
name: ILNumerics Open Numerical Algorithm Language (ONAL)
filename: learnILNumerics.cs
contributors:
    - ["Haymo Kutschbach", "https://ilnumerics.net"]
    - ["Jonas Nordholt", "https://ilnumerics.net"]
    - ["Kai C. Meyer", "https://ilnumerics.net"]
---

# Learn ILNumerics.ONAL in Y Minutes

ILNumerics.ONAL is the free and open source (MIT) reference implementation of the ONAL (Open Numerical Algorithm Language) standard for .NET. 

It provides a complete, production-ready environment for writing numerical algorithms using array-based semantics, covering areas such as linear algebra, elementwise operations, and advanced mathematical functions. The implementation is designed to be fully functional on its own, delivering reliable and robust execution with solid baseline performance.

As the reference implementation, ILNumerics.ONAL defines the canonical behavior of the ONAL language. It intentionally excludes performance-specific APIs, allowing algorithms to remain clean, stable, and independent of execution strategies.

Code written with ILNumerics.ONAL can be executed directly on .NET, and — when needed — can be picked up by ONAL-compatible compilers to enable advanced optimization, parallelization, and hardware acceleration without requiring any changes to the original code.

The language and its reference implementation are derived from the commercial ILNumerics project, which has been used successfully in thousands of industrial and academic applications for over a decade. Its concepts have also served as a foundation for numerous in-house numerical libraries.

The decision to open-source this core in 2026 was driven by a common industry problem: the proliferation of hundreds of custom, domain-specific math libraries, created due to the lack of a comprehensive, non-proprietary, one-for-all solution.

ILNumerics.ONAL addresses this gap by providing a standardized, open, and vendor-neutral numerical language core, enabling organizations to build and maintain their algorithms on a stable, open and shared foundation instead of reinventing their own.

```CSharp
// ILNumerics.ONAL is the Open Numerical Algorithm Language for .NET.
//
// Being a math-domain specific language (DSL) it is embedded into C# /VB 
// and provides MATLAB / NumPy-like numerical computing semantics
// directly in C#, with dense n-dimensional arrays, logical arrays,
// cell arrays, complex numbers, special functions, linear algebra,
// FFTs, Visual Studio tooling and a production-proven numerical core.

// ----------------------------------------------------
// 0. Setup / Installation
// ----------------------------------------------------

/* In an empty directory create a fresh console app and reference 
   the ILNumerics.ONAL nuget package. Using the .NET CLI: 

 \test\ConsoleApp_01> dotnet new console
 \test\ConsoleApp_01> dotnet add package ILNumerics.ONAL

   Paste this file's content into the auto-generated Program.cs.
*/

// Most language features are included by following using's: 
using ILNumerics;
using static ILNumerics.Globals;
using static ILNumerics.ILMath;

// ----------------------------------------------------
// 1. Array data 
// ----------------------------------------------------

// Scalars are either .NET primitives ...
var a = 42.0;

// ... or scalar arrays 
Array<double> A = 42.0;

// printing
Console.WriteLine(A.ToString());
//A                             works in VS as ToolTip and Immediate Window !
//<Double> [1,1] 42 | Dev:0     shows: <Type> [Size] value + storage info
//    [0]:           42                    [Rows with element values...]

// By default, arrays have MATLAB semantics: two or more dimensions 
Console.WriteLine($"{A.ndim} | {A.size_} | {A.GetValue(0)}");
//2 | 1 | 42

// NumPy array mode is enabled per thread: 
Settings.ArrayStyle = ArrayStyles.numpy;
// In NumPy mode arrays have 0 ... 32 dimensions. Scalars size is []
Array<uint> N = 42;
//N
//<UInt32> [] 42 | Dev:0
//    [0]:  42
Console.WriteLine($"{N.ndim} | {N.S.NumberOfElements} | {N.GetValue(0)}");
//0 | 1 | 42

// Vectors, Matrices, more dimensions...
A = vector(1.0, 2.0, -3.9);
//A
//<Double> [3] 1...-3.9 - Dev:0 
//    [0]:            1
//    [1]:            2
//    [2]:    -3.900000

// give element type in creation funcs
var B = ones<int>(3, 4);
//B
//<Int32> [3,4] 1...1 | Dev:0
//    [0]:            1           1           1           1
//    [1]:            1           1           1           1
//    [2]:            1           1           1           1

var C = rand(2, 3, 4);  
//C.ToString() now acts as in NumPy, iterates along last dimensions:  
//<Double> [2,3,4] 0.38770023006373094...0.08473864853602771 | Dev:0
//    [0]: (0,:,:)
//    [1]:     0.387700    0.715513    0.254004    0.308928
//    [2]:     0.297298    0.313920    0.551458    0.551809
//    [3]:     0.040719    0.045817    0.757916    0.915813
//    [4]: (1,:,:)
//    [5]:     0.321033    0.829255    0.592974    0.769929
//    [6]:     0.949368    0.917548    0.708586    0.845221
//    [7]:     0.293954    0.590557    0.195473    0.084739

// produce arbitrary values: vector -> reshape
var D = arange(-1.0, -1.0, -10.0).Reshape(2, 5);
//D
//<Double> [2,5] -1...-10 - Dev:0
//    [0]:           -1          -2          -3          -4          -5
//    [1]:           -6          -7          -8          -9         -10

// n-dimensional arrays: 
A = new[,] { { 1.0, 2.0 }, { 3.0, 4.0 } };  // n-D System.Array
B = empty<int>(1000, 2000, 10);             // un-initialized 3-D array
C = zeros<double>(100, 200, 10, 2);         // cleared 4-D array
eye<double>(6, 5);                          // diagonal matrix, [non/]square          

// Ranges
linspace(1.0, 10.0, length: 100);           // equally spaced vector
logspace(1.0, 10.0, length: 100);           // log. spaced vector
arange(1.0, 0.01, 10.0);                    // ranged array, stepsize 0.01

// Concatenation
A = horzcat(A, D);                          // place D after A, along dim 1
vertcat(rand(2, 3), zeros<double>(8, 3));   // [8,3] below [2,3] => 10x3 array

// See more: https://ilnumerics.net/ArrayCreation3.html

// ----------------------------------------------------
// 2. Array operators
// ----------------------------------------------------
// Array<T> / Logical support all common unary and binary ops: 
// +,-,*,/,%,!, ==, !=, <,>,<=,>=, &, |, ^, <<, >> 
// See: https://ilnumerics.net/array-operators_v5.html

// scalar with array
B = 100 + zeros<int>(3, 2);
//B
//<Int32>[3, 2] 100...100 | Dev:0
//    [0]:          100         100
//    [1]:          100         100
//    [2]:          100         100

// array with array, element wise
B = B - ones<int>(3,2);
//B
//<Int32> [3,2] 99...99 | Dev:0
//    [0]:           99          99
//    [1]:           99          99
//    [2]:           99          99

// broadcasting, numpy mode (align at last dimensions): 
var nump = B * vector(1, 0);
//nump
//<Int32> [3,2] 99...0 | Dev:0
//    [0]:           99           0
//    [1]:           99           0
//    [2]:           99           0

// broadcasting, MATLAB mode (align at first dimensions):  
Settings.ArrayStyle = ArrayStyles.ILNumericsV4; 
var matl = B * vector(1, 0, 1);
//matl
//<Int32> [3,2] 99...99 | Dev:0
//    [0]:           99          99
//    [1]:            0           0
//    [2]:           99          99

// logical/comparison ops / masks: <,>,<=,>=,!=,==,% 
var m = matl > 0;
//m
//Logical [3,2] True...True | Dev:0
//    [0]:  ▮ ▮
//    [1]:  ▯ ▯
//    [2]:  ▮ ▮

// combine masks: &,|,^ 
var comb = m & (nump != 0);
//comb
//Logical [3,2] True...False | Dev:0
//    [0]:  ▮ ▯
//    [1]:  ▯ ▯
//    [2]:  ▮ ▯

// More logicals: https://ilnumerics.net/Logicals.html

// integer binary ops: &,|,^ 
var and_ = nump & 1;
//and
//<Int32> [3,2] 1...0 | Dev:0
//    [0]:            1           0
//    [1]:            1           0
//    [2]:            1           0

// ----------------------------------------------------
// 2.1 Array indexing, read access 
// ----------------------------------------------------
// All indexing is 0(zero)-based !

A = linspace(1.0, 12.0, 12).Reshape(3, 4);   // test matrix
//<Double> [3,4] 1...12 | Dev:0
//    [0]:            1           4           7          10
//    [1]:            2           5           8          11
//    [2]:            3           6           9          12

// read single element 
a = (double)A[0, 0];   // cast scalar array to System.Double 
// or: 
a = A.GetValue(0, 0);

// last element in a dimension
a = (double)A[end, end];

// ranged dimension indices
C = A[1, full];
//<Double> [1,4] 2...11 ? Dev:0
//    [0]:            2           5           8          11

// indices as range or string
C = A[r(0, end), "0:1"];
//<Double> [3,2] 1...6 | Dev:0
//    [0]:            1           4
//    [1]:            2           5
//    [2]:            3           6

// stepped ranges      v--step!
C = A[r(0, 2, end), "0:2:1"];
//<Double> [2,1] 1, 3 ? Dev:0
//    [0]:            1
//    [1]:            3

// Matlab specific indexing
// ========================
Settings.ArrayStyle = ArrayStyles.ILNumericsV4; 

// linear indexing for unspecified trailing dimensions
a = (double)A[4];
// 5

// flattening
Settings.ArrayStyle = ArrayStyles.ILNumericsV4;
C = A[full].T;
//<Double> [1,12] 1...12 | Dev:0
//    [0]:    1   2   3   4   5   6   7   8   9   10   11   12

// string adressing indiv. indices
C = A["0,2,1", "3,2,1,0,1"];
//<Double> [3,5] 10...5 | Dev:0
//    [0]:           10           7           4           1           4
//    [1]:           12           9           6           3           6
//    [2]:           11           8           5           2           5

// arrays as indices
Array<int> i0 = vector(0, 2, 1), i1 = vector(3, 2, 1, 0, 1); 
C = A[i0, i1];
//<Double> [3,5] 10...5 | Dev:0
//    [0]:           10           7           4           1           4
//    [1]:           12           9           6           3           6
//    [2]:           11           8           5           2           5

// NumPy specific indexing
// ========================
Settings.ArrayStyle = ArrayStyles.numpy;

// slice() => [start...stop), exluding stop
C = A[full, slice(0, 4)]; 
//<Double> [3,4] 1...12 | Dev:0
//    [0]:            1           4           7          10
//    [1]:            2           5           8          11
//    [2]:            3           6           9          12

// -> implicit ':' on unspecified trailing dimensions
// -> singleton dimensions are collapsed / removed
C = A[1]; 
//<Double> [4] 2...11 ? Dev:0
//    [0]:            2
//    [1]:            5
//    [2]:            8
//    [3]:           11

// keyword: ellipsis
C = A[ellipsis, 1]; 
//<Double> [3] 4...6 | Dev:0
//    [0]:            4
//    [1]:            5
//    [2]:            6

// keyword: newaxis 
C = A[newaxis, 1];
//<Double> [1,4] 2...11 ? Dev:0
//    [0]:            2           5           8          11

// NumPy advanced indexing
// ========================
i0 = vector(0, 2, 1); i1 = vector(3, 2, 1);
C = A[i0, i1];
//<Double> [3] 10...5 | Dev:0
//    [0]:           10
//    [1]:            9
//    [2]:            5

// Logical indexing
// ========================
var mask = A >= 6;
C = A[mask];
//<Double> [7] 7...12 | Dev:0
//    [0]:            7
//    [1]:           10
//    [2]:            8
//    [3]:           11
//    [4]:            6
//    [5]:            9
//    [6]:           12

// ----------------------------------------------------
// 2.2 Array indexing, write access
// ---------------------------------------------------
//

// simple assignment
A[1, 2] = pi;           // zero based
A[-1, -2] = -0.002;     // from dimension end
A[end] = 199.0; 

// Matlab style indexed writing
// ============================
Settings.ArrayStyle = ArrayStyles.ILNumericsV4;

A = reshape(arange(1.0, 9.0), 3, 3);

// Matlab uses Cartesian indexing: 
// all the same indexing options as for read: 
A["1,2", "1:2"] = 0;            // stringified index list / range, or 
A[vector(1,2), r(1,2)] = 0;     // index as array or range
//<Double> [3,3] 1...0 | Dev:0
//    [0]:            1           4           7
//    [1]:            2           0           0
//    [2]:            3           0           0

// sequential indexing writes
var diagI = vector(0, 4, 8);
A[diagI] = vector(10.0, 20.0, 40.0);
//<Double> [3,3] 10...40 | Dev:0
//    [0]:           10           4           7
//    [1]:            2          20           0
//    [2]:            3           0          40

// expansion
A[2,3] = 30;
//<Double> [3,4] 10...30 | Dev:0
//    [0]:           10           4           7           0
//    [1]:            2          20           0           0
//    [2]:            3           0          40          30

// removal 
A[full, 2] = null;
//<Double> [3,3] 10...30 | Dev:0
//    [0]:           10           4           0
//    [1]:            2          20           0
//    [2]:            3           0          30

// boolean masked writes
A[A >= 10] *= -1; 
//<Double> [3,3] -10...-30 | Dev:0
//    [0]:          -10           4           0
//    [1]:            2         -20           0
//    [2]:            3           0         -30

// boolean cartesian writes
Logical rows = new[] { true, false, true };
Logical cols = new[] { true, true, false };
A[rows, cols] = 99;
//<Double> [3,3] 99...-30 | Dev:0
//    [0]:           99          99           0
//    [1]:            2         -20           0
//    [2]:           99          99         -30

// NumPy specific indexed writing
// ==============================
Settings.ArrayStyle = ArrayStyles.numpy;

A = reshape(arange(1.0, 9.0), 3, 3);

// paired advanced indexing
var firstD = vector(0, 2);
var seconD = vector(1, 2);
A[firstD, seconD] = 100;
//<Double> [3,3] 1...100 - Dev:0
//    [0]:            1         100           3
//    [1]:            4           5           6
//    [2]:            7           8         100

// CAUTION! No views as in NumPy! Views are detached for writing !
C = A[slice(0, 2)]; 
C[1,1] = -99;
//A[1,1] -> remains: 5

// Read more:
//      https://ilnumerics.net/subarrays-v5.html
//
// NumPy/ Matlab specifics:
//      https://ilnumerics.net/array-style-dependent-indexing-features.html

// ----------------------------------------------------
// 2.3 Array properties / instance methods
// ----------------------------------------------------
// See: https://ilnumerics.net/array-members.html

Console.WriteLine($"{A.S} - {A.ndim} - {A.size_}");  // or: A.Size 
// [3,3] - 2 - 9 
C = A.C;                                        // clone of A                     
Console.WriteLine(string.Join(", ",A.flat));    // readonly iterator
// 1, 100, 3, 4, 5, 6, 7, 8, 100

// Mutating properties / methods
// ==============================
A.shape = vector<long>(1, 9); // or:
A.resize(vector<long>(1, 9)); 
//<Double> [1,9] 1...100 | Dev:0
//    [0]:   1      100     3      4      5      6      7      8    100

A.sort(); 
//<Double> [1,9] 1...100 | Dev:0
//    [0]:   1       3      4      5      6      7      8    100    100

A.fill(-1);
//<Double> [1,9] -1...-1 | Dev:0
//    [0]:  -1      -1     -1     -1     -1     -1     -1     -1     -1

A = A.T;                    // transpose
//<Double> [9,1] -1...-1 | Dev:0
//    [0]:           -1
//    [1]:           -1
//    [2]:           -1
//    [3]:           -1
//    [4]:           -1
//    [5]:           -1
//    [6]:           -1
//    [7]:           -1
//    [8]:           -1

A.squeeze();
//<Double> [9] -1...-1 | Dev:0
//    [0]:           -1
//    [1]:           -1
//    [2]:           -1
//    [3]:           -1
//    [4]:           -1
//    [5]:           -1
//    [6]:           -1
//    [7]:           -1
//    [8]:           -1

C = A.Repmat(1, 2);
//C
//<Double> [9,2] -1...-1 | Dev:0
//    [0]:           -1          -1
//    [1]:           -1          -1
//    [2]:           -1          -1
//    [3]:           -1          -1
//    [4]:           -1          -1
//    [5]:           -1          -1
//    [6]:           -1          -1
//    [7]:           -1          -1
//    [8]:           -1          -1

C = C.Reshape(3, 6);
//<Double> [3,6] -1...-1 - Dev:0
//    [0]:          -1         -1         -1         -1         -1         -1
//    [1]:          -1         -1         -1         -1         -1         -1
//    [2]:          -1         -1         -1         -1         -1         -1

var rep = vector<float>(1, 2, 3, 4).Reshape(2, 2).repeat(2, axis: 1); 
//<Single> [2,4] 1...4 - Dev:0
//    [0]:            1           1           2           2
//    [1]:            3           3           4           4

A = randn(20, 30);
A.GetLimits(out var min, out var max);
Console.WriteLine($"Limits: {min}...{max}");
// Limits: -3,1349660523164173...3,296972277066667

Console.WriteLine($"C == A ? {(C.Equals(A) ? "Yes" : "No")}");
// C == A ? No

Console.WriteLine($"A[1,2]: {A.GetValue(1,2)}" +
                    "Type: {A.GetValue(1,2).GetType().Name}");
// A[1,2] is: -0,20381103889630028 Type: Double

// configure ToString()
Console.WriteLine($"A: " + A.ToString(maxNumberElementsPerDimension: 3));
//A: <Double> [20,30]
//    0,041423   -0,070267         ...    0,035125
//   -0,358640   -2,484498         ...   -0,906128
//         ...
//    1,115019   -1,096849         ...    0,587286

// ----------------------------------------------------
// 3. Linear algebra / complex numbers
// ----------------------------------------------------

// matrix multiply
var O = counter(1.0, 1.0, 2, 2, StorageOrders.RowMajor);
var R = vector(-1.0, 3.5);
var result = multiply(O, R); 
//<Double> [2,1] 6, 11 | Dev:0
//    [0]:            6
//    [1]:           11

Array<complex> c = new complex(0, 1);
var z = exp(complex.i * pi) + (complex)1.0;
//<complex> [] 0+i0,00000000 | Dev:0
//    [0]:  0+1,2246467991473532E-16i
Console.WriteLine($"Result: " + (abs(z) < eps ? "ok" : "wrong"));

// complex linear algebra
Array<complex> M = vector(
        new complex(1, 1), new complex(2, 0),
        new complex(3, 0), new complex(4, -1)).Reshape(2, 2); 
//<complex> [2,2] 1+i...4-i - Dev:0
//    [0]:         1+i                 2+i0        
//    [1]:         3+i0                4-i 
Array<complex> rh = vector(
        new complex(1, 0), new complex(2, 1)).Reshape(2,1);
//<complex> [2,1] 1+i, 2+i - Dev:0
//    [0]:         1+i0        
//    [1]:         2+i     

// solve for: M x = rh
var x = linsolve(M, rh);
//<complex> [2] -0,9000000000000001+0,30000000000000004i, 1,1+0,3i | Dev:0
//    [0]:  -0,90000+i0,30000  
//    [1]:   1,10000+i0,30000  
var err = multiply(M, x) - rh;
Console.WriteLine($"Correct? {(all(abs(err) < eps) ? "true" : "false")}");

// FFT Transforms 
// =========================
var t = linspace(0, 2 * pi, 100);

// complex signal
var signal = exp(complex.i * ccomplex(t,0));

// FFT
var spectrum = fft(signal);
// inspect signal, spectrum visually: Array Visualizer plug-in (Visual Studio)
// https://ilnumerics.net/media/png/ILNumerics_SignalVisualizedComplex.png
// https://ilnumerics.net/media/png/ILNumerics_SpectrumVisualizedComplex.png

// real / imaginary parts & Co
var magnitude = abs(spectrum);
var phase = atan(spectrum);
var realPart = real(spectrum);
var imagPart = imag(spectrum);
var conjugate = conj(spectrum);

// ----------------------------------------------------
// 4. Functions
// ----------------------------------------------------

// Array parameters have In/Out semantics
Array<double> ScaleToNewArray(Array<double> A) {
    return pow(A,2); 
}
void ScaleInPlace(Array<double> a) {
    a[full] = 2 * a;
}

C = ScaleToNewArray(A); // A unmodified
ScaleInPlace(A);        // A was modified

// immutable array API for value semantics
Array<uint> ImmutableArgument(InArray<uint> A) {

    // A[full] = ... ; <- error: A is immutable
    
    return A ^ 1 >> 2; 
}

// ----------------------------------------------------
// 5. Predefined functions
// ----------------------------------------------------
// Most static array functions live in ILNumerics.ILMath. 
// They expect Array<T> (T: numeric) and return corresponding Array<T>.

// abs;acos;add_sat;add;all;allall;and;any;anyall;apply;arange;array;asin;
// asinc;atan;atan2;besselJ0;besselJ1;besselJn;besselModifiedI0;xor;zeros;
// besselModifiedI1;besselModifiedIn;besselModifiedK0;besselModifiedK1;
// besselModifiedKn;besselY0;besselY1;besselYn;beta;betaIncomplete;
// binomialCoefficients;binomialCoefficientsLog;bitand;bitneg;bitor;bitxor;
// cart2pol;cart2sphere;ccomplex;ceil;cell;cellv;check;checknull;chol;column;
// concat;conj;conjInplace;convert;cos;cosh;counter;cross;csvread;csvwrite;
// cumprod;cumsum;det;diag;diff;diGamma;distL1;distL2sq;divide_sat;divide;eig;
// eigGen;eigSymm;empty;eq;eqnan;errorFunction;errorFunctionComplement;
// errorFunctionInverse;exp;eye;factorial;factorialLog;fft;fft2;fftn;find;
// find32;fix;flip;fliplr;flipud;floor;free;gamma;gammaIncomplete;gammaLog;ge;
// get_FFTImplementation;get_Lapack;gt;horzcat;ifft;ifft2;ifft2sym;ifftn;
// ifftnsym;ifftsym;imag;ind2sub;isempty;isequal;isequalwithequalnans;isfinite;
// ishermitian;ishesslow;ishessup;isinf;isnan;isneginf;isnull;isnullorempty;
// isposinf;istrilow;istriup;le;length;linsolve;linsolveTriLow;linsolveTriUp;
// linspace;loadArray;loadBinary;log;log10;log10c;logc;logical;logistic;logit;
// logspace;lshift;lt;lu;max;maxall;mean;meshgrid;min;minall;mod;multiply;
// multiplyElem_sat;multiplyElem;ndims;negate;neq;New;norm;not;numel;ones;or;
// permute;pinv;pol2cart;poly;pow_sat;pow;prod;prodall;qr;rand;randn;rank;real;
// real2complex;reinterpret_cast;repmat;reshape;round;row;rshift;
// set_FFTImplementation;set_Lapack;sign;sin;sinh;size;sort;sphere2cart;sqrt;
// sqrtc;squared;squeeze;sub2ind;subtract_sat;subtract;sum;sumall;svd;tan;tanh;
// tocomplex;todouble;tofcomplex;toint16;toint32;toint64;toint8;tological;
// tosingle;touint16;touint32;touint64;touint8;tril;triu;vec;vector;vertcat;

// ----------------------------------------------------
// 6. Data I/O
// ----------------------------------------------------
// These functions are intended for external API. For bulk data
// loading/saving. For algorithmic data manipulation use subarrays/
// indexers (see above)!
// Note: ILNumerics arrays store element data on the unmanaged heap!
// References to these elements are not subject to garbage collection,
// hence do not require pinning!

A = counter(1.0, 1.0, 2, 3, order: StorageOrders.ColumnMajor);

// export element data - direct internal storage reference
var r_span = A.AsReadOnlySpan(order: StorageOrders.RowMajor);   
Console.WriteLine("Element at #2:" + r_span[2]);

unsafe {
    var r_addr = (double*)A.GetHostPointerForRead(StorageOrders.RowMajor);
    Console.WriteLine("Element at #2:" + r_addr[2]);
}

// copy to managed heap
var r_array = A.GetArrayForRead(order: StorageOrders.RowMajor); 
//{double[6]}
//    [0]: 1
//    [1]: 3
//    [2]: 5
//    [3]: 2
//    [4]: 4
//    [5]: 6

// accessing internal storage for writing
var rw_span = A.AsSpan(); 
rw_span[1] = 111; 

unsafe {
    var w_addr = (double*)A.GetHostPointerForWrite(StorageOrders.RowMajor);
    w_addr[A.size_ - 1] = 222;
}
//<Double> [2,3] 1...222 - Dev:0
//    [0]:            1         111           5
//    [1]:            2           4         222

// More: https://ilnumerics.net/array-memory-access.html

// CSV read / write
csvwrite<double>(A, "out.iln");

var AA = csvread<double>(File.ReadAllText("out.iln"));

Console.WriteLine($"Roundtrip A: " + (AA.Equals(A) ? "ok" : "fail"));

// Matlab (v5) write / read
var mat = new MatFile(); 
mat["A"] = A;
mat.Write("mat.iln");

var r_mat = new MatFile("mat.iln"); 
var AAA = mat["A"] as Array<double>;

Console.WriteLine($"Roundtrip MatFile:" + (AAA.Equals(A) ? "ok" : "fail"));

// ----------------------------------------------------
// 7. Cell arrays
// ----------------------------------------------------

var ce = cellv(rand(3, 3), "hello", 42, cellv(A,C,AA,N));
//Cell [4] Dev:0
//    [1]: {<Double> [3,3] 0,8284743716141555...0,9507841514194311}
//    [2]:       {<String> [] hello}
//    [3]:          {<Double> [] 42}
//    [4]: {Cell [4] <Double> [2,3]
//           1         111           5
//           2           4         222...<UInt32> [] 42}

var innerCell = ce.GetCell(-1);
var AAAA = innerCell.GetArray<double>(0); 

Console.WriteLine($"Roundtrip Cell: " + (AAAA.Equals(A) ? "ok" : "fail"));

// deep indexing into inner cell contents: 
var deep = ce.GetArray<double>(3, 0);
Console.WriteLine($"Roundtrip Cell : " + (deep.Equals(A) ? "ok" : "fail"));
``` 
## Summary

ILNumerics.ONAL provides:

    ✓ Dense n-dimensional arrays
    ✓ MATLAB-compatible indexing
    ✓ NumPy-style basic and advanced indexing
    ✓ Logical arrays
    ✓ Cell arrays
    ✓ Complex numbers
    ✓ Broadcasting / implicit expansion / shrinking 
    ✓ Elementary functions
    ✓ Special functions, including Bessel, Gamma, Beta and Erf
    ✓ Linear algebra (auto-translated from FORTRAN sources)
    ✓ FFTs and transforms
    ✓ Array-aware functions
    ✓ InArray<T> for input/value-style semantics
    ✓ Visual Studio debugging support
    ✓ Thread-safe use with standard .NET parallel patterns
    ✓ Optional upgrade path to ILNumerics Computing

ILNumerics.ONAL: The **O**pen **N**umerical **A**lgorithm **L**anguage core for .NET.

ILNumerics: https://ilnumerics.net  
Documentation: https://ilnumerics.net/docs.html  
Source code: https://github.com/ILNumerics/ILNumerics.ONAL  
Nuget: https://www.nuget.org/packages/ILNumerics.ONAL  
Visual Studio Marketplace: [Visual Studio Extension](https://marketplace.visualstudio.com/items?itemName=ILNumericsGmbH.ilnumericsVS600)
