---
language: Fortran
filename: learnfortran-cn.f90
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
translators:
    - ["Corvusnest", "https://github.com/Corvusnest"]
lang: zh-cn
---

Fortran 是最古老的计算机语言之一。它由 IBM 开发于 1950 年用于数值运算（Fortran 为 "Formula
Translation" 的缩写）。虽然该语言已年代久远，但目前仍用于高性能计算，如天气预报。
该语言仍在持续发展，并且基本保持向下兼容。知名的版本为 Fortran 77, Fortran 90,
Fortran 95, Fortran 2008, Fortran 2015 和 Fortran 2023。

这篇概要将讨论 Fortran 2008 的一些特征。因为它是目前所广泛采用的标准版本，并且与最新版本的内容
也基本相同（而 Fortran 77 则是一个非常不同的版本）。

```fortran
! 这是一个注释

program example         ! 声明一个名为 example 的程序

    ! 代码只能存在于程序、函数、子程序或模块中
    ! 使用缩进不是必需的，但推荐使用

    ! 声明变量
    ! =========

    ! 所有的声明必须在语句和表达式之前

    implicit none       ! 防止动态声明变量（推荐！）
    ! implicit none 推荐在每个函数/程序/模块中重新声明...

    ! 注意 - Fortran 中对大小写不敏感
    real z
    REAL Z2

    real :: v, x        ! 警告：默认的初始值取决于编译器！
    real :: a = 3, b = 2E12, c = 0.01
    integer :: i, j, k = 1, m
    real, parameter :: PI = 3.1415926535897931    ! 声明一个常数
    logical :: y = .TRUE., n = .FALSE.            ! 布尔类型
    complex :: w = (0, 1)                         ! 单位虚数
    character(len=3) :: month                     ! 字符串，长度为 3 个字符

    real :: array(6)                              ! 声明一个包含 6 个实数的数组
    real, dimension(4) :: arrayb                  ! 另一种声明数组的方式
    integer :: arrayc(-10:10)                     ! 具有自定义索引的数组
    real :: array2d(3, 2)                         ! 多维数组

    ! 这些分隔符 '::' 并不总是必需的，但推荐使用

    ! 还有许多其他的变量属性：
    real, pointer :: p                            ! 声明一个指针

    integer, parameter :: LP = selected_real_kind(20)
    real(kind=LP) :: d                            ! 长精度变量

    ! 警告：在声明过程中初始化变量会在函数中造成问题，
    ! 因为这会自动暗示 'save' 属性，
    ! 在函数调用之间保存变量的值一般情况下，
    ! 除了常量外，声明和初始化的代码应该分开！

    ! 字符串
    ! =======

    character :: a_char = 'i'
    character(len=6) :: a_str = "qwerty"
    character(len=30) :: str_b
    character(len=*), parameter :: a_long_str = "This is a long string."
    ! 使用 (len=*) 可以自动计算长度，但只适用于常量

    str_b = a_str//" keyboard"      ! 使用 // 运算符连接字符串

    ! 赋值和算术
    ! =============

    Z = 1                           ! 对上面声明的变量 z 进行赋值（对大小写不敏感）
    j = 10 + 2 - 3
    a = 11.54/(2.3*3.1)
    b = 2**3                        ! 幂运算

    ! 流程控制语句和操作符
    ! ===================

    ! 单行 if 语句
    if (z == a) b = 4               ! 条件始终需要在括号中

    if (z /= a) then                ! z 不等于 a
        ! 其他的比较操作符包括 < > <= >= == /=
        b = 4
    else if (z .GT. a) then         ! z 大于 a
        ! 文本等价于符号操作符中的 .LT. .GT. .LE. .GE. .EQ. .NE.
        b = 6
    else if (z < a) then            ! 'then' 必须在本行上
        b = 5                       ! 执行块必须在新的一行上
    else
        b = 10
    end if                          ! end 语句后需要 'if'（或可以使用 'endif'）

    if (.NOT. (x < c .AND. v >= a .OR. z == z)) then    ! 布尔运算符
        inner: if (.TRUE.) then     ! 可以对 if 结构命名
            b = 1
        end if inner                ! then 必须命名对应的 endif 语句
    end if

    i = 20
    select case (i)
    case (0, 1)                     ! 当 i == 0 或 i == 1 时
        j = 0
    case (2:10)                     ! 当 i 在 2 到 10 之间（包括边界）时
        j = 1
    case (11:)                      ! 当 i >= 11 时
        j = 2
    case default
        j = 3
    end select

    month = 'jan'
    ! 条件可以是整数、逻辑、或字符类型
    ! Select 结构也可以命名
    monthly:select case(month)
    case ("jan")
        j = 0
    case default
        j = -1
    end select monthly

    do i = 2, 10, 2             ! 循环从 2 到 10（包括）以 2 为步长
        innerloop: do j = 1, 3  ! 循环也可以命名
            exit                ! 退出循环
        end do innerloop
        cycle                   ! 跳到下一个循环迭代
    end do

    ! 虽然存在 Goto 语句，但它被强烈不推荐
    goto 10
    stop 1                      ! 立即停止代码（并返回指定的条件代码）
10  j = 201                     ! 这一行被标记为 10 行

    ! 数组
    ! =====
    array = (/1, 2, 3, 4, 5, 6/)
    array = [1, 2, 3, 4, 5, 6]  ! 使用 Fortran 2003 的表示法
    arrayb = [10.2, 3e3, 0.41, 4e-5]
    array2d = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2])

    ! Fortran 数组索引从 1 开始
    ! （默认情况下，但对于特定数组可以定义不同的索引）
    v = array(1)                ! 取数组的第一个元素
    v = array2d(2, 2)

    print *, array(3:5)         ! 打印从第三个到第五个元素（包括）
    print *, array2d(1, :)      ! 打印二维数组的第一列

    array = array*3 + 2         ! 可以对数组应用数学表达式
    array = array*array         ! 数组操作是逐元素进行的
    ! array = array*array2d     ! 这两个数组是不兼容的

    ! 有许多内置函数可用于数组
    c = dot_product(array, array)   ! 这是点积
    ! 使用 matmul() 进行矩阵运算
    c = sum(array)
    c = maxval(array)
    print *, minloc(array)
    c = size(array)
    print *, shape(array)
    m = count(array > 0)

    ! 循环数组（通常使用 product() 函数）
    v = 1
    do i = 1, size(array)
        v = v*array(i)
    end do

    ! 条件性地执行逐元素赋值
    array = [1, 2, 3, 4, 5, 6]
    where (array > 3)
        array = array + 1
    elsewhere(array == 2)
        array = 1
    elsewhere
        array = 0
    end where

    ! 隐含 do 循环是创建数组的紧凑方式
    array = [(i, i=1, 6)]       ! 创建一个数组 [1,2,3,4,5,6]
    array = [(i, i=1, 12, 2)]   ! 创建一个数组 [1,3,5,7,9,11]
    array = [(i**2, i=1, 6)]    ! 创建一个数组 [1,4,9,16,25,36]
    array = [(4, 5, i=1, 3)]    ! 创建一个数组 [4,5,4,5,4,5]

    ! 输入/输出
    ! =========

    print *, b                  ! 将变量 'b' 打印到命令行

    ! 可以对打印的输出进行格式化
    print "(I6)", 320           ! 打印 '   320'
    print "(I6.4)", 3           ! 打印 '  0003'
    print "(F6.3)", 4.32        ! 打印 ' 4.320'

    ! 字母表示预期的类型，后面的数字表示用于打印值的字符数
    ! 字母可以是 I（整数），F（实数），E（工程表示法），
    ! L（逻辑），A（字符串）...
    print "(I3)", 3200          ! 打印 '***'，因为该数字不适合

    ! 可以有多个格式规范
    print "(I5,F6.2,E6.2)", 120, 43.41, 43.41
    print "(3I5)", 10, 20, 30                       ! 整数的三次重复（字段宽度为 5）
    print "(2(I5,F6.2))", 120, 43.42, 340, 65.3     ! 格式重复组合

    ! 我们还可以从终端读取输入
    read (*, *) v
    read (*, "(2F6.2)") v, x                        ! 读取两个数字

    ! 写入文件
    open (unit=12, file="records.txt", status="replace")
    ! 文件通过 'unit number' 引用，这个数字可以在 9:99 范围内选择
    ! Status 可以是 {'old','replace','new'} 中的一个
    write (12, "(F10.2,F10.2,F10.2)") c, b, a
    close (12)

    ! 读取文件
    open (newunit=m, file="records.txt", status="old")
    ! 文件通过 'new unit number' 引用，编译器为您选择一个整数
    read (unit=m, fmt="(3F10.2)") a, b, c
    close (m)

    ! 还有更多功能可用，超出了本文所讨论的范围，
    ! 还有由于与旧版本的 Fortran 的向后兼容性而存在的替代方案

    ! 内置函数
    ! ===========

    ! Fortran 大约有 200 个语言内部的函数/子程序
    ! 例如 -
    call cpu_time(v)        ! 将 'v' 设置为以秒为单位的时间
    k = ior(i, j)           ! 两个整数的位 OR 运算
    v = log10(x)            ! 以 10 为底的对数
    i = floor(b)            ! 返回小于或等于 x 的最接近的整数
    v = aimag(w)            ! 复数的虚部

    ! 函数和子程序
    ! ==============

    ! 子程序运行一些代码，并可以对输入值产生副作用或修改输入值

    call routine(a, c, v)   ! 子程序调用

    ! 函数采用一系列输入参数，并返回一个单个值
    ! 不过，输入参数可能仍会被修改，并且会执行副作用

    m = func(3, 2, k)       ! 函数调用

    ! 函数调用还可以在表达式中使用
    print *, func2(3, 2, k)

    ! 一个纯函数是一个不修改其输入参数，
    ! 也不会引起任何副作用的函数
    m = func3(3, 2, k)

contains                    ! 包含程序内部定义的子程序的区域

    ! Fortran 有几种稍微不同的方式来定义函数

    integer function func(a, b, c)      ! 函数返回一个整数值
        ! implicit none                 ! 子变量域可以不再声明 implicit none
        integer, intent(in) :: a, b, c  ! 在函数内部定义输入参数的类型

        if (a >= 2) then
            func = a + b + c            ! 返回变量默认为函数名
            return                      ! 随时可以从函数返回当前值
        end if
        func = a + c

        ! 在函数的末尾不需要 return 语句
    end function func

    function func2(a, b, c) result(f)   ! 返回变量声明为 'f'
        integer, intent(in) :: a, b     ! 可以声明和强制约定变量
        ! 不会被函数修改
        integer, intent(inout) :: c
        integer :: f                    ! 函数返回类型在函数内部声明
        integer :: cnt = 0              ! 注意：初始化暗示变量在函数调用之间保存
        !

        f = a + b - c
        c = 4                           ! 修改输入变量的值
        cnt = cnt + 1                   ! 计算函数调用的次数

    end function func2

    pure function func3(a, b, c)        ! 纯函数不能有副作用
        integer, intent(in) :: a, b, c
        integer :: func3

        func3 = a*b*c

    end function func3

    subroutine routine(d, e, f)
        real, intent(inout) :: f
        real, intent(in) :: d, e

        f = 2*d + 3*e + f

    end subroutine routine

end program example                     ! 程序定义结束--------------------------

! 函数和子程序在程序列表之外声明，在程序之间以及模块中声明时，需要使用 interface 声明（即使它们在同一源文件中）（见下面）将它们定义在模块或程序的 'contains' 部分更容易

elemental real function func4(a) result(res)
! elemental 函数是一个纯函数，它采用标量输入变量，
! 但也可以在数组上独立应用，并返回一个新的数组
    real, intent(in) :: a

    res = a**2 + 1.0

end function func4

! 模块
! =======

! 模块是在可重用性中将相关的声明、函数和子程序结合在一起的有用方式

module fruit

    real :: apple
    real :: pear
    real :: orange

end module fruit

module fruity
    ! 声明的顺序必须是：模块、接口、变量
    !（也可以在程序中声明模块和接口）

    use fruit, only: apple, pear    ! 使用 fruit 模块中的 apple 和 pear
    implicit none                   ! 导入模块之后

    private                         ! 将一些内容私有化（默认为公共）
    ! 显式将一些变量/函数声明为公共
    public :: apple, mycar, create_mycar
    ! 将一些变量/函数声明为模块私有（本例中是多余的）
    private :: func4

    ! 接口
    ! ========
    ! 在模块内部（最好放在 'contains' 部分）显式声明外部函数/过程
    interface
        elemental real function func4(a) result(res)
            real, intent(in) :: a
        end function func4
    end interface

    ! 可以使用命名接口定义重载函数
    interface myabs
        ! 可以使用 'module procedure' 关键字包括模块内已经定义的函数
        module procedure real_abs, complex_abs
    end interface

    ! 派生数据类型
    ! ==================
    ! 可以创建自定义的结构化数据集合
    type car
        character(len=100) :: model
        real :: weight              !（千克）
        real :: dimensions(3)       ! 即，长度-宽度-高度（米）
        character :: colour
    contains
        procedure :: info           ! 将过程绑定到类型
    end type car

    type(car) :: mycar              ! 声明自定义类型的变量
    ! 请查看 create_mycar() 程序的用法

    ! 注意：模块中没有可以执行的语句

contains

    subroutine create_mycar(mycar)
        ! 演示派生数据类型的用法
        type(car), intent(out) :: mycar

        ! 使用 '%' 运算符访问类型元素
        mycar%model = "Ford Prefect"
        mycar%colour = 'r'
        mycar%weight = 1400
        mycar%dimensions(1) = 5.0   ! 默认索引从 1 开始！
        mycar%dimensions(2) = 3.0
        mycar%dimensions(3) = 1.5

    end subroutine create_mycar

    subroutine info(self)
        class(car), intent(in) :: self
        ! 使用 'class' 关键字将过程绑定到类型

        print *, "Model     : ", self%model
        print *, "Colour    : ", self%colour
        print *, "Weight    : ", self%weight
        print *, "Dimensions: ", self%dimensions

    end subroutine info

    real pure function real_abs(x)
        real, intent(in) :: x

        if (x < 0) then
            real_abs = -x
        else
            real_abs = x
        end if

    end function real_abs

    real pure function complex_abs(z)
        complex, intent(in) :: z
        ! 长行可以使用继续字符 '&' 进行延续

        complex_abs = sqrt(real(z)**2 + &
                           aimag(z)**2)

    end function complex_abs

end module fruity
```

### 更多资源

了解更多的 Fortran 信息:

+ [wikipedia](https://en.wikipedia.org/wiki/Fortran)
+ [Fortran-lang Organization](https://fortran-lang.org/)
+ [Fortran_95_language_features](https://en.wikipedia.org/wiki/Fortran_95_language_features)
+ [fortranwiki.org](http://fortranwiki.org)
+ [www.fortran90.org/](http://www.fortran90.org)
+ [list of Fortran 95 tutorials](http://www.dmoz.org/Computers/Programming/Languages/Fortran/FAQs%2C_Help%2C_and_Tutorials/Fortran_90_and_95/)
+ [Fortran wikibook](https://en.wikibooks.org/wiki/Fortran)
+ [Fortran resources](http://www.fortranplus.co.uk/resources/fortran_resources.pdf)
+ [Mistakes in Fortran 90 Programs That Might Surprise You](http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html)
