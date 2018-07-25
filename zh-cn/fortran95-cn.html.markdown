---
language: Fortran
filename: learnfortran-cn.f95
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
translators:
    - ["Corvusnest", "https://github.com/Corvusnest"]
lang: zh-cn
---

Fortran 是最古老的计算机语言之一。它由IBM开发于1950年用于数值运算（Fortran 为 "Formula
Translation" 的缩写）。虽然该语言已年代久远，但目前仍用于高性能计算，如天气预报。
该语言仍在持续发展，并且基本保持向下兼容。知名的版本为 Fortran 77, Fortran 90,
Fortran 95, Fortran 2003, Fortran 2008 与 Fortran 2015。

这篇概要将讨论 Fortran 95 的一些特征。因为它是目前所广泛采用的标准版本，并且与最新版本的内容
也基本相同（而 Fortran 77 则是一个非常不同的版本）。

```fortran

! 这是一行注释


program example   !声明一个叫做 example 的程序

    ! 代码只能放在程序、函数、子程序或者模块内部
    ! 推荐使用缩进，但不是必须的。

    ! 声明变量
    ! ===================

    ! 所有的声明必须放在语句与表达式之前

    implicit none    !阻止变量的隐式声明 (推荐!)
    ! Implicit none 必须在每一个 函数/程序/模块 中进行声明

    ! 重要  - Fortran 对大小写不敏感
    real z
    REAL Z2

    real :: v,x    ! 警告: 默认值取决于编译器!    
    real :: a = 3, b=2E12, c = 0.01
    integer :: i, j, k=1, m
    real, parameter :: PI = 3.1415926535897931    !声明一个常量
    logical :: y = .TRUE. , n = .FALSE.    !布尔值
    complex :: w = (0,1)    !sqrt(-1) (译注: 定义复数，此为-1的平方根)
    character (len=3) :: month    !长度为3的字符串

    real :: array(6)     !声明长度为6的浮点数数组
    real, dimension(4) :: arrayb    !声明数组的另一种方法
    integer :: arrayc(-10:10)   !有着自定义索引的数组
    real :: array2d(3,2)    !多维数组

    ! 分隔符 '::' 并不总是必要的，但推荐使用

    ! 还存在很多其他的变量特征:
    real, pointer :: p    !声明一个指针

    integer, parameter :: LP = selected_real_kind(20)
    real (kind = LP) :: d    !长精度变量

    ! 警告：在声明期间初始化变量将导致在函数内发生问题，因为这将自动具备了 “save” 属性，
    ! 因此变量的值在函数的多次调用期间将被存储。一般来说，除了常量，应分开声明与初始化！

    ! 字符串
    ! =======

    character :: a_char = 'i'
    character (len = 6) :: a_str = "qwerty"
    character (len = 30) :: str_b
    character (len = *), parameter :: a_long_str = "This is a long string."
    !可以通过使用 (len=*) 来自动判断长度，但只对常量有效

    str_b = a_str // " keyboard"    !通过 // 操作符来连接字符串


    ! 任务与计算
    ! =======================

    Z = 1    !向之前声明的变量 z 赋值 (大小写不敏感).
    j = 10 + 2 - 3
    a = 11.54  /  (2.3 * 3.1)
    b = 2**3    !幂


    ! 控制流程语句 与 操作符
    ! ===================================

    !单行 if 语句
    if (z == a) b = 4  !判别句永远需要放在圆括号内

    if (z /= a) then !z 不等于 a
    ! 其他的比较运算符： < > <= >= == /=
      b = 4
    else if (z .GT. a) then !z 大于(Greater) a
    ! 文本形式的比较运算符: .LT. .GT. .LE. .GE. .EQ. .NE.  
      b = 6
    else if (z < a) then !'then' 必须放在该行
      b = 5 !执行部分必须放在新的一行里
    else
      b = 10
    end if !结束语句需要 'if' (也可以用 'endif').


    if (.NOT. (x < c .AND. v >= a .OR. z == z)) then   !布尔操作符
      inner: if (.TRUE.) then    !可以为 if 结构命名
        b = 1
      endif inner    !接下来必须命名 endif 语句.
    endif


    i = 20
    select case (i)
      case (0)    !当 i == 0
        j=0
      case (1:10)    !当 i 为 1 到 10 之内 ( 1 <= i <= 10 )
        j=1
      case (11:)    !当 i>=11
        j=2
      case default
        j=3
    end select


    month = 'jan'
    ! 状态值可以为整数、布尔值或者字符类型
    ! Select 结构同样可以被命名
    monthly: select case (month)
      case ("jan")
         j = 0
      case default
         j = -1
    end select monthly

    do i=2,10,2    !从2到10(包含2和10)以2为步进值循环
      innerloop: do j=1,3    !循环同样可以被命名
        exit    !跳出循环
      end do innerloop
    cycle    !重复跳入下一次循环
    enddo


    ! Goto 语句是存在的，但强烈不建议使用
    goto 10    
    stop 1    !立即停止程序 (返回一个设定的状态码).
10  j = 201    !这一行被标注为 10 行 （line 10）


    ! 数组
    ! ======
    array = (/1,2,3,4,5,6/)
    array = [1,2,3,4,5,6]    !当使用 Fortran 2003 版本.
    arrayb = [10.2,3e3,0.41,4e-5]
    array2d =  reshape([1.0,2.0,3.0,4.0,5.0,6.0], [3,2])

    ! Fortran 数组索引起始于 1
    ! (默认下如此，也可以为数组定义不同的索引起始)
    v = array(1)    !获取数组的第一个元素
    v = array2d(2,2)

    print *, array(3:5)    !打印从第3到第五5之内的所有元素
    print *, array2d(1,:)    !打印2维数组的第一列

    array = array*3 + 2    !可为数组设置数学表达式
    array = array*array    !数组操作支持元素级(操作) (element-wise)
    !array = array*array2d    !这两类数组并不是同一个维度的

    ! 有很多内置的数组操作函数
    c = dot_product(array,array)    !点乘 (点积)
    ! 用 matmul() 来进行矩阵运算.
    c = sum(array)
    c = maxval(array)
    print *, minloc(array)
    c = size(array)
    print *, shape(array)
    m = count(array > 0)

    ! 遍历一个数组 (一般使用 Product() 函数).
    v = 1
    do i = 1, size(array)
        v = v*array(i)
    end do

    ! 有条件地执行元素级操作
    array = [1,2,3,4,5,6]
    where (array > 3)
        array = array + 1
    elsewhere (array == 2)
        array = 1
    elsewhere
        array = 0
    end where

    ! 隐式DO循环可以很方便地创建数组
    array = [ (i, i = 1,6) ]    !创建数组 [1,2,3,4,5,6]
    array = [ (i, i = 1,12,2) ]    !创建数组 [1,3,5,7,9,11]
    array = [ (i**2, i = 1,6) ]    !创建数组  [1,4,9,16,25,36]
    array = [ (4,5, i = 1,3) ]    !创建数组 [4,5,4,5,4,5]


    ! 输入/输出
    ! ============

    print *, b    !向命令行打印变量 'b'

    ! 我们可以格式化输出
    print "(I6)", 320    !打印 '   320'
    print "(I6.4)", 3    !打印 '  0003'
    print "(F6.3)", 4.32    !打印 ' 4.320'


    ! 该字母与数值规定了给定的数值与字符所用于打印输出的类型与格式
    ! 字母可为 I (整数), F (浮点数), E (工程格式),
    ! L (逻辑/布尔值), A (字符) ...
    print "(I3)", 3200    !如果数值无法符合格式将打印 '***'

    ! 可以同时设定多种格式
    print "(I5,F6.2,E6.2)", 120, 43.41, 43.41
    print "(3I5)", 10, 20, 30    !连续打印3个整数 (字段宽度 = 5).
    print "(2(I5,F6.2))", 120, 43.42, 340, 65.3   !连续分组格式

    ! 我们也可以从终端读取输入
    read *, v
    read "(2F6.2)", v, x    !读取2个数值

    ! 读取文件
    open(unit=11, file="records.txt", status="old")
    ! 文件被引用带有一个单位数 'unit', 为一个取值范围在9-99的整数
    ! 'status' 可以为 {'old','replace','new'} 其中之一
    read(unit=11, fmt="(3F10.2)") a, b, c
    close(11)

    ! 写入一个文件
    open(unit=12, file="records.txt", status="replace")
    write(12, "(F10.2,F10.2,F10.2)") c, b, a
    close(12)
    ! 在讨论范围之外的还有更多的细节与可用功能，并于老版本的 Fortran 保持兼容


    ! 内置函数
    ! ==================

    ! Fortran 拥有大约 200 个内置函数/子程序
    ! 例子
    call cpu_time(v)    !以秒为单位设置时间
    k = ior(i,j)    !2个整数的位或运算
    v = log10(x)    !以10为底的log运算
    i = floor(b)    !返回一个最接近的整数小于或等于x (地板数)
    v = aimag(w)    !复数的虚数部分


    ! 函数与子程序
    ! =======================

    ! 一个子程序会根据输入值运行一些代码并会导致副作用 (side-effects) 或修改输入值
    ! (译者注: 副作用是指对子程序/函数外的环境产生影响，如修改变量)

    call routine(a,c,v)    !调用子程序

    ! 一个函数会根据输入的一系列数值来返回一个单独的值
    ! 但输入值仍然可能被修改以及产生副作用

    m = func(3,2,k)  !调用函数

    ! 函数可以在表达式内被调用
    Print *, func2(3,2,k)

    ! 一个纯函数不会去修改输入值或产生副作用
    m = func3(3,2,k)


contains ! 用于定义程序内部的副程序(sub-programs)的区域

    ! Fortran 拥有一些不同的方法去定义函数

    integer function func(a,b,c)    !一个返回一个整数的函数
        implicit none   !最好也在函数内将含蓄模式关闭 (implicit none)
        integer :: a,b,c !输入值类型定义在函数内部
        if (a >= 2) then
            func = a + b + c !返回值默认为函数名
            return !可以在函数内任意时间返回当前值
        endif
        func = a + c
        ! 在函数的结尾不需要返回语句
    end function func


    function func2(a,b,c) result(f)    !将返回值声明为 'f'
        implicit none
        integer, intent(in) :: a,b    !可以声明让变量无法被函数修改
        integer, intent(inout) :: c
        integer :: f     !函数的返回值类型在函数内声明
        integer :: cnt = 0    !注意 - 隐式的初始化变量将在函数的多次调用间被存储
        f = a + b - c
        c = 4    !变动一个输入变量的值
        cnt  = cnt + 1    !记录函数的被调用次数
    end function func2


    pure function func3(a,b,c)  !一个没有副作用的纯函数
        implicit none
        integer, intent(in) :: a,b,c
        integer :: func3
        func3 = a*b*c
    end function func3


    subroutine routine(d,e,f)
        implicit none
        real, intent(inout) :: f
        real, intent(in) :: d,e
        f = 2*d + 3*e + f
    end subroutine routine


end program example   ! 函数定义完毕 -----------------------

! 函数与子程序的外部声明对于生成程序清单来说，需要一个接口声明（即使它们在同一个源文件内）(见下)
! 使用 'contains' 可以很容易地在模块或程序内定义它们

elemental real function func4(a) result(res)
! 一个元函数(elemental function) 为一个纯函数使用一个标量输入值
! 但同时也可以用在一个数组并对其中的元素分别处理，之后返回一个新的数组
    real, intent(in) :: a
    res = a**2 + 1.0
end function func4


! 模块
! =======

! 模块十分适合于存放与复用相关联的一组声明、函数与子程序

module fruit
    real :: apple
    real :: pear
    real :: orange
end module fruit


module fruity

    ! 声明必须按照顺序: 模块、接口、变量
    ! (同样可在程序内声明模块和接口)

    use fruit, only: apple, pear    ! 使用来自于 fruit 模块的 apple 和 pear
    implicit none    !在模块导入后声明

    private    !使得模块内容为私有(private)(默认为公共 public)
    ! 显式声明一些变量/函数为公共
    public :: apple,mycar,create_mycar
    ! 声明一些变量/函数为私有(在当前情况下没必要)(译注: 因为前面声明了模块全局 private)
    private :: func4

    ! 接口
    ! ==========
    ! 在模块内显式声明一个外部函数/程序
    ! 一般最好将函数/程序放进 'contains' 部分内
    interface
        elemental real function func4(a) result(res)
            real, intent(in) :: a
        end function func4
    end interface

    ! 重载函数可以通过已命名的接口来定义
    interface myabs
        ! 可以通过使用 'module procedure' 关键词来包含一个已在模块内定义的函数
        module procedure real_abs, complex_abs
    end interface

    ! 派生数据类型
    ! ==================
    ! 可创建自定义数据结构
    type car
        character (len=100) :: model
        real :: weight    !(公斤 kg)
        real :: dimensions(3)    !例: 长宽高(米)
        character :: colour
    end type car

    type(car) :: mycar    !声明一个自定义类型的变量
    ! 用法具体查看 create_mycar()

    ! 注: 模块内没有可执行的语句

contains

    subroutine create_mycar(mycar)
        ! 展示派生数据类型的使用
        implicit none
        type(car),intent(out) :: mycar

        ! 通过 '%' 操作符来访问(派生数据)类型的元素
        mycar%model = "Ford Prefect"
        mycar%colour = 'r'
        mycar%weight = 1400
        mycar%dimensions(1) = 5.0    !索引默认起始值为 1 !
        mycar%dimensions(2) = 3.0
        mycar%dimensions(3) = 1.5

    end subroutine

    real function real_abs(x)
        real :: x
        if (x<0) then
            real_abs = -x
        else
            real_abs = x
        end if
    end function real_abs

    real function complex_abs(z)
        complex :: z
        ! 过长的一行代码可通过延续符 '&' 来换行
        complex_abs = sqrt(real(z)**2 + &
                                         aimag(z)**2)
    end function complex_abs


end module fruity

```

### 更多资源

了解更多的 Fortran 信息:

+ [wikipedia](https://en.wikipedia.org/wiki/Fortran)
+ [Fortran_95_language_features](https://en.wikipedia.org/wiki/Fortran_95_language_features)
+ [fortranwiki.org](http://fortranwiki.org)
+ [www.fortran90.org/](http://www.fortran90.org)
+ [list of Fortran 95 tutorials](http://www.dmoz.org/Computers/Programming/Languages/Fortran/FAQs%2C_Help%2C_and_Tutorials/Fortran_90_and_95/)
+ [Fortran wikibook](https://en.wikibooks.org/wiki/Fortran)
+ [Fortran resources](http://www.fortranplus.co.uk/resources/fortran_resources.pdf)
+ [Mistakes in Fortran 90 Programs That Might Surprise You](http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html)
