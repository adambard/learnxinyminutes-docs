---
name: Fortran
contributors:
    - ["Robert Steed", "https://github.com/robochat"]
filename: learnfortran.f90
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

Fortran은 가장 오래된 컴퓨터 언어 중 하나입니다. 1950년대에 IBM에서 수치 계산을 위해 개발되었습니다(Fortran은 "Formula Translation"의 약자입니다). 오래되었음에도 불구하고 날씨 예측과 같은 고성능 컴퓨팅에 여전히 사용됩니다. 그러나 언어는 수년에 걸쳐 상당히 변경되었지만 대부분 이전 버전과의 호환성을 유지합니다. 잘 알려진 버전은 FORTRAN 77, Fortran 90, Fortran 95, Fortran 2003, Fortran 2008, Fortran 2018 및 Fortran 2023입니다.

이 개요에서는 가장 널리 구현된 최신 사양 중 하나인 Fortran 2008의 기능에 대해 설명하며, 이후 버전은 대체로 유사합니다(따라서 FORTRAN 77과는 매우 다릅니다).

```fortran
! 이것은 주석입니다.

program example         ! example이라는 프로그램을 선언합니다.

    ! 코드는 프로그램, 함수, 서브루틴 또는 모듈 내에만 존재할 수 있습니다.
    ! 들여쓰기는 필수는 아니지만 권장됩니다.

    ! 변수 선언
    ! ===============

    ! 모든 선언은 문 및 표현식 앞에 와야 합니다.

    implicit none       ! 변수의 동적 선언을 방지합니다.
    ! 권장!
    ! implicit none은 모든 함수/프로그램/모듈에서 다시 선언해야 합니다...

    ! 중요 - Fortran은 대소문자를 구분하지 않습니다.
    real z
    REAL Z2

    real :: v, x        ! 경고: 기본 초기값은 컴파일러에 따라 다릅니다!
    real :: a = 3, b = 2E12, c = 0.01
    integer :: i, j, k = 1, m
    real, parameter :: PI = 3.14159265            ! 상수를 선언합니다.
    logical :: y = .TRUE., n = .FALSE.            ! 부울 유형입니다.
    complex :: w = (0, 1)                         ! sqrt(-1)
    character(len=3) :: month                     ! 3자 문자열입니다.

    ! 6개의 실수 배열을 선언합니다.
    real :: array(6)
    ! 배열을 선언하는 또 다른 방법입니다.
    real, dimension(4) :: arrayb
    ! 사용자 지정 인덱스가 있는 배열 -10에서 10까지(포함)
    integer :: arrayc(-10:10)
    ! 다차원 배열입니다.
    real :: array2d(3, 2)

    ! '::' 구분 기호는 항상 필요한 것은 아니지만 권장됩니다.

    ! 다른 많은 변수 속성도 존재합니다:
    real, pointer :: p                            ! 포인터를 선언합니다.

    integer, parameter :: LP = selected_real_kind(20)
    real(kind=LP) :: d                            ! 긴 정밀도 변수입니다.

    ! 경고: 선언 중에 변수를 초기화하면 함수에서 문제가 발생합니다.
    ! 이는 자동으로 'save' 속성을 의미하며, 값은 함수 호출 간에 저장됩니다. 일반적으로 상수를 제외하고 선언과 초기화 코드를 분리하십시오!

    ! 문자열
    ! =======

    character :: a_char = 'i'
    character(len=6) :: a_str = "qwerty"
    character(len=30) :: str_b
    character(len=*), parameter :: a_long_str = "This is a long string."
    ! (len=*)를 사용하여 길이를 자동으로 계산할 수 있지만 상수에만 해당됩니다.

    str_b = a_str//" keyboard"      ! // 연산자를 사용하여 문자열을 연결합니다.

    ! 할당 및 산술
    ! =======================

    Z = 1                           ! 위에 선언된 변수 z에 할당합니다.
    j = 10 + 2 - 3
    a = 11.54/(2.3*3.1)
    b = 2**3                        ! 거듭제곱

    ! 제어 흐름 문 및 연산자
    ! ===================================

    ! 한 줄 if 문
    if (z == a) b = 4               ! 조건은 항상 괄호가 필요합니다.

    if (z /= a) then                ! z가 a와 같지 않음
        ! 다른 기호 비교는 < > <= >= == /=입니다.
        b = 4
    else if (z .GT. a) then         ! z가 a보다 큼
        ! 기호 연산자에 대한 텍스트 등가물은 .LT. .GT. .LE. .GE. .EQ. .NE.입니다.
        b = 6
    else if (z < a) then            ! 'then'은 이 줄에 있어야 합니다.
        b = 5                       ! 실행 블록은 새 줄에 있어야 합니다.
    else
        b = 10
    end if                          ! end 문에는 'if'가 필요합니다.

    if (.NOT. (x < c .AND. v >= a .OR. z == z)) then    ! 부울 연산자입니다.
        inner: if (.TRUE.) then     ! if 구문에 이름을 지정할 수 있습니다.
            b = 1
        end if inner                ! 그런 다음 endif 문에 이름을 지정해야 합니다.
    endif                           ! 'endif'는 'end if'와 동일합니다.

    i = 20
    select case (i)
    case (0, 1)                     ! i == 0 또는 i == 1인 경우
        j = 0
    case (2:10)                     ! i가 2에서 10까지인 경우(포함).
        j = 1
    case (11:)                      ! i>=11인 모든 경우
        j = 2
    case default
        j = 3
    end select

    month = 'jan'
    ! 조건은 정수, 논리 또는 문자 유형일 수 있습니다.
    ! Select 구문도 이름을 지정할 수 있습니다.
    monthly:select case(month)
    case ("jan")
        j = 0
    case default
        j = -1
    end select monthly

    do i = 2, 10, 2             ! 2에서 10까지(포함) 2씩 증가하는 루프입니다.
        innerloop: do j = 1, 3  ! 루프도 이름을 지정할 수 있습니다.
            exit                ! 루프를 종료합니다.
        end do innerloop
        cycle                   ! 다음 루프 반복으로 이동합니다.
    end do

    ! Goto 문이 있지만 사용을 권장하지 않습니다.
    goto 10
    stop 1                      ! 프로그램을 중지하고 조건 코드 1을 반환합니다.
10  j = 201                     ! 이 줄은 10번 줄로 레이블이 지정됩니다.

    ! 배열
    ! ======
    array = (/1, 2, 3, 4, 5, 6/)
    array = [1, 2, 3, 4, 5, 6]  ! Fortran 2003 표기법 사용.
    arrayb = [10.2, 3e3, 0.41, 4e-5]
    array2d = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [3, 2])

    ! Fortran 배열 인덱싱은 1부터 시작합니다.
    ! (기본적으로 특정 배열에 대해 다르게 정의할 수 있음).
    v = array(1)                ! 배열의 첫 번째 요소를 가져옵니다.
    v = array2d(2, 2)

    print *, array(3:5)         ! 3번째부터 5번째까지(포함) 모든 요소를 인쇄합니다.
    print *, array2d(1, :)      ! 2d 배열의 첫 번째 열을 인쇄합니다.

    array = array*3 + 2         ! 배열에 수학적 표현식을 적용할 수 있습니다.
    array = array*array         ! 배열 연산은 요소별로 발생합니다.
    ! array = array*array2d     ! 이러한 배열은 호환되지 않습니다.

    ! 배열에서 작동하는 많은 내장 함수가 있습니다.
    c = dot_product(array, array)   ! 이것은 내적입니다.
    ! 행렬 수학에는 matmul()을 사용하십시오.
    c = sum(array)
    c = maxval(array)
    print *, minloc(array)
    c = size(array)
    print *, shape(array)
    m = count(array > 0)

    ! 배열 반복 (일반적으로 Product() 함수를 사용할 수 있음).
    v = 1
    do i = 1, size(array)
        v = v*array(i)
    end do

    ! 조건부로 요소별 할당을 실행합니다.
    array = [1, 2, 3, 4, 5, 6]
    where (array > 3)
        array = array + 1
    elsewhere(array == 2)
        array = 1
    elsewhere
        array = 0
    end where

    ! 암시적 DO 루프는 배열을 만드는 간결한 방법입니다.
    array = [(i, i=1, 6)]       ! [1,2,3,4,5,6] 배열을 만듭니다.
    array = [(i, i=1, 12, 2)]   ! [1,3,5,7,9,11] 배열을 만듭니다.
    array = [(i**2, i=1, 6)]    ! [1,4,9,16,25,36] 배열을 만듭니다.
    array = [(4, 5, i=1, 3)]    ! [4,5,4,5,4,5] 배열을 만듭니다.

    ! 입출력
    ! ============

    print *, b                  ! 명령줄에 변수 'b'를 인쇄합니다.

    ! 인쇄된 출력을 서식 지정할 수 있습니다.
    print "(I6)", 320           ! '   320'을 인쇄합니다.
    print "(I6.4)", 3           ! '  0003'을 인쇄합니다.
    print "(F6.3)", 4.32        ! ' 4.320'을 인쇄합니다.

    ! 문자는 예상 유형을 나타내고 그 뒤의 숫자는
    ! 값을 인쇄하는 데 사용할 문자 수를 나타냅니다.
    ! 문자는 I(정수), F(실수), E(공학 형식),
    ! L(논리), A(문자) 등이 될 수 있습니다...
    print "(I3)", 3200          ! 숫자가 맞지 않으므로 '***'를 인쇄합니다.

    ! 여러 형식 사양을 가질 수 있습니다.
    print "(I5,F6.2,E6.2)", 120, 43.41, 43.41

    ! 3개의 정수 반복(필드 너비 = 5).
    print "(3I5)", 10, 20, 30

    ! 형식의 반복 그룹화.
    print "(2(I5,F6.2))", 120, 43.42, 340, 65.3

    ! 터미널에서 입력을 읽을 수도 있습니다.
    read (*, *) v
    read (*, "(2F6.2)") v, x                        ! 두 개의 숫자를 읽습니다.

    ! 파일을 쓰려면.
    open (unit=12, file="records.txt", status="replace")
    ! 파일은 9:99 범위에서 선택한 정수인 '단위 번호'로 참조됩니다. 상태는 {'old','replace','new'} 중 하나일 수 있습니다.
    write (12, "(F10.2,F10.2,F10.2)") c, b, a
    close (12)

    ! 파일을 읽으려면.
    open (newunit=m, file="records.txt", status="old")
    ! 파일은 컴파일러가 선택한 정수인 '새 단위 번호'로 참조됩니다.

    read (unit=m, fmt="(3F10.2)") a, b, c
    close (m)

    ! 여기서 논의된 것보다 더 많은 기능이 있으며, 이전 Fortran 버전과의 이전 버전과의 호환성으로 인해 대체 변형이 있습니다.

    ! 내장 함수
    ! ==================

    ! Fortran에는 언어에 내장된 약 200개의 함수/서브루틴이 있습니다.
    ! 예 -
    call cpu_time(v)        ! 'v'를 초 단위 시간으로 설정합니다.
    k = ior(i, j)           ! 2개의 정수에 대한 비트 OR입니다.
    v = log10(x)            ! 밑이 10인 로그입니다.
    i = floor(b)            ! b를 내림하여 정수로 변환합니다.
    v = aimag(w)            ! 복소수의 허수부입니다.

    ! 함수 및 서브루틴
    ! =======================

    ! 서브루틴은 일부 입력 값에 대해 일부 코드를 실행하고 부작용을 일으키거나 입력 값을 수정할 수 있습니다.

    call routine(a, c, v)   ! 서브루틴 호출입니다.

    ! 함수는 여러 입력 매개변수를 사용하고 단일 값을 반환합니다.
    ! 그러나 입력 매개변수는 여전히 수정될 수 있으며 부작용이 실행될 수 있습니다.

    m = func(3, 2, k)       ! 함수 호출입니다.

    ! 함수 호출은 표현식 내에서도 호출될 수 있습니다.
    print *, func2(3, 2, k)

    ! 순수 함수는 입력 매개변수를 수정하거나 부작용을 일으키지 않는 함수입니다.
    m = func3(3, 2, k)

contains                    ! 프로그램의 내부 프로시저 정의 시작:

    ! Fortran에는 함수를 정의하는 약간 다른 방법이 몇 가지 있습니다.

    integer function func(a, b, c)      ! 정수 값을 반환하는 함수입니다.
        ! implicit none                 ! - 더 이상 하위 변수 필드에서 사용되지 않음
        integer, intent(in) :: a, b, c  ! 입력 매개변수의 유형
        ! 반환 변수는 기본적으로 함수 이름입니다.

        if (a >= 2) then
            func = a + b + c
            return                      ! 'func'에서 현재 값을 반환합니다.
        end if
        func = a + c

        ! 함수 끝에 return 문이 필요하지 않습니다.
    end function func

    function func2(a, b, c) result(f)   ! 반환 변수가 'f'로 선언되었습니다.
        integer, intent(in) :: a, b     ! 변수가 함수에 의해 수정되지 않도록 선언하고 강제할 수 있습니다.
        integer, intent(inout) :: c
        integer :: f
        ! 함수 반환 유형이 함수 내에서 선언되었습니다.
        integer :: cnt = 0               ! GOTCHA -
        ! 초기화 시 값을 할당하면
        ! 변수가 함수 호출 간에 저장됨을 의미합니다.

        f = a + b - c
        c = 4                           ! 입력 변수 c의 값 변경.
        cnt = cnt + 1                   ! 함수 호출 수 계산.

    end function func2

    pure function func3(a, b, c)        ! 순수 함수는 부작용이 없습니다.
        integer, intent(in) :: a, b, c
        integer :: func3

        func3 = a*b*c

    end function func3

    ! 서브루틴은 아무것도 반환하지 않지만,
    ! 인수 값을 변경할 수 있습니다.
    subroutine routine(d, e, f)
        real, intent(inout) :: f
        real, intent(in) :: d, e

        f = 2*d + 3*e + f

    end subroutine routine

end program example
! 프로그램 정의 끝 -----------------------

! 프로그램 목록 외부에 선언된 함수 및 서브루틴은 인터페이스 선언을 사용하여 프로그램에 선언해야 합니다(동일한 소스 파일에 있더라도!). (아래 참조). 모듈 또는 프로그램의 'contains' 섹션 내에 정의하는 것이 더 쉽습니다.

elemental real function func4(a) result(res)
! 요소 함수는 스칼라 입력 변수를 사용하는 순수 함수이지만, 배열에서도 사용할 수 있으며, 배열의 모든 요소에 개별적으로 적용되고 새 배열을 반환합니다.
    real, intent(in) :: a

    res = a**2 + 1.0

end function func4

! 모듈
! =======

! 모듈은 관련 선언, 함수 및 서브루틴을 함께 수집하여 재사용성을 높이는 유용한 방법입니다.

module fruit

    real :: apple
    real :: pear
    real :: orange

end module fruit

module fruity
    ! 선언은 모듈, 인터페이스, 변수 순서여야 합니다.
    ! (프로그램에서도 모듈 및 인터페이스를 선언할 수 있음).

    use fruit, only: apple, pear    ! fruit 모듈에서 apple 및 pear 사용.
    implicit none                   ! 모듈 가져오기 뒤에 옴.

    ! 기본적으로 모든 모듈 데이터 및 함수는 public입니다.
    private                         ! 대신 기본값을 private으로 설정
    ! 일부 변수/함수를 명시적으로 public으로 선언합니다.
    public :: apple, mycar, create_mycar
    ! 모듈에 일부 변수/함수를 private으로 선언합니다(여기서는 중복됨).
    private :: func4

    ! 인터페이스
    ! ==========
    ! 모듈 내에서 외부 함수/프로시저를 명시적으로 선언합니다.
    ! (일반적으로 'contains' 섹션에 함수/프로시저를 넣는 것이 더 좋음).
    interface
        elemental real function func4(a) result(res)
            real, intent(in) :: a
        end function func4
    end interface

    ! 오버로드된 함수는 명명된 인터페이스를 사용하여 정의할 수 있습니다.
    interface myabs
        ! 'module procedure' 키워드를 사용하여 모듈 내에 이미 정의된 함수를 포함할 수 있습니다.
        module procedure real_abs, complex_abs
    end interface

    ! 파생 데이터 유형
    ! ==================
    ! 사용자 지정 구조화된 데이터 컬렉션을 만들 수 있습니다.
    type car
        character(len=100) :: model
        real :: weight              ! (kg)
        real :: dimensions(3)       ! 즉, 길이-너비-높이 (미터).
        character :: colour
    contains
        procedure :: info           ! 프로시저를 유형에 바인딩합니다.
    end type car

    type(car) :: mycar              ! 사용자 지정 유형의 변수를 선언합니다.
    ! 사용법은 create_mycar() 루틴을 참조하십시오.

    ! 참고: 모듈에는 실행 가능한 문이 없습니다.

contains

    subroutine create_mycar(mycar)
        ! 파생 데이터 유형의 사용법을 보여줍니다.
        type(car), intent(out) :: mycar

        ! '%' 연산자를 사용하여 유형 요소에 액세스합니다.
        mycar%model = "Ford Prefect"
        mycar%colour = 'r'
        mycar%weight = 1400
        mycar%dimensions(1) = 5.0   ! 기본 인덱싱은 1부터 시작합니다!
        mycar%dimensions(2) = 3.0
        mycar%dimensions(3) = 1.5

    end subroutine create_mycar

    subroutine info(self)
        class(car), intent(in) :: self
        ! 'class' 키워드는 여기서 프로시저를 유형에 바인딩하는 데 사용됩니다.

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
        ! 긴 줄은 연속 문자 '&'를 사용하여 계속할 수 있습니다.

        complex_abs = sqrt(real(z)**2 + &
                           aimag(z)**2)

    end function complex_abs

end module fruity

! ISO 표준 Fortran 2008은 루프 수준 병렬 처리를 표현할 수 있도록 DO CONCURRENT 구문을 도입했습니다.

integer :: i
real :: array(10)

DO CONCURRENT (i = 1:size(array))
    array(i) = sqrt(real(i)**i)
END DO


! 루프 내에서는 순수 함수 호출만 허용되며 여러 인덱스를 선언할 수 있습니다:

integer :: x, y
real :: array(8, 16)

do concurrent (x = 1:size(array, 1), y = 1:size(array, 2))
    array(x, y) = real(x)
end do

! 루프 인덱스는 구문 내에서도 선언할 수 있습니다:

real :: array(8, 16)

do concurrent (integer :: x = 1:size(array, 1), y = 1:size(array, 2))
    array(x, y) = real(x)
end do
```

### 추가 자료

Fortran에 대한 자세한 내용은 다음을 참조하십시오:

+ [위키백과](https://en.wikipedia.org/wiki/Fortran)
+ [Fortran-lang 조직](https://fortran-lang.org/)
+ [Fortran_95_language_features](https://en.wikipedia.org/wiki/Fortran_95_language_features)
+ [fortranwiki.org](http://fortranwiki.org)
+ [www.fortran90.org/](http://www.fortran90.org)
+ [Fortran 95 튜토리얼 목록](http://www.dmoz.org/Computers/Programming/Languages/Fortran/FAQs%2C_Help%2C_and_Tutorials/Fortran_90_and_95/)
+ [Fortran 위키북](https://en.wikibooks.org/wiki/Fortran)
+ [Fortran 자료](http://www.fortranplus.co.uk/resources/fortran_resources.pdf)
+ [놀랄 수 있는 Fortran 90 프로그램의 실수](http://www.cs.rpi.edu/~szymansk/OOF90/bugs.html)