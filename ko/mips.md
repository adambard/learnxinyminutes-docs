---
name: "MIPS Assembly"
filename: MIPS.asm
contributors:
  - ["Stanley Lim", "https://github.com/Spiderpig86"]
translators:
    - ["Taeyoon Kim", "https://github.com/partrita"]
---

MIPS(Microprocessor without Interlocked Pipeline Stages) 어셈블리 언어는 1981년 J. L. Hennessy가 설계한 MIPS 마이크로프로세서 패러다임과 함께 작동하도록 설계되었습니다. 이러한 RISC 프로세서는 게이트웨이 및 라우터와 같은 임베디드 시스템에 사용됩니다.

[더 읽어보기](https://en.wikipedia.org/wiki/MIPS_architecture)

```asm
# 주석은 '#'으로 표시됩니다.

# '#' 뒤에 오는 모든 것은 어셈블러의 렉서에 의해 무시됩니다.

# 프로그램은 일반적으로 .data와 .text 섹션을 포함합니다.

.data # 데이터가 메모리에 저장되는 섹션(RAM에 할당됨), 상위 수준 언어의
      # 변수와 유사합니다.

  # 선언은 (레이블: .타입 값) 형식의 선언을 따릅니다.
  hello_world: .asciiz "Hello World\n"      # null로 끝나는 문자열 선언
  num1: .word 42                            # 정수는 워드(32비트 값)라고 합니다.

  arr1: .word 1, 2, 3, 4, 5                 # 워드 배열
  arr2: .byte 'a', 'b'                      # 문자 배열 (각 1바이트)
  buffer: .space 60                         # RAM에 공간을 할당합니다.
                                            # (0으로 지워지지 않음)

  # 데이터 타입 크기
  _byte: .byte 'a'                          # 1바이트
  _halfword: .half 53                       # 2바이트
  _word: .word 3                            # 4바이트
  _float: .float 3.14                       # 4바이트
  _double: .double 7.0                      # 8바이트

  .align 2                                  # 데이터의 메모리 정렬, 여기서
                                            # 숫자는 2의 거듭제곱으로
                                            # 바이트 정렬을 나타냅니다.
                                            # (.align 2는 2^2 = 4바이트이므로
                                            # 워드 정렬을 나타냅니다)

.text                                       # 명령어와 프로그램 로직을
                                            # 포함하는 섹션
.globl _main                                # 명령어 레이블을 전역으로 선언하여
                                            # 다른 파일에서 접근할 수 있도록 합니다.

  _main:                                    # MIPS 프로그램은 명령어를
                                            # 순차적으로 실행하며, 이 레이블
                                            # 아래의 코드가 먼저 실행됩니다.

    # "hello world"를 출력해 봅시다
    la $a0, hello_world                     # 메모리에 저장된 문자열의
                                            # 주소 로드
    li $v0, 4                               # 시스템 호출 값 로드 (어떤
                                            # 시스템 호출을 할지 나타내는 숫자)
    syscall                                 # 주어진 인수($a0)로 지정된
                                            # 시스템 호출 수행

    # 레지스터 (프로그램 실행 중 데이터 보관에 사용)
    # $t0 - $t9                             # 서브루틴 내부의 중간 계산에
                                            # 사용되는 임시 레지스터
                                            # (함수 호출 간에 저장되지 않음)

    # $s0 - $s7                             # 서브루틴 호출 간에 값이 저장되는
                                            # 저장 레지스터.
                                            # 일반적으로 스택에 저장됨

    # $a0 - $a3                             # 서브루틴에 인수를 전달하기 위한
                                            # 인수 레지스터
    # $v0 - $v1                             # 호출자 함수에 값을 반환하기 위한
                                            # 반환 레지스터

    # 로드/저장 명령어 유형
    la $t0, label                           # 레이블로 지정된 메모리의 값의
                                            # 주소를 레지스터 $t0에 복사
    lw $t0, label                           # 메모리에서 워드 값 복사
    lw $t1, 4($s0)                          # 4바이트 오프셋이 있는 레지스터에
                                            # 저장된 주소에서 워드 값 복사
                                            # (addr + 4)
    lb $t2, label                           # 바이트 값을 레지스터 $t2의
                                            # 하위 부분에 복사
    lb $t2, 0($s0)                          # $s0의 소스 주소에서 오프셋 0으로
                                            # 바이트 값 복사
    # 하프워드의 경우 'lh'와 동일한 아이디어

    sw $t0, label                           # 레이블로 매핑된 메모리 주소에
                                            # 워드 값 저장
    sw $t0, 8($s0)                          # $s0에 지정된 주소와 8바이트
                                            # 오프셋에 워드 값 저장
    # 바이트와 하프워드의 경우 'sb'와 'sh'를 사용하는 동일한 아이디어. 'sa'는 존재하지 않음

### 수학 ###
  _math:
    # 레지스터에 값을 로드하는 것을 잊지 마십시오
    lw $t0, num                             # 데이터 섹션에서
    li $t0, 5                               # 또는 즉시(상수)에서
    li $t1, 6
    add $t2, $t0, $t1                       # $t2 = $t0 + $t1
    sub $t2, $t0, $t1                       # $t2 = $t0 - $t1
    mul $t2, $t0, $t1                       # $t2 = $t0 * $t1
    div $t2, $t0, $t1                       # $t2 = $t0 / $t1 (일부 MARS
                                            # 버전에서는 지원되지 않을 수 있음)
    div $t0, $t1                            # $t0 / $t1을 수행합니다. 'mflo'를
                                            # 사용하여 몫을, 'mfhi'를 사용하여
                                            # 나머지를 가져옵니다

    # 비트 시프트
    sll $t0, $t0, 2                         # 2의 즉시(상수 값)로
                                            # 왼쪽으로 비트 시프트
    sllv $t0, $t1, $t2                      # 레지스터의 변수 양만큼
                                            # 왼쪽으로 시프트
    srl $t0, $t0, 5                         # 오른쪽으로 비트 시프트 (부호를
                                            # 보존하지 않음, 0으로 부호 확장)
    srlv $t0, $t1, $t2                      # 레지스터의 변수 양만큼
                                            # 오른쪽으로 시프트
    sra $t0, $t0, 7                         # 오른쪽으로 산술 비트 시프트
                                            # (부호 보존)
    srav $t0, $t1, $t2                      # 레지스터의 변수 양만큼
                                            # 오른쪽으로 시프트

    # 비트 연산자
    and $t0, $t1, $t2                       # 비트 AND
    andi $t0, $t1, 0xFFF                    # 즉시와 비트 AND
    or $t0, $t1, $t2                        # 비트 OR
    ori $t0, $t1, 0xFFF                     # 즉시와 비트 OR
    xor $t0, $t1, $t2                       # 비트 XOR
    xori $t0, $t1, 0xFFF                    # 즉시와 비트 XOR
    nor $t0, $t1, $t2                       # 비트 NOR

## 분기 ##
  _branching:
    # 이러한 분기 명령어의 기본 형식은 일반적으로 <instr>
    # <reg1> <reg2> <label>을 따르며, 여기서 label은
    # 주어진 조건이 참으로 평가될 경우 점프하려는 레이블입니다.
    # 아래의 간단한 if 문 예제에서 볼 수 있듯이
    # 조건 논리를 거꾸로 작성하는 것이 더 쉬울 때가 있습니다.

    beq $t0, $t1, reg_eq                    # $t0 == $t1이면 reg_eq로
                                            # 분기하고, 그렇지 않으면
                                            # 다음 줄을 실행합니다.
    bne $t0, $t1, reg_neq                   # $t0 != $t1일 때 분기
    b branch_target                         # 무조건 분기, 항상 실행됨
    beqz $t0, req_eq_zero                   # $t0 == 0일 때 분기
    bnez $t0, req_neq_zero                  # $t0 != 0일 때 분기
    bgt $t0, $t1, t0_gt_t1                  # $t0 > $t1일 때 분기
    bge $t0, $t1, t0_gte_t1                 # $t0 >= $t1일 때 분기
    bgtz $t0, t0_gt0                        # $t0 > 0일 때 분기
    blt $t0, $t1, t0_gt_t1                  # $t0 < $t1일 때 분기
    ble $t0, $t1, t0_gte_t1                 # $t0 <= $t1일 때 분기
    bltz $t0, t0_lt0                        # $t0 < 0일 때 분기
    slt $s0, $t0, $t1                       # "Set on Less Than"
                                            # $t0 < $t1일 때 $s0에 결과
                                            # (참이면 1)

    # 간단한 if 문
    # if (i == j)
    #     f = g + h;
    #  f = f - i;

    # $s0 = f, $s1 = g, $s2 = h, $s3 = i, $s4 = j라고 가정
    bne $s3, $s4, L1 # if (i !=j)
    add $s0, $s1, $s2 # f = g + h

    L1:
      sub $s0, $s0, $s3 # f = f - i

    # 아래는 3개의 숫자 중 최대값을 찾는 예입니다.
    # MIPS 논리에서 Java로 직접 번역:
    # if (a > b)
    #   if (a > c)
    #     max = a;
    #   else
    #     max = c;
    # else
    #   if (b > c)
    #     max = b;
    #   else
    #     max = c;

    # $s0 = a, $s1 = b, $s2 = c, $v0 = 반환 레지스터라고 가정
    ble $s0, $s1, a_LTE_b                   # if(a <= b) branch(a_LTE_b)
    ble $s0, $s2, max_C                     # if(a > b && a <=c) branch(max_C)
    move $v0, $s0                           # else [a > b && a > c] max = a
    j done                                  # 프로그램 끝으로 점프

    a_LTE_b:                                # a <= b일 때의 레이블
      ble $s1, $s2, max_C                   # if(a <= b && b <= c) branch(max_C)
      move $v0, $s1                         # if(a <= b && b > c) max = b
      j done                                # done으로 점프

    max_C:
      move $v0, $s2                         # max = c

    done:                                   # 프로그램 끝

## 루프 ##
  _loops:
    # 루프의 기본 구조는 종료 조건과 실행을 계속하기 위한
    # 점프 명령어를 갖는 것입니다.
    li $t0, 0
    while:
      bgt $t0, 9, end_while                 # $t0이 10보다 작은 동안
                                            # 계속 반복
      # 실제 루프 내용은 여기에 들어갑니다.
      addi $t0, $t0, 1                      # 값 증가
      j while                               # 루프 시작으로 다시 점프
    end_while:

    # 2D 행렬 순회
    # $a0가 3x3 정수 행렬의 주소를 저장한다고 가정
    li $t0, 0                               # i 카운터
    li $t1, 0                               # j 카운터
    matrix_row:
      bgt $t0, 3, matrix_row_end

      matrix_col:
        bgt $t1, 3, matrix_col_end

        # 작업 수행

        addi $t1, $t1, 1                  # 열 카운터 증가
      matrix_col_end:

      # 작업 수행

      addi $t0, $t0, 1
    matrix_row_end:

## 함수 ##
  _functions:
    # 함수는 위와 같이 레이블로 표시되는 인수와 반환 값을
    # 받을 수 있는 호출 가능한 프로시저입니다.

    main:                                 # 프로그램은 main 함수로 시작
      jal return_1                        # jal은 현재 PC를 $ra에 저장한 다음
                                          # return_1로 점프합니다.

      # 인수를 전달하려면 어떻게 해야 할까요?
      # 먼저 매개변수를 인수 레지스터에 전달해야 합니다.
      li $a0, 1
      li $a1, 2
      jal sum                             # 이제 함수를 호출할 수 있습니다.

      # 재귀는 어떨까요?
      # jal이 각 호출에서 자동으로 덮어쓰므로
      # $ra의 이전 PC를 저장하고 복원해야 하므로
      # 조금 더 작업이 필요합니다.
      li $a0, 3
      jal fact

      li $v0, 10
      syscall

    # 이 함수는 1을 반환합니다.
    return_1:
      li $v0, 1                           # 반환 레지스터 $v0에 값 로드
      jr $ra                              # 이전 PC로 다시 점프하여 실행 계속


    # 2개의 인수를 갖는 함수
    sum:
      add $v0, $a0, $a1
      jr $ra                              # 반환

    # 팩토리얼을 찾는 재귀 함수
    fact:
      addi $sp, $sp, -8                   # 스택에 공간 할당
      sw $s0, ($sp)                       # 현재 숫자를 보유하는 레지스터 저장
      sw $ra, 4($sp)                      # 이전 PC 저장

      li $v0, 1                           # 반환 값 초기화
      beq $a0, 0, fact_done               # 매개변수가 0이면 종료

      # 그렇지 않으면 재귀 계속
      move $s0, $a0                       # $a0을 $s0에 복사
      sub $a0, $a0, 1
      jal fact

      mul $v0, $s0, $v0                   # 곱셈 수행

      fact_done:
        lw $s0, ($sp)
        lw $ra, 4($sp)                     # PC 복원
        addi $sp, $sp, 8

        jr $ra

## 매크로 ##
  _macros:
    # 매크로는 반복되는 코드 블록을 가독성을 위해 단일 레이블로
    # 대체하는 데 매우 유용합니다.
    # 이것은 결코 함수를 대체하는 것이 아닙니다.
    # 사용하기 전에 선언해야 합니다.

    # 개행을 출력하는 매크로 (매우 반복적일 수 있으므로)
    .macro println()
      la $a0, newline                     # 여기에 저장된 새 줄 문자열
      li $v0, 4
      syscall
    .end_macro

    println()                             # 어셈블러는 실행 전에 해당 코드 블록을
                                          # 여기에 복사합니다.

    # 매개변수는 매크로를 통해 전달할 수 있습니다.
    # 이것은 '%' 기호와 원하는 이름으로 표시됩니다.
    .macro print_int(%num)
      li $v0, 1
      lw $a0, %num
      syscall
    .end_macro

    li $t0, 1
    print_int($t0)

    # 매크로에 즉시 값을 전달할 수도 있습니다.
    .macro immediates(%a, %b)
      add $t0, %a, %b
    .end_macro

    immediates(3, 5)

    # 레이블 전달과 함께
    .macro print(%string)
      la $a0, %string
      li $v0, 4
      syscall
    .end_macro

    print(hello_world)

## 배열 ##
.data
  list: .word 3, 0, 1, 2, 6                 # 이것은 워드 배열입니다.
  char_arr: .asciiz "hello"                 # 이것은 문자 배열입니다.
  buffer: .space 128                        # 메모리에 블록을 할당하며,
                                            # 자동으로 지워지지 않습니다.
                                            # 이러한 메모리 블록은 서로
                                            # 인접하게 정렬됩니다.

.text
  la $s0, list                              # list의 주소 로드
  li $t0, 0                                 # 카운터
  li $t1, 5                                 # 리스트의 길이

  loop:
    bge $t0, $t1, end_loop

    lw $a0, ($s0)
    li $v0, 1
    syscall                                 # 숫자 출력

    addi $s0, $s0, 4                        # 워드의 크기는 4바이트
    addi $t0, $t0, 1                        # 증가
    j loop
  end_loop:

## 포함 ##
# 외부 파일을 프로그램으로 가져오려면 이 작업을 수행합니다(실제로는
# 해당 파일에 있는 코드를 가져와 include 문이 있는 위치에
# 배치합니다).
.include "somefile.asm"
```
