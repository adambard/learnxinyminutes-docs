---
language: "MIPS Assembly"
filename: MIPS-cn.asm
contributors:
    - ["Stanley Lim", "https://github.com/Spiderpig86"]
translators:
    - ["Liu Yihua", "https://github.com/yihuajack"]
lang: zh-cn
---

MIPS（Microprocessor without Interlocked Pipeline Stages）汇编语言是为了配合约翰·雷洛伊·亨尼西于1981年设计的 MIPS 微处理器范式而设计的，这些 RISC 处理器用于嵌入式系统，例如网关和路由器。

[阅读更多](https://en.wikipedia.org/wiki/MIPS_architecture)

```asm
# 注释用一个 '#' 表示

# 一行中 '#' 之后的所有文本都会被汇编器的词法分析器忽略

# 程序通常包含 .data 和 .text 部分

.data # 数据存储在内存中（在RAM中分配）
      # 类似于高级语言中的变量

  # 声明遵循（ 标签: .类型 值 ）的声明形式
  hello_world: .asciiz "Hello World\n"      # 声明一个 null 结束的字符串
  num1: .word 42                            # 整数被视为字
                                            # （32位值）

  arr1: .word 1, 2, 3, 4, 5                 # 字数组
  arr2: .byte 'a', 'b'                      # 字符数组（每个1字节）
  buffer: .space 60                         # 在 RAM 中分配空间
                                            # （不清除为0）

  # 数据类型的大小
  _byte: .byte 'a'                          # 1字节
  _halfword: .half 53                       # 2字节
  _word: .word 3                            # 4字节
  _float: .float 3.14                       # 4字节
  _double: .double 7.0                      # 8字节

  .align 2                                  # 数据的内存对齐
                                            # 其中数字（应是2的幂）表示几字节对齐
                                            # .align 2 表示字对齐（因为 2^2 = 4 字节）

.text                                       # 这部分包括指令和程序逻辑
.globl _main                                # 声明一个全局指令标签
                                            # 其他文件都可以访问

  _main:                                    # MIPS 程序按顺序执行指令
                                            # 这条标签下的代码将首先执行

    # 打印 "hello world"
    la $a0, hello_world                     # 加载存储在内存中的字符串地址
    li $v0, 4                               # 加载 syscall 的值
                                            # （数字代表要执行哪个 syscall）
    syscall                                 # 使用给定的参数（$a0）执行指定的 syscall

    # 寄存器（用于在程序执行期间保存数据）
    # $t0 - $t9                             # 临时寄存器，用于过程内部的中间计算
                                            # （过程调用时不保存）

    # $s0 - $s7                             # 保留寄存器（被保留的寄存器，过程调用时保存）
                                            # 通常保存在栈中

    # $a0 - $a3                             # 参数寄存器，用于传递过程的参数
    # $v0 - $v1                             # 返回寄存器，用于向调用过程返回值

    # 存取指令
    la $t0, label                           # 将内存中由 label 指定的值的地址复制到寄存器 $t0 中
    lw $t0, label                           # 从内存中复制一个字
    lw $t1, 4($s0)                          # 从寄存器中存储的地址复制一个字
                                            # 偏移量为4字节（地址 + 4）
    lb $t2, label                           # 把一个字节复制到寄存器 $t2 的低阶部分
    lb $t2, 0($s0)                          # 从 $s0 的源地址复制一个字节
                                            # 偏移量为0
    # 同理也适用于 'lh' （取半字）

    sw $t0, label                           # 将字存储到由 label 映射的内存地址中
    sw $t0, 8($s0)                          # 将字存储到 $s0 指定的地址中
                                            # 偏移量为8字节
    # 同理也适用于 'sb' （存字）和 'sh' （存半字）。'sa'不存在

### 数学 ###
  _math:
    # 记住要将值加载到寄存器中
    lw $t0, num                             # 从数据部分
    li $t0, 5                               # 或者从一个立即数（常数）
    li $t1, 6
    add $t2, $t0, $t1                       # $t2 = $t0 + $t1
    sub $t2, $t0, $t1                       # $t2 = $t0 - $t1
    mul $t2, $t0, $t1                       # $t2 = $t0 * $t1
    div $t2, $t0, $t1                       # $t2 = $t0 / $t1
                                            # （MARS 的某些版本可能不支持）
    div $t0, $t1                            # 执行 $t0 / $t1。
                                            # 用 'mflo' 得商，用 'mfhi' 得余数

    # 移位
    sll $t0, $t0, 2                         # 按位左移立即数（常数值）2
    sllv $t0, $t1, $t2                      # 根据一个寄存器中的变量值左移相应位
    srl $t0, $t0, 5                         # 按位右移
                                            # （不保留符号，用0符号扩展）
    srlv $t0, $t1, $t2                      # 根据一个寄存器中的变量值右移相应位
    sra $t0, $t0, 7                         # 按算术位右移（保留符号）
    srav $t0, $t1, $t2                      # 根据一个寄存器中的变量值右移相应算数位

    # 按位运算符
    and $t0, $t1, $t2                       # 按位与
    andi $t0, $t1, 0xFFF                    # 用立即数按位与
    or $t0, $t1, $t2                        # 按位或
    ori $t0, $t1, 0xFFF                     # 用立即数按位或
    xor $t0, $t1, $t2                       # 按位异或
    xori $t0, $t1, 0xFFF                    # 用立即数按位异或
    nor $t0, $t1, $t2                       # 按位或非

## 分支 ##
  _branching:
    # 分支指令的基本格式通常遵循 <指令> <寄存器1> <寄存器2> <标签>
    # 如果给定的条件求值为真，则跳转到标签
    # 有时向后编写条件逻辑更容易，如下面的简单的 if 语句示例所示

    beq $t0, $t1, reg_eq                    # 如果 $t0 == $t1，则跳转到 reg_eq
                                            # 否则执行下一行
    bne $t0, $t1, reg_neq                   # 当 $t0 != $t1 时跳转
    b branch_target                         # 非条件分支，总会执行
    beqz $t0, req_eq_zero                   # 当 $t0 == 0 时跳转
    bnez $t0, req_neq_zero                  # 当 $t0 != 0 时跳转
    bgt $t0, $t1, t0_gt_t1                  # 当 $t0 > $t1 时跳转
    bge $t0, $t1, t0_gte_t1                 # 当 $t0 >= $t1 时跳转
    bgtz $t0, t0_gt0                        # 当 $t0 > 0 时跳转
    blt $t0, $t1, t0_gt_t1                  # 当 $t0 < $t1 时跳转
    ble $t0, $t1, t0_gte_t1                 # 当 $t0 <= $t1 时跳转
    bltz $t0, t0_lt0                        # 当 $t0 < 0 时跳转
    slt $s0, $t0, $t1                       # 当 $t0 < $t1 时结果为 $s0 （1为真）

    # 简单的 if 语句
    # if (i == j)
    #     f = g + h;
    #  f = f - i;

    # 让 $s0 = f, $s1 = g, $s2 = h, $s3 = i, $s4 = j
    bne $s3, $s4, L1 # if (i !=j)
    add $s0, $s1, $s2 # f = g + h

    L1:
      sub $s0, $s0, $s3 # f = f - i
    
    # 下面是一个求3个数的最大值的例子
    # 从 Java 到 MIPS 逻辑的直接翻译：
    # if (a > b)
    #   if (a > c)
    #     max = a;
    #   else
    #     max = c;
    # else
    #     max = b;
    #   else
    #     max = c;

    # 让 $s0 = a, $s1 = b, $s2 = c, $v0 = 返回寄存器
    ble $s0, $s1, a_LTE_b                   # 如果 (a <= b) 跳转到 (a_LTE_b)
    ble $s0, $s2, max_C                     # 如果 (a > b && a <= c) 跳转到 (max_C)
    move $v0, $s0                           # 否则 [a > b && a > c] max = a
    j done                                  # 跳转到程序结束

    a_LTE_b:                                # 当 a <= b 时的标签
      ble $s1, $s2, max_C                   # 如果 (a <= b && b <= c) 跳转到 (max_C)
      move $v0, $s1                         # 如果 (a <= b && b > c) max = b
      j done                                # 跳转到 done

    max_C:
      move $v0, $s2                         # max = c

    done:                                   # 程序结束

## 循环 ##
  _loops:
    # 循环的基本结构是一个退出条件和一个继续执行的跳转指令
    li $t0, 0
    while:
      bgt $t0, 10, end_while                # 当 $t0 小于 10，不停迭代
      addi $t0, $t0, 1                      # 累加值
      j while                               # 跳转回循环开始
    end_while:

    # 二维矩阵遍历
    # 假设 $a0 存储整数 3 × 3 矩阵的地址
    li $t0, 0                               # 计数器 i
    li $t1, 0                               # 计数器 j
    matrix_row:
      bgt $t0, 3, matrix_row_end

      matrix_col:
        bgt $t1, 3, matrix_col_end

        # 执行一些东西

        addi $t1, $t1, 1                  # 累加列计数器
      matrix_col_end:

      # 执行一些东西

      addi $t0, $t0, 1
    matrix_row_end:

## 函数 ##
  _functions:
    # 函数是可调用的过程，可以接受参数并返回所有用标签表示的值，如前所示

    main:                                 # 程序以 main 函数开始
      jal return_1                        # jal 会把当前程序计数器（PC）存储在 $ra
                                          # 并跳转到 return_1

      # 如果我们想传入参数呢？
      # 首先，我们必须将形参传递给参数寄存器
      li $a0, 1
      li $a1, 2
      jal sum                             # 现在我们可以调用函数了

      # 递归怎么样？
      # 这需要更多的工作
      # 由于 jal 会自动覆盖每次调用，我们需要确保在 $ra 中保存并恢复之前的程序计数器
      li $a0, 3
      jal fact

      li $v0, 10
      syscall
    
    # 这个函数返回1
    return_1:
      li $v0, 1                           # 将值取到返回寄存器 $v0 中
      jr $ra                              # 跳转回原先的程序计数器继续执行


    # 有2个参数的函数
    sum:
      add $v0, $a0, $a1
      jr $ra                              # 返回

    # 求阶乘的递归函数
    fact:
      addi $sp, $sp, -8                   # 在栈中分配空间
      sw $s0, ($sp)                       # 存储保存当前数字的寄存器
      sw $ra, 4($sp)                      # 存储先前的程序计数器

      li $v0, 1                           # 初始化返回值
      beq $a0, 0, fact_done               # 如果参数为0则完成

      # 否则继续递归
      move $s0, $a0                       # 复制 $a0 到 $s0
      sub $a0, $a0, 1
      jal fact

      mul $v0, $s0, $v0                   # 做乘法

      fact_done:
        lw $s0, ($sp)
        lw $ra, ($sp)                     # 恢复程序计数器
        addi $sp, $sp, 8

        jr $ra

## 宏 ##
  _macros:
    # 宏可以实现用单个标签替换重复的代码块，这可以增强程序的可读性
    # 它们绝不是函数的替代品
    # 它们必须在使用之前声明

    # 用于打印换行符的宏（这可以被多次重用）
    .macro println()
      la $a0, newline                     # 存储在这里的新行字符串
      li $v0, 4
      syscall
    .end_macro

    println()                             # 汇编器会在运行前复制此代码块

    # 参数可以通过宏传入。
    # 它们由 '%' 符号表示，可以选择起任意名字
    .macro print_int(%num)
      li $v0, 1
      lw $a0, %num
      syscall
    .end_macro
    
    li $t0, 1
    print_int($t0)
    
    # 我们也可以给宏传递立即数
    .macro immediates(%a, %b)
      add $t0, %a, %b
    .end_macro

    immediates(3, 5)

    # 以及标签
    .macro print(%string)
      la $a0, %string
      li $v0, 4
      syscall
    .end_macro

    print(hello_world)

## 数组 ##
.data
  list: .word 3, 0, 1, 2, 6                 # 这是一个字数组
  char_arr: .asciiz "hello"                 # 这是一个字符数组
  buffer: .space 128                        # 在内存中分配块，不会自动清除
                                            # 这些内存块彼此对齐

.text
  la $s0, list                              # 取 list 的地址
  li $t0, 0                                 # 计数器
  li $t1, 5                                 # list 的长度

  loop:
    bgt $t0, $t1, end_loop

    lw $a0, ($s0)
    li $v0, 1
    syscall                                 # 打印数字

    addi $s0, $s0, 4                        # 字的大小为4字节
    addi $t0, $t0, 1                        # 累加
    j loop
  end_loop:

## INCLUDE ##
# 使用 include 语句可以将外部文件导入到程序中
# （它只是将文件中的代码放入 include 语句的位置）
.include "somefile.asm"
```
