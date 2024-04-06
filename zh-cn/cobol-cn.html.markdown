---
language: COBOL
filename: learn-cn.COB
contributors:
    - ["Hyphz", "http://github.com/hyphz/"]
translators:
    - ["GOLGO11", "https://github.com/GOLGO11/"]
lang: zh-cn
---
COBOL是一门面向商业的语言，它从1960年最初设计以来被修订过数次。它被宣称仍然有超过80%的机构在使用它。

```cobol
      *COBOL. 最好是按照它1985年的标准来编程。 
      *用附带GnuCOBOL编译器的OpenCobolIDE 4.7.6来编译。
       
      *COBOL在老版（COBOL-85）和新版（COBOL-2002以及COBOL-2014）之间有明显的差别。
      *老版COBOL要求编码的前一到六列是空着的（它们被用来存储穿孔卡片的序列号...
      *第七列的一个“*”符号表示注释的开始。
      *在老版COBOL中，一条以*开头的注释最长只能占一行，
      *新版COBOL不需要额外的列来补序列号，并且用“*>" 来注释，允许在行中开始注释
      *老版COBOL也强加了对最大行长度的限制
      *关键字在老版COBOL中必须大写，
      *但在新版COBOL中不区分大小写
      *虽然新版COBOL允许你编写大小写混合的代码
      *但是在大多数情况下编写COBOL代码时全用大写字符
      *大多数专业的COBOL开发者都是这么做的。
      *COBOL语句以句点结尾。
      
      *COBOL代码被拆成了四个部。
      *各部按顺序，它们是:
      *IDENTIFICATION DIVSION.（标识部）
      *ENVIRONMENT DIVISION.（环境部）
      *DATA DIVISION.（数据部）
      *PROCEDURE DIVISION.（过程部）

      *第一步，我们必须给我们的程序一个ID。
      *Identification division 也能包含其他的值，
      *但它们都只是程序的元数据。Program-id是唯一一个必须给出的值。
       IDENTIFICATION DIVISION.
           PROGRAM-ID.    LEARN.
           AUTHOR.        JOHN DOE.
           DATE-WRITTEN.  05/02/2020.

      *让我们来声明一些变量。
      *我们要在DATA DIVISION的WORKING-STORAGE节来完成这个事情。
      *每个数据项（又名变量）从一个级别编号开始。
      *然后是数据项的名字，后边再跟着一个picture关键字，
      *来描述这个变量将要包含的数据的类型。
      *几乎所有的COBOL开发者都会把PICTURE简写为PIC。
      *A代表字母，X代表字母和数字，9代表数字。
       
      *举例：
      01  MYNAME PIC xxxxxxxxxx.    *> 十个字符的字符串。
       
      *但是逐个数那些x会导致错误，
      *所以以上代码可以，并且应该
      *这样重写：
      01 MYNAME PIC X(10).
       
      *这是几个更多的例子：
      01  AGE             PIC      9(3).   *> 数字最多三位
      01  LAST_NAME       PIC      X(10).  *> 字符串最多十个字符
       
      *在COBOL里，一行中多个空格和一个空格的效果是一样的， 所以通常
      *情况下都用多个空格排列代码来便于
      *其他的开发者阅读。
      01  inyear picture s9(7). *> S 使数字为正数.
                                 *> 括号里意思是重复7次9，
                                 *> 即六位数字（不是数组）

      *现在让我们来写一点儿代码。这是一个简单的Hello World程序。
      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 THE-MESSAGE      PIC X(20).
      PROCEDURE DIVSION.
          DISPLAY "STARTING PROGRAM".
          MOVE "HELLO WORLD" TO THE-MESSAGE.
          DISPLAY THE-MESSAGE.
          STOP RUN.
      
      *以上的代码会输出：
      *STARTING PROGRAM
      *HELLO WORLD
      

      
      ********用COBOL可以做数学运算***************
      ADD 1 TO AGE GIVING NEW-AGE.
      SUBTRACT 1 FROM COUNT.
      DIVIDE VAR-1 INTO VAR-2 GIVING VAR-3.
      COMPUTE TOTAL-COUNT = COUNT1 PLUS COUNT2.
      
      
      *********PERFORM********************
      *PERFORM关键字允许你跳到代码中其他特殊的代码段，
      *当这段特殊的代码被执行完后继续回来执行下面的可执行语句。 
      *你必须把PERFORM这个词写完整，不可以缩写它。

      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLOCOBOL.

      PROCEDURE DIVISION.
         FIRST-PARA.
             DISPLAY 'THIS IS IN FIRST-PARA'.
         PERFORM THIRD-PARA THRU FOURTH-PARA. *>跳过second-para，执行3rd&4th
         *> 之后当third和fourth执行完，
         *> 回到这里继续往下执行直到遇到STOP RUN.
   
         SECOND-PARA.
             DISPLAY 'THIS IS IN SECOND-PARA'.
         STOP RUN.
   
         THIRD-PARA.
             DISPLAY 'THIS IS IN THIRD-PARA'.
   
         FOURTH-PARA.
             DISPLAY 'THIS IS IN FOURTH-PARA'.
   
   
      *当你编译执行以上程序时，它会生成以下结果：
          THIS IS IN FIRST-PARA
          THIS IS IN THIRD-PARA
          THIS IS IN FOURTH-PARA
          THIS IS IN SECOND-PARA
          
          
      **********用STRING关键字把变量组合到一起************
      
      *现在是时候学习两个类似的COBOL动词了：string和unstring。.

      *string动词经常被用来连接两个或多个字符串（把它们拼在一起）。
      *没什么特别的，Unstring被用来把一个字符串拆分成两个或多个更小的字符串。       
      *当你在程序中使用string或unstring时不要忘记使用”delimited by“，这个很重要。
      
      IDENTIFICATION DIVISION.
      PROGRAM-ID. LEARNING.
      ENVIRONMENT DIVISION.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 FULL-NAME PIC X(20).
      01 FIRST-NAME PIC X(13) VALUE "BOB GIBBERISH".
      01 LAST-NAME PIC X(5) VALUE "COBB".
      PROCEDURE DIVISION.
          STRING FIRST-NAME DELIMITED BY SPACE
            " "
            LAST-NAME DELIMITED BY SIZE
            INTO FULL-NAME
          END-STRING.
          DISPLAY "THE FULL NAME IS: "FULL-NAME.
      STOP RUN.


      *以上代码将会输出：
      THE FULL NAME IS: BOB COBB


      *让我们来看看为什么是这样。

      *首先，我们在DATA DIVISION声明了所有的变量，
      *包括我们想存储string命令生成的新字符串用到的的变量。

      *这个操作过程在PROCEDURE DIVISION完成。
      *我们从STRING 关键字开始，到END-STRING结束。         
      *在它们之间我们列出我们想要组合变量形成更大的主变量的过程。
      *这里，我们组合了FIRST-NAME, 一个空格和LAST-NAME。

      *跟在FIRST-NAME和LAST-NAME后面的DELIMITED BY短语告诉程序我们想要在各自变量上截取字符的规则。
      *DELIMITED BY SPACE告诉程序从最开始截取字符直到遇到一个空格。
      *DELIMITED BY SIZE告诉程序截取字符的完整长度。
      *我们在FIRST-NAME后面使用DELIMITED BY SPACE，字符串中的GIBBERISH部分就被忽略了。

      *为了更清楚，改变代码中的第10行如下:

      STRING FIRST-NAME DELIMITED BY SIZE

      *然后重新执行程序. 这次的输出变成:

      THE FULL NAME IS: BOB GIBBERISH COBB
```

## 想了解更多吗?

* [GnuCOBOL](https://sourceforge.net/projects/open-cobol/)

