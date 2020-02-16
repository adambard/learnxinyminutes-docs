---
language: COBOL
filename: learncobol.cbl
contributors:
    - ["Mikael Kemstedt", "https://github.com/MikaelKemstedt"]
---

COBOL stands for "**Co**mmon **B**usiness **O**riented **Language**.
It is a high-level programming language first developed in 1959 
(standardised in 1968) and as the name suggests it is designed for developing 
business with mostly file-oriented applications.

Today it's still in use mainly because there's just so much code written, and
the cost of maintaining it is cheaper than translating it to another language 
(like Java). Another part is the "If it ain't broke don't fix it" mentality. 

The syntax is pretty easy, it's almost like talking to the computer. But 
actually using what you've learned is a bit harder since if you're going to be 
working with COBOL you'll need to learn a bunch of other things surrounding it. 
Like JCL, PL/1, CICS, DB2 etc. 

This guide will use:
    COBOL v6.3.0 as well as the standard IBM z/OS compiler. 



Before we go into the code block.
The first thing you have to think about is the coloumns (80 in total):
12345678901234567890123456789012345678901234567890123456789012345678901234567890
The first 6 are reserved for line numbers looking something like this:
00100
00200
00300
...

If you're working on a mainframe you can type in line commands on those numbers.
But if you're on any other platform you can ignore these numbers. 

The 7th columnis reserved for comments, continuation, printer stopper and 
debug indication marks:
* 
- 
/
D

Column 8-11 is called "Column A" and column 12-72 is "Column B". 
Column A is where you write the first level of your code, indention matters. 
Column B is where you write everything else, indention does not matter.

Column 73-80 is used for identification purpose. So the program will not read
anything that is written here, except for the system generated numbers. Just 
don't write anything here and you're good.

```cobol
           *> Inline comments look like this.
      *****************************************************************
      * In COBOL you usually write an annotation of when the program was
      * written as well as changed, who wrote / changed it, and what did
      * they do. 
      * Keep in mind everyone got different standards so it might not 
      * look the same for you as it does here. 
      *
      * Program name:    LEARNCOBOL                               
      * Original author: MIKAEL KEMSTEDT                                
      *
      * Maintenence Log                                              
      * Date      Author        Maintenance Requirement               
      * --------- ------------  --------------------------------------- 
      * 16/02/2020 MIKAEL KEMSTEDT  Started writing the guide.         
      *                                                               
      *****************************************************************
      * There are 4 kinds of levels in COBOL:
      * DIVISION
      * SECTION
      * PARAGRAPH
      * CODE
      
      * As you see below the IDENTIFICATION DIVISION is the highest 
      * level So everything below it will run until it hits another
      * division.
      * Inside this division there are sections. The section will run
      * until it hits another section or the division ends.
      * Inside that are paragraphs, which will run until it hits
      * another paragraph, section, or division.
      * Lastly is the code, which runs until it hits the end of itself,
      * paragraph, section, or division.
      
      * In COBOL you always need 4 divisions, the 1st one is this:
       IDENTIFICATION DIVISION.
      * This is where you write a some information about the program
      * itself. Though only PROGRAM-ID. is the mandatory field.
      * Filenames should always be max 8 characters. Some characters are
      * reserved, these are:
      * AFB
      * AFH
      * CBC
      * CEE
      * CEH
      * CEL
      * CEQ
      * CEU
      * DFH
      * DSN
      * EDC
      * FOR
      * IBM
      * IFY
      * IGY
      * IGZ
      * ILB
      
      * Having one of those 3 characters at the start of your program
      * name might confuse the compiler. Which you do not want. 
      * It's recommended to keep the ID the same as the file name for
      * easier maintanence. 
       PROGRAM-ID.  LEARNCBL.
       AUTHOR. MIKAEL KEMSTEDT. 
       DATE-WRITTEN. 16/02/2020. 
       DATE-COMPILED. 16/02/2020. 
       SECURITY. NON-CONFIDENTIAL.
      *****************************************************************
      *****************************************************************
      * The 2nd division is this one:
       ENVIRONMENT DIVISION.
      * Enviroment division has two sections,
      * One for configuration (all optional):
       CONFIGURATION SECTION.
      * Source computer is where the program will be compiled on.
       SOURCE-COMPUTER. IBM-370.
      * Object computer is where the program will run. 
       OBJECT-COMPUTER. IBM-370.
      * Special names changes the reading of the data that you get. 
       SPECIAL-NAMES. 
      * The most used ones are:
      *    DECIMAL-POINT IS COMMA. 
      * Makes COBOL read 1,000.99 as 1.000,99

           CURRENCY SIGN IS 'USD' WITH PICTURE SYMBOL '$'. 
      * Currency sign is characters while the symbol is well, a symbol.
      * $ is the default currency symbol. 
      * If $ is written as currency sign then it is ignored.

      * And One for files (all optional): 
       INPUT-OUTPUT SECTION.
      * FILE-CONTROL is where you connect external files with your own
      * program as well as define a few extra variables if you want.
       FILE-CONTROL.
      * Read the following as:
      * Select <file_var> to assign to the file "file.txt"
           SELECT FILEIN ASSIGN TO 'FILEIN.TXT'
      * In mainframe you'll write another vairable name instead of the 
      * file because it is sent by the JCL instead.
      *
      * The one varoable that you almost always want is this one:
              FILE STATUS IS FILEIN-STATUS
      * The ones you want to remember are "00" for success and "10" for 
      * end-of-file. You can find the rest of the codes here:
      * http://ibmmainframes.com/references/a27.html
      
      * You also got the following.
      * You got 3 types of files, sequential, relative, indexed. 
      * The syntax for those are the following:
      * Sequential (most used):
              ACCESS MODE SEQUENTIAL.  
      * Relative:
      *       ACCESS MODE {SEQUENTIAL/DYNAMIC/RANDOM} 
      *       [RELATIVE KEY IS <var>]
      * Indexed:
      *       ACCESS MODE {SEQUENTIAL/DYNAMIC/RANDOM}
          
      *       ALTERNATE RECORD KEY IS <var> [WITH DUPLICATES]
             
      * Following is only needed if you use relative or indexed file.
      * This describes the logical structore of the file.
      * Default is sequential
      *       ORGANIZATION IS {SEQUENTIAL/RELATIVE/INDEXED}

      * Optional in the following make the program create the file if it
      * does not exist. 
      *    SELECT OPTIONAL FILEOUT ASSIGN TO 'FILEOUT.TXT'.
           SELECT FILEOUT ASSIGN TO 'FILEOUT.TXT'
              FILE STATUS IS FILEOUT-STATUS
              ACCESS MODE SEQUENTIAL.
      * Note that the '.' is after the last row to connect all rows 
      * about the file together.

      * I-O-Control specifies when checkpoints are to be taken and the 
      * storage areas to be shared by different files. Read more about
      * it here:
      * https://www.ibm.com/support/knowledgecenter/SSQ2R2_9.1.1/com.ibm.ent.cbl.zos.doc/PGandLR/ref/rliosioc.html
       I-O-CONTROL. 
      
      * DATA DIVISION is the 3rd division, it contains all the variables
      * and their information that you're using in your program. 
      * ALL the variables that you use have to be defined here. There
      * is no such thing as creating a variable inside the code. 
       DATA DIVISION.
      * There are 3 sections under DATA DIVISiON
      * File section defines how the files you declared above, look.
       FILE SECTION.
      * FD <file_var>.
       FD  FILEIN.
      * Let's say the file looks like this:
      * TRANSACTIONID  AMMOUNT  BALANCEBEFORE  BALANCEAFTER
      * The file declaration can represent that like this:
       01  FILEIN-POST. *> The "group variable"
           05 FILEIN-TRANSID             PIC 9(5)X(5),99.
           05 FILEIN-AMMOUNT             PIC ZZZ,ZZZ,ZZ9.99.
           05 FILEIN-BALANCEBEFORE       PIC ZZZ,ZZZ,ZZ9.99.
           05 FILEIN-BALANCEAFTER        PIC ZZZ,ZZZ,ZZ9.99.
      * What exacly the PIC 9(5)... means will be covered after the file 
      * section.

      * But you might not know exactly what the file looks like, in that
      * case. Just declare it as a single long variable
       FD  FILEOUT.
       01  FILEOUT-POST                  PIC X(80).
      * You can also change other things, similar to the FILE-CONTROL
      * declarations. It would look something like this:
      *FD FILEIN
      *    RECORD CONTAINS 80 CHARACTERS
      *    BLOCK CONTAINS 10 RECORDS
      *    DATA RECORD IS FILEIN-POST

      *    RECORDING MODE IS {F/D/V} 
      * Using this changes recod and block size.
      * F = FIXED LENGTH syntax is like above.

      * D = DYNAMIC LENGTH syntax is:
      *    RECORD IS VARYING IN SIZE 1 TO 80 [DEPENDING ON <var>]
      *    BLOCK CONTRAINS IN SIZE 1 TO 10 [DEPENDING ON <var>]

      * V = VARIABLE LENGTH syntax is:
      *    RECORD CONTAINS 1 TO 80 CHARACTERS
      *    BLOCK CONTRAINS 1 TO 10 CHARACTERS
      * More in-depth here:
      * https://www.mainframestechhelp.com/tutorials/cobol/cobol-file-section.htm

      *****************************************************************
      * In WORKING-STORAGE we declare every variable that has its origin
      * in the program. 
       WORKING-STORAGE SECTION.
      * A variable is declared in 3 steps:
      * level-number data-name           picture clause
      * The level-number is two numbers that say what type of variable 
      * it is (var, const, boolean) and at what level it is at.
      * The level-numbers that you use are:
      * 01-49, 66, 77, and 88.
      * 01-49 are general level-numbers that you use to declare normal
      * variables.
      * 66 is not really used anymore, it has to do with groups which 
      * will be explained in a little bit.
      * 77 is not a parent nor child variable. It is generally not used.
      * 88 are booleans. Either true or false.

      * Every variable has to start at a 01 or 77 level in column A.
      * Then you got the data-name or the actual variable name you'll
      * reference when writing your code. 
      * Different places has different standards on how these are named.
      
      * Last part is the declaration of what the variable contains.
      * PIC is short for PICTURE. After PIC You write what type of data
      * it is. Here in the first we have 99 which means two numbers.
      * A = ALPHABETIC (OR SPACE)
      * B = SPACE
      * P = DECIMAL SCALING POSITION (doesn't count in size of the item)
      * S = OPERATIONAL SIGN (doesn't count in size of the item)
      * V = ASSUMED DECIMAL POINT (doesn't count in size of the item)
      * X = ALPHANUMERIC CHARACTER (any from the EBCDIC set)
      * Z = ZERO SUPRESSION CAHRACTER (001.00 -> 1.00)
      * There are some more special ones, you can find them here:
      * https://www.ibm.com/support/knowledgecenter/en/ssw_ibm_i_73/rzasb/picsym.htm#picsym

      * The ones you're going to use 99% of the time are: X, A, 9, V, Z,
      * and S
      
      * When defining a file-status variable, it's two numbers.
      * So we just put two 9s together when declaring it.
       01 FILEIN-STATUS                  PIC 99. 
           88 FILEIN-EOF                     VALUE '10'.
      * 88 levels don't use pic, they use VALUE instead. 
      * if FILEIN-STATUS = 10 then FILEIN-EOF = TRUE 
      * if it's something else     FILEIN-EOF = FALSE.
      * If you have one 88 level it's generally called a switch.
       01 FILEOUT-STATUS                 PIC 99.
           88 FILEOUT-EOF                    VALUE '10'.  
           88 FILEOUT-NOTOPEN                VALUE '48'.
      * If you have multiple, it's called flags.
      
      * You have groups as well. This way you can easily split up a
      * variable into smaller pieces in case you only want to change
      * a small part of it
       01 CURRENT-DATE. *> In this case, this group is declared as:
      * PIC X(8).
      * Groups are always declared as X.
           05 C-YEAR                     PIC 9(4). *> same as PIC 9999.
      * We use 05 in case we want to add something later on and we don't
      * want to change every single level-number. So adding a 03 item
      * above this and below the 01 will make the 03 item the parent
      * and the 01 item the grand parent. 
           05 C-MONTH                    PIC 99.
      * Note the '.' are after every declaration.
           05 C-DAY                      PIC 99.

      * We can also declare tables. (avoid if you're not sure they
      * will hold for ~50 years, also avoid big ones since they take up
      * a lot of memory)
       01 NOTABLE.
      * This will make all the variables below have 10 instances.
           05 YESTABLE OCCURS 10
              INDEXED BY YES-INDEX. *> Dot after this since it all goes 
                                    *> together
      * You don't have to declare the index combined with the table,
      * defining it alone is also ok. But linking it to the table 
      * prevents you from using it somewhere else.
              10 YESTABLE-1              PIC A(3).
              *> You can make it variable only.
              10 YESTABLE-2              PIC X OCCURS 2.
              *> YESTABLE-2 has 10 * 2 instances.

      * You can also nest them until you run out of level-numbers.
      * It gets confusing after a couple of levels though.
                 15 YESNEST1             OCCURS 10.
                    20 YESNEST2          OCCURS 10.
                       25 YESNEST3       OCCURS 10.
                          30 YESNEST4    OCCURS 10.
      * They don't have to be indented by 4 every time in column B. But
      * it has to be more than the one above.
                             35 YESNEST5 OCCURS 10.
                              45 YESNEST6 OCCURS 10000000.
                          *> It also doesn't have to be on the same row.
                                                                      49 
                                                                    YEET
                                                             PIC ZZ9.99.
      * Just because you can doesn't mean you should. 
      * Try to keep the declarations tidy.

      * There are a few data typs that we can add to after the 
      * pic clause. One of them is COMP.
       01 NO-INDEX                       PIC 99 USAGE IS COMP.
                                 *> Just PIC 99 COMP. Also works.
      * The data types you have for numbers (only S and 9) are:
      * COMP / COMP-1 / COMP-4 / BINARY = 2 or 4 bytes int
      * COMP-2 = 8 bytes int
      * COMP-3 / PACKED DECIAMAL = (n + 1) / 2 bytes float
      * The ones you're going to use the most are COMP and COMP-3

      * Now we can talk about the number-level 66.
      * First we make a normal group.
       01 TIMESTAMP.
           05 T-YEAR                     PIC X(4) VALUE '2020'. 
           *> Since it's x we have to use '' for the value.
           05 T-MONTH                    PIC X(2). 
           *> Don't use VALUE if you change the value later on.
           05 T-DAY                      PIC X(2). 
           05 T-HOUR                     PIC 9(2) VALUE 00.
           *> With 9 you get an error if you use ''. 
           05 T-MIN                      PIC 9(2). 
           05 T-SEC                      PIC 9(2). 
      
      * Now the 66 renames the variables above. 
       66 T-DATE RENAMES T-YEAR THRU T-DAY.
      * So now if you only want the date instead of the full timestamp,
      * you only get the date when you reference T-DATE.
      * Note that this only references the variables that you enter
      * it doesn't change them.

      * If you have something like a date and you want to make it look
      * like this instead: 2020-01-01
      * You can use filler like this:
       01 FIXED-TIMESTAMP.
           05 FT-YEAR                    PIC X(4).
           05 FILLER                     PIC X VALUE '-'.
           05 FT-MONTH                   PIC X(2). 
           05 FILLER                     PIC X VALUE '-'.
           05 FT-DAY                     PIC X(2). 
           05 FILLER                     PIC X VALUE 'T'.
           05 FT-HOUR                    PIC 9(2). 
           05 FILLER                     PIC X VALUE ':'.
           05 FT-MIN                     PIC 9(2). 
           05 FILLER                     PIC X VALUE ':'.
           05 FT-SEC                     PIC 9(2).
      * The good thing about this is that you can't change the value
      * directly, you have to change the whole FIXED-TIMESTAMP. Which 
      * you shouldn't do anyway, so using fillers prevents small 
      * mistakes like that. 

      * Some variables we will use in the code below.
       01 A                              PIC X.
       01 B                              PIC X.
      *****************************************************************
      * The 4th and last division. Under here we write all the code.
       PROCEDURE DIVISION.
      * First let's check out the DIVISION > SECTION > PARAGRAPH > CODE
      * thing I mentioned earlier.
      * Since divisions are only used above in the right places we don't
      * use them here.

      * The code starts right after the PRECEDURE DIVISION so you can 
      * write whatever you want after that and it will run. In general 
      * you want to start with a section or paragraph.
       A100-MAIN SECTION.
           
      * To start another section / paragraph you use perform.
           PERFORM B100-INITIALIZE
           PERFORM C100-PROCESS 
           . *> A100-MAIN SECTION ends here with the .

       B100-INITIALIZE SECTION.
      * The code then jumps here and then when this section is over it 
      * will go back to where it was called from. In this case A100-MAIN

      * In here you can have paragraphs
       B110-FILL-DATE. *> Note that pragraphs are written exactly the
      * same as a section except you remove the word "SECTION" from the
      * end. 
           
           *> We will look at functions more but just as an example
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE 

           . *> B110-FILL-DATE ends here
      * But because B110-FILL-DATE is a paragraph the section hasn't 
      * ended and we keep going.
      
       B120-DISPLAY-DATE.
           DISPLAY CURRENT-DATE *> Writes the date to the console.

           . *> Paragraph ends here section keeps going
      * I wouldn't recommend letting it bleed over like this. Instead 
      * just call the paragraph in the end of the paragraph above for 
      * easier maintanence. 
       
      * You don't need to end the section in some way. 
      * It ends as the next section starts
      * But for clarity you can do something like this:
       SECTION-END.
           EXIT SECTION
           .
      * Visual summary would be something like this:
      * PROGRAM
      *    DIVISION
      *       SECTION
      *          PARAGRAPH
      *             CODE (like the move statement we did earlier)


       C100-PROCESS SECTION.
      * Statements
      * Before you start coding you want to initialize the variables
           INITIALIZE A
                      B 
      
      * What you will use the most is this:
           MOVE 'B' TO A
           MOVE A TO B
      * With move you just copy one content into the other. 
      * In other languages this would be the same as A = B

           SET YES-INDEX TO 1
      * YES-INDEX = 1
           SET YES-INDEX UP BY 1
           SET YES-INDEX DOWN BY 1
       
           OPEN INPUT FILEIN.
           OPEN OUTPUT FILEOUT.
       
      
      * Two statements you want to avoid:
           ALTER Z999-ABEND TO A100-MAIN 
      * This just makes it incredibly hard to maintain. NEVER use this.
           GO TO Z999-ABEND *> We altered it so we just start over.
      * Only time you may use go to is when you have to exit the code 
      * because you go an error or something like that. 
           .

       Z999-ABEND.
           GOBACK
           .
```


## Further Reading

 * [IBM Documentation](https://www.ibm.com/support/knowledgecenter/en/SS6SG3_6.3.0/welcome.html)
 * [IBM Programming Guide](https://www.ibm.com/support/knowledgecenter/en/SS6SG3_6.3.0/pg/abouthst.html)